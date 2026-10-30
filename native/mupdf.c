/* MuPDF/Lean ABI bridge. No CLI, filesystem operations or request scheduling.

   Threading model (MuPDF docs: fitz/context.h fz_clone_context, device.h
   fz_cookie): the base context is created with pthread locks. Every call
   borrows a cloned context from a small pool, so several Lean threads can
   rasterize at once. Anything that touches the fz_document (loading pages,
   building display lists, text extraction, outlines) holds doc_lock. Display
   lists are immutable once built and are replayed concurrently without it. */
#include <lean/lean.h>
#include <mupdf/fitz.h>
#include <mupdf/pdf.h>
#include <math.h>
#include <pthread.h>
#include <stdbool.h>
#include <sched.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <openssl/evp.h>

#define POOL_MAX 16
#define LISTS_MAX 24
#define TEXTS_MAX 2

typedef struct {
    int page;
    unsigned long long used;
    fz_display_list *list;
} v_list;

typedef struct {
    fz_context *ctx;                 /* base context: clone source only */
    pthread_mutex_t locks[FZ_LOCK_MAX];
    fz_locks_context lock_ctx;
    pthread_mutex_t doc_lock;        /* fz_document, texts[] */
    pthread_mutex_t state_lock;      /* pool[], lists[] */
    fz_context *pool[POOL_MAX];
    int pool_count;
    fz_document *doc;
    fz_buffer *source;
    int count;
    fz_rect *bounds;
    v_list lists[LISTS_MAX];
    unsigned long long clock;
    fz_stext_page *texts[TEXTS_MAX];
    int text_pages[TEXTS_MAX];
    unsigned next_text;
    int locks_ready;
} v_document;

typedef struct { fz_cookie cookie; } v_cookie;

extern lean_obj_res vellumory_mk_render_result(lean_obj_arg bytes, uint64_t size, uint8_t built, double list_ms,
                                               double raster_ms);

static lean_external_class *document_class;
static lean_external_class *cookie_class;

static void lock_fn(void *user, int n) { pthread_mutex_lock(&((pthread_mutex_t *)user)[n]); }
static void unlock_fn(void *user, int n) { pthread_mutex_unlock(&((pthread_mutex_t *)user)[n]); }

/* Defined with the XMP code below. */
static void xmp_info_fallback(fz_context *ctx, pdf_document *pdf, char out[8][1024], bool have[8]);

static lean_object *failure(const char *message) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(message)));
}

static lean_object *caught_failure(fz_context *ctx) {
    int code;
    /* Explicitly convert the native exception to a language-binding error. */
    const char *message = fz_convert_error(ctx, &code);
    return failure(code == FZ_ERROR_ABORT ? "cancelled" : message);
}

static fz_context *acquire(v_document *d) {
    pthread_mutex_lock(&d->state_lock);
    fz_context *ctx = d->pool_count ? d->pool[--d->pool_count] : fz_clone_context(d->ctx);
    pthread_mutex_unlock(&d->state_lock);
    return ctx;
}

static void release(v_document *d, fz_context *ctx) {
    pthread_mutex_lock(&d->state_lock);
    if (d->pool_count < POOL_MAX) { d->pool[d->pool_count++] = ctx; ctx = NULL; }
    pthread_mutex_unlock(&d->state_lock);
    if (ctx) fz_drop_context(ctx);
}

/* Convert the error while the borrowed context still owns its message. */
static lean_object *release_failure(v_document *d, fz_context *ctx) {
    lean_object *error = caught_failure(ctx);
    release(d, ctx);
    return error;
}

static void finalize(void *data) {
    v_document *d = data;
    if (d->ctx) {
        for (int i = 0; i < LISTS_MAX; ++i) fz_drop_display_list(d->ctx, d->lists[i].list);
        for (int i = 0; i < TEXTS_MAX; ++i) fz_drop_stext_page(d->ctx, d->texts[i]);
        for (int i = 0; i < d->pool_count; ++i) fz_drop_context(d->pool[i]);
        fz_drop_document(d->ctx, d->doc);
        fz_drop_buffer(d->ctx, d->source);
        fz_free(d->ctx, d->bounds);
        fz_drop_context(d->ctx);
    }
    if (d->locks_ready) {
        for (int i = 0; i < FZ_LOCK_MAX; ++i) pthread_mutex_destroy(&d->locks[i]);
        pthread_mutex_destroy(&d->doc_lock);
        pthread_mutex_destroy(&d->state_lock);
    }
    free(d);
}
static void finalize_cookie(void *data) { free(data); }
static void foreach(void *data, b_lean_obj_arg f) { (void)data; (void)f; }

static void register_classes(void) {
    /* Called from Lean's initialization path before any worker thread exists. */
    if (!document_class) document_class = lean_register_external_class(finalize, foreach);
    if (!cookie_class) cookie_class = lean_register_external_class(finalize_cookie, foreach);
}

lean_obj_res vellumory_native_cookie_new(lean_obj_arg world) {
    (void)world;
    register_classes();
    v_cookie *c = calloc(1, sizeof(*c));
    if (!c) return failure("Cannot allocate cancellation cookie");
    return lean_io_result_mk_ok(lean_alloc_external(cookie_class, c));
}

/* fz_cookie is documented as unsynchronized: MuPDF polls abort periodically.
   An atomic store is the strongest guarantee available from the C side. */
lean_obj_res vellumory_native_cookie_abort(b_lean_obj_arg handle, lean_obj_arg world) {
    (void)world;
    v_cookie *c = lean_get_external_data(handle);
    __atomic_store_n(&c->cookie.abort, 1, __ATOMIC_RELEASE);
    return lean_io_result_mk_ok(lean_box(0));
}

static int aborted(fz_cookie *cookie) {
    return cookie && __atomic_load_n(&cookie->abort, __ATOMIC_ACQUIRE);
}

static fz_rect page_bounds(fz_context *ctx, fz_document *doc, pdf_document *pdf, int i) {
    if (pdf) {
        /* Page-tree attributes only: no content streams or annotations. */
        fz_rect box; fz_matrix ctm;
        pdf_page_obj_transform(ctx, pdf_lookup_page_obj(ctx, pdf, i), &box, &ctm);
        return fz_transform_rect(box, ctm);
    }
    fz_page *page = fz_load_page(ctx, doc, i);
    fz_rect r = fz_empty_rect;
    fz_var(r);
    fz_try(ctx) r = fz_bound_page(ctx, page);
    fz_always(ctx) fz_drop_page(ctx, page);
    fz_catch(ctx) fz_rethrow(ctx);
    return r;
}

/* A page's rectangle is usable: finite, and a positive size of at most 10^7 points. */
static int valid_bounds(fz_rect r) {
    double w = (double)r.x1 - r.x0, h = (double)r.y1 - r.y0;
    return isfinite(r.x0) && isfinite(r.y0) && isfinite(w) && isfinite(h) && w > 0 && h > 0 && w <= 1e7 && h <= 1e7;
}

/* Measure every page of D into d->bounds (allocated by the caller). */
static void measure_bounds(fz_context *ctx, v_document *d) {
    pdf_document *pdf = pdf_document_from_fz_document(ctx, d->doc);
    for (int i = 0; i < d->count; ++i) {
        fz_rect r = page_bounds(ctx, d->doc, pdf, i);
        if (!valid_bounds(r)) fz_throw(ctx, FZ_ERROR_ARGUMENT, "Invalid page dimensions on page %d", i + 1);
        d->bounds[i] = r;
    }
}

/* Open a document; a reflowable one (EPUB, FB2, XHTML) is laid out on pages of
   WIDTH × HEIGHT points with EM-point text and the user stylesheet CSS (empty:
   none), as DocView's EPUB font size and stylesheet. */
static lean_obj_res open_document(b_lean_obj_arg bytes, b_lean_obj_arg magic, b_lean_obj_arg password,
                                  double width, double height, double em, const char *css, int defer) {
    if (!(width >= 72 && width <= 14400 && height >= 72 && height <= 14400 && em >= 4 && em <= 144))
        return failure("A reflowed page is 72–14400 points a side with 4–144 point text");
    register_classes();
    v_document *d = calloc(1, sizeof(*d));
    if (!d) return failure("Cannot allocate document handle");
    for (int i = 0; i < FZ_LOCK_MAX; ++i) pthread_mutex_init(&d->locks[i], NULL);
    pthread_mutex_init(&d->doc_lock, NULL);
    pthread_mutex_init(&d->state_lock, NULL);
    d->locks_ready = 1;
    d->lock_ctx.user = d->locks;
    d->lock_ctx.lock = lock_fn;
    d->lock_ctx.unlock = unlock_fn;
    d->ctx = fz_new_context(NULL, &d->lock_ctx, 256 * 1024 * 1024);
    if (!d->ctx) { finalize(d); return failure("Cannot allocate MuPDF context"); }
    for (int i = 0; i < LISTS_MAX; ++i) d->lists[i].page = -1;
    for (int i = 0; i < TEXTS_MAX; ++i) d->text_pages[i] = -1;
    fz_context *ctx = d->ctx;
    fz_stream *stream = NULL;
    fz_var(stream);
    fz_try(ctx) {
        fz_register_document_handlers(ctx);
        d->source = fz_new_buffer_from_copied_data(ctx, lean_sarray_cptr(bytes), lean_sarray_size(bytes));
        stream = fz_open_buffer(ctx, d->source);
        /* magic is a file name or MIME type: PDF, XPS, EPUB, CBZ, images... */
        d->doc = fz_open_document_with_stream(ctx, lean_string_cstr(magic), stream);
        if (fz_needs_password(ctx, d->doc) &&
            !fz_authenticate_password(ctx, d->doc, lean_string_cstr(password)))
            fz_throw(ctx, FZ_ERROR_ARGUMENT, "Document requires a valid password");
        if (fz_is_document_reflowable(ctx, d->doc)) {
            fz_set_user_css(ctx, *css ? css : NULL);
            fz_layout_document(ctx, d->doc, (float)width, (float)height, (float)em);
        }
        d->count = fz_count_pages(ctx, d->doc);
        if (d->count <= 0) fz_throw(ctx, FZ_ERROR_ARGUMENT, "Document contains no pages");
        /* Deferred: `vellumory_native_finish_bounds' measures them, or reads them. */
        if (!defer) {
            d->bounds = fz_malloc_array(ctx, d->count, fz_rect);
            measure_bounds(ctx, d);
        }
    }
    fz_always(ctx) { fz_drop_stream(ctx, stream); }
    fz_catch(ctx) {
        lean_object *error = caught_failure(ctx);
        finalize(d);
        return error;
    }
    return lean_io_result_mk_ok(lean_alloc_external(document_class, d));
}

lean_obj_res vellumory_native_open(b_lean_obj_arg bytes, b_lean_obj_arg magic,
        b_lean_obj_arg password, lean_obj_arg world) {
    (void)world;
    return open_document(bytes, magic, password, 450, 600, 11, "", 0);
}

lean_obj_res vellumory_native_open_reflowed(b_lean_obj_arg bytes, b_lean_obj_arg magic, b_lean_obj_arg password,
        double width, double height, double em, b_lean_obj_arg css, lean_obj_arg world) {
    (void)world;
    return open_document(bytes, magic, password, width, height, em, lean_string_cstr(css), 0);
}

/* Like `vellumory_native_open_reflowed', with the pages not measured yet:
   `vellumory_native_finish_bounds' must follow before any other use. */
lean_obj_res vellumory_native_open_deferred(b_lean_obj_arg bytes, b_lean_obj_arg magic, b_lean_obj_arg password,
        double width, double height, double em, b_lean_obj_arg css, lean_obj_arg world) {
    (void)world;
    return open_document(bytes, magic, password, width, height, em, lean_string_cstr(css), 1);
}

/* Read all LENGTH bytes of FD into DATA; 0 on failure or a short file. */
static int read_all(int fd, void *data, size_t length) {
    unsigned char *p = data;
    while (length) {
        ssize_t n = read(fd, p, length);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) return 0;
        p += n;
        length -= (size_t)n;
    }
    return 1;
}

static int write_all(int fd, const void *data, size_t length);

/* Write LENGTH bytes of DATA to PATH by way of a pending file renamed into
   place; 0 on failure, which leaves nothing behind. */
static int write_atomically(const char *path, const void *data, size_t length) {
    static unsigned counter;
    char pending[4096];
    if (snprintf(pending, sizeof(pending), "%s.pending-%ld-%u", path, (long)getpid(),
                 __atomic_fetch_add(&counter, 1, __ATOMIC_RELAXED)) >= (int)sizeof(pending))
        return 0;
    int fd = open(pending, O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC, 0600);
    if (fd < 0) return 0;
    int ok = write_all(fd, data, length);
    if (close(fd) != 0) ok = 0;
    if (ok && rename(pending, path) != 0) ok = 0;
    if (!ok) unlink(pending);
    return ok;
}

/* Measure the pages of a document opened deferred, or read their rectangles
   from PATH ("": no cache): a file of exactly one rectangle per page, every
   one usable. Measured, they are written there, whole or not at all; a cache
   that cannot be written never keeps a document from opening. Returns
   whether the pages were measured. */
lean_obj_res vellumory_native_finish_bounds(b_lean_obj_arg handle, b_lean_obj_arg path, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    if (d->bounds) return lean_io_result_mk_ok(lean_box(0));
    fz_context *ctx = d->ctx;
    const char *file = lean_string_cstr(path);
    size_t want = (size_t)d->count * sizeof(fz_rect);
    int measured = 0;
    fz_var(measured);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        d->bounds = fz_malloc_array(ctx, d->count, fz_rect);
        int known = 0;
        int fd = *file ? open(file, O_RDONLY | O_CLOEXEC) : -1;
        if (fd >= 0) {
            struct stat st;
            known = fstat(fd, &st) == 0 && (size_t)st.st_size == want && read_all(fd, d->bounds, want);
            close(fd);
        }
        for (int i = 0; known && i < d->count; ++i) known = valid_bounds(d->bounds[i]);
        if (!known) {
            measure_bounds(ctx, d);
            measured = 1;
            if (*file) (void)write_atomically(file, d->bounds, want);
        }
    }
    fz_always(ctx) pthread_mutex_unlock(&d->doc_lock);
    fz_catch(ctx) {
        fz_free(ctx, d->bounds);
        d->bounds = NULL;
        return caught_failure(ctx);
    }
    return lean_io_result_mk_ok(lean_box(measured));
}

lean_obj_res vellumory_native_sizes(b_lean_obj_arg handle, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    lean_object *result = lean_mk_empty_array();
    for (int i = 0; i < d->count; ++i) {
        lean_object *pair = lean_alloc_ctor(0, 2, 0);
        lean_ctor_set(pair, 0, lean_box_float(d->bounds[i].x1 - d->bounds[i].x0));
        lean_ctor_set(pair, 1, lean_box_float(d->bounds[i].y1 - d->bounds[i].y0));
        result = lean_array_push(result, pair);
    }
    return lean_io_result_mk_ok(result);
}

/* Slow reference for tests: bounds from fully loaded pages. */
lean_obj_res vellumory_native_loaded_sizes(b_lean_obj_arg handle, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    fz_page *page = NULL;
    lean_object *result = lean_mk_empty_array();
    fz_var(page); fz_var(result);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        for (int i = 0; i < d->count; ++i) {
            page = fz_load_page(ctx, d->doc, i);
            fz_rect r = fz_bound_page(ctx, page);
            lean_object *pair = lean_alloc_ctor(0, 2, 0);
            lean_ctor_set(pair, 0, lean_box_float(r.x1 - r.x0));
            lean_ctor_set(pair, 1, lean_box_float(r.y1 - r.y0));
            result = lean_array_push(result, pair);
            fz_drop_page(ctx, page); page = NULL;
        }
    }
    fz_always(ctx) { fz_drop_page(ctx, page); pthread_mutex_unlock(&d->doc_lock); }
    fz_catch(ctx) { lean_dec(result); return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

/* Returns a new reference, building and publishing the list when absent.
   Lists that were aborted or failed are never published. */
static double now_ms(void) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1e3 + t.tv_nsec / 1e6;
}

static fz_display_list *cached_list(fz_context *ctx, v_document *d, int number) {
    fz_display_list *list = NULL;
    pthread_mutex_lock(&d->state_lock);
    for (int i = 0; i < LISTS_MAX; ++i)
        if (d->lists[i].page == number) {
            d->lists[i].used = ++d->clock;
            list = fz_keep_display_list(ctx, d->lists[i].list);
            break;
        }
    pthread_mutex_unlock(&d->state_lock);
    return list;
}

static fz_display_list *display_list(fz_context *ctx, v_document *d, int number, fz_cookie *cookie,
        int *built) {
    *built = 0;
    fz_display_list *list = cached_list(ctx, d, number);
    if (list) return list;

    fz_page *page = NULL;
    fz_device *device = NULL;
    fz_var(page); fz_var(device); fz_var(list);
    pthread_mutex_lock(&d->doc_lock);
    /* Threads queued here for the same page must reuse the list the first
       one built: measured 4 redundant builds of one page after a jump. */
    list = cached_list(ctx, d, number);
    if (list) { pthread_mutex_unlock(&d->doc_lock); return list; }
    fz_try(ctx) {
        page = fz_load_page(ctx, d->doc, number);
        list = fz_new_display_list(ctx, fz_bound_page(ctx, page));
        device = fz_new_list_device(ctx, list);
        fz_run_page(ctx, page, device, fz_identity, cookie);
        fz_close_device(ctx, device);
        if (aborted(cookie)) fz_throw(ctx, FZ_ERROR_ABORT, "cancelled");
    }
    fz_always(ctx) {
        fz_drop_device(ctx, device);
        fz_drop_page(ctx, page);
        pthread_mutex_unlock(&d->doc_lock);
    }
    fz_catch(ctx) { fz_drop_display_list(ctx, list); fz_rethrow(ctx); }

    *built = 1;
    fz_display_list *evicted = NULL;
    pthread_mutex_lock(&d->state_lock);
    int slot = -1;
    for (int i = 0; i < LISTS_MAX && slot < 0; ++i)
        if (d->lists[i].page == number) slot = i;   /* another thread won */
    if (slot < 0) {
        slot = 0;
        for (int i = 1; i < LISTS_MAX; ++i)
            if (d->lists[i].used < d->lists[slot].used) slot = i;
        evicted = d->lists[slot].list;
        d->lists[slot].list = fz_keep_display_list(ctx, list);
        d->lists[slot].page = number;
    }
    d->lists[slot].used = ++d->clock;
    pthread_mutex_unlock(&d->state_lock);
    fz_drop_display_list(ctx, evicted);
    return list;
}

static void composite_overlays(fz_context *ctx, fz_pixmap *pix, fz_matrix matrix,
        const double *quads, size_t count, const uint8_t *kinds,
        const uint8_t *palette, size_t colors, int multiply) {
    fz_device *device = NULL;
    fz_path *path = NULL;
    fz_var(device); fz_var(path);
    fz_try(ctx) {
        device = fz_new_draw_device(ctx, fz_identity, pix);
        fz_rect area = fz_rect_from_irect(fz_pixmap_bbox(ctx, pix));
        for (size_t c = 0; c < colors; ++c) {
            int any = 0;
            for (size_t i = 0; i < count && !any; ++i) any = kinds[i] == c;
            if (!any) continue;
            path = fz_new_path(ctx);
            for (size_t i = 0; i < count; ++i) {
                if (kinds[i] != c) continue;
                const double *q = quads + 8 * i;
                fz_moveto(ctx, path, q[0], q[1]);
                fz_lineto(ctx, path, q[2], q[3]);
                fz_lineto(ctx, path, q[4], q[5]);
                fz_lineto(ctx, path, q[6], q[7]);
                fz_closepath(ctx, path);
            }
            const uint8_t *p = palette + 4 * c;
            float rgb[3] = { p[0] / 255.0f, p[1] / 255.0f, p[2] / 255.0f };
            fz_begin_group(ctx, device, area, NULL, 0, 0,
                           multiply ? FZ_BLEND_MULTIPLY : FZ_BLEND_NORMAL, 1.0f);
            fz_fill_path(ctx, device, path, 0, matrix, fz_device_rgb(ctx), rgb, p[3] / 255.0f,
                         fz_default_color_params);
            fz_end_group(ctx, device);
            fz_drop_path(ctx, path); path = NULL;
        }
        fz_close_device(ctx, device);
    }
    fz_always(ctx) { fz_drop_path(ctx, path); fz_drop_device(ctx, device); }
    fz_catch(ctx) fz_rethrow(ctx);
}

/* Rasterize pixels [left,right) x [top,bottom) of the page scaled to WIDTH
   pixels, as binary PPM. Overlay quads are page points (top-left origin),
   eight doubles each. Theme 1 maps black/white to FG/BG; theme 2 inverts
   luminance while preserving hue. */
/* Write all LENGTH bytes of DATA to FD, retrying short writes; 0 on failure. */
static int write_all(int fd, const void *data, size_t length) {
    const unsigned char *p = data;
    while (length) {
        ssize_t n = write(fd, p, length);
        if (n < 0) { if (errno == EINTR) continue; return 0; }
        p += n;
        length -= (size_t)n;
    }
    return 1;
}

/* Write the P6 raster of PIX (HEADER, then ROWS rows of STRIDE bytes) to PATH:
   to a pending file first, renamed into place, so a reader never sees part of
   one. Written from here, the pixels need no second buffer. */
static void write_ppm_file(fz_context *ctx, const char *path, const char *header, size_t header_length,
                           const fz_pixmap *pix, size_t stride, size_t rows) {
    static unsigned counter;
    char pending[4096];
    if (snprintf(pending, sizeof(pending), "%s.pending-%ld-%u", path, (long)getpid(),
                 __atomic_fetch_add(&counter, 1, __ATOMIC_RELAXED)) >= (int)sizeof(pending))
        fz_throw(ctx, FZ_ERROR_ARGUMENT, "Raster path is too long");
    int fd = open(pending, O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC, 0600);
    if (fd < 0) fz_throw(ctx, FZ_ERROR_SYSTEM, "Cannot create %s: %s", pending, strerror(errno));
    int ok = write_all(fd, header, header_length);
    if (pix->stride == (ptrdiff_t)stride) ok = ok && write_all(fd, pix->samples, stride * rows);
    else for (size_t y = 0; ok && y < rows; ++y) ok = write_all(fd, pix->samples + y * pix->stride, stride);
    int saved = errno;
    if (close(fd) != 0 && ok) { ok = 0; saved = errno; }
    if (ok && rename(pending, path) != 0) { ok = 0; saved = errno; }
    if (!ok) {
        unlink(pending);
        fz_throw(ctx, FZ_ERROR_SYSTEM, "Cannot write %s: %s", path, strerror(saved));
    }
}

/* A raster of the page at WIDTH pixels wide, columns LEFT..RIGHT and rows
   TOP..BOTTOM: as bytes, or written to PATH when it is not empty. */
lean_obj_res vellumory_native_render(b_lean_obj_arg handle, b_lean_obj_arg cookie_handle,
        uint32_t number, uint32_t width, uint32_t left, uint32_t top, uint32_t right, uint32_t bottom,
        b_lean_obj_arg quads, b_lean_obj_arg kinds, b_lean_obj_arg palette, uint8_t multiply,
        uint32_t theme, uint32_t fg, uint32_t bg, b_lean_obj_arg path, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    fz_cookie *cookie = &((v_cookie *)lean_get_external_data(cookie_handle))->cookie;
    size_t overlay_count = lean_sarray_size(kinds), colors = lean_sarray_size(palette) / 4;
    if (number >= (uint32_t)d->count || width < 1 || width > 32768 || bottom <= top ||
        right <= left || right > width || (uint64_t)(right - left) * (bottom - top) > 33554432 ||
        theme > 2 || lean_sarray_size(palette) % 4 || lean_sarray_size(quads) != 8 * overlay_count)
        return failure("Invalid page, render bounds, pixel budget or overlays");
    for (size_t i = 0; i < overlay_count; ++i)
        if (lean_sarray_cptr(kinds)[i] >= colors) return failure("Overlay colour outside palette");
    if (aborted(cookie)) return failure("cancelled");
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    fz_display_list *list = NULL;
    fz_pixmap *pix = NULL;
    fz_device *device = NULL;
    lean_object *result = NULL;
    int built = 0;
    size_t size = 0;
    double started = now_ms(), listed = started, rastered = started;
    fz_var(list); fz_var(pix); fz_var(device); fz_var(result); fz_var(built); fz_var(size);
    fz_var(listed); fz_var(rastered);
    fz_try(ctx) {
        fz_rect rect = d->bounds[number];
        double scale = width / (double)(rect.x1 - rect.x0);
        double total = ceil((rect.y1 - rect.y0) * scale);
        if (!isfinite(total) || total <= 0 || bottom > total || total > 2147483647)
            fz_throw(ctx, FZ_ERROR_ARGUMENT, "Render band exceeds the page");
        list = display_list(ctx, d, number, cookie, &built);
        listed = now_ms();
        fz_irect bounds = {(int)left, (int)top, (int)right, (int)bottom};
        pix = fz_new_pixmap_with_bbox(ctx, fz_device_rgb(ctx), bounds, NULL, 0);
        fz_clear_pixmap_with_value(ctx, pix, 255);
        device = fz_new_draw_device(ctx, fz_identity, pix);
        fz_matrix matrix = fz_concat(fz_translate(-rect.x0, -rect.y0), fz_scale(scale, scale));
        fz_run_display_list(ctx, list, device, matrix, fz_rect_from_irect(bounds), cookie);
        fz_close_device(ctx, device);
        rastered = now_ms();
        if (aborted(cookie)) fz_throw(ctx, FZ_ERROR_ABORT, "cancelled");
        if (theme == 1) fz_tint_pixmap(ctx, pix, (int)fg, (int)bg);
        else if (theme == 2) fz_invert_pixmap_luminance(ctx, pix);
        if (overlay_count)
            composite_overlays(ctx, pix, fz_scale(scale, scale), lean_float_array_cptr(quads),
                               overlay_count, lean_sarray_cptr(kinds), lean_sarray_cptr(palette),
                               colors, multiply);
        char header[64];
        int header_length = snprintf(header, sizeof(header), "P6\n%u %u\n255\n", right - left, bottom - top);
        size_t stride = (size_t)(right - left) * 3, rows = bottom - top;
        size_t length = header_length + stride * rows;
        size = length;
        if (*lean_string_cstr(path)) {
            write_ppm_file(ctx, lean_string_cstr(path), header, (size_t)header_length, pix, stride, rows);
            result = lean_alloc_sarray(1, 0, 0);
        } else {
            result = lean_alloc_sarray(1, length, length);
            uint8_t *out = lean_sarray_cptr(result);
            memcpy(out, header, header_length);
            for (size_t y = 0; y < rows; ++y)
                memcpy(out + header_length + y * stride, pix->samples + y * pix->stride, stride);
        }
    }
    fz_always(ctx) {
        fz_drop_device(ctx, device);
        fz_drop_pixmap(ctx, pix);
        fz_drop_display_list(ctx, list);
    }
    fz_catch(ctx) { if (result) lean_dec(result); return release_failure(d, ctx); }
    release(d, ctx);
    /* list_ms includes waiting for doc_lock; raster_ms is pure replay. */
    return lean_io_result_mk_ok(vellumory_mk_render_result(result, (uint64_t)size, (uint8_t)built,
        listed - started, rastered - listed));
}

lean_obj_res vellumory_native_text(b_lean_obj_arg handle, uint32_t number, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    if (number >= (uint32_t)d->count) return failure("Page outside document");
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    fz_stext_page *text = NULL;
    fz_buffer *buffer = NULL;
    lean_object *result = NULL;
    fz_var(text); fz_var(buffer); fz_var(result);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        text = fz_new_stext_page_from_page_number(ctx, d->doc, number, NULL);
        buffer = fz_new_buffer_from_stext_page(ctx, text);
        unsigned char *data;
        size_t length = fz_buffer_storage(ctx, buffer, &data);
        result = lean_mk_string_from_bytes((char *)data, length);
    }
    fz_always(ctx) {
        fz_drop_buffer(ctx, buffer); fz_drop_stext_page(ctx, text);
        pthread_mutex_unlock(&d->doc_lock);
    }
    fz_catch(ctx) { return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

/* Drop what a document keeps to work again quickly: its display lists, the
   text of recent pages and MuPDF's store (decoded fonts and images). All are
   made again when needed; a render in flight holds its own references, and
   text is only used under doc_lock. */
lean_obj_res vellumory_native_forget_caches(b_lean_obj_arg handle, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_display_list *lists[LISTS_MAX];
    pthread_mutex_lock(&d->state_lock);
    for (int i = 0; i < LISTS_MAX; ++i) {
        lists[i] = d->lists[i].list;
        d->lists[i].list = NULL;
        d->lists[i].page = -1;
    }
    pthread_mutex_unlock(&d->state_lock);
    for (int i = 0; i < LISTS_MAX; ++i) fz_drop_display_list(ctx, lists[i]);
    pthread_mutex_lock(&d->doc_lock);
    for (int i = 0; i < TEXTS_MAX; ++i) {
        fz_drop_stext_page(ctx, d->texts[i]);
        d->texts[i] = NULL;
        d->text_pages[i] = -1;
    }
    pthread_mutex_unlock(&d->doc_lock);
    fz_empty_store(ctx);
    release(d, ctx);
    return lean_io_result_mk_ok(lean_box(0));
}

/* Caller holds doc_lock. */
static fz_stext_page *cached_text(fz_context *ctx, v_document *d, uint32_t number) {
    for (int i = 0; i < TEXTS_MAX; ++i)
        if (d->text_pages[i] == (int)number) return d->texts[i];
    fz_stext_page *text = fz_new_stext_page_from_page_number(ctx, d->doc, number, NULL);
    unsigned slot = d->next_text++ % TEXTS_MAX;
    fz_drop_stext_page(ctx, d->texts[slot]);
    d->texts[slot] = text; d->text_pages[slot] = number;
    return text;
}

extern lean_obj_res vellumory_mk_quad(double, double, double, double, double, double, double, double);
extern lean_obj_res vellumory_mk_search_page(lean_obj_arg hits, uint8_t truncated);
extern lean_obj_res vellumory_mk_selection(lean_obj_arg text, lean_obj_arg quads);

static int finite_quad(fz_quad q) {
    return isfinite(q.ul.x) && isfinite(q.ul.y) && isfinite(q.ur.x) && isfinite(q.ur.y) &&
        isfinite(q.lr.x) && isfinite(q.lr.y) && isfinite(q.ll.x) && isfinite(q.ll.y);
}

static lean_object *box_quad(fz_quad q, fz_rect box) {
    return vellumory_mk_quad(q.ul.x-box.x0, q.ul.y-box.y0, q.ur.x-box.x0, q.ur.y-box.y0,
        q.lr.x-box.x0, q.lr.y-box.y0, q.ll.x-box.x0, q.ll.y-box.y0);
}

/* Logical boundary, including RTL and rotated glyphs. */
static fz_point character_boundary(fz_stext_char *ch, int end) {
    int right = end ^ (ch->bidi & 1);
    fz_point a = right ? ch->quad.ur : ch->quad.ul;
    fz_point b = right ? ch->quad.lr : ch->quad.ll;
    return fz_make_point((a.x+b.x)/2, (a.y+b.y)/2);
}

lean_obj_res vellumory_native_select(b_lean_obj_arg handle, uint32_t number,
        double x0, double y0, double x1, double y1, uint8_t has_start, uint8_t has_end,
        uint32_t mode, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    if (number >= (uint32_t)d->count || mode > 2 ||
        !isfinite(x0) || !isfinite(y0) || !isfinite(x1) || !isfinite(y1) ||
        fabs(x0) > 1e7 || fabs(y0) > 1e7 || fabs(x1) > 1e7 || fabs(y1) > 1e7)
        return failure("Invalid selection page, coordinates or mode");
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    char *copied = NULL;
    fz_quad *quads = NULL;
    lean_object *result = NULL;
    fz_var(copied); fz_var(quads); fz_var(result);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        fz_stext_page *text = cached_text(ctx, d, number);
        fz_stext_char *first = NULL, *last = NULL;
        for (fz_stext_block *block = text->first_block; block; block = block->next) {
            if (block->type != FZ_STEXT_BLOCK_TEXT) continue;
            for (fz_stext_line *line = block->u.t.first_line; line; line = line->next)
                for (fz_stext_char *ch = line->first_char; ch; ch = ch->next) {
                    if (!first) first = ch;
                    last = ch;
                }
        }
        if (!first) {
            result = vellumory_mk_selection(lean_mk_string(""), lean_mk_empty_array());
        } else {
            fz_point a = has_start ? fz_make_point(x0+text->mediabox.x0, y0+text->mediabox.y0)
                                  : character_boundary(first, 0);
            fz_point b = has_end ? fz_make_point(x1+text->mediabox.x0, y1+text->mediabox.y0)
                                : character_boundary(last, 1);
            if (mode) fz_snap_selection(ctx, text, &a, &b, mode);
            int count, capacity = 64;
            for (;;) {
                quads = fz_realloc_array(ctx, quads, capacity, fz_quad);
                count = fz_highlight_selection(ctx, text, a, b, quads, capacity);
                if (count < capacity) break;
                if (capacity == 16384)
                    fz_throw(ctx, FZ_ERROR_LIMIT, "Selection exceeds polygon budget");
                capacity *= 2;
            }
            for (int i = 0; i < count; ++i)
                if (!finite_quad(quads[i])) fz_throw(ctx, FZ_ERROR_FORMAT, "Nonfinite selection polygon");
            copied = fz_copy_selection(ctx, text, a, b, 0);
            if (strlen(copied) > 1048576)
                fz_throw(ctx, FZ_ERROR_LIMIT, "Selected page text exceeds 1 MiB");
            lean_object *boxed = lean_mk_empty_array();
            for (int i = 0; i < count; ++i) boxed = lean_array_push(boxed, box_quad(quads[i], text->mediabox));
            result = vellumory_mk_selection(lean_mk_string(copied), boxed);
        }
    }
    fz_always(ctx) {
        fz_free(ctx, copied); fz_free(ctx, quads);
        pthread_mutex_unlock(&d->doc_lock);
    }
    fz_catch(ctx) { if (result) lean_dec(result); return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

lean_obj_res vellumory_native_search(b_lean_obj_arg handle, uint32_t number,
        b_lean_obj_arg query, uint8_t case_sensitive, uint32_t limit, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    size_t length = lean_string_size(query) - 1;
    if (number >= (uint32_t)d->count || limit < 1 || limit > 4096 ||
        length < 1 || length > 4096 || memchr(lean_string_cstr(query), 0, length))
        return failure("Invalid search page, query or result limit");
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    fz_search *search = NULL;
    lean_object *hits = lean_mk_empty_array();
    uint8_t truncated = 0;
    fz_var(search); fz_var(hits); fz_var(truncated);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        fz_stext_page *text = cached_text(ctx, d, number);
        search = fz_new_search(ctx);
        fz_search_set_options(ctx, search,
            case_sensitive ? FZ_SEARCH_EXACT : FZ_SEARCH_IGNORE_CASE, lean_string_cstr(query));
        fz_feed_search(ctx, search, fz_keep_stext_page(ctx, text), 0);
        unsigned total_quads = 0;
        /* Use the iterator directly. In 1.27.2 the callback convenience wrapper
           reads details->quads[num_quads] and ignores the callback stop value. */
        for (;;) {
            fz_search_result result = fz_search_forwards(ctx, search);
            if (result.reason == FZ_SEARCH_COMPLETE) break;
            if (result.reason == FZ_SEARCH_MORE_INPUT) {
                /* This request searches one page only; no cross-page matches. */
                fz_feed_search(ctx, search, NULL, result.u.more_input.seq_needed);
                continue;
            }
            fz_search_result_details *details = result.u.match.result;
            if (details->num_quads <= 0) continue;
            if (lean_array_size(hits) == limit) { truncated = 1; break; }
            if ((unsigned)details->num_quads > 16384 - total_quads)
                fz_throw(ctx, FZ_ERROR_LIMIT, "Search polygons exceed per-page budget");
            total_quads += details->num_quads;
            /* Validate before allocating a Lean array that an exception could leak. */
            for (int i = 0; i < details->num_quads; ++i) {
                fz_quad q = details->quads[i].quad;
                if (!finite_quad(q))
                    fz_throw(ctx, FZ_ERROR_FORMAT, "Nonfinite text polygon");
            }
            lean_object *quads = lean_mk_empty_array();
            for (int i = 0; i < details->num_quads; ++i) {
                fz_quad q = details->quads[i].quad;
                quads = lean_array_push(quads, box_quad(q, text->mediabox));
            }
            hits = lean_array_push(hits, quads);
        }
    }
    fz_always(ctx) { fz_drop_search(ctx, search); pthread_mutex_unlock(&d->doc_lock); }
    fz_catch(ctx) { lean_dec(hits); return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(vellumory_mk_search_page(hits, truncated));
}

extern lean_obj_res vellumory_mk_outline_entry(lean_obj_arg title, lean_obj_arg uri,
    uint32_t depth, uint32_t page_plus_one, double x, double y, uint8_t expanded);

lean_obj_res vellumory_native_outline(b_lean_obj_arg handle, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    fz_outline *outline = NULL;
    lean_object *result = lean_mk_empty_array();
    fz_var(outline); fz_var(result);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        outline = fz_load_outline(ctx, d->doc);
        /* Iterative preorder traversal: do not recurse on untrusted depth. */
        fz_outline *parents[256];
        unsigned depth = 0, count = 0;
        fz_outline *item = outline;
        while (item) {
            if (++count > 100000)
                fz_throw(ctx, FZ_ERROR_LIMIT, "Outline exceeds 100000 entries");
            int page = item->page.page < 0 ? -1 : fz_page_number_from_location(ctx, d->doc, item->page);
            if (page >= d->count) page = -1;
            lean_object *entry = vellumory_mk_outline_entry(
                lean_mk_string(item->title ? item->title : ""),
                lean_mk_string(item->uri ? item->uri : ""), depth,
                page < 0 ? 0 : (uint32_t)page + 1,
                isfinite(item->x) ? item->x : 0, isfinite(item->y) ? item->y : 0,
                item->is_open);
            result = lean_array_push(result, entry);
            if (item->down) {
                if (depth == 255)
                    fz_throw(ctx, FZ_ERROR_LIMIT, "Outline exceeds 256 levels");
                parents[depth++] = item->next;
                item = item->down;
            } else {
                item = item->next;
                while (!item && depth) item = parents[--depth];
            }
        }
    }
    fz_always(ctx) { fz_drop_outline(ctx, outline); pthread_mutex_unlock(&d->doc_lock); }
    fz_catch(ctx) { lean_dec(result); return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

lean_obj_res vellumory_native_labels(b_lean_obj_arg handle, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    fz_page *page = NULL;
    lean_object *result = lean_mk_empty_array();
    fz_var(page); fz_var(result);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        pdf_document *pdf = pdf_document_from_fz_document(ctx, d->doc);
        int has_labels = pdf && pdf_dict_getp(ctx, pdf_trailer(ctx, pdf), "Root/PageLabels") != NULL;
        for (int i = 0; i < d->count; ++i) {
            char label[4096];
            /* 1.27.2's PDF fallback uses an incomplete %z format specifier.
               Supply physical numbers only when there is no PageLabels tree;
               deliberately empty labels in an existing tree remain empty. */
            /* pdf_page_label reads the number tree without loading the page:
               756 labels took ~175 ms through fz_load_page, holding doc_lock. */
            if (has_labels) pdf_page_label(ctx, pdf, i, label, sizeof(label));
            else snprintf(label, sizeof(label), "%d", i + 1);
            if (strlen(label) >= sizeof(label) - 1)
                fz_throw(ctx, FZ_ERROR_LIMIT, "Page label exceeds 4094 bytes");
            result = lean_array_push(result, lean_mk_string(label));
        }
    }
    fz_always(ctx) { fz_drop_page(ctx, page); pthread_mutex_unlock(&d->doc_lock); }
    fz_catch(ctx) { lean_dec(result); return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

/* OpenSSL dispatches to SHA-NI or AVX2 where available: measured 1.96 GB/s
   versus about 0.3 GB/s for MuPDF's portable fz_sha256 on this host. */
lean_obj_res vellumory_native_sha256(b_lean_obj_arg bytes, lean_obj_arg world) {
    (void)world;
    unsigned char digest[EVP_MAX_MD_SIZE];
    unsigned int length = 0;
    char hex[65];
    if (!EVP_Digest(lean_sarray_cptr(bytes), lean_sarray_size(bytes), digest, &length, EVP_sha256(), NULL)
        || length != 32)
        return failure("SHA-256 failed");
    for (int i = 0; i < 32; ++i) snprintf(hex + 2*i, 3, "%02x", digest[i]);
    return lean_io_result_mk_ok(lean_mk_string(hex));
}

lean_obj_res vellumory_native_version(lean_obj_arg world) {
    (void)world;
    return lean_io_result_mk_ok(lean_mk_string(FZ_VERSION));
}

#include <unistd.h>
lean_obj_res vellumory_native_cores(lean_obj_arg world) {
    (void)world;
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    return lean_io_result_mk_ok(lean_box_uint32(n < 1 ? 1 : (uint32_t)n));
}

#ifdef __GLIBC__
#include <malloc.h>
#endif
#ifdef LEAN_MIMALLOC
#include <lean/mimalloc.h>
#endif
/* glibc keeps freed memory in its arenas for reuse, one arena per rendering
   thread; malloc_trim gives what is free back to the system. Lean's objects
   come from mimalloc, which is asked the same. Elsewhere the allocator decides
   for itself. */
#include <signal.h>
/* Whether a process with PID exists, whoever owns it. */
lean_obj_res vellumory_native_process_alive(uint32_t pid, lean_obj_arg world) {
    (void)world;
    int alive = pid > 0 && pid <= INT32_MAX && (kill((pid_t)pid, 0) == 0 || errno == EPERM);
    return lean_io_result_mk_ok(lean_box(alive));
}

lean_obj_res vellumory_native_trim_memory(lean_obj_arg world) {
    (void)world;
#ifdef LEAN_MIMALLOC
    mi_collect(true);
#endif
#ifdef __GLIBC__
    return lean_io_result_mk_ok(lean_box(malloc_trim(0) ? 1 : 0));
#else
    return lean_io_result_mk_ok(lean_box(0));
#endif
}

extern lean_obj_res vellumory_mk_link(double x0, double y0, double x1, double y1, lean_obj_arg uri,
    uint8_t external, uint32_t page_plus_one, double x, double y);

/* Links of one page in top-left page points relative to the page bounds.
   Internal destinations are resolved to a physical page and point. */
lean_obj_res vellumory_native_links(b_lean_obj_arg handle, uint32_t number, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    if (number >= (uint32_t)d->count) return failure("Page outside document");
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    fz_page *page = NULL;
    fz_link *links = NULL;
    lean_object *result = lean_mk_empty_array();
    fz_var(page); fz_var(links); fz_var(result);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        fz_rect box = d->bounds[number];
        page = fz_load_page(ctx, d->doc, number);
        links = fz_load_links(ctx, page);
        unsigned count = 0;
        for (fz_link *link = links; link; link = link->next) {
            if (++count > 100000) fz_throw(ctx, FZ_ERROR_LIMIT, "Page has more than 100000 links");
            const char *uri = link->uri ? link->uri : "";
            int external = fz_is_external_link(ctx, uri);
            int target = -1;
            float x = 0, y = 0;
            if (!external && *uri) {
                fz_location loc = fz_resolve_link(ctx, d->doc, uri, &x, &y);
                if (loc.page >= 0) target = fz_page_number_from_location(ctx, d->doc, loc);
                if (target >= d->count) target = -1;
            }
            fz_rect r = link->rect;
            if (!isfinite(r.x0) || !isfinite(r.y0) || !isfinite(r.x1) || !isfinite(r.y1)) continue;
            result = lean_array_push(result, vellumory_mk_link(
                r.x0 - box.x0, r.y0 - box.y0, r.x1 - box.x0, r.y1 - box.y0, lean_mk_string(uri),
                external, target < 0 ? 0 : (uint32_t)target + 1,
                isfinite(x) ? x : 0, isfinite(y) ? y : 0));
        }
    }
    fz_always(ctx) {
        fz_drop_link(ctx, links); fz_drop_page(ctx, page);
        pthread_mutex_unlock(&d->doc_lock);
    }
    fz_catch(ctx) { lean_dec(result); return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

#include <sys/stat.h>
/* Permission bits of a file, so an atomic replacement keeps them. */
lean_obj_res vellumory_native_file_mode(b_lean_obj_arg path, lean_obj_arg world) {
    (void)world;
    struct stat st;
    if (stat(lean_string_cstr(path), &st) != 0) return failure("Cannot stat file");
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)(st.st_mode & 07777)));
}

lean_obj_res vellumory_native_set_file_mode(b_lean_obj_arg path, uint32_t mode, lean_obj_arg world) {
    (void)world;
    if (chmod(lean_string_cstr(path), (mode_t)(mode & 07777)) != 0) return failure("Cannot set file mode");
    return lean_io_result_mk_ok(lean_box(0));
}

/* ---- Document inspection: one JSON report built here, parsed by Lean. ---- */

typedef struct { fz_buffer *out; int depth; int first[64]; } jb;

static void jb_sep(fz_context *ctx, jb *b) {
    if (b->depth > 0) {
        if (!b->first[b->depth]) fz_append_byte(ctx, b->out, ',');
        b->first[b->depth] = 0;
    }
}
static void jb_raw_string(fz_context *ctx, jb *b, const char *s, size_t n) {
    static const char hex[] = "0123456789abcdef";
    fz_append_byte(ctx, b->out, '"');
    for (size_t i = 0; i < n; ++i) {
        unsigned char c = (unsigned char)s[i];
        if (c == '"' || c == '\\') { fz_append_byte(ctx, b->out, '\\'); fz_append_byte(ctx, b->out, c); }
        else if (c == '\n') fz_append_string(ctx, b->out, "\\n");
        else if (c == '\r') fz_append_string(ctx, b->out, "\\r");
        else if (c == '\t') fz_append_string(ctx, b->out, "\\t");
        else if (c < 0x20) {
            fz_append_string(ctx, b->out, "\\u00");
            fz_append_byte(ctx, b->out, hex[c >> 4]); fz_append_byte(ctx, b->out, hex[c & 15]);
        } else fz_append_byte(ctx, b->out, c);
    }
    fz_append_byte(ctx, b->out, '"');
}
static void jb_open(fz_context *ctx, jb *b, char c) {
    jb_sep(ctx, b);
    fz_append_byte(ctx, b->out, c);
    if (b->depth < 63) b->first[++b->depth] = 1;
}
static void jb_close(fz_context *ctx, jb *b, char c) { fz_append_byte(ctx, b->out, c); --b->depth; }
static void jb_key(fz_context *ctx, jb *b, const char *k) {
    jb_sep(ctx, b); jb_raw_string(ctx, b, k, strlen(k)); fz_append_byte(ctx, b->out, ':');
    b->first[b->depth] = 1;   /* the value follows without a comma */
}
static void jb_str(fz_context *ctx, jb *b, const char *s) {
    jb_sep(ctx, b);
    if (!s) { fz_append_string(ctx, b->out, "null"); return; }
    /* PDF text may be invalid UTF-8; replace bad bytes so the report stays valid JSON. */
    size_t n = strlen(s);
    char *clean = fz_malloc(ctx, n + 1);
    size_t j = 0;
    for (size_t i = 0; i < n;) {
        int rune; int len = fz_chartorune(&rune, s + i);
        if (rune == FZ_REPLACEMENT_CHARACTER && (unsigned char)s[i] >= 0x80) { clean[j++] = '?'; i += 1; }
        else { memcpy(clean + j, s + i, len); j += len; i += len; }
    }
    jb_raw_string(ctx, b, clean, j);
    fz_free(ctx, clean);
}
/* fz_append_printf has no %lld; libc formats integers exactly. */
static void jb_int(fz_context *ctx, jb *b, int64_t v) {
    char digits[32];
    jb_sep(ctx, b);
    snprintf(digits, sizeof(digits), "%lld", (long long)v);
    fz_append_string(ctx, b->out, digits);
}
static void jb_num(fz_context *ctx, jb *b, double v) {
    jb_sep(ctx, b);
    char digits[40];
    if (!isfinite(v)) { fz_append_string(ctx, b->out, "null"); return; }
    snprintf(digits, sizeof(digits), "%.17g", v);
    fz_append_string(ctx, b->out, digits);
}
static void jb_bool(fz_context *ctx, jb *b, int v) { jb_sep(ctx, b); fz_append_string(ctx, b->out, v ? "true" : "false"); }
static void jb_kstr(fz_context *ctx, jb *b, const char *k, const char *v) { jb_key(ctx, b, k); jb_str(ctx, b, v); }
static void jb_kint(fz_context *ctx, jb *b, const char *k, int64_t v) { jb_key(ctx, b, k); jb_int(ctx, b, v); }
static void jb_kbool(fz_context *ctx, jb *b, const char *k, int v) { jb_key(ctx, b, k); jb_bool(ctx, b, v); }

static void jb_rect(fz_context *ctx, jb *b, const char *k, pdf_obj *obj) {
    jb_key(ctx, b, k);
    if (!pdf_is_array(ctx, obj)) { jb_sep(ctx, b); fz_append_string(ctx, b->out, "null"); return; }
    fz_rect r = pdf_to_rect(ctx, obj);
    jb_open(ctx, b, '[');
    jb_num(ctx, b, r.x0); jb_num(ctx, b, r.y0); jb_num(ctx, b, r.x1); jb_num(ctx, b, r.y1);
    jb_close(ctx, b, ']');
}

static int font_embedded(fz_context *ctx, pdf_obj *font) {
    pdf_obj *desc = pdf_dict_get(ctx, font, PDF_NAME(FontDescriptor));
    if (!desc) {
        pdf_obj *descendants = pdf_dict_get(ctx, font, PDF_NAME(DescendantFonts));
        if (pdf_array_len(ctx, descendants) > 0)
            desc = pdf_dict_get(ctx, pdf_array_get(ctx, descendants, 0), PDF_NAME(FontDescriptor));
    }
    return desc && (pdf_dict_get(ctx, desc, PDF_NAME(FontFile)) || pdf_dict_get(ctx, desc, PDF_NAME(FontFile2))
                    || pdf_dict_get(ctx, desc, PDF_NAME(FontFile3)));
}

/* Each helper owns its exception scope, keeping setjmp out of the caller's loops. */
static pdf_obj *load_or_null(fz_context *ctx, pdf_document *pdf, int num) {
    pdf_obj *obj = NULL;
    fz_var(obj);
    fz_try(ctx) obj = pdf_load_object(ctx, pdf, num);
    fz_catch(ctx) { fz_report_error(ctx); obj = NULL; }
    return obj;
}

static void jb_stream_text(fz_context *ctx, jb *b, pdf_obj *stream, size_t cap) {
    fz_buffer *packet = pdf_load_stream(ctx, stream);
    char *copy = NULL;
    fz_var(copy);
    fz_try(ctx) {
        unsigned char *data; size_t n = fz_buffer_storage(ctx, packet, &data);
        if (n > cap) n = cap;
        copy = fz_malloc(ctx, n + 1);
        memcpy(copy, data, n); copy[n] = 0;
        jb_str(ctx, b, copy);
    }
    fz_always(ctx) { fz_free(ctx, copy); fz_drop_buffer(ctx, packet); }
    fz_catch(ctx) fz_rethrow(ctx);
}

lean_obj_res vellumory_native_info(b_lean_obj_arg handle, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    jb b = {0};
    pdf_obj *tree = NULL;
    lean_object *result = NULL;
    fz_var(tree); fz_var(result);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        b.out = fz_new_buffer(ctx, 4096);
        char text[4096];
        jb_open(ctx, &b, '{');
        static const char *meta[][2] = {
            {"format", FZ_META_FORMAT}, {"encryption", FZ_META_ENCRYPTION},
            {"title", FZ_META_INFO_TITLE}, {"author", FZ_META_INFO_AUTHOR},
            {"subject", FZ_META_INFO_SUBJECT}, {"keywords", FZ_META_INFO_KEYWORDS},
            {"creator", FZ_META_INFO_CREATOR}, {"producer", FZ_META_INFO_PRODUCER},
            {"created", FZ_META_INFO_CREATIONDATE}, {"modified", FZ_META_INFO_MODIFICATIONDATE}};
        /* PDF 2.0 deprecates the Info dictionary: fields it lacks come from XMP. */
        char fallback[8][1024];
        bool from_xmp[8] = {0};
        pdf_document *meta_pdf = pdf_document_from_fz_document(ctx, d->doc);
        bool have[8] = {0};
        if (meta_pdf) xmp_info_fallback(ctx, meta_pdf, fallback, have);
        for (size_t i = 0; i < sizeof(meta) / sizeof(meta[0]); ++i) {
            int n = fz_lookup_metadata(ctx, d->doc, meta[i][1], text, sizeof(text));
            if (n < 0 && i >= 2 && have[i - 2]) { from_xmp[i - 2] = true; jb_kstr(ctx, &b, meta[i][0], fallback[i - 2]); }
            else jb_kstr(ctx, &b, meta[i][0], n >= 0 ? text : NULL);
        }
        jb_key(ctx, &b, "from_xmp");
        jb_open(ctx, &b, '[');
        for (size_t i = 2; i < sizeof(meta) / sizeof(meta[0]); ++i)
            if (from_xmp[i - 2]) jb_str(ctx, &b, meta[i][0]);
        jb_close(ctx, &b, ']');
        jb_kint(ctx, &b, "pages", d->count);
        jb_kint(ctx, &b, "bytes", (int64_t)d->source->len);
        pdf_document *pdf = pdf_document_from_fz_document(ctx, d->doc);
        jb_kbool(ctx, &b, "pdf", pdf != NULL);
        if (pdf) {
            pdf_obj *trailer = pdf_trailer(ctx, pdf);
            pdf_obj *root = pdf_dict_get(ctx, trailer, PDF_NAME(Root));
            int version = pdf_version(ctx, pdf);
            snprintf(text, sizeof(text), "%d.%d", version / 10, version % 10);
            jb_kstr(ctx, &b, "version", text);
            jb_kint(ctx, &b, "objects", pdf_xref_len(ctx, pdf));
            jb_kint(ctx, &b, "revisions", pdf_count_versions(ctx, pdf));
            jb_kbool(ctx, &b, "linearized", pdf_doc_was_linearized(ctx, pdf));
            jb_kbool(ctx, &b, "repaired", pdf_was_repaired(ctx, pdf));
            int perms = pdf_document_permissions(ctx, pdf);
            jb_key(ctx, &b, "permissions");
            jb_open(ctx, &b, '{');
            jb_kbool(ctx, &b, "print", perms & PDF_PERM_PRINT);
            jb_kbool(ctx, &b, "modify", perms & PDF_PERM_MODIFY);
            jb_kbool(ctx, &b, "copy", perms & PDF_PERM_COPY);
            jb_kbool(ctx, &b, "annotate", perms & PDF_PERM_ANNOTATE);
            jb_kbool(ctx, &b, "forms", perms & PDF_PERM_FORM);
            jb_kbool(ctx, &b, "assemble", perms & PDF_PERM_ASSEMBLE);
            jb_kbool(ctx, &b, "print_high_quality", perms & PDF_PERM_PRINT_HQ);
            jb_close(ctx, &b, '}');
            jb_kstr(ctx, &b, "language", pdf_to_text_string(ctx, pdf_dict_get(ctx, root, PDF_NAME(Lang))));
            pdf_obj *mark = pdf_dict_gets(ctx, root, "MarkInfo");
            jb_kbool(ctx, &b, "tagged", pdf_to_bool(ctx, pdf_dict_gets(ctx, mark, "Marked")));
            jb_kbool(ctx, &b, "structure_tree", pdf_dict_get(ctx, root, PDF_NAME(StructTreeRoot)) != NULL);
            jb_kstr(ctx, &b, "page_layout", pdf_to_name(ctx, pdf_dict_gets(ctx, root, "PageLayout")));
            jb_kstr(ctx, &b, "page_mode", pdf_to_name(ctx, pdf_dict_get(ctx, root, PDF_NAME(PageMode))));
            pdf_obj *prefs = pdf_dict_gets(ctx, root, "ViewerPreferences");
            jb_kbool(ctx, &b, "display_title", pdf_to_bool(ctx, pdf_dict_gets(ctx, prefs, "DisplayDocTitle")));
            jb_kbool(ctx, &b, "open_action", pdf_dict_gets(ctx, root, "OpenAction") != NULL);
            pdf_obj *names = pdf_dict_get(ctx, root, PDF_NAME(Names));
            jb_kbool(ctx, &b, "javascript", pdf_dict_get(ctx, names, PDF_NAME(JavaScript)) != NULL);
            pdf_obj *form = pdf_dict_get(ctx, root, PDF_NAME(AcroForm));
            jb_kint(ctx, &b, "form_fields", pdf_array_len(ctx, pdf_dict_get(ctx, form, PDF_NAME(Fields))));
            jb_kbool(ctx, &b, "xfa", pdf_dict_get(ctx, form, PDF_NAME(XFA)) != NULL);
            jb_kint(ctx, &b, "outline_entries", pdf_dict_get(ctx, root, PDF_NAME(Outlines)) ? 1 : 0);
            jb_kbool(ctx, &b, "page_labels", pdf_dict_get(ctx, root, PDF_NAME(PageLabels)) != NULL);
            /* Output intents: required by PDF/A for device colour. */
            pdf_obj *intents = pdf_dict_get(ctx, root, PDF_NAME(OutputIntents));
            jb_key(ctx, &b, "output_intents");
            jb_open(ctx, &b, '[');
            for (int i = 0; i < pdf_array_len(ctx, intents) && i < 32; ++i) {
                pdf_obj *intent = pdf_array_get(ctx, intents, i);
                jb_open(ctx, &b, '{');
                jb_kstr(ctx, &b, "subtype", pdf_to_name(ctx, pdf_dict_get(ctx, intent, PDF_NAME(S))));
                jb_kstr(ctx, &b, "condition", pdf_to_text_string(ctx,
                    pdf_dict_get(ctx, intent, PDF_NAME(OutputConditionIdentifier))));
                jb_kbool(ctx, &b, "profile", pdf_dict_get(ctx, intent, PDF_NAME(DestOutputProfile)) != NULL);
                jb_close(ctx, &b, '}');
            }
            jb_close(ctx, &b, ']');
            /* XMP packet, verbatim (capped): Lisp reads pdfaid/pdfuaid claims. */
            pdf_obj *xmp = pdf_dict_get(ctx, root, PDF_NAME(Metadata));
            jb_key(ctx, &b, "xmp");
            if (xmp && pdf_is_stream(ctx, xmp)) jb_stream_text(ctx, &b, xmp, 1048576);
            else { jb_sep(ctx, &b); fz_append_string(ctx, b.out, "null"); }
            /* Fonts and images across all objects (no page loading). */
            int fonts = 0, embedded = 0, unicode = 0, images = 0;
            jb_key(ctx, &b, "fonts");
            jb_open(ctx, &b, '[');
            int len = pdf_xref_len(ctx, pdf);
            for (int num = 1; num < len; ++num) {
                /* A full scan of a large file takes hundreds of milliseconds:
                   let renderers take the document lock in between. */
                if ((num & 1023) == 0) {
                    pthread_mutex_unlock(&d->doc_lock);
                    sched_yield();
                    pthread_mutex_lock(&d->doc_lock);
                }
                pdf_obj *obj = load_or_null(ctx, pdf, num);
                if (!obj) continue;
                if (pdf_is_dict(ctx, obj)) {
                    pdf_obj *type = pdf_dict_get(ctx, obj, PDF_NAME(Type));
                    pdf_obj *subtype = pdf_dict_get(ctx, obj, PDF_NAME(Subtype));
                    if (pdf_name_eq(ctx, type, PDF_NAME(Font)) && !pdf_name_eq(ctx, subtype, PDF_NAME(CIDFontType0))
                        && !pdf_name_eq(ctx, subtype, PDF_NAME(CIDFontType2))) {
                        int e = font_embedded(ctx, obj), u = pdf_dict_get(ctx, obj, PDF_NAME(ToUnicode)) != NULL;
                        fonts++; embedded += e; unicode += u;
                        if (fonts <= 500) {
                            jb_open(ctx, &b, '{');
                            jb_kint(ctx, &b, "object", num);
                            jb_kstr(ctx, &b, "name", pdf_to_name(ctx, pdf_dict_get(ctx, obj, PDF_NAME(BaseFont))));
                            jb_kstr(ctx, &b, "type", pdf_to_name(ctx, subtype));
                            jb_kbool(ctx, &b, "embedded", e);
                            jb_kbool(ctx, &b, "to_unicode", u);
                            jb_close(ctx, &b, '}');
                        }
                    } else if (pdf_name_eq(ctx, subtype, PDF_NAME(Image)) && pdf_is_stream(ctx, obj)) images++;
                }
                pdf_drop_obj(ctx, obj);
            }
            jb_close(ctx, &b, ']');
            jb_kint(ctx, &b, "font_count", fonts);
            jb_kint(ctx, &b, "fonts_embedded", embedded);
            jb_kint(ctx, &b, "fonts_with_unicode", unicode);
            jb_kint(ctx, &b, "images", images);
            /* Annotations by subtype, from page dictionaries only. */
            int annots = 0, links = 0, widgets = 0, markup = 0, transparency = 0;
            for (int i = 0; i < d->count; ++i) {
                pdf_obj *page = pdf_lookup_page_obj(ctx, pdf, i);
                pdf_obj *list = pdf_dict_get(ctx, page, PDF_NAME(Annots));
                pdf_obj *group = pdf_dict_get(ctx, page, PDF_NAME(Group));
                if (pdf_name_eq(ctx, pdf_dict_get(ctx, group, PDF_NAME(S)), PDF_NAME(Transparency))) transparency++;
                for (int j = 0; j < pdf_array_len(ctx, list); ++j) {
                    pdf_obj *s = pdf_dict_get(ctx, pdf_array_get(ctx, list, j), PDF_NAME(Subtype));
                    annots++;
                    if (pdf_name_eq(ctx, s, PDF_NAME(Link))) links++;
                    else if (pdf_name_eq(ctx, s, PDF_NAME(Widget))) widgets++;
                    else markup++;
                }
            }
            jb_kint(ctx, &b, "annotations", annots);
            jb_kint(ctx, &b, "links", links);
            jb_kint(ctx, &b, "widgets", widgets);
            jb_kint(ctx, &b, "markup_annotations", markup);
            jb_kint(ctx, &b, "transparency_pages", transparency);
            /* Embedded files (the EmbeddedFiles name tree, in its order). */
            tree = pdf_load_name_tree(ctx, pdf, PDF_NAME(EmbeddedFiles));
            /* Associated files of the document (PDF/A-3, -4f): listed in the catalog's /AF. */
            pdf_obj *associated = pdf_dict_get(ctx, pdf_dict_get(ctx, pdf_trailer(ctx, pdf), PDF_NAME(Root)), PDF_NAME(AF));
            jb_key(ctx, &b, "attachments");
            jb_open(ctx, &b, '[');
            for (int i = 0; i < pdf_dict_len(ctx, tree); ++i) {
                pdf_obj *fs = pdf_dict_get_val(ctx, tree, i);
                if (!pdf_is_embedded_file(ctx, fs)) continue;
                pdf_filespec_params params;
                pdf_get_filespec_params(ctx, fs, &params);
                jb_open(ctx, &b, '{');
                jb_kint(ctx, &b, "index", i);
                pdf_obj *key = pdf_dict_get_key(ctx, tree, i);
                jb_kstr(ctx, &b, "key", pdf_is_name(ctx, key) ? pdf_to_name(ctx, key) : pdf_to_text_string(ctx, key));
                jb_kstr(ctx, &b, "filename", params.filename);
                jb_kstr(ctx, &b, "mimetype", params.mimetype);
                jb_kint(ctx, &b, "size", params.size);
                jb_kint(ctx, &b, "created", params.created);
                jb_kint(ctx, &b, "modified", params.modified);
                jb_kstr(ctx, &b, "description", pdf_to_text_string(ctx, pdf_dict_get(ctx, fs, PDF_NAME(Desc))));
                jb_kstr(ctx, &b, "relationship", pdf_to_name(ctx, pdf_dict_get(ctx, fs, PDF_NAME(AFRelationship))));
                jb_kbool(ctx, &b, "associated", pdf_is_array(ctx, associated) && pdf_array_contains(ctx, associated, fs));
                jb_close(ctx, &b, '}');
            }
            jb_close(ctx, &b, ']');
        }
        jb_close(ctx, &b, '}');
        unsigned char *data;
        size_t n = fz_buffer_storage(ctx, b.out, &data);
        result = lean_mk_string_from_bytes((char *)data, n);
    }
    fz_always(ctx) {
        pdf_drop_obj(ctx, tree);
        fz_drop_buffer(ctx, b.out);
        pthread_mutex_unlock(&d->doc_lock);
    }
    fz_catch(ctx) { return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

/* Page boxes, rotation and resources of one page. */
lean_obj_res vellumory_native_page_info(b_lean_obj_arg handle, uint32_t number, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    if (number >= (uint32_t)d->count) return failure("Page outside document");
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    jb b = {0};
    lean_object *result = NULL;
    fz_var(result);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        b.out = fz_new_buffer(ctx, 1024);
        jb_open(ctx, &b, '{');
        jb_kint(ctx, &b, "page", number);
        pdf_document *pdf = pdf_document_from_fz_document(ctx, d->doc);
        if (pdf) {
            pdf_obj *page = pdf_lookup_page_obj(ctx, pdf, number);
            jb_kint(ctx, &b, "object", pdf_to_num(ctx, page));
            jb_rect(ctx, &b, "media_box", pdf_dict_get_inheritable(ctx, page, PDF_NAME(MediaBox)));
            jb_rect(ctx, &b, "crop_box", pdf_dict_get_inheritable(ctx, page, PDF_NAME(CropBox)));
            jb_rect(ctx, &b, "bleed_box", pdf_dict_get(ctx, page, PDF_NAME(BleedBox)));
            jb_rect(ctx, &b, "trim_box", pdf_dict_get(ctx, page, PDF_NAME(TrimBox)));
            jb_rect(ctx, &b, "art_box", pdf_dict_get(ctx, page, PDF_NAME(ArtBox)));
            jb_kint(ctx, &b, "rotate", pdf_to_int(ctx, pdf_dict_get_inheritable(ctx, page, PDF_NAME(Rotate))));
            jb_key(ctx, &b, "user_unit");
            jb_num(ctx, &b, pdf_dict_get_real_default(ctx, page, PDF_NAME(UserUnit), 1));
            /* Presentations: /Dur seconds before advancing (null: never), the /Trans style. */
            pdf_obj *dur = pdf_dict_get(ctx, page, PDF_NAME(Dur));
            jb_key(ctx, &b, "duration");
            if (pdf_is_number(ctx, dur)) jb_num(ctx, &b, pdf_to_real(ctx, dur));
            else { jb_sep(ctx, &b); fz_append_string(ctx, b.out, "null"); }
            pdf_obj *trans = pdf_dict_get(ctx, page, PDF_NAME(Trans));
            /* The /Trans dictionary with ISO 32000 defaults (S R, D 1, Dm H, M I, Di 0, SS 1, B false), or null. */
            jb_key(ctx, &b, "transition");
            if (pdf_is_dict(ctx, trans)) {
                const char *style = pdf_to_name(ctx, pdf_dict_gets(ctx, trans, "S"));
                const char *dimension = pdf_to_name(ctx, pdf_dict_gets(ctx, trans, "Dm"));
                const char *motion = pdf_to_name(ctx, pdf_dict_gets(ctx, trans, "M"));
                pdf_obj *direction = pdf_dict_gets(ctx, trans, "Di");
                jb_open(ctx, &b, '{');
                jb_kstr(ctx, &b, "style", *style ? style : "R");
                jb_key(ctx, &b, "duration");
                jb_num(ctx, &b, pdf_is_number(ctx, pdf_dict_gets(ctx, trans, "D"))
                                ? pdf_to_real(ctx, pdf_dict_gets(ctx, trans, "D")) : 1);
                jb_kstr(ctx, &b, "dimension", *dimension ? dimension : "H");
                jb_kstr(ctx, &b, "motion", *motion ? motion : "I");
                jb_key(ctx, &b, "direction");
                if (pdf_is_name(ctx, direction)) jb_str(ctx, &b, pdf_to_name(ctx, direction));
                else jb_num(ctx, &b, pdf_is_number(ctx, direction) ? pdf_to_real(ctx, direction) : 0);
                /* Fly only: the scale changes start or end at, and whether the flown area is opaque. */
                pdf_obj *scale = pdf_dict_gets(ctx, trans, "SS");
                jb_key(ctx, &b, "scale");
                jb_num(ctx, &b, pdf_is_number(ctx, scale) ? pdf_to_real(ctx, scale) : 1);
                jb_kbool(ctx, &b, "opaque", pdf_to_bool(ctx, pdf_dict_gets(ctx, trans, "B")));
                jb_close(ctx, &b, '}');
            } else { jb_sep(ctx, &b); fz_append_string(ctx, b.out, "null"); }
            pdf_obj *resources = pdf_dict_get_inheritable(ctx, page, PDF_NAME(Resources));
            pdf_obj *fonts = pdf_dict_get(ctx, resources, PDF_NAME(Font));
            jb_key(ctx, &b, "fonts");
            jb_open(ctx, &b, '[');
            for (int i = 0; i < pdf_dict_len(ctx, fonts) && i < 256; ++i) {
                pdf_obj *font = pdf_dict_get_val(ctx, fonts, i);
                jb_open(ctx, &b, '{');
                jb_kstr(ctx, &b, "resource", pdf_to_name(ctx, pdf_dict_get_key(ctx, fonts, i)));
                jb_kstr(ctx, &b, "name", pdf_to_name(ctx, pdf_dict_get(ctx, font, PDF_NAME(BaseFont))));
                jb_kstr(ctx, &b, "type", pdf_to_name(ctx, pdf_dict_get(ctx, font, PDF_NAME(Subtype))));
                jb_kbool(ctx, &b, "embedded", font_embedded(ctx, font));
                jb_close(ctx, &b, '}');
            }
            jb_close(ctx, &b, ']');
            pdf_obj *xobjects = pdf_dict_get(ctx, resources, PDF_NAME(XObject));
            jb_key(ctx, &b, "images");
            jb_open(ctx, &b, '[');
            for (int i = 0; i < pdf_dict_len(ctx, xobjects) && i < 256; ++i) {
                pdf_obj *x = pdf_dict_get_val(ctx, xobjects, i);
                if (!pdf_name_eq(ctx, pdf_dict_get(ctx, x, PDF_NAME(Subtype)), PDF_NAME(Image))) continue;
                pdf_obj *filter = pdf_dict_get(ctx, x, PDF_NAME(Filter));
                if (pdf_is_array(ctx, filter)) filter = pdf_array_get(ctx, filter, 0);
                pdf_obj *cs = pdf_dict_get(ctx, x, PDF_NAME(ColorSpace));
                if (pdf_is_array(ctx, cs)) cs = pdf_array_get(ctx, cs, 0);
                jb_open(ctx, &b, '{');
                jb_kstr(ctx, &b, "resource", pdf_to_name(ctx, pdf_dict_get_key(ctx, xobjects, i)));
                jb_kint(ctx, &b, "width", pdf_dict_get_int(ctx, x, PDF_NAME(Width)));
                jb_kint(ctx, &b, "height", pdf_dict_get_int(ctx, x, PDF_NAME(Height)));
                jb_kint(ctx, &b, "bits", pdf_dict_get_int(ctx, x, PDF_NAME(BitsPerComponent)));
                jb_kstr(ctx, &b, "colorspace", pdf_to_name(ctx, cs));
                jb_kstr(ctx, &b, "filter", pdf_to_name(ctx, filter));
                jb_close(ctx, &b, '}');
            }
            jb_close(ctx, &b, ']');
            pdf_obj *list = pdf_dict_get(ctx, page, PDF_NAME(Annots));
            jb_key(ctx, &b, "annotations");
            jb_open(ctx, &b, '[');
            for (int j = 0; j < pdf_array_len(ctx, list) && j < 1024; ++j)
                jb_str(ctx, &b, pdf_to_name(ctx, pdf_dict_get(ctx, pdf_array_get(ctx, list, j), PDF_NAME(Subtype))));
            jb_close(ctx, &b, ']');
        }
        jb_close(ctx, &b, '}');
        unsigned char *data;
        size_t n = fz_buffer_storage(ctx, b.out, &data);
        result = lean_mk_string_from_bytes((char *)data, n);
    }
    fz_always(ctx) { fz_drop_buffer(ctx, b.out); pthread_mutex_unlock(&d->doc_lock); }
    fz_catch(ctx) { return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

/* Reflowable HTML of a page, for reading it in a column of any width.

   Tagged documents give the elements through their structure tree:
   paragraphs, headings, lists, tables, code, notes, figures.  Untagged pages
   are read by their layout: each text block is a paragraph, much larger type
   a heading, monospaced blocks code.  Lines of a paragraph flow together,
   hyphens at line ends joined; lines of code keep their breaks and indents.
   Figures, formulas, images and clusters of vector drawing are rendered from
   the page as PNG, so drawings survive the reflow, and their text becomes the
   image's alt text.  Text outside the structure of a tagged page (artifacts:
   running heads, page numbers) and one-line blocks in the margins of an
   untagged one are marked <vellumory-running>, for the reader to hide.
   Links point at "#page-N" or their URI.  Every block has y, its top as a
   fraction of the page height, to map reading positions both ways; images
   have pw and ph, their size on the page in points; the page has size, its
   body text size in points. */

enum { RF_BOLD = 1, RF_ITALIC = 2, RF_MONO = 4, RF_SUP = 8 };

typedef struct { fz_rect rect; char href[512]; } rf_link;
typedef struct { fz_rect rect; int members, images, done; } rf_region;

typedef struct {
    fz_context *ctx;
    fz_buffer *out;
    fz_display_list *list;
    fz_rect page;
    int images, invert, structured;
    const fz_stext_block *first;  /* the page's blocks */
    int chars;         /* characters on the page outside figures */
    int artifact;      /* inside marked content outside the structure tree */
    int pending[32], npending;  /* figures met inside a paragraph, shown after it */
    float body;
    rf_link *links; int nlinks;
    rf_region *regions; int nregions;
    int inline_depth;  /* inside a paragraph-like element */
    int pre;           /* inside code that keeps its lines */
    int style, link;   /* the open inline style, and link index + 1 */
    int space;         /* before the next character: 0 none, 1 a space, 2 one if the
                          layout parts them (a line or block boundary) */
    int started;       /* the current element has text */
    float last_y, last_x;
} rf;

static void rf_puts(rf *r, const char *s) { fz_append_string(r->ctx, r->out, s); }

static void rf_rune(fz_context *ctx, fz_buffer *out, int c) {
    switch (c) {
    case '&': fz_append_string(ctx, out, "&amp;"); break;
    case '<': fz_append_string(ctx, out, "&lt;"); break;
    case '>': fz_append_string(ctx, out, "&gt;"); break;
    case '"': fz_append_string(ctx, out, "&quot;"); break;
    default: fz_append_rune(ctx, out, c);
    }
}

/* Append the UTF-8 bytes S[0..N) escaped for an attribute value. */
static void rf_attr(rf *r, const char *s, size_t n) {
    for (size_t i = 0; i < n && s[i]; ++i) {
        unsigned char c = (unsigned char)s[i];
        if (c == '&' || c == '<' || c == '>' || c == '"') rf_rune(r->ctx, r->out, c);
        else fz_append_byte(r->ctx, r->out, c);
    }
}

static float rf_y(rf *r, fz_rect b) {
    float h = r->page.y1 - r->page.y0;
    return h > 0 ? fz_clamp((b.y0 - r->page.y0) / h, 0, 1) : 0;
}

static int rf_ignorable(int c) {
    return c == 0xAD || c == 0x2060 || c == 0xFEFF || c == 0x200B || (c < 0x20 && c != '\t');
}

static void rf_close_inline(rf *r) {
    if (r->style & RF_SUP) rf_puts(r, "</sup>");
    if (r->style & RF_MONO) rf_puts(r, "</code>");
    if (r->style & RF_ITALIC) rf_puts(r, "</i>");
    if (r->style & RF_BOLD) rf_puts(r, "</b>");
    if (r->link) rf_puts(r, "</a>");
    r->style = 0; r->link = 0;
}

static void rf_open_inline(rf *r, int style, int link) {
    if (link) {
        rf_puts(r, "<a href=\"");
        rf_attr(r, r->links[link - 1].href, sizeof r->links[link - 1].href);
        rf_puts(r, "\">");
    }
    if (style & RF_BOLD) rf_puts(r, "<b>");
    if (style & RF_ITALIC) rf_puts(r, "<i>");
    if (style & RF_MONO) rf_puts(r, "<code>");
    if (style & RF_SUP) rf_puts(r, "<sup>");
    r->style = style; r->link = link;
}

static void rf_open(rf *r, const char *tag, fz_rect bbox) {
    rf_close_inline(r);
    fz_append_printf(r->ctx, r->out, "<%s y=\"%.4f\">", tag, rf_y(r, bbox));
    r->space = 0; r->started = 0;
}

static void rf_close(rf *r, const char *tag) {
    rf_close_inline(r);
    fz_append_printf(r->ctx, r->out, "</%s>\n", tag);
    r->space = 0; r->started = 0;
}

static int rf_link_at(rf *r, const fz_stext_char *ch) {
    fz_point c = { (ch->quad.ll.x + ch->quad.ur.x) / 2, (ch->quad.ll.y + ch->quad.ur.y) / 2 };
    for (int i = 0; i < r->nlinks; ++i)
        if (fz_is_point_inside_rect(c, r->links[i].rect)) return i + 1;
    return 0;
}

static void rf_char(rf *r, int c, int style, int link, const fz_stext_char *ch) {
    int space = r->started && (r->space == 1 || (r->space == 2 &&
        !(fabsf(ch->origin.y - r->last_y) < 0.6f * ch->size && ch->quad.ll.x - r->last_x < 0.2f * ch->size)));
    if (style != r->style || link != r->link) {
        rf_close_inline(r);
        if (space) fz_append_byte(r->ctx, r->out, ' ');
        space = 0;
        rf_open_inline(r, style, link);
    }
    if (space) fz_append_byte(r->ctx, r->out, ' ');
    r->space = 0;
    rf_rune(r->ctx, r->out, c);
    r->started = 1;
    r->last_x = fmaxf(ch->quad.ur.x, ch->quad.lr.x);
    r->last_y = ch->origin.y;
}

/* The lines of text block B, flowing (or, in code, keeping their breaks). */
static void rf_text(rf *r, const fz_stext_block *b) {
    for (fz_stext_line *line = b->u.t.first_line; line; line = line->next) {
        float size = 0, baseline = 0;
        for (fz_stext_char *ch = line->first_char; ch; ch = ch->next)
            if (ch->size > size) { size = ch->size; baseline = ch->origin.y; }
        int horizontal = fabsf(line->dir.y) < 0.1f;
        if (r->pre && r->started && line->first_char) {
            if (fabsf(baseline - r->last_y) > 0.5f * size) fz_append_byte(r->ctx, r->out, '\n');
            else {
                float gap = line->first_char->quad.ll.x - r->last_x;
                for (int n = gap > 0.3f * size ? (int)fminf(40, roundf(gap / (0.6f * size))) : 0; n > 0; --n)
                    fz_append_byte(r->ctx, r->out, ' ');
            }
        }
        for (fz_stext_char *ch = line->first_char; ch; ch = ch->next) {
            int c = ch->c == '\t' ? ' ' : ch->c;
            if (rf_ignorable(c)) continue;
            if (c == ' ' && !r->pre) { r->space = 1; continue; }
            int style = 0;
            if ((ch->flags & FZ_STEXT_BOLD) || (ch->font && fz_font_is_bold(r->ctx, ch->font))) style |= RF_BOLD;
            if (ch->font && fz_font_is_italic(r->ctx, ch->font)) style |= RF_ITALIC;
            if (!r->pre && ch->font && fz_font_is_monospaced(r->ctx, ch->font)) style |= RF_MONO;
            if (horizontal && ch->size < 0.8f * size && ch->origin.y < baseline - 0.2f * size) style |= RF_SUP;
            rf_char(r, c, style, rf_link_at(r, ch), ch);
        }
        /* A hyphen at the line end, kept as a soft hyphen, joins the lines. */
        if (!r->pre && !(line->last_char && line->last_char->c == 0xAD) &&
            !(line->flags & FZ_STEXT_LINE_FLAGS_JOINED))
            r->space = 2;
    }
    if (!r->pre && r->space != 1) r->space = 2;
}

static void rf_lines_text(fz_context *ctx, const fz_stext_block *b, fz_buffer *out) {
    for (fz_stext_line *line = b->u.t.first_line; line; line = line->next) {
        for (fz_stext_char *ch = line->first_char; ch; ch = ch->next)
            if (!rf_ignorable(ch->c)) fz_append_rune(ctx, out, ch->c == '\t' ? ' ' : ch->c);
        fz_append_byte(ctx, out, ' ');
    }
}

/* Text of the blocks from FIRST, whitespace collapsed, into OUT (at most
   LIMIT bytes); captions are left out when SKIP_CAPTIONS. */
static void rf_collect(fz_context *ctx, const fz_stext_block *first, fz_buffer *out, size_t limit,
                       int skip_captions) {
    for (const fz_stext_block *b = first; b && fz_buffer_storage(ctx, out, NULL) < limit; b = b->next) {
        if (b->type == FZ_STEXT_BLOCK_STRUCT && b->u.s.down) {
            if (!(skip_captions && b->u.s.down->standard == FZ_STRUCTURE_CAPTION))
                rf_collect(ctx, b->u.s.down->first_block, out, limit, 0);
        } else if (b->type == FZ_STEXT_BLOCK_TEXT) rf_lines_text(ctx, b, out);
    }
}

static fz_rect rf_bbox(const fz_stext_block *first, int skip_captions) {
    fz_rect box = fz_empty_rect;
    for (const fz_stext_block *b = first; b; b = b->next) {
        if (b->type == FZ_STEXT_BLOCK_STRUCT) {
            if (b->u.s.down && !(skip_captions && b->u.s.down->standard == FZ_STRUCTURE_CAPTION))
                box = fz_union_rect(box, rf_bbox(b->u.s.down->first_block, 0));
        } else box = fz_union_rect(box, b->bbox);
    }
    return box;
}

/* AREA of the page as an image with ALT text, inline or as a figure. */
static void rf_image(rf *r, fz_rect area, fz_buffer *alt, int inline_image) {
    fz_context *ctx = r->ctx;
    area = fz_intersect_rect(area, r->page);
    float w = area.x1 - area.x0, h = area.y1 - area.y0;
    if (fz_is_empty_rect(area) || w < 4 || h < 4) return;
    if (!inline_image) fz_append_printf(ctx, r->out, "<figure y=\"%.4f\">", rf_y(r, area));
    fz_append_printf(ctx, r->out, "<img pw=\"%.1f\" ph=\"%.1f\" alt=\"", w, h);
    if (alt) {
        unsigned char *data;
        size_t n = fz_buffer_storage(ctx, alt, &data);
        rf_attr(r, (const char *)data, n);
    }
    rf_puts(r, "\"");
    if (r->images && r->list) {
        /* Twice the page's resolution (144 dpi) for sharp scaling, at most 4 Mpx. */
        float scale = 2.0f;
        if (w * h * scale * scale > 4e6f) scale = sqrtf(4e6f / (w * h));
        fz_matrix m = fz_scale(scale, scale);
        fz_irect box = fz_round_rect(fz_transform_rect(area, m));
        fz_pixmap *pix = NULL;
        fz_device *device = NULL;
        fz_buffer *png = NULL;
        fz_var(pix); fz_var(device); fz_var(png);
        fz_try(ctx) {
            pix = fz_new_pixmap_with_bbox(ctx, fz_device_rgb(ctx), box, NULL, 0);
            fz_clear_pixmap_with_value(ctx, pix, 255);
            device = fz_new_draw_device(ctx, fz_identity, pix);
            fz_run_display_list(ctx, r->list, device, m, fz_rect_from_irect(box), NULL);
            fz_close_device(ctx, device);
            if (r->invert) fz_invert_pixmap_luminance(ctx, pix);
            png = fz_new_buffer_from_pixmap_as_png(ctx, pix, fz_default_color_params);
            rf_puts(r, " src=\"data:image/png;base64,");
            fz_append_base64_buffer(ctx, r->out, png, 0);
            rf_puts(r, "\"");
        }
        fz_always(ctx) {
            fz_drop_buffer(ctx, png);
            fz_drop_device(ctx, device);
            fz_drop_pixmap(ctx, pix);
        }
        fz_catch(ctx) { fz_ignore_error(ctx); }
    }
    rf_puts(r, ">");
    if (!inline_image) rf_puts(r, "</figure>\n");
}

static void rf_blocks(rf *r, const fz_stext_block *first, int top);
static void rf_flush(rf *r);

static void rf_flow(rf *r, const fz_stext_block *first) { rf_blocks(r, first, 0); }

static void rf_element(rf *r, const char *tag, fz_rect bbox, const fz_stext_block *first, int paragraph) {
    rf_open(r, tag, bbox);
    if (paragraph) r->inline_depth++;
    rf_blocks(r, first, 0);
    if (paragraph) r->inline_depth--;
    rf_close(r, tag);
    rf_flush(r);
}

static void rf_struct(rf *r, const fz_stext_struct *s, int top) {
    const fz_stext_block *first = s->first_block;
    fz_rect bbox = rf_bbox(first, 0);
    /* Artifacts, and the nameless elements MuPDF makes of marked content
       outside the structure tree: running text at the top of a tagged
       page, else flowing with what holds them. */
    if (s->standard == FZ_STRUCTURE_ARTIFACT || !s->raw[0]) {
        if (!r->structured) rf_blocks(r, first, top);
        else if (top) rf_element(r, "vellumory-running", bbox, first, 1);
        else { r->artifact++; rf_flow(r, first); r->artifact--; }
        return;
    }
    const char *tag = NULL;
    int paragraph = 0;
    switch (s->standard) {
    case FZ_STRUCTURE_FIGURE: case FZ_STRUCTURE_FORMULA: {
        fz_rect art = rf_bbox(first, 1);
        int inline_image = (r->inline_depth > 0 || r->pre) && art.y1 - art.y0 < 3 * r->body;
        fz_buffer *alt = fz_new_buffer(r->ctx, 256);
        fz_try(r->ctx) {
            rf_collect(r->ctx, first, alt, 400, 1);
            if (inline_image) {
                rf_close_inline(r);
                if (r->space && r->started) fz_append_byte(r->ctx, r->out, ' ');
                rf_image(r, art, alt, 1);
                r->started = 1;
            } else rf_image(r, art, alt, 0);
        }
        fz_always(r->ctx) fz_drop_buffer(r->ctx, alt);
        fz_catch(r->ctx) fz_rethrow(r->ctx);
        for (const fz_stext_block *b = first; b; b = b->next)
            if (b->type == FZ_STEXT_BLOCK_STRUCT && b->u.s.down && b->u.s.down->standard == FZ_STRUCTURE_CAPTION)
                rf_struct(r, b->u.s.down, 0);
        return;
    }
    case FZ_STRUCTURE_H: case FZ_STRUCTURE_H1: tag = "h1"; paragraph = 1; break;
    case FZ_STRUCTURE_H2: tag = "h2"; paragraph = 1; break;
    case FZ_STRUCTURE_H3: tag = "h3"; paragraph = 1; break;
    case FZ_STRUCTURE_H4: tag = "h4"; paragraph = 1; break;
    case FZ_STRUCTURE_H5: tag = "h5"; paragraph = 1; break;
    case FZ_STRUCTURE_H6: tag = "h6"; paragraph = 1; break;
    case FZ_STRUCTURE_P: case FZ_STRUCTURE_TOCI: case FZ_STRUCTURE_BIBENTRY: case FZ_STRUCTURE_TITLE:
        tag = "p"; paragraph = 1; break;
    case FZ_STRUCTURE_CAPTION: tag = "figcaption"; paragraph = 1; break;
    case FZ_STRUCTURE_LISTITEM: tag = "li"; paragraph = 1; break;
    case FZ_STRUCTURE_TH: tag = "th"; paragraph = 1; break;
    case FZ_STRUCTURE_TD: tag = "td"; paragraph = 1; break;
    case FZ_STRUCTURE_LIST: tag = "ul"; break;
    case FZ_STRUCTURE_TABLE: tag = "table"; break;
    case FZ_STRUCTURE_THEAD: tag = "thead"; break;
    case FZ_STRUCTURE_TBODY: tag = "tbody"; break;
    case FZ_STRUCTURE_TFOOT: tag = "tfoot"; break;
    case FZ_STRUCTURE_TR: tag = "tr"; break;
    case FZ_STRUCTURE_BLOCKQUOTE: tag = "blockquote"; break;
    case FZ_STRUCTURE_ASIDE: case FZ_STRUCTURE_FENOTE: tag = "aside"; break;
    case FZ_STRUCTURE_NOTE: if (!r->inline_depth) tag = "aside"; break;
    case FZ_STRUCTURE_CODE:
        if (!r->inline_depth && !r->pre && bbox.y1 - bbox.y0 > 1.8f * r->body) {
            rf_open(r, "pre", bbox);
            r->pre = 1;
            rf_blocks(r, first, 0);
            r->pre = 0;
            rf_close(r, "pre");
            rf_flush(r);
            return;
        }
        break;
    case FZ_STRUCTURE_LABEL:
        rf_blocks(r, first, 0);
        r->space = 1;
        return;
    default: break;
    }
    /* Inside a paragraph (or code), elements flow along with it. */
    if (!tag || r->inline_depth || r->pre) rf_flow(r, first);
    else rf_element(r, tag, bbox, first, paragraph);
}

static int rf_lines(const fz_stext_block *b) {
    int n = 0;
    for (fz_stext_line *line = b->u.t.first_line; line; line = line->next) ++n;
    return n;
}

/* The element for a text block by its layout: a heading, code or a paragraph. */
static const char *rf_layout_tag(rf *r, const fz_stext_block *b) {
    float sum = 0;
    int n = 0, bold = 0, mono = 0, lines = rf_lines(b);
    for (fz_stext_line *line = b->u.t.first_line; line; line = line->next)
        for (fz_stext_char *ch = line->first_char; ch; ch = ch->next) {
            if (ch->c == ' ' || rf_ignorable(ch->c)) continue;
            sum += ch->size; ++n;
            if ((ch->flags & FZ_STEXT_BOLD) || (ch->font && fz_font_is_bold(r->ctx, ch->font))) ++bold;
            if (ch->font && fz_font_is_monospaced(r->ctx, ch->font)) ++mono;
        }
    if (!n) return NULL;
    if (mono == n && lines > 1) return "pre";
    float size = sum / n;
    if (r->body > 0 && lines <= 3) {
        if (size >= 1.6f * r->body) return "h1";
        if (size >= 1.3f * r->body) return "h2";
        if (size >= 1.12f * r->body || (bold == n && lines == 1 && n < 100)) return "h3";
    }
    return "p";
}

/* Text block B as one element TAG; "pre" keeps its lines. */
static void rf_text_element(rf *r, const char *tag, const fz_stext_block *b) {
    int code = tag[0] == 'p' && tag[1] == 'r';
    rf_open(r, tag, b->bbox);
    if (code) r->pre = 1; else r->inline_depth++;
    rf_text(r, b);
    if (code) r->pre = 0; else r->inline_depth--;
    rf_close(r, tag);
    rf_flush(r);
}

/* Whether an image at BOX is left out: too small, or a background. */
static int rf_skip_image(rf *r, fz_rect box) {
    float w = box.x1 - box.x0, h = box.y1 - box.y0;
    float page_area = (r->page.x1 - r->page.x0) * (r->page.y1 - r->page.y0);
    return w < 8 || h < 8 || (w * h > 0.7f * page_area && r->chars >= 50);
}

static int rf_region_at(rf *r, fz_rect b) {
    fz_point c = { (b.x0 + b.x1) / 2, (b.y0 + b.y1) / 2 };
    for (int i = 0; i < r->nregions; ++i)
        if (fz_is_point_inside_rect(c, r->regions[i].rect)) return i;
    return -1;
}

static void rf_region_text(rf *r, const fz_stext_block *first, int i, fz_buffer *alt) {
    for (const fz_stext_block *b = first; b && fz_buffer_storage(r->ctx, alt, NULL) < 400; b = b->next) {
        if (b->type == FZ_STEXT_BLOCK_STRUCT && b->u.s.down) rf_region_text(r, b->u.s.down->first_block, i, alt);
        else if (b->type == FZ_STEXT_BLOCK_TEXT && rf_region_at(r, b->bbox) == i) rf_lines_text(r->ctx, b, alt);
    }
}

/* Region I as a figure, the text in it as the alt text. */
static void rf_region_figure(rf *r, int i) {
    fz_buffer *alt = fz_new_buffer(r->ctx, 256);
    fz_try(r->ctx) {
        rf_region_text(r, r->first, i, alt);
        rf_image(r, r->regions[i].rect, alt, 0);
    }
    fz_always(r->ctx) fz_drop_buffer(r->ctx, alt);
    fz_catch(r->ctx) fz_rethrow(r->ctx);
}

/* Region I's figure, once: here, or after the paragraph it was met in. */
static void rf_region_emit(rf *r, int i) {
    if (r->regions[i].done) return;
    r->regions[i].done = 1;
    if ((r->inline_depth || r->pre) && r->npending < (int)(sizeof r->pending / sizeof *r->pending))
        r->pending[r->npending++] = i;
    else rf_region_figure(r, i);
}

static void rf_flush(rf *r) {
    if (r->inline_depth || r->pre) return;
    for (int k = 0; k < r->npending; ++k) rf_region_figure(r, r->pending[k]);
    r->npending = 0;
}

static int rf_is(const fz_stext_block *b, fz_structure standard) {
    return b && b->type == FZ_STEXT_BLOCK_STRUCT && b->u.s.down && b->u.s.down->standard == standard;
}

static void rf_blocks(rf *r, const fz_stext_block *first, int top) {
    for (const fz_stext_block *b = first; b; b = b->next) {
        if (b->type != FZ_STEXT_BLOCK_STRUCT && r->nregions) {
            int region = rf_region_at(r, b->bbox);
            /* Drawing and images in a figure's region are the figure; so is
               text there that is not content of a tagged page. */
            if (region >= 0 && (b->type != FZ_STEXT_BLOCK_TEXT || top || r->artifact || !r->structured)) {
                if (b->type == FZ_STEXT_BLOCK_TEXT || b->type == FZ_STEXT_BLOCK_IMAGE ||
                    b->type == FZ_STEXT_BLOCK_VECTOR)
                    rf_region_emit(r, region);
                continue;
            }
        }
        switch (b->type) {
        case FZ_STEXT_BLOCK_STRUCT:
            if (!b->u.s.down) break;
            /* A caption tagged before its figure reads after it. */
            if (rf_is(b, FZ_STRUCTURE_CAPTION) && (rf_is(b->next, FZ_STRUCTURE_FIGURE) || rf_is(b->next, FZ_STRUCTURE_FORMULA))) {
                rf_struct(r, b->next->u.s.down, top);
                rf_struct(r, b->u.s.down, top);
                b = b->next;
            } else rf_struct(r, b->u.s.down, top);
            break;
        case FZ_STEXT_BLOCK_TEXT: {
            if (top && !r->structured) {
                float h = r->page.y1 - r->page.y0;
                int margin = b->bbox.y1 <= r->page.y0 + 0.08f * h || b->bbox.y0 >= r->page.y1 - 0.08f * h;
                if (margin && rf_lines(b) == 1) { rf_text_element(r, "vellumory-running", b); break; }
            }
            /* Text outside a tagged page's structure is an artifact. */
            if (top && r->structured) { rf_text_element(r, "vellumory-running", b); break; }
            if (r->inline_depth || r->pre) { rf_text(r, b); break; }
            const char *tag = top || !r->structured ? rf_layout_tag(r, b) : "p";
            if (tag) rf_text_element(r, tag, b);
            break;
        }
        case FZ_STEXT_BLOCK_IMAGE:
            /* Images large enough are figures (regions); these are small ones
               in the text, or backgrounds. */
            if (!rf_skip_image(r, b->bbox)) rf_image(r, b->bbox, NULL, r->inline_depth > 0 || r->pre);
            break;
        default: break;
        }
    }
}

/* The most common character size of the page: its body text. */
static void rf_sizes(const fz_stext_block *first, int *histogram) {
    for (const fz_stext_block *b = first; b; b = b->next) {
        if (b->type == FZ_STEXT_BLOCK_STRUCT && b->u.s.down) rf_sizes(b->u.s.down->first_block, histogram);
        else if (b->type == FZ_STEXT_BLOCK_TEXT)
            for (fz_stext_line *line = b->u.t.first_line; line; line = line->next)
                for (fz_stext_char *ch = line->first_char; ch; ch = ch->next) {
                    int bucket = (int)roundf(ch->size * 2);
                    if (ch->c != ' ' && bucket > 0 && bucket < 400) ++histogram[bucket];
                }
    }
}

/* Whether the blocks from FIRST hold elements of a structure tree (MuPDF
   also makes nameless elements of marked content outside it). */
static int rf_tagged(const fz_stext_block *first) {
    for (const fz_stext_block *b = first; b; b = b->next)
        if (b->type == FZ_STEXT_BLOCK_STRUCT && b->u.s.down &&
            (b->u.s.down->raw[0] || rf_tagged(b->u.s.down->first_block)))
            return 1;
    return 0;
}

/* Characters, and drawing or image blocks outside figures, from FIRST. */
static void rf_census(const fz_stext_block *first, int *chars, int *shapes) {
    for (const fz_stext_block *b = first; b; b = b->next) {
        if (b->type == FZ_STEXT_BLOCK_STRUCT && b->u.s.down) {
            fz_structure s = b->u.s.down->standard;
            if (s != FZ_STRUCTURE_FIGURE && s != FZ_STRUCTURE_FORMULA)
                rf_census(b->u.s.down->first_block, chars, shapes);
        } else if (b->type == FZ_STEXT_BLOCK_TEXT) {
            for (fz_stext_line *line = b->u.t.first_line; line; line = line->next)
                for (fz_stext_char *ch = line->first_char; ch; ch = ch->next) ++*chars;
        } else if (b->type == FZ_STEXT_BLOCK_VECTOR || b->type == FZ_STEXT_BLOCK_IMAGE) ++*shapes;
    }
}

static void rf_candidates(rf *r, const fz_stext_block *first, int capacity) {
    float page_area = (r->page.x1 - r->page.x0) * (r->page.y1 - r->page.y0);
    for (const fz_stext_block *b = first; b && r->nregions < capacity; b = b->next) {
        if (b->type == FZ_STEXT_BLOCK_STRUCT && b->u.s.down) {
            fz_structure s = b->u.s.down->standard;
            if (s != FZ_STRUCTURE_FIGURE && s != FZ_STRUCTURE_FORMULA)
                rf_candidates(r, b->u.s.down->first_block, capacity);
            continue;
        }
        fz_rect box = fz_intersect_rect(b->bbox, r->page);
        float w = box.x1 - box.x0, h = box.y1 - box.y0;
        if (fz_is_empty_rect(box)) continue;
        if (b->type == FZ_STEXT_BLOCK_VECTOR) {
            /* Page backgrounds and rules are not drawings. */
            if (w * h > 0.7f * page_area || w < 1.5f || h < 1.5f) continue;
        } else if (b->type == FZ_STEXT_BLOCK_IMAGE) {
            /* Small images stay in their line of text. */
            if (rf_skip_image(r, box) || (h < 2.5f * r->body && w < 8 * r->body)) continue;
        } else continue;
        rf_region *g = &r->regions[r->nregions++];
        g->rect = box; g->members = 1; g->images = b->type == FZ_STEXT_BLOCK_IMAGE; g->done = 0;
    }
}

static float rf_covered(const fz_stext_block *first, fz_rect g) {
    float covered = 0;
    for (const fz_stext_block *b = first; b; b = b->next) {
        if (b->type == FZ_STEXT_BLOCK_STRUCT && b->u.s.down) covered += rf_covered(b->u.s.down->first_block, g);
        else if (b->type == FZ_STEXT_BLOCK_TEXT) {
            fz_point c = { (b->bbox.x0 + b->bbox.x1) / 2, (b->bbox.y0 + b->bbox.y1) / 2 };
            if (fz_is_point_inside_rect(c, g)) covered += (b->bbox.x1 - b->bbox.x0) * (b->bbox.y1 - b->bbox.y0);
        }
    }
    return covered;
}

/* Clusters of drawing and images outside tagged figures: figures too. */
static void rf_find_regions(rf *r, const fz_stext_page *text) {
    float page_area = (r->page.x1 - r->page.x0) * (r->page.y1 - r->page.y0);
    int chars = 0, capacity = 0;
    rf_census(text->first_block, &chars, &capacity);
    if (!capacity || page_area <= 0) return;
    if (capacity > 4096) capacity = 4096;
    r->regions = fz_malloc_array(r->ctx, capacity, rf_region);
    r->chars = chars;
    rf_candidates(r, text->first_block, capacity);
    /* Merge regions closer than 6 points until none are. */
    for (int merged = 1; merged; ) {
        merged = 0;
        for (int i = 0; i < r->nregions; ++i)
            for (int j = i + 1; j < r->nregions; ++j) {
                fz_rect a = fz_expand_rect(r->regions[i].rect, 6);
                if (fz_is_empty_rect(fz_intersect_rect(a, r->regions[j].rect))) continue;
                r->regions[i].rect = fz_union_rect(r->regions[i].rect, r->regions[j].rect);
                r->regions[i].members += r->regions[j].members;
                r->regions[i].images += r->regions[j].images;
                r->regions[j--] = r->regions[--r->nregions];
                merged = 1;
            }
    }
    /* A figure has an image, or many strokes over a sizeable area with
       little text among them (a shaded box behind paragraphs is not one). */
    int kept = 0;
    for (int i = 0; i < r->nregions; ++i) {
        rf_region g = r->regions[i];
        float w = g.rect.x1 - g.rect.x0, h = g.rect.y1 - g.rect.y0;
        if (g.images || (g.members >= 4 && w >= 36 && h >= 36 && w * h >= 0.015f * page_area
                         && rf_covered(text->first_block, g.rect) < 0.4f * w * h))
            r->regions[kept++] = g;
    }
    r->nregions = kept;
}

/* The page an internal link URI leads to, or -1. */
static int rf_target(fz_context *ctx, fz_document *doc, const char *uri) {
    int target = -1;
    fz_var(target);
    fz_try(ctx) target = fz_page_number_from_location(ctx, doc, fz_resolve_link(ctx, doc, uri, NULL, NULL));
    fz_catch(ctx) { fz_ignore_error(ctx); return -1; }
    return target;
}

static void rf_load_links(rf *r, v_document *d, int number) {
    fz_context *ctx = r->ctx;
    fz_page *page = NULL;
    fz_link *links = NULL;
    fz_var(page); fz_var(links);
    fz_try(ctx) {
        page = fz_load_page(ctx, d->doc, number);
        links = fz_load_links(ctx, page);
        int n = 0;
        for (fz_link *l = links; l; l = l->next) ++n;
        if (n) r->links = fz_malloc_array(ctx, n, rf_link);
        for (fz_link *l = links; l; l = l->next) {
            if (!l->uri) continue;
            rf_link *k = &r->links[r->nlinks];
            k->rect = l->rect;
            if (fz_is_external_link(ctx, l->uri)) snprintf(k->href, sizeof k->href, "%s", l->uri);
            else {
                int target = rf_target(ctx, d->doc, l->uri);
                if (target < 0) continue;
                snprintf(k->href, sizeof k->href, "#page-%d", target + 1);
            }
            ++r->nlinks;
        }
    }
    fz_always(ctx) { fz_drop_link(ctx, links); fz_drop_page(ctx, page); }
    fz_catch(ctx) fz_ignore_error(ctx);
}

/* FLAGS: 1 renders figures, 2 inverts their luminance (for dark reading). */
lean_obj_res vellumory_native_page_reflow(b_lean_obj_arg handle, uint32_t number, uint8_t flags, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    if (number >= (uint32_t)d->count) return failure("Page outside document");
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    rf r = {0};
    r.ctx = ctx;
    r.images = flags & 1;
    r.invert = (flags & 2) != 0;
    r.page = d->bounds[number];
    fz_stext_page *text = NULL;
    lean_object *result = NULL;
    int built = 0;
    fz_var(text); fz_var(result); fz_var(built);
    fz_var(r.list); fz_var(r.out); fz_var(r.links); fz_var(r.regions);
    fz_try(ctx) {
        pthread_mutex_lock(&d->doc_lock);
        fz_try(ctx) rf_load_links(&r, d, (int)number);
        fz_always(ctx) pthread_mutex_unlock(&d->doc_lock);
        fz_catch(ctx) fz_rethrow(ctx);
        /* The page's cached display list: no document lock, and figures
           render from it. */
        r.list = display_list(ctx, d, (int)number, NULL, &built);
        fz_stext_options options = {0};
        options.flags = FZ_STEXT_DEHYPHENATE | FZ_STEXT_COLLECT_STRUCTURE | FZ_STEXT_PARAGRAPH_BREAK
            | FZ_STEXT_SEGMENT | FZ_STEXT_PRESERVE_IMAGES | FZ_STEXT_COLLECT_VECTORS | FZ_STEXT_LAZY_VECTORS;
        text = fz_new_stext_page_from_display_list(ctx, r.list, &options);
        r.structured = rf_tagged(text->first_block);
        r.first = text->first_block;
        int histogram[400] = {0}, best = 0;
        rf_sizes(text->first_block, histogram);
        for (int i = 1; i < 400; ++i) if (histogram[i] > histogram[best]) best = i;
        r.body = best ? best / 2.0f : 10.0f;
        rf_find_regions(&r, text);
        r.out = fz_new_buffer(ctx, 16384);
        fz_append_printf(ctx, r.out, "<vellumory-page n=\"%u\" size=\"%.1f\" width=\"%.1f\">\n",
                         number + 1, r.body, r.page.x1 - r.page.x0);
        rf_blocks(&r, text->first_block, 1);
        rf_flush(&r);
        rf_close_inline(&r);
        rf_puts(&r, "</vellumory-page>\n");
        unsigned char *data;
        size_t n = fz_buffer_storage(ctx, r.out, &data);
        result = lean_mk_string_from_bytes((char *)data, n);
    }
    fz_always(ctx) {
        fz_drop_buffer(ctx, r.out);
        fz_drop_stext_page(ctx, text);
        fz_drop_display_list(ctx, r.list);
        fz_free(ctx, r.links);
        fz_free(ctx, r.regions);
    }
    fz_catch(ctx) { return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

/* Contents of the INDEXth entry of the EmbeddedFiles name tree. */
lean_obj_res vellumory_native_attachment(b_lean_obj_arg handle, uint32_t index, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    pdf_obj *tree = NULL;
    fz_buffer *contents = NULL;
    lean_object *result = NULL;
    fz_var(tree); fz_var(contents); fz_var(result);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        pdf_document *pdf = pdf_document_from_fz_document(ctx, d->doc);
        if (!pdf) fz_throw(ctx, FZ_ERROR_ARGUMENT, "Not a PDF");
        tree = pdf_load_name_tree(ctx, pdf, PDF_NAME(EmbeddedFiles));
        if ((int)index >= pdf_dict_len(ctx, tree)) fz_throw(ctx, FZ_ERROR_ARGUMENT, "No such attachment");
        pdf_obj *fs = pdf_dict_get_val(ctx, tree, index);
        if (!pdf_is_embedded_file(ctx, fs)) fz_throw(ctx, FZ_ERROR_ARGUMENT, "Attachment is not embedded");
        contents = pdf_load_embedded_file_contents(ctx, fs);
        unsigned char *data;
        size_t n = fz_buffer_storage(ctx, contents, &data);
        result = lean_alloc_sarray(1, n, n);
        memcpy(lean_sarray_cptr(result), data, n);
    }
    fz_always(ctx) {
        fz_drop_buffer(ctx, contents); pdf_drop_obj(ctx, tree);
        pthread_mutex_unlock(&d->doc_lock);
    }
    fz_catch(ctx) { return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

extern lean_obj_res vellumory_mk_word(lean_obj_arg text, lean_obj_arg quad);

static int word_separator(int c) {
    return c == ' ' || c == '\t' || c == 0xa0 || c == 0x2002 || c == 0x2003 || c == 0x2009 || c == 0x3000;
}

/* Words of one page in reading order, each with the polygon spanning its
   glyphs (top-left page points), for diffs and highlights. */
lean_obj_res vellumory_native_words(b_lean_obj_arg handle, uint32_t number, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    if (number >= (uint32_t)d->count) return failure("Page outside document");
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    fz_buffer *word = NULL;
    lean_object *result = lean_mk_empty_array();
    fz_var(word); fz_var(result);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        fz_stext_page *text = cached_text(ctx, d, number);
        word = fz_new_buffer(ctx, 64);
        for (fz_stext_block *block = text->first_block; block; block = block->next) {
            if (block->type != FZ_STEXT_BLOCK_TEXT) continue;
            for (fz_stext_line *line = block->u.t.first_line; line; line = line->next) {
                fz_stext_char *first = NULL, *last = NULL;
                for (fz_stext_char *ch = line->first_char; ; ch = ch->next) {
                    int end = !ch || word_separator(ch->c);
                    if (!end) {
                        if (!first) first = ch;
                        last = ch;
                        fz_append_rune(ctx, word, ch->c);
                    }
                    if (end && first) {
                        fz_quad q;
                        q.ul = first->quad.ul; q.ur = last->quad.ur;
                        q.ll = first->quad.ll; q.lr = last->quad.lr;
                        if (finite_quad(q) && lean_array_size(result) < 200000) {
                            unsigned char *data; size_t n = fz_buffer_storage(ctx, word, &data);
                            result = lean_array_push(result, vellumory_mk_word(
                                lean_mk_string_from_bytes((char *)data, n), box_quad(q, text->mediabox)));
                        }
                        fz_clear_buffer(ctx, word);
                        first = last = NULL;
                    }
                    if (!ch) break;
                }
            }
        }
    }
    fz_always(ctx) { fz_drop_buffer(ctx, word); pthread_mutex_unlock(&d->doc_lock); }
    fz_catch(ctx) { lean_dec(result); return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

/* ---- SyncTeX, through TeX Live's libsynctex (the library Skim, Sumatra and
   Texworks use). Coordinates are 72 dpi from the page's top-left corner. */
#include <synctex/synctex_parser.h>

extern lean_obj_res vellumory_mk_sync_box(uint32_t page, double x, double y, double w, double h);
extern lean_obj_res vellumory_mk_sync_source(lean_obj_arg file, uint32_t line, uint32_t column);

lean_obj_res vellumory_native_synctex_forward(b_lean_obj_arg pdf, b_lean_obj_arg source,
        uint32_t line, uint32_t column, lean_obj_arg world) {
    (void)world;
    synctex_scanner_p scanner = synctex_scanner_new_with_output_file(lean_string_cstr(pdf), NULL, 1);
    if (!scanner) return failure("No SyncTeX data next to the document");
    lean_object *result = lean_mk_empty_array();
    if (synctex_display_query(scanner, lean_string_cstr(source), (int)line, (int)column, -1) > 0) {
        synctex_node_p node;
        while ((node = synctex_scanner_next_result(scanner)) && lean_array_size(result) < 1024) {
            double h = synctex_node_box_visible_h(node), v = synctex_node_box_visible_v(node);
            double width = synctex_node_box_visible_width(node);
            double height = synctex_node_box_visible_height(node), depth = synctex_node_box_visible_depth(node);
            if (!isfinite(h) || !isfinite(v) || !isfinite(width) || !isfinite(height) || !isfinite(depth)) continue;
            result = lean_array_push(result, vellumory_mk_sync_box((uint32_t)synctex_node_page(node),
                h, v - height, width, height + depth));
        }
    }
    synctex_scanner_free(scanner);
    return lean_io_result_mk_ok(result);
}

lean_obj_res vellumory_native_synctex_backward(b_lean_obj_arg pdf, uint32_t page, double x, double y,
        lean_obj_arg world) {
    (void)world;
    if (!isfinite(x) || !isfinite(y)) return failure("Invalid SyncTeX position");
    synctex_scanner_p scanner = synctex_scanner_new_with_output_file(lean_string_cstr(pdf), NULL, 1);
    if (!scanner) return failure("No SyncTeX data next to the document");
    lean_object *result = lean_mk_empty_array();
    if (synctex_edit_query(scanner, (int)page, (float)x, (float)y) > 0) {
        synctex_node_p node;
        while ((node = synctex_scanner_next_result(scanner)) && lean_array_size(result) < 16) {
            const char *name = synctex_scanner_get_name(scanner, synctex_node_tag(node));
            if (!name) continue;
            int line = synctex_node_line(node), column = synctex_node_column(node);
            result = lean_array_push(result, vellumory_mk_sync_source(lean_mk_string(name),
                line < 0 ? 0 : (uint32_t)line, column < 0 ? 0 : (uint32_t)column));
        }
    }
    synctex_scanner_free(scanner);
    return lean_io_result_mk_ok(result);
}

/* ---- XMP, through Exempi (Adobe's XMP Toolkit). Unknown properties,
   extension schemas and PDF/A or PDF/UA identification are kept as parsed.
   An empty value deletes the field. */
#include <exempi/xmp.h>
#include <exempi/xmpconsts.h>

static pthread_once_t xmp_once = PTHREAD_ONCE_INIT;
static bool xmp_ready = false;
static void xmp_start(void) { xmp_ready = xmp_init(); }

static bool xmp_apply(XmpPtr xmp, const char *field, const char *value) {
    bool remove = value[0] == 0;
    const char *schema = NULL, *name = NULL;
    enum { SIMPLE, ALT_TEXT, SEQ, BAG } kind = SIMPLE;
    if (!strcmp(field, "title")) { schema = NS_DC; name = "title"; kind = ALT_TEXT; }
    else if (!strcmp(field, "subject")) { schema = NS_DC; name = "description"; kind = ALT_TEXT; }
    else if (!strcmp(field, "author")) { schema = NS_DC; name = "creator"; kind = SEQ; }
    else if (!strcmp(field, "language")) { schema = NS_DC; name = "language"; kind = BAG; }
    else if (!strcmp(field, "keywords")) { schema = NS_PDF; name = "Keywords"; }
    else if (!strcmp(field, "producer")) { schema = NS_PDF; name = "Producer"; }
    else if (!strcmp(field, "creator")) { schema = NS_XAP; name = "CreatorTool"; }
    else if (!strcmp(field, "created")) { schema = NS_XAP; name = "CreateDate"; }
    else if (!strcmp(field, "modified")) { schema = NS_XAP; name = "ModifyDate"; }
    else if (!strcmp(field, "metadata-date")) { schema = NS_XAP; name = "MetadataDate"; }
    else return false;
    if (xmp_has_property(xmp, schema, name) && !xmp_delete_property(xmp, schema, name)) return false;
    if (remove) return true;
    switch (kind) {
    case ALT_TEXT: return xmp_set_localized_text(xmp, schema, name, "", "x-default", value, 0);
    case SEQ: return xmp_append_array_item(xmp, schema, name, XMP_PROP_ARRAY_IS_ORDERED, value, 0);
    case BAG: return xmp_append_array_item(xmp, schema, name, XMP_PROP_VALUE_IS_ARRAY, value, 0);
    default: return xmp_set_property(xmp, schema, name, value, 0);
    }
}

lean_obj_res vellumory_native_xmp_update(b_lean_obj_arg old, b_lean_obj_arg fields, b_lean_obj_arg values,
        lean_obj_arg world) {
    (void)world;
    pthread_once(&xmp_once, xmp_start);
    if (!xmp_ready) return failure("The XMP toolkit could not start");
    size_t count = lean_array_size(fields);
    if (lean_array_size(values) != count) return failure("XMP fields and values differ in number");
    size_t size = lean_sarray_size(old);
    XmpPtr xmp = size ? xmp_new((const char *)lean_sarray_cptr(old), size) : xmp_new_empty();
    if (!xmp) {
        char message[96];
        snprintf(message, sizeof message, "The document's XMP packet does not parse (XMP error %d)", xmp_get_error());
        return failure(message);
    }
    for (size_t i = 0; i < count; i++) {
        const char *field = lean_string_cstr(lean_array_get_core(fields, i));
        if (!xmp_apply(xmp, field, lean_string_cstr(lean_array_get_core(values, i)))) {
            char message[160];
            snprintf(message, sizeof message, "Cannot set XMP field %.64s (XMP error %d)", field, xmp_get_error());
            xmp_free(xmp);
            return failure(message);
        }
    }
    XmpStringPtr text = xmp_string_new();
    /* A writable packet with the toolkit's usual 2 KiB padding. */
    bool ok = xmp_serialize_and_format(xmp, text, 0, 2048, "\n", " ", 0);
    lean_object *result = NULL;
    if (ok) {
        const char *bytes = xmp_string_cstr(text);
        size_t length = strlen(bytes);
        result = lean_alloc_sarray(1, length, length);
        memcpy(lean_sarray_cptr(result), bytes, length);
    }
    xmp_string_free(text);
    xmp_free(xmp);
    return ok ? lean_io_result_mk_ok(result) : failure("The XMP packet could not be written");
}

/* One simple XMP property (SCHEMA is a namespace URI, so prefixes do not
   matter), or none. */
lean_obj_res vellumory_native_xmp_property(b_lean_obj_arg packet, b_lean_obj_arg schema, b_lean_obj_arg name,
        lean_obj_arg world) {
    (void)world;
    pthread_once(&xmp_once, xmp_start);
    if (!xmp_ready) return failure("The XMP toolkit could not start");
    size_t size = lean_sarray_size(packet);
    if (!size) return lean_io_result_mk_ok(lean_box(0));
    XmpPtr xmp = xmp_new((const char *)lean_sarray_cptr(packet), size);
    if (!xmp) return lean_io_result_mk_ok(lean_box(0));
    XmpStringPtr value = xmp_string_new();
    lean_object *result = lean_box(0);
    if (xmp_get_property(xmp, lean_string_cstr(schema), lean_string_cstr(name), value, NULL)) {
        result = lean_alloc_ctor(1, 1, 0);
        lean_ctor_set(result, 0, lean_mk_string(xmp_string_cstr(value)));
    }
    xmp_string_free(value);
    xmp_free(xmp);
    return lean_io_result_mk_ok(result);
}

/* Info fields a document keeps only in XMP (PDF 2.0 deprecates the Info
   dictionary): title, author, subject, keywords, creator, producer and the
   creation and modification dates, in that order. */
static void xmp_info_fallback(fz_context *ctx, pdf_document *pdf, char out[8][1024], bool have[8]) {
    fz_buffer *buffer = NULL;
    fz_var(buffer);
    fz_try(ctx) {
        pdf_obj *root = pdf_dict_get(ctx, pdf_trailer(ctx, pdf), PDF_NAME(Root));
        pdf_obj *stream = pdf_dict_get(ctx, root, PDF_NAME(Metadata));
        if (stream && pdf_is_stream(ctx, stream)) buffer = pdf_load_stream(ctx, stream);
    }
    fz_catch(ctx) { fz_report_error(ctx); return; }
    if (!buffer) return;
    unsigned char *data = NULL;
    size_t length = fz_buffer_storage(ctx, buffer, &data);
    pthread_once(&xmp_once, xmp_start);
    XmpPtr xmp = xmp_ready && length ? xmp_new((const char *)data, length) : NULL;
    fz_drop_buffer(ctx, buffer);
    if (!xmp) return;
    XmpStringPtr value = xmp_string_new();
    for (int i = 0; i < 8; i++) {
        bool found = false;
        switch (i) {
        case 0: found = xmp_get_localized_text(xmp, NS_DC, "title", "", "x-default", NULL, value, NULL); break;
        case 1: found = xmp_get_array_item(xmp, NS_DC, "creator", 1, value, NULL); break;
        case 2: found = xmp_get_localized_text(xmp, NS_DC, "description", "", "x-default", NULL, value, NULL); break;
        case 3: found = xmp_get_property(xmp, NS_PDF, "Keywords", value, NULL); break;
        case 4: found = xmp_get_property(xmp, NS_XAP, "CreatorTool", value, NULL); break;
        case 5: found = xmp_get_property(xmp, NS_PDF, "Producer", value, NULL); break;
        case 6: found = xmp_get_property(xmp, NS_XAP, "CreateDate", value, NULL); break;
        default: found = xmp_get_property(xmp, NS_XAP, "ModifyDate", value, NULL); break;
        }
        if (found) { snprintf(out[i], 1024, "%s", xmp_string_cstr(value)); have[i] = true; }
    }
    xmp_string_free(value);
    xmp_free(xmp);
}

/* ---- Annotations. Coordinates are points relative to the page's crop box,
   as everywhere in Vellumory; links, popups and form widgets are listed
   elsewhere (links, info) or are not annotations a reader writes. */

lean_obj_res vellumory_native_annotations(b_lean_obj_arg handle, uint32_t number, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    if (number >= (uint32_t)d->count) return failure("Page outside document");
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    jb b = {0};
    pdf_page *page = NULL;
    lean_object *result = NULL;
    fz_var(page); fz_var(result);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        b.out = fz_new_buffer(ctx, 1024);
        fz_rect box = d->bounds[number];
        pdf_document *pdf = pdf_document_from_fz_document(ctx, d->doc);
        jb_open(ctx, &b, '[');
        if (pdf) {
            page = pdf_load_page(ctx, pdf, (int)number);
            unsigned count = 0;
            for (pdf_annot *a = pdf_first_annot(ctx, page); a && count < 4096; a = pdf_next_annot(ctx, a)) {
                enum pdf_annot_type type = pdf_annot_type(ctx, a);
                if (type == PDF_ANNOT_LINK || type == PDF_ANNOT_POPUP || type == PDF_ANNOT_WIDGET) continue;
                ++count;
                pdf_obj *obj = pdf_annot_obj(ctx, a);
                fz_rect r = pdf_bound_annot(ctx, a);
                jb_open(ctx, &b, '{');
                jb_kint(ctx, &b, "object", pdf_to_num(ctx, obj));
                jb_kstr(ctx, &b, "type", pdf_string_from_annot_type(ctx, type));
                jb_key(ctx, &b, "rect");
                jb_open(ctx, &b, '[');
                jb_num(ctx, &b, r.x0 - box.x0); jb_num(ctx, &b, r.y0 - box.y0);
                jb_num(ctx, &b, r.x1 - box.x0); jb_num(ctx, &b, r.y1 - box.y0);
                jb_close(ctx, &b, ']');
                jb_kstr(ctx, &b, "contents", pdf_dict_get_text_string(ctx, obj, PDF_NAME(Contents)));
                jb_kstr(ctx, &b, "author", pdf_dict_get_text_string(ctx, obj, PDF_NAME(T)));
                jb_kstr(ctx, &b, "modified", pdf_dict_get_text_string(ctx, obj, PDF_NAME(M)));
                pdf_obj *color = pdf_dict_get(ctx, obj, PDF_NAME(C));
                jb_key(ctx, &b, "color");
                jb_open(ctx, &b, '[');
                for (int i = 0; i < pdf_array_len(ctx, color) && i < 4; ++i) jb_num(ctx, &b, pdf_array_get_real(ctx, color, i));
                jb_close(ctx, &b, ']');
                jb_key(ctx, &b, "quads");
                jb_open(ctx, &b, '[');
                if (pdf_annot_has_quad_points(ctx, a)) {
                    int n = pdf_annot_quad_point_count(ctx, a);
                    for (int i = 0; i < n && i < 4096; ++i) {
                        fz_quad q = pdf_annot_quad_point(ctx, a, i);
                        jb_open(ctx, &b, '[');
                        jb_num(ctx, &b, q.ul.x - box.x0); jb_num(ctx, &b, q.ul.y - box.y0);
                        jb_num(ctx, &b, q.ur.x - box.x0); jb_num(ctx, &b, q.ur.y - box.y0);
                        /* Clockwise, as Vellumory's quads are everywhere. */
                        jb_num(ctx, &b, q.lr.x - box.x0); jb_num(ctx, &b, q.lr.y - box.y0);
                        jb_num(ctx, &b, q.ll.x - box.x0); jb_num(ctx, &b, q.ll.y - box.y0);
                        jb_close(ctx, &b, ']');
                    }
                }
                jb_close(ctx, &b, ']');
                jb_close(ctx, &b, '}');
            }
        }
        jb_close(ctx, &b, ']');
        unsigned char *data;
        size_t n = fz_buffer_storage(ctx, b.out, &data);
        result = lean_mk_string_from_bytes((char *)data, n);
    }
    fz_always(ctx) {
        fz_drop_page(ctx, (fz_page *)page);
        fz_drop_buffer(ctx, b.out);
        pthread_mutex_unlock(&d->doc_lock);
    }
    fz_catch(ctx) { return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

/* Save DOC to OUTPUT, appending only what changed when the file allows it;
   qpdf rewrites the result afterwards like every other edit. */
static void annot_save(fz_context *ctx, pdf_document *doc, const char *output) {
    pdf_write_options opts = pdf_default_write_options;
    opts.do_incremental = pdf_can_be_saved_incrementally(ctx, doc);
    pdf_save_document(ctx, doc, output, &opts);
}

static pdf_annot *annot_find(fz_context *ctx, pdf_page *page, int object) {
    for (pdf_annot *a = pdf_first_annot(ctx, page); a; a = pdf_next_annot(ctx, a))
        if (pdf_to_num(ctx, pdf_annot_obj(ctx, a)) == object) return a;
    fz_throw(ctx, FZ_ERROR_ARGUMENT, "No annotation %d on this page", object);
}

lean_obj_res vellumory_native_annot_add(b_lean_obj_arg input, b_lean_obj_arg output, uint32_t number,
        b_lean_obj_arg kind, b_lean_obj_arg quads, b_lean_obj_arg rect, b_lean_obj_arg color,
        b_lean_obj_arg contents, b_lean_obj_arg author, double time, lean_obj_arg world) {
    (void)world;
    fz_context *ctx = fz_new_context(NULL, NULL, FZ_STORE_DEFAULT);
    if (!ctx) return failure("Cannot create MuPDF context");
    pdf_document *doc = NULL;
    pdf_page *page = NULL;
    pdf_annot *annot = NULL;
    int object = 0;
    char message[256] = "";
    fz_var(doc); fz_var(page); fz_var(annot); fz_var(object);
    fz_try(ctx) {
        doc = pdf_open_document(ctx, lean_string_cstr(input));
        if ((int)number >= pdf_count_pages(ctx, doc)) fz_throw(ctx, FZ_ERROR_ARGUMENT, "Page outside document");
        page = pdf_load_page(ctx, doc, (int)number);
        fz_rect box = pdf_bound_page(ctx, page, FZ_CROP_BOX);
        enum pdf_annot_type type = pdf_annot_type_from_string(ctx, lean_string_cstr(kind));
        if (type == PDF_ANNOT_UNKNOWN) fz_throw(ctx, FZ_ERROR_ARGUMENT, "Unknown annotation type");
        annot = pdf_create_annot(ctx, page, type);
        const double *q = lean_float_array_cptr(quads);
        size_t nq = lean_sarray_size(quads) / 8;
        for (size_t i = 0; i < nq; ++i, q += 8) {
            /* Vellumory's quads run clockwise (upper left, upper right, lower right,
               lower left); fz_quad is upper left, upper right, lower left, lower right. */
            fz_quad quad = {{q[0] + box.x0, q[1] + box.y0}, {q[2] + box.x0, q[3] + box.y0},
                            {q[6] + box.x0, q[7] + box.y0}, {q[4] + box.x0, q[5] + box.y0}};
            pdf_add_annot_quad_point(ctx, annot, quad);
        }
        if (lean_sarray_size(rect) == 4) {
            const double *r = lean_float_array_cptr(rect);
            pdf_set_annot_rect(ctx, annot, fz_make_rect(r[0] + box.x0, r[1] + box.y0, r[2] + box.x0, r[3] + box.y0));
        }
        if (lean_sarray_size(color) == 3) {
            const double *c = lean_float_array_cptr(color);
            float rgb[3] = {(float)c[0], (float)c[1], (float)c[2]};
            pdf_set_annot_color(ctx, annot, 3, rgb);
        }
        pdf_set_annot_contents(ctx, annot, lean_string_cstr(contents));
        if (*lean_string_cstr(author)) pdf_set_annot_author(ctx, annot, lean_string_cstr(author));
        /* Printable, as PDF/A requires of every annotation. */
        pdf_set_annot_flags(ctx, annot, PDF_ANNOT_IS_PRINT);
        pdf_set_annot_creation_date(ctx, annot, (int64_t)time);
        pdf_set_annot_modification_date(ctx, annot, (int64_t)time);
        pdf_update_annot(ctx, annot);
        /* A note's popup is an annotation too: PDF/A wants it printable (ISO
           19005-1 6.5.3; veraPDF failed it otherwise). */
        pdf_obj *popup = pdf_dict_get(ctx, pdf_annot_obj(ctx, annot), PDF_NAME(Popup));
        if (popup) pdf_dict_put_int(ctx, popup, PDF_NAME(F), PDF_ANNOT_IS_PRINT);
        object = pdf_to_num(ctx, pdf_annot_obj(ctx, annot));
        annot_save(ctx, doc, lean_string_cstr(output));
    }
    fz_always(ctx) {
        pdf_drop_annot(ctx, annot);
        fz_drop_page(ctx, (fz_page *)page);
        pdf_drop_document(ctx, doc);
    }
    fz_catch(ctx) snprintf(message, sizeof message, "%s", fz_convert_error(ctx, NULL));
    fz_drop_context(ctx);
    return message[0] ? failure(message) : lean_io_result_mk_ok(lean_box_uint32((uint32_t)object));
}

/* Edit (REMOVE false: set CONTENTS) or delete one annotation, by object number. */
lean_obj_res vellumory_native_annot_change(b_lean_obj_arg input, b_lean_obj_arg output, uint32_t number,
        uint32_t object, uint8_t remove, b_lean_obj_arg contents, double time, lean_obj_arg world) {
    (void)world;
    fz_context *ctx = fz_new_context(NULL, NULL, FZ_STORE_DEFAULT);
    if (!ctx) return failure("Cannot create MuPDF context");
    pdf_document *doc = NULL;
    pdf_page *page = NULL;
    char message[256] = "";
    fz_var(doc); fz_var(page);
    fz_try(ctx) {
        doc = pdf_open_document(ctx, lean_string_cstr(input));
        if ((int)number >= pdf_count_pages(ctx, doc)) fz_throw(ctx, FZ_ERROR_ARGUMENT, "Page outside document");
        page = pdf_load_page(ctx, doc, (int)number);
        pdf_annot *annot = annot_find(ctx, page, (int)object);
        if (remove) pdf_delete_annot(ctx, page, annot);
        else {
            pdf_set_annot_contents(ctx, annot, lean_string_cstr(contents));
            pdf_set_annot_modification_date(ctx, annot, (int64_t)time);
            pdf_update_annot(ctx, annot);
        }
        annot_save(ctx, doc, lean_string_cstr(output));
    }
    fz_always(ctx) {
        fz_drop_page(ctx, (fz_page *)page);
        pdf_drop_document(ctx, doc);
    }
    fz_catch(ctx) snprintf(message, sizeof message, "%s", fz_convert_error(ctx, NULL));
    fz_drop_context(ctx);
    return message[0] ? failure(message) : lean_io_result_mk_ok(lean_box(0));
}

/* ---- Digital signatures: found through the AcroForm field tree (no page
   loading), verified with OpenSSL's CMS against the signed byte ranges of the
   immutable document bytes. "intact": the signed bytes match the signature;
   "trusted": the signer's certificate chains to the system's trust store. */
#include <openssl/cms.h>
#include <openssl/bio.h>
#include <openssl/x509.h>
#include <openssl/x509_vfy.h>
#include <openssl/err.h>

#define SIGNATURES_MAX 256

static void sig_collect(fz_context *ctx, pdf_obj *field, pdf_obj *ft, pdf_obj **out, int *n, int depth) {
    if (!field || depth > 32 || *n >= SIGNATURES_MAX) return;
    pdf_obj *own = pdf_dict_get(ctx, field, PDF_NAME(FT));
    if (own) ft = own;
    pdf_obj *kids = pdf_dict_get(ctx, field, PDF_NAME(Kids));
    int has_field_kids = 0;
    for (int i = 0; i < pdf_array_len(ctx, kids); ++i)
        if (pdf_dict_get(ctx, pdf_array_get(ctx, kids, i), PDF_NAME(T))) has_field_kids = 1;
    if (has_field_kids) {
        for (int i = 0; i < pdf_array_len(ctx, kids); ++i)
            sig_collect(ctx, pdf_array_get(ctx, kids, i), ft, out, n, depth + 1);
    } else if (pdf_name_eq(ctx, ft, PDF_NAME(Sig))) {
        out[(*n)++] = field;
    }
}

static int sig_fields(fz_context *ctx, pdf_document *pdf, pdf_obj **out) {
    int n = 0;
    pdf_obj *fields = pdf_dict_getp(ctx, pdf_trailer(ctx, pdf), "Root/AcroForm/Fields");
    for (int i = 0; i < pdf_array_len(ctx, fields); ++i)
        sig_collect(ctx, pdf_array_get(ctx, fields, i), NULL, out, &n, 0);
    return n;
}

/* Signed signature fields: cheap enough to report on every open. */
lean_obj_res vellumory_native_signed_count(b_lean_obj_arg handle, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    uint32_t count = 0;
    fz_var(count);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        pdf_document *pdf = pdf_document_from_fz_document(ctx, d->doc);
        if (pdf) {
            pdf_obj *fields[SIGNATURES_MAX];
            int n = sig_fields(ctx, pdf, fields);
            for (int i = 0; i < n; ++i)
                if (pdf_dict_get(ctx, fields[i], PDF_NAME(V))) ++count;
        }
    }
    fz_always(ctx) pthread_mutex_unlock(&d->doc_lock);
    fz_catch(ctx) { return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(lean_box_uint32(count));
}

/* Verify DER (detached CMS) over DATA; with STORE, also the certificate chain.
   Returns 1 if verified; on failure writes OpenSSL's reason to ERROR. */
static int sig_cms_verify(const unsigned char *der, size_t der_len, const unsigned char *data, size_t data_len,
                          X509_STORE *store, char *error, size_t error_len, char *signer, size_t signer_len) {
    const unsigned char *p = der;
    CMS_ContentInfo *cms = d2i_CMS_ContentInfo(NULL, &p, (long)der_len);
    if (!cms) { snprintf(error, error_len, "not a CMS signature"); return 0; }
    if (signer && signer_len) {
        STACK_OF(X509) *certs = CMS_get1_certs(cms);
        STACK_OF(CMS_SignerInfo) *infos = CMS_get0_SignerInfos(cms);
        signer[0] = 0;
        if (certs && infos && sk_CMS_SignerInfo_num(infos) > 0) {
            CMS_SignerInfo *si = sk_CMS_SignerInfo_value(infos, 0);
            for (int i = 0; i < sk_X509_num(certs); ++i) {
                X509 *cert = sk_X509_value(certs, i);
                if (CMS_SignerInfo_cert_cmp(si, cert) == 0) {
                    X509_NAME_oneline(X509_get_subject_name(cert), signer, (int)signer_len);
                    break;
                }
            }
        }
        if (certs) sk_X509_pop_free(certs, X509_free);
    }
    BIO *content = BIO_new_mem_buf(data, (int)data_len);
    ERR_clear_error();
    int ok = CMS_verify(cms, NULL, store, content, NULL,
                        CMS_BINARY | (store ? 0 : CMS_NO_SIGNER_CERT_VERIFY)) == 1;
    if (!ok) {
        unsigned long code = ERR_peek_last_error();
        if (code) ERR_error_string_n(code, error, error_len);
        else snprintf(error, error_len, "verification failed");
    }
    BIO_free(content);
    CMS_ContentInfo_free(cms);
    return ok;
}

/* Why the signer's certificate does not chain to STORE, in OpenSSL's words
   (e.g. "self-signed certificate"), or "" when it does. */
static void sig_trust(const unsigned char *der, size_t der_len, X509_STORE *store, char *error, size_t error_len) {
    error[0] = 0;
    const unsigned char *p = der;
    CMS_ContentInfo *cms = d2i_CMS_ContentInfo(NULL, &p, (long)der_len);
    if (!cms) { snprintf(error, error_len, "not a CMS signature"); return; }
    STACK_OF(X509) *certs = CMS_get1_certs(cms);
    STACK_OF(CMS_SignerInfo) *infos = CMS_get0_SignerInfos(cms);
    X509 *signer = NULL;
    if (certs && infos && sk_CMS_SignerInfo_num(infos) > 0)
        for (int i = 0; i < sk_X509_num(certs) && !signer; ++i)
            if (CMS_SignerInfo_cert_cmp(sk_CMS_SignerInfo_value(infos, 0), sk_X509_value(certs, i)) == 0)
                signer = sk_X509_value(certs, i);
    if (!signer) snprintf(error, error_len, "the signer's certificate is not included");
    else {
        X509_STORE_CTX *verify = X509_STORE_CTX_new();
        if (verify && X509_STORE_CTX_init(verify, store, signer, certs) == 1) {
            X509_STORE_CTX_set_purpose(verify, X509_PURPOSE_ANY);
            if (X509_verify_cert(verify) != 1)
                snprintf(error, error_len, "%s", X509_verify_cert_error_string(X509_STORE_CTX_get_error(verify)));
        } else snprintf(error, error_len, "cannot check the certificate");
        X509_STORE_CTX_free(verify);
    }
    if (certs) sk_X509_pop_free(certs, X509_free);
    CMS_ContentInfo_free(cms);
}

/* One signature field's report; a broken signature reports its error instead
   of stopping the others. */
static void sig_report(fz_context *ctx, jb *b, pdf_document *pdf, pdf_obj *field,
                       const unsigned char *file, size_t file_len, X509_STORE *store) {
    fz_range *ranges = NULL;
    char *contents = NULL;
    unsigned char *signed_data = NULL;
    char error[256] = "", trust_error[256] = "", signer[512] = "";
    int intact = 0, trusted = 0, whole = 0, changed = 0;
    fz_var(ranges); fz_var(contents); fz_var(signed_data); fz_var(intact); fz_var(trusted);
    fz_var(whole); fz_var(changed);
    pdf_obj *value = pdf_dict_get(ctx, field, PDF_NAME(V));
    const char *subfilter = pdf_to_name(ctx, pdf_dict_get(ctx, value, PDF_NAME(SubFilter)));
    fz_try(ctx) {
        /* Who signed is in the signature itself, even when its bytes no longer match. */
        size_t der_len = pdf_signature_contents(ctx, pdf, field, &contents);
        if (der_len) {
            char ignored[8];
            sig_cms_verify((unsigned char *)contents, der_len, (const unsigned char *)"", 0, NULL,
                           ignored, sizeof ignored, signer, sizeof signer);
        }
        int count = pdf_signature_byte_range(ctx, pdf, field, NULL);
        if (count <= 0) fz_throw(ctx, FZ_ERROR_FORMAT, "no signed byte range");
        ranges = fz_malloc_array(ctx, count, fz_range);
        pdf_signature_byte_range(ctx, pdf, field, ranges);
        size_t total = 0, end = 0;
        for (int r = 0; r < count; ++r) {
            if (ranges[r].offset < 0 || (uint64_t)ranges[r].offset + ranges[r].length > file_len)
                fz_throw(ctx, FZ_ERROR_FORMAT, "byte range outside the file");
            total += (size_t)ranges[r].length;
            end = (size_t)(ranges[r].offset + ranges[r].length);
        }
        whole = end == file_len;
        changed = pdf_signature_incremental_change_since_signing(ctx, pdf, field);
        if (strcmp(subfilter, "adbe.pkcs7.detached") && strcmp(subfilter, "ETSI.CAdES.detached"))
            fz_throw(ctx, FZ_ERROR_UNSUPPORTED, "unsupported signature format %s", subfilter);
        if (!der_len) fz_throw(ctx, FZ_ERROR_FORMAT, "no signature contents");
        signed_data = fz_malloc(ctx, total ? total : 1);
        size_t at = 0;
        for (int r = 0; r < count; ++r) {
            memcpy(signed_data + at, file + ranges[r].offset, (size_t)ranges[r].length);
            at += (size_t)ranges[r].length;
        }
        intact = sig_cms_verify((unsigned char *)contents, der_len, signed_data, total, NULL,
                                error, sizeof error, signer, sizeof signer);
        if (intact && store) {
            sig_trust((unsigned char *)contents, der_len, store, trust_error, sizeof trust_error);
            trusted = trust_error[0] == 0;
        }
    }
    fz_always(ctx) {
        fz_free(ctx, ranges); fz_free(ctx, contents); fz_free(ctx, signed_data);
    }
    fz_catch(ctx) {
        snprintf(error, sizeof error, "%s", fz_convert_error(ctx, NULL));
        intact = trusted = 0;
    }
    jb_kstr(ctx, b, "name", pdf_dict_get_text_string(ctx, value, PDF_NAME(Name)));
    jb_kstr(ctx, b, "reason", pdf_dict_get_text_string(ctx, value, PDF_NAME(Reason)));
    jb_kstr(ctx, b, "location", pdf_dict_get_text_string(ctx, value, PDF_NAME(Location)));
    jb_kstr(ctx, b, "time", pdf_dict_get_text_string(ctx, value, PDF_NAME(M)));
    jb_kstr(ctx, b, "subfilter", subfilter);
    jb_kbool(ctx, b, "covers_whole_file", whole);
    jb_kbool(ctx, b, "changed_after_signing", changed);
    jb_kstr(ctx, b, "signer", signer);
    jb_kbool(ctx, b, "intact", intact);
    jb_kbool(ctx, b, "trusted", trusted);
    jb_kstr(ctx, b, "error", error);
    jb_kstr(ctx, b, "trust_error", trust_error);
}

lean_obj_res vellumory_native_signatures(b_lean_obj_arg handle, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    jb b = {0};
    lean_object *result = NULL;
    X509_STORE *store = NULL;
    fz_var(result); fz_var(store);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        b.out = fz_new_buffer(ctx, 1024);
        jb_open(ctx, &b, '[');
        pdf_document *pdf = pdf_document_from_fz_document(ctx, d->doc);
        unsigned char *file = NULL;
        size_t file_len = fz_buffer_storage(ctx, d->source, &file);
        store = X509_STORE_new();
        if (store) X509_STORE_set_default_paths(store);
        pdf_obj *fields[SIGNATURES_MAX];
        int n = pdf ? sig_fields(ctx, pdf, fields) : 0;
        for (int i = 0; i < n; ++i) {
            pdf_obj *field = fields[i];
            jb_open(ctx, &b, '{');
            jb_kstr(ctx, &b, "field", pdf_dict_get_text_string(ctx, field, PDF_NAME(T)));
            pdf_obj *page = pdf_dict_get(ctx, field, PDF_NAME(P));
            jb_kint(ctx, &b, "page", page ? pdf_lookup_page_number(ctx, pdf, page) : -1);
            int is_signed = pdf_dict_get(ctx, field, PDF_NAME(V)) != NULL;
            jb_kbool(ctx, &b, "signed", is_signed);
            if (is_signed) sig_report(ctx, &b, pdf, field, file, file_len, store);
            jb_close(ctx, &b, '}');
        }
        jb_close(ctx, &b, ']');
        unsigned char *data;
        size_t len = fz_buffer_storage(ctx, b.out, &data);
        result = lean_mk_string_from_bytes((char *)data, len);
    }
    fz_always(ctx) {
        if (store) X509_STORE_free(store);
        fz_drop_buffer(ctx, b.out);
        pthread_mutex_unlock(&d->doc_lock);
    }
    fz_catch(ctx) { return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

/* ---- Forms (AcroForm). Fields are listed by walking the field tree (no page
   loading); filling sets values with JavaScript triggers ignored, so no
   document script ever runs, then regenerates the widgets' appearances. */

static const char *form_type_name(int type) {
    switch (type) {
    case PDF_WIDGET_TYPE_BUTTON: return "button";
    case PDF_WIDGET_TYPE_CHECKBOX: return "checkbox";
    case PDF_WIDGET_TYPE_COMBOBOX: return "combobox";
    case PDF_WIDGET_TYPE_LISTBOX: return "listbox";
    case PDF_WIDGET_TYPE_RADIOBUTTON: return "radio";
    case PDF_WIDGET_TYPE_SIGNATURE: return "signature";
    case PDF_WIDGET_TYPE_TEXT: return "text";
    default: return "unknown";
    }
}

#define FORM_FIELDS_MAX 4096

static void form_collect(fz_context *ctx, pdf_obj *field, pdf_obj **out, int *n, int depth) {
    if (!field || depth > 32 || *n >= FORM_FIELDS_MAX) return;
    pdf_obj *kids = pdf_dict_get(ctx, field, PDF_NAME(Kids));
    int has_field_kids = 0;
    for (int i = 0; i < pdf_array_len(ctx, kids); ++i)
        if (pdf_dict_get(ctx, pdf_array_get(ctx, kids, i), PDF_NAME(T))) has_field_kids = 1;
    if (has_field_kids)
        for (int i = 0; i < pdf_array_len(ctx, kids); ++i) form_collect(ctx, pdf_array_get(ctx, kids, i), out, n, depth + 1);
    else out[(*n)++] = field;
}

/* The appearance states other than Off of FIELD's widgets: what a checkbox or
   radio button is set to when chosen. */
static void form_on_states(fz_context *ctx, jb *b, pdf_obj *field) {
    pdf_obj *kids = pdf_dict_get(ctx, field, PDF_NAME(Kids));
    int count = pdf_array_len(ctx, kids);
    jb_key(ctx, b, "states");
    jb_open(ctx, b, '[');
    for (int k = 0; k < (count ? count : 1); ++k) {
        pdf_obj *widget = count ? pdf_array_get(ctx, kids, k) : field;
        pdf_obj *normal = pdf_dict_getp(ctx, widget, "AP/N");
        for (int i = 0; i < pdf_dict_len(ctx, normal); ++i) {
            pdf_obj *key = pdf_dict_get_key(ctx, normal, i);
            if (!pdf_name_eq(ctx, key, PDF_NAME(Off))) jb_str(ctx, b, pdf_to_name(ctx, key));
        }
    }
    jb_close(ctx, b, ']');
}

static int form_page(fz_context *ctx, pdf_document *pdf, pdf_obj *field) {
    pdf_obj *kids = pdf_dict_get(ctx, field, PDF_NAME(Kids));
    pdf_obj *widget = pdf_array_len(ctx, kids) > 0 ? pdf_array_get(ctx, kids, 0) : field;
    pdf_obj *page = pdf_dict_get(ctx, widget, PDF_NAME(P));
    if (page) return pdf_lookup_page_number(ctx, pdf, page);
    /* Widgets need not name their page (hyperref's do not): find the page
       whose annotations list the widget, without loading pages. */
    int count = pdf_count_pages(ctx, pdf);
    for (int p = 0; p < count; ++p) {
        pdf_obj *annots = pdf_dict_get(ctx, pdf_lookup_page_obj(ctx, pdf, p), PDF_NAME(Annots));
        for (int i = 0; i < pdf_array_len(ctx, annots); ++i)
            if (!pdf_objcmp(ctx, pdf_array_get(ctx, annots, i), widget)) return p;
    }
    return -1;
}

lean_obj_res vellumory_native_form_fields(b_lean_obj_arg handle, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    jb b = {0};
    lean_object *result = NULL;
    pdf_obj **fields = NULL;
    fz_var(result); fz_var(fields);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        b.out = fz_new_buffer(ctx, 1024);
        jb_open(ctx, &b, '[');
        pdf_document *pdf = pdf_document_from_fz_document(ctx, d->doc);
        if (pdf) {
            fields = fz_malloc_array(ctx, FORM_FIELDS_MAX, pdf_obj *);
            int n = 0;
            pdf_obj *root = pdf_dict_getp(ctx, pdf_trailer(ctx, pdf), "Root/AcroForm/Fields");
            for (int i = 0; i < pdf_array_len(ctx, root); ++i) form_collect(ctx, pdf_array_get(ctx, root, i), fields, &n, 0);
            for (int i = 0; i < n; ++i) {
                pdf_obj *field = fields[i];
                int type = pdf_field_type(ctx, field);
                int flags = pdf_field_flags(ctx, field);
                char *name = pdf_load_field_name(ctx, field);
                jb_open(ctx, &b, '{');
                jb_kstr(ctx, &b, "name", name);
                fz_free(ctx, name);
                jb_kstr(ctx, &b, "type", form_type_name(type));
                jb_kstr(ctx, &b, "value", pdf_field_value(ctx, field));
                jb_kstr(ctx, &b, "label", pdf_dict_get_text_string(ctx, field, PDF_NAME(TU)));
                jb_kbool(ctx, &b, "read_only", (flags & PDF_FIELD_IS_READ_ONLY) != 0);
                jb_kbool(ctx, &b, "required", (flags & PDF_FIELD_IS_REQUIRED) != 0);
                int page = form_page(ctx, pdf, field);
                jb_kint(ctx, &b, "page", page);
                if (page >= 0 && page < d->count) {
                    /* The first widget's rectangle, in points from the crop box's corner. */
                    pdf_obj *kids = pdf_dict_get(ctx, field, PDF_NAME(Kids));
                    pdf_obj *widget = pdf_array_len(ctx, kids) > 0 ? pdf_array_get(ctx, kids, 0) : field;
                    fz_rect box; fz_matrix ctm;
                    pdf_page_obj_transform(ctx, pdf_lookup_page_obj(ctx, pdf, page), &box, &ctm);
                    fz_rect r = fz_transform_rect(pdf_to_rect(ctx, pdf_dict_get(ctx, widget, PDF_NAME(Rect))), ctm);
                    fz_rect origin = d->bounds[page];
                    jb_key(ctx, &b, "rect");
                    jb_open(ctx, &b, '[');
                    jb_num(ctx, &b, r.x0 - origin.x0); jb_num(ctx, &b, r.y0 - origin.y0);
                    jb_num(ctx, &b, r.x1 - origin.x0); jb_num(ctx, &b, r.y1 - origin.y0);
                    jb_close(ctx, &b, ']');
                }
                if (type == PDF_WIDGET_TYPE_COMBOBOX || type == PDF_WIDGET_TYPE_LISTBOX) {
                    jb_key(ctx, &b, "options");
                    jb_open(ctx, &b, '[');
                    int count = pdf_choice_field_option_count(ctx, field);
                    for (int o = 0; o < count && o < 1024; ++o) jb_str(ctx, &b, pdf_choice_field_option(ctx, field, 1, o));
                    jb_close(ctx, &b, ']');
                }
                if (type == PDF_WIDGET_TYPE_CHECKBOX || type == PDF_WIDGET_TYPE_RADIOBUTTON) form_on_states(ctx, &b, field);
                jb_close(ctx, &b, '}');
            }
        }
        jb_close(ctx, &b, ']');
        unsigned char *data;
        size_t len = fz_buffer_storage(ctx, b.out, &data);
        result = lean_mk_string_from_bytes((char *)data, len);
    }
    fz_always(ctx) {
        fz_free(ctx, fields);
        fz_drop_buffer(ctx, b.out);
        pthread_mutex_unlock(&d->doc_lock);
    }
    fz_catch(ctx) { return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

/* Set the fields NAMES to VALUES in INPUT, writing OUTPUT. */
lean_obj_res vellumory_native_fill_form(b_lean_obj_arg input, b_lean_obj_arg output, b_lean_obj_arg names,
        b_lean_obj_arg values, lean_obj_arg world) {
    (void)world;
    size_t count = lean_array_size(names);
    if (lean_array_size(values) != count) return failure("Field names and values differ in number");
    fz_context *ctx = fz_new_context(NULL, NULL, FZ_STORE_DEFAULT);
    if (!ctx) return failure("Cannot create MuPDF context");
    pdf_document *doc = NULL;
    pdf_page *page = NULL;
    char message[256] = "";
    fz_var(doc); fz_var(page);
    fz_try(ctx) {
        doc = pdf_open_document(ctx, lean_string_cstr(input));
        pdf_obj *form = pdf_dict_getp(ctx, pdf_trailer(ctx, doc), "Root/AcroForm/Fields");
        if (!form) fz_throw(ctx, FZ_ERROR_ARGUMENT, "The document has no form");
        for (size_t i = 0; i < count; ++i) {
            const char *name = lean_string_cstr(lean_array_get_core(names, i));
            pdf_obj *field = pdf_lookup_field(ctx, form, name);
            if (!field) fz_throw(ctx, FZ_ERROR_ARGUMENT, "No form field %s", name);
            if (pdf_field_flags(ctx, field) & PDF_FIELD_IS_READ_ONLY)
                fz_throw(ctx, FZ_ERROR_ARGUMENT, "Form field %s is read-only", name);
            if (pdf_field_type(ctx, field) == PDF_WIDGET_TYPE_SIGNATURE)
                fz_throw(ctx, FZ_ERROR_ARGUMENT, "Form field %s is a signature", name);
            pdf_set_field_value(ctx, doc, field, lean_string_cstr(lean_array_get_core(values, i)), 1);
        }
        /* New appearances for the widgets whose values changed. */
        int pages = pdf_count_pages(ctx, doc);
        for (int p = 0; p < pages; ++p) {
            page = pdf_load_page(ctx, doc, p);
            /* Values set on field objects do not mark the page's widgets: ask for new
               appearances of the widgets of every filled field. */
            int dirty = 0;
            for (pdf_annot *w = pdf_first_widget(ctx, page); w; w = pdf_next_widget(ctx, w)) {
                char *name = pdf_load_field_name(ctx, pdf_annot_obj(ctx, w));
                for (size_t i = 0; i < count && name; ++i)
                    if (!strcmp(name, lean_string_cstr(lean_array_get_core(names, i)))) {
                        pdf_annot_request_resynthesis(ctx, w);
                        dirty = 1;
                    }
                fz_free(ctx, name);
            }
            if (dirty) pdf_update_page(ctx, page);
            fz_drop_page(ctx, (fz_page *)page);
            page = NULL;
        }
        annot_save(ctx, doc, lean_string_cstr(output));
    }
    fz_always(ctx) {
        fz_drop_page(ctx, (fz_page *)page);
        pdf_drop_document(ctx, doc);
    }
    fz_catch(ctx) snprintf(message, sizeof message, "%s", fz_convert_error(ctx, NULL));
    fz_drop_context(ctx);
    return message[0] ? failure(message) : lean_io_result_mk_ok(lean_box(0));
}

/* The catalog's /PageLayout and /PageMode names, space-separated ("" when
   absent): how the document asks to be displayed. */
lean_obj_res vellumory_native_display_preferences(b_lean_obj_arg handle, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    char text[128] = " ";
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        pdf_document *pdf = pdf_document_from_fz_document(ctx, d->doc);
        if (pdf) {
            pdf_obj *root = pdf_dict_get(ctx, pdf_trailer(ctx, pdf), PDF_NAME(Root));
            snprintf(text, sizeof text, "%s %s", pdf_to_name(ctx, pdf_dict_gets(ctx, root, "PageLayout")),
                     pdf_to_name(ctx, pdf_dict_gets(ctx, root, "PageMode")));
        }
    }
    fz_always(ctx) pthread_mutex_unlock(&d->doc_lock);
    fz_catch(ctx) { return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(lean_mk_string(text));
}

/* The page's characters in reading order, for a keyboard caret:
   {"lines":[{"block":B,"box":[x0,y0,x1,y1],"chars":[[TEXT,SX,SY,EX,EY],...]},...]}
   with each character's logical start and end points (as selections use them)
   in points from the page's top left. */
lean_obj_res vellumory_native_page_chars(b_lean_obj_arg handle, uint32_t number, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    if (number >= (uint32_t)d->count) return failure("Page outside document");
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    jb b = {0};
    lean_object *result = NULL;
    fz_var(result);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        fz_stext_page *text = cached_text(ctx, d, number);
        fz_rect box = text->mediabox;
        long total = 0;
        int block_index = 0;
        b.out = fz_new_buffer(ctx, 4096);
        jb_open(ctx, &b, '{');
        jb_key(ctx, &b, "lines");
        jb_open(ctx, &b, '[');
        for (fz_stext_block *block = text->first_block; block; block = block->next) {
            if (block->type != FZ_STEXT_BLOCK_TEXT) continue;
            for (fz_stext_line *line = block->u.t.first_line; line; line = line->next) {
                if (!line->first_char) continue;
                jb_open(ctx, &b, '{');
                jb_kint(ctx, &b, "block", block_index);
                jb_key(ctx, &b, "box");
                jb_open(ctx, &b, '[');
                jb_num(ctx, &b, line->bbox.x0 - box.x0); jb_num(ctx, &b, line->bbox.y0 - box.y0);
                jb_num(ctx, &b, line->bbox.x1 - box.x0); jb_num(ctx, &b, line->bbox.y1 - box.y0);
                jb_close(ctx, &b, ']');
                jb_key(ctx, &b, "chars");
                jb_open(ctx, &b, '[');
                for (fz_stext_char *ch = line->first_char; ch; ch = ch->next) {
                    if (++total > 200000) fz_throw(ctx, FZ_ERROR_LIMIT, "Page has too many characters");
                    if (!finite_quad(ch->quad)) continue;
                    char utf8[FZ_UTFMAX + 1];
                    utf8[fz_runetochar(utf8, ch->c)] = 0;
                    fz_point start = character_boundary(ch, 0), end = character_boundary(ch, 1);
                    jb_open(ctx, &b, '[');
                    jb_str(ctx, &b, utf8);
                    jb_num(ctx, &b, start.x - box.x0); jb_num(ctx, &b, start.y - box.y0);
                    jb_num(ctx, &b, end.x - box.x0); jb_num(ctx, &b, end.y - box.y0);
                    jb_close(ctx, &b, ']');
                }
                jb_close(ctx, &b, ']');
                jb_close(ctx, &b, '}');
            }
            ++block_index;
        }
        jb_close(ctx, &b, ']');
        jb_close(ctx, &b, '}');
        unsigned char *data = NULL;
        size_t n = fz_buffer_storage(ctx, b.out, &data);
        result = lean_mk_string_from_bytes((char *)data, n);
    }
    fz_always(ctx) { fz_drop_buffer(ctx, b.out); pthread_mutex_unlock(&d->doc_lock); }
    fz_catch(ctx) { return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

#include <sys/statvfs.h>

/* Bytes available to unprivileged writers on the filesystem holding PATH (0 if
   unknown): the worker keeps its rasters within a share of it. */
lean_obj_res vellumory_native_free_bytes(b_lean_obj_arg path, lean_obj_arg world) {
    (void)world;
    struct statvfs s;
    uint64_t free = 0;
    if (statvfs(lean_string_cstr(path), &s) == 0) free = (uint64_t)s.f_bavail * (uint64_t)s.f_frsize;
    return lean_io_result_mk_ok(lean_box_uint64(free));
}

/* Whether DEST (an explicit array, a named destination, or a dict with /D)
   leads to a page of this document; MuPDF's link resolution falls back to the
   first page instead, which would hide a broken destination. */
static int destination_ok(fz_context *ctx, pdf_document *doc, pdf_obj *dest, int pages) {
    if (pdf_is_name(ctx, dest) || pdf_is_string(ctx, dest)) {
        dest = pdf_lookup_dest(ctx, doc, dest);
        if (!dest) return 0;
    }
    if (pdf_is_dict(ctx, dest)) dest = pdf_dict_get(ctx, dest, PDF_NAME(D));
    if (!pdf_is_array(ctx, dest)) return 0;
    pdf_obj *page = pdf_array_get(ctx, dest, 0);
    if (pdf_is_int(ctx, page)) return pdf_to_int(ctx, page) >= 0 && pdf_to_int(ctx, page) < pages;
    return pdf_is_dict(ctx, page) && pdf_lookup_page_number(ctx, doc, page) >= 0;
}

/* The destination of a link annotation or outline item, if it has one. */
static pdf_obj *goto_destination(fz_context *ctx, pdf_obj *item) {
    pdf_obj *dest = pdf_dict_get(ctx, item, PDF_NAME(Dest));
    if (dest) return dest;
    pdf_obj *action = pdf_dict_get(ctx, item, PDF_NAME(A));
    if (action && pdf_name_eq(ctx, pdf_dict_get(ctx, action, PDF_NAME(S)), PDF_NAME(GoTo)))
        return pdf_dict_get(ctx, action, PDF_NAME(D));
    return NULL;
}

static void broken_outline(fz_context *ctx, pdf_document *doc, pdf_obj *item, int pages, jb *b, int *budget) {
    for (; item && (*budget)-- > 0; item = pdf_dict_get(ctx, item, PDF_NAME(Next))) {
        pdf_obj *dest = goto_destination(ctx, item);
        if (dest && !destination_ok(ctx, doc, dest, pages))
            jb_str(ctx, b, pdf_dict_get_text_string(ctx, item, PDF_NAME(Title)));
        broken_outline(ctx, doc, pdf_dict_get(ctx, item, PDF_NAME(First)), pages, b, budget);
    }
}

/* {"links":[zero-based pages with a link that leads nowhere],"outline":[titles]} */
lean_obj_res vellumory_native_broken_destinations(b_lean_obj_arg handle, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    jb b = {0};
    lean_object *result = NULL;
    fz_var(result);
    pthread_mutex_lock(&d->doc_lock);
    fz_try(ctx) {
        b.out = fz_new_buffer(ctx, 256);
        jb_open(ctx, &b, '{');
        pdf_document *pdf = pdf_document_from_fz_document(ctx, d->doc);
        jb_key(ctx, &b, "links");
        jb_open(ctx, &b, '[');
        if (pdf) {
            int pages = pdf_count_pages(ctx, pdf);
            for (int p = 0; p < pages; ++p) {
                pdf_obj *annots = pdf_dict_get(ctx, pdf_lookup_page_obj(ctx, pdf, p), PDF_NAME(Annots));
                for (int i = 0; i < pdf_array_len(ctx, annots); ++i) {
                    pdf_obj *annot = pdf_array_get(ctx, annots, i);
                    if (!pdf_name_eq(ctx, pdf_dict_get(ctx, annot, PDF_NAME(Subtype)), PDF_NAME(Link))) continue;
                    pdf_obj *dest = goto_destination(ctx, annot);
                    if (dest && !destination_ok(ctx, pdf, dest, pages)) { jb_int(ctx, &b, p); break; }
                }
            }
        }
        jb_close(ctx, &b, ']');
        jb_key(ctx, &b, "outline");
        jb_open(ctx, &b, '[');
        if (pdf) {
            int budget = 100000;
            pdf_obj *outlines = pdf_dict_getp(ctx, pdf_trailer(ctx, pdf), "Root/Outlines");
            broken_outline(ctx, pdf, pdf_dict_get(ctx, outlines, PDF_NAME(First)), pdf_count_pages(ctx, pdf), &b, &budget);
        }
        jb_close(ctx, &b, ']');
        jb_close(ctx, &b, '}');
        unsigned char *data = NULL;
        size_t n = fz_buffer_storage(ctx, b.out, &data);
        result = lean_mk_string_from_bytes((char *)data, n);
    }
    fz_always(ctx) { fz_drop_buffer(ctx, b.out); pthread_mutex_unlock(&d->doc_lock); }
    fz_catch(ctx) { return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

/* Page transitions that blend two pages (ISO 32000-1 12.4.4.1, Table 162),
   one frame at a time, from two equally sized rasters (binary PPM):
   Dissolve reveals the new page cell by cell in a fixed random order, Glitter
   does so in a wide band sweeping in the direction Di, Fade mixes the two. */
static uint32_t mix32(uint32_t x) {
    x ^= x >> 16; x *= 0x7feb352du; x ^= x >> 15; x *= 0x846ca68bu; x ^= x >> 16;
    return x;
}

static int ppm_skip(const unsigned char *b, size_t n, size_t *i) {
    for (;;) {
        while (*i < n && (b[*i] == ' ' || b[*i] == '\t' || b[*i] == '\n' || b[*i] == '\r')) ++*i;
        if (*i < n && b[*i] == '#') { while (*i < n && b[*i] != '\n') ++*i; continue; }
        return *i < n;
    }
}

static int ppm_number(const unsigned char *b, size_t n, size_t *i, unsigned *out) {
    if (!ppm_skip(b, n, i) || b[*i] < '0' || b[*i] > '9') return 0;
    unsigned long v = 0;
    while (*i < n && b[*i] >= '0' && b[*i] <= '9') { v = v * 10 + (unsigned)(b[*i] - '0'); if (v > 65535) return 0; ++*i; }
    *out = (unsigned)v;
    return 1;
}

/* The pixels of a binary PPM file (8-bit RGB), or NULL; *bytes is the whole file. */
static unsigned char *ppm_read(const char *path, unsigned *w, unsigned *h, unsigned char **bytes) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;
    unsigned char *b = NULL;
    size_t n = 0, cap = 0, got;
    do {
        if (n == cap) {
            cap = cap ? cap * 2 : 1 << 20;
            unsigned char *grown = realloc(b, cap);
            if (!grown) { free(b); fclose(f); return NULL; }
            b = grown;
        }
        got = fread(b + n, 1, cap - n, f);
        n += got;
    } while (got > 0);
    fclose(f);
    size_t i = 2;
    unsigned maxval;
    if (n < 2 || b[0] != 'P' || b[1] != '6' || !ppm_number(b, n, &i, w) || !ppm_number(b, n, &i, h) ||
        !ppm_number(b, n, &i, &maxval) || maxval != 255 || i >= n) { free(b); return NULL; }
    ++i; /* the single whitespace before the pixels */
    if (n - i < (size_t)*w * *h * 3) { free(b); return NULL; }
    *bytes = b;
    return b + i;
}

/* FLY_* flags of a Fly transition: M O, Di None, B true; FRAME_ARGB writes the
   frame as an Emacs canvas reads its :file, native-endian ARGB32 words without
   a header, instead of a binary PPM. */
enum { FLY_OUTWARD = 1, FLY_UNDIRECTED = 2, FLY_OPAQUE = 4, FRAME_ARGB = 8 };

lean_obj_res vellumory_native_transition_frame(b_lean_obj_arg old_path, b_lean_obj_arg new_path,
                                               b_lean_obj_arg out_path, b_lean_obj_arg style_name,
                                               double progress, uint32_t direction, double scale_start,
                                               uint8_t fly, lean_obj_arg world) {
    (void)world;
    const char *style = lean_string_cstr(style_name);
    int dissolve = !strcmp(style, "Dissolve"), glitter = !strcmp(style, "Glitter"), fade = !strcmp(style, "Fade");
    int flying = !strcmp(style, "Fly");
    if (!dissolve && !glitter && !fade && !flying)
        return failure("composed transitions are Dissolve, Glitter, Fade and Fly");
    if (!(scale_start > 0.0) || scale_start > 64.0) scale_start = 1.0;
    if (!(progress >= 0.0)) progress = 0.0;
    if (progress > 1.0) progress = 1.0;
    unsigned ow, oh, nw, nh;
    unsigned char *old_file = NULL, *new_file = NULL;
    const unsigned char *old_px = ppm_read(lean_string_cstr(old_path), &ow, &oh, &old_file);
    const unsigned char *new_px = old_px ? ppm_read(lean_string_cstr(new_path), &nw, &nh, &new_file) : NULL;
    if (!old_px || !new_px || ow != nw || oh != nh || ow == 0 || oh == 0) {
        free(old_file); free(new_file);
        return failure("transition rasters are missing, unreadable or of different sizes");
    }
    char header[64];
    int header_length = snprintf(header, sizeof(header), "P6\n%u %u\n255\n", ow, oh);
    size_t pixels = (size_t)ow * oh;
    unsigned char *out = malloc((size_t)header_length + pixels * 3);
    if (!out) { free(old_file); free(new_file); return failure("out of memory"); }
    memcpy(out, header, (size_t)header_length);
    unsigned char *px = out + header_length;
    if (flying) {
        /* The moving layer: inward the new page over the old, outward the old
           over the new.  It scales from SS to 1 (inward) or 1 to SS (outward)
           about its centre, which travels along Di from (to) just offscreen;
           with Di None it only scales.  Without B only changes fly: layer
           pixels equal on both pages are transparent. */
        int outward = fly & FLY_OUTWARD;
        const unsigned char *layer = outward ? old_px : new_px, *under = outward ? new_px : old_px;
        double s = outward ? 1.0 + (scale_start - 1.0) * progress : scale_start + (1.0 - scale_start) * progress;
        double angle = (double)(direction % 360) * M_PI / 180.0;
        double dx = (fly & FLY_UNDIRECTED) ? 0.0 : cos(angle), dy = (fly & FLY_UNDIRECTED) ? 0.0 : -sin(angle);
        /* Far enough along Di that the layer (s times the page) is wholly off screen. */
        double away = (fabs(dx) * ow + fabs(dy) * oh) * (1.0 + s) / 2.0;
        double travelled = outward ? progress : progress - 1.0;
        double cx = (ow - 1) / 2.0 + dx * travelled * away, cy = (oh - 1) / 2.0 + dy * travelled * away;
        memcpy(px, under, pixels * 3);
        for (unsigned y = 0; y < oh; ++y) {
            double v = (oh - 1) / 2.0 + (y - cy) / s;
            if (v < 0.0 || v > oh - 1) continue;
            unsigned v0 = (unsigned)v, v1 = v0 + 1 < oh ? v0 + 1 : v0;
            double fv = v - v0;
            for (unsigned x = 0; x < ow; ++x) {
                double u = (ow - 1) / 2.0 + (x - cx) / s;
                if (u < 0.0 || u > ow - 1) continue;
                unsigned u0 = (unsigned)u, u1 = u0 + 1 < ow ? u0 + 1 : u0;
                double fu = u - u0;
                size_t nearest = ((size_t)(fv < 0.5 ? v0 : v1) * ow + (fu < 0.5 ? u0 : u1)) * 3;
                if (!(fly & FLY_OPAQUE) && !memcmp(layer + nearest, under + nearest, 3)) continue;
                size_t a = ((size_t)v0 * ow + u0) * 3, b = ((size_t)v0 * ow + u1) * 3;
                size_t c = ((size_t)v1 * ow + u0) * 3, d = ((size_t)v1 * ow + u1) * 3, k = ((size_t)y * ow + x) * 3;
                for (int ch = 0; ch < 3; ++ch) {
                    double top = layer[a + ch] + (layer[b + ch] - layer[a + ch]) * fu;
                    double bottom = layer[c + ch] + (layer[d + ch] - layer[c + ch]) * fu;
                    px[k + ch] = (unsigned char)(top + (bottom - top) * fv + 0.5);
                }
            }
        }
    } else if (fade) {
        uint32_t a = (uint32_t)(progress * 256.0 + 0.5);
        for (size_t k = 0; k < pixels * 3; ++k)
            px[k] = (unsigned char)((old_px[k] * (256 - a) + new_px[k] * a) >> 8);
    } else {
        /* Cells of a few device pixels: fine enough to dissolve, coarse enough to see. */
        unsigned cell = ow / 480 > 1 ? ow / 480 : 1;
        /* Glitter: the position along Di (0 at the side it starts from, 1 at the
           other); a wide band of noise sweeps across. */
        double angle = (double)(direction % 360) * M_PI / 180.0;
        double dx = cos(angle), dy = -sin(angle); /* screen y grows downwards */
        double lo = fmin(fmin(0.0, dx), fmin(dy, dx + dy)), hi = fmax(fmax(0.0, dx), fmax(dy, dx + dy));
        const double band = 0.3;
        for (unsigned y = 0; y < oh; ++y) {
            uint32_t row = mix32(y / cell + 0x632be5abu);
            double v = oh > 1 ? (double)y / (oh - 1) : 0.0;
            for (unsigned x = 0; x < ow; ++x) {
                double noise = mix32((x / cell) * 0x9e3779b1u ^ row) / 4294967296.0;
                double threshold = noise;
                if (glitter) {
                    double u = ow > 1 ? (double)x / (ow - 1) : 0.0;
                    double along = hi > lo ? (u * dx + v * dy - lo) / (hi - lo) : 0.0;
                    threshold = along * (1.0 - band) + noise * band;
                }
                size_t k = ((size_t)y * ow + x) * 3;
                const unsigned char *from = threshold < progress ? new_px : old_px;
                px[k] = from[k]; px[k + 1] = from[k + 1]; px[k + 2] = from[k + 2];
            }
        }
    }
    free(old_file); free(new_file);
    const unsigned char *written = out;
    size_t total = (size_t)header_length + pixels * 3;
    uint32_t *words = NULL;
    if (fly & FRAME_ARGB) {
        words = malloc(pixels * 4);
        if (!words) { free(out); return failure("out of memory"); }
        for (size_t k = 0; k < pixels; ++k)
            words[k] = 0xff000000u | (uint32_t)px[3 * k] << 16 | (uint32_t)px[3 * k + 1] << 8 | px[3 * k + 2];
        written = (const unsigned char *)words;
        total = pixels * 4;
    }
    /* Written beside, then renamed: a reader never sees half a frame. */
    const char *target = lean_string_cstr(out_path);
    size_t target_length = strlen(target);
    char *partial = malloc(target_length + 9);
    if (!partial) { free(out); free(words); return failure("out of memory"); }
    memcpy(partial, target, target_length);
    memcpy(partial + target_length, ".partial", 9);
    FILE *f = fopen(partial, "wb");
    int ok = f && fwrite(written, 1, total, f) == total;
    if (f && fclose(f) != 0) ok = 0;
    free(out);
    free(words);
    if (!ok || rename(partial, target) != 0) {
        remove(partial);
        free(partial);
        return failure("could not write the transition frame");
    }
    free(partial);
    return lean_io_result_mk_ok(lean_box(0));
}

/* The box around a page's visible content: its ink against the paper, in
   points from the page's top-left, from a 400-pixel-wide rendering.  Paper is
   the colour of the page's four corners when they agree, white otherwise; a
   pixel is ink when a channel differs from it by more than 40.  A column (row)
   holds content when at least 1/150 of its pixels are ink, so specks of dust in
   a scan's margin do not count.  JSON [x0, y0, x1, y1], or null for a blank
   page.  A reader zooms to it (fit to content width). */
lean_obj_res vellumory_native_content_box(b_lean_obj_arg handle, uint32_t number, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    if (number >= (uint32_t)d->count) return failure("Page outside document");
    fz_context *ctx = acquire(d);
    if (!ctx) return failure("Cannot clone MuPDF context");
    fz_var(ctx);
    fz_display_list *list = NULL;
    fz_pixmap *pix = NULL;
    fz_device *device = NULL;
    int *columns = NULL, *rows = NULL;
    lean_object *result = NULL;
    int built = 0;
    fz_var(list); fz_var(pix); fz_var(device); fz_var(result); fz_var(columns); fz_var(rows);
    fz_try(ctx) {
        fz_rect rect = d->bounds[number];
        double scale = 400.0 / (rect.x1 - rect.x0);
        int w = 400, h = (int)ceil((rect.y1 - rect.y0) * scale);
        if (!isfinite(scale) || h < 1 || h > 8000) fz_throw(ctx, FZ_ERROR_ARGUMENT, "Unusual page size");
        list = display_list(ctx, d, number, NULL, &built);
        fz_irect bounds = {0, 0, w, h};
        pix = fz_new_pixmap_with_bbox(ctx, fz_device_rgb(ctx), bounds, NULL, 0);
        fz_clear_pixmap_with_value(ctx, pix, 255);
        device = fz_new_draw_device(ctx, fz_identity, pix);
        fz_run_display_list(ctx, list, device, fz_concat(fz_translate(-rect.x0, -rect.y0), fz_scale(scale, scale)),
                            fz_rect_from_irect(bounds), NULL);
        fz_close_device(ctx, device);
        const unsigned char *px = fz_pixmap_samples(ctx, pix);
        ptrdiff_t stride = fz_pixmap_stride(ctx, pix);
        /* Paper: the corners, if they agree, two pixels in: the last row or
           column may be partly covered (a 792-point page is 517.6 pixels here). */
        int in = w > 8 && h > 8 ? 2 : 0;
        const unsigned char *corner[4] = {px + in * stride + 3 * in, px + in * stride + 3 * (w - 1 - in),
                                          px + (h - 1 - in) * stride + 3 * in,
                                          px + (h - 1 - in) * stride + 3 * (w - 1 - in)};
        int paper[3] = {255, 255, 255}, agree = 1;
        for (int c = 1; c < 4; ++c)
            for (int ch = 0; ch < 3; ++ch)
                if (abs(corner[c][ch] - corner[0][ch]) > 8) agree = 0;
        if (agree) for (int ch = 0; ch < 3; ++ch) paper[ch] = corner[0][ch];
        columns = fz_calloc(ctx, w, sizeof(int));
        rows = fz_calloc(ctx, h, sizeof(int));
        for (int y = 0; y < h; ++y)
            for (int x = 0; x < w; ++x) {
                const unsigned char *p = px + y * stride + 3 * x;
                if (abs(p[0] - paper[0]) > 40 || abs(p[1] - paper[1]) > 40 || abs(p[2] - paper[2]) > 40) {
                    columns[x]++; rows[y]++;
                }
            }
        int column_min = h / 150 > 1 ? h / 150 : 1, row_min = w / 150 > 1 ? w / 150 : 1;
        int x0 = 0, x1 = w, y0 = 0, y1 = h;
        while (x0 < w && columns[x0] < column_min) ++x0;
        while (x1 > x0 && columns[x1 - 1] < column_min) --x1;
        while (y0 < h && rows[y0] < row_min) ++y0;
        while (y1 > y0 && rows[y1 - 1] < row_min) --y1;
        char json[160];
        if (x0 >= x1 || y0 >= y1) snprintf(json, sizeof json, "null");
        else snprintf(json, sizeof json, "[%.2f,%.2f,%.2f,%.2f]", x0 / scale, y0 / scale, x1 / scale, y1 / scale);
        result = lean_mk_string(json);
    }
    fz_always(ctx) {
        fz_free(ctx, columns); fz_free(ctx, rows);
        fz_drop_device(ctx, device);
        fz_drop_pixmap(ctx, pix);
        fz_drop_display_list(ctx, list);
    }
    fz_catch(ctx) { return release_failure(d, ctx); }
    release(d, ctx);
    return lean_io_result_mk_ok(result);
}

/* A PDF of pages from several documents in a given order, as pdf-tools' virtual
   documents list them: part i takes pages FIRST to LAST (one-based, inclusive;
   LAST 0 means the last page) of FILES[i], and NUMBERS[6i..6i+5] holds FIRST,
   LAST and a region (LEFT TOP RIGHT BOTTOM, relative to the page as displayed;
   LEFT < 0: the whole page) that becomes the page's CropBox.  Pages are grafted
   with their resources (MuPDF's graft maps share what a document's pages share)
   and written, garbage-collected and compressed, to OUT (beside, then renamed).
   Returns the number of pages. */
lean_obj_res vellumory_native_compose(b_lean_obj_arg files, b_lean_obj_arg numbers, b_lean_obj_arg out_path,
                                      lean_obj_arg world) {
    (void)world;
    size_t parts = lean_array_size(files);
    if (lean_sarray_size(numbers) != 6 * parts || parts == 0) return failure("Composition parts mismatch");
    const double *n = lean_float_array_cptr(numbers);
    fz_context *ctx = fz_new_context(NULL, NULL, FZ_STORE_DEFAULT);
    if (!ctx) return failure("Cannot create MuPDF context");
    pdf_document *out = NULL, *src = NULL;
    pdf_graft_map *map = NULL;
    const char *open_file = NULL;
    char *partial = NULL;
    int total = 0;
    char message[512] = "";
    fz_var(out); fz_var(src); fz_var(map); fz_var(open_file); fz_var(partial); fz_var(total);
    fz_try(ctx) {
        fz_register_document_handlers(ctx);
        out = pdf_create_document(ctx);
        for (size_t i = 0; i < parts; ++i) {
            const char *file = lean_string_cstr(lean_array_get_core(files, i));
            if (!open_file || strcmp(open_file, file)) {
                pdf_drop_graft_map(ctx, map); map = NULL;
                pdf_drop_document(ctx, src); src = NULL;
                src = pdf_open_document(ctx, file);
                map = pdf_new_graft_map(ctx, out);
                open_file = file;
            }
            int count = pdf_count_pages(ctx, src);
            int first = (int)n[6 * i], last = (int)n[6 * i + 1];
            if (last == 0) last = count;
            if (first < 1 || last > count || first > last)
                fz_throw(ctx, FZ_ERROR_ARGUMENT, "Pages %d-%d are not within the %d of %s", first, last, count, file);
            for (int p = first; p <= last; ++p) {
                pdf_graft_mapped_page(ctx, map, -1, src, p - 1);
                ++total;
                double u0 = n[6 * i + 2], v0 = n[6 * i + 3], u1 = n[6 * i + 4], v1 = n[6 * i + 5];
                if (u0 < 0) continue;
                if (!(0 <= u0 && u0 < u1 && u1 <= 1 && 0 <= v0 && v0 < v1 && v1 <= 1))
                    fz_throw(ctx, FZ_ERROR_ARGUMENT, "A region is four fractions: left top right bottom");
                pdf_obj *page = pdf_lookup_page_obj(ctx, out, total - 1);
                pdf_obj *crop = pdf_dict_get_inheritable(ctx, page, PDF_NAME(CropBox));
                fz_rect box = pdf_to_rect(ctx, pdf_is_array(ctx, crop) ? crop
                                               : pdf_dict_get_inheritable(ctx, page, PDF_NAME(MediaBox)));
                int rotate = ((pdf_to_int(ctx, pdf_dict_get_inheritable(ctx, page, PDF_NAME(Rotate))) % 360) + 360) % 360;
                double w = box.x1 - box.x0, h = box.y1 - box.y0, x[2], y[2];
                double us[2] = {u0, u1}, vs[2] = {v0, v1};
                /* The displayed top-left corner, and where right (u) and down (v) go in PDF space. */
                for (int c = 0; c < 2; ++c) {
                    double u = us[c], v = vs[c];
                    switch (rotate) {
                    case 90:  x[c] = box.x0 + v * w; y[c] = box.y0 + u * h; break;
                    case 180: x[c] = box.x1 - u * w; y[c] = box.y0 + v * h; break;
                    case 270: x[c] = box.x1 - v * w; y[c] = box.y1 - u * h; break;
                    default:  x[c] = box.x0 + u * w; y[c] = box.y1 - v * h; break;
                    }
                }
                fz_rect region = {fmin(x[0], x[1]), fmin(y[0], y[1]), fmax(x[0], x[1]), fmax(y[0], y[1])};
                pdf_dict_put_rect(ctx, page, PDF_NAME(CropBox), region);
            }
        }
        const char *target = lean_string_cstr(out_path);
        size_t length = strlen(target);
        partial = fz_malloc(ctx, length + 9);
        memcpy(partial, target, length);
        memcpy(partial + length, ".partial", 9);
        pdf_write_options options = pdf_default_write_options;
        options.do_garbage = 3;
        options.do_compress = 1;
        pdf_save_document(ctx, out, partial, &options);
        if (rename(partial, target) != 0) fz_throw(ctx, FZ_ERROR_SYSTEM, "Cannot write %s", target);
    }
    fz_always(ctx) {
        pdf_drop_graft_map(ctx, map);
        pdf_drop_document(ctx, src);
        pdf_drop_document(ctx, out);
    }
    fz_catch(ctx) {
        snprintf(message, sizeof message, "%s", fz_convert_error(ctx, NULL));
        if (partial) remove(partial);
    }
    fz_free(ctx, partial);
    fz_drop_context(ctx);
    if (*message) return failure(message);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)total));
}

/* Whether the document reflows (EPUB, FB2, XHTML): its pages are a layout. */
lean_obj_res vellumory_native_reflowable(b_lean_obj_arg handle, lean_obj_arg world) {
    (void)world;
    v_document *d = lean_get_external_data(handle);
    pthread_mutex_lock(&d->doc_lock);
    int reflowable = fz_is_document_reflowable(d->ctx, d->doc);
    pthread_mutex_unlock(&d->doc_lock);
    return lean_io_result_mk_ok(lean_box(reflowable ? 1 : 0));
}
