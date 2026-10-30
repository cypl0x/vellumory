import Vellumory.Json

namespace Vellumory.Native

/-- Clockwise text polygon in top-left page points, including rotated text. -/
structure Quad where
  x0 : Float
  y0 : Float
  x1 : Float
  y1 : Float
  x2 : Float
  y2 : Float
  x3 : Float
  y3 : Float
  deriving Inhabited

instance : ToJson Quad where
  toJson q := .mkObj [("x0", toJson q.x0), ("y0", toJson q.y0), ("x1", toJson q.x1), ("y1", toJson q.y1),
    ("x2", toJson q.x2), ("y2", toJson q.y2), ("x3", toJson q.x3), ("y3", toJson q.y3)]

@[export vellumory_mk_quad]
def mkQuad (x0 y0 x1 y1 x2 y2 x3 y3 : Float) : Quad :=
  { x0, y0, x1, y1, x2, y2, x3, y3 }

structure SearchPage where
  hits : Array (Array Quad)
  truncated : Bool
  deriving Inhabited

instance : ToJson SearchPage where
  toJson s := .mkObj [("hits", toJson s.hits), ("truncated", toJson s.truncated)]

@[export vellumory_mk_search_page]
def mkSearchPage (hits : Array (Array Quad)) (truncated : Bool) : SearchPage :=
  { hits, truncated }

structure Selection where
  text : String
  quads : Array Quad
  deriving Inhabited

instance : ToJson Selection where
  toJson s := .mkObj [("text", toJson s.text), ("quads", toJson s.quads)]

@[export vellumory_mk_selection]
def mkSelection (text : String) (quads : Array Quad) : Selection := { text, quads }

/-- Flattened preorder outline. Coordinates are MuPDF's top-left page points.
    A missing page retains headings and external/unresolved URI entries. -/
structure OutlineEntry where
  title : String
  uri : String
  depth : Nat
  page : Option Nat
  x : Float
  y : Float
  expanded : Bool
  deriving Inhabited

instance : ToJson OutlineEntry where
  toJson e := .mkObj [("title", toJson e.title), ("uri", toJson e.uri), ("depth", toJson e.depth),
    ("page", toJson e.page), ("x", toJson e.x), ("y", toJson e.y), ("expanded", toJson e.expanded)]

/-- Export a constructor instead of duplicating Lean's structure layout in C. -/
@[export vellumory_mk_outline_entry]
def mkOutlineEntry (title uri : String) (depth pagePlusOne : UInt32)
    (x y : Float) (expanded : Bool) : OutlineEntry :=
  { title, uri, depth := depth.toNat,
    page := if pagePlusOne == 0 then none else some (pagePlusOne.toNat - 1), x, y, expanded }

/-- A link area in top-left page points. Internal targets carry a page and point. -/
structure Link where
  x0 : Float
  y0 : Float
  x1 : Float
  y1 : Float
  uri : String
  external : Bool
  page : Option Nat
  x : Float
  y : Float
  deriving Inhabited

instance : ToJson Link where
  toJson l := .mkObj [("x0", toJson l.x0), ("y0", toJson l.y0), ("x1", toJson l.x1), ("y1", toJson l.y1),
    ("uri", toJson l.uri), ("external", toJson l.external), ("page", toJson l.page),
    ("x", toJson l.x), ("y", toJson l.y)]

@[export vellumory_mk_link]
def mkLink (x0 y0 x1 y1 : Float) (uri : String) (external : Bool) (pagePlusOne : UInt32)
    (x y : Float) : Link :=
  { x0, y0, x1, y1, uri, external, x, y,
    page := if pagePlusOne == 0 then none else some (pagePlusOne.toNat - 1) }

/-- A word and the polygon spanning its glyphs. -/
structure Word where
  text : String
  quad : Quad
  deriving Inhabited

instance : ToJson Word where
  toJson w := .mkObj [("text", .str w.text), ("quad", toJson w.quad)]

@[export vellumory_mk_word]
def mkWord (text : String) (quad : Quad) : Word := { text, quad }

/-- A SyncTeX result box on a one-based page, in points from the top-left. -/
structure SyncBox where
  page : Nat
  x : Float
  y : Float
  width : Float
  height : Float
  deriving Inhabited

instance : ToJson SyncBox where
  toJson b := .mkObj [("page", toJson b.page), ("x", toJson b.x), ("y", toJson b.y),
    ("width", toJson b.width), ("height", toJson b.height)]

@[export vellumory_mk_sync_box]
def mkSyncBox (page : UInt32) (x y width height : Float) : SyncBox :=
  { page := page.toNat, x, y, width, height }

structure SyncSource where
  file : String
  line : Nat
  column : Nat
  deriving Inhabited

instance : ToJson SyncSource where
  toJson s := .mkObj [("file", .str s.file), ("line", toJson s.line), ("column", toJson s.column)]

@[export vellumory_mk_sync_source]
def mkSyncSource (file : String) (line column : UInt32) : SyncSource :=
  { file, line := line.toNat, column := column.toNat }

private opaque DocumentType : NonemptyType
/-- Owned MuPDF document with a locked base context. Safe to share between
    threads: every native call borrows a cloned context and serializes
    document access internally; display lists are replayed concurrently. -/
def Document := DocumentType.type

private opaque CookieType : NonemptyType
/-- MuPDF cancellation cookie for one request. Aborting is irreversible. -/
def Cookie := CookieType.type

@[extern "vellumory_native_cookie_new"]
opaque Cookie.new : IO Cookie

/-- Ask a running or future render using this cookie to stop soon. -/
@[extern "vellumory_native_cookie_abort"]
opaque Cookie.abort (cookie : @& Cookie) : IO Unit

/-- `magic` is a file name or MIME type selecting the MuPDF document handler. -/
@[extern "vellumory_native_open"]
opaque openDocumentAs (bytes : @& ByteArray) (magic : @& String) (password : @& String) : IO Document

/-- Like `openDocumentAs`; a reflowable document (EPUB, FB2, XHTML) is laid out on
    pages of WIDTH × HEIGHT points with EM-point text and the user stylesheet CSS
    (empty: none). -/
@[extern "vellumory_native_open_reflowed"]
opaque openDocumentReflowed (bytes : @& ByteArray) (magic password : @& String) (width height em : Float)
  (css : @& String) : IO Document

/-- Like `openDocumentReflowed`, with the pages not measured yet: `finishBounds`
    must follow before any other use of the document. -/
@[extern "vellumory_native_open_deferred"]
opaque openDocumentDeferred (bytes : @& ByteArray) (magic password : @& String) (width height em : Float)
  (css : @& String) : IO Document

/-- Measure the pages of a document opened deferred, or read their rectangles from
    the cache file `path` ("": none) when it holds a usable one for every page.
    True when they were measured, and then written there. -/
@[extern "vellumory_native_finish_bounds"]
opaque finishBounds (doc : @& Document) (path : @& String) : IO Bool

/-- Whether the document reflows: its pages are a layout. -/
@[extern "vellumory_native_reflowable"]
opaque reflowable (doc : @& Document) : IO Bool

def openDocument (bytes : ByteArray) (password : String) : IO Document :=
  openDocumentAs bytes "application/pdf" password

/-- Page sizes in points from page-tree attributes (no content parsing). -/
@[extern "vellumory_native_sizes"]
opaque pageSizes (doc : @& Document) : IO (Array (Float × Float))

/-- Reference sizes from fully loaded pages, for cross-checking `pageSizes`. -/
@[extern "vellumory_native_loaded_sizes"]
opaque loadedPageSizes (doc : @& Document) : IO (Array (Float × Float))

/-- Raster bytes plus where the time went. `listMs` includes waiting for the
    document lock; `rasterMs` is pure display-list replay. -/
structure RenderResult where
  /-- Empty when the raster went to a file. -/
  bytes : ByteArray
  /-- The raster's length in bytes, wherever it went. -/
  size : Nat
  built : Bool
  listMs : Float
  rasterMs : Float
  deriving Inhabited

@[export vellumory_mk_render_result]
def mkRenderResult (bytes : ByteArray) (size : UInt64) (built : Bool) (listMs rasterMs : Float) : RenderResult :=
  { bytes, size := size.toNat, built, listMs, rasterMs }

@[extern "vellumory_native_render"]
private opaque renderRaw (doc : @& Document) (cookie : @& Cookie)
    (page width left top right bottom : UInt32)
    (quads : @& FloatArray) (kinds : @& ByteArray) (palette : @& ByteArray) (multiply : Bool)
    (theme fg bg : UInt32) (path : @& String) : IO RenderResult

/-- Page recolouring applied before overlays. -/
inductive Theme where
  | none
  | tint (foreground background : UInt32)
  | invertLuminance
  deriving BEq, Repr, Inhabited

/-- Translucent polygons (page points, top-left origin) composited after theming. -/
structure Overlay where
  quads : FloatArray := .empty
  kinds : ByteArray := .empty
  palette : ByteArray := .empty
  multiply : Bool := true
  deriving Inhabited

/-- Binary PPM (P6) of pixels `[left,right) × [top,bottom)` of the page scaled
    to `width` pixels wide, with timing. With `file`, the raster is written
    there (whole or not at all) instead of returned: the pixels then never
    pass through a Lean array. -/
def renderTimed (doc : Document) (cookie : Cookie) (page width left top right bottom : UInt32)
    (theme : Theme := .none) (overlay : Overlay := {}) (file : Option System.FilePath := none) :
    IO RenderResult :=
  let (mode, fg, bg) := match theme with
    | .none => ((0 : UInt32), (0 : UInt32), (0 : UInt32))
    | .tint f b => (1, f, b)
    | .invertLuminance => (2, 0, 0)
  renderRaw doc cookie page width left top right bottom overlay.quads overlay.kinds overlay.palette
    overlay.multiply mode fg bg ((file.map (·.toString)).getD "")


def renderPpm (doc : Document) (cookie : Cookie) (page width left top right bottom : UInt32)
    (theme : Theme := .none) (overlay : Overlay := {}) : IO ByteArray :=
  (·.bytes) <$> renderTimed doc cookie page width left top right bottom theme overlay

/-- Whole-width band, as used by the original strip tests. -/
def render (doc : Document) (page width top bottom : UInt32) : IO ByteArray := do
  renderPpm doc (← Cookie.new) page width 0 top width bottom

@[extern "vellumory_native_text"]
opaque pageText (doc : @& Document) (page : UInt32) : IO String

@[extern "vellumory_native_search"]
opaque searchPage (doc : @& Document) (page : UInt32) (query : @& String)
    (caseSensitive : Bool) (limit : UInt32) : IO SearchPage

/-- Missing endpoints mean the first/last character in the page's text order.
    Modes are characters (0), words (1), and lines (2). -/
@[extern "vellumory_native_select"]
opaque selectText (doc : @& Document) (page : UInt32) (x0 y0 x1 y1 : Float)
    (hasStart hasEnd : Bool) (mode : UInt32) : IO Selection

@[extern "vellumory_native_outline"]
opaque outline (doc : @& Document) : IO (Array OutlineEntry)

@[extern "vellumory_native_links"]
opaque pageLinks (doc : @& Document) (page : UInt32) : IO (Array Link)

@[extern "vellumory_native_labels"]
opaque pageLabels (doc : @& Document) : IO (Array String)

@[extern "vellumory_native_sha256"]
opaque sha256 (bytes : @& ByteArray) : IO String

@[extern "vellumory_native_file_mode"]
opaque fileMode (path : @& String) : IO UInt32

@[extern "vellumory_native_set_file_mode"]
opaque setFileMode (path : @& String) (mode : UInt32) : IO Unit

/-- Document report as JSON text (see native/mupdf.c `vellumory_native_info`). -/
@[extern "vellumory_native_info"]
opaque info (doc : @& Document) : IO String

@[extern "vellumory_native_page_info"]
opaque pageInfo (doc : @& Document) (page : UInt32) : IO String

/-- A page as reflowable HTML: its structure, or its layout when untagged, with
    figures rendered as images when FLAGS has 1, inverted when it has 2
    (native/mupdf.c `vellumory_native_page_reflow`). -/
@[extern "vellumory_native_page_reflow"]
opaque pageReflow (doc : @& Document) (page : UInt32) (flags : UInt8) : IO String

/-- Pages with links, and titles of contents entries, whose destination is missing (JSON). -/
@[extern "vellumory_native_broken_destinations"]
opaque brokenDestinations (doc : @& Document) : IO String

/-- The page's characters line by line with their logical start and end points (JSON). -/
@[extern "vellumory_native_page_chars"]
opaque pageChars (doc : @& Document) (page : UInt32) : IO String

@[extern "vellumory_native_attachment"]
opaque attachment (doc : @& Document) (index : UInt32) : IO ByteArray

@[extern "vellumory_native_words"]
opaque pageWords (doc : @& Document) (page : UInt32) : IO (Array Word)

/-- Every terminal form field as JSON: name (fully qualified), type (text,
    checkbox, radio, combobox, listbox, button, signature), value, label,
    read_only, required, page, options (choices), states (on states). -/
@[extern "vellumory_native_form_fields"]
opaque formFields (doc : @& Document) : IO String

/-- Set form fields NAMES to VALUES in INPUT, writing OUTPUT; triggers are
    ignored, so no document JavaScript runs. -/
@[extern "vellumory_native_fill_form"]
opaque fillForm (input output : @& String) (names values : @& Array String) : IO Unit

/-- The catalog's /PageLayout and /PageMode names, separated by a space. -/
@[extern "vellumory_native_display_preferences"]
opaque displayPreferences (doc : @& Document) : IO String

/-- Signed signature fields (a quick check, no verification). -/
@[extern "vellumory_native_signed_count"]
opaque signedCount (doc : @& Document) : IO UInt32

/-- Every signature field, verified: field, page, signed, signer, time, reason,
    subfilter, intact (signed bytes match), trusted (chain to the system store),
    covers_whole_file, changed_after_signing, error, trust_error. -/
@[extern "vellumory_native_signatures"]
opaque signatures (doc : @& Document) : IO String

/-- The annotations of PAGE as JSON (object, type, rect, contents, author,
    modified, color, quads), in points relative to the crop box. -/
@[extern "vellumory_native_annotations"]
opaque annotations (doc : @& Document) (page : UInt32) : IO String

/-- Add an annotation of KIND (a PDF subtype such as Highlight or Text) to
    zero-based PAGE of INPUT, writing OUTPUT; QUADS are 8 numbers per quad, RECT
    4 or none, COLOR 3 or none; TIME, seconds since 1970, dates it. Returns its
    object number. -/
@[extern "vellumory_native_annot_add"]
opaque annotAdd (input output : @& String) (page : UInt32) (kind : @& String) (quads rect color : @& FloatArray)
  (contents author : @& String) (time : Float) : IO UInt32

/-- Set the contents of annotation OBJECT on zero-based PAGE, or REMOVE it. -/
@[extern "vellumory_native_annot_change"]
opaque annotChange (input output : @& String) (page object : UInt32) (remove : Bool) (contents : @& String)
  (time : Float) : IO Unit

/-- A simple XMP property by namespace URI and name (prefixes do not matter). -/
@[extern "vellumory_native_xmp_property"]
opaque xmpProperty (packet : @& ByteArray) (schema name : @& String) : IO (Option String)

/-- Rewrite an XMP packet (empty: start one) through Adobe's XMP Toolkit.
    FIELDS are title, subject, author, language, keywords, producer, creator,
    created, modified and metadata-date; an empty value deletes the field.
    Everything else in the packet is kept. -/
@[extern "vellumory_native_xmp_update"]
opaque xmpUpdate (packet : @& ByteArray) (fields values : @& Array String) : IO ByteArray

/-- Where SOURCE:LINE:COLUMN appears in the PDF (reads PDF.synctex.gz). -/
@[extern "vellumory_native_synctex_forward"]
opaque synctexForward (pdf : @& String) (source : @& String) (line column : UInt32) : IO (Array SyncBox)

/-- Which source produced the point at X, Y on one-based PAGE. -/
@[extern "vellumory_native_synctex_backward"]
opaque synctexBackward (pdf : @& String) (page : UInt32) (x y : Float) : IO (Array SyncSource)

@[extern "vellumory_native_cores"]
opaque cores : IO UInt32

/-- Whether a process with this PID exists, whoever owns it. -/
@[extern "vellumory_native_process_alive"]
opaque processAlive (pid : UInt32) : IO Bool

/-- Give memory the C library keeps after freeing back to the system (glibc);
    true when some was. -/
@[extern "vellumory_native_trim_memory"]
opaque trimMemory : IO Bool

/-- One frame of a page transition composed from two pages (ISO 32000-1 Table 162):
    STYLE Dissolve, Glitter, Fade or Fly at PROGRESS (0 to 1) from the raster OLD
    to NEW (equally sized binary PPM files), written to OUT. Glitter and Fly move
    along DIRECTION, in degrees counterclockwise from left to right. Fly scales
    from or to SCALESTART (/SS); FLY holds 1 for outward (/M /O), 2 for no
    direction (/Di /None), 4 for an opaque flown area (/B true), and 8 to write
    the frame as an Emacs canvas reads it (native-endian ARGB32, no header). -/
@[extern "vellumory_native_transition_frame"]
opaque transitionFrame (old new out style : @& String) (progress : Float) (direction : UInt32)
  (scaleStart : Float := 1.0) (fly : UInt8 := 0) : IO Unit

/-- The box around PAGE's visible content (ink against the paper), in points from
    its top-left, as JSON [x0, y0, x1, y1], or null for a blank page. -/
@[extern "vellumory_native_content_box"]
opaque contentBox (doc : @& Document) (page : UInt32) : IO String

/-- Write to OUT a PDF of pages from several documents, in order (pdf-tools' virtual
    documents): part i takes pages NUMBERS[6i] to NUMBERS[6i+1] (one-based; 0:
    to the last) of FILES[i], cropped to the region NUMBERS[6i+2..6i+5] (left,
    top, right, bottom, fractions of the page as displayed; left < 0: none).
    Returns the number of pages. -/
@[extern "vellumory_native_compose"]
opaque compose (files : @& Array String) (numbers : @& FloatArray) (out : @& String) : IO UInt32

/-- Bytes free for unprivileged writers on the filesystem holding the path (0: unknown). -/
@[extern "vellumory_native_free_bytes"]
opaque freeBytes (path : @& String) : IO UInt64

@[extern "vellumory_native_version"]
opaque version : IO String

end Vellumory.Native
