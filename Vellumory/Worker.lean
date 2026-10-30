import Vellumory.Native
import Vellumory.Geometry
import Vellumory.Navigation
import Vellumory.Scheduler
import Std.Sync.Mutex
import Std.Data.HashMap
import Vellumory.Tools

open System Std
namespace Vellumory.Worker

structure Opened where
  doc : Native.Document
  hash : String
  sizes : Array (Float × Float)

structure Point where
  x : Float
  y : Float

instance : FromJson Point where
  fromJson? json := return { x := ← json.getObjValAs? Float "x", y := ← json.getObjValAs? Float "y" }

def field (json : Json) (name : String) : IO Json :=
  IO.ofExcept (json.getObjVal? name)
def stringField (json : Json) (name : String) : IO String :=
  IO.ofExcept (json.getObjValAs? String name)
def natField (json : Json) (name : String) : IO Nat :=
  IO.ofExcept (json.getObjValAs? Nat name)
def optionalNat (json : Json) (name : String) (default : Nat) : IO Nat :=
  match json.getObjVal? name with
  | .error _ => pure default
  | .ok .null => pure default
  | .ok value => IO.ofExcept (fromJson? value : Except String Nat)

def privateDirectory (path : FilePath) : IO Unit := do
  IO.FS.createDirAll path
  IO.setAccessRights path { user := { read := true, write := true, execution := true } }

def xdgDirectory (name fallback : String) : IO FilePath := do
  match ← IO.getEnv name with
  | some p => if p.isEmpty then home else pure p
  | none => home
where home := do return FilePath.mk ((← IO.getEnv "HOME").getD ".") / fallback

/-- Trim the persistent rasters to BUDGET bytes, oldest first. -/
def trimPersistent (dir : FilePath) (budget : Nat) : IO Unit := do
  let mut files : Array (FilePath × Nat × Int) := #[]
  for sub in ← (try dir.readDir catch _ => pure #[]) do
    for file in ← (try sub.path.readDir catch _ => pure #[]) do
      try
        let m ← file.path.metadata
        files := files.push (file.path, m.byteSize.toNat, m.modified.sec)
      catch _ => pure ()
  let total := files.foldl (fun sum f => sum + f.2.1) 0
  if total > budget then
    let mut excess := total - budget
    for (path, size, _) in files.qsort (fun a b => a.2.2 < b.2.2) do
      if excess == 0 then break
      try
        IO.FS.removeFile path
        excess := excess - min excess size
      catch _ => pure ()

/-- Whether a running process with this PID is a Vellumory worker (a PID can be
    reused by an unrelated program). Without /proc (macOS) only whether it
    runs: a directory then waits for its PID to be free, but a running
    worker's is never taken. -/
def liveWorker (pid : String) : IO Bool := do
  let some number := pid.toNat? | return false
  unless ← (FilePath.mk "/proc/self").pathExists do
    return number < 2147483648 && (← Native.processAlive number.toUInt32)
  try
    let command ← IO.FS.readFile s!"/proc/{pid}/cmdline"
    let parts := command.splitOn "\x00"
    return parts.any (·.endsWith "vellumory") && parts.contains "serve"
  catch _ => return false

/-- Remove the raster directories of workers that are gone: a worker killed
    outright (SIGKILL, Emacs exiting) cannot remove its own, and on a tmpfs
    they would fill memory. -/
def sweepStale (parent : FilePath) : IO Unit := do
  let entries ← try parent.readDir catch _ => return
  for entry in entries do
    match entry.fileName.splitOn "-" with
    | ["worker", pid, _] =>
      unless ← liveWorker pid do
        try IO.FS.removeDirAll entry.path catch _ => pure ()
    | _ => pure ()

/-- Per-process raster directory on tmpfs when available. Strips here are
    disposable: they exist so Emacs can decode raw PPM without any encoding
    step, and the directory is removed when the worker exits. -/
def runtimeDirectory : IO FilePath := do
  let root ← match ← IO.getEnv "XDG_RUNTIME_DIR" with
    | some p => if p.isEmpty then xdgDirectory "XDG_CACHE_HOME" ".cache" else pure (FilePath.mk p)
    | none => xdgDirectory "XDG_CACHE_HOME" ".cache"
  let dir := root / "vellumory" / s!"worker-{← IO.Process.getPID}-{← IO.rand 0 0x7fffffff}"
  privateDirectory (root / "vellumory")
  sweepStale (root / "vellumory")
  privateDirectory dir
  return dir

def atomicWrite (path : FilePath) (bytes : ByteArray) : IO Unit := do
  let suffix ← IO.rand 0 0x7fffffff
  let temporary := FilePath.mk (path.toString ++ s!".pending-{suffix}")
  try
    IO.FS.writeBinFile temporary bytes
    IO.FS.rename temporary path
  finally
    if ← temporary.pathExists then IO.FS.removeFile temporary

/-! ## Bounded raster store -/

structure Store where
  /-- Least recently used first. -/
  order : List (String × Nat) := []
  total : Nat := 0
  budget : Nat

/-- Record use of `key`, returning keys to delete to respect the budget.
    The key just used is never evicted, even if it alone exceeds the budget. -/
def Store.touch (s : Store) (key : String) (size : Nat) : Store × List String :=
  let rest := s.order.filter (·.1 != key)
  let total := rest.foldl (· + ·.2) 0 + size
  let rec evict (entries : List (String × Nat)) (total : Nat) (gone : List String) :=
    match entries with
    | (k, n) :: tail => if total > s.budget then evict tail (total - n) (k :: gone)
                        else (entries, total, gone)
    | [] => ([], total, gone)
  let (kept, total, gone) := evict rest total []
  ({ s with order := kept ++ [(key, size)], total }, gone)

/-! ## Lanes: priority queues served by dedicated threads -/

structure Work where
  idJson : Json
  cookie : Native.Cookie
  action : IO Json

structure LaneState where
  queue : List (Scheduler.Job Work) := []
  running : List (Nat × Native.Cookie) := []
  seq : Nat := 0
  stopping : Bool := false

structure Lane where
  state : Std.Mutex LaneState
  wake : Std.Condvar

def Lane.new : IO Lane := return { state := ← Std.Mutex.new {}, wake := ← Std.Condvar.new }

structure Shared where
  output : Std.Mutex IO.FS.Stream
  render : Lane
  document : Lane
  store : Std.Mutex Store
  runtime : FilePath
  /-- Small rasters (previews, page thumbnails) kept across sessions. -/
  persistent : FilePath
  engine : String
  threads : Nat
  /-- Serial of the next transition frame. -/
  frames : Std.Mutex Nat

def Shared.send (shared : Shared) (json : Json) : IO Unit :=
  shared.output.atomically do
    let out ← get
    out.putStrLn json.compress
    out.flush

def errorReply (id : Json) (message : String) : Json :=
  if message == "cancelled" then
    Json.mkObj [("id", id), ("error", toJson message), ("cancelled", toJson true)]
  else Json.mkObj [("id", id), ("error", toJson message)]

def Lane.push (lane : Lane) (id : Nat) (priority : Nat) (work : Work) : IO Unit := do
  lane.state.atomically do
    let s ← get
    set { s with queue := { id, priority, seq := s.seq, payload := work } :: s.queue, seq := s.seq + 1 }
  lane.wake.notifyOne

/-- Remove queued jobs with these ids and abort running ones.
    Returns the removed jobs so the caller can acknowledge them. -/
def Lane.cancel (lane : Lane) (ids : List Nat) : IO (List (Scheduler.Job Work)) := do
  let (removed, running) ← lane.state.atomically do
    let s ← get
    let (removed, kept) := Scheduler.cancel ids s.queue
    set { s with queue := kept }
    return (removed, s.running)
  for (id, cookie) in running do
    if ids.contains id then cookie.abort
  return removed

partial def Lane.serve (lane : Lane) (shared : Shared) : IO Unit := do
  let next ← lane.state.atomicallyOnce lane.wake
    (do let s ← get; return s.stopping || !s.queue.isEmpty)
    (do
      let s ← get
      if s.stopping then return none
      match Scheduler.pop s.queue with
      | none => return none
      | some (job, rest) =>
        set { s with queue := rest, running := (job.id, job.payload.cookie) :: s.running }
        return some job)
  match next with
  | none => return ()
  | some job =>
    let reply ← try
        pure <| Json.mkObj [("id", job.payload.idJson), ("result", ← job.payload.action)]
      catch e => pure (errorReply job.payload.idJson e.toString)
    lane.state.atomically (modify fun s => { s with running := s.running.filter (·.1 != job.id) })
    shared.send reply
    lane.serve shared

def Lane.stop (lane : Lane) : IO Unit := do
  lane.state.atomically (modify fun s => { s with stopping := true })
  lane.wake.notifyAll

/-! ## Requests -/

def openFile (params : Json) : IO (Opened × Json) := do
  let path ← stringField params "path"
  let password := (params.getObjValAs? String "password").toOption.getD ""
  let info ← (FilePath.mk path).metadata
  if info.byteSize > 2147483648 then throw <| IO.userError "Document exceeds the 2 GiB input limit"
  let started ← IO.monoNanosNow
  let bytes ← IO.FS.readBinFile path
  let read ← IO.monoNanosNow
  -- Hash on another thread while MuPDF parses the same immutable bytes.
  let hashing ← IO.asTask (Native.sha256 bytes) .dedicated
  let magic := (params.getObjValAs? String "magic").toOption.getD path
  -- Reflowable documents (EPUB, FB2) laid out as asked: page size, text size, stylesheet.
  let reflow := (params.getObjVal? "reflow").toOption
  let number (key : String) (default : Float) :=
    ((reflow.getD .null).getObjValAs? Float key).toOption.getD default
  let css := ((reflow.getD .null).getObjValAs? String "css").toOption.getD ""
  let doc ← if reflow.isSome then
      Native.openDocumentReflowed bytes magic password (number "width" 450) (number "height" 600) (number "em" 11) css
    else Native.openDocumentAs bytes magic password
  let sizes ← Native.pageSizes doc
  let parsed ← IO.monoNanosNow
  let hash ← IO.ofExcept (← IO.wait hashing)
  -- A reflowed document's pages are its layout: rasters and positions belong to
  -- the layout as much as to the bytes.
  let hash ← match reflow with
    | some r => if ← Native.reflowable doc then Native.sha256 (hash ++ r.compress).toUTF8 else pure hash
    | none => pure hash
  let finished ← IO.monoNanosNow
  let ms (a b : Nat) : Float := (b - a).toFloat / 1e6
  let pages := sizes.map fun (w, h) => Json.mkObj [("width", toJson w), ("height", toJson h)]
  return ({ doc, hash, sizes }, Json.mkObj [("hash", toJson hash), ("pages", toJson pages),
    -- Signed signature fields: every edit rewrites the file and invalidates them.
    ("signed", toJson (← Native.signedCount doc).toNat),
    -- Pages that are a layout (EPUB, FB2): text size and page size change them.
    ("reflowable", toJson (← Native.reflowable doc)),
    -- How the document asks to be shown: two-page layouts, an outline sidebar.
    ("page_layout", toJson ((← Native.displayPreferences doc).splitOn " ").head!),
    ("page_mode", toJson (((← Native.displayPreferences doc).splitOn " ")[1]?.getD "")),
    ("bytes", toJson bytes.size), ("ms", toJson (ms started finished)),
    ("read_ms", toJson (ms started read)), ("parse_ms", toJson (ms read parsed)),
    ("hash_wait_ms", toJson (ms parsed finished))])

def parseColor (json : Json) : Except String (UInt8 × UInt8 × UInt8 × UInt8) := do
  let parts : Array Nat ← fromJson? json
  match parts with
  | #[r, g, b, a] =>
    if r < 256 ∧ g < 256 ∧ b < 256 ∧ a < 256 then pure (r.toUInt8, g.toUInt8, b.toUInt8, a.toUInt8)
    else throw "Colour channels must be 0–255"
  | _ => throw "Colours are [r, g, b, a]"

/-- `{"quads": [[x0..y3], ...], "kinds": [k...], "palette": [[r,g,b,a]...], "blend": "multiply"}` -/
def parseOverlay (json : Json) : Except String Native.Overlay := do
  let quads : Array (Array Float) ← json.getObjValAs? _ "quads"
  let kinds : Array Nat ← json.getObjValAs? _ "kinds"
  let palette : Array Json ← json.getObjValAs? _ "palette"
  if quads.size != kinds.size || quads.size > 65536 || palette.size > 16 then
    throw "Overlay quads, kinds and palette are inconsistent or too large"
  let mut floats : FloatArray := .empty
  for q in quads do
    unless q.size == 8 && q.all Float.isFinite do throw "Each overlay quad has eight finite numbers"
    for v in q do floats := floats.push v
  let mut colors : ByteArray := .empty
  for c in palette do
    let (r, g, b, a) ← parseColor c
    colors := colors.push r |>.push g |>.push b |>.push a
  unless kinds.all (· < palette.size) do throw "Overlay kind outside palette"
  let blend := (json.getObjValAs? String "blend").toOption.getD "multiply"
  return { quads := floats, kinds := ⟨kinds.map (·.toUInt8)⟩, palette := colors,
           multiply := blend == "multiply" }

def parseTheme (json : Json) : Except String Native.Theme := do
  match json with
  | .null => pure .none
  | .str "invert" => pure .invertLuminance
  | _ =>
    let fg : Nat ← json.getObjValAs? _ "fg"
    let bg : Nat ← json.getObjValAs? _ "bg"
    if fg < 0x1000000 ∧ bg < 0x1000000 then pure (.tint fg.toUInt32 bg.toUInt32)
    else throw "Theme colours are 0xRRGGBB integers"

def render (shared : Shared) (opened : Opened) (cookie : Native.Cookie) (params : Json) : IO Json := do
  let page ← natField params "page"
  let width ← natField params "width"
  if width == 0 || width > 32768 then throw <| IO.userError "Width must be 1–32768 pixels"
  let some (pageWidth, pageHeight) := opened.sizes[page]?
    | throw <| IO.userError "Page is outside document"
  let total := (pageHeight * width.toFloat / pageWidth).ceil.toUInt64.toNat
  let band := clipBand total ⟨0, total⟩ ⟨← natField params "top", ← natField params "bottom"⟩
  let columns := clipBand width ⟨0, width⟩ ⟨← optionalNat params "left" 0, ← optionalNat params "right" width⟩
  if band.start ≥ band.stop || columns.start ≥ columns.stop ||
      (columns.stop - columns.start) * (band.stop - band.start) > 33554432 then
    throw <| IO.userError "Empty or oversized render region"
  let themeJson := (params.getObjVal? "theme").toOption.getD .null
  let theme ← IO.ofExcept (parseTheme themeJson)
  let overlayJson := (params.getObjVal? "overlay").toOption.getD .null
  let overlay ← match overlayJson with
    | .null => pure {}
    | json => IO.ofExcept (parseOverlay json)
  let spec := Json.arr #[toJson "raster-v2", toJson shared.engine, toJson opened.hash,
    toJson page, toJson width, toJson columns.start, toJson band.start, toJson columns.stop,
    toJson band.stop, themeJson, overlayJson]
  let key ← Native.sha256 spec.compress.toUTF8
  -- Previews and thumbnails are small and asked for on every visit: they are
  -- kept across sessions under their content address. Sharp strips are not.
  let persist := (params.getObjValAs? Bool "persist").toOption.getD false
  let path := if persist then shared.persistent / (key.take 2).toString / (key ++ ".ppm")
    else shared.runtime / (key ++ ".ppm")
  if persist then IO.FS.createDirAll (shared.persistent / (key.take 2).toString)
  let started ← IO.monoNanosNow
  let hit ← path.pathExists
  let (size, timing) ← if hit then pure ((← path.metadata).byteSize.toNat, none) else do
    let result ← Native.renderTimed opened.doc cookie page.toUInt32 width.toUInt32
      columns.start.toUInt32 band.start.toUInt32 columns.stop.toUInt32 band.stop.toUInt32 theme overlay
      (file := some path)
    pure (result.size, some result)
  let gone ← if persist then pure [] else shared.store.atomically do
    let (next, gone) := (← get).touch key size
    set next
    return gone
  for old in gone do
    try IO.FS.removeFile (shared.runtime / (old ++ ".ppm")) catch _ => pure ()
  let elapsed := ((← IO.monoNanosNow) - started).toFloat / 1e6
  let timed : List (String × Json) := match timing with
    | some t => [("raster_ms", toJson t.rasterMs), ("list_ms", toJson t.listMs), ("built", toJson t.built)]
    | none => []
  return Json.mkObj <| [("path", toJson path.toString), ("key", toJson key),
    ("page", toJson page), ("width", toJson width), ("left", toJson columns.start),
    ("right", toJson columns.stop), ("top", toJson band.start), ("bottom", toJson band.stop),
    ("cached", toJson hit),
    ("pixels", toJson (if hit then 0 else (columns.stop - columns.start) * (band.stop - band.start))),
    ("ms", toJson elapsed)] ++ timed

/-- One frame of a Dissolve, Glitter, Fade or Fly transition between two of this
    worker's rasters, written to its runtime directory. Only the last few frames
    are kept: a presentation shows each once. -/
def transitionFrame (shared : Shared) (params : Json) : IO Json := do
  let old ← stringField params "old"
  let new ← stringField params "new"
  -- Only rasters this worker wrote are read.
  for path in [old, new] do
    let inside := [shared.runtime, shared.persistent].any fun dir => path.startsWith (dir.toString ++ "/")
    unless inside && !(path.splitOn "/").contains ".." do
      throw <| IO.userError "Transitions blend this worker's own rasters only"
  let progress := (params.getObjValAs? Float "progress").toOption.getD 0
  -- /Di is degrees, or the name None (Fly: scaling only).
  let undirected := (params.getObjValAs? String "direction").toOption == some "None"
  let direction := if undirected then 0 else ((params.getObjValAs? Float "direction").toOption.getD 0).toUInt32.toNat
  let fly : Nat := (if (params.getObjValAs? String "motion").toOption == some "O" then 1 else 0) +
    (if undirected then 2 else 0) + (if (params.getObjValAs? Bool "opaque").toOption == some true then 4 else 0)
  let scale := (params.getObjValAs? Float "scale").toOption.getD 1.0
  -- For an Emacs canvas: every frame of a presentation replaces one file (its
  -- slot, of eight), which the canvas rereads; image specs naming new files
  -- would each stay in Emacs's image cache.
  let canvas := (params.getObjValAs? String "format").toOption == some "argb"
  let path ← if canvas then pure (shared.runtime / s!"canvas-frame-{(← optionalNat params "slot" 0) % 8}.argb")
    else do
      let serial ← shared.frames.atomically (modifyGet fun n => (n, n + 1))
      if serial ≥ 4 then
        try IO.FS.removeFile (shared.runtime / s!"frame-{serial - 4}.ppm") catch _ => pure ()
      pure (shared.runtime / s!"frame-{serial}.ppm")
  Native.transitionFrame old new path.toString (← stringField params "style") progress (direction % 360).toUInt32
    scale (fly + if canvas then 8 else 0).toUInt8
  return Json.mkObj [("path", toJson path.toString)]

/-- A file name from an untrusted document: no directories, no control characters. -/
def safeFileName (name : String) : String :=
  let base := (name.splitOn "/").getLast!.splitOn "\\" |>.getLast!
  let clean := String.ofList (base.toList.filter fun c => c.toNat ≥ 32 && c != ':')
  if clean.isEmpty || clean == "." || clean == ".." then "attachment.bin" else clean.take 200 |>.toString

def documentRequest (opened : Opened) (method : String) (params : Json) : IO Json := do
  match method with
  | "navigation" =>
    let entries := (← Native.outline opened.doc).map fun entry =>
      { entry with page := Navigation.checkedPage opened.sizes.size entry.page }
    return Json.mkObj [("hash", toJson opened.hash), ("outline", toJson entries),
      ("labels", toJson (← Native.pageLabels opened.doc))]
  | "select" =>
    let page ← natField params "page"
    if page ≥ opened.sizes.size then throw <| IO.userError "Page is outside document"
    let start ← IO.ofExcept (params.getObjValAs? (Option Point) "start")
    let finish ← IO.ofExcept (params.getObjValAs? (Option Point) "end")
    let mode ← match ← stringField params "mode" with
      | "chars" => pure (0 : UInt32)
      | "words" => pure 1
      | "lines" => pure 2
      | _ => throw <| IO.userError "Unknown selection mode"
    let a := start.getD ⟨0, 0⟩
    let b := finish.getD ⟨0, 0⟩
    let selected ← Native.selectText opened.doc page.toUInt32 a.x a.y b.x b.y
      start.isSome finish.isSome mode
    return Json.mkObj [("hash", toJson opened.hash), ("page", toJson page),
      ("text", toJson selected.text), ("quads", toJson selected.quads)]
  | "search" =>
    let page ← natField params "page"
    let query ← stringField params "query"
    let sensitive ← match params.getObjVal? "case_sensitive" with
      | .error _ => pure false
      | .ok value => IO.ofExcept (fromJson? value : Except String Bool)
    let limit ← optionalNat params "limit" 1000
    if page ≥ opened.sizes.size || query.isEmpty || query.utf8ByteSize > 4096 || query.contains '\x00' ||
        limit == 0 || limit > 4096 then throw <| IO.userError "Invalid search page, query or limit"
    let found ← Native.searchPage opened.doc page.toUInt32 query sensitive limit.toUInt32
    return Json.mkObj [("hash", toJson opened.hash), ("page", toJson page),
      ("query", toJson query), ("hits", toJson found.hits), ("truncated", toJson found.truncated)]
  | "text" =>
    let page ← natField params "page"
    if page ≥ opened.sizes.size then throw <| IO.userError "Page is outside document"
    return Json.mkObj [("page", toJson page), ("text", toJson (← Native.pageText opened.doc page.toUInt32))]
  | "info" =>
    let report ← IO.ofExcept (Json.parse (← Native.info opened.doc))
    return report.setObjVal! "hash" (.str opened.hash)
  | "chars" =>
    let page ← natField params "page"
    if page ≥ opened.sizes.size then throw <| IO.userError "Page is outside document"
    let chars ← IO.ofExcept (Json.parse (← Native.pageChars opened.doc page.toUInt32))
    return (chars.setObjVal! "page" (toJson page)).setObjVal! "hash" (.str opened.hash)
  | "page-info" =>
    let page ← natField params "page"
    if page ≥ opened.sizes.size then throw <| IO.userError "Page is outside document"
    IO.ofExcept (Json.parse (← Native.pageInfo opened.doc page.toUInt32))
  | "page-reflow" =>
    let page ← natField params "page"
    if page ≥ opened.sizes.size then throw <| IO.userError "Page is outside document"
    let flag (name : String) (bit : UInt8) := if (params.getObjValAs? Bool name).toOption.getD false then bit else 0
    return .str (← Native.pageReflow opened.doc page.toUInt32 (flag "images" 1 ||| flag "invert" 2))
  | "content-box" =>
    let page ← natField params "page"
    if page ≥ opened.sizes.size then throw <| IO.userError "Page is outside document"
    IO.ofExcept (Json.parse (← Native.contentBox opened.doc page.toUInt32))
  | "signatures" => IO.ofExcept (Json.parse (← Native.signatures opened.doc))
  | "form-fields" => IO.ofExcept (Json.parse (← Native.formFields opened.doc))
  | "annotations" =>
    let page ← natField params "page"
    if page ≥ opened.sizes.size then throw <| IO.userError "Page is outside document"
    return .mkObj [("page", Vellumory.toJson page),
      ("annotations", ← IO.ofExcept (Json.parse (← Native.annotations opened.doc page.toUInt32)))]
  | "attachment" =>
    let bytes ← Native.attachment opened.doc (← natField params "index").toUInt32
    let hash ← Native.sha256 bytes
    let name := safeFileName ((params.getObjValAs? String "filename").toOption.getD "")
    let dir := (← xdgDirectory "XDG_CACHE_HOME" ".cache") / "vellumory" / "attachments" / hash
    let path := dir / name
    unless ← path.pathExists do
      privateDirectory dir
      atomicWrite path bytes
    return Json.mkObj [("path", toJson path.toString), ("hash", toJson hash), ("bytes", toJson bytes.size)]
  | "links" =>
    let page ← natField params "page"
    if page ≥ opened.sizes.size then throw <| IO.userError "Page is outside document"
    return Json.mkObj [("hash", toJson opened.hash), ("page", toJson page),
      ("links", toJson (← Native.pageLinks opened.doc page.toUInt32))]
  | _ => throw <| IO.userError s!"Unknown method: {method}"

def documentMethods : List String := ["navigation", "select", "chars", "search", "text", "links", "info", "page-info", "page-reflow", "content-box", "annotations", "signatures", "form-fields",
  "attachment"]

/-- Asynchronous RPC worker. Each line has an id, method and params; each
    reply carries the same id and either result or error, possibly out of
    order. Renders run on a pool of dedicated threads; other document work
    runs on one document thread; `open`, `close`, `hello` and `cancel` are
    handled immediately on the reading thread. Requests name their document
    (`doc`, default 0): one worker serves every document of a session. -/
def Shared.new (runtime persistent : FilePath) (threads budgetBytes : Nat) : IO Shared := return {
  output := ← Std.Mutex.new (← IO.getStdout), render := ← Lane.new, document := ← Lane.new,
  store := ← Std.Mutex.new { budget := budgetBytes }, runtime, persistent, engine := ← Native.version, threads,
  frames := ← Std.Mutex.new 0 }

def serve : IO UInt32 := do
  let cores ← Native.cores
  let threads := match (← IO.getEnv "VELLUMORY_RENDER_THREADS").bind String.toNat? with
    | some n => max 1 (min 16 n)
    | none => max 2 (min 6 (cores.toNat - 2))
  -- Enough for the screens Emacs keeps around the view, even on HiDPI.
  let budget := ((← IO.getEnv "VELLUMORY_RUNTIME_BUDGET_MB").bind String.toNat?).getD 128
  let runtime ← runtimeDirectory
  -- At most a quarter of what the filesystem (a tmpfs, in memory) has free.
  let free := (← Native.freeBytes runtime.toString).toNat
  let bytes := if free == 0 then budget * 1048576 else min (budget * 1048576) (max 16777216 (free / 4))
  -- Content-addressed and valid forever: only the budget limits it.
  -- Without a writable cache (a build sandbox, a read-only home) previews only
  -- last the session: a cache must never keep the worker from starting.
  let wanted := (← xdgDirectory "XDG_CACHE_HOME" ".cache") / "vellumory" / "rasters"
  let persistent ← try privateDirectory wanted; pure wanted catch _ => pure (runtime / "kept")
  let kept := ((← IO.getEnv "VELLUMORY_PERSISTENT_BUDGET_MB").bind String.toNat?).getD 256
  try trimPersistent persistent (kept * 1048576) catch _ => pure ()
  let shared ← Shared.new runtime persistent threads bytes
  let cores := cores.toNat
  let mut tasks := #[]
  for _ in [0:threads] do
    tasks := tasks.push (← IO.asTask (shared.render.serve shared) .dedicated)
  tasks := tasks.push (← IO.asTask (shared.document.serve shared) .dedicated)
  let input ← IO.getStdin
  -- Documents by the client's id: one worker, one render pool and one raster
  -- budget serve every document of an Emacs session.
  let mut opened : Std.HashMap Nat Opened := {}
  repeat
    let line ← input.getLine
    if line.isEmpty then break
    let parsed := if line.utf8ByteSize > 16777216 then
      Except.error "Request exceeds 16 MiB" else Json.parse line
    -- Keep correlation outside the exception handler: mutable values assigned
    -- inside Lean's try block are rolled back when that block throws.
    let id := (parsed.toOption.bind fun request => (request.getObjVal? "id").toOption).getD Json.null
    try
      let request ← IO.ofExcept parsed
      let method ← stringField request "method"
      let params := (request.getObjVal? "params").toOption.getD (Json.mkObj [])
      let numeric := (fromJson? id : Except String Nat).toOption.getD 0
      match method with
      | "hello" =>
        shared.send <| Json.mkObj [("id", id), ("result", Json.mkObj [("protocol", toJson (3 : Nat)),
          ("mupdf", toJson shared.engine), ("threads", toJson threads), ("cores", toJson cores),
          ("runtime", toJson shared.runtime.toString),
          -- The programs it runs: absolute in the Nix package, names on PATH otherwise.
          ("tools", Json.mkObj [("qpdf", toJson Tools.qpdf), ("verapdf", toJson Tools.verapdf),
            ("ocrmypdf", toJson Tools.ocrmypdf)])])]
      | "open" =>
        let (next, info) ← openFile params
        opened := opened.insert (← optionalNat params "doc" 0) next
        shared.send <| Json.mkObj [("id", id), ("result", info)]
      | "close" =>
        -- Queued jobs hold the document they were queued for; it is freed after them.
        opened := opened.erase (← optionalNat params "doc" 0)
        unless id.isNull do
          shared.send <| Json.mkObj [("id", id), ("result", Json.mkObj [("documents", toJson opened.size)])]
      | "transition-frame" =>
        let priority ← optionalNat params "priority" 0
        let cookie ← Native.Cookie.new
        shared.render.push numeric priority { idJson := id, cookie, action := transitionFrame shared params }
      | "rest" =>
        -- Reading rests: what the C library kept of freed memory goes back.
        let trimmed ← Native.trimMemory
        unless id.isNull do
          shared.send <| Json.mkObj [("id", id), ("result", Json.mkObj [("trimmed", toJson trimmed)])]
      | "cancel" =>
        let ids : List Nat ← IO.ofExcept (params.getObjValAs? (List Nat) "ids")
        let removed := (← shared.render.cancel ids) ++ (← shared.document.cancel ids)
        for job in removed do shared.send (errorReply job.payload.idJson "cancelled")
        unless id.isNull do
          shared.send <| Json.mkObj [("id", id), ("result", Json.mkObj [("dequeued", toJson removed.length)])]
      | _ =>
        let some doc := opened.get? (← optionalNat params "doc" 0) | throw <| IO.userError "No document is open"
        let cookie ← Native.Cookie.new
        let priority ← optionalNat params "priority" 0
        if method == "render" then
          shared.render.push numeric priority { idJson := id, cookie, action := render shared doc cookie params }
        else if documentMethods.contains method then
          shared.document.push numeric priority { idJson := id, cookie, action := documentRequest doc method params }
        else throw <| IO.userError s!"Unknown method: {method}"
    catch e => shared.send (errorReply id e.toString)
  shared.render.stop
  shared.document.stop
  for task in tasks do
    let _ ← IO.wait task
  try IO.FS.removeDirAll shared.runtime catch _ => pure ()
  return 0

end Vellumory.Worker
