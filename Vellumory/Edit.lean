import Vellumory.Json
import Vellumory.Shell
import Vellumory.History
import Vellumory.Native
import Vellumory.Tools
import Vellumory.Metadata
import Std.Time.DateTime.Timestamp

/-! Safe, reversible document edits.

Every edit runs qpdf on an immutable snapshot from the content-addressed
store, checks the result, stores it, records it in the document's history,
and only then replaces the file atomically. The file is never edited in
place, and no state is ever deleted: undo, redo and checkout move between
stored snapshots. If the file changed since the history last saw it, that
external state is recorded first, so it can always be returned to.

Snapshots are durable data, not cache: they live in `$XDG_DATA_HOME`, so
clearing caches never destroys history. -/

open System

namespace Vellumory.Edit

inductive Operation where
  /-- Keep these 1-based pages in this order: reorders, deletes or duplicates. -/
  | select (pages : Array Nat)
  /-- Rotate pages clockwise by a multiple of 90 degrees. -/
  | rotate (pages : Array Nat) (degrees : Int)
  /-- Append all pages of other documents. -/
  | append (files : Array String)
  /-- Lossless: object streams, Flate recompression at level 9. -/
  | compress
  | linearize
  | decrypt (password : String)
  /-- qpdf page-label specs such as `1:r` or `5:D/1/A-`. -/
  | setLabels (specs : Array String)
  | removeLabels
  /-- RELATIONSHIP (Source, Data, Supplement, …) makes it an associated file (PDF/A-3, -4f). -/
  | attach (file key description relationship : String)
  | detach (key : String)
  /-- Add a searchable text layer with ocrmypdf; `mode` is skip, redo or force. -/
  | ocr (languages : String) (mode : String)
  /-- Adopt an edited QDF rendering of the document (from `vellumory qdf`). -/
  | syntax (qdf : String)
  /-- Info, XMP, language and title display, kept consistent; DATE (ISO 8601)
      is the modification date, recorded so replaying the edit is deterministic. -/
  | metadata (set : Array (String × String)) (remove : Array String) (date : String)
  /-- The first page of FILE drawn over (or, UNDER, beneath) the PAGES given in
      qpdf page-range syntax: a stamp, watermark or letterhead. -/
  | stamp (file : String) (under : Bool) (pages : String)
  /-- AES-256 encryption. Passwords are never recorded; PRINT is full, low or
      none, MODIFY all, annotate, form, assembly or none. -/
  | encrypt (user owner : String) (print modify : String) (extract : Bool)
  /-- A KIND annotation (Highlight, Underline, StrikeOut, Squiggly on QUADS, 8 numbers
      each; Text at RECT) on one-based PAGE, dated TIME (seconds since 1970). -/
  | annotate (page : Nat) (kind : String) (quads rect color : Array Float) (contents author : String)
      (time : Float)
  | annotationEdit (page object : Nat) (contents : String) (time : Float)
  | annotationDelete (page object : Nat)
  /-- Form fields set by fully qualified name; JavaScript triggers never run. -/
  | fillForm (values : Array (String × String))
  deriving Inhabited

def pageList (pages : Array Nat) : String := ",".intercalate (pages.toList.map toString)

def Operation.label : Operation → String
  | .select pages => s!"keep pages {pageList pages}"
  | .rotate pages degrees => s!"rotate pages {pageList pages} by {if degrees > 0 then "+" else ""}{degrees}°"
  | .append files => s!"append {", ".intercalate (files.toList.map fun f => (FilePath.mk f).fileName.getD f)}"
  | .compress => "compress losslessly"
  | .linearize => "linearize for fast web view"
  | .decrypt _ => "remove encryption"
  | .setLabels specs => s!"set page labels {" ".intercalate specs.toList}"
  | .removeLabels => "remove page labels"
  | .attach file key _ relationship =>
    s!"attach {(FilePath.mk file).fileName.getD file} as {key}{if relationship.isEmpty then "" else s!" ({relationship.toLower} of the document)"}"
  | .detach key => s!"remove attachment {key}"
  | .ocr languages mode => s!"recognize text ({languages}, {mode})"
  | .syntax _ => "edit PDF syntax"
  | .metadata set remove _ => s!"set metadata ({", ".intercalate ((set.map (·.1) ++ remove).toList)})"
  | .stamp file under pages =>
    s!"{if under then "underlay" else "stamp"} {(FilePath.mk file).fileName.getD file} on pages {pages}"
  | .encrypt user _ print modify extract =>
    s!"encrypt (AES-256{if user.isEmpty then "" else ", password to open"}; print {print}, modify {modify}{if extract then "" else ", no copying"})"
  | .annotate page kind .. => s!"{if kind == "Text" then "note" else kind.toLower} on page {page}"
  | .annotationEdit page .. => s!"edit annotation on page {page}"
  | .annotationDelete page _ => s!"delete annotation on page {page}"
  | .fillForm values => s!"fill form ({", ".intercalate (values.map (·.1)).toList})"

def Operation.toJson : Operation → Json
  | .select pages => .mkObj [("op", .str "select"), ("pages", Vellumory.toJson pages)]
  | .rotate pages degrees => .mkObj [("op", .str "rotate"), ("pages", Vellumory.toJson pages),
      ("degrees", .num (Float.ofInt degrees))]
  | .append files => .mkObj [("op", .str "append"), ("files", Vellumory.toJson files)]
  | .compress => .mkObj [("op", .str "compress")]
  | .linearize => .mkObj [("op", .str "linearize")]
  | .decrypt _ => .mkObj [("op", .str "decrypt")]   -- the password is never recorded
  | .setLabels specs => .mkObj [("op", .str "labels"), ("specs", Vellumory.toJson specs)]
  | .removeLabels => .mkObj [("op", .str "remove-labels")]
  | .attach file key description relationship => .mkObj [("op", .str "attach"), ("file", .str file),
      ("key", .str key), ("description", .str description), ("relationship", .str relationship)]
  | .detach key => .mkObj [("op", .str "detach"), ("key", .str key)]
  | .ocr languages mode => .mkObj [("op", .str "ocr"), ("languages", .str languages), ("mode", .str mode)]
  | .syntax _ => .mkObj [("op", .str "syntax")]
  | .metadata set remove date => .mkObj [("op", .str "metadata"),
      ("set", .obj (set.map fun (k, v) => (k, .str v))), ("remove", Vellumory.toJson remove), ("date", .str date)]
  | .stamp file under pages => .mkObj [("op", .str "stamp"), ("file", .str file), ("under", .bool under),
      ("pages", .str pages)]
  -- Neither password is ever recorded.
  | .encrypt _ _ print modify extract => .mkObj [("op", .str "encrypt"), ("print", .str print),
      ("modify", .str modify), ("extract", .bool extract)]
  | .annotate page kind quads rect color contents author time => .mkObj [("op", .str "annotate"),
      ("page", Vellumory.toJson page), ("kind", .str kind), ("quads", .arr (quads.map .num)),
      ("rect", .arr (rect.map .num)), ("color", .arr (color.map .num)), ("contents", .str contents),
      ("author", .str author), ("time", .num time)]
  | .annotationEdit page object contents time => .mkObj [("op", .str "annotation-edit"),
      ("page", Vellumory.toJson page), ("object", Vellumory.toJson object), ("contents", .str contents),
      ("time", .num time)]
  | .annotationDelete page object => .mkObj [("op", .str "annotation-delete"), ("page", Vellumory.toJson page),
      ("object", Vellumory.toJson object)]
  | .fillForm values => .mkObj [("op", .str "fill-form"), ("values", .obj (values.map fun (k, v) => (k, .str v)))]

def Operation.fromJson (j : Json) : Except String Operation := do
  match ← j.getObjValAs? String "op" with
  | "select" => return .select (← j.getObjValAs? (Array Nat) "pages")
  | "rotate" =>
    let degrees ← j.getObjValAs? Float "degrees"
    return .rotate (← j.getObjValAs? (Array Nat) "pages") (if degrees < 0 then -((-degrees).toUInt64.toNat : Int)
                                                           else (degrees.toUInt64.toNat : Int))
  | "append" => return .append (← j.getObjValAs? (Array String) "files")
  | "compress" => return .compress
  | "linearize" => return .linearize
  | "decrypt" => return .decrypt ((j.getObjValAs? String "password").toOption.getD "")
  | "labels" => return .setLabels (← j.getObjValAs? (Array String) "specs")
  | "remove-labels" => return .removeLabels
  | "attach" => return .attach (← j.getObjValAs? String "file") (← j.getObjValAs? String "key")
                  ((j.getObjValAs? String "description").toOption.getD "")
                  ((j.getObjValAs? String "relationship").toOption.getD "")
  | "detach" => return .detach (← j.getObjValAs? String "key")
  | "syntax" => return .syntax (← j.getObjValAs? String "qdf")
  | "metadata" =>
    let set ← match j.getObjValD "set" with
      | .obj fields => fields.mapM fun (k, v) => match v with
        | .str s => pure (k, s)
        | _ => throw s!"metadata value of {k} must be a string"
      | .null => pure #[]
      | _ => throw "metadata set must be an object"
    return .metadata set ((j.getObjValAs? (Array String) "remove").toOption.getD #[])
      ((j.getObjValAs? String "date").toOption.getD "")
  | "ocr" => return .ocr ((j.getObjValAs? String "languages").toOption.getD "eng")
                        ((j.getObjValAs? String "mode").toOption.getD "skip")
  | "stamp" => return .stamp (← j.getObjValAs? String "file") ((j.getObjValAs? Bool "under").toOption.getD false)
                  ((j.getObjValAs? String "pages").toOption.getD "1-z")
  | "annotate" =>
    let numbers := fun (key : String) => (j.getObjValAs? (Array Float) key).toOption.getD #[]
    return .annotate (← j.getObjValAs? Nat "page") (← j.getObjValAs? String "kind") (numbers "quads")
      (numbers "rect") (numbers "color") ((j.getObjValAs? String "contents").toOption.getD "")
      ((j.getObjValAs? String "author").toOption.getD "") ((j.getObjValAs? Float "time").toOption.getD 0)
  | "annotation-edit" =>
    return .annotationEdit (← j.getObjValAs? Nat "page") (← j.getObjValAs? Nat "object")
      ((j.getObjValAs? String "contents").toOption.getD "") ((j.getObjValAs? Float "time").toOption.getD 0)
  | "fill-form" =>
    match j.getObjValD "values" with
    | .obj fields => return .fillForm (← fields.mapM fun (k, v) => match v with
        | .str s => pure (k, s)
        | _ => throw s!"the value of {k} must be a string")
    | _ => throw "fill-form needs an object of values"
  | "annotation-delete" => return .annotationDelete (← j.getObjValAs? Nat "page") (← j.getObjValAs? Nat "object")
  | "encrypt" => return .encrypt ((j.getObjValAs? String "user").toOption.getD "")
                  ((j.getObjValAs? String "owner").toOption.getD "")
                  ((j.getObjValAs? String "print").toOption.getD "full")
                  ((j.getObjValAs? String "modify").toOption.getD "all")
                  ((j.getObjValAs? Bool "extract").toOption.getD true)
  | other => throw s!"unknown operation {other}"

/-- Reject operations that cannot apply to a document with `count` pages. -/
def Operation.validate (count : Nat) : Operation → Except String Unit
  | .select pages => do
    if pages.isEmpty then throw "a document needs at least one page"
    unless pages.all (fun p => 1 ≤ p && p ≤ count) do throw s!"pages must be within 1–{count}"
  | .rotate pages degrees => do
    if pages.isEmpty then throw "no pages to rotate"
    unless pages.all (fun p => 1 ≤ p && p ≤ count) do throw s!"pages must be within 1–{count}"
    unless degrees % 90 == 0 && degrees != 0 do throw "rotation must be a nonzero multiple of 90°"
  | .append files => if files.isEmpty then throw "no documents to append" else pure ()
  | .setLabels specs => if specs.isEmpty then throw "no page-label specification" else pure ()
  | .attach _ key _ relationship => do
    if key.isEmpty then throw "attachment key is empty"
    -- ISO 32000-2 14.13.2.
    unless ["", "Source", "Data", "Alternative", "Supplement", "EncryptedPayload", "FormData", "Schema",
        "Unspecified"].contains relationship do
      throw s!"{relationship} is not an associated-file relationship (Source, Data, Supplement, …)"
  | .detach key => if key.isEmpty then throw "attachment key is empty" else pure ()
  | .ocr languages mode => do
    unless ["skip", "redo", "force"].contains mode do throw "OCR mode is skip, redo or force"
    unless !languages.isEmpty && languages.all (fun c => c.isAlphanum || c == '+' || c == '_') do
      throw "OCR languages are tesseract codes such as eng or deu+eng"
  | .metadata set remove date => Metadata.validate set remove date
  | .stamp file _ pages => do
    if file.isEmpty then throw "no stamp document"
    unless !pages.isEmpty && pages.toList.all (fun c => c.isDigit || ",-zr".toList.contains c) do
      throw "pages are a qpdf page range such as 1-z, 1,3,5 or 2-r1"
  | .annotate page kind quads rect color _ _ _ => do
    unless 1 ≤ page && page ≤ count do throw s!"page must be within 1–{count}"
    unless ["Highlight", "Underline", "StrikeOut", "Squiggly", "Text"].contains kind do
      throw "annotations are Highlight, Underline, StrikeOut, Squiggly or Text"
    if kind == "Text" then
      unless rect.size == 4 do throw "a note needs its rectangle"
    else
      unless !quads.isEmpty && quads.size % 8 == 0 do throw "markup needs quadrilaterals of 8 numbers each"
    unless color.isEmpty || (color.size == 3 && color.all (fun c => 0 ≤ c && c ≤ 1)) do
      throw "a colour is three numbers from 0 to 1"
  | .annotationEdit page .. | .annotationDelete page _ =>
    unless 1 ≤ page && page ≤ count do throw s!"page must be within 1–{count}"
  | .fillForm values => if values.isEmpty then throw "no form fields to fill" else pure ()
  | .encrypt user owner print modify _ => do
    if owner.isEmpty then throw "an owner password is required to keep the permissions"
    if user == owner then throw "the passwords to open and to change permissions must differ"
    if (user ++ owner).any (· == '\n') then throw "passwords cannot contain line breaks"
    unless ["full", "low", "none"].contains print do throw "print is full, low or none"
    unless ["all", "annotate", "form", "assembly", "none"].contains modify do
      throw "modify is all, annotate, form, assembly or none"
  | _ => pure ()

/-- An attachment's MIME type from its extension (PDF/A-3 and -4 require one). -/
def mimeType (file : String) : String :=
  match ((FilePath.mk file).extension.getD "").toLower with
  | "org" => "text/x-org" | "tex" => "text/x-tex" | "typ" => "text/x-typst" | "bib" => "text/x-bibtex"
  | "md" => "text/markdown" | "txt" => "text/plain" | "csv" => "text/csv" | "json" => "application/json"
  | "xml" => "application/xml" | "html" => "text/html" | "pdf" => "application/pdf" | "png" => "image/png"
  | "jpg" | "jpeg" => "image/jpeg" | "svg" => "image/svg+xml" | "zip" => "application/zip"
  | _ => "application/octet-stream"

/-- qpdf arguments. Input and output are passed as literal argv elements;
    the primary input keeps document-level data (outlines, tags) for page
    selection, per qpdf's page-selection documentation. PDFA is the PDF/A part
    the input claims. -/
def Operation.qpdfArgs (op : Operation) (input output : String) (pdfa : Option Nat := none) : Array String :=
  -- Keeps PDF/A: without it, rewriting a PDF/A-2b file failed veraPDF's
  -- ISO 19005-2 6.1.7.1 (EOL around stream keywords) on every stream.
  let deterministic := #["--deterministic-id", "--newline-before-endstream"]
  -- PDF/A-1 allows no object streams (ISO 19005-1 6.1.4; veraPDF failed it).
  let objectStreams := if pdfa == some 1 then "--object-streams=disable" else "--object-streams=generate"
  match op with
  -- Files as --file=: given positionally, a name such as 2-3 would be read as
  -- a page range of the file before it (qpdf 11.9 and later).
  | .select pages => #[input, "--pages", "--file=.", s!"--range={pageList pages}", "--", output] ++ deterministic
  | .rotate pages degrees =>
    let normalized := degrees % 360
    let angle := if normalized < 0 then normalized + 360 else normalized
    #[input, output, s!"--rotate=+{angle}:{pageList pages}"] ++ deterministic
  | .append files => #[input, "--pages", "--file=."] ++ files.map ("--file=" ++ ·) ++ #["--", output] ++ deterministic
  | .compress => #[input, output, objectStreams, "--compress-streams=y",
      "--recompress-flate", "--compression-level=9"] ++ deterministic
  | .linearize => #[input, output, "--linearize"] ++ deterministic
  -- Secrets go through files beside the output, never the command line.
  | .decrypt _ => #[input, output, s!"--password-file={output}.password", "--decrypt"] ++ deterministic
  | .setLabels specs => #[input, output, "--set-page-labels"] ++ specs ++ #["--"] ++ deterministic
  | .removeLabels => #[input, output, "--remove-page-labels"] ++ deterministic
  -- The update (written by Metadata.dissociate) comes first: qpdf applies it on reading.
  -- PDF/UA-2 requires a description of every embedded file (ISO 14289-2 8.14.1).
  | .attach file key description _ => #[input, s!"--update-from-json={output}.update.json", output,
      "--add-attachment", file, s!"--key={key}",
      s!"--description={if description.isEmpty then (FilePath.mk file).fileName.getD file else description}",
      s!"--mimetype={mimeType file}", "--replace", "--"] ++ deterministic
  | .detach key => #[input, s!"--update-from-json={output}.update.json", output, s!"--remove-attachment={key}",
      s!"@{output}.args"] ++ deterministic
  | .ocr languages mode =>
    let flag := match mode with | "redo" => "--redo-ocr" | "force" => "--force-ocr" | _ => "--skip-text"
    #[flag, "--language", languages, "--output-type", "pdf", "--quiet", input, output]
  | .syntax qdf => #[qdf, output, objectStreams, "--compress-streams=y"] ++ deterministic
  | .metadata .. => #[input, s!"--update-from-json={output}.update.json", output, s!"@{output}.args"] ++
      deterministic
  | .stamp file under pages => #[input, output, if under then "--underlay" else "--overlay", s!"--file={file}",
      s!"--to={pages}", "--from=1", "--repeat=1", "--"] ++ deterministic
  | .encrypt .. => #[input, output, s!"@{output}.args", "--newline-before-endstream"]
  -- MuPDF writes the annotation beside the output; qpdf rewrites it like any edit.
  | .annotate .. | .annotationEdit .. | .annotationDelete .. | .fillForm .. =>
    #[s!"{output}.annotated", output] ++ deterministic

/-- The program that performs an operation: ocrmypdf for OCR, qpdf otherwise. -/
def Operation.program : Operation → String
  | .ocr .. => Tools.ocrmypdf
  | _ => Tools.qpdf

/-- One step of an edit: a tool run with literal arguments, or work done here. -/
inductive Step where
  | tool (program : String) (args : Array String)
  | native (run : IO Unit)

/-- Files beside the output that edits write for qpdf: metadata updates, and
    the secrets of encryption and decryption (in the store's private directory,
    removed after the edit). -/
def sideFiles (output : String) : Array String :=
  #[s!"{output}.update.json", s!"{output}.xmp", s!"{output}.args", s!"{output}.password", s!"{output}.annotated",
    s!"{output}.stage.pdf", s!"{output}.stage.pdf.update.json"]

/-- qpdf can lose a JSON update of an object inside an object stream: a PDF/A-4f
    document's /AF array kept its old value, and qpdf exited 0 (qpdf 12.3.2). As
    its manual advises, updates go to a copy written without object streams;
    these arguments make that copy. -/
def flatten (input flat : String) : Array String := #[input, flat, "--object-streams=disable", "--deterministic-id"]

/-- The argument file for writing an update's result: object streams again if
    INPUT had them, except in PDF/A-1 (ISO 19005-1 6.1.4). -/
def streamArguments (input : String) (pdfa : Option Nat) (file : String) : IO Unit := do
  let regenerate := pdfa != some 1 && (← Metadata.hasObjectStreams input)
  IO.FS.writeFile file s!"--object-streams={if regenerate then "generate" else "disable"}\n"

/-- The qpdf argument file of an encryption: one argument per line. -/
def encryptionArguments (user owner print modify : String) (extract : Bool) : String :=
  "\n".intercalate ["--encrypt", s!"--user-password={user}", s!"--owner-password={owner}", "--bits=256",
    s!"--print={print}", s!"--modify={modify}", s!"--extract={if extract then "y" else "n"}", "--"] ++ "\n"

/-- Steps to run, in order. A syntax edit first lets fix-qdf recompute
    offsets and stream lengths, then qpdf writes a normal PDF from it. A
    metadata edit first prepares the qpdf update and the new XMP packet. -/
def Operation.steps (op : Operation) (input output scratch : String) (pdfa : Option Nat := none) :
    Array Step :=
  match op with
  | .syntax qdf =>
    #[.tool Tools.fixQdf #[qdf, scratch], .tool Tools.qpdf ((Operation.syntax scratch).qpdfArgs input output pdfa)]
  | .metadata set remove date =>
    #[.native (streamArguments input pdfa s!"{output}.args"), .tool Tools.qpdf (flatten input scratch),
      .native (Metadata.prepare scratch s!"{output}.xmp" s!"{output}.update.json" set remove date),
      .tool Tools.qpdf (op.qpdfArgs scratch output pdfa)]
  | .attach file key _ relationship =>
    -- PDF/A-3 and -4 require every embedded file to name its relationship
    -- (ISO 19005-3 6.8; veraPDF rule 6.9-4).
    let relationship := if relationship.isEmpty && (pdfa == some 3 || pdfa == some 4) then "Unspecified"
      else relationship
    let stage := s!"{output}.stage.pdf"
    -- A replaced attachment leaves the associated files first, or it would stay
    -- in the file; then the new one joins them.
    #[.native (Metadata.checkAttachable input file), .native (streamArguments input pdfa s!"{output}.args"),
      .tool Tools.qpdf (flatten input scratch), .native (Metadata.dissociate scratch key s!"{stage}.update.json"),
      .tool Tools.qpdf (op.qpdfArgs scratch stage pdfa),
      .native (Metadata.associate stage key relationship s!"{output}.update.json"),
      .tool Tools.qpdf ((Operation.metadata #[] #[] "").qpdfArgs stage output pdfa)]
  | .detach key =>
    #[.native (streamArguments input pdfa s!"{output}.args"), .tool Tools.qpdf (flatten input scratch),
      .native (Metadata.dissociate scratch key s!"{output}.update.json"),
      .tool Tools.qpdf (op.qpdfArgs scratch output pdfa)]
  | .encrypt user owner print modify extract =>
    #[.native (IO.FS.writeFile s!"{output}.args" (encryptionArguments user owner print modify extract)),
      .tool Tools.qpdf (op.qpdfArgs input output pdfa)]
  | .decrypt password =>
    #[.native (IO.FS.writeFile s!"{output}.password" (password ++ "\n")), .tool Tools.qpdf (op.qpdfArgs input output pdfa)]
  | .annotate page kind quads rect color contents author time =>
    -- A highlight's appearance multiplies its colour with the page: transparency,
    -- which PDF/A-1 forbids (ISO 19005-1 6.4; veraPDF failed it).
    if pdfa == some 1 && kind == "Highlight" then
      #[.native (throw <| IO.userError "PDF/A-1 forbids the transparency highlights need; underline instead")]
    else
    #[.native (discard <| Native.annotAdd input s!"{output}.annotated" (page - 1).toUInt32 kind ⟨quads⟩ ⟨rect⟩ ⟨color⟩
        contents author time),
      .tool Tools.qpdf (op.qpdfArgs input output pdfa)]
  | .annotationEdit page object contents time =>
    #[.native (Native.annotChange input s!"{output}.annotated" (page - 1).toUInt32 object.toUInt32 false contents time),
      .tool Tools.qpdf (op.qpdfArgs input output pdfa)]
  | .annotationDelete page object =>
    #[.native (Native.annotChange input s!"{output}.annotated" (page - 1).toUInt32 object.toUInt32 true "" 0),
      .tool Tools.qpdf (op.qpdfArgs input output pdfa)]
  | .fillForm values =>
    #[.native (Native.fillForm input s!"{output}.annotated" (values.map (·.1)) (values.map (·.2))),
      .tool Tools.qpdf (op.qpdfArgs input output pdfa)]
  | _ => #[.tool op.program (op.qpdfArgs input output pdfa)]

/-- Whether qpdf reads the current state directly: every operation except
    adopting edited syntax (which replaces it) and annotations (MuPDF writes
    them to an intermediate file that qpdf then reads). -/
def Operation.reads : Operation → Bool
  | .syntax _ | .annotate .. | .annotationEdit .. | .annotationDelete .. | .fillForm .. => false
  | _ => true

/-- The output, and the input of every operation that reads it, are passed
    through as literal arguments, whatever their characters. -/
theorem qpdfArgs_paths (op : Operation) (input output : String) (pdfa : Option Nat) :
    output ∈ op.qpdfArgs input output pdfa ∧ (op.reads → input ∈ op.qpdfArgs input output pdfa) := by
  cases op <;> simp [Operation.qpdfArgs, Operation.reads]

/-! ## Store -/

structure Store where
  root : FilePath

def Store.default : IO Store := do
  if let some root := ← IO.getEnv "VELLUMORY_STORE" then return { root := FilePath.mk root }
  let data ← match ← IO.getEnv "XDG_DATA_HOME" with
    | some p => if p.isEmpty then home else pure (FilePath.mk p)
    | none => home
  return { root := data / "vellumory" }
where home := do return FilePath.mk ((← IO.getEnv "HOME").getD ".") / ".local" / "share"

def Store.object (s : Store) (hash : String) : FilePath :=
  s.root / "objects" / String.ofList (hash.toList.take 2) / (hash ++ ".pdf")

def Store.historyFile (s : Store) (key : String) : FilePath :=
  s.root / "history" / (key ++ ".json")

def privateDirectory (path : FilePath) : IO Unit := do
  IO.FS.createDirAll path
  IO.setAccessRights path { user := { read := true, write := true, execution := true } }

def nonce : IO String := return s!"{← IO.Process.getPID}-{← IO.rand 0 0x7fffffff}"

/-- Write bytes next to `path` and rename over it: readers see old or new, never half. -/
def atomicWrite (path : FilePath) (bytes : ByteArray) : IO Unit := do
  let temporary := FilePath.mk s!"{path}.vellumory-{← nonce}"
  try
    IO.FS.writeBinFile temporary bytes
    if ← path.pathExists then
      -- Keep the original permissions, not the umask default.
      Native.setFileMode temporary.toString (← Native.fileMode path.toString)
    IO.FS.rename temporary path
  finally
    if ← temporary.pathExists then IO.FS.removeFile temporary

/-- Store bytes under their hash. Existing objects are never rewritten. -/
def Store.put (s : Store) (bytes : ByteArray) : IO String := do
  let hash ← Native.sha256 bytes
  let path := s.object hash
  unless ← path.pathExists do
    privateDirectory (path.parent.getD s.root)
    atomicWrite path bytes
  return hash

def Store.get (s : Store) (hash : String) : IO ByteArray := do
  let bytes ← IO.FS.readBinFile (s.object hash)
  -- Content addressing is checked, not assumed, on every restore.
  unless (← Native.sha256 bytes) == hash do
    throw <| IO.userError s!"snapshot {hash.take 12} is corrupt"
  return bytes

def nowMs : IO Nat := do
  return (← Std.Time.Timestamp.now).toMillisecondsSinceUnixEpoch.val.toNat

def historyKey (path : FilePath) : IO String := Native.sha256 path.toString.toUTF8

def loadHistory (s : Store) (path : FilePath) : IO (Option History.History) := do
  let file := s.historyFile (← historyKey path)
  unless ← file.pathExists do return none
  match Json.parse (← IO.FS.readFile file) >>= History.fromJson with
  | .ok h => return some h
  | .error e => throw <| IO.userError s!"history {file} is unreadable: {e}"

def saveHistory (s : Store) (h : History.History) : IO Unit := do
  privateDirectory (s.root / "history")
  atomicWrite (s.historyFile (← historyKey h.path)) (h.toJson.compress ++ "\n").toUTF8

/-- Serialize all history changes for one document across processes. -/
def withLock (s : Store) (path : FilePath) (action : IO α) : IO α := do
  privateDirectory (s.root / "locks")
  let handle ← IO.FS.Handle.mk (s.root / "locks" / ((← historyKey path) ++ ".lock")) .append
  handle.lock
  try action finally handle.unlock

def pageCount (bytes : ByteArray) (password : String := "") : IO Nat := do
  return (← Native.pageSizes (← Native.openDocument bytes password)).size

/-- The history of `path`, with the file's current bytes as head. A file
    changed behind the history's back gets a node of its own. -/
def synchronize (s : Store) (path : FilePath) : IO (History.History × ByteArray) := do
  let bytes ← IO.FS.readBinFile path
  let hash ← s.put bytes
  let time ← nowMs
  match ← loadHistory s path with
  | none =>
    let h := History.start path.toString hash time (← pageCount bytes)
    saveHistory s h
    return (h, bytes)
  | some h =>
    if (h.current.map (·.hash)) == some hash then return (h, bytes)
    -- Prefer moving to an existing node with these bytes over a duplicate.
    match h.nodes.findIdx? (·.hash == hash) with
    | some i =>
      let h := (h.checkout i).getD h
      saveHistory s h
      return (h, bytes)
    | none =>
      let h := h.commit hash (.mkObj [("op", .str "external")]) time "changed outside Vellumory"
      saveHistory s h
      return (h, bytes)

def nodeJson (h : History.History) (i : Nat) : Json :=
  match h.nodes[i]? with
  | some n => (n.toJson).setObjVal! "index" (Vellumory.toJson i)
  | none => .null

def summary (h : History.History) (changed : Bool) : Json :=
  .mkObj [("changed", .bool changed), ("head", Vellumory.toJson h.head),
          ("node", nodeJson h h.head), ("nodes", Vellumory.toJson h.nodes.size)]

def runTool (program : String) (args : Array String) : IO Unit := do
  let result ← Shell.run <| Shell.capture { program, args }
  -- qpdf: 0 clean, 3 warnings; ocrmypdf: 0 success. Both produced a usable file.
  unless result.exitCode == 0 || (program == Tools.qpdf && result.exitCode == 3) do
    throw <| IO.userError s!"{program} failed ({result.exitCode}): {result.stderr.trimAscii}"

def runQpdf (args : Array String) : IO Unit := runTool Tools.qpdf args

/-- Apply `op` to the document at `path`, recording it in its history. -/
def apply (s : Store) (path : FilePath) (op : Operation) : IO Json := do
  let path ← IO.FS.realPath path
  withLock s path do
    let (h, bytes) ← synchronize s path
    let some head := h.current | throw <| IO.userError "history has no head"
    -- Decryption is the one edit that can read a document needing a password.
    IO.ofExcept (op.validate (← pageCount bytes (match op with | .decrypt password => password | _ => "")))
    privateDirectory (s.root / "tmp")
    let output := s.root / "tmp" / s!"edit-{← nonce}.pdf"
    try
      let scratch := s.root / "tmp" / s!"scratch-{← nonce}.pdf"
      try
        let input := (s.object head.hash).toString
        for step in op.steps input output.toString scratch.toString (← Metadata.pdfaPart input) do
          match step with
          | .tool program args => runTool program args
          | .native run => run
      finally
        for file in #[scratch.toString] ++ sideFiles output.toString do
          if ← FilePath.pathExists file then IO.FS.removeFile file
      let result ← IO.FS.readBinFile output
      -- The result must open and have pages before it may replace the file.
      let pages ← pageCount result (match op with | .encrypt user .. => user | _ => "")
      let hash ← s.put result
      if hash == head.hash then return summary h false
      let label := match op with
        | .compress => s!"{op.label} ({bytes.size} → {result.size} bytes)"
        | _ => s!"{op.label} ({pages} pages)"
      let h := h.commit hash op.toJson (← nowMs) label
      saveHistory s h
      atomicWrite path result
      return summary h true
    finally
      if ← output.pathExists then IO.FS.removeFile output

/-- Move to another stored state of `path` and write it to the file. -/
def move (s : Store) (path : FilePath) (step : History.History → Option History.History)
    (failure : String) : IO Json := do
  let path ← IO.FS.realPath path
  withLock s path do
    let (h, _) ← synchronize s path
    let some next := step h | throw <| IO.userError failure
    let some node := next.current | throw <| IO.userError "history has no head"
    let bytes ← s.get node.hash
    saveHistory s next
    atomicWrite path bytes
    return summary next true

def undo (s : Store) (path : FilePath) : IO Json := move s path (·.undo) "Nothing to undo"
def redo (s : Store) (path : FilePath) : IO Json := move s path (·.redo) "Nothing to redo"
def checkout (s : Store) (path : FilePath) (i : Nat) : IO Json :=
  move s path (·.checkout i) s!"No state {i} in this history"

def history (s : Store) (path : FilePath) : IO Json := do
  let path ← IO.FS.realPath path
  withLock s path do
    let (h, _) ← synchronize s path
    return .mkObj [("path", .str h.path), ("head", Vellumory.toJson h.head),
      ("nodes", .arr ((List.range h.nodes.size).toArray.map (nodeJson h)))]

/-- Bytes of a stored state, for previews and diffs; the file is untouched. -/
def snapshot (s : Store) (path : FilePath) (i : Nat) : IO FilePath := do
  let path ← IO.FS.realPath path
  let some h := ← loadHistory s path | throw <| IO.userError "No history for this document"
  let some node := h.nodes[i]? | throw <| IO.userError s!"No state {i} in this history"
  let _ ← s.get node.hash
  return s.object node.hash

end Vellumory.Edit
