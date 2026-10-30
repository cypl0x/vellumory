import Cli
import Vellumory.Pdf
import Vellumory.Geometry
import Vellumory.Worker
import Vellumory.Edit
import Vellumory.Validate
import Vellumory.DocDiff
import Vellumory.Preflight

open Cli Vellumory

def checkPdf (p : Parsed) : IO UInt32 := do
  let report ← Pdf.checkWithQpdf (p.positionalArg! "file" |>.as! String)
  IO.print report.stdout
  IO.eprint report.stderr
  IO.eprintln "Evidence: external qpdf structural check; not a Lean proof of PDF conformance."
  return report.exitCode

def planClip (p : Parsed) : IO UInt32 := do
  let height := p.positionalArg! "height" |>.as! Nat
  let top := p.positionalArg! "top" |>.as! Nat
  let bottom := p.positionalArg! "bottom" |>.as! Nat
  if top > bottom then throw <| IO.userError "top must not exceed bottom"
  let band := clipBand height ⟨0, height⟩ ⟨top, bottom⟩
  IO.println <| Json.compress <| Json.mkObj
    [("start", toJson band.start), ("stop", toJson band.stop)]
  return 0

def checkCmd : Cmd := `[Cli|
  check VIA checkPdf;
  "Check PDF structure with qpdf. Preserve its exit codes and warnings."
  ARGS:
    file : String; "PDF file to inspect without modifying it."
]

def clipCmd : Cmd := `[Cli|
  clip VIA planClip;
  "Plan a clipped raster interval using the proved integer geometry."
  ARGS:
    height : Nat; "Page height in raster pixels."
    top : Nat; "Viewport top (inclusive)."
    bottom : Nat; "Viewport bottom (exclusive)."
]

def runWorker (_ : Parsed) : IO UInt32 := Worker.serve

def serveCmd : Cmd := `[Cli|
  serve VIA runWorker;
  "Serve the asynchronous Emacs PDF protocol over JSON lines on stdio."
]

/-- Print JSON on success; report errors as JSON on stderr with exit 1. -/
def jsonCommand (action : IO Json) : IO UInt32 := do
  try
    IO.println (← action).compress
    return 0
  catch e =>
    IO.eprintln (Json.mkObj [("error", .str e.toString)]).compress
    return 1

def fileArg (p : Parsed) : System.FilePath := p.positionalArg! "file" |>.as! String

def runEdit (p : Parsed) : IO UInt32 := jsonCommand do
  -- "-": read the operation from standard input, so secrets never appear in argv.
  let text := p.positionalArg! "operation" |>.as! String
  let text ← if text == "-" then (← IO.getStdin).readToEnd else pure text
  let op ← IO.ofExcept (Json.parse text >>= Edit.Operation.fromJson)
  Edit.apply (← Edit.Store.default) (fileArg p) op

def editCmd : Cmd := `[Cli|
  edit VIA runEdit;
  "Apply a recorded, reversible edit. The file is replaced atomically; every state stays in the store."
  ARGS:
    file : String; "Document to edit."
    operation : String; "JSON operation, e.g. {\"op\":\"rotate\",\"pages\":[1],\"degrees\":90}, or - to read it from standard input."
]

def runUndo (p : Parsed) : IO UInt32 := jsonCommand do Edit.undo (← Edit.Store.default) (fileArg p)
def runRedo (p : Parsed) : IO UInt32 := jsonCommand do Edit.redo (← Edit.Store.default) (fileArg p)
def runHistory (p : Parsed) : IO UInt32 := jsonCommand do Edit.history (← Edit.Store.default) (fileArg p)
def runCheckout (p : Parsed) : IO UInt32 := jsonCommand do
  Edit.checkout (← Edit.Store.default) (fileArg p) (p.positionalArg! "state" |>.as! Nat)
def runSnapshot (p : Parsed) : IO UInt32 := jsonCommand do
  let path ← Edit.snapshot (← Edit.Store.default) (fileArg p) (p.positionalArg! "state" |>.as! Nat)
  return .mkObj [("path", .str path.toString)]

def undoCmd : Cmd := `[Cli|
  undo VIA runUndo; "Restore the state before the last edit (a stored snapshot)."
  ARGS:
    file : String; "Document."
]
def redoCmd : Cmd := `[Cli|
  redo VIA runRedo; "Return to the most recent state after the current one."
  ARGS:
    file : String; "Document."
]
def historyCmd : Cmd := `[Cli|
  history VIA runHistory; "Print the document's branching history as JSON."
  ARGS:
    file : String; "Document."
]
def checkoutCmd : Cmd := `[Cli|
  checkout VIA runCheckout; "Make any recorded state current; nothing is discarded."
  ARGS:
    file : String; "Document."
    state : Nat; "Index of the state in the history."
]
def snapshotCmd : Cmd := `[Cli|
  snapshot VIA runSnapshot; "Print the store path of a recorded state without changing the file."
  ARGS:
    file : String; "Document."
    state : Nat; "Index of the state in the history."
]

def runText (p : Parsed) : IO UInt32 := do
  let bytes ← IO.FS.readBinFile (fileArg p)
  let doc ← Native.openDocumentAs bytes (fileArg p).toString ""
  let count := (← Native.pageSizes doc).size
  for page in [0:count] do
    IO.print (← Native.pageText doc page.toUInt32)
    if page + 1 < count then IO.print "\x0c"
  return 0
def textCmd : Cmd := `[Cli|
  text VIA runText; "Print the text of every page; pages are separated by form feeds."
  ARGS:
    file : String; "Document."
]

def runInfo (p : Parsed) : IO UInt32 := do
  let bytes ← IO.FS.readBinFile (fileArg p)
  let doc ← Native.openDocumentAs bytes (fileArg p).toString ""
  IO.println (← Native.info doc)
  return 0
def infoCmd : Cmd := `[Cli|
  info VIA runInfo; "Print format, structure, metadata, fonts, images and attachments as JSON."
  ARGS:
    file : String; "Document."
]

def runPreflight (p : Parsed) : IO UInt32 := do
  let bytes ← IO.FS.readBinFile (fileArg p)
  let doc ← Native.openDocumentAs bytes (fileArg p).toString ""
  IO.println (toJson (← Preflight.check doc)).compress
  return 0
def preflightCmd : Cmd := `[Cli|
  preflight VIA runPreflight; "Check beyond veraPDF: broken links and contents, earlier versions inside, weak encryption, scripts, unembedded fonts, page boxes."
  ARGS:
    file : String; "Document."
]

def runDiff (p : Parsed) : IO UInt32 := jsonCommand do
  DocDiff.compare (p.positionalArg! "old" |>.as! String) (p.positionalArg! "new" |>.as! String)
def diffCmd : Cmd := `[Cli|
  diff VIA runDiff; "Compare two versions: aligned pages, changed words with positions, structure."
  ARGS:
    old : String; "Earlier version."
    new : String; "Later version."
]

def runSynctex (p : Parsed) : IO UInt32 := jsonCommand do
  let column := (p.flag? "column").map (·.as! Nat) |>.getD 0
  let boxes ← Native.synctexForward (fileArg p).toString (p.positionalArg! "source" |>.as! String)
    (p.positionalArg! "line" |>.as! Nat).toUInt32 column.toUInt32
  return .mkObj [("boxes", toJson boxes)]
def synctexCmd : Cmd := `[Cli|
  synctex VIA runSynctex; "Where a source line appears in the PDF (SyncTeX forward search)."
  FLAGS:
    c, column : Nat; "Source column (default 0: the whole line)."
  ARGS:
    file : String; "PDF next to its .synctex.gz."
    source : String; "TeX source file."
    line : Nat; "One-based source line."
]
def runSynctexBack (p : Parsed) : IO UInt32 := jsonCommand do
  let x := (p.positionalArg! "x" |>.as! String).toNat?.getD 0
  let y := (p.positionalArg! "y" |>.as! String).toNat?.getD 0
  let sources ← Native.synctexBackward (fileArg p).toString (p.positionalArg! "page" |>.as! Nat).toUInt32
    x.toFloat y.toFloat
  return .mkObj [("sources", toJson sources)]
def synctexBackCmd : Cmd := `[Cli|
  synctexback VIA runSynctexBack; "Which source produced a point on a page (SyncTeX backward search)."
  ARGS:
    file : String; "PDF next to its .synctex.gz."
    page : Nat; "One-based page."
    x : String; "Points from the left edge."
    y : String; "Points from the top edge."
]

def runQdf (p : Parsed) : IO UInt32 := jsonCommand do
  let bytes ← IO.FS.readBinFile (fileArg p)
  let hash ← Native.sha256 bytes
  let root := System.FilePath.mk ((← IO.getEnv "XDG_CACHE_HOME").getD (((← IO.getEnv "HOME").getD ".") ++ "/.cache"))
  let dir := root / "vellumory" / "qdf"
  let path := dir / (hash ++ ".qdf.pdf")
  unless ← path.pathExists do
    IO.FS.createDirAll dir
    let temporary := dir / s!"{hash}.{← IO.rand 0 0x7fffffff}.pending"
    Edit.runQpdf #[(fileArg p).toString, temporary.toString, "--qdf", "--object-streams=disable",
      "--decode-level=generalized", "--static-id"]
    IO.FS.rename temporary path
  return .mkObj [("path", .str path.toString), ("hash", .str hash)]
def qdfCmd : Cmd := `[Cli|
  qdf VIA runQdf; "Write an editable QDF rendering (normalized, uncompressed) to the cache; print its path."
  ARGS:
    file : String; "Document."
]

def runValidate (p : Parsed) : IO UInt32 := jsonCommand do
  let flavour := (p.flag? "flavour").map (·.as! String) |>.getD "0"
  Validate.run (fileArg p).toString flavour
def validateCmd : Cmd := `[Cli|
  validate VIA runValidate;
  "Validate PDF/A, PDF/UA or WTPDF conformance with veraPDF; print normalized JSON."
  FLAGS:
    f, flavour : String; "0 (detect from metadata), 1a 1b 2a 2b 2u 3a 3b 3u 4 4e 4f ua1 ua2 wt1r wt1a."
  ARGS:
    file : String; "Document."
]

def runForm (p : Parsed) : IO UInt32 := jsonCommand do
  let doc ← Native.openDocumentAs (← IO.FS.readBinFile (fileArg p)) (fileArg p).toString ""
  return .mkObj [("fields", ← IO.ofExcept (Json.parse (← Native.formFields doc)))]
def formCmd : Cmd := `[Cli|
  form VIA runForm; "List form fields: name, type, value, choices; fill them with edit fill-form."
  ARGS:
    file : String; "Document."
]

def runSignatures (p : Parsed) : IO UInt32 := jsonCommand do
  let doc ← Native.openDocumentAs (← IO.FS.readBinFile (fileArg p)) (fileArg p).toString ""
  return .mkObj [("signatures", ← IO.ofExcept (Json.parse (← Native.signatures doc)))]
def signaturesCmd : Cmd := `[Cli|
  signatures VIA runSignatures; "Verify digital signatures: integrity of the signed bytes, trust of the signer."
  ARGS:
    file : String; "Document."
]

def runAnnotations (p : Parsed) : IO UInt32 := jsonCommand do
  let doc ← Native.openDocumentAs (← IO.FS.readBinFile (fileArg p)) (fileArg p).toString ""
  let mut pages : Array Json := #[]
  for page in [0:(← Native.pageSizes doc).size] do
    let list ← IO.ofExcept (Json.parse (← Native.annotations doc page.toUInt32))
    for annotation in (match list with | .arr a => a | _ => #[]) do
      pages := pages.push (annotation.setObjVal! "page" (Vellumory.toJson (page + 1)))
  return .mkObj [("annotations", .arr pages)]
def annotationsCmd : Cmd := `[Cli|
  annotations VIA runAnnotations; "List annotations (not links or form fields) with their pages, as JSON."
  ARGS:
    file : String; "Document."
]

def runBatchValidate (p : Parsed) : IO UInt32 := jsonCommand do
  let flavour := (p.flag? "flavour").map (·.as! String) |>.getD "0"
  let files := p.variableArgsAs! String
  return .mkObj [("results", .arr (← Validate.runMany files flavour))]
def batchValidateCmd : Cmd := `[Cli|
  "batch-validate" VIA runBatchValidate;
  "Validate many documents in one veraPDF run; print one normalized result per file."
  FLAGS:
    f, flavour : String; "As for validate; 0 detects each file's claim."
  ARGS:
    ...files : String; "Documents."
]

def runExtract (p : Parsed) : IO UInt32 := jsonCommand do
  let output := p.positionalArg! "output" |>.as! String
  Edit.runQpdf #[(fileArg p).toString, "--pages", "--file=.", s!"--range={p.positionalArg! "pages" |>.as! String}", "--",
    output, "--deterministic-id", "--newline-before-endstream"]
  return .mkObj [("output", .str output)]
def extractCmd : Cmd := `[Cli|
  extract VIA runExtract; "Write selected pages to a new document, keeping outlines and tags."
  ARGS:
    file : String; "Source document (unchanged)."
    pages : String; "qpdf page range, e.g. 1-3,7,r1 or 1-z:odd."
    output : String; "New document."
]

def runSplit (p : Parsed) : IO UInt32 := jsonCommand do
  let every := p.positionalArg! "every" |>.as! Nat
  let output := p.positionalArg! "pattern" |>.as! String
  Edit.runQpdf #[s!"--split-pages={max 1 every}", (fileArg p).toString, output,
    "--deterministic-id", "--newline-before-endstream"]
  return .mkObj [("pattern", .str output)]
def splitCmd : Cmd := `[Cli|
  split VIA runSplit; "Split into documents of N pages; %d in the pattern becomes the page range."
  ARGS:
    file : String; "Source document (unchanged)."
    every : Nat; "Pages per output document."
    pattern : String; "Output name, e.g. out/part-%d.pdf."
]

def runMerge (p : Parsed) : IO UInt32 := jsonCommand do
  let output := p.positionalArg! "output" |>.as! String
  let inputs := p.variableArgsAs! String
  if inputs.isEmpty then throw <| IO.userError "no input documents"
  -- --file=: a name such as 2-3 given positionally would be a page range.
  Edit.runQpdf (#[inputs[0]!, "--pages", "--file=."] ++ (inputs.extract 1 inputs.size).map ("--file=" ++ ·) ++ #["--", output, "--deterministic-id", "--newline-before-endstream"])
  return .mkObj [("output", .str output), ("inputs", toJson inputs)]
def mergeCmd : Cmd := `[Cli|
  merge VIA runMerge; "Concatenate documents; document-level data comes from the first."
  ARGS:
    output : String; "New document."
    ...inputs : String; "Documents to concatenate, in order."
]

/-- Compose a virtual document (pdf-tools' .vpdf): parts read as JSON from
    standard input, [{"file", "first", "last", "region"}] (pages one-based, last 0
    the last page, region left top right bottom as fractions), into the cache under
    a key of the parts and every source's content, so an unchanged composition is
    reused and a changed source composes anew. Prints the path and each part's
    place in it. -/
def runCompose (_ : Parsed) : IO UInt32 := jsonCommand do
  let parts ← match ← IO.ofExcept (Json.parse (← (← IO.getStdin).readToEnd)) with
    | .arr parts => pure parts
    | _ => throw <| IO.userError "the parts are a JSON array"
  if parts.isEmpty then throw <| IO.userError "no pages to compose"
  let mut files : Array String := #[]
  let mut numbers : Array Float := #[]
  let mut key := "vellumory-virtual-1\n"
  let mut known : Std.HashMap String (String × Nat) := {}
  let mut placed : Array Json := #[]
  let mut start := 0
  for part in parts do
    let file := (← IO.FS.realPath (← IO.ofExcept (part.getObjValAs? String "file"))).toString
    unless known.contains file do
      let bytes ← IO.FS.readBinFile file
      known := known.insert file (← Native.sha256 bytes, (← Native.pageSizes (← Native.openDocument bytes "")).size)
    let (digest, count) := known.getD file ("", 0)
    let first := (part.getObjValAs? Nat "first").toOption.getD 1
    let last := match (part.getObjValAs? Nat "last").toOption.getD 0 with | 0 => count | n => n
    unless 1 ≤ first && first ≤ last && last ≤ count do
      throw <| IO.userError s!"pages {first}–{last} are not within the {count} of {file}"
    let region := (part.getObjValAs? (Array Float) "region").toOption.getD #[-1, 0, 0, 0]
    unless region.size == 4 do throw <| IO.userError "a region is four fractions: left top right bottom"
    files := files.push file
    numbers := numbers ++ #[first.toFloat, last.toFloat] ++ region
    key := key ++ s!"{digest} {first} {last} {region}\n"
    placed := placed.push (.mkObj [("file", .str file), ("first", toJson first), ("last", toJson last),
      ("start", toJson start)])
    start := start + (last - first + 1)
  let dir := (← Worker.xdgDirectory "XDG_CACHE_HOME" ".cache") / "vellumory" / "virtual"
  Worker.privateDirectory dir
  let out := dir / s!"{← Native.sha256 key.toUTF8}.pdf"
  let cached ← out.pathExists
  unless cached do
    let _ ← Native.compose files ⟨numbers⟩ out.toString
  return .mkObj [("path", .str out.toString), ("pages", toJson start), ("cached", .bool cached),
    ("parts", .arr placed)]
def composeCmd : Cmd := `[Cli|
  compose VIA runCompose; "Compose pages of several documents into one, cached (pdf-tools' virtual PDFs)."
]

def rootCmd : Cmd := `[Cli|
  vellumory NOOP; ["0.1.1"]
  "Lean PDF tooling. Native shell DSL, generated CLI, explicit proof boundaries."
  SUBCOMMANDS:
    checkCmd;
    clipCmd;
    serveCmd;
    editCmd;
    undoCmd;
    redoCmd;
    historyCmd;
    checkoutCmd;
    snapshotCmd;
    textCmd;
    infoCmd;
    preflightCmd;
    validateCmd;
    batchValidateCmd;
    annotationsCmd;
    signaturesCmd;
    formCmd;
    qdfCmd;
    diffCmd;
    synctexCmd;
    synctexBackCmd;
    extractCmd;
    splitCmd;
    mergeCmd;
    composeCmd
]

def main (args : List String) : IO UInt32 := do
  try
    return ← rootCmd.validate (if args.isEmpty then ["--help"] else args)
  catch e =>
    IO.eprintln s!"vellumory: {e}"
    return (1 : UInt32)
