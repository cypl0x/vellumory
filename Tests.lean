import Vellumory.Pdf
import Vellumory.Geometry
import Vellumory.Native
import Vellumory.Worker
import Vellumory.Fixtures
import Vellumory.Scheduler
import Vellumory.Edit
import Vellumory.Validate
import Vellumory.DocDiff
import Vellumory.Metadata
import Vellumory.Preflight

open Vellumory

def ensure (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw <| IO.userError message

/-- Like `IO.FS.withTempDir`, but removing the directory never replaces the
    body's own error: when a test fails with a worker still running, the worker
    deletes its strips while the directory is removed (macOS CI saw that ENOENT
    instead of the failure). One retry after a pause, then the directory stays. -/
def withScratchDir (body : System.FilePath → IO α) : IO α := do
  let dir ← IO.FS.createTempDir
  try body dir
  finally
    try IO.FS.removeDirAll dir
    catch _ =>
      IO.sleep 300
      try IO.FS.removeDirAll dir catch _ => pure ()

/-- Pixel bytes of a binary PPM (after the third header newline). -/
def ppmPixels (ppm : ByteArray) : ByteArray := Id.run do
  let mut lines := 0
  let mut i := 0
  while lines < 3 && i < ppm.size do
    if ppm[i]! == 10 then lines := lines + 1
    i := i + 1
  return ppm.extract i ppm.size

def testJson : IO Unit := do
  let parse (s : String) := (Json.parse s).toOption
  ensure (parse "{\"a\":[1,2.5,-3e2,true,false,null,\"x\"]}" == some (Json.mkObj [("a", .arr #[.num 1,
    .num 2.5, .num (-300), .bool true, .bool false, .null, .str "x"])])) "basic document"
  ensure (parse " \n\t[ ]\r " == some (.arr #[])) "whitespace and empty array"
  ensure (parse "\"\\u00e9\\ud83d\\ude00\\n\\\"\\\\\\/\"" == some (.str "é😀\n\"\\/"))
    "escapes and surrogate pairs"
  for bad in ["", "[1,]", "{\"a\" 1}", "01x", "1.", "1e", "-", "\"\\ud800\"", "\"\\udc00\"", "[1] 2",
      "nul", "\"a\nb\"", "{\"a\":1,}", String.join (List.replicate 600 "["), "\"\\x\""] do
    ensure (parse bad).isNone s!"accepted invalid JSON: {bad.take 20}"
  ensure ((parse "{\"k\":1,\"k\":2}").bind (fun j => (j.getObjValAs? Nat "k").toOption) == some 2)
    "duplicate keys: last wins"
  ensure ((Json.str "a\"b\\c\n\x01λ").compress == "\"a\\\"b\\\\c\\n\\u0001λ\"") "string escaping"
  ensure ((Json.num (0.0 / 0.0)).compress == "null") "non-finite numbers"
  ensure ((toJson (9007199254740991 : Nat)).compress == "9007199254740991") "exact 2^53-1"
  ensure ((toJson (595.275574 : Float)).compress == "595.275574") "fractional precision"
  ensure ((fromJson? (.num 2.5) : Except String Nat).toOption.isNone) "fractional is not Nat"
  ensure ((fromJson? (.num (-1)) : Except String Nat).toOption.isNone) "negative is not Nat"
  ensure (((Json.mkObj [("a", .null)]).getObjValAs? (Option Nat) "b").toOption == some none)
    "missing optional field"
  -- Round trips: printing then parsing yields the same value.
  let samples : List Json := [.null, .bool true, .num 0, .num (-17), .num 123456789,
    .str "", .str "tab\tquote\"λ😀", .arr #[.arr #[], .mkObj []],
    .mkObj [("x", .num 1.5), ("nested", .mkObj [("y", .arr #[.str "z", .null])])]]
  for sample in samples do
    ensure (parse sample.compress == some sample) s!"round trip: {sample.compress}"
    ensure (!sample.compress.contains '\n') "compact rendering contains a newline"
  IO.println "PASS JSON grammar, escapes, numbers, limits and round trips"

def testEdit : IO Unit := do
  withScratchDir fun dir => do
    let store : Edit.Store := { root := dir / "store" }
    let file := dir / "doc.pdf"
    let original := Fixtures.reader.toUTF8
    IO.FS.writeBinFile file original
    Native.setFileMode file.toString 0o640
    let pages (path : System.FilePath) : IO Nat := do
      return (← Native.pageSizes (← Native.openDocument (← IO.FS.readBinFile path) "")).size
    let headOf (j : Json) := (j.getObjValAs? Nat "head").toOption.getD 999
    let rotated ← Edit.apply store file (.rotate #[1] 90)
    ensure (headOf rotated == 1 && (← pages file) == 3) "rotate recorded"
    let sizes ← Native.pageSizes (← Native.openDocument (← IO.FS.readBinFile file) "")
    ensure (sizes[0]! == (792.0, 612.0)) "page 1 rotated to landscape"
    let selected ← Edit.apply store file (.select #[3, 1])
    ensure (headOf selected == 2 && (← pages file) == 2) "select recorded"
    ensure ((← Native.fileMode file.toString) == 0o640) "atomic replacement keeps permissions"
    -- Undo and redo move between exact stored states.
    let edited ← IO.FS.readBinFile file
    let _ ← Edit.undo store file
    ensure ((← pages file) == 3) "undo restores three pages"
    let _ ← Edit.redo store file
    ensure ((← IO.FS.readBinFile file) == edited) "redo restores identical bytes"
    -- Invalid edits leave everything untouched.
    let rejected ← try let _ ← Edit.apply store file (.select #[9]); pure false catch _ => pure true
    ensure (rejected && (← IO.FS.readBinFile file) == edited) "invalid edit rejected without change"
    let rejected ← try let _ ← Edit.apply store file (.rotate #[1] 45); pure false catch _ => pure true
    ensure rejected "non-right-angle rotation rejected"
    -- An external change becomes its own state before anything else happens.
    IO.FS.writeBinFile file (Fixtures.reader false).toUTF8
    let h ← Edit.history store file
    let nodes : Array Json := (h.getObjValAs? (Array Json) "nodes").toOption.getD #[]
    ensure (nodes.size == 4 && ((nodes[3]!.getObjVal? "operation").toOption.bind
      (fun o => (o.getObjValAs? String "op").toOption)) == some "external") "external change recorded"
    let _ ← Edit.undo store file
    ensure ((← IO.FS.readBinFile file) == edited) "undo of an external change"
    -- Branching: a new edit after undo keeps the other branch reachable.
    let _ ← Edit.apply store file (.select #[2])
    let _ ← Edit.checkout store file 0
    ensure ((← IO.FS.readBinFile file) == original) "checkout of the first state is byte-identical"
    let _ ← Edit.checkout store file 3
    ensure ((← IO.FS.readBinFile file) == (Fixtures.reader false).toUTF8) "checkout of the external branch"
    -- Applying an edit twice to identical input is deterministic: same object.
    let _ ← Edit.checkout store file 0
    let again ← Edit.apply store file (.rotate #[1] 90)
    ensure ((again.getObjVal? "node" >>= (·.getObjValAs? String "hash")).toOption ==
      (rotated.getObjVal? "node" >>= (·.getObjValAs? String "hash")).toOption) "deterministic edits"
    -- Lossless compression keeps every rendered pixel.
    let _ ← Edit.checkout store file 0
    let before ← Native.openDocument (← IO.FS.readBinFile file) ""
    let _ ← Edit.apply store file .compress
    let after ← Native.openDocument (← IO.FS.readBinFile file) ""
    for page in [0:3] do
      ensure ((← Native.render before page.toUInt32 300 0 100) == (← Native.render after page.toUInt32 300 0 100))
        s!"compression changed page {page + 1}"
    -- Tampered snapshots are detected on restore, not silently written.
    let some hist := ← Edit.loadHistory store (← IO.FS.realPath file) | throw (IO.userError "no history")
    let victim := store.object hist.nodes[0]!.hash
    IO.FS.writeBinFile victim "tampered".toUTF8
    let caught ← try let _ ← Edit.checkout store file 0; pure false catch _ => pure true
    ensure caught "corrupt snapshot accepted"
    -- Malformed histories are rejected rather than trusted.
    let cyclic := "{\"path\":\"x\",\"head\":0,\"nodes\":[{\"hash\":\"a\",\"parent\":0,\"time\":0,\"label\":\"\"}]}"
    ensure ((Json.parse cyclic >>= History.fromJson).toOption.isNone) "cyclic history accepted"
  IO.println "PASS reversible edits, branching history, external changes, determinism, lossless compression, integrity"

/-- A one-page PDF whose only content is a grayscale image of `pixels`: no text layer. -/
def scannedPdf (width height : Nat) (gray : ByteArray) : String := Id.run do
  let hexDigit (n : Nat) : Char := "0123456789abcdef".toList[n]!
  let mut hex := ""
  for b in gray do
    hex := (hex.push (hexDigit (b.toNat / 16))).push (hexDigit (b.toNat % 16))
  let data := hex ++ ">"
  return Fixtures.document #[
    "<< /Type /Catalog /Pages 2 0 R >>",
    "<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /XObject << /Im0 4 0 R >> >> /Contents 5 0 R >>",
    s!"<< /Type /XObject /Subtype /Image /Width {width} /Height {height} /ColorSpace /DeviceGray /BitsPerComponent 8 /Filter /ASCIIHexDecode /Length {data.utf8ByteSize} >>\nstream\n{data}\nendstream",
    Fixtures.stream "q 612 0 0 792 0 0 cm /Im0 Do Q\n"]

def testValidateAndOcr : IO Unit := do
  ensure (Validate.contextPage "root/document[0]/pages[12](3 0 obj PDPage)/x" == some 12) "context page"
  ensure (Validate.contextPage "root/document[0]/metadata" == none) "context without page"
  withScratchDir fun dir => do
    let file := dir / "doc.pdf"
    IO.FS.writeFile file Fixtures.reader
    let report ← Validate.run file.toString "2b"
    ensure ((report.getObjValAs? Bool "compliant").toOption == some false) "fixture is not PDF/A-2b"
    let rules := (report.getObjValAs? (Array Json) "rules").toOption.getD #[]
    let clauses := rules.filterMap fun r => (r.getObjValAs? String "clause").toOption
    -- The fixture has no trailer ID and does not embed Helvetica.
    ensure (clauses.contains "6.1.3" && clauses.contains "6.2.11.4.1") s!"expected clauses, got {clauses}"
    ensure (rules.any fun r => ((r.getObjValAs? (Array Json) "checks").toOption.getD #[]).any fun c =>
      (c.getObjValAs? Nat "page").toOption == some 0) "checks carry page indices"
    let rejected ← try let _ ← Validate.run file.toString "9z"; pure false catch _ => pure true
    ensure rejected "unknown flavour accepted"
    -- One veraPDF run for many files; one that cannot be opened still gets a row.
    let other := dir / "other.pdf"
    IO.FS.writeFile other Fixtures.reader
    let missing := (dir / "missing.pdf").toString
    let many ← Validate.runMany #[file.toString, other.toString, missing] "2b"
    ensure (many.size == 3) s!"one result per file: {many.size}"
    let fileOf := fun (r : Json) => (r.getObjValAs? String "file").toOption.getD ""
    ensure (many.any fun r => fileOf r == missing && (r.getObjValAs? String "error").isOk) "missing file reported"
    ensure ((many.filter fun r => (r.getObjValAs? Bool "compliant").toOption == some false).size == 2)
      "both fixtures judged in one run"
    -- OCR: an image-only page gains a text layer, as an undoable edit.
    let doc ← Native.openDocument Fixtures.reader.toUTF8 ""
    let (w, h) := (1020, 1320)
    let rgb := ppmPixels (← Native.render doc 0 w.toUInt32 0 h.toUInt32)
    let mut gray := ByteArray.empty
    for i in [0:w * h] do
      gray := gray.push ((rgb[3*i]!.toNat + rgb[3*i+1]!.toNat + rgb[3*i+2]!.toNat) / 3).toUInt8
    let scan := dir / "scan.pdf"
    IO.FS.writeFile scan (scannedPdf w h gray)
    let before ← Native.pageText (← Native.openDocument (← IO.FS.readBinFile scan) "") 0
    ensure (!(before.splitOn "Continuous").length > 1) "the scan must start without text"
    let store : Edit.Store := { root := dir / "store" }
    let _ ← Edit.apply store scan (.ocr "eng" "skip")
    let after ← Native.pageText (← Native.openDocument (← IO.FS.readBinFile scan) "") 0
    ensure ((after.splitOn "Continuous reading").length > 1 && (after.splitOn "raster strip").length > 1)
      s!"OCR text missing: {after.take 200}"
    let _ ← Edit.undo store scan
    ensure ((← IO.FS.readFile scan) == scannedPdf w h gray) "undo restores the scan byte for byte"
  IO.println "PASS veraPDF validation report, clauses, pages, batches; OCR text layer as an undoable edit"

def testMetadata : IO Unit := do
  -- PDF dates ↔ XMP dates.
  let iso := Metadata.isoDate?
  ensure (iso "D:20261028054000+01'00'" == some "2026-10-28T05:40:00+01:00") "offset date"
  ensure (iso "D:20261028054000-05'30" == some "2026-10-28T05:40:00-05:30") "offset without closing quote"
  ensure (iso "D:19800101000000+00'00'" == some "1980-01-01T00:00:00Z") "zero offset is Z"
  ensure (iso "D:20261028054000Z" == some "2026-10-28T05:40:00Z") "Z"
  ensure (iso "D:202610280540" == some "2026-10-28T05:40") "no seconds"
  ensure (iso "D:2026" == some "2026-01-01" && iso "D:202610" == some "2026-10-01") "defaults"
  ensure (iso "junk" == none && iso "D:20x6" == none) "invalid PDF dates"
  let pdf := Metadata.pdfDate?
  ensure (pdf "2026-10-28T05:40:00+01:00" == some "D:20261028054000+01'00'") "ISO to PDF"
  ensure (pdf "2026-10-28T05:40Z" == some "D:20261028054000Z") "ISO without seconds"
  ensure (pdf "2026-10-28T05:40:00.125-03:00" == some "D:20261028054000-03'00'") "fractions dropped"
  ensure (pdf "2026-10-28" == some "D:20261028") "date only"
  ensure (pdf "2026-10-28T05" == none && pdf "28.10.2026" == none) "invalid ISO dates"
  for d in ["2026-10-28T05:40:00+01:00", "1999-12-31T23:59:59-08:00", "2026-01-02T03:04:05Z"] do
    ensure ((pdf d).bind iso == some d) s!"round trip {d}"
  ensure (Metadata.languageTag? "en-GB" && Metadata.languageTag? "zh-Hant-TW" && Metadata.languageTag? "de")
    "language tags"
  ensure (!Metadata.languageTag? "" && !Metadata.languageTag? "en_GB" && !Metadata.languageTag? "1en")
    "invalid language tags"
  let rejects := fun (set : Array (String × String)) (remove : Array String) (date : String) =>
    match Metadata.validate set remove date with | .error _ => true | .ok _ => false
  ensure (rejects #[] #[] "" && rejects #[("colour", "red")] #[] "" && rejects #[("title", "")] #[] "" &&
    rejects #[("language", "en GB")] #[] "" && rejects #[("display-title", "yes")] #[] "" &&
    rejects #[("title", "a")] #["title"] "" && rejects #[("title", "a")] #[] "yesterday") "validation"
  withScratchDir fun dir => do
    -- A document without Info dictionary or XMP packet.
    let doc := dir / "doc.pdf"
    IO.FS.writeFile doc Fixtures.reader
    let store : Edit.Store := { root := dir / "store" }
    let date := "2026-10-28T05:40:00+01:00"
    let edit : Edit.Operation := .metadata #[("title", "Größe — 東京"), ("author", "A. Person"),
      ("language", "de-DE"), ("display-title", "true"), ("keywords", "pdf, xmp")] #[] date
    let first ← Edit.apply store doc edit
    let infoOf := fun (file : System.FilePath) => do
      IO.ofExcept (Json.parse (← Native.info (← Native.openDocument (← IO.FS.readBinFile file) "")))
    let info ← infoOf doc
    let field := fun (j : Json) (k : String) => (j.getObjValAs? String k).toOption
    ensure (field info "title" == some "Größe — 東京" && field info "author" == some "A. Person" &&
      field info "keywords" == some "pdf, xmp" && field info "language" == some "de-DE" &&
      (info.getObjValAs? Bool "display_title").toOption == some true &&
      field info "modified" == some "D:20261028054000+01'00'") s!"Info and catalog: {info.compress.take 400}"
    let packet ← Metadata.packet doc.toString
    let xmp := fun (ns name : String) => Native.xmpProperty packet ns name
    ensure ((← xmp "http://ns.adobe.com/pdf/1.3/" "Keywords") == some "pdf, xmp" &&
      (← xmp "http://ns.adobe.com/xap/1.0/" "ModifyDate") == some date &&
      (← xmp "http://ns.adobe.com/xap/1.0/" "MetadataDate") == some date) "XMP mirrors Info"
    let text := String.fromUTF8! packet
    ensure ((text.splitOn "Größe — 東京").length == 2 && (text.splitOn "<rdf:li>de-DE</rdf:li>").length == 2 &&
      (text.splitOn "<rdf:li>A. Person</rdf:li>").length == 2) "dc:title, dc:language, dc:creator"
    -- Removal, then undo.
    let _ ← Edit.apply store doc (.metadata #[] #["keywords"] "")
    let info ← infoOf doc
    ensure (field info "keywords" == none && field info "title" == some "Größe — 東京") "keywords removed"
    ensure ((← Native.xmpProperty (← Metadata.packet doc.toString) "http://ns.adobe.com/pdf/1.3/" "Keywords")
      == none) "pdf:Keywords removed"
    let _ ← Edit.undo store doc
    ensure (field (← infoOf doc) "keywords" == some "pdf, xmp") "undo restores keywords"
    -- The same edit of the same bytes is the same object.
    let again := dir / "again.pdf"
    IO.FS.writeFile again Fixtures.reader
    let second ← Edit.apply { root := dir / "store2" } again edit
    let hashOf := fun (j : Json) => ((j.getObjValD "node").getObjValAs? String "hash").toOption
    ensure (hashOf first == hashOf second && (hashOf first).isSome) "deterministic metadata edit"
    -- PDF/UA-1: the title, language and title display failures are gone.
    let failing := fun (report : Json) =>
      ((report.getObjValAs? (Array Json) "rules").toOption.getD #[]).filterMap fun r => do
        return s!"{← (r.getObjValAs? String "clause").toOption}-{← (r.getObjValAs? Nat "test_number").toOption}"
    let original := dir / "original.pdf"
    IO.FS.writeFile original Fixtures.reader
    let before := failing (← Validate.run original.toString "ua1")
    let after := failing (← Validate.run doc.toString "ua1")
    for rule in ["7.1-8", "7.1-10", "7.2-34"] do
      ensure (before.contains rule && !after.contains rule) s!"PDF/UA rule {rule}: {before} → {after}"
    -- PDF/A: every edit keeps a conforming document conforming.
    for (part, flavour, ops) in [(2, "2b", #[edit, .rotate #[1] 90, .compress, .select #[1, 3], .setLabels #["1:r/1"]]),
        (1, "1b", #[edit, .compress, .rotate #[2] (-90)])] do
      let archival := dir / s!"pdfa-{part}.pdf"
      let made ← IO.Process.output {
        cmd := Tools.ocrmypdf
        args := #["--skip-text", "--output-type", s!"pdfa-{part}", "--quiet", original.toString, archival.toString] }
      ensure (made.exitCode == 0) s!"ocrmypdf PDF/A-{part}: {made.stderr}"
      let compliant := fun (file : System.FilePath) => do
        return ((← Validate.run file.toString flavour).getObjValAs? Bool "compliant").toOption == some true
      ensure (← compliant archival) s!"PDF/A-{flavour} fixture"
      for op in ops do
        let copy := dir / s!"pdfa-{part}-edit.pdf"
        IO.FS.writeBinFile copy (← IO.FS.readBinFile archival)
        let _ ← Edit.apply { root := dir / s!"store-a{part}" } copy op
        ensure (← compliant copy) s!"PDF/A-{flavour} after {op.label}"
        ensure ((← Metadata.pdfaPart copy.toString) == some part) s!"PDF/A-{part} claim kept by {op.label}"
    -- PDF 2.0 (PDF/A-4f, PDF/UA-2, tagged): metadata lives in XMP only; no Info
    -- dictionary may appear (ISO 19005-4 6.1.3).
    let some fixture ← IO.getEnv "VELLUMORY_PDFA4_FIXTURE"
      | throw <| IO.userError "VELLUMORY_PDFA4_FIXTURE is not set (nix develop provides it)"
    let modern := dir / "pdfa4.pdf"
    IO.FS.writeBinFile modern (← IO.FS.readBinFile s!"{fixture}/doc.pdf")
    let _ ← Edit.apply { root := dir / "store-a4" } modern
      (.metadata #[("title", "Renamed"), ("subject", "Testing")] #[] date)
    for flavour in ["4f", "ua2"] do
      ensure (((← Validate.run modern.toString flavour).getObjValAs? Bool "compliant").toOption == some true)
        s!"{flavour} after a metadata edit of a PDF 2.0 document"
    let info ← infoOf modern
    let fromXmp := ((info.getObjValAs? (Array String) "from_xmp").toOption.getD #[])
    ensure (field info "title" == some "Renamed" && field info "subject" == some "Testing" &&
      field info "author" == some "A. Person" && fromXmp.contains "title" && fromXmp.contains "subject")
      s!"PDF 2.0 metadata from XMP: {info.compress.take 300}"
    ensure ((← Metadata.pdfaPart modern.toString) == some 4) "PDF/A-4 claim kept"
  IO.println "PASS metadata: dates, Info/XMP/catalog consistency, removal, undo, determinism; PDF/UA rules fixed; PDF/A-1b, -2b, -4f kept; PDF 2.0 in XMP only"

def testAssociatedFiles : IO Unit := do
  withScratchDir fun dir => do
    let some fixture ← IO.getEnv "VELLUMORY_PDFA4_FIXTURE"
      | throw <| IO.userError "VELLUMORY_PDFA4_FIXTURE is not set (nix develop provides it)"
    let store : Edit.Store := { root := dir / "store" }
    let doc := dir / "a4.pdf"
    let source := dir / "doc.org"
    IO.FS.writeBinFile doc (← IO.FS.readBinFile s!"{fixture}/doc.pdf")
    let original ← IO.FS.readBinFile doc
    let attachment := fun (key : String) => do
      let info ← IO.ofExcept (Json.parse (← Native.info (← Native.openDocument (← IO.FS.readBinFile doc) "")))
      return ((info.getObjValAs? (Array Json) "attachments").toOption.getD #[]).find?
        (fun a => (a.getObjValAs? String "key").toOption == some key)
    let text := fun (j : Json) (k : String) => (j.getObjValAs? String k).toOption.getD ""
    -- Every embedded file stream in the file, reachable from anywhere: nothing
    -- replaced or removed may stay behind.
    let embedded := fun (_ : Unit) => do
      let out ← IO.Process.output {
        cmd := Tools.qpdf
        args := #["--json-output", "--json-key=qpdf", "--json-stream-data=none", doc.toString, "-"] }
      let objects := match (← IO.ofExcept (Json.parse out.stdout)).getObjValD "qpdf" with
        | .arr #[_, .obj objects] => objects
        | _ => #[]
      return (objects.filter fun (_, o) =>
        (((o.getObjValD "stream").getObjValD "dict").getObjValD "/Type") == .str "/EmbeddedFile").size
    let compliant := fun (flavour : String) => do
      return ((← Validate.run doc.toString flavour).getObjValAs? Bool "compliant").toOption == some true
    let before ← embedded ()
    IO.FS.writeFile source "* First version\n"
    let _ ← Edit.apply store doc (.attach source.toString "doc.org" "Org source" "Source")
    let some a ← attachment "doc.org" | throw (IO.userError "attachment missing")
    ensure (text a "relationship" == "Source" && text a "mimetype" == "text/x-org" &&
      (a.getObjValAs? Bool "associated").toOption == some true) s!"associated source: {a.compress}"
    ensure ((← embedded ()) == before + 1) "one embedded file added"
    -- Replaced (as an export does again): the first version leaves the file.
    IO.FS.writeFile source "* Second version\n"
    let _ ← Edit.apply store doc (.attach source.toString "doc.org" "Org source" "Source")
    ensure ((← embedded ()) == before + 1) "a replaced associated file stays inside the PDF"
    -- PDF/A-4f requires a relationship: attaching without one names it Unspecified.
    let _ ← Edit.apply store doc (.attach source.toString "notes.org" "" "")
    let some n ← attachment "notes.org" | throw (IO.userError "second attachment missing")
    ensure (text n "relationship" == "Unspecified") s!"PDF/A-4f default relationship: {n.compress}"
    for flavour in ["4f", "ua2"] do
      ensure (← compliant flavour) s!"{flavour} with associated files"
    -- Removed: gone from the file, with no null left in /AF.
    let _ ← Edit.apply store doc (.detach "doc.org")
    let _ ← Edit.apply store doc (.detach "notes.org")
    ensure ((← embedded ()) == before && (← attachment "doc.org").isNone) "removed associated files stay inside the PDF"
    for flavour in ["4f", "ua2"] do
      ensure (← compliant flavour) s!"{flavour} after removing associated files"
    for _ in [0:5] do let _ ← Edit.undo store doc
    ensure ((← IO.FS.readBinFile doc) == original) "undo restores the original bytes"
    let rejected := fun (op : Edit.Operation) => match op.validate 3 with | .error _ => true | .ok _ => false
    ensure (rejected (.attach source.toString "doc.org" "" "Origin")) "unknown relationship accepted"
    -- PDF/A-1 allows no attachments, PDF/A-2 only PDF/A ones: refused, not broken.
    let plain := dir / "plain.pdf"
    IO.FS.writeFile plain Fixtures.reader
    for part in [1, 2] do
      let archival := dir / s!"pdfa-{part}.pdf"
      let made ← IO.Process.output {
        cmd := Tools.ocrmypdf
        args := #["--skip-text", "--output-type", s!"pdfa-{part}", "--quiet", plain.toString, archival.toString] }
      ensure (made.exitCode == 0) s!"ocrmypdf PDF/A-{part}: {made.stderr}"
      let kept ← IO.FS.readBinFile archival
      let refused ← try
          let _ ← Edit.apply { root := dir / s!"store-a{part}" } archival (.attach source.toString "doc.org" "" "Source")
          pure false
        catch _ => pure true
      ensure (refused && (← IO.FS.readBinFile archival) == kept) s!"Org source attached to PDF/A-{part}"
    -- A PDF/A-2 attachment may go into a PDF/A-2 document.
    let _ ← Edit.apply { root := dir / "store-a2" } (dir / "pdfa-2.pdf")
      (.attach (dir / "pdfa-1.pdf").toString "earlier.pdf" "" "")
    ensure (((← Validate.run (dir / "pdfa-2.pdf").toString "2b").getObjValAs? Bool "compliant").toOption == some true)
      "PDF/A-2b with a PDF/A-1 attachment"
  IO.println "PASS associated files: relationship, MIME type, /AF; replaced and removed files leave nothing behind; PDF/A-4f and PDF/UA-2 kept; PDF/A-1 and -2 refuse non-PDF/A attachments"

def testStampAndEncryption : IO Unit := do
  withScratchDir fun dir => do
    let store : Edit.Store := { root := dir / "store" }
    let doc := dir / "doc.pdf"
    let stamp := dir / "stamp.pdf"
    IO.FS.writeFile doc Fixtures.reader
    IO.FS.writeFile stamp Fixtures.reader
    let textOf := fun (page : Nat) (password : String) => do
      Native.pageText (← Native.openDocument (← IO.FS.readBinFile doc) password) page.toUInt32
    -- The stamp's first page is drawn on pages 2 and 3, over and then under.
    let _ ← Edit.apply store doc (.stamp stamp.toString false "2-z")
    ensure (((← textOf 1 "").splitOn "Vellumory / 1").length == 2 && ((← textOf 0 "").splitOn "Vellumory / 1").length == 2)
      "stamped onto pages 2-z only"
    let _ ← Edit.undo store doc
    let _ ← Edit.apply store doc (.stamp stamp.toString true "1")
    ensure (((← textOf 0 "").splitOn "Vellumory / 1").length == 3) "underlay on page 1"
    let _ ← Edit.undo store doc
    ensure ((← IO.FS.readFile doc) == Fixtures.reader) "undo restores the unstamped document"
    let rejected := fun (op : Edit.Operation) => match op.validate 3 with | .error _ => true | .ok _ => false
    ensure (rejected (.stamp stamp.toString false "all") && rejected (.stamp "" false "1-z")) "stamp validation"
    -- AES-256: the result opens only with the password; no password is recorded.
    let user := "s3cret-user"
    let owner := "s3cret-owner"
    let _ ← Edit.apply store doc (.encrypt user owner "low" "none" false)
    let locked ← (Native.openDocument (← IO.FS.readBinFile doc) "").toBaseIO
    ensure (match locked with | .error _ => true | .ok _ => false) "encrypted: no password, no document"
    ensure (((← textOf 0 user).splitOn "Continuous").length > 1) "the password to open opens it"
    let mut recorded := ""
    for entry in ← (dir / "store" / "history").readDir do
      recorded := recorded ++ (← IO.FS.readFile entry.path)
    ensure (!(recorded.splitOn "s3cret").length > 1) "no password in the history"
    ensure ((← (dir / "store" / "tmp").readDir).isEmpty) "no secret files left behind"
    ensure (rejected (.encrypt "a" "a" "full" "all" true) && rejected (.encrypt "a" "" "full" "all" true) &&
      rejected (.encrypt "a" "b" "some" "all" true)) "encryption validation"
    -- Decryption with the password, then back through the history.
    let _ ← Edit.apply store doc (.decrypt user)
    ensure (((← textOf 0 "").splitOn "Continuous").length > 1) "decrypted"
    ensure ((← (dir / "store" / "tmp").readDir).isEmpty) "no password file left behind"
    let _ ← Edit.undo store doc
    let _ ← Edit.undo store doc
    ensure ((← IO.FS.readFile doc) == Fixtures.reader) "undo returns to the original bytes"
  IO.println "PASS stamps and underlays; AES-256 encryption without recorded or exposed passwords; decryption"

def testAnnotations : IO Unit := do
  withScratchDir fun dir => do
    -- Clockwise from the upper left, as selection and search quads are.
    let quads := #[42.0, 179.1, 300.0, 179.1, 300.0, 195.6, 42.0, 195.6]
    let highlight : Edit.Operation := .annotate 1 "Highlight" quads #[] #[1, 0.85, 0.2] "Line 1" "Reader" 1793166790
    let note : Edit.Operation := .annotate 2 "Text" #[] #[400, 100, 400, 100] #[] "Check this" "Reader" 1793166790
    let listOf := fun (file : System.FilePath) (page : Nat) => do
      let doc ← Native.openDocument (← IO.FS.readBinFile file) ""
      match ← IO.ofExcept (Json.parse (← Native.annotations doc page.toUInt32)) with
      | .arr a => return a
      | _ => return #[]
    -- Add, edit, delete, each recorded; the same edit of the same bytes is the same object.
    let doc := dir / "doc.pdf"
    IO.FS.writeFile doc Fixtures.reader
    let store : Edit.Store := { root := dir / "store" }
    let first ← Edit.apply store doc highlight
    let found ← listOf doc 0
    ensure (found.size == 1 && (found[0]!.getObjValAs? String "type").toOption == some "Highlight" &&
      (found[0]!.getObjValAs? String "contents").toOption == some "Line 1") s!"highlight: {(Json.arr found).compress}"
    let again := dir / "again.pdf"
    IO.FS.writeFile again Fixtures.reader
    let second ← Edit.apply { root := dir / "store2" } again highlight
    let hashOf := fun (j : Json) => ((j.getObjValD "node").getObjValAs? String "hash").toOption
    ensure (hashOf first == hashOf second && (hashOf first).isSome) "deterministic annotation"
    -- The quads come back as given, and the highlight covers exactly them.
    let back := (found[0]!.getObjValAs? (Array (Array Float)) "quads").toOption.getD #[]
    ensure (back.size == 1 && (back[0]!.zip quads).all (fun (a, b) => (a - b).abs < 0.05))
      s!"quads round trip: {(Json.arr (back.map fun q => Json.arr (q.map .num))).compress}"
    let rgbOf := fun (bytes : ByteArray) => do
      return ppmPixels (← Native.render (← Native.openDocument bytes "") 0 612 0 792)
    let rgb ← rgbOf (← IO.FS.readBinFile doc)
    let plain ← rgbOf Fixtures.reader.toUTF8
    let sample := fun (image : ByteArray) (x y : Nat) => (image[3 * (y * 612 + x)]!.toNat,
      image[3 * (y * 612 + x) + 1]!.toNat, image[3 * (y * 612 + x) + 2]!.toNat)
    let pixel := sample rgb
    -- A point inside the band where the page has no ink.
    let blank := (List.range 240).find? (fun i => let (r, g, b) := sample plain (50 + i) 186; r > 245 && g > 245 && b > 245)
    let x := 50 + blank.getD 0
    let (r, g, b) := pixel x 186
    ensure (r > 200 && g > 160 && b < 120) s!"highlight colour inside the band: {(r, g, b)}"
    let (r, g, b) := pixel 250 172
    ensure (r > 240 && g > 240 && b > 240) s!"nothing above the line: {(r, g, b)}"
    let object := (found[0]!.getObjValAs? Nat "object").toOption.getD 0
    let _ ← Edit.apply store doc (.annotationEdit 1 object "Revised" 1793170000)
    ensure (((← listOf doc 0)[0]!.getObjValAs? String "contents").toOption == some "Revised") "edited"
    let object := (((← listOf doc 0)[0]!).getObjValAs? Nat "object").toOption.getD 0
    let _ ← Edit.apply store doc (.annotationDelete 1 object)
    ensure ((← listOf doc 0).isEmpty) "deleted"
    let rejected := fun (op : Edit.Operation) => match op.validate 3 with | .error _ => true | .ok _ => false
    ensure (rejected (.annotate 4 "Highlight" quads #[] #[] "" "" 0) && rejected (.annotate 1 "Ink" quads #[] #[] "" "" 0) &&
      rejected (.annotate 1 "Highlight" #[1, 2, 3] #[] #[] "" "" 0) && rejected (.annotate 1 "Text" #[] #[] #[] "" "" 0) &&
      rejected (.annotate 1 "Underline" quads #[] #[2, 0, 0] "" "" 0)) "annotation validation"
    -- PDF/A: annotations keep conforming documents conforming; PDF/A-1 refuses highlights.
    let original := dir / "original.pdf"
    IO.FS.writeFile original Fixtures.reader
    for (part, flavour, ops) in [(2, "2b", #[highlight, note]),
        (1, "1b", #[.annotate 1 "Underline" quads #[] #[0.1, 0.45, 0.9] "Line 1" "Reader" 1793166790, note])] do
      let archival := dir / s!"pdfa-{part}.pdf"
      let made ← IO.Process.output {
        cmd := Tools.ocrmypdf
        args := #["--skip-text", "--output-type", s!"pdfa-{part}", "--quiet", original.toString, archival.toString] }
      ensure (made.exitCode == 0) s!"ocrmypdf PDF/A-{part}: {made.stderr}"
      for op in ops do
        let copy := dir / s!"pdfa-{part}-annotated.pdf"
        IO.FS.writeBinFile copy (← IO.FS.readBinFile archival)
        let _ ← Edit.apply { root := dir / s!"store-a{part}" } copy op
        ensure (((← Validate.run copy.toString flavour).getObjValAs? Bool "compliant").toOption == some true)
          s!"PDF/A-{flavour} after {op.label}"
      let copy := dir / "pdfa-1-highlight.pdf"
      if part == 1 then
        IO.FS.writeBinFile copy (← IO.FS.readBinFile archival)
        let refused ← (Edit.apply { root := dir / "store-h" } copy highlight).toBaseIO
        ensure (match refused with | .error _ => true | .ok _ => false) "PDF/A-1 refuses a highlight"
        ensure ((← IO.FS.readBinFile copy) == (← IO.FS.readBinFile archival)) "and leaves the file as it was"
  IO.println "PASS annotations: add, edit, delete, deterministic; PDF/A-1b and -2b kept, PDF/A-1 highlights refused"

def testSignatures : IO Unit := do
  let some fixture ← IO.getEnv "VELLUMORY_SIGNED_FIXTURE"
    | throw <| IO.userError "VELLUMORY_SIGNED_FIXTURE is not set (nix develop provides it)"
  let reportOf := fun (bytes : ByteArray) => do
    let doc ← Native.openDocument bytes ""
    let list ← IO.ofExcept (Json.parse (← Native.signatures doc))
    return ((← Native.signedCount doc).toNat, match list with | .arr a => a | _ => #[])
  let signedBytes ← IO.FS.readBinFile s!"{fixture}/signed.pdf"
  let (count, report) ← reportOf signedBytes
  let field := fun (j : Json) (k : String) => (j.getObjValAs? Bool k).toOption
  ensure (count == 1 && report.size == 1) "one signed field"
  let sig := report[0]!
  ensure (field sig "signed" == some true && field sig "intact" == some true && field sig "trusted" == some false &&
    field sig "covers_whole_file" == some true && field sig "changed_after_signing" == some false)
    s!"signature report: {sig.compress}"
  ensure ((sig.getObjValAs? String "trust_error").toOption == some "self-signed certificate") "why it is not trusted"
  ensure ((((sig.getObjValAs? String "signer").toOption.getD "").splitOn "Vellumory Test Signer").length == 2) "signer"
  let (count, report) ← reportOf (← IO.FS.readBinFile s!"{fixture}/unsigned.pdf")
  ensure (count == 0 && report.size == 1 && field report[0]! "signed" == some false) "an empty signature field"
  -- Trusting the test certificate (OpenSSL's default store honours SSL_CERT_FILE).
  let app := (← IO.appPath).parent.get! / "vellumory"
  let trusted ← IO.Process.output {
    cmd := app.toString, args := #["signatures", s!"{fixture}/signed.pdf"]
    env := #[("SSL_CERT_FILE", some s!"{fixture}/cert.pem")] }
  ensure ((trusted.stdout.splitOn "\"trusted\":true").length == 2) s!"trusted with its certificate: {trusted.stdout}"
  -- An edit rewrites the file, which breaks the signature; undo brings it back.
  withScratchDir fun dir => do
    let doc := dir / "signed.pdf"
    IO.FS.writeBinFile doc signedBytes
    let store : Edit.Store := { root := dir / "store" }
    let _ ← Edit.apply store doc (.rotate #[1] 90)
    let (_, after) ← reportOf (← IO.FS.readBinFile doc)
    ensure (after.size == 1 && field after[0]! "intact" == some false) s!"broken by the edit: {(Json.arr after).compress}"
    let _ ← Edit.undo store doc
    ensure ((← IO.FS.readBinFile doc) == signedBytes) "undo restores the signed bytes"
    let (_, restored) ← reportOf (← IO.FS.readBinFile doc)
    ensure (field restored[0]! "intact" == some true) "and the signature is intact again"
  IO.println "PASS signatures: integrity, trust and its reason, signer; broken by edits, restored by undo"

def testForms : IO Unit := do
  let some fixture ← IO.getEnv "VELLUMORY_FORM_FIXTURE"
    | throw <| IO.userError "VELLUMORY_FORM_FIXTURE is not set (nix develop provides it)"
  let original ← IO.FS.readBinFile s!"{fixture}/form.pdf"
  let fieldsOf := fun (bytes : ByteArray) => do
    match ← IO.ofExcept (Json.parse (← Native.formFields (← Native.openDocument bytes ""))) with
    | .arr a => return a
    | _ => return #[]
  let valueOf := fun (fields : Array Json) (name : String) =>
    (fields.find? fun f => (f.getObjValAs? String "name").toOption == some name).bind
      fun f => (f.getObjValAs? String "value").toOption
  let fields ← fieldsOf original
  ensure (fields.size == 4 && valueOf fields "agree" == some "Off" && valueOf fields "reference" == some "R-17")
    s!"form fields: {(Json.arr fields).compress}"
  withScratchDir fun dir => do
    let doc := dir / "form.pdf"
    IO.FS.writeBinFile doc original
    let store : Edit.Store := { root := dir / "store" }
    let fill : Edit.Operation := .fillForm #[("fullname", "Ada Lovelace"), ("agree", "Yes"), ("colour", "Green")]
    let first ← Edit.apply store doc fill
    let filled ← fieldsOf (← IO.FS.readBinFile doc)
    ensure (valueOf filled "fullname" == some "Ada Lovelace" && valueOf filled "agree" == some "Yes" &&
      valueOf filled "colour" == some "Green") s!"filled: {(Json.arr filled).compress}"
    -- The new value is drawn in its field (text extraction skips widgets, so
    -- count ink: the blank field has none inside its border).
    let rect := ((filled.find? fun f => (f.getObjValAs? String "name").toOption == some "fullname").bind
      fun f => (f.getObjValAs? (Array Float) "rect").toOption).getD #[]
    ensure (rect.size == 4) s!"the field's rectangle: {rect}"
    let ink := fun (bytes : ByteArray) => do
      let rgb := ppmPixels (← Native.render (← Native.openDocument bytes "") 0 612 0 792)
      let mut dark := 0
      for y in [(rect[1]! + 3).toUInt64.toNat:(rect[3]! - 3).toUInt64.toNat] do
        for x in [(rect[0]! + 3).toUInt64.toNat:(rect[2]! - 3).toUInt64.toNat] do
          let i := 3 * (y * 612 + x)
          if rgb[i]!.toNat + rgb[i + 1]!.toNat + rgb[i + 2]!.toNat < 300 then dark := dark + 1
      return dark
    let blank ← ink original
    let drawn ← ink (← IO.FS.readBinFile doc)
    ensure (blank == 0 && drawn > 50) s!"the value is drawn: {blank} → {drawn} dark pixels"
    let again := dir / "again.pdf"
    IO.FS.writeBinFile again original
    let second ← Edit.apply { root := dir / "store2" } again fill
    let hashOf := fun (j : Json) => ((j.getObjValD "node").getObjValAs? String "hash").toOption
    ensure (hashOf first == hashOf second && (hashOf first).isSome) "deterministic form filling"
    for (bad, why) in [(Edit.Operation.fillForm #[("reference", "R-99")], "read-only"),
                       (.fillForm #[("nonexistent", "x")], "unknown field")] do
      let before ← IO.FS.readBinFile doc
      let refused ← (Edit.apply store doc bad).toBaseIO
      ensure (match refused with | .error _ => true | .ok _ => false) s!"refuses a {why}"
      ensure ((← IO.FS.readBinFile doc) == before) s!"and leaves the file after a {why}"
    let _ ← Edit.undo store doc
    ensure ((← IO.FS.readBinFile doc) == original) "undo restores the blank form"
  IO.println "PASS forms: fields, values, choices; filling drawn on the page, deterministic, refusals, undo"

def testDiff : IO Unit := do
  let words (s : String) := (s.splitOn " ").toArray
  let script := Diff.diff id (words "the quick brown fox jumps") (words "the slow brown fox leaps high")
  let kept := script.filter fun | .keep .. => true | _ => false
  ensure (kept.length == 3) "LCS keeps the three shared words"
  ensure (Diff.olds script == (words "the quick brown fox jumps").toList) "old side reconstructs"
  ensure (Diff.news script == (words "the slow brown fox leaps high").toList) "new side reconstructs"
  -- Over budget the script is coarse but still exact.
  let coarse := Diff.diff id (words "a b c") (words "a x c") (budget := 0)
  ensure (Diff.olds coarse == ["a", "b", "c"] && Diff.news coarse == ["a", "x", "c"]) "coarse script exact"
  withScratchDir fun dir => do
    let old := dir / "old.pdf"
    let new := dir / "new.pdf"
    IO.FS.writeFile old Fixtures.reader
    -- Only the first page's line: String.replace would change every page.
    let parts := Fixtures.reader.splitOn "Line 3: text and shapes"
    IO.FS.writeFile new (parts.head! ++ "Line 3: data and shapes" ++ "Line 3: text and shapes".intercalate parts.tail!)
    let store : Edit.Store := { root := dir / "store" }
    let _ ← Edit.apply store new (.select #[1, 3])
    let result ← DocDiff.compare old.toString new.toString
    let summary := result.getObjValD "summary"
    ensure ((summary.getObjValAs? Nat "changed_pages").toOption == some 1 &&
      (summary.getObjValAs? Nat "removed_pages").toOption == some 1 &&
      (summary.getObjValAs? Nat "added_words").toOption == some 1) s!"diff summary {summary.compress}"
    let pages := (result.getObjValAs? (Array Json) "pages").toOption.getD #[]
    let changed := pages.find? fun p => (p.getObjValAs? String "status").toOption == some "changed"
    let added := changed.bind fun p => ((p.getObjValAs? (Array Json) "added").toOption.bind (·[0]?))
    ensure ((added.bind fun a => (a.getObjValAs? String "text").toOption) == some "data") "changed word"
    ensure ((added.bind fun a => (a.getObjValAs? (Array Json) "quads").toOption.map (·.size)) == some 1)
      "changed word keeps its polygon"
    -- A page turned, its words unchanged, is a changed page too.
    let turned := dir / "turned.pdf"
    IO.FS.writeFile turned Fixtures.reader
    let _ ← Edit.apply store turned (.rotate #[1] 90)
    let rotated ← DocDiff.compare old.toString turned.toString
    let summary := rotated.getObjValD "summary"
    ensure ((summary.getObjValAs? Nat "changed_pages").toOption == some 1 &&
      (summary.getObjValAs? Nat "added_words").toOption == some 0) s!"turned page: {summary.compress}"
    let page := ((rotated.getObjValAs? (Array Json) "pages").toOption.getD #[])[0]?
    let rotation (side : String) := page.bind fun p =>
      ((p.getObjVal? "geometry").toOption.bind fun g => (g.getObjVal? side).toOption.bind
        fun s => (s.getObjValAs? Nat "rotate").toOption)
    ensure (rotation "old" == some 0 && rotation "new" == some 90) "rotation 0 → 90 reported"
  IO.println "PASS sound and minimal diffs; aligned pages, changed words with polygons, turned pages"

def testSynctex : IO Unit := do
  let some dir ← IO.getEnv "VELLUMORY_SYNCTEX_FIXTURE"
    | throw <| IO.userError "VELLUMORY_SYNCTEX_FIXTURE is not set (nix develop provides it)"
  let pdf := s!"{dir}/doc.pdf"
  -- doc.tex: lines 3 and 5 are page one, line 7 follows \newpage.
  let line7 ← Native.synctexForward pdf "doc.tex" 7 0
  ensure (line7.size ≥ 1 && line7.all (·.page == 2)) s!"line 7 is on page 2: {(toJson line7).compress}"
  let line3 ← Native.synctexForward pdf s!"{dir}/doc.tex" 3 0
  let first := line3[0]!
  ensure (first.page == 1 && first.x > 100 && first.x < 200 && first.y > 100 && first.y < 160 &&
    first.width > 300 && first.height > 5 && first.height < 15) s!"line 3 box: {(toJson line3).compress}"
  let line5 ← Native.synctexForward pdf "doc.tex" 5 0
  ensure (line5.all fun b => b.page == 1 && b.y > first.y) "line 5 lies below line 3 on page one"
  ensure ((← Native.synctexForward pdf "doc.tex" 400 0).all (·.page ≥ 1)) "lines past the end stay valid"
  -- Backward: the middle of line 3's box names line 3 of doc.tex.
  let sources ← Native.synctexBackward pdf 1 (first.x + first.width / 2) (first.y + first.height / 2)
  ensure (sources.any fun s => s.file.endsWith "doc.tex" && s.line == 3) s!"backward: {(toJson sources).compress}"
  let page2 ← Native.synctexBackward pdf 2 (line7[0]!.x + 5) (line7[0]!.y + 4)
  ensure (page2.any (·.line == 7)) "backward on page two"
  let missing ← (Native.synctexForward s!"{dir}/absent.pdf" "doc.tex" 3 0).toBaseIO
  ensure (match missing with | .error _ => true | .ok _ => false) "a document without SyncTeX data is an error"
  IO.println "PASS SyncTeX forward and backward search through libsynctex"

def testGeometry : IO Unit := do
  ensure (clipBand 1000 ⟨0, 1000⟩ ⟨900, 1400⟩ == ⟨900, 1000⟩) "page end clipping"
  ensure (clipBand 1000 ⟨0, 1000⟩ ⟨1200, 1400⟩ == ⟨1000, 1000⟩) "off-page viewport"
  ensure (clipBand 0 ⟨0, 1000⟩ ⟨0, 800⟩ == ⟨0, 0⟩) "empty page"
  for height in [0:20] do
    for top in [0:25] do
      for bottom in [top:25] do
        let band := clipBand height ⟨0, height⟩ ⟨top, bottom⟩
        ensure (band.stop - band.start ≤ bottom - top) "raster work exceeds viewport"
  IO.println "PASS geometry boundaries and viewport work bounds"

def testShell : IO Unit := do
  let self ← IO.appPath
  let literals := #["a b", "$(touch should-not-exist)", "; false", "'\"\\", "a\nb", "", "λ"]
  let output ← Shell.run <| Shell.checked { program := self.toString, args := #["--echo-args"] ++ literals }
  ensure (output.stdout.trimAscii.toString == (toJson literals).compress) "argument boundaries changed"
  let nonzero ← Shell.run <| Shell.capture (command% self.toString ["--exit-seven"])
  ensure (nonzero.exitCode == 7) "exit status lost"
  let rejected ← try
    let _ ← Shell.run <| Shell.checked (command% self.toString ["--exit-seven"])
    pure false
  catch _ => pure true
  ensure rejected "checked command did not propagate failure"
  let nulRejected ← try
    let _ ← Shell.run <| Shell.capture (command% self.toString ["\x00"])
    pure false
  catch _ => pure true
  ensure nulRejected "NUL argument accepted"
  withScratchDir fun dir => do
    let value ← Shell.run <| Shell.inDirectory dir <| Shell.withEnv "VELLUMORY_TEST_VALUE" "a b" <|
      Shell.checked (command% self.toString ["--echo-context"])
    ensure (value.stdout.trimAscii.toString ==
      (toJson #[dir.toString, "a b"]).compress) "cwd/environment context not applied"
  IO.println "PASS literal argv, exit codes, rejection and scoped process context"

/-- A minimal one-page PDF fixture with byte-accurate classic xref offsets.
    This is test data construction, not the application's PDF serializer. -/
def pdfFixture (brokenXref := false) : String := Id.run do
  let objects := #[
    "<< /Type /Catalog /Pages 2 0 R >>",
    "<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << >> >>"]
  let mut text := "%PDF-1.7\n"
  let mut offsets : Array Nat := #[]
  for i in [0:objects.size] do
    offsets := offsets.push text.utf8ByteSize
    text := text ++ s!"{i + 1} 0 obj\n{objects[i]!}\nendobj\n"
  let startXref := if brokenXref then 1 else text.utf8ByteSize
  text := text ++ "xref\n0 4\n0000000000 65535 f \n"
  for offset in offsets do
    let digits := toString offset
    let padding := String.ofList (List.replicate (10 - digits.length) '0')
    text := text ++ padding ++ digits ++ " 00000 n \n"
  return text ++ s!"trailer\n<< /Size 4 /Root 1 0 R >>\nstartxref\n{startXref}\n%%EOF\n"

def testPdf : IO Unit := do
  ensure ((Pdf.checkMarker "%PDF-1.7\n".toUTF8).isSome) "PDF marker rejected"
  ensure ((Pdf.checkMarker ByteArray.empty).isNone) "empty marker accepted"
  ensure ((Pdf.checkMarker "hello".toUTF8).isNone) "non-PDF marker accepted"
  ensure (Pdf.qpdfStatus 3 == .warnings) "warnings promoted to clean"
  withScratchDir fun dir => do
    let good := dir / "--literal ; $() λ.pdf"
    IO.FS.writeFile good pdfFixture
    let before ← IO.FS.readBinFile good
    let report ← Pdf.checkWithQpdf good
    ensure (report.status == .clean) s!"qpdf rejected the one-page fixture: {report.stderr}"
    ensure (before == (← IO.FS.readBinFile good)) "check modified source PDF"
    let bad := dir / "broken.pdf"
    IO.FS.writeFile bad "%PDF-1.7\nthis is not a PDF\n"
    let failure ← Pdf.checkWithQpdf bad
    ensure (failure.status == .failed) "malformed PDF accepted"
    ensure ((Pdf.checkMarker (← IO.FS.readBinFile bad)).isSome)
      "fixture must show a marker does not imply PDF validity"
    let repairable := dir / "repairable.pdf"
    IO.FS.writeFile repairable (pdfFixture true)
    let warning ← Pdf.checkWithQpdf repairable
    ensure (warning.status == .warnings && warning.exitCode == 3)
      "recoverable xref warning was not preserved"
  IO.println "PASS PDF evidence distinction, qpdf integration and source preservation"

def testCli : IO Unit := do
  let app := (← IO.appPath).parent.get! / "vellumory"
  let help ← Shell.run <| Shell.checked (command% app.toString ["--help"])
  ensure ((help.stdout.splitOn "SUBCOMMANDS").length > 1) "generated subcommand help missing"
  let good ← Shell.run <| Shell.checked (command% app.toString ["clip", "1000", "900", "1400"])
  let json := Json.parse good.stdout
  ensure (json.toOption == some (Json.mkObj [("start", toJson (900 : Nat)), ("stop", toJson (1000 : Nat))]))
    "CLI did not use clipped geometry"
  for args in #[#["clip", "bad", "0", "1"], #["clip", "10", "8", "2"], #["unknown"]] do
    let result ← Shell.run <| Shell.capture { program := app.toString, args }
    ensure (result.exitCode != 0) "invalid CLI input accepted"
  -- A document named like a page range is a document: given to qpdf
  -- positionally, 2-3 would select pages 2 and 3 of the file before it.
  withScratchDir fun dir => do
    IO.FS.writeFile (dir / "doc.pdf") Fixtures.reader
    IO.FS.writeFile (dir / "2-3") Fixtures.reader
    let pages (name : String) : IO Nat := do
      return (← Native.pageSizes (← Native.openDocument (← IO.FS.readBinFile (dir / name)) "")).size
    let _ ← Shell.run <| Shell.inDirectory dir <|
      Shell.checked (command% app.toString ["merge", "out.pdf", "doc.pdf", "2-3"])
    ensure ((← pages "out.pdf") == 6) s!"merge with a document named 2-3: {← pages "out.pdf"} pages"
    let _ ← Shell.run <| Shell.inDirectory dir <|
      Shell.checked (command% app.toString ["extract", "doc.pdf", "1,3", "two.pdf"])
    ensure ((← pages "two.pdf") == 2) "extract of pages 1 and 3"
  IO.println "PASS generated CLI parsing, help, errors, geometry and page selection by --file"

def testSearch : IO Unit := do
  let doc ← Native.openDocument Fixtures.reader.toUTF8 ""
  let normal ← Native.searchPage doc 0 "Vellumory" true 1000
  ensure (normal.hits.size == 1 && normal.hits[0]!.size == 1 && !normal.truncated) "title search"
  ensure ((← Native.searchPage doc 0 "vellumory" true 1000).hits.isEmpty) "case-sensitive search"
  ensure ((← Native.searchPage doc 0 "vellumory" false 1000).hits.size == 1) "case-insensitive search"
  ensure ((← Native.searchPage doc 0 "absent phrase" false 1000).hits.isEmpty) "no-match search"
  let many ← Native.searchPage doc 0 "text" false 1000
  ensure (many.hits.size == 17 && !many.truncated) "separate matches must not merge"
  let limited ← Native.searchPage doc 0 "text" false 1
  ensure (limited.hits.size == 1 && limited.truncated) "result truncation must be explicit"
  let exact ← Native.searchPage doc 0 "text" false 17
  ensure (exact.hits.size == 17 && !exact.truncated) "exact result limit is not truncation"
  let multiline ← Native.searchPage doc 0 "boundaries. Line 2:" false 1000
  ensure (multiline.hits.size == 1 && multiline.hits[0]!.size == 2) "multiline hit polygon grouping"
  let rotated ← Native.searchPage doc 1 "Vellumory" true 1000
  let cropped ← Native.searchPage doc 2 "Vellumory" true 1000
  ensure (rotated.hits.size == 1 && cropped.hits.size == 1) "rotated/cropped matches"
  let a := normal.hits[0]![0]!
  let b := rotated.hits[0]![0]!
  let c := cropped.hits[0]![0]!
  ensure ((b.x0 - (792 - a.y0)).abs < 0.001 && (b.y0 - a.x0).abs < 0.001) "rotated text quad"
  ensure ((c.x0 - (a.x0 - 22)).abs < 0.001 && (c.y0 - (a.y0 - 22)).abs < 0.001) "cropped text quad"
  let unicode ← Native.openDocument (Fixtures.document #[
    "<< /Type /Catalog /Pages 2 0 R >>",
    "<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 300 300] /Resources << /Font << /F1 4 0 R >> >> /Contents 5 0 R >>",
    "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>",
    Fixtures.stream "BT /F1 18 Tf 30 200 Td (caf\\351 CAF\\311) Tj ET\n"]).toUTF8 ""
  ensure ((← Native.searchPage unicode 0 "café" false 1000).hits.size == 2) "Unicode search case folding"
  ensure ((← Native.searchPage unicode 0 "café" true 1000).hits.size == 1) "Unicode case-sensitive search"
  for (page, query, limit) in #[(3, "text", 1000), (0, "", 1000), (0, "a\x00b", 1000), (0, "a", 0)] do
    let rejected ← try
      let _ ← Native.searchPage doc page query false limit
      pure false
    catch _ => pure true
    ensure rejected "invalid native search accepted"
  IO.println "PASS text search, case, Unicode, grouped polygons, rotation/crop and explicit limits"

def ppmHeader (ppm : ByteArray) : String :=
  String.fromUTF8! (ppm.extract 0 (ppm.size - (ppmPixels ppm).size))

def pixelAt (ppm : ByteArray) (width x y : Nat) : Nat × Nat × Nat :=
  let p := ppmPixels ppm
  let i := (y * width + x) * 3
  (p[i]!.toNat, p[i+1]!.toNat, p[i+2]!.toNat)

def testNative : IO Unit := do
  let textOnly ← Native.openDocument (Fixtures.reader false).toUTF8 ""
  let rich ← Native.openDocument Fixtures.reader.toUTF8 ""
  let outline ← Native.outline rich
  ensure (outline.map (·.title) == #["Reading", "Geometry", "Geometry", "Resources", "Lean website"])
    "outline titles/preorder/duplicate preservation"
  ensure (outline.map (·.depth) == #[0, 1, 1, 0, 1]) "outline hierarchy"
  ensure ((← Native.displayPreferences rich) == " ") "no display preferences in the plain fixture"
  let asking ← Native.openDocument (Fixtures.reader (catalog := "/PageLayout /TwoColumnRight /PageMode /UseOutlines ")).toUTF8 ""
  ensure ((← Native.displayPreferences asking) == "TwoColumnRight UseOutlines") "catalog /PageLayout and /PageMode"
  let slide ← Native.openDocument (Fixtures.document #["<< /Type /Catalog /Pages 2 0 R >>",
    "<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 200 100] /Dur 3.5 /Trans << /S /Dissolve /D 0.5 >> >>"]).toUTF8 ""
  let timed ← IO.ofExcept (Json.parse (← Native.pageInfo slide 0))
  let trans := (timed.getObjVal? "transition").toOption.getD .null
  ensure ((timed.getObjValAs? Float "duration").toOption == some 3.5 &&
      (trans.getObjValAs? String "style").toOption == some "Dissolve" &&
      (trans.getObjValAs? Float "duration").toOption == some 0.5 &&
      (trans.getObjValAs? String "dimension").toOption == some "H" &&
      (trans.getObjValAs? String "motion").toOption == some "I" &&
      (trans.getObjValAs? Float "direction").toOption == some 0 &&
      (trans.getObjValAs? Float "scale").toOption == some 1 &&
      (trans.getObjValAs? Bool "opaque").toOption == some false) s!"page /Dur and /Trans with defaults: {timed.compress}"
  -- Characters for a keyboard caret: lines in reading order, logical boundaries.
  let chars ← IO.ofExcept (Json.parse (← Native.pageChars rich 0))
  let lines := (chars.getObjValAs? (Array Json) "lines").toOption.getD #[]
  let firstLine := lines[0]?.getD .null
  let glyphs : Array (Array Json) := (firstLine.getObjValAs? (Array (Array Json)) "chars").toOption.getD #[]
  let lineText := String.join (glyphs.toList.map fun g => match g[0]? with | some (Json.str s) => s | _ => "")
  ensure (lines.size > 10 && lineText.startsWith "Vellumory") s!"page characters by line: {lineText}"
  let starts := glyphs.filterMap fun g => match g[1]? with | some (Json.num x) => some x | _ => none
  ensure (starts.size == glyphs.size && (List.range (starts.size - 1)).all fun i => decide (starts[i]! < starts[i+1]!))
    "character starts advance left to right"
  let untimed ← IO.ofExcept (Json.parse (← Native.pageInfo rich 0))
  ensure ((untimed.getObjVal? "duration").toOption == some .null &&
      (untimed.getObjVal? "transition").toOption == some .null) "no /Dur or /Trans is null"
  ensure (outline.map (·.page) == #[some 0, some 1, some 2, none, none]) "outline destinations"
  ensure (outline[0]!.y == 192 && outline[2]!.y == 120) "outline top-left/crop coordinates"
  ensure (outline[0]!.expanded && !outline[3]!.expanded) "outline expansion state"
  ensure (outline[4]!.uri == "https://lean-lang.org/") "external outline URI"
  ensure ((← Native.pageLabels rich) == #["i", "Ch-1", "Ch-2"]) "page label styles/prefixes"
  let links ← Native.pageLinks rich 0
  ensure (links.size == 2) s!"page links: {links.size}"
  let internal := links.filter (!·.external)
  let external := links.filter (·.external)
  ensure (internal.size == 1 && internal[0]!.page == some 2 && internal[0]!.y == 120) "internal link target"
  ensure ((internal[0]!.x0 - 40).abs < 0.01 && (internal[0]!.y0 - 32).abs < 0.01 &&
    (internal[0]!.y1 - 77).abs < 0.01) "link rectangle in top-left page points"
  ensure (external.size == 1 && external[0]!.uri == "https://example.org/vellumory" &&
    external[0]!.page.isNone) "external link"
  ensure ((← Native.pageLinks rich 1).isEmpty) "page without links"
  let dimensions ← Native.pageSizes rich
  ensure (dimensions == #[(612.0, 792.0), (792.0, 612.0), (568.0, 748.0)])
    "rotation/crop dimensions"
  ensure (dimensions == (← Native.loadedPageSizes rich))
    "page-tree bounds differ from loaded-page bounds"
  for page in [0:3] do
    ensure (((← Native.pageText rich page.toUInt32).splitOn "Vellumory").length > 1)
      "text missing from fixture"
    for width in #[317, 612] do
      let (w, h) := dimensions[page]!
      let height := (h * width.toFloat / w).ceil.toUInt64.toNat
      let whole := ppmPixels (← Native.render rich page.toUInt32 width.toUInt32 0 height.toUInt32)
      let textWhole := ppmPixels (← Native.render textOnly page.toUInt32 width.toUInt32 0 height.toUInt32)
      let mut assembled := ByteArray.empty
      let mut textAssembled := ByteArray.empty
      let mut top := 0
      while top < height do
        let bottom := min height (top + 193)
        assembled := assembled ++ ppmPixels (← Native.render rich page.toUInt32 width.toUInt32
          top.toUInt32 bottom.toUInt32)
        textAssembled := textAssembled ++ ppmPixels (← Native.render textOnly page.toUInt32
          width.toUInt32 top.toUInt32 bottom.toUInt32)
        top := bottom
      ensure (textAssembled == textWhole) s!"text/rectangle strip mismatch page {page}, width {width}"
      -- Horizontal tiles stitched row by row must equal whole-width bands.
      let cut := width / 3
      let cookie ← Native.Cookie.new
      let left := ppmPixels (← Native.renderPpm textOnly cookie page.toUInt32 width.toUInt32
        0 0 cut.toUInt32 height.toUInt32)
      let right := ppmPixels (← Native.renderPpm textOnly cookie page.toUInt32 width.toUInt32
        cut.toUInt32 0 width.toUInt32 height.toUInt32)
      let mut stitched := ByteArray.empty
      for y in [0:height] do
        stitched := stitched ++ left.extract (y * cut * 3) ((y + 1) * cut * 3)
        stitched := stitched ++ right.extract (y * (width - cut) * 3) ((y + 1) * (width - cut) * 3)
      ensure (stitched == textWhole) s!"tile stitching mismatch page {page}, width {width}"
      if assembled != whole then
        let mut count := 0
        let mut largest := 0
        for i in [0:whole.size] do
          if assembled[i]! != whole[i]! then
            count := count + 1
            let a := assembled[i]!.toNat
            let b := whole[i]!.toNat
            largest := max largest (max (a - b) (b - a))
        -- MuPDF's clipped edge rasterizer rounds diagonal antialiasing differently.
        -- The upstream CLI reproduces the same 2029 channels / max 14 at width 317.
        -- Keep a strict fixture-specific subpixel tolerance, not a general theorem.
        ensure (largest ≤ 17 && count * 100 ≤ whole.size)
          s!"raster drift: page {page}, width {width}, {count} channels, max delta {largest}"
        IO.println s!"AA comparison: page {page}, width {width}, {count} channels, max delta {largest}"
  ensure ((← Native.sha256 "abc".toUTF8) ==
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad") "SHA-256 known answer"
  let bytes := pdfFixture.toUTF8
  let doc ← Native.openDocument bytes ""
  ensure ((← Native.outline doc).isEmpty) "missing outline must be empty"
  let defaultLabels ← Native.pageLabels doc
  ensure (defaultLabels == #["1"]) s!"default physical page label: {repr defaultLabels}"
  let emptyLabels ← Native.openDocument (Fixtures.document #[
    "<< /Type /Catalog /Pages 2 0 R /PageLabels << /Nums [0 << /P () >>] >> >>",
    "<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << >> >>"]).toUTF8 ""
  ensure ((← Native.pageLabels emptyLabels) == #[""]) "intentional empty label was replaced"
  let sizes ← Native.pageSizes doc
  ensure (sizes.size == 1 && sizes[0]! == (612.0, 792.0)) "native page dimensions"
  let fullPpm ← Native.render doc 0 612 0 792
  let full := ppmPixels fullPpm
  ensure (ppmHeader fullPpm == "P6\n612 792\n255\n") "PPM header"
  let first := ppmPixels (← Native.render doc 0 612 0 193)
  let second := ppmPixels (← Native.render doc 0 612 193 792)
  ensure (first ++ second == full) "strip rasterization differs from full-page rendering"
  ensure (first.size == 612 * 193 * 3) "strip allocation is not bounded to requested pixels"
  for (page, width, top, bottom) in #[(1, 612, 0, 1), (0, 0, 0, 1), (0, 612, 30, 20), (0, 612, 0, 793)] do
    let rejected ← try
      let _ ← Native.render doc page width top bottom
      pure false
    catch _ => pure true
    ensure rejected "invalid native render accepted"
  -- Themes recolour before overlays; overlays use page-point polygons.
  let cookie ← Native.Cookie.new
  let tinted ← Native.renderPpm doc cookie 0 612 0 0 10 10 (.tint 0xffffff 0x202122)
  ensure (pixelAt tinted 10 5 5 == (0x20, 0x21, 0x22)) "tint must map white to background"
  let inverted ← Native.renderPpm doc cookie 0 612 0 0 10 10 .invertLuminance
  -- MuPDF 1.27.2 maps white to (4, 4, 4) through its fixed-point luminance path.
  let (r, g, b) := pixelAt inverted 10 5 5
  ensure (r ≤ 8 && g ≤ 8 && b ≤ 8) s!"luminance inversion of white: {(r, g, b)}"
  let square : FloatArray := ⟨#[2, 2, 8, 2, 8, 8, 2, 8]⟩
  let overlay : Native.Overlay :=
    { quads := square, kinds := ⟨#[0]⟩, palette := ⟨#[255, 0, 0, 255]⟩, multiply := false }
  let marked ← Native.renderPpm doc cookie 0 612 0 0 10 10 .none overlay
  ensure (pixelAt marked 10 5 5 == (255, 0, 0) && pixelAt marked 10 0 0 == (255, 255, 255))
    "opaque overlay placement"
  let yellow ← Native.renderPpm doc cookie 0 612 0 0 10 10 (.tint 0 0x808080)
    { overlay with palette := ⟨#[255, 255, 0, 255]⟩, multiply := true }
  ensure (pixelAt yellow 10 5 5 == (128, 128, 0)) "multiply overlay after theme"
  -- Written to a file, a raster is the same bytes, and nothing is left pending.
  let rasters ← IO.FS.createTempDir
  try
    let target := rasters / "strip.ppm"
    let written ← Native.renderTimed doc cookie 0 612 0 0 612 193 (.tint 0 0x808080) overlay (file := some target)
    let returned ← Native.renderPpm doc cookie 0 612 0 0 612 193 (.tint 0 0x808080) overlay
    ensure (written.bytes.isEmpty && written.size == returned.size) "a raster written to a file is returned too"
    ensure ((← IO.FS.readBinFile target) == returned) "a raster written to a file differs from the returned one"
    ensure ((← rasters.readDir).size == 1) "a pending raster file was left behind"
    let refused ← try
      let _ ← Native.renderTimed doc cookie 0 612 0 0 10 10 (file := some (rasters / "missing" / "strip.ppm"))
      pure false
    catch _ => pure true
    ensure refused "a raster for a directory that does not exist was accepted"
    ensure ((← rasters.readDir).size == 1) "a failed raster write left a file behind"
  finally IO.FS.removeDirAll rasters
  for bad in #[{ overlay with kinds := ⟨#[1]⟩ }, { overlay with quads := ⟨#[1, 2]⟩ }] do
    let rejected ← try
      let _ ← Native.renderPpm doc cookie 0 612 0 0 10 10 .none bad
      pure false
    catch _ => pure true
    ensure rejected "inconsistent overlay accepted"
  -- An aborted cookie stops work without poisoning the display-list cache.
  let stopped ← Native.Cookie.new
  stopped.abort
  let cancelled ← try
    let _ ← Native.renderPpm rich stopped 0 612 0 0 612 100
    pure ""
  catch e => pure e.toString
  ensure (cancelled == "cancelled") s!"aborted render: {cancelled}"
  let after ← Native.render rich 0 612 0 100
  ensure (ppmPixels after == (ppmPixels (← Native.render rich 0 612 0 792)).extract 0 (612 * 100 * 3))
    "render after cancellation"
  -- Concurrent renders of one document equal serial renders.
  let jobs := (List.range 24).map fun i => (i % 3, 300 + 37 * (i % 5), 7 * i % 100)
  let serial ← jobs.mapM fun (page, width, top) =>
    Native.render rich page.toUInt32 width.toUInt32 top.toUInt32 (top + 50).toUInt32
  let tasks ← jobs.mapM fun (page, width, top) => IO.asTask (prio := .dedicated) do
    Native.render rich page.toUInt32 width.toUInt32 top.toUInt32 (top + 50).toUInt32
  let parallel ← tasks.mapM fun task => do IO.ofExcept (← IO.wait task)
  ensure (serial == parallel) "concurrent rendering differs from serial rendering"
  let malformed ← try
    let _ ← Native.openDocument "not a PDF".toUTF8 ""
    pure false
  catch _ => pure true
  ensure malformed "malformed native document accepted"
  -- Raster store: least recently used eviction never drops the current key.
  let store : Worker.Store := { budget := 10 }
  let (store, gone) := store.touch "a" 4
  ensure gone.isEmpty "store evicted below budget"
  let (store, _) := store.touch "b" 4
  let (store, _) := store.touch "a" 4
  let (store, gone) := store.touch "c" 4
  ensure (gone == ["b"] && store.order.map (·.1) == ["a", "c"] && store.total == 8) "LRU eviction"
  let (store, gone) := store.touch "huge" 50
  ensure (store.order.map (·.1) == ["huge"] && gone.length == 2) "oversized entry retained alone"
  withScratchDir fun dir => do
    let path := dir / "fixture.pdf"
    IO.FS.writeBinFile path bytes
    let (opened, _) ← Worker.openFile (Json.mkObj [("path", toJson path.toString)])
    let shared ← Worker.Shared.new dir dir 1 (64 * 1048576)
    let params := Json.mkObj [("page", toJson (0 : Nat)), ("width", toJson (612 : Nat)),
      ("top", toJson (750 : Nat)), ("bottom", toJson (900 : Nat))]
    let cold ← Worker.render shared opened (← Native.Cookie.new) params
    let warm ← Worker.render shared opened (← Native.Cookie.new) params
    ensure ((cold.getObjValAs? Nat "pixels").toOption == some (612 * 42)) "worker clipping budget"
    ensure ((warm.getObjValAs? Bool "cached").toOption == some true) "warm cache missed"
    ensure ((warm.getObjValAs? Nat "pixels").toOption == some 0) "warm cache rasterized pixels"
    let tile ← Worker.render shared opened (← Native.Cookie.new) (params.setObjVal! "left" (toJson (600 : Nat))
      |>.setObjVal! "right" (toJson (700 : Nat)))
    ensure ((tile.getObjValAs? Nat "pixels").toOption == some (12 * 42)) "worker horizontal clipping"
    IO.FS.writeFile path "replaced while open"
    let stillOpen := ppmPixels (← Native.render opened.doc 0 612 0 10)
    ensure (stillOpen == full.extract 0 (612 * 10 * 3)) "opened bytes changed after source replacement"
  IO.println "PASS native rendering, tiles, themes, overlays, cancellation, concurrency, store and immutable bytes"

/-- Dropping a document's caches changes nothing it shows: the same pixels and
    text after, made again from the document. -/
def testForgetCaches : IO Unit := do
  let doc ← Native.openDocument Fixtures.reader.toUTF8 ""
  let before ← Native.render doc 1 612 0 400
  let text ← Native.pageText doc 1
  Native.forgetCaches doc
  Native.forgetCaches doc
  ensure ((← Native.render doc 1 612 0 400) == before) "a render after dropping the caches differs"
  ensure ((← Native.pageText doc 1) == text && !text.isEmpty) "the text after dropping the caches differs"
  IO.println "PASS caches dropped and made again, the same pixels and text"

/-- End-to-end lanes: every request gets exactly one reply, cancellation is
    acknowledged, and replies may arrive out of order. -/
def testWorkerProtocol : IO Unit := do
  let app := (← IO.appPath).parent.get! / "vellumory"
  withScratchDir fun dir => do
    let pdf := dir / "reader.pdf"
    IO.FS.writeFile pdf Fixtures.reader
    let child ← IO.Process.spawn {
        cmd := app.toString, args := #["serve"], stdin := .piped, stdout := .piped,
        stderr := .null, env := #[("XDG_RUNTIME_DIR", some dir.toString),
          ("VELLUMORY_RENDER_THREADS", some "4")] }
    let send (json : Json) := do child.stdin.putStrLn json.compress; child.stdin.flush
    send <| Json.mkObj [("id", toJson (1 : Nat)), ("method", toJson "hello")]
    send <| Json.mkObj [("id", toJson (2 : Nat)), ("method", toJson "open"),
      ("params", Json.mkObj [("path", toJson pdf.toString)])]
    let ids := List.range' 10 40
    for id in ids do
      send <| Json.mkObj [("id", toJson id), ("method", toJson "render"),
        ("params", Json.mkObj [("page", toJson (id % 3)), ("width", toJson (300 + id)),
          ("top", toJson (0 : Nat)), ("bottom", toJson (400 : Nat)), ("priority", toJson (id % 4))])]
    send <| Json.mkObj [("id", toJson (3 : Nat)), ("method", toJson "cancel"),
      ("params", Json.mkObj [("ids", toJson ((List.range' 30 20).filter (· % 2 == 0)))])]
    send <| Json.mkObj [("id", toJson (4 : Nat)), ("method", toJson "text"),
      ("params", Json.mkObj [("page", toJson (0 : Nat))])]
    send <| Json.mkObj [("id", toJson (5 : Nat)), ("method", toJson "bogus")]
    let (stdin, child) ← child.takeStdin
    let mut replies : Array Json := #[]
    for _ in [0:(ids.length + 5)] do
      let line ← child.stdout.getLine
      replies := replies.push (← IO.ofExcept (Json.parse line))
    stdin.flush
    let idsSeen := replies.filterMap fun r => (r.getObjValAs? Nat "id").toOption
    for id in ([1, 2, 3, 4, 5] ++ ids : List Nat) do
      ensure ((idsSeen.filter (· == id)).size == 1) s!"request {id} answered {(idsSeen.filter (· == id)).size} times"
    let opened := replies.find? fun r => (r.getObjValAs? Nat "id").toOption == some 2
    ensure ((opened.bind fun r => (r.getObjVal? "result" >>= (·.getObjValAs? String "page_layout")).toOption) == some "")
      "open reports the (absent) page layout"
    let hello := replies.find? fun r => (r.getObjValAs? Nat "id").toOption == some 1
    ensure ((hello.bind fun r => (r.getObjVal? "result" >>= (·.getObjValAs? Nat "threads")).toOption) == some 4)
      "hello must report render threads"
    for r in replies do
      let id := (r.getObjValAs? Nat "id").toOption.getD 0
      if 10 ≤ id && id < 50 then
        match r.getObjVal? "result" with
        | .ok result =>
          let path := (result.getObjValAs? String "path").toOption.getD ""
          ensure (path.startsWith dir.toString && (← (System.FilePath.mk path).pathExists)) s!"render {id} file"
        | .error _ =>
          ensure (30 ≤ id && id % 2 == 0 && (r.getObjValAs? Bool "cancelled").toOption == some true)
            s!"unexpected render failure {id}: {r.compress}"
    let bogus := replies.find? fun r => (r.getObjValAs? Nat "id").toOption == some 5
    ensure ((bogus.bind fun r => (r.getObjValAs? String "error").toOption).isSome) "unknown method error"
    let code ← child.wait
    ensure (code == 0) s!"worker exit status {code}"
    let leftovers ← (dir / "vellumory").readDir
    ensure leftovers.isEmpty "worker runtime directory survived exit"
  IO.println "PASS worker lanes, out-of-order replies, cancellation and runtime cleanup"

/-- Previews and thumbnails survive the session: a second worker finds them under
    their content address; sharp strips stay in the session's runtime directory. -/
def testPersistentRasters : IO Unit := do
  let app := (← IO.appPath).parent.get! / "vellumory"
  withScratchDir fun dir => do
    let pdf := dir / "doc.pdf"
    IO.FS.writeFile pdf Fixtures.reader
    let opened : IO.Ref (Array Json) ← IO.mkRef #[]
    let session (n : Nat) (persist : Bool) : IO Json := do
      let child ← IO.Process.spawn {
          cmd := app.toString, args := #["serve"], stdin := .piped, stdout := .piped, stderr := .null,
          env := #[("XDG_RUNTIME_DIR", some (dir / s!"run{n}").toString), ("XDG_CACHE_HOME", some (dir / "cache").toString)] }
      IO.FS.createDirAll (dir / s!"run{n}")
      child.stdin.putStrLn (Json.mkObj [("id", toJson (1 : Nat)), ("method", toJson "open"),
        ("params", Json.mkObj [("path", toJson pdf.toString)])]).compress
      child.stdin.putStrLn (Json.mkObj [("id", toJson (2 : Nat)), ("method", toJson "render"),
        ("params", Json.mkObj [("page", toJson (1 : Nat)), ("width", toJson (120 : Nat)), ("top", toJson (0 : Nat)),
          ("bottom", toJson (160 : Nat)), ("persist", toJson persist)])]).compress
      child.stdin.flush
      let openReply ← IO.ofExcept (Json.parse (← child.stdout.getLine))
      opened.modify (·.push ((openReply.getObjVal? "result").toOption.getD .null))
      let reply ← IO.ofExcept (Json.parse (← child.stdout.getLine))
      child.kill
      let _ ← child.wait
      IO.ofExcept (reply.getObjVal? "result")
    let first ← session 1 true
    let second ← session 2 true
    let path (r : Json) := (r.getObjValAs? String "path").toOption.getD ""
    ensure (path first == path second && (path first).startsWith (dir / "cache" / "vellumory" / "rasters").toString)
      s!"one persistent raster for both sessions: {path first}"
    ensure ((first.getObjValAs? Bool "cached").toOption == some false &&
        (second.getObjValAs? Bool "cached").toOption == some true) "rendered once, found the second time"
    let sharp ← session 3 false
    ensure ((path sharp).startsWith (dir / "run3").toString) "sharp strips stay in the session"
    -- Page measurements are kept by content address: measured by the first
    -- session, read by the next, measured again when the cache is damaged.
    let reads := (← opened.get).map fun r => (r.getObjValAs? Bool "bounds_cached").toOption
    let pages := (← opened.get).map fun r => (r.getObjVal? "pages").toOption.map (·.compress)
    ensure (reads == #[some false, some true, some true]) s!"page measurements read back: {reads}"
    ensure (pages.all (· == pages[0]!) && pages[0]!.isSome) "page sizes from the cache are the measured ones"
    let sizes := dir / "cache" / "vellumory" / "rasters" / "sizes"
    let kept ← sizes.readDir
    ensure (kept.size == 1) s!"one measurement file: {kept.map (·.fileName)}"
    let file := (kept.map (·.path))[0]!
    IO.FS.writeFile file "damaged"
    let _ ← session 4 true
    let after ← opened.get
    ensure ((after[3]!.getObjValAs? Bool "bounds_cached").toOption == some false &&
        (after[3]!.getObjVal? "pages").toOption.map (·.compress) == pages[0]!)
      "a damaged measurement file is measured again"
    ensure ((← IO.FS.readBinFile file).size > 7) "and written again"
    -- The budget trims the oldest first.
    let store := dir / "trim"
    IO.FS.createDirAll (store / "aa")
    for i in [0:4] do
      IO.FS.writeFile (store / "aa" / s!"{i}.ppm") (String.ofList (List.replicate 1000 'x'))
      IO.sleep 1100
    Worker.trimPersistent store 2500
    let left := (← (store / "aa").readDir).map (·.fileName) |>.qsort (· < ·)
    ensure (left == #["2.ppm", "3.ppm"]) s!"oldest trimmed first: {left}"
  IO.println "PASS persistent previews and thumbnails across sessions, trimmed oldest first"

/-- Preflight finds what veraPDF does not judge, without flagging what is allowed. -/
def testPreflight : IO Unit := do
  let codes (doc : Native.Document) : IO (Array String) := do return (← Preflight.check doc).map (·.code)
  let plain ← codes (← Native.openDocument Fixtures.reader.toUTF8 "")
  ensure (["fonts-not-embedded", "no-title", "no-language", "untagged"].all plain.contains) s!"plain fixture: {plain}"
  ensure (!plain.contains "broken-contents" && !plain.contains "broken-links")
    s!"a heading without a destination is not broken: {plain}"
  let broken ← Native.openDocument (Fixtures.document #[
    "<< /Type /Catalog /Pages 2 0 R /Outlines 6 0 R /Names << /JavaScript << /Names [(hi) 8 0 R] >> >> >>",
    "<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 200 200] /TrimBox [0 0 300 300] /Annots [4 0 R] /Contents 5 0 R >>",
    "<< /Type /Annot /Subtype /Link /Rect [10 10 50 50] /Dest [99 0 R /Fit] >>",
    Fixtures.stream "0 0 1 rg 10 10 40 40 re f\n",
    "<< /Type /Outlines /First 7 0 R /Last 7 0 R /Count 1 >>",
    "<< /Title (Gone) /Parent 6 0 R /Dest [99 0 R /Fit] >>",
    "<< /S /JavaScript /JS (app.alert\\(1\\)) >>"]).toUTF8 ""
  let findings ← Preflight.check broken
  let found := findings.map (·.code)
  for code in ["broken-links", "broken-contents", "javascript", "box-outside-page"] do
    ensure (found.contains code) s!"{code} expected in {found}"
  ensure ((findings[0]?.map (·.severity)) == some "error") "errors come first"
  ensure ((findings.find? (·.code == "broken-links")).map (·.pages) == some #[0]) "the page of the broken link"
  IO.println "PASS preflight beyond veraPDF: broken links and contents, scripts, boxes; allowed headings kept quiet"

/-- A worker removes the raster directories of workers that are gone, never
    those of running ones. -/
def testRuntimeSweep : IO Unit := do
  -- Without /proc (macOS) the sweep asks the kernel whether a worker runs.
  ensure (← Native.processAlive (← IO.Process.getPID)) "this process is not seen running"
  ensure (!(← Native.processAlive 999999999)) "a process with PID 999999999 is seen running"
  ensure (!(← Native.processAlive 0)) "PID 0 is seen as a worker"
  let app := (← IO.appPath).parent.get! / "vellumory"
  withScratchDir fun dir => do
    let parent := dir / "vellumory"
    let stale := parent / "worker-999999999-1"
    IO.FS.createDirAll stale
    IO.FS.writeFile (stale / "left.ppm") "P6"
    let spawn := IO.Process.spawn {
        cmd := app.toString, args := #["serve"], stdin := .piped, stdout := .piped,
        stderr := .null, env := #[("XDG_RUNTIME_DIR", some dir.toString)] }
    let hello (child : IO.Process.Child { stdin := .piped, stdout := .piped, stderr := .null }) := do
      child.stdin.putStrLn "{\"id\":1,\"method\":\"hello\"}"; child.stdin.flush
      let _ ← child.stdout.getLine
    let first ← spawn
    hello first
    ensure (!(← stale.pathExists)) "the gone worker's rasters are removed"
    let mine := s!"worker-{first.pid}-"
    let second ← spawn
    hello second
    let names := (← parent.readDir).map (·.fileName)
    ensure (names.any (·.startsWith mine)) "a running worker's directory stays"
    -- Killed, they cannot clean up; the next worker removes what they left.
    first.kill
    second.kill
    let _ ← first.wait
    let _ ← second.wait
    ensure ((← parent.readDir).size == 2) "killed workers leave their directories"
    let third ← spawn
    hello third
    let left := (← parent.readDir).map (·.fileName)
    ensure (left.size == 1 && left.all (·.startsWith s!"worker-{third.pid}-"))
      s!"the next worker sweeps them: {left}"
    -- (A worker that exits removes its own: see testWorkerProtocol.)
    third.kill
    let _ ← third.wait
  IO.println "PASS stale raster directories swept, running workers' kept"

/-- One worker, two documents: each request is answered from its own document;
    closing one leaves the other. -/
def testReflow : IO Unit := do
  let some fixture ← IO.getEnv "VELLUMORY_EPUB_FIXTURE"
    | throw <| IO.userError "VELLUMORY_EPUB_FIXTURE is not set (nix develop provides it)"
  let book := s!"{fixture}/book.epub"
  let bytes ← IO.FS.readBinFile book
  let pagesWith (width height em : Float) (css : String := "") : IO Nat := do
    return (← Native.pageSizes (← Native.openDocumentReflowed bytes book "" width height em css)).size
  let standard ← pagesWith 450 600 11
  ensure (standard ≥ 2) s!"the book is laid out on pages: {standard}"
  ensure ((← pagesWith 450 600 22) > standard) "larger text, more pages"
  ensure ((← pagesWith 900 600 11) < standard) "wider pages, fewer pages"
  ensure ((← pagesWith 450 600 11 "p { font-size: 300% }") > standard) "a user stylesheet applies"
  let doc ← Native.openDocumentReflowed bytes book "" 450 600 11 ""
  ensure (← Native.reflowable doc) "an EPUB reflows"
  let mut text := ""
  for page in [0:(← Native.pageSizes doc).size] do text := text ++ (← Native.pageText doc page.toUInt32)
  ensure ((text.splitOn "Chapter one").length > 1 && (text.splitOn "Chapter two").length > 1)
    s!"its text: {text.take 80}"
  ensure (!(← Native.reflowable (← Native.openDocument Fixtures.reader.toUTF8 ""))) "a PDF does not"
  -- Through the worker: the layout is part of a reflowed document's identity.
  withScratchDir fun dir => do
    let pdf := dir / "doc.pdf"
    IO.FS.writeFile pdf Fixtures.reader
    let app := (← IO.appPath).parent.get! / "vellumory"
    let child ← IO.Process.spawn {
        cmd := app.toString, args := #["serve"], stdin := .piped, stdout := .piped,
        stderr := .null, env := #[("XDG_RUNTIME_DIR", some dir.toString)] }
    let request (id : Nat) (params : List (String × Json)) : IO Json := do
      child.stdin.putStrLn (Json.mkObj [("id", toJson id), ("method", toJson "open"),
        ("params", Json.mkObj params)]).compress
      child.stdin.flush
      IO.ofExcept (Json.parse (← child.stdout.getLine))
    let hash (reply : Json) := ((reply.getObjVal? "result") >>= (·.getObjValAs? String "hash")).toOption.getD ""
    let count (reply : Json) :=
      (((reply.getObjVal? "result") >>= (·.getObjValAs? (Array Json) "pages")).toOption.getD #[]).size
    let small ← request 1 [("path", toJson book), ("doc", toJson (1 : Nat)),
      ("reflow", Json.mkObj [("width", toJson (450 : Float)), ("height", toJson (600 : Float)), ("em", toJson (11 : Float))])]
    let large ← request 2 [("path", toJson book), ("doc", toJson (2 : Nat)),
      ("reflow", Json.mkObj [("width", toJson (450 : Float)), ("height", toJson (600 : Float)), ("em", toJson (22 : Float))])]
    ensure (count small == standard && count large > standard) "the worker lays the book out as asked"
    ensure (hash small != hash large && hash small != "") "each layout its own identity"
    let plain ← request 3 [("path", toJson pdf.toString), ("doc", toJson (3 : Nat))]
    let asked ← request 4 [("path", toJson pdf.toString), ("doc", toJson (4 : Nat)),
      ("reflow", Json.mkObj [("em", toJson (22 : Float))])]
    ensure (hash plain == hash asked) "a PDF\x27s identity ignores reflow settings"
    let (stdin, child) ← child.takeStdin
    stdin.flush
    ensure ((← child.wait) == 0) "worker exits cleanly"
  IO.println "PASS reflow: EPUB laid out by page size, text size and user stylesheet; layout part of its identity"

def testCompose : IO Unit := do
  withScratchDir fun dir => do
    let reader := dir / "reader.pdf"
    let long := dir / "long.pdf"
    let turned := dir / "turned.pdf"
    IO.FS.writeFile reader Fixtures.reader
    IO.FS.writeFile long (Fixtures.longReader 4)
    -- A portrait page shown landscape: /Rotate 90.
    IO.FS.writeFile turned (Fixtures.document #["<< /Type /Catalog /Pages 2 0 R >>",
      "<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
      "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Rotate 90 /Contents 4 0 R >>",
      Fixtures.stream "0 g 0 0 306 792 re f\n"])
    let out := dir / "virtual.pdf"
    -- Pages 2–3 of one document, the top half of page 4 of another, the left half
    -- (as displayed) of a turned page.
    let pages ← Native.compose #[reader.toString, long.toString, turned.toString]
      ⟨#[2, 3, -1, 0, 0, 0, 4, 4, 0, 0, 1, 0.5, 1, 0, 0, 0, 0.5, 1]⟩ out.toString
    ensure (pages == 4) s!"composed {pages} pages"
    let doc ← Native.openDocument (← IO.FS.readBinFile out) ""
    let sizes ← Native.pageSizes doc
    ensure (sizes == #[(792, 612), (568, 748), (612, 396), (396, 612)]) s!"page sizes: {sizes}"
    ensure (((← Native.pageText doc 0).splitOn "Vellumory / 2").length > 1 &&
      ((← Native.pageText doc 2).splitOn "Vellumory / 4").length > 1) "pages in the order given"
    let fails := fun (act : IO UInt32) => do try let _ ← act; pure false catch _ => pure true
    ensure (← fails (Native.compose #[reader.toString] ⟨#[2, 9, -1, 0, 0, 0]⟩ (dir / "x.pdf").toString))
      "pages beyond the document are refused"
    -- The command line: cached by the parts and the sources' content.
    let app := (← IO.appPath).parent.get! / "vellumory"
    let compose (parts : String) : IO Json := do
      let child ← IO.Process.spawn {
        cmd := app.toString
        args := #["compose"]
        stdin := .piped
        stdout := .piped
        stderr := .null
        env := #[("XDG_CACHE_HOME", some (dir / "cache").toString)] }
      let (stdin, child) ← child.takeStdin
      stdin.putStr parts
      let output ← child.stdout.readToEnd
      let _ ← child.wait
      IO.ofExcept (Json.parse output)
    let parts := s!"[\{\"file\":\"{long}\",\"first\":2}]"
    let first ← compose parts
    let again ← compose parts
    let flag (j : Json) := (j.getObjValAs? Bool "cached").toOption
    ensure (flag first == some false && flag again == some true &&
      (first.getObjValAs? Nat "pages").toOption == some 3) s!"composition cached: {first.compress}"
    IO.FS.writeFile long (Fixtures.longReader 5)
    let changed ← compose parts
    ensure (flag changed == some false && (changed.getObjValAs? Nat "pages").toOption == some 4)
      "a changed source composes anew"
  IO.println "PASS virtual documents: pages of several documents in order, regions as crop boxes (turned pages too), cached by content"

def testPageReflow : IO Unit := do
  let doc ← Native.openDocument Fixtures.reader.toUTF8 ""
  let page ← Native.pageReflow doc 0 0
  IO.println page
  ensure (page.startsWith "<vellumory-page n=\"1\"") s!"one page of reflowable HTML: {page.take 200}"
  ensure ((page.splitOn "Line 1: text and shapes").length > 1) "the page's words"
  let rejected ← try let _ ← Native.pageReflow doc 99 0; pure false catch _ => pure true
  ensure rejected "a page outside the document is refused"
  IO.println "PASS a page as XHTML for reflowed reading"

def testContentBox : IO Unit := do
  let page (content : String) := s!"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents {content} >>"
  let doc ← Native.openDocument (Fixtures.document #["<< /Type /Catalog /Pages 2 0 R >>",
    "<< /Type /Pages /Kids [3 0 R 5 0 R 7 0 R] /Count 3 >>",
    page "4 0 R",
    -- A block of ink from (100, 150) to (500, 650), and a speck of dust in the margin.
    Fixtures.stream "0 g 100 150 400 500 re f 20 20 1 1 re f\n",
    page "6 0 R",
    -- The same block on light yellow paper.
    Fixtures.stream "1 1 0.8 rg 0 0 612 792 re f 0 g 100 150 400 500 re f\n",
    page "8 0 R",
    Fixtures.stream "\n"]).toUTF8 ""
  let box (number : UInt32) : IO (Option (Array Float)) := do
    match ← IO.ofExcept (Json.parse (← Native.contentBox doc number)) with
    | .null => return none
    | json => return (fromJson? json : Except String (Array Float)).toOption
  -- In points from the top-left: x 100–500, y 792 − 650 = 142 to 792 − 150 = 642.
  let near (b : Array Float) := b.size == 4 && (b[0]! - 100).abs < 2 && (b[1]! - 142).abs < 2 &&
    (b[2]! - 500).abs < 2 && (b[3]! - 642).abs < 2
  ensure ((← box 0).map near == some true) s!"content box without the speck: {← box 0}"
  ensure ((← box 1).map near == some true) s!"content box on coloured paper: {← box 1}"
  ensure ((← box 2).isNone) "a blank page has no content box"
  IO.println "PASS content box: ink against the paper, specks and coloured paper ignored, blank pages none"

def testTransitionFrames : IO Unit := do
  -- The page report gives Fly its /Di /None, /SS and /B.
  let flying ← Native.openDocument (Fixtures.document #["<< /Type /Catalog /Pages 2 0 R >>",
    "<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 200 100] /Trans << /S /Fly /Di /None /SS 0.25 /B true /M /O >> >>"]).toUTF8 ""
  let fly := ((← IO.ofExcept (Json.parse (← Native.pageInfo flying 0))).getObjVal? "transition").toOption.getD .null
  ensure ((fly.getObjValAs? String "direction").toOption == some "None" &&
      (fly.getObjValAs? Float "scale").toOption == some 0.25 && (fly.getObjValAs? Bool "opaque").toOption == some true &&
      (fly.getObjValAs? String "motion").toOption == some "O") s!"Fly /Trans: /Di /None, /SS, /B: {fly.compress}"
  withScratchDir fun dir => do
    let (w, h) := (960, 540)
    let header := s!"P6\n{w} {h}\n255\n"
    let plain (value : UInt8) : ByteArray := header.toUTF8 ++ ⟨Array.replicate (w * h * 3) value⟩
    let old := dir / "old.ppm"
    let new := dir / "new.ppm"
    IO.FS.writeBinFile old (plain 0)
    IO.FS.writeBinFile new (plain 255)
    let frame (style : String) (progress : Float) (direction : UInt32 := 0) : IO ByteArray := do
      let out := dir / "frame.ppm"
      Native.transitionFrame old.toString new.toString out.toString style progress direction
      let bytes ← IO.FS.readBinFile out
      return bytes.extract header.utf8ByteSize bytes.size
    let at_ (px : ByteArray) (x y : Nat) : UInt8 := px.get! ((y * w + x) * 3)
    let share (px : ByteArray) (x0 x1 y0 y1 : Nat) : Float := Id.run do
      let mut n := 0
      for y in [y0:y1] do
        for x in [x0:x1] do
          if at_ px x y == 255 then n := n + 1
      return n.toFloat / ((x1 - x0) * (y1 - y0)).toFloat
    -- Fade mixes: halfway, every channel is half of white.
    let half ← frame "Fade" 0.5
    ensure (half.size == w * h * 3 && half.toList.all (· == 127)) "Fade at 0.5 is half of each page"
    ensure ((← frame "Fade" 0.0).toList.all (· == 0) && (← frame "Fade" 1.0).toList.all (· == 255)) "Fade ends"
    -- Dissolve: about the share of the progress, a growing set, the same every time.
    let third ← frame "Dissolve" 0.3
    let twoThirds ← frame "Dissolve" 0.6
    let revealed := share third 0 w 0 h
    ensure ((revealed - 0.3).abs < 0.03) s!"Dissolve at 0.3 shows {revealed} of the new page"
    ensure ((List.range (w * h)).all fun i => third.get! (i * 3) != 255 || twoThirds.get! (i * 3) == 255)
      "Dissolve only ever adds new pixels"
    ensure ((← frame "Dissolve" 0.3) == third) "Dissolve is the same each time"
    ensure ((← frame "Dissolve" 0.0).toList.all (· == 0) && (← frame "Dissolve" 1.0).toList.all (· == 255))
      "Dissolve ends"
    -- Glitter sweeps along /Di: the side it starts from is through, the far side untouched.
    for (direction, through, untouched) in [(0, (0, 96, 0, h), (864, w, 0, h)),
        (270, (0, w, 0, 54), (0, w, 486, h)), (315, (0, 96, 0, 54), (864, w, 486, h))] do
      let px ← frame "Glitter" 0.5 direction.toUInt32
      let (a0, a1, b0, b1) := through
      let (c0, c1, d0, d1) := untouched
      ensure (share px a0 a1 b0 b1 == 1.0 && share px c0 c1 d0 d1 == 0.0)
        s!"Glitter at {direction}°: {share px a0 a1 b0 b1} through, {share px c0 c1 d0 d1} beyond"
    -- Fly: the page moves along /Di from (to) just offscreen, scaling with /SS.
    let fly (progress : Float) (direction : UInt32) (scale : Float) (flags : UInt8) : IO ByteArray := do
      let out := dir / "fly.ppm"
      Native.transitionFrame old.toString new.toString out.toString "Fly" progress direction scale flags
      let bytes ← IO.FS.readBinFile out
      return bytes.extract header.utf8ByteSize bytes.size
    let inward ← fly 0.5 0 1.0 4
    ensure (at_ inward 240 270 == 255 && at_ inward 720 270 == 0) "Fly in from the left: halfway, the left half"
    let outward ← fly 0.5 270 1.0 5
    ensure (at_ outward 480 135 == 255 && at_ outward 480 405 == 0) "Fly out downwards: halfway, the top half free"
    let small ← fly 0.0 0 0.5 6
    ensure (at_ small 480 270 == 255 && at_ small 5 5 == 0 && at_ small 230 270 == 0 && at_ small 250 270 == 255)
      "Fly with /Di /None from /SS 0.5: the new page at half size in the middle"
    ensure ((← fly 1.0 0 0.5 6).toList.all (· == 255) && (← fly 1.0 270 1.0 5).toList.all (· == 255)) "Fly ends"
    -- Without /B only changes fly: where the pages agree the old one stays.
    let halves (left right : UInt8) : ByteArray := Id.run do
      let mut px : Array UInt8 := Array.mkEmpty (w * h * 3)
      for _ in [0:h] do
        for x in [0:w] do
          let value := if x < w / 2 then left else right
          px := px.push value |>.push value |>.push value
      return header.toUTF8 ++ ⟨px⟩
    IO.FS.writeBinFile old (halves 0 128)
    IO.FS.writeBinFile new (halves 255 128)
    ensure (at_ (← fly 0.5 0 1.0 4) 240 270 == 128 && at_ (← fly 0.5 0 1.0 0) 240 270 == 0)
      "/B true flies the whole page, /B false only what changed"
    -- For an Emacs canvas: native-endian ARGB32 words, opaque, no header.
    IO.FS.writeBinFile old (plain 0)
    IO.FS.writeBinFile new (plain 255)
    Native.transitionFrame old.toString new.toString (dir / "frame.argb").toString "Fade" 0.5 0 1.0 8
    let words ← IO.FS.readBinFile (dir / "frame.argb")
    ensure (words.size == w * h * 4 && (List.range (w * h)).all fun i =>
        words.get! (4 * i) == 127 && words.get! (4 * i + 1) == 127 && words.get! (4 * i + 2) == 127 &&
        words.get! (4 * i + 3) == 255) "a canvas frame: B G R A bytes, half of white"
    -- Refused: other styles, rasters of other sizes.
    let fails := fun (act : IO Unit) => do try act; pure false catch _ => pure true
    ensure (← fails (Native.transitionFrame old.toString new.toString (dir / "x.ppm").toString "Wipe" 0.5 0))
      "only blended styles"
    IO.FS.writeBinFile new ("P6\n2 2\n255\n".toUTF8 ++ ⟨Array.replicate 12 255⟩)
    ensure (← fails (Native.transitionFrame old.toString new.toString (dir / "x.ppm").toString "Fade" 0.5 0))
      "rasters of other sizes"
    -- Through the worker: its own rasters only, and only the last frames kept.
    let app := (← IO.appPath).parent.get! / "vellumory"
    let pdf := dir / "doc.pdf"
    -- Pages of one size, as a transition needs.
    IO.FS.writeFile pdf (Fixtures.longReader 3)
    let child ← IO.Process.spawn {
        cmd := app.toString, args := #["serve"], stdin := .piped, stdout := .piped,
        stderr := .null, env := #[("XDG_RUNTIME_DIR", some dir.toString)] }
    let request (id : Nat) (method : String) (params : List (String × Json)) : IO Json := do
      child.stdin.putStrLn (Json.mkObj [("id", toJson id), ("method", toJson method),
        ("params", Json.mkObj params)]).compress
      child.stdin.flush
      IO.ofExcept (Json.parse (← child.stdout.getLine))
    let path (reply : Json) := ((reply.getObjVal? "result") >>= (·.getObjValAs? String "path")).toOption.getD ""
    let _ ← request 1 "open" [("path", toJson pdf.toString)]
    let first := path (← request 2 "render" [("page", toJson (0 : Nat)), ("width", toJson (300 : Nat)),
      ("top", toJson (0 : Nat)), ("bottom", toJson (388 : Nat))])
    let second := path (← request 3 "render" [("page", toJson (1 : Nat)), ("width", toJson (300 : Nat)),
      ("top", toJson (0 : Nat)), ("bottom", toJson (388 : Nat))])
    let mut frames : Array String := #[]
    for i in [0:6] do
      let reply ← request (10 + i) "transition-frame" [("old", toJson first), ("new", toJson second),
        ("style", toJson "Dissolve"), ("progress", toJson (i.toFloat / 5))]
      frames := frames.push (path reply)
    ensure (frames.all (·.startsWith dir.toString)) s!"frames in the runtime directory: {frames}"
    let kept ← frames.filterM fun f => (System.FilePath.mk f).pathExists
    ensure (kept == frames.extract 2 6) s!"the last four frames kept: {kept}"
    -- Canvas frames of a slot all go to one file, which the canvas rereads.
    let slotted ← [30, 31].mapM fun id => do
      return path (← request id "transition-frame" [("old", toJson first), ("new", toJson second),
        ("style", toJson "Fade"), ("progress", toJson (0.5 : Float)), ("format", toJson "argb"),
        ("slot", toJson (3 : Nat))])
    ensure (slotted.all (·.endsWith "/canvas-frame-3.argb") && slotted[0]! == slotted[1]!)
      s!"canvas frames in their slot's file: {slotted}"
    let outside ← request 20 "transition-frame" [("old", toJson pdf.toString), ("new", toJson second),
      ("style", toJson "Fade"), ("progress", toJson (0.5 : Float))]
    ensure ((outside.getObjValAs? String "error").toOption.isSome) "a file outside the worker's rasters is refused"
    let (stdin, child) ← child.takeStdin
    stdin.flush
    ensure ((← child.wait) == 0) "worker exits cleanly"
  IO.println "PASS transition frames: Fade mixes, Dissolve grows deterministically, Glitter sweeps along /Di, Fly moves and scales (/SS, /Di /None, /B); own rasters only, last frames kept"

def testWorkerDocuments : IO Unit := do
  let app := (← IO.appPath).parent.get! / "vellumory"
  withScratchDir fun dir => do
    let first := dir / "first.pdf"
    let second := dir / "second.pdf"
    IO.FS.writeFile first Fixtures.reader
    IO.FS.writeFile second (Fixtures.longReader 5)
    let child ← IO.Process.spawn {
        cmd := app.toString, args := #["serve"], stdin := .piped, stdout := .piped,
        stderr := .null, env := #[("XDG_RUNTIME_DIR", some dir.toString)] }
    let send (json : Json) := do child.stdin.putStrLn json.compress; child.stdin.flush
    let request (id : Nat) (method : String) (params : List (String × Json)) :=
      send <| Json.mkObj [("id", toJson id), ("method", toJson method), ("params", Json.mkObj params)]
    let replies : IO (Array Json) := do
      let mut out := #[]
      for _ in [0:2] do out := out.push (← IO.ofExcept (Json.parse (← child.stdout.getLine)))
      return out
    let byId (rs : Array Json) (id : Nat) := rs.find? fun r => (r.getObjValAs? Nat "id").toOption == some id
    let pages (r : Option Json) := r.bind fun r => (r.getObjVal? "result" >>= (·.getObjValAs? (Array Json) "pages")).toOption |>.map (·.size)
    request 1 "open" [("path", toJson first.toString), ("doc", toJson (1 : Nat))]
    request 2 "open" [("path", toJson second.toString), ("doc", toJson (2 : Nat))]
    let opened ← replies
    ensure (pages (byId opened 1) == some 3 && pages (byId opened 2) == some 5) "two documents open in one worker"
    request 3 "text" [("page", toJson (0 : Nat)), ("doc", toJson (1 : Nat))]
    request 4 "text" [("page", toJson (4 : Nat)), ("doc", toJson (2 : Nat))]
    let texts ← replies
    let text (id : Nat) := ((byId texts id).bind fun r => (r.getObjVal? "result" >>= (·.getObjValAs? String "text")).toOption).getD ""
    ensure (((text 3).splitOn "Vellumory / 1").length > 1) "the first document answers for doc 1"
    ensure ((byId texts 4).bind fun r => (r.getObjVal? "result").toOption).isSome "the second answers for its page 5"
    request 5 "close" [("doc", toJson (1 : Nat))]
    request 6 "text" [("page", toJson (0 : Nat)), ("doc", toJson (1 : Nat))]
    let closed ← replies
    ensure (((byId closed 6).bind fun r => (r.getObjValAs? String "error").toOption) == some "No document is open")
      "a closed document is gone"
    request 7 "text" [("page", toJson (0 : Nat)), ("doc", toJson (2 : Nat))]
    let _ ← child.stdout.getLine
    let (stdin, child) ← child.takeStdin
    stdin.flush
    ensure ((← child.wait) == 0) "worker exits cleanly"
  IO.println "PASS one worker for many documents: requests by document, close"

def main (args : List String) : IO UInt32 := do
  match args with
  | "--echo-args" :: rest => IO.println (toJson rest).compress; return 0
  | ["--exit-seven"] => return 7
  | ["--echo-context"] =>
    IO.println (toJson #[(← IO.currentDir).toString,
      (← IO.getEnv "VELLUMORY_TEST_VALUE").getD ""]).compress
    return 0
  | _ =>
    try
      testJson
      testGeometry
      testShell
      testPdf
      testCli
      testNative
      testSearch
      testForgetCaches
      testWorkerProtocol
      testWorkerDocuments
      testTransitionFrames
      testContentBox
      testPageReflow
      testCompose
      testReflow
      testRuntimeSweep
      testPreflight
      testPersistentRasters
      testEdit
      testValidateAndOcr
      testDiff
      testSynctex
      testMetadata
      testAssociatedFiles
      testStampAndEncryption
      testAnnotations
      testSignatures
      testForms
      return 0
    catch e =>
      -- On stdout too, after everything already printed: stderr is unbuffered.
      IO.println s!"FAIL: {e}"
      (← IO.getStdout).flush
      IO.eprintln s!"FAIL: {e}"
      return 1
