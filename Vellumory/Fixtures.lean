
namespace Vellumory.Fixtures

/-- Test data only, deliberately independent of the renderer. -/
def document (objects : Array String) : String := Id.run do
  let mut text := "%PDF-1.7\n"
  let mut offsets : Array Nat := #[]
  for i in [0:objects.size] do
    offsets := offsets.push text.utf8ByteSize
    text := text ++ s!"{i + 1} 0 obj\n{objects[i]!}\nendobj\n"
  let start := text.utf8ByteSize
  text := text ++ s!"xref\n0 {objects.size + 1}\n0000000000 65535 f \n"
  for offset in offsets do
    let digits := toString offset
    text := text ++ String.ofList (List.replicate (10 - digits.length) '0') ++ digits ++ " 00000 n \n"
  return text ++ s!"trailer\n<< /Size {objects.size + 1} /Root 1 0 R >>\nstartxref\n{start}\n%%EOF\n"

def stream (text : String) : String :=
  s!"<< /Length {text.utf8ByteSize} >>\nstream\n{text}endstream"

def content (number : Nat) (diagonal := true) : String := Id.run do
  let mut text := "0.06 0.12 0.22 rg 0 640 612 152 re f\n"
  text := text ++ s!"1 1 1 rg BT /F1 32 Tf 42 722 Td (Vellumory / {number}) Tj ET\n"
  text := text ++ "BT /F1 14 Tf 42 682 Td (Continuous reading, one viewport at a time.) Tj ET\n"
  for i in [0:17] do
    let y := 600 - i * 30
    text := text ++ s!"0.12 0.19 0.28 rg BT /F1 12 Tf 42 {y} Td (Line {i + 1}: text and shapes cross raster strip boundaries.) Tj ET\n"
    text := text ++ s!"0.1 0.6 0.65 rg 550 {y - 3} 20 18 re f\n"
  if diagonal then text := text ++ "0.9 0.3 0.1 RG 2.5 w 22 22 m 590 620 l S\n"
  return text

def reader (diagonal := true) (catalog := "") : String :=
  document #[
    s!"<< /Type /Catalog {catalog}/Pages 2 0 R /Outlines 10 0 R /PageLabels << /Nums [0 << /S /r >> 1 << /S /D /P (Ch-) /St 1 >>] >> >>",
    "<< /Type /Pages /Kids [3 0 R 6 0 R 8 0 R] /Count 3 >>",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 4 0 R >> >> /Contents 5 0 R /Annots [16 0 R 17 0 R] >>",
    "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>",
    stream (content 1 diagonal),
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Rotate 90 /Resources << /Font << /F1 4 0 R >> >> /Contents 7 0 R >>",
    stream (content 2 diagonal),
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /CropBox [22 22 590 770] /Resources << /Font << /F1 4 0 R >> >> /Contents 9 0 R >>",
    stream (content 3 diagonal),
    "<< /Type /Outlines /First 11 0 R /Last 14 0 R /Count 4 >>",
    "<< /Title (Reading) /Parent 10 0 R /Dest [3 0 R /XYZ 0 600 null] /First 12 0 R /Last 13 0 R /Next 14 0 R /Count 2 >>",
    "<< /Title (Geometry) /Parent 11 0 R /Dest [6 0 R /Fit] /Next 13 0 R >>",
    "<< /Title (Geometry) /Parent 11 0 R /Dest [8 0 R /XYZ 22 650 null] /Prev 12 0 R >>",
    "<< /Title (Resources) /Parent 10 0 R /Prev 11 0 R /First 15 0 R /Last 15 0 R /Count -1 >>",
    "<< /Title (Lean website) /Parent 14 0 R /A << /S /URI /URI (https://lean-lang.org/) >> >>",
    "<< /Type /Annot /Subtype /Link /Rect [40 715 300 760] /Border [0 0 0] /Dest [8 0 R /XYZ 22 650 null] >>",
    "<< /Type /Annot /Subtype /Link /Rect [40 670 400 700] /Border [0 0 0] /A << /S /URI /URI (https://example.org/vellumory) >> >>"]

/-- A many-page document for checking viewport work and sidebar navigation. -/
def longReader (count := 80) : String := Id.run do
  let kids := String.intercalate " " ((List.range count).map fun i => s!"{4 + 2*i} 0 R")
  let mut objects := #[
    "<< /Type /Catalog /Pages 2 0 R >>",
    s!"<< /Type /Pages /Kids [{kids}] /Count {count} >>",
    "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"]
  for i in [0:count] do
    objects := objects.push s!"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 3 0 R >> >> /Contents {5 + 2*i} 0 R >>"
    objects := objects.push (stream (content (i+1)))
  return document objects

def bigEndian (bytes : Nat) (n : Nat) : ByteArray :=
  ⟨((List.range bytes).reverse.map fun i => ((n >>> (8 * i)) % 256).toUInt8).toArray⟩

def littleEndian (bytes : Nat) (n : Nat) : ByteArray :=
  ⟨((List.range bytes).map fun i => ((n >>> (8 * i)) % 256).toUInt8).toArray⟩

/-- A MOBI book (a Palm database, type BOOKMOBI) of one uncompressed UTF-8
    text record holding HTML, as https://wiki.mobileread.com/wiki/MOBI lays
    it out; at most 4096 bytes of HTML. -/
def mobi (html : String) : ByteArray := Id.run do
  let text := html.toUTF8
  -- PalmDOC header: no compression, the text's length, one text record of
  -- up to 4096 bytes; then the MOBI header naming UTF-8 (65001).
  let header := bigEndian 2 1 ++ bigEndian 2 0 ++ bigEndian 4 text.size ++ bigEndian 2 1 ++ bigEndian 2 4096 ++
    bigEndian 4 0 ++ "MOBI".toUTF8 ++ bigEndian 4 16 ++ bigEndian 4 2 ++ bigEndian 4 65001
  let first := 78 + 2 * 8 + 2
  let name := "Vellumory".toUTF8 ++ ⟨Array.replicate (32 - 9) 0⟩
  return name ++ ⟨Array.replicate 28 0⟩ ++ "BOOKMOBI".toUTF8 ++ bigEndian 8 0 ++ bigEndian 2 2 ++
    bigEndian 4 first ++ bigEndian 4 0 ++ bigEndian 4 (first + header.size) ++ bigEndian 4 1 ++
    bigEndian 2 0 ++ header ++ text

/-- CRC-32 as zip uses it (ISO 3309, reflected, polynomial 0xEDB88320). -/
def crc32 (data : ByteArray) : UInt32 := Id.run do
  let mut crc : UInt32 := 0xFFFFFFFF
  for byte in data do
    crc := crc ^^^ byte.toUInt32
    for _ in [0:8] do
      crc := if crc &&& 1 == 1 then (crc >>> 1) ^^^ 0xEDB88320 else crc >>> 1
  return crc ^^^ 0xFFFFFFFF

/-- A zip archive of FILES (name, contents), stored without compression. -/
def storedZip (files : Array (String × String)) : ByteArray := Id.run do
  let mut out := ByteArray.empty
  let mut central := ByteArray.empty
  for (name, contents) in files do
    let data := contents.toUTF8
    let crc := (crc32 data).toNat
    -- Version 2.0, no flags, stored, no date; sizes and the name's length.
    let common := littleEndian 2 20 ++ littleEndian 2 0 ++ littleEndian 2 0 ++ littleEndian 4 0 ++
      littleEndian 4 crc ++ littleEndian 4 data.size ++ littleEndian 4 data.size ++
      littleEndian 2 name.utf8ByteSize ++ littleEndian 2 0
    central := central ++ littleEndian 4 0x02014b50 ++ littleEndian 2 20 ++ common ++ littleEndian 2 0 ++
      littleEndian 2 0 ++ littleEndian 2 0 ++ littleEndian 4 0 ++ littleEndian 4 out.size ++ name.toUTF8
    out := out ++ littleEndian 4 0x04034b50 ++ common ++ name.toUTF8 ++ data
  return out ++ central ++ littleEndian 4 0x06054b50 ++ littleEndian 4 0 ++ littleEndian 2 files.size ++
    littleEndian 2 files.size ++ littleEndian 4 central.size ++ littleEndian 4 out.size ++ littleEndian 2 0

/-- A Word document (Office Open XML) of PARAGRAPHS. -/
def docx (paragraphs : Array String) : ByteArray :=
  let body := String.join (paragraphs.toList.map fun p => s!"<w:p><w:r><w:t>{p}</w:t></w:r></w:p>")
  storedZip #[
    ("[Content_Types].xml", "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"><Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/><Default Extension=\"xml\" ContentType=\"application/xml\"/><Override PartName=\"/word/document.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml\"/></Types>"),
    ("_rels/.rels", "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"word/document.xml\"/></Relationships>"),
    ("word/_rels/document.xml.rels", "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"/>"),
    ("word/document.xml", s!"<?xml version=\"1.0\" encoding=\"UTF-8\"?><w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:body>{body}</w:body></w:document>")]

end Vellumory.Fixtures
