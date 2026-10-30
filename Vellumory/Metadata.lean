import Vellumory.Json
import Vellumory.Native
import Vellumory.Tools

/-! Document metadata edits that keep the Info dictionary, the catalog
(`/Lang`, `/ViewerPreferences`) and the XMP packet consistent.

PDF/A requires every Info entry to equal its XMP counterpart (ISO 19005-1
6.7.3, ISO 19005-2/3 6.6.2.3); PDF/UA-1 requires `dc:title`, a document
language and `DisplayDocTitle` (7.1, 7.2). qpdf reads the objects involved as
qpdf JSON and writes the update, leaving every other object as it was; Adobe's
XMP Toolkit (through Exempi) rewrites the packet, keeping every property it
is not asked to change, including PDF/A and PDF/UA identification. -/

namespace Vellumory.Metadata

/-- Editable fields. Text fields map to Info keys; `language` is the
    catalog's `/Lang`; `display-title` is `/ViewerPreferences /DisplayDocTitle`;
    `created` is an ISO 8601 date. -/
def fields : List String :=
  ["title", "author", "subject", "keywords", "creator", "producer", "language", "display-title", "created"]

def infoKey : String → Option String
  | "title" => some "/Title" | "author" => some "/Author" | "subject" => some "/Subject"
  | "keywords" => some "/Keywords" | "creator" => some "/Creator" | "producer" => some "/Producer"
  | "created" => some "/CreationDate" | "modified" => some "/ModDate"
  | _ => none

/-! ## Dates -/

private def digits (cs : List Char) (n : Nat) : Option (String × List Char) :=
  let taken := cs.take n
  if taken.length == n && taken.all Char.isDigit then some (String.ofList taken, cs.drop n) else none

/-- A PDF date (`D:YYYY[MM[DD[HH[mm[SS[O[HH['mm[']]]]]]]]]`, ISO 32000-2
    7.9.4) as an XMP date (ISO 8601 profile), or none if it does not parse.
    Missing month and day default to 01 per the PDF rules; a time without
    minutes gets :00; no offset means none is stated. -/
def isoDate? (pdf : String) : Option String := do
  let cs := pdf.toList
  let cs := if cs.take 2 == ['D', ':'] then cs.drop 2 else cs
  let (year, cs) ← digits cs 4
  let field := fun (cs : List Char) (default : String) =>
    match digits cs 2 with
    | some (v, rest) => (v, rest, true)
    | none => (default, cs, false)
  let (month, cs, _) := field cs "01"
  let (day, cs, _) := field cs "01"
  let (hour, cs, hasTime) := field cs "00"
  let (minute, cs, _) := field cs "00"
  let (second, cs, hasSeconds) := field cs "00"
  let date := s!"{year}-{month}-{day}"
  if !hasTime then return date
  let time := s!"{date}T{hour}:{minute}" ++ (if hasSeconds then s!":{second}" else "")
  match cs with
  | [] => return time
  | 'Z' :: _ => return time ++ "Z"
  | sign :: rest =>
    unless sign == '+' || sign == '-' do none
    let (oh, rest) ← digits rest 2
    let rest := if rest.head? == some '\'' then rest.tail else rest
    let om := (digits rest 2).map (·.1) |>.getD "00"
    if oh == "00" && om == "00" then return time ++ "Z"
    return time ++ s!"{sign}{oh}:{om}"

/-- An ISO 8601 date or date-time (`YYYY-MM-DD[Thh:mm[:ss[.f]][Z|±hh:mm]]`)
    as a PDF date, or none if it does not parse. -/
def pdfDate? (iso : String) : Option String := do
  let cs := iso.toList
  let (year, cs) ← digits cs 4
  let cs ← if cs.head? == some '-' then some cs.tail else none
  let (month, cs) ← digits cs 2
  let cs ← if cs.head? == some '-' then some cs.tail else none
  let (day, cs) ← digits cs 2
  let date := s!"D:{year}{month}{day}"
  match cs with
  | [] => return date
  | 'T' :: cs =>
    let (hour, cs) ← digits cs 2
    let cs ← if cs.head? == some ':' then some cs.tail else none
    let (minute, cs) ← digits cs 2
    let (second, cs) ← if cs.head? == some ':' then digits cs.tail 2 else some ("00", cs)
    -- Fractions of a second have no PDF form.
    let cs := if cs.head? == some '.' then (cs.tail.dropWhile Char.isDigit) else cs
    let time := s!"{date}{hour}{minute}{second}"
    match cs with
    | [] => return time
    | ['Z'] => return time ++ "Z"
    | [sign, h1, h2, ':', m1, m2] =>
      unless (sign == '+' || sign == '-') && [h1, h2, m1, m2].all Char.isDigit do none
      return time ++ s!"{sign}{h1}{h2}'{m1}{m2}'"
    | _ => none
  | _ => none

/-! ## Validation -/

/-- BCP 47 shape: alphanumeric subtags of 1–8 characters, the first alphabetic. -/
def languageTag? (tag : String) : Bool :=
  let parts := tag.splitOn "-"
  !tag.isEmpty && parts.all (fun p => 1 ≤ p.length && p.length ≤ 8 && p.all Char.isAlphanum) &&
    (parts.head?.map (·.all Char.isAlpha) |>.getD false)

def validate (set : Array (String × String)) (remove : Array String) (date : String) : Except String Unit := do
  if set.isEmpty && remove.isEmpty then throw "no metadata to change"
  for (field, value) in set do
    unless fields.contains field do throw s!"unknown metadata field {field}"
    if value.isEmpty then throw s!"{field} is empty; remove it instead"
    if field == "language" && !languageTag? value then throw s!"{value} is not a language tag such as en-GB"
    if field == "display-title" && !(value == "true" || value == "false") then
      throw "display-title is true or false"
    if field == "created" && (pdfDate? value).isNone then throw s!"{value} is not an ISO 8601 date"
  for field in remove do
    unless fields.contains field do throw s!"unknown metadata field {field}"
    if set.any (·.1 == field) then throw s!"{field} is both set and removed"
  unless date.isEmpty || (pdfDate? date).isSome do throw s!"{date} is not an ISO 8601 date"

/-! ## qpdf JSON -/

private def ref? (j : Json) : Option (Nat × Nat) :=
  match j with
  | .str s => match s.splitOn " " with
    | [n, g, "R"] => do return (← n.toNat?, ← g.toNat?)
    | _ => none
  | _ => none

private def refKey (r : Nat × Nat) : String := s!"obj:{r.1} {r.2} R"
private def refValue (r : Nat × Nat) : Json := .str s!"{r.1} {r.2} R"

private def fieldsOf : Json → Array (String × Json)
  | .obj fs => fs
  | _ => #[]

private def put (fs : Array (String × Json)) (key : String) (value : Option Json) : Array (String × Json) :=
  let kept := fs.filter (·.1 != key)
  match value with
  | some v => kept.push (key, v)
  | none => kept

/-- The text of a qpdf JSON string value (`u:` prefix); none for binary strings. -/
private def text? : Json → Option String
  | .str s => if s.startsWith "u:" then some (s.drop 2).toString else none
  | _ => none

private def qpdfJson (args : Array String) : IO (Json × Json) := do
  let out ← IO.Process.output { cmd := Tools.qpdf, args := #["--json-output", "--json-key=qpdf"] ++ args }
  unless out.exitCode == 0 || out.exitCode == 3 do
    throw <| IO.userError s!"qpdf failed ({out.exitCode}): {out.stderr.trimAscii}"
  let json ← IO.ofExcept (Json.parse out.stdout)
  match json.getObjValD "qpdf" with
  | .arr #[header, objects] => return (header, objects)
  | _ => throw <| IO.userError "qpdf JSON without header and objects"

/-- Decoded stream data, as bytes. -/
private def streamData (input : String) (r : Nat × Nat) : IO ByteArray := do
  let child ← IO.Process.spawn {
    cmd := Tools.qpdf, stdout := .piped, stderr := .piped
    args := #[s!"--show-object={r.1},{r.2}", "--filtered-stream-data", input] }
  let bytes ← child.stdout.readBinToEnd
  let errors ← child.stderr.readToEnd
  let code ← child.wait
  unless code == 0 || code == 3 do throw <| IO.userError s!"qpdf failed ({code}): {errors.trimAscii}"
  return bytes

/-- The document's XMP packet (empty if it has none). -/
def packet (input : String) : IO ByteArray := do
  let (_, objects) ← qpdfJson #["--json-object=trailer", input, "-"]
  let trailer := (objects.getObjValD "trailer").getObjValD "value"
  let some root := ref? (trailer.getObjValD "/Root") | return ByteArray.empty
  let (_, objects) ← qpdfJson #[s!"--json-object={root.1},{root.2}", "--json-stream-data=none", input, "-"]
  match ref? (((objects.getObjValD (refKey root)).getObjValD "value").getObjValD "/Metadata") with
  | some r => streamData input r
  | none => return ByteArray.empty

/-- The PDF/A part the document claims in its XMP identification schema. -/
def pdfaPart (input : String) : IO (Option Nat) := do
  try
    let value ← Native.xmpProperty (← packet input) "http://www.aiim.org/pdfa/ns/id/" "part"
    return value.bind String.toNat?
  catch _ => return none

/-- Write the qpdf update to `json` (and the new XMP packet to `data`) that
    applies SET and REMOVE to `input`; DATE (ISO 8601, may be empty) becomes
    the modification and metadata date. Unedited Info text fields and dates are
    synchronized into the XMP packet as well, so a document leaves consistent. -/
def prepare (input data json : String) (set : Array (String × String)) (remove : Array String)
    (date : String) : IO Unit := do
  let (header, objects) ← qpdfJson #["--json-object=trailer", input, "-"]
  let trailer := (objects.getObjValD "trailer").getObjValD "value"
  if let .ok _ := trailer.getObjVal? "/Encrypt" then
    throw <| IO.userError "the document is encrypted; remove encryption first"
  let some root := ref? (trailer.getObjValD "/Root") | throw <| IO.userError "the trailer has no catalog"
  let infoRef := ref? (trailer.getObjValD "/Info")
  let maxObject := match header.getObjValD "maxobjectid" with | .num n => n.toUInt64.toNat | _ => 0
  let size := match trailer.getObjValD "/Size" with | .num n => n.toUInt64.toNat | _ => 0
  let selection := #[s!"--json-object={root.1},{root.2}"] ++
    (infoRef.map (fun r => #[s!"--json-object={r.1},{r.2}"]) |>.getD #[])
  let (_, objects) ← qpdfJson (selection ++ #["--json-stream-data=none", input, "-"])
  let catalog := fieldsOf ((objects.getObjValD (refKey root)).getObjValD "value")
  -- A direct Info dictionary becomes an object of its own.
  let info := match infoRef with
    | some r => fieldsOf ((objects.getObjValD (refKey r)).getObjValD "value")
    | none => fieldsOf (trailer.getObjValD "/Info")
  let lookup := fun (fs : Array (String × Json)) (key : String) => (fs.find? (·.1 == key)).map (·.2)
  let metadataRef := (lookup catalog "/Metadata").bind ref?
  let preferences := lookup catalog "/ViewerPreferences"
  let preferencesRef := preferences.bind ref?
  let extra := (metadataRef.map (fun r => #[s!"--json-object={r.1},{r.2}"]) |>.getD #[]) ++
    (preferencesRef.map (fun r => #[s!"--json-object={r.1},{r.2}"]) |>.getD #[])
  let (_, more) ← if extra.isEmpty then pure (Json.null, Json.null)
    else qpdfJson (extra ++ #["--json-stream-data=none", input, "-"])
  let oldPacket ← match metadataRef with
    | some r => streamData input r
    | none => pure ByteArray.empty
  -- PDF 2.0 deprecates the Info dictionary: it is never created there, only
  -- kept in step where it exists. PDF/A-4 allows at most ModDate in it
  -- (ISO 19005-4 6.1.3; veraPDF failed a full Info dictionary).
  let modern := match header.getObjValD "pdfversion" with | .str v => v.startsWith "2" | _ => false
  let part := (← Native.xmpProperty oldPacket "http://www.aiim.org/pdfa/ns/id/" "part").bind String.toNat?
  let hasInfo := infoRef.isSome || !(fieldsOf (trailer.getObjValD "/Info")).isEmpty
  let writeInfo := part != some 4 && (hasInfo || !modern)
  let onlyModDate := part == some 4 && infoRef.isSome
  -- New objects are numbered after the highest existing one.
  let mut next := maxObject + 1
  let infoId := infoRef.getD (next, 0)
  if writeInfo && infoRef.isNone then next := next + 1
  let metadataId := metadataRef.getD (next, 0)
  if metadataRef.isNone then next := next + 1
  -- Info dictionary.
  let mut info := info
  if writeInfo then
    for (field, value) in set do
      if let some key := infoKey field then
        let value := if field == "created" then (pdfDate? value).getD value else value
        info := put info key (some (.str s!"u:{value}"))
    for field in remove do
      if let some key := infoKey field then info := put info key none
  if (writeInfo || onlyModDate) && !date.isEmpty then
    info := put info "/ModDate" (some (.str s!"u:{(pdfDate? date).getD date}"))
  -- Catalog: language, XMP reference, viewer preferences.
  let mut catalog := catalog
  let mut preferenceObject : Option (Array (String × Json)) := none
  if let some language := (set.find? (·.1 == "language")).map (·.2) then
    catalog := put catalog "/Lang" (some (.str s!"u:{language}"))
  if remove.contains "language" then catalog := put catalog "/Lang" none
  catalog := put catalog "/Metadata" (some (refValue metadataId))
  let display := (set.find? (·.1 == "display-title")).map (·.2 == "true")
  if display.isSome || remove.contains "display-title" then
    let current := match preferencesRef with
      | some r => fieldsOf ((more.getObjValD (refKey r)).getObjValD "value")
      | none => fieldsOf (preferences.getD (.obj #[]))
    let updated := put current "/DisplayDocTitle" (display.map Json.bool)
    match preferencesRef with
    | some _ => preferenceObject := some updated
    | none => catalog := put catalog "/ViewerPreferences" (some (.obj updated))
  -- XMP: with an Info dictionary, every text field and date it now holds (so
  -- the two agree); without one, the edits themselves. Removals in both cases.
  let mut xmpFields : Array String := #[]
  let mut xmpValues : Array String := #[]
  for field in ["title", "author", "subject", "keywords", "creator", "producer", "created", "modified"] do
    let some key := infoKey field | continue
    let edited := if field == "modified" then (if date.isEmpty then none else some date)
      else (set.find? (·.1 == field)).map (·.2)
    let value := if writeInfo then (lookup info key).bind text? |>.bind fun v =>
        if field == "created" || field == "modified" then isoDate? v else some v
      else edited
    match value with
    | some value => xmpFields := xmpFields.push field; xmpValues := xmpValues.push value
    | none =>
      if remove.contains field then
        xmpFields := xmpFields.push field; xmpValues := xmpValues.push ""
  match (lookup catalog "/Lang").bind text? with
  | some language => xmpFields := xmpFields.push "language"; xmpValues := xmpValues.push language
  | none => if remove.contains "language" then
      xmpFields := xmpFields.push "language"; xmpValues := xmpValues.push ""
  if !date.isEmpty then
    xmpFields := xmpFields.push "metadata-date"; xmpValues := xmpValues.push date
  let packet ← Native.xmpUpdate oldPacket xmpFields xmpValues
  IO.FS.writeBinFile data packet
  -- The metadata stream keeps its other keys; the packet is written unfiltered.
  let streamDict := match metadataRef with
    | some r => (fieldsOf (((more.getObjValD (refKey r)).getObjValD "stream").getObjValD "dict")).filter
        fun (k, _) => !["/Filter", "/DecodeParms", "/Length", "/DL"].contains k
    | none => #[]
  let streamDict := put (put streamDict "/Type" (some (.str "/Metadata"))) "/Subtype" (some (.str "/XML"))
  let mut update : Array (String × Json) := #[
    (refKey root, .mkObj [("value", .obj catalog)]),
    (refKey metadataId, .mkObj [("stream", .mkObj [("dict", .obj streamDict), ("datafile", .str data)])])]
  if writeInfo || onlyModDate then
    update := update.push (refKey infoId, .mkObj [("value", .obj info)])
  if let (some r, some prefs) := (preferencesRef, preferenceObject) then
    update := update.push (refKey r, .mkObj [("value", .obj prefs)])
  if writeInfo && infoRef.isNone then
    let trailer := put (put (fieldsOf trailer) "/Info" (some (refValue infoId)))
      "/Size" (some (.num (max size next).toFloat))
    update := update.push ("trailer", .mkObj [("value", .obj trailer)])
  IO.FS.writeFile json (Json.mkObj [("qpdf", .arr #[.mkObj [("jsonversion", .num 2)], .obj update])]).compress


/-- The PDF/A conformance level the document claims (A, B, U, E, F); none for
    base PDF/A-4, which has no level. -/
def pdfaConformance (input : String) : IO (Option String) := do
  try
    return (← Native.xmpProperty (← packet input) "http://www.aiim.org/pdfa/ns/id/" "conformance").map String.toUpper
  catch _ => return none

/-- Refuse to attach FILE where INPUT's PDF/A claim forbids it: PDF/A-1 allows
    no embedded files (ISO 19005-1 6.1.11), PDF/A-2 only PDF/A-1 and -2 files
    (ISO 19005-2 6.8), base PDF/A-4 only PDF/A-1, -2 and -4 files (veraPDF rule
    6.9-3; PDF/A-4e and -4f allow any). -/
def checkAttachable (input file : String) : IO Unit := do
  let name := (System.FilePath.mk file).fileName.getD file
  match ← pdfaPart input with
  | some 1 => throw <| IO.userError "PDF/A-1 allows no attached files; the document would no longer be PDF/A-1"
  | some 2 =>
    unless [some 1, some 2].contains (← pdfaPart file) do
      throw <| IO.userError s!"PDF/A-2 allows only PDF/A-1 and -2 attachments, and {name} is neither"
  | some 4 =>
    if (← pdfaConformance input).isNone && ![some 1, some 2, some 4].contains (← pdfaPart file) then
      throw <| IO.userError s!"PDF/A-4 allows only PDF/A attachments (PDF/A-4f any), and {name} is none"
  | _ => pure ()

/-- Whether INPUT keeps objects in object streams. -/
def hasObjectStreams (input : String) : IO Bool := do
  let out ← IO.Process.output { cmd := Tools.qpdf, args := #["--show-xref", input] }
  unless out.exitCode == 0 || out.exitCode == 3 do
    throw <| IO.userError s!"qpdf failed ({out.exitCode}): {out.stderr.trimAscii}"
  return (out.stdout.splitOn ": compressed;").length > 1

/-- The file specification of attachment KEY in INPUT, if it has one. -/
private def attachmentSpec (input key : String) : IO (Option (Nat × Nat)) := do
  let out ← IO.Process.output { cmd := Tools.qpdf, args := #["--json-output", "--json-key=attachments", input, "-"] }
  unless out.exitCode == 0 || out.exitCode == 3 do
    throw <| IO.userError s!"qpdf failed ({out.exitCode}): {out.stderr.trimAscii}"
  let attachments := (← IO.ofExcept (Json.parse out.stdout)).getObjValD "attachments"
  return ref? ((attachments.getObjValD key).getObjValD "filespec")

/-- The qpdf JSON update of INPUT, written to JSON, that takes DROP out of the
    document's associated files (the catalog's /AF) and puts ADD in, its file
    specification naming RELATIONSHIP. Nothing to change writes an empty update. -/
private def updateAssociated (input json : String) (drop add : Option (Nat × Nat)) (relationship : String) :
    IO Unit := do
  let (_, trailerObjects) ← qpdfJson #["--json-object=trailer", input, "-"]
  let trailer := (trailerObjects.getObjValD "trailer").getObjValD "value"
  let some root := ref? (trailer.getObjValD "/Root") | throw <| IO.userError "the trailer has no catalog"
  let selection := #[s!"--json-object={root.1},{root.2}"] ++
    (add.map (fun s => #[s!"--json-object={s.1},{s.2}"]) |>.getD #[])
  let (_, objects) ← qpdfJson (selection ++ #["--json-stream-data=none", input, "-"])
  let catalog := fieldsOf ((objects.getObjValD (refKey root)).getObjValD "value")
  -- /AF may be an array object of its own (LaTeX writes one): it is read and
  -- updated there, keeping every other file it lists.
  let af := (catalog.find? (·.1 == "/AF")).map (·.2)
  let afRef := af.bind ref?
  let afObject ← match afRef with
    | some r => do
      let (_, found) ← qpdfJson #[s!"--json-object={r.1},{r.2}", "--json-stream-data=none", input, "-"]
      pure ((found.getObjValD (refKey r)).getObjValD "value")
    | none => pure (af.getD .null)
  let listed := match afObject with
    | .arr files => files
    | _ => #[]
  let kept := match drop with
    | some d => listed.filter (· != refValue d)
    | none => listed
  let files := match add with
    | some s => if kept.contains (refValue s) then kept else kept.push (refValue s)
    | none => kept
  let mut update : Array (String × Json) := #[]
  if files != listed then
    update := update.push <| match afRef with
      | some r => (refKey r, .mkObj [("value", .arr files)])
      | none => (refKey root,
        .mkObj [("value", .obj (put catalog "/AF" (if files.isEmpty then none else some (.arr files))))])
  if let some s := add then
    let filespec := fieldsOf ((objects.getObjValD (refKey s)).getObjValD "value")
    update := update.push (refKey s,
      .mkObj [("value", .obj (put filespec "/AFRelationship" (some (.str s!"/{relationship}"))))])
  IO.FS.writeFile json (Json.mkObj [("qpdf", .arr #[.mkObj [("jsonversion", .num 2)], .obj update])]).compress

/-- Before attachment KEY of INPUT is replaced or removed: the update (to JSON)
    that takes it out of the document's associated files. qpdf otherwise keeps
    a replaced file reachable through /AF, and a removed one becomes null there. -/
def dissociate (input key json : String) : IO Unit := do
  updateAssociated input json (← attachmentSpec input key) none ""

/-- After attachment KEY was added to INPUT: the update (to JSON) that makes it an
    associated file of the document (PDF/A-3, PDF/A-4f) with RELATIONSHIP; without
    one, an empty update. -/
def associate (input key relationship json : String) : IO Unit := do
  if relationship.isEmpty then return (← updateAssociated input json none none "")
  let some spec ← attachmentSpec input key | throw <| IO.userError s!"no attachment {key}"
  updateAssociated input json none (some spec) relationship

end Vellumory.Metadata
