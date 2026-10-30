import Vellumory.Json
import Vellumory.Native
import Vellumory.Diff

/-! What changed between two versions of a document.

Pages are aligned by their text (so inserted or removed pages do not shift
every later comparison); pages whose text differs are paired and diffed word
by word. Changed words are grouped into phrases that keep their polygons, so
a reader can highlight them. Structural fields (metadata, counts, fonts,
attachments, claims) are compared from both inspection reports. -/

namespace Vellumory.DocDiff

open Diff

structure Phrase where
  words : Array Native.Word

def Phrase.toJson (p : Phrase) : Json :=
  .mkObj [("text", .str (" ".intercalate (p.words.toList.map (·.text)))),
          ("quads", .arr (p.words.map (Vellumory.toJson ·.quad)))]

/-- Group the deleted (or inserted) side of a word script into phrases. -/
def phrases (edits : List (Edit Native.Word)) (deleted : Bool) : Array Phrase := Id.run do
  let mut out : Array Phrase := #[]
  let mut current : Array Native.Word := #[]
  for edit in edits do
    match edit, deleted with
    | .delete w, true | .insert w, false => current := current.push w
    | .keep .., _ =>
      if !current.isEmpty then out := out.push ⟨current⟩; current := #[]
    | _, _ => pure ()
  if !current.isEmpty then out := out.push ⟨current⟩
  return out

def fingerprint (words : Array Native.Word) : String := " ".intercalate (words.toList.map (·.text))

def structural (old new : Json) : Array Json := Id.run do
  let mut changes := #[]
  for field in ["title", "author", "subject", "keywords", "creator", "producer", "created", "modified",
                "version", "format", "encryption", "pages", "font_count", "fonts_embedded", "images",
                "annotations", "links", "form_fields", "tagged", "language", "javascript", "linearized"] do
    let a := old.getObjValD field
    let b := new.getObjValD field
    if a != b then changes := changes.push (.mkObj [("field", .str field), ("old", a), ("new", b)])
  let names (j : Json) : List String :=
    ((j.getObjValAs? (Array Json) "attachments").toOption.getD #[]).toList.filterMap
      fun a => (a.getObjValAs? String "filename").toOption
  let removed := (names old).filter (!(names new).contains ·)
  let added := (names new).filter (!(names old).contains ·)
  unless removed.isEmpty && added.isEmpty do
    changes := changes.push (.mkObj [("field", .str "attachments"),
      ("old", Vellumory.toJson removed), ("new", Vellumory.toJson added)])
  return changes

/-- A page's rotation and boxes from its page report: what it looks like
    apart from its content. -/
def geometry (report : Json) : Json :=
  .mkObj (["rotate", "media_box", "crop_box", "bleed_box", "trim_box", "art_box"].map
    fun field => (field, report.getObjValD field))

def pageChange (oldPage newPage : Option Nat) (oldWords newWords : Array Native.Word)
    (geometryChange : Option (Json × Json) := none) : Json :=
  let edits := diff (·.text) oldWords newWords
  let removed := phrases edits true
  let added := phrases edits false
  .mkObj <| [("old_page", Vellumory.toJson oldPage), ("new_page", Vellumory.toJson newPage),
    ("status", .str (match oldPage, newPage with
      | some _, some _ => "changed" | some _, none => "removed" | _, _ => "added")),
    ("removed_words", Vellumory.toJson (removed.foldl (· + ·.words.size) 0)),
    ("added_words", Vellumory.toJson (added.foldl (· + ·.words.size) 0)),
    ("removed", .arr (removed.map Phrase.toJson)), ("added", .arr (added.map Phrase.toJson))] ++
    (match geometryChange with
     | some (old, new) => [("geometry", .mkObj [("old", old), ("new", new)])]
     | none => [])

def compare (oldPath newPath : String) : IO Json := do
  let open' (path : String) : IO Native.Document := do
    Native.openDocumentAs (← IO.FS.readBinFile path) path ""
  let oldDoc ← open' oldPath
  let newDoc ← open' newPath
  let oldInfo ← IO.ofExcept (Json.parse (← Native.info oldDoc))
  let newInfo ← IO.ofExcept (Json.parse (← Native.info newDoc))
  let words (doc : Native.Document) : IO (Array (Array Native.Word)) := do
    let count := (← Native.pageSizes doc).size
    (List.range count).toArray.mapM fun p => Native.pageWords doc p.toUInt32
  let oldPages ← words oldDoc
  let newPages ← words newDoc
  -- Rotation and boxes: a page turned or cropped is changed though its text is not.
  let shapes (doc : Native.Document) (count : Nat) : IO (Array Json) :=
    (List.range count).toArray.mapM fun p => do
      return geometry (← IO.ofExcept (Json.parse (← Native.pageInfo doc p.toUInt32)))
  let oldShapes ← shapes oldDoc oldPages.size
  let newShapes ← shapes newDoc newPages.size
  let shapeChange (o n : Nat) : Option (Json × Json) :=
    let a := oldShapes[o]!
    let b := newShapes[n]!
    if a == b then none else some (a, b)
  let oldKeys := oldPages.mapIdx fun i w => (i, fingerprint w)
  let newKeys := newPages.mapIdx fun i w => (i, fingerprint w)
  -- Align pages; pair the runs between equal pages positionally.
  let pageEdits := diff (·.2) oldKeys newKeys (budget := 25000000)
  let mut changes : Array Json := #[]
  let mut deleted : Array Nat := #[]
  let mut inserted : Array Nat := #[]
  let flush (deleted inserted : Array Nat) : Array Json := Id.run do
    let mut out := #[]
    for k in [0:max deleted.size inserted.size] do
      let o := deleted[k]?
      let n := inserted[k]?
      let shape := match o, n with | some o, some n => shapeChange o n | _, _ => none
      out := out.push (pageChange o n (o.map (oldPages[·]!) |>.getD #[]) (n.map (newPages[·]!) |>.getD #[]) shape)
    return out
  for edit in pageEdits do
    match edit with
    | .keep (i, _) (j, _) =>
      changes := changes ++ flush deleted inserted
      deleted := #[]; inserted := #[]
      if let some shape := shapeChange i j then
        changes := changes.push (pageChange (some i) (some j) #[] #[] shape)
    | .delete (i, _) => deleted := deleted.push i
    | .insert (j, _) => inserted := inserted.push j
  changes := changes ++ flush deleted inserted
  let total (field : String) := changes.foldl (fun s c => s + ((c.getObjValAs? Nat field).toOption.getD 0)) 0
  let count (status : String) := (changes.filter fun c => (c.getObjValAs? String "status").toOption == some status).size
  return .mkObj [
    ("old", .mkObj [("path", .str oldPath), ("pages", Vellumory.toJson oldPages.size)]),
    ("new", .mkObj [("path", .str newPath), ("pages", Vellumory.toJson newPages.size)]),
    ("structure", .arr (structural oldInfo newInfo)),
    ("summary", .mkObj [("changed_pages", Vellumory.toJson (count "changed")),
      ("added_pages", Vellumory.toJson (count "added")), ("removed_pages", Vellumory.toJson (count "removed")),
      ("added_words", Vellumory.toJson (total "added_words")),
      ("removed_words", Vellumory.toJson (total "removed_words"))]),
    ("pages", .arr changes)]

end Vellumory.DocDiff
