import Vellumory.Native

/-! # Preflight: checks beyond veraPDF

veraPDF judges conformance to PDF/A and PDF/UA. These checks report what
makes any PDF break, leak or surprise its readers, conformance claimed or not:
links and contents that lead nowhere, earlier content still inside the file,
weak encryption, scripts, fonts that are not embedded, page boxes outside the
page, a damaged cross-reference table, and the accessibility basics. -/

namespace Vellumory.Preflight

open Vellumory

structure Finding where
  /-- `error` (something is broken), `warning` (a risk) or `note`. -/
  severity : String
  code : String
  message : String
  pages : Array Nat := #[]
  deriving Inhabited

def finding (severity code message : String) (pages : Array Nat := #[]) : Finding :=
  { severity, code, message, pages }

instance : ToJson Finding where
  toJson f := .mkObj [("severity", toJson f.severity), ("code", toJson f.code),
    ("message", toJson f.message), ("pages", toJson (f.pages.map (· + 1)))]

def bool (json : Json) (key : String) : Bool := (json.getObjValAs? Bool key).toOption.getD false
def nat (json : Json) (key : String) : Nat := (json.getObjValAs? Nat key).toOption.getD 0
def str (json : Json) (key : String) : String := (json.getObjValAs? String key).toOption.getD ""

/-- A box as [x0, y0, x1, y1], if present. -/
def box (json : Json) (key : String) : Option (Array Float) :=
  match (json.getObjValAs? (Array Float) key).toOption with
  | some b => if b.size == 4 then some b else none
  | none => none

/-- Whether box B lies within box M (a point of slack for rounding). -/
def within (b m : Array Float) : Bool :=
  b[0]! + 1.0 ≥ m[0]! && b[1]! + 1.0 ≥ m[1]! && b[2]! ≤ m[2]! + 1.0 && b[3]! ≤ m[3]! + 1.0

/-- The findings that the document-wide report alone shows. -/
def fromInfo (info : Json) (signed : Nat) : Array Finding := Id.run do
  let mut out : Array Finding := #[]
  if bool info "repaired" then
    out := out.push (finding "error" "damaged-xref"
      ("The cross-reference table is damaged; readers repair it differently. Rewriting the file (e c) fixes it."))
  let encryption := str info "encryption"
  if (encryption.splitOn "RC4").length > 1 || (encryption.splitOn "40-bit").length > 1 then
    out := out.push (finding (if (encryption.splitOn "40-bit").length > 1 then "error" else "warning") "weak-encryption"
      (s!"Weak encryption ({encryption}): RC4 can be broken; AES-256 (e E) protects it."))
  let revisions := nat info "revisions"
  if revisions > 1 && signed == 0 then
    out := out.push (finding "warning" "earlier-versions"
      (s!"{revisions - 1} incremental update(s): earlier versions of the content are still inside the file and can be recovered. Rewriting it (e c) removes them."))
  if bool info "javascript" then
    out := out.push (finding "warning" "javascript"
      ("Contains JavaScript. Vellumory never runs it; other readers may."))
  if bool info "xfa" then
    out := out.push (finding "warning" "xfa"
      ("An XFA form: most readers cannot show it, and PDF 2.0 removed XFA."))
  let fonts := (info.getObjValAs? (Array Json) "fonts").toOption.getD #[]
  let unembedded := (fonts.filter fun f => !bool f "embedded").map (str · "name")
  unless unembedded.isEmpty do
    out := out.push (finding "warning" "fonts-not-embedded"
      (s!"Fonts not embedded: {", ".intercalate (unembedded.toList.eraseDups)}. Text may look different on other systems; PDF/A requires embedding."))
  -- Composite (Type 0) fonts map codes to glyphs, not to letters: without
  -- ToUnicode their text cannot be recovered. Simple fonts fall back on glyph names.
  let noUnicode := (fonts.filter fun f => str f "type" == "Type0" && !bool f "to_unicode").map (str · "name")
  unless noUnicode.isEmpty do
    out := out.push (finding "note" "fonts-without-unicode"
      (s!"Composite fonts without a ToUnicode map: {", ".intercalate (noUnicode.toList.eraseDups)}. Their text cannot be copied or searched reliably."))
  if (str info "title").isEmpty then
    out := out.push (finding "note" "no-title"
      ("No title: windows and lists show the file name (PDF/UA requires one; e i sets it)."))
  if (str info "language").isEmpty then
    out := out.push (finding "note" "no-language"
      ("No document language: screen readers guess how to pronounce it (e i sets it)."))
  if !bool info "tagged" then
    out := out.push (finding "note" "untagged"
      ("Not tagged: no reading order or structure for assistive technology."))
  if nat info "bytes" > 10485760 && !bool info "linearized" then
    out := out.push (finding "note" "not-linearized"
      ("Larger than 10 MB and not linearized: on the web it shows only once fully downloaded (e L)."))
  return out

/-- All findings for an open document. -/
def check (doc : Native.Document) : IO (Array Finding) := do
  let info ← IO.ofExcept (Json.parse (← Native.info doc))
  let pages := nat info "pages"
  let mut out := fromInfo info (← Native.signedCount doc).toNat
  let mut outside : Array Nat := #[]
  let mut sizes : Array (Float × Float) := #[]
  for page in [0:pages] do
    let pageInfo ← IO.ofExcept (Json.parse (← Native.pageInfo doc page.toUInt32))
    if let some media := box pageInfo "media_box" then
      let size := (media[2]! - media[0]!, media[3]! - media[1]!)
      unless sizes.any (fun (w, h) => (w - size.1).abs < 1 && (h - size.2).abs < 1) do sizes := sizes.push size
      if ["crop_box", "trim_box", "bleed_box", "art_box"].any (fun k =>
          match box pageInfo k with | some b => !within b media | none => false) then
        outside := outside.push page
  -- From the destinations themselves: MuPDF would resolve a missing one to page 1.
  let broken ← IO.ofExcept (Json.parse (← Native.brokenDestinations doc))
  let brokenLinks := (broken.getObjValAs? (Array Nat) "links").toOption.getD #[]
  let brokenContents := (broken.getObjValAs? (Array String) "outline").toOption.getD #[]
  unless brokenLinks.isEmpty do
    out := out.push (finding "error" "broken-links" (pages := brokenLinks)
      ("Links that lead nowhere: their destination is missing or outside the document."))
  -- A heading may have no destination at all; one naming a missing one is broken.
  unless brokenContents.isEmpty do
    out := out.push (finding "error" "broken-contents"
      (s!"Contents entries that lead nowhere: {", ".intercalate brokenContents.toList}."))
  unless outside.isEmpty do
    out := out.push (finding "warning" "box-outside-page" (pages := outside)
      ("A crop, trim, bleed or art box extends beyond the page (media box); readers and printers clip differently."))
  if sizes.size > 1 then
    out := out.push (finding "note" "mixed-page-sizes"
      (s!"Pages of {sizes.size} different sizes."))
  -- Errors first, then warnings, then notes.
  let rank (f : Finding) : Nat := if f.severity == "error" then 0 else if f.severity == "warning" then 1 else 2
  return out.insertionSort (fun a b => rank a < rank b)

end Vellumory.Preflight
