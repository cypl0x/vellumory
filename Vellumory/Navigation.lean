
namespace Vellumory.Navigation

/-- Check a foreign outline destination against the opened snapshot's page count.
    Missing or invalid destinations remain headings/URIs, never an arbitrary page. -/
def checkedPage (count : Nat) (page : Option Nat) : Option Nat :=
  page.bind fun n => if n < count then some n else none

theorem accepted_page_in_document (count : Nat) (raw : Option Nat) (page : Nat)
    (h : checkedPage count raw = some page) : page < count := by
  cases raw with
  | none => simp [checkedPage] at h
  | some n =>
    simp only [checkedPage, Option.bind_some] at h
    split at h <;> simp_all

theorem valid_page_preserved (count page : Nat) (h : page < count) :
    checkedPage count (some page) = some page := by
  simp [checkedPage, h]

end Vellumory.Navigation
