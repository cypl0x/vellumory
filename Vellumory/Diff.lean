/-! Sequence diffs for documents: pages first, then words within pages.

`script` turns two sequences into keep/delete/insert edits. Whatever the
alignment choices, reading the old side of the script gives back exactly the
first sequence and the new side exactly the second (`olds_script`,
`news_script`): no change is ever invented or lost. An LCS table only steers
the choices towards a minimal script; it cannot break that property. -/

namespace Vellumory.Diff

inductive Edit (α : Type) where
  | keep (old new : α)
  | delete (old : α)
  | insert (new : α)
  deriving Inhabited

def olds : List (Edit α) → List α
  | [] => []
  | .keep x _ :: rest => x :: olds rest
  | .delete x :: rest => x :: olds rest
  | .insert _ :: rest => olds rest

def news : List (Edit α) → List α
  | [] => []
  | .keep _ y :: rest => y :: news rest
  | .delete _ :: rest => news rest
  | .insert y :: rest => y :: news rest

/-- `prefer n m` says whether to delete the old element when `n` old and `m`
    new elements remain and their keys differ. -/
def script [BEq κ] (key : α → κ) (prefer : Nat → Nat → Bool) : List α → List α → List (Edit α)
  | [], ys => ys.map .insert
  | x :: xs, [] => (x :: xs).map .delete
  | x :: xs, y :: ys =>
    if key x == key y then .keep x y :: script key prefer xs ys
    else if prefer (xs.length + 1) (ys.length + 1) then .delete x :: script key prefer xs (y :: ys)
    else .insert y :: script key prefer (x :: xs) ys
termination_by xs ys => xs.length + ys.length

theorem olds_map_insert (ys : List α) : olds (ys.map Edit.insert) = [] := by
  induction ys <;> simp_all [olds]

theorem olds_map_delete (xs : List α) : olds (xs.map Edit.delete) = xs := by
  induction xs <;> simp_all [olds]

theorem news_map_insert (ys : List α) : news (ys.map Edit.insert) = ys := by
  induction ys <;> simp_all [news]

theorem news_map_delete (xs : List α) : news (xs.map Edit.delete) = [] := by
  induction xs <;> simp_all [news]

/-- The old side of any script is exactly the old sequence. -/
theorem olds_script [BEq κ] (key : α → κ) (prefer : Nat → Nat → Bool) (xs ys : List α) :
    olds (script key prefer xs ys) = xs := by
  induction xs, ys using script.induct key prefer with
  | case1 ys => simp [script, olds_map_insert]
  | case2 x xs => simp [script, olds, olds_map_delete]
  | case3 x xs y ys h ih => simp [script, h, olds, ih]
  | case4 x xs y ys h p ih => simp [script, h, p, olds, ih]
  | case5 x xs y ys h p ih => simp [script, h, p, olds, ih]

/-- The new side of any script is exactly the new sequence. -/
theorem news_script [BEq κ] (key : α → κ) (prefer : Nat → Nat → Bool) (xs ys : List α) :
    news (script key prefer xs ys) = ys := by
  induction xs, ys using script.induct key prefer with
  | case1 ys => simp [script, news_map_insert]
  | case2 x xs => simp [script, news, news_map_delete]
  | case3 x xs y ys h ih => simp [script, h, news, ih]
  | case4 x xs y ys h p ih => simp [script, h, p, news, ih]
  | case5 x xs y ys h p ih => simp [script, h, p, news, ih]

/-- Longest-common-subsequence lengths of all suffix pairs, as `prefer`.
    Quadratic: callers bound the input sizes. -/
def lcsPrefer [BEq κ] (a b : Array κ) : Nat → Nat → Bool := Id.run do
  let n := a.size
  let m := b.size
  let width := m + 1
  -- table[i * width + j] = LCS length of a[i:] and b[j:]
  let mut table : Array Nat := Array.replicate ((n + 1) * width) 0
  for di in [0:n] do
    let i := n - 1 - di
    for dj in [0:m] do
      let j := m - 1 - dj
      let value := if a[i]? == b[j]? then table[(i + 1) * width + j + 1]! + 1
        else max table[(i + 1) * width + j]! table[i * width + j + 1]!
      table := table.set! (i * width + j) value
  return fun remainingOld remainingNew =>
    let i := n - remainingOld
    let j := m - remainingNew
    table[(i + 1) * width + j]! ≥ table[i * width + j + 1]!

/-- A minimal script when `a.size * b.size ≤ budget`, otherwise a sound but
    coarse one that deletes before inserting at the first difference. -/
def diff [BEq κ] (key : α → κ) (a b : Array α) (budget : Nat := 4000000) : List (Edit α) :=
  let prefer := if a.size * b.size ≤ budget then lcsPrefer (a.map key) (b.map key) else fun _ _ => true
  script key prefer a.toList b.toList

end Vellumory.Diff
