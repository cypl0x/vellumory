import Vellumory.Json

/-! Branching document history: every state a document has had, as a tree.

Nodes are append-only and refer to their parent by a smaller index, so the
tree is acyclic by construction. `head` is the state the file on disk should
have. Editing commits a child of `head`; undo moves to the parent; redo moves
to the most recent child; checkout moves anywhere. Nothing is ever removed.
The CLI and Emacs use exactly these functions; the theorems below are about
them, not a model of them. -/

namespace Vellumory.History

structure Node where
  /-- SHA-256 of the snapshot's bytes: the content address of its object. -/
  hash : String
  parent : Option Nat
  /-- Structured description of the edit that produced this state. -/
  operation : Json
  /-- Milliseconds since the Unix epoch. -/
  time : Nat
  label : String
  deriving Inhabited

structure History where
  path : String
  nodes : Array Node
  head : Nat
  deriving Inhabited

def Node.parentBefore (index : Nat) (node : Node) : Prop :=
  match node.parent with
  | none => index = 0
  | some p => p < index

/-- Root first, every other node's parent earlier, head in range. -/
def WellFormed (h : History) : Prop :=
  h.nodes.size > 0 ∧ h.head < h.nodes.size ∧
    ∀ (i : Nat) (hi : i < h.nodes.size), h.nodes[i].parentBefore i

def start (path hash : String) (time : Nat) (pages : Nat) : History :=
  { path, head := 0,
    nodes := #[{ hash, parent := none, time, label := s!"opened ({pages} pages)",
                 operation := .mkObj [("op", .str "open")] }] }

def History.current (h : History) : Option Node := h.nodes[h.head]?

/-- Record a new state as a child of `head`, and make it the head. -/
def History.commit (h : History) (hash : String) (operation : Json) (time : Nat) (label : String) : History :=
  { h with nodes := h.nodes.push { hash, parent := some h.head, operation, time, label },
           head := h.nodes.size }

def History.undo (h : History) : Option History :=
  (h.nodes[h.head]?).bind fun node => node.parent.map fun p => { h with head := p }

/-- Children of node `i`, oldest first. -/
def History.children (h : History) (i : Nat) : List Nat :=
  (List.range h.nodes.size).filter fun j => (h.nodes[j]?.bind (·.parent)) == some i

def History.redo (h : History) : Option History :=
  (h.children h.head).getLast?.map fun c => { h with head := c }

def History.checkout (h : History) (i : Nat) : Option History :=
  if i < h.nodes.size then some { h with head := i } else none

theorem start_wellFormed (path hash : String) (time pages : Nat) :
    WellFormed (start path hash time pages) := by
  refine ⟨by simp [start], by simp [start], ?_⟩
  intro i hi
  simp only [start, List.size_toArray, List.length_cons, List.length_nil] at hi
  have : i = 0 := by omega
  subst this
  simp [start, Node.parentBefore]

theorem commit_wellFormed (h : History) (wf : WellFormed h) (hash : String) (op : Json)
    (time : Nat) (label : String) : WellFormed (h.commit hash op time label) := by
  obtain ⟨nonempty, head, parents⟩ := wf
  refine ⟨by simp [History.commit], by simp [History.commit], ?_⟩
  intro i hi
  simp only [History.commit, Array.size_push] at hi
  by_cases last : i = h.nodes.size
  · subst last
    simp [History.commit, Node.parentBefore, Array.getElem_push_eq, head]
  · have lt : i < h.nodes.size := by omega
    simp only [History.commit, Array.getElem_push_lt lt]
    exact parents i lt

/-- Undo after an edit returns exactly to the state before the edit. -/
theorem undo_commit (h : History) (hash : String) (op : Json) (time : Nat) (label : String) :
    ((h.commit hash op time label).undo).map (·.head) = some h.head := by
  simp [History.undo, History.commit]

/-- Committing never loses a state: every earlier node is still there. -/
theorem commit_preserves (h : History) (hash : String) (op : Json) (time : Nat) (label : String)
    (i : Nat) (hi : i < h.nodes.size) :
    (h.commit hash op time label).nodes[i]? = h.nodes[i]? := by
  simp [History.commit, Array.getElem?_push_lt hi]

theorem checkout_head (h : History) (i : Nat) (hi : i < h.nodes.size) :
    (h.checkout i).map (·.head) = some i := by
  simp [History.checkout, hi]

theorem checkout_out_of_range (h : History) (i : Nat) (hi : ¬ i < h.nodes.size) :
    h.checkout i = none := by
  simp [History.checkout, hi]

/-! ## JSON -/

def Node.toJson (n : Node) : Json :=
  .mkObj [("hash", .str n.hash), ("parent", Vellumory.toJson n.parent), ("operation", n.operation),
          ("time", Vellumory.toJson n.time), ("label", .str n.label)]

def Node.fromJson (j : Json) : Except String Node := do
  return { hash := ← j.getObjValAs? String "hash", parent := ← j.getObjValAs? (Option Nat) "parent",
           operation := j.getObjValD "operation", time := ← j.getObjValAs? Nat "time",
           label := ← j.getObjValAs? String "label" }

def History.toJson (h : History) : Json :=
  .mkObj [("version", .num 1), ("path", .str h.path), ("head", Vellumory.toJson h.head),
          ("nodes", .arr (h.nodes.map Node.toJson))]

/-- Parse and check well-formedness, so a corrupted file is rejected rather
    than silently producing a cyclic or dangling tree. -/
def fromJson (j : Json) : Except String History := do
  let nodes : Array Json ← j.getObjValAs? (Array Json) "nodes"
  let nodes ← nodes.mapM Node.fromJson
  let h : History := { path := ← j.getObjValAs? String "path", head := ← j.getObjValAs? Nat "head", nodes }
  if nodes.isEmpty || h.head ≥ nodes.size then throw "history has no nodes or head out of range"
  for i in [0:nodes.size] do
    match nodes[i]!.parent with
    | none => if i != 0 then throw s!"node {i} has no parent"
    | some p => if p ≥ i then throw s!"node {i} has a parent that is not earlier"
  return h

end Vellumory.History
