
/-! Pure request queue used by the worker lanes.

Jobs are ordered by `(priority, seq)`: lower priority values first, then
arrival order. The worker calls exactly these functions under its mutex, so the
theorems below describe the runtime queue, not a separate model. -/

namespace Vellumory.Scheduler

structure Job (α : Type) where
  id : Nat
  priority : Nat
  seq : Nat
  payload : α

def Job.before (a b : Job α) : Bool :=
  a.priority < b.priority || (a.priority == b.priority && a.seq < b.seq)

/-- Remove the earliest job. -/
def pop : List (Job α) → Option (Job α × List (Job α))
  | [] => none
  | j :: rest =>
    match pop rest with
    | none => some (j, [])
    | some (best, others) => if best.before j then some (best, j :: others) else some (j, rest)

/-- Split into (cancelled, kept). -/
def cancel (ids : List Nat) (queue : List (Job α)) : List (Job α) × List (Job α) :=
  queue.partition fun j => ids.contains j.id

theorem pop_none_iff (q : List (Job α)) : pop q = none ↔ q = [] := by
  cases q with
  | nil => simp [pop]
  | cons j rest =>
    simp only [pop, reduceCtorEq, iff_false]
    split <;> (try split) <;> simp

theorem pop_perm (q : List (Job α)) (j : Job α) (r : List (Job α))
    (h : pop q = some (j, r)) : (j :: r).Perm q := by
  induction q generalizing j r with
  | nil => simp [pop] at h
  | cons x rest ih =>
    simp only [pop] at h
    split at h
    · rename_i hp
      have := (pop_none_iff rest).1 hp
      simp_all
    · rename_i best others hp
      have hperm := ih best others hp
      split at h
      · simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        exact (List.Perm.swap x best others).trans (List.Perm.cons x hperm)
      · simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        exact List.Perm.refl _

private theorem not_before_trans {a b c : Job α}
    (hab : (a.before b) = false) (hbc : (b.before c) = false) : (a.before c) = false := by
  simp only [Job.before, Bool.or_eq_false_iff, Bool.and_eq_false_iff, decide_eq_false_iff_not,
    beq_eq_false_iff_ne, ne_eq] at *
  omega

private theorem before_asymm {a b : Job α} (h : (a.before b) = true) : (b.before a) = false := by
  simp only [Job.before, Bool.or_eq_true, Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq] at h
  simp only [Job.before, Bool.or_eq_false_iff, Bool.and_eq_false_iff, decide_eq_false_iff_not,
    beq_eq_false_iff_ne, ne_eq]
  omega

/-- The popped job is not preceded by any remaining job. -/
theorem pop_minimal (q : List (Job α)) (j : Job α) (r : List (Job α))
    (h : pop q = some (j, r)) : ∀ x ∈ r, (x.before j) = false := by
  induction q generalizing j r with
  | nil => simp [pop] at h
  | cons y rest ih =>
    simp only [pop] at h
    split at h
    · simp_all
    · rename_i best others hp
      have hmin := ih best others hp
      have hperm := pop_perm rest best others hp
      split at h
      · rename_i hb
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        intro x hx
        simp only [List.mem_cons] at hx
        rcases hx with rfl | hx
        · exact before_asymm hb
        · exact hmin x hx
      · rename_i hb
        simp only [Bool.not_eq_true] at hb
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        intro x hx
        have hx' : x ∈ best :: others := hperm.symm.mem_iff.mp hx
        simp only [List.mem_cons] at hx'
        rcases hx' with rfl | hx'
        · exact hb
        · exact not_before_trans (hmin x hx') hb

theorem pop_length (q : List (Job α)) (j : Job α) (r : List (Job α))
    (h : pop q = some (j, r)) : r.length + 1 = q.length := by
  simpa using (pop_perm q j r h).length_eq

/-- Cancellation keeps no job with a cancelled id. -/
theorem cancel_kept (ids : List Nat) (q : List (Job α)) :
    ∀ j ∈ (cancel ids q).2, ids.contains j.id = false := by
  intro j hj
  simp only [cancel, List.partition_eq_filter_filter, List.mem_filter] at hj
  simpa using hj.2

/-- Cancellation loses no job: every job is either cancelled or kept. -/
theorem cancel_perm (ids : List Nat) (q : List (Job α)) :
    ((cancel ids q).1 ++ (cancel ids q).2).Perm q := by
  simp only [cancel, List.partition_eq_filter_filter]
  exact List.filter_append_perm _ q

end Vellumory.Scheduler
