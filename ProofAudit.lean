import Vellumory.Pdf
import Vellumory.Geometry
import Vellumory.Navigation
import Vellumory.Scheduler
import Vellumory.History
import Vellumory.Edit
import Vellumory.Diff
import Lean.Util.CollectAxioms
import Lean.Elab.Command

open Lean Elab Command in
run_cmd do
  let allowed := #[``propext, ``Classical.choice, ``Quot.sound]
  for name in #[``Vellumory.Shell.arguments_preserved,
                ``Vellumory.Pdf.checkMarker_sound,
                ``Vellumory.clipped_pixel_in_page,
                ``Vellumory.clipped_pixel_in_viewport,
                ``Vellumory.split_covers,
                ``Vellumory.split_disjoint,
                ``Vellumory.Navigation.accepted_page_in_document,
                ``Vellumory.Navigation.valid_page_preserved,
                ``Vellumory.Scheduler.pop_none_iff,
                ``Vellumory.Scheduler.pop_perm,
                ``Vellumory.Scheduler.pop_minimal,
                ``Vellumory.Scheduler.pop_length,
                ``Vellumory.Scheduler.cancel_kept,
                ``Vellumory.Scheduler.cancel_perm,
                ``Vellumory.History.start_wellFormed,
                ``Vellumory.History.commit_wellFormed,
                ``Vellumory.History.undo_commit,
                ``Vellumory.History.commit_preserves,
                ``Vellumory.History.checkout_head,
                ``Vellumory.History.checkout_out_of_range,
                ``Vellumory.Edit.qpdfArgs_paths,
                ``Vellumory.Diff.olds_script,
                ``Vellumory.Diff.news_script] do
    for axiomName in ← collectAxioms name do
      unless allowed.contains axiomName do
        throwError "{name} depends on forbidden axiom {axiomName}"

-- Query elaborated proof dependencies, not source text.
#print axioms Vellumory.Shell.arguments_preserved
#print axioms Vellumory.Pdf.checkMarker_sound
#print axioms Vellumory.clipped_pixel_in_page
#print axioms Vellumory.clipped_pixel_in_viewport
#print axioms Vellumory.split_covers
#print axioms Vellumory.split_disjoint
#print axioms Vellumory.Navigation.accepted_page_in_document
#print axioms Vellumory.Navigation.valid_page_preserved
#print axioms Vellumory.Scheduler.pop_none_iff
#print axioms Vellumory.Scheduler.pop_perm
#print axioms Vellumory.Scheduler.pop_minimal
#print axioms Vellumory.Scheduler.pop_length
#print axioms Vellumory.Scheduler.cancel_kept
#print axioms Vellumory.Scheduler.cancel_perm
#print axioms Vellumory.History.start_wellFormed
#print axioms Vellumory.History.commit_wellFormed
#print axioms Vellumory.History.undo_commit
#print axioms Vellumory.History.commit_preserves
#print axioms Vellumory.History.checkout_head
#print axioms Vellumory.History.checkout_out_of_range
#print axioms Vellumory.Edit.qpdfArgs_paths
#print axioms Vellumory.Diff.olds_script
#print axioms Vellumory.Diff.news_script
