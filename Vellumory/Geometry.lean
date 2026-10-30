
namespace Vellumory

/-- Integer raster intervals are half-open: [start, stop). -/
structure Band where
  start : Nat
  stop : Nat
  deriving Repr, BEq

def Band.contains (b : Band) (pixel : Nat) : Prop := b.start ≤ pixel ∧ pixel < b.stop

/-- Clip first to a page, then to a visible vertical interval. -/
def clipBand (pageHeight : Nat) (request viewport : Band) : Band :=
  ⟨min pageHeight (max request.start viewport.start),
   min pageHeight (min request.stop viewport.stop)⟩

theorem clipped_pixel_in_page (pageHeight : Nat) (request viewport : Band) (p : Nat)
    (h : (clipBand pageHeight request viewport).contains p) : p < pageHeight := by
  simp only [clipBand, Band.contains] at h
  omega

theorem clipped_pixel_in_viewport (pageHeight : Nat) (request viewport : Band) (p : Nat)
    (h : (clipBand pageHeight request viewport).contains p) : viewport.contains p := by
  simp only [clipBand, Band.contains] at h ⊢
  omega

/-- Split at a clamped boundary; no pixel is duplicated or omitted. -/
def splitBand (b : Band) (cut : Nat) : Band × Band :=
  let boundary := max b.start (min cut b.stop)
  (⟨b.start, boundary⟩, ⟨boundary, b.stop⟩)

theorem split_covers (b : Band) (valid : b.start ≤ b.stop) (cut p : Nat) :
    b.contains p ↔ (splitBand b cut).1.contains p ∨ (splitBand b cut).2.contains p := by
  simp only [Band.contains, splitBand]
  omega

theorem split_disjoint (b : Band) (cut p : Nat) :
    ¬ ((splitBand b cut).1.contains p ∧ (splitBand b cut).2.contains p) := by
  simp only [Band.contains, splitBand]
  omega

end Vellumory
