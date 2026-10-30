import Vellumory.Shell
import Vellumory.Tools

namespace Vellumory.Pdf

/-- A narrow byte-level predicate. It says nothing about the rest of the PDF. -/
def hasPdfMarker (bytes : ByteArray) : Bool :=
  bytes.size ≥ 5 && bytes[0]! == 37 && bytes[1]! == 80 &&
    bytes[2]! == 68 && bytes[3]! == 70 && bytes[4]! == 45

/-- Successful construction carries evidence about these exact bytes. -/
structure MarkedBytes where
  bytes : ByteArray
  marker : hasPdfMarker bytes = true

def checkMarker (bytes : ByteArray) : Option MarkedBytes :=
  if h : hasPdfMarker bytes = true then some ⟨bytes, h⟩ else none

theorem checkMarker_sound (bytes : ByteArray) (checked : MarkedBytes)
    (h : checkMarker bytes = some checked) :
    checked.bytes = bytes ∧ hasPdfMarker bytes = true := by
  unfold checkMarker at h
  split at h
  next yes => cases h; exact ⟨rfl, yes⟩
  next no => cases h

inductive CheckStatus where
  | clean | warnings | failed
  deriving Repr, BEq

/-- qpdf's documented check status: warnings (3) are not clean (0). -/
def qpdfStatus (exitCode : UInt32) : CheckStatus :=
  if exitCode == 0 then .clean else if exitCode == 3 then .warnings else .failed

/-- An external report deliberately has no proof field for PDF conformance. -/
structure ExternalReport where
  engine : String
  status : CheckStatus
  exitCode : UInt32
  stdout : String
  stderr : String
  deriving Repr

def checkWithQpdf (path : System.FilePath) : IO ExternalReport := do
  let absolute ← IO.FS.realPath path
  let result ← Shell.run <| Shell.capture (command% Tools.qpdf ["--check", absolute.toString])
  return { engine := "qpdf", status := qpdfStatus result.exitCode,
           exitCode := result.exitCode, stdout := result.stdout, stderr := result.stderr }

end Vellumory.Pdf
