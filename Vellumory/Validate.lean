import Vellumory.Json
import Vellumory.Shell
import Vellumory.Tools

/-! PDF/A, PDF/UA and WTPDF validation through veraPDF.

veraPDF's JSON report (`--format json`) is normalized into a smaller,
stable shape: verdict, counts, and failed rules with their checks. Page
indices are taken from check contexts such as
`root/document[0]/pages[4](12 0 obj PDPage)/…`. The verdict is veraPDF's:
machine validation cannot establish the human-judged parts of PDF/UA. -/

namespace Vellumory.Validate

/-- veraPDF built-in flavours (`--flavour`); "0" detects from metadata. -/
def flavours : List String :=
  ["0", "1a", "1b", "2a", "2b", "2u", "3a", "3b", "3u", "4", "4e", "4f", "ua1", "ua2", "wt1r", "wt1a"]

/-- The page index in a veraPDF context, if it names one. -/
def contextPage (context : String) : Option Nat := do
  let rest ← (context.splitOn "pages[").tail?.bind (·.head?)
  (rest.takeWhile Char.isDigit).toString.toNat?

/-- veraPDF exit codes (CliConstants.java): 0 valid, 1 invalid, others errors. -/
def exitMeaning : UInt32 → String
  | 0 => "valid" | 1 => "invalid" | 2 => "bad parameters" | 3 => "out of memory"
  | 4 => "no files" | 6 => "input/output error" | 7 => "parse failure" | 8 => "encrypted"
  | 9 => "veraPDF exception" | 10 => "JAXB error" | 11 => "multiprocess failure"
  | 12 => "interrupted" | n => s!"exit code {n}"

def normalizeCheck (check : Json) : Json :=
  let context := (check.getObjValAs? String "context").toOption.getD ""
  .mkObj [("context", .str context), ("message", check.getObjValD "errorMessage"),
          ("page", toJson (contextPage context))]

def normalizeRule (rule : Json) : Json :=
  let checks := (rule.getObjValAs? (Array Json) "checks").toOption.getD #[]
  .mkObj [("specification", rule.getObjValD "specification"), ("clause", rule.getObjValD "clause"),
          ("test_number", rule.getObjValD "testNumber"), ("status", rule.getObjValD "status"),
          ("description", rule.getObjValD "description"), ("object", rule.getObjValD "object"),
          ("test", rule.getObjValD "test"), ("failed_checks", rule.getObjValD "failedChecks"),
          ("checks", .arr (checks.map normalizeCheck))]

/-- Normalize one job (one file) of a veraPDF JSON report. -/
def normalizeJob (job : Json) (exitCode : UInt32) : Except String Json := do
  let results := (job.getObjValAs? (Array Json) "validationResult").toOption.getD #[]
  let some result := results[0]?
    | throw s!"veraPDF did not validate the file ({exitMeaning exitCode})"
  let details := result.getObjValD "details"
  let rules := (details.getObjValAs? (Array Json) "ruleSummaries").toOption.getD #[]
  return .mkObj [("compliant", result.getObjValD "compliant"), ("profile", result.getObjValD "profileName"),
    ("statement", result.getObjValD "statement"), ("exit", .str (exitMeaning exitCode)),
    ("passed_rules", details.getObjValD "passedRules"), ("failed_rules", details.getObjValD "failedRules"),
    ("passed_checks", details.getObjValD "passedChecks"), ("failed_checks", details.getObjValD "failedChecks"),
    ("rules", .arr (rules.map normalizeRule))]

/-- Normalize a veraPDF JSON report of one file. -/
def normalize (report : Json) (exitCode : UInt32) : Except String Json := do
  let jobs : Array Json ← (report.getObjValD "report").getObjValAs? (Array Json) "jobs"
  let some job := jobs[0]? | throw "veraPDF report has no job"
  normalizeJob job exitCode

def run (file : String) (flavour : String) (maxChecks : Nat := 20) : IO Json := do
  unless flavours.contains flavour do
    throw <| IO.userError s!"unknown veraPDF flavour {flavour}; one of {", ".intercalate flavours}"
  let args := #["--format", "json", "--flavour", flavour, "--maxfailuresdisplayed", toString maxChecks, file]
  let result ← Shell.run <| Shell.capture { program := Tools.verapdf, args }
  unless result.exitCode == 0 || result.exitCode == 1 do
    throw <| IO.userError s!"veraPDF: {exitMeaning result.exitCode}: {result.stderr.trimAscii}"
  IO.ofExcept (Json.parse result.stdout >>= (normalize · result.exitCode))


/-- Validate FILES in one veraPDF run (one JVM start). Each result names its
    file; a file veraPDF could not validate gets an error instead. -/
def runMany (files : Array String) (flavour : String) (maxChecks : Nat := 20) : IO (Array Json) := do
  unless flavours.contains flavour do
    throw <| IO.userError s!"unknown veraPDF flavour {flavour}; one of {", ".intercalate flavours}"
  if files.isEmpty then return #[]
  let args := #["--format", "json", "--flavour", flavour, "--maxfailuresdisplayed", toString maxChecks] ++ files
  let result ← Shell.run <| Shell.capture { program := Tools.verapdf, args }
  let some report := (Json.parse result.stdout).toOption
    | throw <| IO.userError s!"veraPDF: {exitMeaning result.exitCode}: {result.stderr.trimAscii}"
  let jobs := ((report.getObjValD "report").getObjValAs? (Array Json) "jobs").toOption.getD #[]
  let results := jobs.map fun job =>
    let name := ((job.getObjValD "itemDetails").getObjValAs? String "name").toOption.getD ""
    match normalizeJob job result.exitCode with
    | .ok json =>
      let compliant := (json.getObjValAs? Bool "compliant").toOption == some true
      (json.setObjVal! "exit" (.str (if compliant then "valid" else "invalid"))).setObjVal! "file" (.str name)
    | .error message => .mkObj [("file", .str name), ("error", .str message)]
  -- veraPDF skips files it cannot open without a job: every input gets a row.
  let named := results.filterMap fun r => (r.getObjValAs? String "file").toOption
  let missing ← files.filterM fun (f : String) => do
    let absolute := (← (IO.FS.realPath (System.FilePath.mk f)).toBaseIO).toOption.map (·.toString)
    return !(named.contains f || absolute.any named.contains)
  return results ++ missing.map fun f => .mkObj [("file", .str f), ("error", .str "veraPDF could not open the file")]

end Vellumory.Validate
