import Lean

/- Bootstrap uses only the Lean standard library, before the CLI dependency is
   prepared. It is also the Nix builder, avoiding a project shell build phase. -/

open System

partial def copyTree (source destination : FilePath) : IO Unit := do
  IO.FS.createDirAll destination
  for entry in ← source.readDir do
    if [".git", ".lake", "target", "vendor"].contains entry.fileName || entry.fileName.endsWith ".elc" then continue
    let target := destination / entry.fileName
    if ← entry.path.isDir then
      copyTree entry.path target
    else
      IO.FS.writeBinFile target (← IO.FS.readBinFile entry.path)

def requiredEnv (name : String) : IO String := do
  match ← IO.getEnv name with
  | some value => return value
  | none => throw <| IO.userError s!"Missing {name}; use nix develop"

def checked (cmd : String) (args : Array String) (cwd : FilePath) : IO Unit := do
  let result ← IO.Process.output { cmd, args, cwd := some cwd }
  IO.print result.stdout
  IO.eprint result.stderr
  unless result.exitCode == 0 do
    throw <| IO.userError s!"{cmd} failed with exit {result.exitCode}"

/-- Like `checked`, and a warning on standard error fails the step too: the
    manual must build cleanly. A missing program is named with its package. -/
def clean (cmd : String) (args : Array String) (cwd : FilePath) (package : String) : IO Unit := do
  let result ← try IO.Process.output { cmd, args, cwd := some cwd }
    catch e => throw <| IO.userError s!"{cmd} could not run ({e}); it comes with {package}"
  IO.print result.stdout
  IO.eprint result.stderr
  unless result.exitCode == 0 do
    throw <| IO.userError s!"{cmd} failed with exit {result.exitCode}"
  if (result.stderr.toLower.splitOn "warning").length > 1 then
    throw <| IO.userError s!"{cmd} warned; the manual must build without warnings"

/-- One check of `lint`: it fails on a non-zero exit and, when SILENT (the tool
    prints nothing when clean), on any output too. Either way a line says how
    it went, so nothing passes unseen; a missing program is named with its
    package. -/
def lintStep (name cmd : String) (args : Array String) (package : String) (silent := true) : IO Bool := do
  -- A spawn forks first: unflushed output would be written twice.
  (← IO.getStdout).flush
  let result ← try IO.Process.output { cmd, args, cwd := some (← IO.currentDir), env := #[("NO_COLOR", some "1")] }
    catch e => do
      IO.println s!"✗ {name}: {cmd} could not run ({e}); it comes with {package}"
      return false
  let output := (result.stdout ++ result.stderr).trimAscii.toString
  if result.exitCode == 0 && (!silent || output.isEmpty) then
    IO.println s!"✓ {name}"
    return true
  IO.println s!"✗ {name} (exit {result.exitCode})"
  unless output.isEmpty do IO.println output
  return false

/-- The Emacs Lisp files of the package, the generated autoloads aside. -/
def lispFiles : IO (Array String) := do
  let files ← System.FilePath.readDir "lisp"
  return (files.filterMap fun entry =>
    if entry.fileName.endsWith ".el" && entry.fileName != "vellumory-autoloads.el" then
      some s!"lisp/{entry.fileName}" else none).qsort (· < ·)

/-- Org files that are documentation: the README, the manual and the notes. -/
def orgFiles : IO (Array String) := do
  let docs ← System.FilePath.readDir "docs"
  return #["README.org", "doom/tools/vellumory/README.org"] ++
    ((docs.filterMap fun entry => if entry.fileName.endsWith ".org" then some s!"docs/{entry.fileName}" else none).qsort (· < ·))

/-- Every linter over the sources; any finding fails. -/
def lintAll : IO Bool := do
  let lisp ← lispFiles
  let org ← orgFiles
  let evil := (← IO.getEnv "VELLUMORY_EVIL_LISP").getD "."
  let packageLint := (← IO.getEnv "VELLUMORY_PACKAGE_LINT_LISP").getD "."
  IO.FS.createDirAll "target/elc"
  let emacs (name : String) (args : Array String) (silent := true) :=
    lintStep name "emacs" (#["--batch", "-Q", "-L", "lisp", "-L", evil] ++ args) "GNU Emacs" silent
  let quoted (files : Array String) := " ".intercalate (files.map (fun f => (repr f).pretty)).toList
  -- Each check runs once and reports; all run, whatever fails before.
  let mut ok := true
  -- Warnings are errors; the compiled files go to target/elc.
  ok := (← emacs "byte-compile (warnings are errors)" (#["--eval",
    "(setq byte-compile-error-on-warn t byte-compile-dest-file-function (lambda (f) (expand-file-name (concat (file-name-base f) \".elc\") \"target/elc/\")))",
    "-f", "batch-byte-compile"] ++ lisp)) && ok
  ok := (← emacs "checkdoc" #["--eval",
    s!"(progn (require 'checkdoc) (dolist (file (list {quoted lisp})) (checkdoc-file file)))"]) && ok
  ok := (← emacs "package-lint" #["-L", packageLint, "--eval",
    "(progn (require 'package-lint) (setq package-lint-main-file \"lisp/vellumory.el\") (package-lint-batch-and-exit))",
    "lisp/vellumory.el"] (silent := false)) && ok
  ok := (← emacs "org-lint" #["--eval",
    s!"(progn (require 'org-lint) (dolist (file (list {quoted org})) (with-current-buffer (find-file-noselect file) (dolist (report (org-lint)) (princ (format \"%s:%s: %s\\n\" file (aref (cadr report) 0) (aref (cadr report) 2)))))))"]) && ok
  ok := (← lintStep "typos (spelling)" "typos" #[] "typos") && ok
  ok := (← lintStep "vale (prose)" "vale" (#["--no-wrap", "--minAlertLevel=warning"] ++ org) "vale" (silent := false)) && ok
  ok := (← lintStep "nixfmt" "nixfmt" #["--check", "flake.nix"] "nixfmt") && ok
  ok := (← lintStep "statix" "statix" #["check", "."] "statix") && ok
  ok := (← lintStep "deadnix" "deadnix" #["--fail", "flake.nix"] "deadnix") && ok
  ok := (← lintStep "actionlint" "actionlint" #[] "actionlint") && ok
  ok := (← lintStep "zizmor (workflow security)" "zizmor"
    #["--offline", "--min-severity=low", ".github/workflows"] "zizmor" (silent := false)) && ok
  return ok

def lakeArgs : IO (Array String) := do
  return #["--reconfigure", "-KmupdfInclude=" ++ (← requiredEnv "VELLUMORY_MUPDF_INCLUDE"),
           "-KmupdfLib=" ++ (← requiredEnv "VELLUMORY_MUPDF_LIB"),
           "-KopensslInclude=" ++ (← requiredEnv "VELLUMORY_OPENSSL_INCLUDE"),
           "-KopensslLib=" ++ (← requiredEnv "VELLUMORY_OPENSSL_LIB"),
           "-KsynctexInclude=" ++ (← requiredEnv "VELLUMORY_SYNCTEX_INCLUDE"),
           "-KsynctexLib=" ++ (← requiredEnv "VELLUMORY_SYNCTEX_LIB"),
           "-KexempiInclude=" ++ (← requiredEnv "VELLUMORY_EXEMPI_INCLUDE"),
           "-KexempiLib=" ++ (← requiredEnv "VELLUMORY_EXEMPI_LIB")]

def main (args : List String) : IO UInt32 := do
  try
    match args with
    | ["prepare", cli] => copyTree cli "vendor/Cli"
    | ["prepare"] => copyTree (← requiredEnv "VELLUMORY_CLI_SRC") "vendor/Cli"
    | ["lint"] =>
      unless ← lintAll do
        (← IO.getStdout).flush
        throw <| IO.userError "lint found problems (✗ above)"
    | ["autoloads"] =>
      -- Generated, not committed (package managers make their own), and
      -- checked by ERT; packagers without package.el (nix trivialBuild, Doom
      -- unstraightened) load it explicitly.
      checked "emacs" #["--batch", "-Q", "--eval",
        "(loaddefs-generate \"lisp\" (expand-file-name \"lisp/vellumory-autoloads.el\") (list \"vellumory-autoloads.el\") nil nil t)"]
        (← IO.currentDir)
    | ["manual"] =>
      -- docs/vellumory.org is the manual's source. Org's Texinfo exporter
      -- writes docs/vellumory.texi (generated and committed, as the autoloads
      -- are), and makeinfo builds target/vellumory.info from it, which proves
      -- the Texinfo valid. Warnings from either fail the build.
      let cwd ← IO.currentDir
      clean "emacs" #["--batch", "-Q", "--eval",
        "(progn (require 'ox-texinfo) (with-current-buffer (find-file-noselect \"docs/vellumory.org\") (org-texinfo-export-to-texinfo)))"]
        cwd "GNU Emacs"
      IO.FS.createDirAll (cwd / "target")
      clean "makeinfo" #["--no-split", "-o", "target/vellumory.info", "docs/vellumory.texi"] cwd "GNU Texinfo"
    | "lake" :: rest => checked "lake" ((← lakeArgs) ++ rest.toArray) (← IO.currentDir)
    | ["nix"] =>
      let cwd ← IO.currentDir
      let work := cwd / "source"
      copyTree (← requiredEnv "src") work
      copyTree (← requiredEnv "VELLUMORY_CLI_SRC") (work / "vendor/Cli")
      let tool (name key : String) : IO String := do
        return s!"def {name} : String := {repr (← requiredEnv key)}\n"
      IO.FS.writeFile (work / "Vellumory/Tools.lean") <|
        "namespace Vellumory.Tools\n" ++ (← tool "qpdf" "VELLUMORY_QPDF") ++
        (← tool "verapdf" "VELLUMORY_VERAPDF") ++ (← tool "ocrmypdf" "VELLUMORY_OCRMYPDF") ++
        (← tool "fixQdf" "VELLUMORY_FIX_QDF") ++
        "end Vellumory.Tools\n"
      -- The autoloads are generated (they are not committed) before the tests,
      -- which load them as the packages do.
      checked "emacs" #["--batch", "-Q", "--eval",
        "(loaddefs-generate \"lisp\" (expand-file-name \"lisp/vellumory-autoloads.el\") (list \"vellumory-autoloads.el\") nil nil t)"] work
      let options ← lakeArgs
      checked "lake" (options ++ #["build", "vellumory", "vellumoryTests"]) work
      checked "lake" (options ++ #["test"]) work
      checked "lake" (options ++ #["env", "lean", "ProofAudit.lean"]) work
      checked "lake" (options ++ #["env", "lean", "--run", "scripts/Review.lean", "test", "emacs"]) work
      let output := FilePath.mk (← requiredEnv "out")
      IO.FS.createDirAll (output / "bin")
      let executable := output / "bin/vellumory"
      IO.FS.writeBinFile executable (← IO.FS.readBinFile (work / ".lake/build/bin/vellumory"))
      IO.setAccessRights executable {
        user := { read := true, write := true, execution := true }
        group := { read := true, execution := true }
        other := { read := true, execution := true }
      }
      -- The manual is built beside the Lisp, where `vellumory-info' looks first.
      copyTree (work / "lisp") (output / "share/emacs/site-lisp/vellumory")
      checked "makeinfo" #["--no-split", "-o", (output / "share/emacs/site-lisp/vellumory/vellumory.info").toString,
        "docs/vellumory.texi"] work
      -- And where Info looks by default, for profiles that put share/info on its path.
      IO.FS.createDirAll (output / "share/info")
      IO.FS.writeBinFile (output / "share/info/vellumory.info")
        (← IO.FS.readBinFile (output / "share/emacs/site-lisp/vellumory/vellumory.info"))
    | _ => throw <| IO.userError "Usage: lean --run scripts/Build.lean prepare [CLI_SOURCE]"
    return 0
  catch e =>
    IO.eprintln e
    return 1
