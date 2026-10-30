import Cli
import Std.Time
import Vellumory.Shell
import Vellumory.Fixtures

open Cli Vellumory System

def writeFixture (_ : Parsed) : IO UInt32 := do
  let dir := (← IO.currentDir) / "target/review"
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "reader.pdf.pending") Fixtures.reader
  IO.FS.rename (dir / "reader.pdf.pending") (dir / "reader.pdf")
  IO.FS.writeFile (dir / "long-reader.pdf") Fixtures.longReader
  return 0

def fixtureCmd : Cmd := `[Cli|
  fixture VIA writeFixture; "Write the current reader fixture for live review."
]

/-- The X display reviews use: VELLUMORY_REVIEW_DISPLAY, else DISPLAY. A private
    Xvfb (see `xvfb`) keeps automated reviews apart from other programs. -/
def reviewDisplay : IO String := do
  match (← IO.getEnv "VELLUMORY_REVIEW_DISPLAY") <|> (← IO.getEnv "DISPLAY") with
  | some display => return display
  | none => throw <| IO.userError "No display: set VELLUMORY_REVIEW_DISPLAY (e.g. :99 after `xvfb :99`)"
/-- The Doom review daemon's socket: `vellumory-doom` is the reviewer's own Doom;
    another name (VELLUMORY_DOOM_SOCKET) runs a separate one with its own state. -/
def doomSocket : IO String := do return (← IO.getEnv "VELLUMORY_DOOM_SOCKET").getD "vellumory-doom"

/-- A review display named like a Wayland socket (`wayland-1`) is a Wayland one. -/
def isWayland (display : String) : Bool := display.startsWith "wayland"

/-- Start a headless Wayland compositor (sway on wlroots' headless backend with
    the pixman renderer, no XWayland) and print its socket for
    VELLUMORY_REVIEW_DISPLAY: the review then runs as on a Wayland desktop. -/
def startWayland (_ : Parsed) : IO UInt32 := do
  let root ← IO.currentDir
  let runtime := System.FilePath.mk ((← IO.getEnv "XDG_RUNTIME_DIR").getD "/tmp")
  let sockets : IO (List String) := do
    return (← runtime.readDir).toList.filterMap fun e =>
      if e.fileName.startsWith "wayland-" && !e.fileName.endsWith ".lock" then some e.fileName else none
  let before ← sockets
  let config := root / "target/review/sway.conf"
  IO.FS.createDirAll (root / "target/review")
  IO.FS.writeFile config "xwayland disable\noutput * resolution 1280x1024\ndefault_border none\n"
  let _ ← IO.Process.spawn {
    cmd := "sway", args := #["--config", config.toString]
    env := #[("WLR_BACKENDS", some "headless"), ("WLR_RENDERER", some "pixman"),
      ("WLR_LIBINPUT_NO_DEVICES", some "1"), ("DISPLAY", none), ("WAYLAND_DISPLAY", none)]
    stdin := .null, stdout := .null, stderr := .null }
  for _ in [0:200] do
    if let some name := (← sockets).find? (!before.contains ·) then
      IO.println s!"sway ready on {name}; export VELLUMORY_REVIEW_DISPLAY={name}"
      return 0
    IO.sleep 50
  throw <| IO.userError "sway did not start"

def waylandCmd : Cmd := `[Cli|
  wayland VIA startWayland; "Start a headless Wayland compositor for reviews under Wayland."
]

/-- Start a private X server on DISPLAY (e.g. :7) and wait until it accepts clients. -/
def startXvfb (p : Parsed) : IO UInt32 := do
  let display := p.positionalArg! "display" |>.as! String
  let _ ← IO.Process.spawn {
    cmd := "Xvfb", args := #[display, "-screen", "0", "1280x1024x24", "-nolisten", "tcp"]
    stdin := .null, stdout := .null, stderr := .null }
  let socket := System.FilePath.mk s!"/tmp/.X11-unix/X{display.drop 1}"
  for _ in [0:100] do
    if ← socket.pathExists then
      IO.println s!"Xvfb ready on {display}; export VELLUMORY_REVIEW_DISPLAY={display}"
      return 0
    IO.sleep 50
  throw <| IO.userError s!"Xvfb did not start on {display}"
def xvfbCmd : Cmd := `[Cli|
  xvfb VIA startXvfb; "Start a private X server for reviews that must not share a display."
  ARGS:
    display : String; "Display name, e.g. :7."
]

/-- Slides of one colour each: red; blue wipes in from the left in 0.6 s;
    green fades in, red dissolves in, yellow glitters in from the left and
    white flies in from the left, opaque, each in 0.8 s. -/
def slides : String :=
  Fixtures.document #["<< /Type /Catalog /Pages 2 0 R >>",
    "<< /Type /Pages /Kids [3 0 R 5 0 R 7 0 R 9 0 R 11 0 R 13 0 R] /Count 6 >>",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 400 300] /Contents 4 0 R >>",
    Fixtures.stream "1 0 0 rg 0 0 400 300 re f\n",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 400 300] /Contents 6 0 R /Trans << /S /Wipe /D 0.6 /Di 0 >> >>",
    Fixtures.stream "0 0 1 rg 0 0 400 300 re f\n",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 400 300] /Contents 8 0 R /Trans << /S /Fade /D 0.8 >> >>",
    Fixtures.stream "0 1 0 rg 0 0 400 300 re f\n",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 400 300] /Contents 10 0 R /Trans << /S /Dissolve /D 0.8 >> >>",
    Fixtures.stream "1 0 0 rg 0 0 400 300 re f\n",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 400 300] /Contents 12 0 R /Trans << /S /Glitter /D 0.8 /Di 0 >> >>",
    Fixtures.stream "1 1 0 rg 0 0 400 300 re f\n",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 400 300] /Contents 14 0 R /Trans << /S /Fly /D 0.8 /Di 0 /B true >> >>",
    Fixtures.stream "1 1 1 rg 0 0 400 300 re f\n"]

/-- Two pages of a black block with wide white margins: 100 points at the
    sides, about 140 above and below. -/
def margins : String :=
  Fixtures.document #["<< /Type /Catalog /Pages 2 0 R >>",
    "<< /Type /Pages /Kids [3 0 R 5 0 R] /Count 2 >>",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents 4 0 R >>",
    Fixtures.stream "0 g 100 150 412 500 re f\n",
    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents 6 0 R >>",
    Fixtures.stream "0 g 100 150 412 500 re f\n"]

/-- A document that asks to open as a book (odd pages right) with its outline. -/
def bookReader : String :=
  Fixtures.reader (catalog := "/PageLayout /TwoColumnRight /PageMode /UseOutlines ")

def startReview (p : Parsed) : IO UInt32 := do
  let root ← IO.currentDir
  let output := root / "target/review"
  IO.FS.createDirAll output
  IO.FS.writeFile (output / "reader.pdf") Fixtures.reader
  IO.FS.writeFile (output / "long-reader.pdf") Fixtures.longReader
  IO.FS.writeFile (output / "book-reader.pdf") bookReader
  IO.FS.writeFile (output / "slides.pdf") slides
  IO.FS.writeFile (output / "margins.pdf") margins
  let emacs := p.positionalArg! "emacs" |>.as! String
  let display := p.positionalArg! "display" |>.as! String
  -- GTK's integer scale: 2 renders the review as on a HiDPI display.
  let scale := (p.flag? "scale").map (·.as! Nat) |>.getD 1
  -- Under Wayland pgtk must not fall back to X: no DISPLAY, the Wayland GDK backend.
  let onDisplay (script : Shell.Script Unit) : Shell.Script Unit :=
    if isWayland display then
      Shell.withoutEnv "DISPLAY" <| Shell.withEnv "WAYLAND_DISPLAY" display <|
        Shell.withEnv "GDK_BACKEND" "wayland" script
    else Shell.withEnv "DISPLAY" display script
  Shell.run <| onDisplay <| Shell.withEnv "VELLUMORY_REVIEW_ROOT" root.toString <|
    Shell.withEnv "GDK_SCALE" (toString scale) do
    let daemon ← Shell.checked (command% emacs ["-Q", "--daemon=vellumory-review"])
    IO.print daemon.stdout
    let client ← Shell.checked (command% "emacsclient" ["--socket-name=vellumory-review", "--create-frame",
      "--no-wait", "--display", display, "--eval", "(load (expand-file-name \"tests/gui-init.el\" (getenv \"VELLUMORY_REVIEW_ROOT\")))"])
    IO.print client.stdout
  return 0

def snapshot (_ : Parsed) : IO UInt32 := do
  let display ← reviewDisplay
  let result ← Shell.run <|
    if isWayland display then
      Shell.withEnv "WAYLAND_DISPLAY" display <| Shell.checked (command% "grim" ["target/review/screenshot.png"])
    else Shell.withEnv "DISPLAY" display <|
      Shell.checked (command% "magick" ["import", "-window", "Vellumory review", "target/review/screenshot.png"])
  IO.print result.stdout
  return 0

def startCmd : Cmd := `[Cli|
  start VIA startReview; "Start an isolated graphical Emacs review with a generated PDF."
  FLAGS:
    s, scale : Nat; "GTK scale factor (2: HiDPI)."
  ARGS:
    emacs : String; "Emacs executable."
    display : String; "X display, e.g. :1."
]
def screenshotCmd : Cmd := `[Cli|
  screenshot VIA snapshot; "Capture DISPLAY=:1 into target/review/screenshot.png."
]
def testEmacs (p : Parsed) : IO UInt32 := do
  let root ← IO.currentDir
  let emacs := p.positionalArg! "emacs" |>.as! String
  IO.FS.withTempDir fun dir => do
    let pdf := dir / "reader.pdf"
    IO.FS.writeFile pdf Fixtures.reader
    let result ← Shell.run <| Shell.withEnv "VELLUMORY_TEST_PDF" pdf.toString <|
      Shell.withEnv "VELLUMORY_TEST_PROGRAM" (root / ".lake/build/bin/vellumory").toString <|
      Shell.withEnv "XDG_CACHE_HOME" (dir / "cache").toString <|
      Shell.checked (command% emacs ["--batch", "-Q", "-L", "lisp", "-l", "tests/vellumory-tests.el",
        "-f", "ert-run-tests-batch-and-exit"])
    IO.print result.stdout
    IO.eprint result.stderr
  return 0
def testCmd : Cmd := `[Cli|
  test VIA testEmacs; "Run headless Emacs tests against the real Lean worker."
  ARGS:
    emacs : String; "Emacs executable."
]
def rasterAudit (_ : Parsed) : IO UInt32 := do
  let root ← IO.currentDir
  let dir := root / "target/review"
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "reader.pdf") Fixtures.reader
  for (name, extra) in #[("whole", #[]), ("bands", #["-B", "193", "-T", "2"])] do
    let result ← Shell.run <| Shell.checked {
      program := "mutool"
      args := #["draw", "-q", "-c", "rgb", "-r", "72", "-w", "317", "-o",
        (dir / (name ++ ".ppm")).toString] ++ extra ++ #[(dir / "reader.pdf").toString, "1"] }
    IO.eprint result.stderr
  let whole ← IO.FS.readBinFile (dir / "whole.ppm")
  let bands ← IO.FS.readBinFile (dir / "bands.ppm")
  unless whole.size == bands.size do throw <| IO.userError "PPM dimensions differ"
  let mut changed := 0
  let mut largest := 0
  for i in [0:whole.size] do
    let a := whole[i]!.toNat
    let b := bands[i]!.toNat
    if a != b then
      changed := changed + 1
      largest := max largest (max (a-b) (b-a))
  IO.println s!"MuPDF CLI whole vs bands: {changed} channels differ; maximum delta {largest} / 255"
  return 0
def rasterCmd : Cmd := `[Cli|
  raster VIA rasterAudit; "Measure upstream MuPDF banded antialiasing differences."
]
def guiTests (_ : Parsed) : IO UInt32 := do
  let root ← IO.currentDir
  IO.FS.writeFile (root / "target/review/long-reader.pdf") Fixtures.longReader
  let code := s!"(with-selected-window (get-buffer-window \"reader.pdf\" t) (load {repr (root / "tests/gui-tests.el").toString}) (let ((stats (ert-run-tests-batch \"vellumory-gui\"))) (unless (zerop (ert-stats-completed-unexpected stats)) (error \"GUI tests failed\")) (list :passed (ert-stats-completed-expected stats) :unexpected (ert-stats-completed-unexpected stats)))))"
  let result ← Shell.run <| Shell.checked (command% "emacsclient"
    ["--socket-name=vellumory-review", "--eval", code])
  IO.print result.stdout
  return 0
/-- The GUI suite in a graphical Emacs started directly: no X server, Wayland
    compositor or daemon, as on macOS, where Emacs draws with Cocoa. A
    graphical Emacs prints ERT's report to *Messages*, not to standard output,
    so the report, the log and the counts go to target/review/gui-direct.txt;
    on macOS a screenshot of the reader goes to target/review/gui-direct.png.
    The exit status is Emacs's: 0 when every test passed or was skipped. -/
def guiDirect (p : Parsed) : IO UInt32 := do
  let root ← IO.currentDir
  let output := root / "target/review"
  IO.FS.createDirAll output
  IO.FS.writeFile (output / "reader.pdf") Fixtures.reader
  IO.FS.writeFile (output / "long-reader.pdf") Fixtures.longReader
  IO.FS.writeFile (output / "book-reader.pdf") bookReader
  IO.FS.writeFile (output / "slides.pdf") slides
  IO.FS.writeFile (output / "margins.pdf") margins
  let report := output / "gui-direct.txt"
  if ← report.pathExists then IO.FS.removeFile report
  -- Positions saved by an earlier run would open the reader elsewhere than
  -- the suite expects: this run keeps its own, from nothing.
  let state := output / "gui-direct-state"
  if ← state.pathExists then IO.FS.removeDirAll state
  let emacs := p.positionalArg! "emacs" |>.as! String
  -- --eval runs while Emacs starts, before its first frame is shown: windows
  -- have no final size yet. The suite starts from a timer in the command
  -- loop, as an emacsclient request does, once the frame is visible.
  let code := s!"(run-with-timer 1 nil (lambda () (let ((report {repr report.toString}) (shot {repr (output / "gui-direct.png").toString}))
  (setq message-log-max t)
  (condition-case err
      (progn
        (let ((deadline (+ (float-time) 30)))
          (while (and (< (float-time) deadline) (not (frame-visible-p (selected-frame))))
            (sit-for 0.1)))
        (load {repr (root / "tests/gui-init.el").toString})
        (with-current-buffer \"reader.pdf\"
          (let ((deadline (+ (float-time) 60)))
            (while (and (< (float-time) deadline) (not vellumory--pages))
              (accept-process-output nil 0.05) (redisplay t))))
        (sit-for 2)
        (when (executable-find \"screencapture\") (call-process \"screencapture\" nil nil nil \"-x\" shot))
        (with-selected-window (get-buffer-window \"reader.pdf\" t)
          (load {repr (root / "tests/gui-tests.el").toString})
          (let ((stats (ert-run-tests-batch \"vellumory-gui\")))
            (with-temp-file report
              (insert (format \"%s %s: passed %d, unexpected %d, skipped %d\\n\\n\" window-system emacs-version
                              (ert-stats-completed-expected stats) (ert-stats-completed-unexpected stats)
                              (ert-stats-skipped stats)))
              (insert-buffer-substring \"*Messages*\"))
            (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))))
    (error (with-temp-file report
             (insert (format \"error before the suite finished: %S\\n\\n\" err))
             (insert-buffer-substring \"*Messages*\"))
           (kill-emacs 2))))))"
  let result ← IO.Process.output { cmd := emacs, args := #["-Q", "--eval", code],
                                   env := #[("VELLUMORY_REVIEW_ROOT", some root.toString),
                                          ("VELLUMORY_REVIEW_STATE", some state.toString)] }
  IO.print result.stdout
  IO.eprint result.stderr
  if ← report.pathExists then IO.print (← IO.FS.readFile report)
  else IO.println s!"{emacs} wrote no report (exit {result.exitCode})"
  return if result.exitCode == 0 && (← report.pathExists) then 0 else 1
def guiDirectCmd : Cmd := `[Cli|
  guidirect VIA guiDirect; "Run the GUI suite in a graphical Emacs started directly (macOS: no X server)."
  ARGS:
    emacs : String; "A graphical Emacs executable."
]
def guiCmd : Cmd := `[Cli|
  gui VIA guiTests; "Run real redisplay tests through the isolated emacsclient socket."
]
def clickReview (p : Parsed) : IO UInt32 := do
  let x := p.positionalArg! "x" |>.as! Nat
  let y := p.positionalArg! "y" |>.as! Nat
  let button := p.positionalArg! "button" |>.as! Nat
  -- A held modifier, as a user would press it: keydown, click, keyup.
  let modifier := (p.flag? "modifier").map (·.as! String)
  let held := match modifier with
    | some key => ["keydown", key, "mousemove", toString x, toString y, "click", toString button, "keyup", key]
    | none => ["mousemove", toString x, toString y, "click", toString button]
  let result ← Shell.run <| Shell.withEnv "DISPLAY" (← reviewDisplay) <|
    Shell.checked { program := "xdotool", args := held.toArray }
  IO.print result.stdout
  return 0
def clickCmd : Cmd := `[Cli|
  click VIA clickReview; "Send a mouse event on the review display."
  FLAGS:
    m, modifier : String; "Key held during the click, e.g. ctrl."
  ARGS:
    x : Nat; "Root x coordinate."
    y : Nat; "Root y coordinate."
    button : Nat; "Mouse button (4/5 for wheel)."
]
def hoverReview (p : Parsed) : IO UInt32 := do
  let x := p.positionalArg! "x" |>.as! Nat
  let y := p.positionalArg! "y" |>.as! Nat
  let _ ← Shell.run <| Shell.withEnv "DISPLAY" (← reviewDisplay) <| Shell.checked
    (command% "xdotool" ["mousemove", toString x, toString y])
  return 0
def hoverCmd : Cmd := `[Cli|
  hover VIA hoverReview; "Move the pointer on the review display (tooltips, hover help)."
  ARGS:
    x : Nat; "Root x coordinate."
    y : Nat; "Root y coordinate."
]
def focusReview : Shell.Script Unit := do
  let windows ← Shell.checked (command% "xdotool" ["search", "--onlyvisible", "--name", "^Vellumory review$"])
  -- The frame first; a tooltip or popup may share its title while shown.
  let id := ((windows.stdout.trimAscii.toString.splitOn "\n").head?.getD "").trimAscii.toString
  unless id.toNat?.isSome do throw <| IO.userError "Expected a visible review window"
  -- A private Xvfb has no window manager to activate windows: focus directly.
  let activated ← Shell.capture (command% "xdotool" ["windowactivate", "--sync", id])
  unless activated.exitCode == 0 do
    let _ ← Shell.checked (command% "xdotool" ["windowfocus", "--sync", id])

def keyReview (p : Parsed) : IO UInt32 := do
  let keys := p.positionalArg! "keys" |>.as! String
  let display ← reviewDisplay
  -- Wayland: the virtual-keyboard protocol; the compositor focuses the review.
  -- One keyboard for the whole sequence, and a pause first: GTK binds a new
  -- keyboard asynchronously and drops keys that arrive before it has.
  if isWayland display then
    let strokes := (keys.splitOn " ").filter (!·.isEmpty) |>.foldl (fun acc key => acc ++ ["-k", key, "-s", "60"]) []
    Shell.run <| Shell.withEnv "WAYLAND_DISPLAY" display do
      let _ ← Shell.checked { program := "wtype", args := (["-s", "400"] ++ strokes ++ ["-s", "200"]).toArray }
    return 0
  Shell.run <| Shell.withEnv "DISPLAY" display do
    focusReview
    -- A sequence such as "e i" is typed key by key.
    for key in (keys.splitOn " ").filter (!·.isEmpty) do
      let result ← Shell.checked (command% "xdotool" ["key", "--clearmodifiers", key])
      IO.print result.stdout
  return 0
def keyCmd : Cmd := `[Cli|
  key VIA keyReview; "Send keys to the focused review window."
  ARGS:
    keys : String; "xdotool keys separated by spaces, e.g. j, space or 'e i'."
]
def typeReview (p : Parsed) : IO UInt32 := do
  let value := p.positionalArg! "text" |>.as! String
  let display ← reviewDisplay
  if isWayland display then
    Shell.run <| Shell.withEnv "WAYLAND_DISPLAY" display <| do
      let _ ← Shell.checked (command% "wtype" ["-s", "400", "--", value])
    return 0
  Shell.run <| Shell.withEnv "DISPLAY" display do
    focusReview
    let result ← Shell.checked (command% "xdotool" ["type", "--clearmodifiers", "--", value])
    IO.print result.stdout
  return 0
def typeCmd : Cmd := `[Cli|
  type VIA typeReview; "Type literal text into the focused review window."
  ARGS:
    text : String; "Text to type."
]
def requiredEnv (name : String) : IO String := do
  match ← IO.getEnv name with
  | some value => return value
  | none => throw <| IO.userError s!"Missing {name}; use nix develop"

def benchReview (p : Parsed) : IO UInt32 := do
  let root ← IO.currentDir
  let file := p.positionalArg! "file" |>.as! String
  let file := if file.startsWith "/" then file else (root / file).toString
  let code := s!"(progn (select-frame-by-name \"Vellumory review\") (load {repr (root / "tests/bench.el").toString}) (prin1-to-string (vellumory-bench-all {repr file} {repr (← requiredEnv "VELLUMORY_PDF_TOOLS_LISP")} {repr (← requiredEnv "VELLUMORY_TABLIST_LISP")} {repr (← requiredEnv "VELLUMORY_READER_LISP")})))"
  let result ← Shell.run <| Shell.checked (command% "emacsclient" ["--socket-name=vellumory-review", "--eval", code])
  IO.print result.stdout
  return 0
def benchCmd : Cmd := `[Cli|
  bench VIA benchReview; "Compare Vellumory, pdf-tools (page and roll) and DocView in the review frame."
  ARGS:
    file : String; "Document to measure (e.g. target/corpus/pdf32000-2008.pdf)."
]
/-- The home of the Doom review daemon on SOCKET: XDG directories of its own,
    so a review never touches anyone's own Emacs state. -/
def doomHome (root : System.FilePath) (socket : String) : System.FilePath :=
  if socket == "vellumory-doom" then root / "target/doom-home" else root / "target" / s!"doom-home-{socket}"

def doomEnv (root : System.FilePath) (display socket : String) (script : Shell.Script α) : Shell.Script α :=
  let home := doomHome root socket
  Shell.withEnv "DISPLAY" display <| Shell.withEnv "EMACS_SERVER_NAME" socket <|
  Shell.withEnv "XDG_STATE_HOME" (home / "state").toString <|
  Shell.withEnv "XDG_DATA_HOME" (home / "data").toString <|
  Shell.withEnv "XDG_CACHE_HOME" (home / "cache").toString <| script

/-- Start a Doom review daemon on DISPLAY under SOCKET, its frame (named TITLE)
    visiting FILE. -/
def startDoom (root : System.FilePath) (display socket file : String)
    (title := "Vellumory Doom review") : IO String := do
  -- Doom's mode line and icons draw with Symbols Nerd Font: fontconfig finds
  -- it in $XDG_DATA_HOME/fonts, so it goes into this Doom's own data folder.
  let fonts := doomHome root socket / "data/fonts"
  IO.FS.createDirAll fonts
  for font in ← System.FilePath.walkDir (← requiredEnv "VELLUMORY_NERD_FONTS") do
    if font.extension == some "ttf" then
      IO.FS.writeBinFile (fonts / (font.fileName.getD "font.ttf")) (← IO.FS.readBinFile font)
  let emacs := (root / "target/doom-review/bin/emacs").toString
  let client := (root / "target/doom-review/bin/emacsclient").toString
  let result ← Shell.run <| doomEnv root display socket do
    let _ ← Shell.checked (command% emacs [s!"--daemon={socket}"])
    Shell.checked (command% client [s!"--socket-name={socket}", "--create-frame", "--no-wait",
      "--display", display, "--eval",
      s!"(progn (set-frame-name {repr title}) (set-frame-size (selected-frame) 120 48) (find-file {repr file}))"])
  return result.stdout

def doomReview (p : Parsed) : IO UInt32 := do
  let root ← IO.currentDir
  let file := p.positionalArg! "file" |>.as! String
  let file := if file.startsWith "/" then file else (root / file).toString
  IO.print (← startDoom root (← reviewDisplay) (← doomSocket) file)
  return 0

/-- Two versions of a short document, for the comparison screenshot. -/
def draftTypst (revised : Bool) : String :=
  let (verb, extra) := if revised then ("records", " Recorded traces are compiled once and reused.")
                       else ("collects", "")
  s!"#set document(title: \"Field notes\", author: \"Vellumory\")
#set text(lang: \"en\", size: 12pt)
#set page(width: 210mm, height: 297mm, margin: 25mm)
= Field notes
A tracing compiler {verb} the hot loops of a program as it runs.{extra}

== Type specialization
Each trace is compiled for the types seen while it was recorded, and guards
leave the trace when a value has another type.

== Nested loops
An inner loop becomes a trace of its own; the outer loop calls it.
"

/-- The scenes of tests/screenshots.el, in order. -/
def screenshotScenes : List String :=
  ["reading", "search", "contents", "spread", "info", "validation", "history", "diff", "syntax", "menu",
   "reflow", "preview"]

/-- The README's screenshots: each scene of tests/screenshots.el set up in a
    Doom of its own on the review display (a private Xvfb), the frame captured
    and losslessly optimized (oxipng) into docs/screenshots/doom-SCENE.png. -/
def screenshots (p : Parsed) : IO UInt32 := do
  let root ← IO.currentDir
  let display ← reviewDisplay
  let socket := "vellumory-shots"
  let work := root / "target/screenshots"
  -- A neutral, fixed place: headers and Dired show this path in the pictures.
  let docs : System.FilePath := "/tmp/vellumory-papers"
  let out := root / "docs/screenshots"
  -- A fresh start: no documents, histories or positions from an earlier run.
  for dir in [docs, root / "target" / s!"doom-home-{socket}"] do
    if ← dir.pathExists then IO.FS.removeDirAll dir
  IO.FS.createDirAll docs
  IO.FS.createDirAll (work / "raw")
  IO.FS.createDirAll out
  let corpus : System.FilePath := ← requiredEnv "VELLUMORY_CORPUS"
  IO.FS.writeBinFile (docs / "tracemonkey.pdf") (← IO.FS.readBinFile (corpus / "tracemonkey.pdf"))
  IO.FS.writeBinFile (docs / "pdf32000.pdf") (← IO.FS.readBinFile (corpus / "pdf32000-2008.pdf"))
  IO.FS.writeBinFile (docs / "editable.pdf") (← IO.FS.readBinFile (corpus / "tracemonkey.pdf"))
  -- The book the reading scenes show: --book, else the tracemonkey paper.
  let book : System.FilePath := ((p.flag? "book").map (·.as! String)).getD (corpus / "tracemonkey.pdf").toString
  IO.FS.writeBinFile (docs / "book.pdf") (← IO.FS.readBinFile book)
  for (name, revised) in [("draft", false), ("revised", true)] do
    IO.FS.writeFile (docs / s!"{name}.typ") (draftTypst revised)
    let _ ← Shell.run <| Shell.checked (command% "typst"
      ["compile", (docs / s!"{name}.typ").toString, (docs / s!"{name}.pdf").toString])
  let files := ["book", "tracemonkey", "pdf32000", "editable", "draft", "revised"].map fun n =>
    s!"(\"{n}\" . {repr (docs / s!"{n}.pdf").toString})"
  let client := (root / "target/doom-review/bin/emacsclient").toString
  let eval (code : String) : IO Unit := do
    let _ ← Shell.run <| doomEnv root display socket <|
      Shell.checked (command% client [s!"--socket-name={socket}", "--eval", code])
  -- A title of its own: a capture never picks another review frame.
  let title := "Vellumory screenshots"
  let _ ← startDoom root display socket (docs / "tracemonkey.pdf").toString title
  try
    IO.sleep 8000
    eval s!"(progn (setenv \"VELLUMORY_REVIEW_ROOT\" {repr root.toString}) (select-frame-by-name {repr title}) (set-frame-size (selected-frame) 1280 800 t) (set-frame-position (selected-frame) 0 0) (load {repr (root / "tests/screenshots.el").toString} nil t) (setq vellumory-shot-files '({" ".intercalate files})) (setq doom-modeline-buffer-file-name-style 'buffer-name) t)"
    let only := (p.flag? "only").map (·.as! String)
    for scene in screenshotScenes do
      if only.isSome && only != some scene then continue
      eval s!"(progn (select-frame-by-name {repr title}) (vellumory-shot-reset) (vellumory-shot '{scene}) t)"
      let raw := work / "raw" / s!"doom-{scene}.png"
      let _ ← Shell.run <| Shell.withEnv "DISPLAY" display do
        -- On a shared display with a window manager, other windows may cover
        -- the frame: raise it first (no window manager, no need).
        let found ← Shell.capture (command% "xdotool" ["search", "--onlyvisible", "--name", s!"^{title}$"])
        if let some id := (found.stdout.trimAscii.toString.splitOn "\n").head? then
          if id.toNat?.isSome then
            let _ ← Shell.capture (command% "xdotool" ["windowactivate", "--sync", id])
            IO.sleep 400
        Shell.checked (command% "magick" ["import", "-window", title, raw.toString])
      let _ ← Shell.run <| Shell.checked (command% "oxipng"
        ["--quiet", "--opt", "max", "--strip", "safe", "--out", (out / s!"doom-{scene}.png").toString, raw.toString])
      IO.println s!"{scene}: {(← (out / s!"doom-{scene}.png").metadata).byteSize} bytes"
  finally
    let _ ← Shell.run <| doomEnv root display socket <|
      Shell.capture (command% client [s!"--socket-name={socket}", "--eval", "(kill-emacs)"])
  return 0

def screenshotsCmd : Cmd := `[Cli|
  screenshots VIA screenshots; "Capture the README's screenshots in a Doom of their own, optimized into docs/screenshots/."
  FLAGS:
    only : String; "Capture one scene only."
    book : String; "The document the reading scenes show (default: the tracemonkey paper)."
]
/-- The GUI suite inside the Doom review instance: evil, popups, workspaces and
    doom-modeline are all active, unlike the plain `emacs -Q` review. -/
def doomGui (_ : Parsed) : IO UInt32 := do
  let root ← IO.currentDir
  IO.FS.writeFile (root / "target/review/reader.pdf") Fixtures.reader
  IO.FS.writeFile (root / "target/review/long-reader.pdf") Fixtures.longReader
  IO.FS.writeFile (root / "target/review/book-reader.pdf") bookReader
  IO.FS.writeFile (root / "target/review/slides.pdf") slides
  let client := (root / "target/doom-review/bin/emacsclient").toString
  let reader := (root / "target/review/reader.pdf").toString
  let tests := (root / "tests/gui-tests.el").toString
  let code := s!"(progn (setenv \"VELLUMORY_REVIEW_ROOT\" {repr root.toString}) (setq revert-without-query '(\".*\")) (select-frame-by-name \"Vellumory Doom review\") (delete-other-windows) (find-file {repr reader}) (let ((deadline (+ (float-time) 10))) (while (and (< (float-time) deadline) (not vellumory--pages)) (accept-process-output nil 0.05))) (with-selected-window (get-buffer-window \"reader.pdf\" t) (load {repr tests}) (let ((stats (ert-run-tests-batch \"vellumory-gui\"))) (list :passed (ert-stats-completed-expected stats) :unexpected (ert-stats-completed-unexpected stats) :failed (mapcar (lambda (test) (ert-test-name test)) (seq-filter (lambda (test) (not (ert-test-result-expected-p test (ert-test-most-recent-result test)))) (append (ert--stats-tests stats) nil)))))))"
  let socket ← doomSocket
  let result ← Shell.run <| doomEnv root (← reviewDisplay) socket <|
    Shell.checked (command% client [s!"--socket-name={socket}", "--eval", code])
  IO.print result.stdout
  return 0
def doomGuiCmd : Cmd := `[Cli|
  doomgui VIA doomGui; "Run the real-redisplay GUI suite inside the Doom review instance."
]
def doomCmd : Cmd := `[Cli|
  doom VIA doomReview; "Start the reproducible Doom review daemon on :1 and visit a document."
  ARGS:
    file : String; "Document to visit."
]
/-- The Doom frame's title; packages such as org-noter rename the frame they use. -/
def doomWindow (p : Parsed) : String := (p.flag? "window").map (·.as! String) |>.getD "Vellumory Doom review"

def doomScreenshot (p : Parsed) : IO UInt32 := do
  let result ← Shell.run <| Shell.withEnv "DISPLAY" (← reviewDisplay) <|
    Shell.checked (command% "magick" ["import", "-window", doomWindow p, "target/review/doom.png"])
  IO.print result.stdout
  return 0
def doomShotCmd : Cmd := `[Cli|
  doomshot VIA doomScreenshot; "Capture the Doom review frame into target/review/doom.png."
  FLAGS:
    w, window : String; "Frame title (default: Vellumory Doom review)."
]
def doomKeys (p : Parsed) : IO UInt32 := do
  let keys := p.positionalArg! "keys" |>.as! String
  Shell.run <| Shell.withEnv "DISPLAY" (← reviewDisplay) do
    let windows ← Shell.checked (command% "xdotool" ["search", "--onlyvisible", "--name", s!"^{doomWindow p}$"])
    let id := (windows.stdout.trimAscii.toString.splitOn "\n").head!
    -- A private Xvfb (the isolated Doom's :7) has no window manager to
    -- activate windows: focus directly.
    let activated ← Shell.capture (command% "xdotool" ["windowactivate", "--sync", id])
    unless activated.exitCode == 0 do
      let _ ← Shell.checked (command% "xdotool" ["windowfocus", "--sync", id])
    for key in keys.splitOn " " do
      let _ ← Shell.checked (command% "xdotool" ["key", "--clearmodifiers", key])
  return 0
def doomKeysCmd : Cmd := `[Cli|
  doomkeys VIA doomKeys; "Send space-separated xdotool keys to the Doom review frame."
  FLAGS:
    w, window : String; "Frame title (default: Vellumory Doom review)."
  ARGS:
    keys : String; "Keys, e.g. \"space m o\" or \"j j G\"."
]
/-! ## Screen benchmark: the same reading in every viewer, timed on the display

Every viewer runs `tests/screen-bench.el` in a fresh Emacs on a private X
display while ffmpeg records a region of the page area at 200 frames a second,
as checksums with timestamps. An action's latency is the time from its start
until the region first changes; it has settled when the region last changes
before the next action. Viewers drawing outside Emacs (EAF's Qt window) are
measured exactly like those inside. -/

/-- A decimal such as 1793200107.010687. -/
def decimal? (text : String) : Option Float := do
  match text.trimAscii.toString.splitOn "." with
  | [whole] => return (← whole.toNat?).toFloat
  | [whole, fraction] =>
    let digits := (fraction.takeWhile Char.isDigit).toString
    return (← whole.toNat?).toFloat + (← digits.toNat?).toFloat / (10.0 ^ digits.length.toFloat)
  | _ => none

structure Frame where
  time : Float
  hash : String

/-- A framemd5 recording: each frame's wall-clock time (START plus its pts in
    microseconds) and checksum. -/
def readFrames (path : System.FilePath) (start : Float) : IO (Array Frame) := do
  let mut frames := #[]
  for line in (← IO.FS.readFile path).splitOn "\n" do
    let parts := (line.splitOn ",").map (·.trimAscii.toString)
    if line.startsWith "#" || parts.length < 6 then continue
    let some pts := parts[1]!.toNat? | continue
    frames := frames.push { time := start + pts.toFloat / 1e6, hash := parts[5]! }
  return frames

/-- How long after TIME the region first changed, and when it last changed
    before NEXT, in milliseconds; none if it never changed. -/
def response (frames : Array Frame) (time next : Float) : Option (Float × Float) := do
  let base ← (frames.filter (·.time ≤ time)).back?
  let mut previous := base.hash
  let mut first : Option Float := none
  let mut last : Option Float := none
  for frame in frames do
    if frame.time ≤ time || frame.time ≥ next then continue
    if frame.hash != previous then
      if first.isNone then first := some frame.time
      last := some frame.time
      previous := frame.hash
  let f ← first
  let l ← last
  return ((f - time) * 1000, (l - time) * 1000)

def median (values : Array Float) : Option Float :=
  let sorted := values.qsort (· < ·)
  sorted[sorted.size / 2]?

def ms (value : Option Float) : String :=
  match value with
  | some v => let tenths := (v * 10).round.toUInt64.toNat; s!"{tenths / 10}.{tenths % 10}"
  | none => "–"

/-- Seconds since the epoch: the clock of Emacs' `float-time` and of x11grab's timestamps. -/
def wallClock : IO Float := do
  return Float.ofInt (← Std.Time.Timestamp.now).toNanosecondsSinceUnixEpoch.val / 1e9

/-- A recording of DISPLAY's page area into MD5 for SECONDS: 640 × 480 in the
    middle of the page area (never a mode line, header or sidebar), at 200
    frames a second, as timestamped checksums. -/
def startRecording (display : String) (md5 : System.FilePath) (seconds : Nat) :
    IO (IO.Process.Child { stdin := .inherit, stdout := .null, stderr := .piped }) :=
  IO.Process.spawn {
    cmd := "ffmpeg"
    stdout := .null
    stderr := .piped
    args := #["-y", "-hide_banner", "-f", "x11grab", "-draw_mouse", "0", "-framerate", "200",
      "-video_size", "640x480", "-t", toString seconds, "-i", s!"{display}+320,260", "-fps_mode", "passthrough",
      "-f", "framemd5", md5.toString] }

/-- Wait for the recording to end; its frames, timed. -/
def recordedFrames (viewer : String) (recording : IO.Process.Child { stdin := .inherit, stdout := .null, stderr := .piped })
    (md5 : System.FilePath) : IO (Array Frame) := do
  let errors ← recording.stderr.readToEnd
  let _ ← recording.wait
  let start ← match (errors.splitOn "start: ")[1]? >>= fun rest => decimal? ((rest.splitOn ",")[0]!) with
    | some s => pure s
    | none => throw <| IO.userError s!"{viewer}: no recording start in ffmpeg's output"
  readFrames md5 start

/-- Each action's response: until the next action (or for three seconds). -/
def responses (frames : Array Frame) (marks : Array (String × Float)) :
    Array (String × Option (Float × Float)) :=
  (List.range marks.size).toArray.map fun i =>
    let (label, time) := marks[i]!
    let next := (marks[i + 1]?.map (·.2)).getD (time + 3)
    (label, response frames time next)

/-! ### Desktop readers and X input

Readers outside Emacs (GNOME's Papers and Evince, KDE's Okular) run no Lisp:
the harness gives them the script as a user's input with xdotool, and with
`--keys` the viewers in Emacs get the same. The input comes from one xdotool
reading commands on its standard input, each run as its line arrives: a mark
is taken just before the line is written. (A new xdotool for each input took
15–20 ms to start, there, on this server.) -/

/-- Run xdotool on DISPLAY. -/
def xdo (display : String) (args : Array String) : IO String := do
  let out ← IO.Process.output { cmd := "xdotool", args, env := #[("DISPLAY", some display)] }
  unless out.exitCode == 0 do throw <| IO.userError s!"xdotool {" ".intercalate args.toList}: {out.stderr.trimAscii}"
  return out.stdout

/-- Ask ACTION every 50 ms until it answers, for at most SECONDS. -/
def waitFor (what : String) (seconds : Nat) (action : IO (Option α)) : IO α := do
  for _ in [0:seconds * 20] do
    if let some value ← action then return value
    IO.sleep 50
  throw <| IO.userError s!"no {what} after {seconds} s"

/-- Every process with its parent, from /proc. -/
def processParents : IO (Array (Nat × Nat)) := do
  let mut pairs := #[]
  for entry in ← System.FilePath.readDir "/proc" do
    let some pid := entry.fileName.toNat? | continue
    let stat ← try IO.FS.readFile (entry.path / "stat") catch _ => pure ""
    -- After the command's name in parentheses: its state, then its parent.
    let fields := ((stat.splitOn ")").getLast!).trimAscii.toString.splitOn " "
    if let some parent := fields[1]? >>= String.toNat? then pairs := pairs.push (pid, parent)
  return pairs

/-- PID and its descendants. -/
def processTree (pid : Nat) : IO (Array Nat) := do
  let parents ← processParents
  let mut tree := #[pid]
  let mut i := 0
  while i < tree.size do
    let parent := tree[i]!
    tree := tree ++ (parents.filter (·.2 == parent)).map (·.1)
    i := i + 1
  return tree

/-- A process's use of the machine: CPU seconds (user and system, all its
    threads), its resident set now and at its largest, in KiB. -/
structure Usage where
  cpu : Float := 0
  rss : Nat := 0
  peak : Nat := 0

def processUsage (ticks : Float) (pid : Nat) : IO Usage := do
  try
    let stat ← IO.FS.readFile s!"/proc/{pid}/stat"
    -- utime and stime, fields 14 and 15 of proc(5), counted after the name.
    let fields := ((stat.splitOn ")").getLast!).trimAscii.toString.splitOn " "
    let tick (i : Nat) := ((fields[i]? >>= String.toNat?).getD 0).toFloat
    let status ← IO.FS.readFile s!"/proc/{pid}/status"
    let kib (key : String) := ((status.splitOn "\n").findSome? fun line =>
      if line.startsWith key then ((line.drop key.length).trimAscii.toString.splitOn " ")[0]? >>= String.toNat?
      else none).getD 0
    return { cpu := (tick 11 + tick 12) / ticks, rss := kib "VmRSS:", peak := kib "VmHWM:" }
  catch _ => return {}

/-- The use of PID and its descendants, summed. -/
def treeUsage (ticks : Float) (pid : Nat) : IO Usage := do
  let mut total : Usage := {}
  for p in ← processTree pid do
    let u ← processUsage ticks p
    total := { cpu := total.cpu + u.cpu, rss := total.rss + u.rss, peak := total.peak + u.peak }
  return total

/-- During the steps: CPU seconds of the viewer's processes and of the X
    server; the viewer's resident set right after them, three seconds later
    (reading has rested: a viewer may free memory then), and its peak, KiB. -/
structure StepUsage where
  viewer : Float
  server : Option Float
  rss : Nat
  rested : Nat
  peak : Nat

/-- The script of tests/screen-bench.el after opening, as a user's input to
    WINDOW: jumps to pages 5, 12 and 3 through the viewer's page prompt
    (PROMPT, xdotool commands that open it; then the number and Return), thirty
    Down keys, then thirty wheel notches with the pointer resting on the page's
    left margin (never on a link or text). REFOCUS: after a jump the keys stay
    in the page entry until a click on the page. Every input is marked, so that
    a dialog, a click or typing never counts as the previous action's response.
    VIEWER's processes and the X server's (SERVER) are measured over the steps. -/
def readByInput (display window : String) (prompt : Array (Array String)) (refocus : Bool)
    (viewer : Nat) (server : Option Nat) (ticks : Float) : IO (Array (String × Float) × StepUsage) := do
  let xdotool ← IO.Process.spawn {
    cmd := "xdotool"
    args := #["-"]
    env := #[("DISPLAY", some display)]
    stdin := .piped
    stdout := .null
    stderr := .null }
  let (input, xdotool) ← xdotool.takeStdin
  let run (args : Array String) : IO Unit := do
    input.putStrLn (" ".intercalate args.toList)
    input.flush
  let send (label : String) (args : Array String) : IO (String × Float) := do
    let time ← wallClock
    run args
    return (label, time)
  let key (keys : Array String) := #["key", "--delay", "0"] ++ keys
  let rest := #["mousemove", "60", "600"]
  let mut marks := #[← send "focus" #["windowfocus", "--sync", window]]
  IO.sleep 500
  marks := marks.push (← send "pointer" rest)
  IO.sleep 500
  for page in [5, 12, 3] do
    marks := marks.push (← send "prompt" (prompt[0]?.getD #[]))
    for command in prompt.extract 1 prompt.size do
      IO.sleep 100
      run command
    IO.sleep 300
    run (key ((toString page).toList.map (String.singleton ·)).toArray)
    IO.sleep 300
    marks := marks.push (← send s!"jump {page}" (key #["Return"]))
    IO.sleep 2000
    if refocus then
      marks := marks.push (← send "refocus" (rest ++ #["click", "1"]))
      IO.sleep 500
  let before ← treeUsage ticks viewer
  let serverBefore ← server.mapM (processUsage ticks)
  -- Wheel notches 0.6 s apart: closer, Emacs counts them as double and
  -- triple clicks (double-click-time 0.5 s), which Vellumory turns into
  -- longer and longer steps (80 pixels times the count).
  for (label, args, pause) in [("step key", key #["Down"], 300), ("step wheel", #["click", "5"], 600)] do
    for _ in [0:30] do
      marks := marks.push (← send label args)
      IO.sleep pause
  let after ← treeUsage ticks viewer
  let serverAfter ← server.mapM (processUsage ticks)
  IO.sleep 3000
  let rested ← treeUsage ticks viewer
  xdotool.kill
  let _ ← xdotool.wait
  let serverCpu := do return (← serverAfter).cpu - (← serverBefore).cpu
  return (marks, { viewer := after.cpu - before.cpu, server := serverCpu, rss := after.rss,
                   rested := rested.rss, peak := max after.peak rested.peak })

/-- A reader outside Emacs, started as a user starts it, with a fresh home set
    up for the same reading as the viewers in Emacs: fit width, no sidebar, a
    1280 × 1000 window at the top left (Okular's size is keyed to the screen,
    1280x1024 as `xvfb` makes it). -/
structure Desktop where
  name : String
  /-- The environment variable naming its executable (development shell). -/
  program : String
  /-- Files of its home from the document's largest page (points). -/
  files : Float × Float → List (String × String)
  /-- xdotool commands that open its page prompt, its text selected. -/
  prompt : Array (Array String)
  refocus : Bool := false

def desktops : List Desktop := [
  { name := "papers", program := "VELLUMORY_PAPERS", prompt := #[#["key", "ctrl+l"]], refocus := true
    files := fun _ => [("config/glib-2.0/settings/keyfile",
      "[org/gnome/papers/default]\nsizing-mode='fit-width'\nshow-sidebar=false\nwindow-width=1280\nwindow-height=1000\n")] },
  -- With GNOME's metadata store (VELLUMORY_GVFS) a new window is the largest
  -- page times these ratios (shell/ev-window.c, setup_document_from_metadata).
  { name := "evince", program := "VELLUMORY_EVINCE", prompt := #[#["key", "ctrl+l"]]
    files := fun (width, height) => [("config/glib-2.0/settings/keyfile",
      s!"[org/gnome/evince/default]\nsizing-mode='fit-width'\nshow-sidebar=false\nwindow-ratio=({1280 / width}, {1000 / height})\n")] },
  -- The page number in its toolbar: an instant jump (part/minibar.cpp), where
  -- Go to Page (C-g) animates for 200 ms (part/part.cpp). Its messages over
  -- the page ("Loaded a 14-page document") are left out.
  { name := "okular", program := "VELLUMORY_OKULAR", refocus := true
    prompt := #[#["mousemove", "557", "37", "click", "1"], #["key", "ctrl+a"]]
    files := fun _ => [("config/okularrc", "[General]\nShowSidebar=false\n"),
      ("config/okularpartrc", "[General]\nShowOSD=false\n\n[Main View]\nShowLeftPanel=false\n\n[Zoom]\nZoomMode=1\n"),
      ("state/okularstaterc", "[MainWindow]\n1280x1024 screen: Height=1000\n1280x1024 screen: Width=1280\n1280x1024 screen: XPosition=0\n1280x1024 screen: YPosition=0\n")] }]

/-- The largest page width and height of FILE in points, from pdfinfo. -/
def largestPage (file : String) : IO (Float × Float) := do
  let out ← IO.Process.output { cmd := "pdfinfo", args := #["-f", "1", "-l", "1000000", file] }
  let mut size := (0.0, 0.0)
  -- "Page    1 size:  612 x 792 pts (letter)"
  for line in out.stdout.splitOn "\n" do
    let some rest := (line.splitOn " size: ")[1]? | continue
    match (rest.splitOn " ").filter (!·.isEmpty) with
    | w :: "x" :: h :: _ =>
      if let (some w, some h) := (decimal? w, decimal? h) then size := (max size.1 w, max size.2 h)
    | _ => pure ()
  if size.1 == 0 then throw <| IO.userError s!"pdfinfo found no page size in {file}"
  return size

/-- What one run measured: each action's first and last change, and when the
    run was X input, the use of the machine during the steps. -/
structure ScreenRun where
  results : Array (String × Option (Float × Float))
  usage : Option StepUsage := none

/-- Read FILE in desktop reader D on DISPLAY, by X input. -/
def desktopViewer (root : System.FilePath) (display file : String) (d : Desktop) (server : Option Nat)
    (ticks : Float) : IO ScreenRun := do
  let dir := root / "target/review/screen"
  let home := dir / s!"{d.name}-home"
  if ← home.pathExists then IO.FS.removeDirAll home
  for (path, text) in d.files (← largestPage file) do
    let target := home / path
    if let some parent := target.parent then IO.FS.createDirAll parent
    IO.FS.writeFile target text
  let program ← requiredEnv d.program
  -- GNOME's metadata store, as on its desktop, when VELLUMORY_GVFS names it:
  -- "SERVICES:MODULES", gvfs's share/ and a directory holding its GIO client
  -- module libgvfsdbus.so alone (its volume monitors wait for a system bus).
  let gvfs ← match ((← IO.getEnv "VELLUMORY_GVFS").map (·.splitOn ":")) with
    | some [services, modules] => pure #[("GIO_EXTRA_MODULES", some modules),
        ("XDG_DATA_DIRS", some s!"{services}:{(← IO.getEnv "XDG_DATA_DIRS").getD ""}")]
    | some _ => throw <| IO.userError "VELLUMORY_GVFS is not SERVICES:MODULES"
    | none => pure #[]
  let env := #[("DISPLAY", some display), ("GDK_BACKEND", some "x11"), ("QT_QPA_PLATFORM", some "xcb"),
    ("WAYLAND_DISPLAY", none), ("HOME", some home.toString),
    ("XDG_CONFIG_HOME", some (home / "config").toString), ("XDG_DATA_HOME", some (home / "data").toString),
    ("XDG_CACHE_HOME", some (home / "cache").toString), ("XDG_STATE_HOME", some (home / "state").toString),
    ("GSETTINGS_BACKEND", some "keyfile")] ++ gvfs
  let md5 := dir / s!"{d.name}.md5"
  let recording ← startRecording display md5 52
  -- As the script in Emacs rests before it opens.
  IO.sleep 1500
  let opened ← wallClock
  -- A private session bus: no instance elsewhere takes the file over.
  let bus ← IO.Process.spawn {
    cmd := "dbus-run-session"
    args := #["--", program, file]
    env := env
    stdin := .null
    stdout := .null
    stderr := .null }
  let reader : IO (Option Nat) := do
    for pid in ← (·.filterMap fun (p, parent) => if parent == bus.pid.toNat then some p else none) <$> processParents do
      let command ← try IO.FS.readFile s!"/proc/{pid}/cmdline" catch _ => pure ""
      unless ((command.splitOn "\x00").head!).endsWith "dbus-daemon" do return some pid
    return none
  let started ← IO.mkRef (none : Option Nat)
  try
    -- The session's child that is not its bus, and its window.
    let pid ← waitFor s!"{d.name} process" 30 reader
    started.set (some pid)
    let window ← waitFor s!"{d.name} window" 30 do
      let args := #["search", "--onlyvisible", "--pid", toString pid]
      let out ← IO.Process.output { cmd := "xdotool", args, env := #[("DISPLAY", some display)] }
      return (out.stdout.splitOn "\n").head?.filter (·.toNat?.isSome)
    -- Without the metadata store Evince comes up at 600 × 600: sized at
    -- once, its first layout may still be at that size (counted in the open).
    let geometry ← xdo display #["getwindowgeometry", "--shell", window]
    if !(geometry.splitOn "\n").any (·.startsWith "WIDTH=12") then
      let _ ← xdo display #["windowsize", window, "1280", "1000", "windowmove", window, "0", "0"]
    IO.sleep (UInt32.ofNat (5000 - min 4000 (((← wallClock) - opened) * 1000).toUInt64.toNat))
    let (marks, usage) ← readByInput display window d.prompt d.refocus pid server ticks
    let frames ← recordedFrames d.name recording md5
    return { results := responses frames (#[("open", opened)] ++ marks), usage }
  finally
    if let some pid ← started.get then
      let _ ← IO.Process.output { cmd := "kill", args := #[toString pid] }
    let _ ← bus.wait

/-- The actions' times logged by tests/screen-bench.el. -/
def readMarks (log : System.FilePath) : IO (Array (String × Float)) := do
  return (((← IO.FS.readFile log).splitOn "\n").filterMap fun line =>
    match line.splitOn "\t" with
    | [label, time] => (decimal? time).map (label, ·)
    | _ => none).toArray

/-- Read FILE in VIEWER in a fresh EMACS on DISPLAY: the script in Lisp, or
    with INPUT the open in Lisp and the rest as X input (`readByInput`). -/
def screenViewer (root : System.FilePath) (emacs display viewer file : String) (setup : String)
    (input : Bool) (server : Option Nat) (ticks : Float) : IO ScreenRun := do
  let dir := root / "target/review/screen"
  IO.FS.createDirAll dir
  -- One daemon per display: benchmarks on other displays keep theirs.
  let name := s!"vellumory-screen{display.replace ":" ""}"
  let socket := s!"--socket-name={name}"
  let env := #[("DISPLAY", some display), ("GDK_BACKEND", some "x11"), ("WAYLAND_DISPLAY", none),
    ("VELLUMORY_REVIEW_ROOT", some root.toString)]
  let client (code : String) : IO String := do
    let out ← IO.Process.output { cmd := "emacsclient", args := #[socket, "--eval", code] }
    unless out.exitCode == 0 do throw <| IO.userError s!"{viewer}: {out.stderr.trimAscii}"
    return out.stdout
  let daemon ← IO.Process.output { cmd := emacs, args := #["-Q", s!"--daemon={name}"], env }
  unless daemon.exitCode == 0 do throw <| IO.userError s!"{viewer}: the daemon did not start"
  try
    let _ ← IO.Process.output { cmd := "emacsclient", env, args := #[socket,
      "--create-frame", "--no-wait", "--display", display, "--eval",
      "(progn (set-frame-name \"Screen bench\") (tool-bar-mode -1) (menu-bar-mode -1) (scroll-bar-mode -1) (set-frame-position (selected-frame) 0 0) (set-frame-size (selected-frame) 1280 1000 t))"] }
    IO.sleep 2000
    let _ ← client s!"(progn (select-frame-by-name \"Screen bench\") (load {repr (root / "tests/screen-bench.el").toString} nil t) {setup} t)"
    let md5 := dir / s!"{viewer}.md5"
    let log := dir / s!"{viewer}.tsv"
    let recording ← startRecording display md5 (if input then 52 else 27)
    IO.sleep 500
    if input then
      let pid := ((← client "(emacs-pid)").trimAscii.toString.toNat?).getD 0
      let _ ← client s!"(progn (select-frame-by-name \"Screen bench\") (screen-bench-open '{viewer} {repr file} {repr log.toString}))"
      IO.sleep 4000
      let found ← xdo display #["search", "--onlyvisible", "--name", "^Screen bench$"]
      let window := ((found.splitOn "\n").head!).trimAscii.toString
      -- Its page prompt: M-g g, bound to a go-to-page command by Vellumory,
      -- pdf-tools and DocView alike.
      let (marks, usage) ← readByInput display window #[#["key", "alt+g", "g"]] false pid server ticks
      let frames ← recordedFrames viewer recording md5
      return { results := responses frames ((← readMarks log) ++ marks), usage }
    let _ ← client s!"(progn (select-frame-by-name \"Screen bench\") (screen-bench-run '{viewer} {repr file} {repr log.toString}))"
    let frames ← recordedFrames viewer recording md5
    return { results := responses frames (← readMarks log) }
  finally
    let _ ← IO.Process.output { cmd := "emacsclient", args := #[socket, "--eval", "(kill-emacs)"] }

/-- A run's figures: latencies in milliseconds, CPU in milliseconds, memory in MiB. -/
def figures (run : ScreenRun) : List (String × Option Float) :=
  let pick (kind : String) := run.results.filter (·.1.startsWith kind) |>.filterMap (·.2)
  let worst (steps : Array (Float × Float)) :=
    if steps.isEmpty then none else some (steps.foldl (fun m (f, _) => max m f) 0)
  let opened := (pick "open")[0]?
  let jumps := pick "jump"
  let kinds := if run.usage.isSome then [("key", pick "step key"), ("wheel", pick "step wheel")]
    else [("step", pick "step")]
  [("open first", opened.map (·.1)), ("open settled", opened.map (·.2)),
    ("jump first", median (jumps.map (·.1))), ("jump settled", median (jumps.map (·.2)))] ++
  kinds.flatMap (fun (name, steps) => [(s!"{name} first", median (steps.map (·.1))),
    (s!"{name} worst", worst steps), (s!"{name} settled", median (steps.map (·.2)))]) ++
  match run.usage with
  | some u => [("CPU ms", some (u.viewer * 1000)), ("X server CPU ms", u.server.map (· * 1000)),
      ("RSS MiB", some (u.rss.toFloat / 1024)), ("rested RSS MiB", some (u.rested.toFloat / 1024)),
      ("peak RSS MiB", some (u.peak.toFloat / 1024))]
  | none => []

def screenBench (p : Parsed) : IO UInt32 := do
  let root ← IO.currentDir
  let file := p.positionalArg! "file" |>.as! String
  let file := if file.startsWith "/" then file else (root / file).toString
  let emacs := p.positionalArg! "emacs" |>.as! String
  let display := (← IO.getEnv "VELLUMORY_REVIEW_DISPLAY").getD ":7"
  let state := root / "target/review/screen/state"
  IO.FS.createDirAll state
  let pdfTools := s!"(progn (load {repr (root / "tests/bench.el").toString} nil t) (vellumory-bench--pdf-tools-load {repr (← requiredEnv "VELLUMORY_PDF_TOOLS_LISP")} {repr (← requiredEnv "VELLUMORY_TABLIST_LISP")}))"
  let viewers : List (String × String × String) := [
    -- Fit width (its default zoom) in the whole frame, as every other viewer
    -- here: no thumbnail or contents sidebar beside the page (none asked for by
    -- the document, /PageMode, either).
    ("vellumory", emacs, s!"(progn (add-to-list 'load-path {repr (root / "lisp").toString}) (require 'vellumory) (setq vellumory-program {repr (root / ".lake/build/bin/vellumory").toString} vellumory-state-directory {repr state.toString} vellumory-show-thumbnails nil vellumory-follow-document-display nil))"),
    ("pdf-tools", emacs, pdfTools),
    ("pdf-tools-roll", emacs, pdfTools),
    -- Its server writes logs into ~: a home of its own.
    ("pdf-tools-vimura", emacs, s!"(screen-bench-load-vimura {repr (← requiredEnv "VELLUMORY_VIMURA")} {repr (← requiredEnv "VELLUMORY_TABLIST_LISP")} {repr (root / "target/review/screen/vimura-home").toString})"),
    ("doc-view", emacs, "(require 'doc-view)"),
    ("reader", emacs, s!"(progn (add-to-list 'load-path {repr (← requiredEnv "VELLUMORY_READER_LISP")}) (require 'reader))"),
    -- Its default step is 50 pixels; the script's is 96.
    ("doc-tools", emacs, s!"(progn (screen-bench-load-doc-tools {repr (← requiredEnv "VELLUMORY_DOC_TOOLS")}) (setq doc-scroll-step-size 96))"),
    ("eaf", ← requiredEnv "VELLUMORY_EAF_EMACS", "(progn (require 'eaf) (require 'eaf-pdf-viewer))")]
  let only := (p.flag? "only").map (·.as! String)
  let input := p.hasFlag "keys"
  let runs := (p.flag? "runs").map (·.as! Nat) |>.getD 1
  -- CPU time comes in clock ticks; the X server's from its lock file.
  let ticks := ((← IO.Process.output { cmd := "getconf", args := #["CLK_TCK"] }).stdout.trimAscii.toString.toNat?.getD 100).toFloat
  let server ← try pure ((← IO.FS.readFile s!"/tmp/.X{display.drop 1}-lock").trimAscii.toString.toNat?)
    catch _ => pure none
  IO.println "viewer            open first/settled   jump first/settled (median)   step first (median, worst)"
  IO.println "X input: open and jump first/settled; key and wheel steps first (median, worst) / settled (median); CPU of the viewer's processes (and the X server's) over the 60 steps; its resident memory after them (peak)"
  let mut all : Array (String × List (String × Option Float)) := #[]
  for run in [1:runs + 1] do
    if runs > 1 then IO.println s!"run {run}"
    -- Viewers in Emacs (their program and setup), then the desktop readers.
    let jobs : List (String × Sum (String × String) Desktop) :=
      viewers.map (fun (viewer, program, setup) => (viewer, .inl (program, setup))) ++
        desktops.map fun d => (d.name, .inr d)
    for (viewer, job) in jobs do
      if let some names := only then
        unless (names.splitOn ",").contains viewer do continue
      let measure := match job with
        | .inl (program, setup) => screenViewer root program display viewer file setup input server ticks
        | .inr d => desktopViewer root display file d server ticks
      let measured ← try measure catch e => do IO.println s!"{viewer}: {e}"; continue
      all := all.push (viewer, figures measured)
      let results := measured.results
      let pick (kind : String) := results.filter (·.1.startsWith kind) |>.filterMap (·.2)
      let opened := (pick "open")[0]?
      let jumps := pick "jump"
      let column := viewer.pushn ' ' (18 - viewer.length)
      if let some usage := measured.usage then
        let steps (kind : String) :=
          let s := pick kind
          s!"{ms (median (s.map (·.1)))}, {ms (if s.isEmpty then none else some (s.foldl (fun m (f, _) => max m f) 0))} / {ms (median (s.map (·.2)))} ({s.size}/30)"
        IO.println s!"{column}{ms (opened.map (·.1))} / {ms (opened.map (·.2))}   {ms (median (jumps.map (·.1)))} / {ms (median (jumps.map (·.2)))} ({jumps.size}/3)   key {steps "step key"}   wheel {steps "step wheel"}   CPU {ms (some (usage.viewer * 1000))} ms (X {ms (usage.server.map (· * 1000))} ms)   {usage.rss / 1024} MiB, rested {usage.rested / 1024} (peak {usage.peak / 1024})"
      else
        let steps := pick "step"
        let worst := steps.foldl (fun m (f, _) => max m f) 0
        IO.println s!"{column}{ms (opened.map (·.1))} / {ms (opened.map (·.2))}   {ms (median (jumps.map (·.1)))} / {ms (median (jumps.map (·.2)))} ({jumps.size}/3)   {ms (median (steps.map (·.1)))}, {ms (if steps.isEmpty then none else some worst)} ({steps.size}/30)"
      if p.hasFlag "steps" then
        -- Each step's first change, after the garbage collections done before it.
        let gcs (label : String) : String :=
          match (label.splitOn "gc ")[1]? with
          | some count => s!"@{(count.dropEnd 1).toString}"
          | none => ""
        let labelled := results.filter (·.1.startsWith "step") |>.filterMap fun (label, r) => r.map (label, ·.1)
        IO.println s!"  steps (first ms, at garbage collection count): {" ".intercalate (labelled.toList.map fun (label, f) => s!"{ms (some f)}{gcs label}")}"
  -- Over the runs: each figure's range.
  if runs > 1 then
    IO.println s!"ranges over {runs} runs"
    let viewersMeasured := (all.map (·.1)).foldl (fun (seen : Array String) v => if seen.contains v then seen else seen.push v) #[]
    for viewer in viewersMeasured do
      let mine := all.filter (·.1 == viewer) |>.map (·.2)
      let names := (mine[0]?.getD []).map (·.1)
      let range (name : String) :=
        let values := mine.filterMap fun fs => (fs.lookup name).join
        match values[0]? with
        | none => "–"
        | some first =>
          let lo := values.foldl min first
          let hi := values.foldl max first
          if ms (some lo) == ms (some hi) then ms (some lo) else s!"{ms (some lo)}–{ms (some hi)}"
      IO.println s!"{viewer.pushn ' ' (18 - viewer.length)}{", ".intercalate (names.map fun n => s!"{n} {range n}")}"
  return 0

def screenBenchCmd : Cmd := `[Cli|
  screenbench VIA screenBench; "Time the same reading on the display in Vellumory, pdf-tools, DocView, emacs-reader, doc-tools, EAF, and in Papers, Evince and Okular by X input."
  FLAGS:
    only : String; "Measure these viewers only (names separated by commas)."
    steps; "Also list each reading step's first and last change."
    keys; "Viewers in Emacs too get the reading as X input (M-g g, Down, wheel), as the desktop readers do."
    runs : Nat; "Measure every viewer this many times, interleaved, then print each figure's range."
  ARGS:
    file : String; "Document to read."
    emacs : String; "Emacs for the viewers inside Emacs (EAF brings its own)."
]

def cli : Cmd := `[Cli|
  review NOOP; "Lean GUI review harness."
  SUBCOMMANDS:
    doomCmd;
    doomGuiCmd;
    doomShotCmd;
    doomKeysCmd;
    screenshotsCmd;
    benchCmd;
    screenBenchCmd;
    fixtureCmd;
    startCmd;
    xvfbCmd;
    waylandCmd;
    screenshotCmd;
    testCmd;
    rasterCmd;
    guiCmd;
    guiDirectCmd;
    clickCmd;
    hoverCmd;
    keyCmd;
    typeCmd
]
def main (args : List String) : IO UInt32 := cli.validate args
