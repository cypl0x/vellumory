
namespace Vellumory.Shell

/-- An executable and literal arguments, never a string parsed by a shell. -/
structure Command where
  program : String
  args : Array String := #[]
  deriving Repr, BEq

/-- Embedded shell-script DSL. Arguments are ordinary Lean expressions;
    spaces, dollar signs, newlines and semicolons remain literal argument data. -/
syntax "command% " term:max " [" term,* "]" : term
macro_rules
  | `(command% $program [$args,*]) =>
      `(({ program := $program, args := #[$args,*] } : Vellumory.Shell.Command))

structure Context where
  cwd : Option System.FilePath := none
  env : Array (String × Option String) := #[]
  deriving Inhabited

abbrev Script := ReaderT Context IO

def Command.spawnArgs (command : Command) (context : Context) : IO.Process.SpawnArgs :=
  { cmd := command.program, args := command.args, cwd := context.cwd, env := context.env }

def capture (command : Command) : Script IO.Process.Output := do
  if command.program.isEmpty || command.program.contains '\x00'
      || command.args.any (·.contains '\x00') then
    throw <| IO.userError "Executable and arguments must be NUL-free"
  IO.Process.output (command.spawnArgs (← read))

def checked (command : Command) : Script IO.Process.Output := do
  let output ← capture command
  unless output.exitCode == 0 do
    throw <| IO.userError s!"{command.program} exited {output.exitCode}: {output.stderr}"
  return output

def inDirectory (path : System.FilePath) (script : Script α) : Script α :=
  withReader (fun c => { c with cwd := some path }) script

def withEnv (name value : String) (script : Script α) : Script α :=
  withReader (fun c => { c with env := c.env.push (name, some value) }) script

/-- Run SCRIPT with NAME removed from the environment. -/
def withoutEnv (name : String) (script : Script α) : Script α :=
  withReader (fun c => { c with env := c.env.push (name, none) }) script

def run (script : Script α) : IO α := script.run {}

/-- Pure boundary property only: not a theorem about the OS process API. -/
theorem arguments_preserved (c : Command) (ctx : Context) :
    (c.spawnArgs ctx).args = c.args := rfl

end Vellumory.Shell
