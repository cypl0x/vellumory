import Lake
open System Lake DSL

-- MuPDF keeps no stable ABI. On Linux the worker links the library file of
-- the exact version (its soname); on macOS nixpkgs installs it without one,
-- as libmupdf.dylib. Either way MuPDF refuses a context when the library's
-- version differs from the headers'.
def mupdfLibrary : String := if System.Platform.isOSX then "-lmupdf" else "-l:libmupdf.so.27.2"

package vellumory where
  version := v!"0.1.0"
  moreLeanArgs := #["-DwarningAsError=true"]
  moreLinkArgs := #["-L" ++ (get_config? mupdfLib |>.getD "/usr/lib"),
    "-Wl,-rpath," ++ (get_config? mupdfLib |>.getD "/usr/lib"), mupdfLibrary,
    "-L" ++ (get_config? opensslLib |>.getD "/usr/lib"),
    "-Wl,-rpath," ++ (get_config? opensslLib |>.getD "/usr/lib"), "-lcrypto",
    "-L" ++ (get_config? synctexLib |>.getD "/usr/lib"),
    "-Wl,-rpath," ++ (get_config? synctexLib |>.getD "/usr/lib"), "-lsynctex",
    "-L" ++ (get_config? exempiLib |>.getD "/usr/lib"),
    "-Wl,-rpath," ++ (get_config? exempiLib |>.getD "/usr/lib"), "-lexempi", "-lm"]

-- Prepared from the pinned flake input by scripts/Build.lean, offline.
require Cli from "vendor/Cli"

target mupdf.o pkg : FilePath := do
  let source ← inputTextFile (pkg.dir / "native/mupdf.c")
  buildO (pkg.buildDir / "native/mupdf.o") source
    #["-I", (← getLeanIncludeDir).toString]
    #["-I", get_config? mupdfInclude |>.getD "/usr/include",
      "-I", get_config? opensslInclude |>.getD "/usr/include",
      "-I", get_config? synctexInclude |>.getD "/usr/include",
      "-I", get_config? exempiInclude |>.getD "/usr/include", "-fPIC", "-O2", "-Wall", "-Wextra", "-Werror"]
    "cc" getLeanTrace

extern_lib libvellumoryNative pkg := do
  let object ← mupdf.o.fetch
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "vellumoryNative") #[object]

lean_lib Vellumory

@[default_target]
lean_exe vellumory where
  root := `Main

@[test_driver]
lean_exe vellumoryTests where
  root := `Tests
  extraDepTargets := #[`vellumory]
