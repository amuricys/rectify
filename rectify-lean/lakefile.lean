import Lake
open Lake DSL
open System (FilePath)

package rectify where
  leanOptions := #[⟨`pp.unicode.fun, true⟩, ⟨`pp.proofs.withType, false⟩]
  moreLinkArgs := #["-lwebsockets"]

require scilean from git "https://github.com/amuricys/SciLean.git"

target websockets pkg : FilePath := do
  let oFile := pkg.buildDir / "src" / "Rectify" / "WebSockets.o"
  let srcJob ← inputFile (pkg.dir / "src" / "Rectify" / "WebSockets.c") true
  let flags := #["-I", (← getLeanIncludeDir).toString, "-fPIC"]
  buildO oFile srcJob flags

extern_lib websocketFFI pkg := do
  let name := nameToStaticLib "websocketFFI"
  let ffiO ← fetch <| pkg.target ``websockets
  buildStaticLib (pkg.nativeLibDir / name) #[ffiO]

lean_lib «Rectify» where
  needs := #[websockets]
  srcDir := "src"

@[default_target]
lean_exe "rectify" where
  needs := #[Rectify]
  srcDir := "app"
  root := `Main
