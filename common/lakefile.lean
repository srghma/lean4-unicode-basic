/-
Copyright © 2023-2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

import Lake
open System Lake DSL

package UnicodeBasicCommon where
  description := "Basic Unicode support for Lean 4"
  keywords := #["unicode"]
  reservoir := true
  precompileModules := true

@[default_target]
target UnicodeBasicCommonClib pkg : FilePath := do
  let mut oFiles : Array (Job FilePath) := #[]
  for file in (← (pkg.dir / "UnicodeBasicCommonClib").readDir) do
    if file.path.extension == some "c" then
      let obj := pkg.buildDir / "UnicodeBasicCommonClib" / ((file.fileName.dropSuffix ".c" |>.copy) ++ ".o")
      let src ← inputTextFile file.path
      let weakArgs := #[
        "-I", (← getLeanIncludeDir).toString,
        "-I", (pkg.dir / "UnicodeBasicCommonClib").toString,
        "-O", "-fPIC"
      ]
      oFiles := oFiles.push <| ← buildO obj src weakArgs
  let name := nameToStaticLib "unicodebasiccommonclib"
  buildStaticLib (pkg.sharedLibDir / name) oFiles

extern_lib libunicodebasiccommonclib := UnicodeBasicCommonClib.fetch

@[default_target]
lean_lib UnicodeBasicCommon where
  roots := #[]
  globs := #[
    `UnicodeBasicCommon.CharacterDatabase,
    `UnicodeBasicCommon.Hangul,
    `UnicodeBasicCommon.Types.BidiBracketType,
    `UnicodeBasicCommon.Types.EastAsianWidth,
    `UnicodeBasicCommon.Types.GeneralCategory,
    `UnicodeBasicCommon.Types.Hex,
    `UnicodeBasicCommon.Types.VerticalOrientation,
    `UnicodeBasicCommon.Types.BidiClass,
    `UnicodeBasicCommon.Types.LineBreak,
    `UnicodeBasicCommon.Types.NumericType,
    `UnicodeBasicCommon.Types.DecompositionMapping,
    `UnicodeBasicCommon.Types.Bounds,
    `UnicodeBasicCommon.Types.Script
  ]
  moreLinkObjs := #[UnicodeBasicCommonClib]
