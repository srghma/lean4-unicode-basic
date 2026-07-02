/-
Copyright © 2023-2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

import Lake
open System Lake DSL

package UnicodeBasic where
  description := "Basic Unicode support for Lean 4"
  keywords := #["unicode"]
  reservoir := true

require UnicodeBasicCommon from ".."/"common"

@[default_target]
lean_lib UnicodeBasic where
  roots := #[]
  globs := #[
    `UnicodeBasic,
    `UnicodeBasic.Hangul,
    `UnicodeBasic.CharacterDatabase,
    `UnicodeBasic.Types,
    `UnicodeBasic.Types.GraphemeClusterBreak,
    `UnicodeBasic.Types.SentenceBreak,
    `UnicodeBasic.Types.WordBreak,
    `UnicodeBasic.Bidi,
    `UnicodeBasic.Segmentation,
    `UnicodeBasic.Lookup,
    `UnicodeBasic.Lookup.*,
    `UnicodeBasic.TableLookup,
    `UnicodeBasic.TableLookup.*,
    `UnicodeBasic.TableLookupCommon,
    `UnicodeBasic.TableLookupTables.*
  ]
  precompileModules := true
