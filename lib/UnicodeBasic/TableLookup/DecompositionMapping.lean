/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.Types.Hex
public import UnicodeBasicCommon.Hangul
public import UnicodeBasicCommon.Types.DecompositionMapping
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.DecompositionMapping

namespace Unicode

/-- Get decomposition mapping using lookup table

  Unicode properties:
    `Decomposition_Mapping`
    `Decomposition_Type` -/
public def lookupDecompositionMapping? (c : UInt32) : Option DecompositionMapping :=
  -- Hangul syllables
  if Hangul.Syllable.base ≤ c && c ≤ Hangul.Syllable.last then
    let s := Hangul.getSyllable! c
    match s.getTChar? with
    | some t => some ⟨none, [s.getLVChar, t]⟩
    | none => some ⟨none, [s.getLChar, s.getVChar]⟩
  else
    if h : TableLookupTables.DecompositionMapping.BetweenOrEqStartEnd c then
      match TableLookupTables.DecompositionMapping.lookupSparseKVTable? c h with
      | some s =>
        let parts := (s.split ";").toArray
        let tag := Id.run do
          let tag := parts[0]!
          if tag == "" then
            return none
          else if tag == "<font>" then
            return some CompatibilityTag.font
          else if tag == "<noBreak>" then
            return some CompatibilityTag.noBreak
          else if tag == "<initial>" then
            return some CompatibilityTag.initial
          else if tag == "<medial>" then
            return some CompatibilityTag.medial
          else if tag == "<final>" then
            return some CompatibilityTag.final
          else if tag == "<isolated>" then
            return some CompatibilityTag.isolated
          else if tag == "<circle>" then
            return some CompatibilityTag.circle
          else if tag == "<super>" then
            return some CompatibilityTag.super
          else if tag == "<sub>" then
            return some CompatibilityTag.sub
          else if tag == "<vertical>" then
            return some CompatibilityTag.vertical
          else if tag == "<wide>" then
            return some CompatibilityTag.wide
          else if tag == "<narrow>" then
            return some CompatibilityTag.narrow
          else if tag == "<small>" then
            return some CompatibilityTag.small
          else if tag == "<square>" then
            return some CompatibilityTag.square
          else if tag == "<fraction>" then
            return some CompatibilityTag.fraction
          else if tag == "<compat>" then
            return some CompatibilityTag.compat
          else
            return none
        let mapping := (parts.drop 1).toList.map fun s => Char.ofNat (ofHexString! s).toNat
        some ⟨tag, mapping⟩
      | none => none
    else
      none
