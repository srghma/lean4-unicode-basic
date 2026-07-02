/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
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
    let table := table
    if c < table[0]!.1 then none else
      match table[find c (fun i => table[i]!.1) 0 table.usize]! with
      | (v, t, l) =>
        if c == v then
          some <| .mk t (l.map fun c => Char.ofNat c.toNat).toList
        else
          none
where
  table : Array (UInt32 × Option CompatibilityTag × Array UInt32) := TableLookupTables.DecompositionMapping.table
