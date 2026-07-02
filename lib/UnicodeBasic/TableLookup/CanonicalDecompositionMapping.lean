/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.Hangul
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.CanonicalDecompositionMapping

namespace Unicode

/-- Get canonical decomposition mapping using lookup table

  Unicode properties:
    `Decomposition_Mapping`
    `Decomposition_Type=Canonical` -/
public def lookupCanonicalDecompositionMapping (c : UInt32) : List UInt32 :=
  -- Hangul syllables
  if Hangul.Syllable.base ≤ c && c ≤ Hangul.Syllable.last then
    let s := Hangul.getSyllable! c
    match s.getTChar? with
    | some t => [s.getLChar.val, s.getVChar.val, t.val]
    | none => [s.getLChar.val, s.getVChar.val]
  else
    let table := table
    if c < table[0]!.1 then [c] else
      match table[find c (fun i => table[i]!.1) 0 table.usize]! with
      | (v, l) => if c == v then l else [c]
where
  table : Array (UInt32 × List UInt32) := TableLookupTables.CanonicalDecompositionMapping.table
