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
  if h : Hangul.Syllable.base ≤ c ∧ c ≤ Hangul.Syllable.last then
    let s := Hangul.getSyllable c h
    match s.getTChar? with
    | some t => some { tag := none, mapping := [s.getLVChar, t], validMapping := Or.inr (Or.inl rfl) }
    | none => some { tag := none, mapping := [s.getLChar, s.getVChar], validMapping := Or.inr (Or.inl rfl) }
  else
    if h : TableLookupTables.DecompositionMapping.BetweenOrEqStartEnd c then
      TableLookupTables.DecompositionMapping.lookupSparseKVTable? c h
    else
      none

public def lookupDecompositionMapping (c : UInt32) (h : (Hangul.Syllable.base ≤ c ∧ c ≤ Hangul.Syllable.last) ∨ TableLookupTables.DecompositionMapping.BetweenOrEqStartEnd c) : DecompositionMapping :=
  if hhangul : Hangul.Syllable.base ≤ c ∧ c ≤ Hangul.Syllable.last then
    let s := Hangul.getSyllable c hhangul
    match s.getTChar? with
    | some t => { tag := none, mapping := [s.getLVChar, t], validMapping := Or.inr (Or.inl rfl) }
    | none => { tag := none, mapping := [s.getLChar, s.getVChar], vmkUnsafealidMapping := Or.inr (Or.inl rfl) }
  else
    have htable : TableLookupTables.DecompositionMapping.BetweenOrEqStartEnd c := by
      rcases h with h1 | h2
      · exact False.elim (hhangul h1)
      · exact h2
    match hlookup : TableLookupTables.DecompositionMapping.lookupSparseKVTable? c htable with
    | some s => s
    | none => { tag := none, mapping := [], validMapping := Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (by decide)))))))) }
