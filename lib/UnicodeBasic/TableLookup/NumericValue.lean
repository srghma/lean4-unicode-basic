/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.Types.NumericType
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.NumericValue

namespace Unicode

/-- Get numeric value of a code point using lookup table.

  Keep this definition in sync with the generated
  `UnicodeBasic.TableLookupTables.NumericValue` module.

  Unicode properties:
    `Numeric_Type`
    `Numeric_Value` -/
public def lookupNumericValue (c : UInt32) : Option NumericType :=
  let table := table
  if c < table[0]!.1 then none else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (c₀, _, .decimal _) =>
      let val := c.toNat - c₀.toNat
      if h : val < 10 then
        some <| NumericType.decimal ⟨val, h⟩
      else
        none
    | (c₀, c₁, .digit i) =>
      if c ≤ c₁ then
        let val := c.toNat - c₀.toNat + i.val
        if h : val < 10 then
          some <| NumericType.digit ⟨val, h⟩
        else
          panic! "invalid `Numeric_Value` table"
      else
        none
    | ⟨v, _, n⟩ =>
      if c == v then some n else none
where
  table : Array (UInt32 × UInt32 × NumericType) := TableLookupTables.NumericValue.table
