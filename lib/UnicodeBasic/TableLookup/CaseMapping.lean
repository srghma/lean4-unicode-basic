/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.CaseMapping

namespace Unicode

/-- Get simple case mappings of a code point using lookup table

  Unicode properties:
    `Simple_Lowercase_Mapping`
    `Simple_Uppercase_Mapping`
    `Simple_Titlecase_Mapping` -/
public def lookupCaseMapping (c : UInt32) : UInt32 × UInt32 × UInt32 :=
  let table : Array (UInt32 × UInt32 × UInt32 × UInt32 × UInt32) := TableLookupTables.CaseMapping.table
  if c < table[0]!.1 then (c, c, c) else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, stop, upper, lower, title) =>
      if c ≤ stop then (upper, lower, title) else (c, c, c)
