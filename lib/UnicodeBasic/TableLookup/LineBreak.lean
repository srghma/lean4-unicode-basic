/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.Types.LineBreak
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.LineBreak

namespace Unicode

/-- Get line break property using lookup table -/
public def lookupLineBreak (c : UInt32) : LineBreak :=
  let table := table
  if c < table[0]!.1 then .unknown else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v, b) => if c ≤ v then b else .unknown
where
  table : Array (UInt32 × UInt32 × LineBreak) := TableLookupTables.LineBreak.table
