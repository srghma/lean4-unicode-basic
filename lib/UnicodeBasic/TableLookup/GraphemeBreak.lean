/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.Types.GraphemeClusterBreak
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.GraphemeBreak

namespace Unicode

/-- Get grapheme cluster break property using lookup table -/
public def lookupGraphemeClusterBreak (c : UInt32) : GraphemeClusterBreak :=
  let table := table
  if c < table[0]!.1 then .other else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v, b) => if c ≤ v then b else .other
where
  table : Array (UInt32 × UInt32 × GraphemeClusterBreak) := TableLookupTables.GraphemeBreak.table
