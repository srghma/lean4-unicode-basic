/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.BlockName

namespace Unicode

/-- Get block name for a code point.

  Unicode property: `Block`
-/
public def lookupBlockName (c : UInt32) : String :=
  let table := table
  if table.size == 0 || c < table[0]!.1 then "No_Block" else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, stop, name) => if c ≤ stop then name else "No_Block"
where
  table : Array (UInt32 × UInt32 × String) := TableLookupTables.BlockName.table
