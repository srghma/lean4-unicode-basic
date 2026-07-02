/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.SimpleCaseFolding

namespace Unicode

/-- Get simple case folding of a code point using lookup table -/
public def lookupSimpleCaseFolding (c : UInt32) : UInt32 :=
  let table := table
  if c < table[0]!.1 then c else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (v, m) => if c == v then m else c
where
  table : Array (UInt32 × UInt32) := TableLookupTables.SimpleCaseFolding.table
