/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.CaseFolding

namespace Unicode

/-- Get case folding of a code point using lookup table -/
public def lookupCaseFolding (c : UInt32) : Array UInt32 :=
  let table := table
  if c < table[0]!.1 then #[] else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (v, m) => if c == v then m else #[]
where
  table : Array (UInt32 × Array UInt32) := TableLookupTables.CaseFolding.table
