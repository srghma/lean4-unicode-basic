/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.Types.BidiClass
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.BidiClass

namespace Unicode

/-- Get bidirectional class using the generated lookup table.

  Unicode property: `Bidi_Class` -/
public def lookupBidiClass (c : UInt32) : BidiClass :=
  let table := table
  if c < table[0]!.1 then .L else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (_, v, bc) => if c ≤ v then bc else .L
where
  table : Array (UInt32 × UInt32 × BidiClass) := TableLookupTables.BidiClass.table
