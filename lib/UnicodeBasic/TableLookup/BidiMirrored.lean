/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.BidiMirrored

namespace Unicode

/-- Check if code point is bidi mirrored using lookup table

  Unicode property: `Bidi_Mirrored`
-/
public def lookupBidiMirrored (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.BidiMirrored.table
