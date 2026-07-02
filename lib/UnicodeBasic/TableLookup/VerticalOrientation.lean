/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.Types.VerticalOrientation
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.VerticalOrientation

namespace Unicode

/-- Get vertical orientation for a code point.

  Unicode property: `Vertical_Orientation`
-/
public def lookupVerticalOrientation (c : UInt32) : VerticalOrientation :=
  let table := table
  if table.size == 0 || c < table[0]!.1 then .rotated else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, stop, v) => if c ≤ stop then v else .rotated
where
  table : Array (UInt32 × UInt32 × VerticalOrientation) := TableLookupTables.VerticalOrientation.table
