/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.Types.EastAsianWidth
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.EastAsianWidth

namespace Unicode

/-- Get East Asian width for a code point.

  Unicode property: `East_Asian_Width`
-/
public def lookupEastAsianWidth (c : UInt32) : EastAsianWidth :=
  let table := table
  if table.size == 0 || c < table[0]!.1 then .neutral else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, stop, v) => if c ≤ stop then v else .neutral
where
  table : Array (UInt32 × UInt32 × EastAsianWidth) := TableLookupTables.EastAsianWidth.table
