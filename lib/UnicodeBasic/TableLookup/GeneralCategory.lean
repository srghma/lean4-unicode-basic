/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.Types.GeneralCategory
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.GeneralCategory

namespace Unicode

/-- Get general category of a code point using lookup table

  Unicode property: `General_Category` -/
@[inline]
public def lookupGC (c : UInt32) : GC :=
  let table : Array (UInt32 × UInt32 × UInt32) := TableLookupTables.GeneralCategory.table
  if c < table[0]!.1 then GC.Cn else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, stop, raw) => if c ≤ stop then decodeGeneralCategory c raw else GC.Cn
