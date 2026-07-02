/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.Types.Script
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.Script

namespace Unicode

/-- Get the script of a code point using lookup table

  Unicode property: `Script` -/
@[inline]
public def lookupScript (c : UInt32) : Script :=
  let table : Array (UInt32 × UInt32 × Script) := TableLookupTables.Script.table
  if c < table[0]!.1 then default else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, stop, script) => if c ≤ stop then script else default
