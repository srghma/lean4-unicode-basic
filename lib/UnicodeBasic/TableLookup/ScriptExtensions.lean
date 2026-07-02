/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.Types.Script
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.ScriptExtensions

namespace Unicode

/-- Get script extensions of a code point using lookup table

  Unicode property: `Script_Extensions` -/
public def lookupScriptExtensions (c : UInt32) : Array Script :=
  let table := table
  if c < table[0]!.1 then #[] else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v, scs) =>
      if c ≤ v then
        scs.filterMap fun code =>
          if h : Script.isValid code then
            some { code, is_valid := h }
          else
            none
      else
        #[]
where
  table : Array (UInt32 × UInt32 × Array UInt32) := TableLookupTables.ScriptExtensions.table
