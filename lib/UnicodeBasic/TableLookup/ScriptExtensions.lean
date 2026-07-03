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
    let entry := table[find c (fun i => table[i]!.1) 0 table.usize]!
    let v := entry.1
    let scs := entry.2
    let parts := (scs.split ";").toArray
    if parts.size < 2 then
      #[]
    else if c == v then
      let scripts := (parts[1]!.split " ").toArray
      scripts.filterMap fun code =>
        let code := Script.ofAbbrev! code
        if h : Script.isValid code.code then
          some { code := code.code, is_valid := h }
        else
          none
    else
      #[]
where
  table : Array (UInt32 × String) := TableLookupTables.ScriptExtensions.table
