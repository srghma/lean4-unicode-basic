/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.Types.Script
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.ScriptName

namespace Unicode

/-- Get the name of a script

  Unicode property: `Script` -/
public def lookupScriptName (s : Script) : Option String :=
  let table := table
  if s.code < table[0]!.1 then none else
    match table[find s.code (fun i => table[i]!.1) 0 table.usize]! with
    | (c, v) => if s.code = c then some v else none
where
  table : Array (UInt32 × String) := TableLookupTables.ScriptName.table
