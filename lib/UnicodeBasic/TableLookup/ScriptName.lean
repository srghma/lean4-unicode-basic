/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.Types.Script
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.ScriptName
public import UnicodeBasic.Types.ScriptName

namespace Unicode

-- NOTE:
-- could make code in Script be TableLookupTables.ScriptName.BetweenOrEqStartEnd from beginning,
-- but the table is anyway sparse - so need even better proof IsValid

/-- Get the name of a script

  Unicode property: `Script` -/
public def lookupScriptName (s : Script) : Option ScriptName :=
  if h : TableLookupTables.ScriptName.BetweenOrEqStartEnd s.code then
    TableLookupTables.ScriptName.lookupSparseKVTable? s.code h
  else
    none
