/-
Copyright © 2023-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.TableLookup.Script

/-!
  This file uses lookup tables:
  `Script`, `ScriptName`, `ScriptExtensions`.
-/

namespace Unicode

/-!
  ## Script ##
-/

/-- Get character script

  Unicode property: `Script`
-/
@[inline]
public def getScript (char : Char) : Script := lookupScript char.val

/-- Get script name

  Returns `none` if the script code is unassigned.

  Unicode property: `Script`
-/
@[inline]
public def getScriptName? (s : Script) : Option String :=
  lookupScriptName s

/-- Get character script extensions

  Unicode property: `Script_Extensions`
-/
@[inline]
public def getScriptExtensions (char : Char) : Array Script :=
  let scs := lookupScriptExtensions char.val
  if scs.isEmpty then #[lookupScript char.val] else scs

/-- Check if character is part of a script

  This function checks the `Script` property of the character.
-/
@[inline]
public def isScript (sc : Script) (char : Char) : Bool :=
  lookupScript char.val == sc

/-- Check if character has a script extension

  This function checks the `Script_Extensions` property of the character.
  The `Script_Extensions` property includes the `Script` property.
-/
@[inline]
public def hasScript (sc : Script) (char : Char) : Bool :=
  getScriptExtensions char |>.contains sc

end Unicode
