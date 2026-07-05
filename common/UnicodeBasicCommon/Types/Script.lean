module
public import UnicodeBasicCommon.Types.Script.Common
public import UnicodeBasicCommon.Types.Script.IsValidCodePoint

namespace Unicode

/-!
  ## Scripts ##
-/

/-- Script identifier type -/
public structure Script where
  public code : UInt32
  public is_valid : Script.IsValidCodePoint code := by decide
deriving DecidableEq, Hashable

namespace Script

/-- Default value is `Zyyy` (`Common`) -/
public instance : Inhabited Script where
  default := {
    code := Script.CodePoints.common
    is_valid := by decide
  }

/-- String abbreviation of script -/
@[inline]
public def toAbbrev (s : Script) → String := toAbbrevImpl s.code

/-- Get script from abbreviation -/
@[inline]
public def ofAbbrev? (abbr : String.Slice) : Option Script :=
  match ofAbbrevAux abbr with
  | some code =>
    if h_val : IsValidCodePoint code then
      some ⟨code, h_val⟩
    else
      none
  | none => none

@[inline, inherit_doc ofAbbrev?]
public def ofAbbrev! (abbr : String.Slice) : Script := ofAbbrev? abbr |>.get!

end Script

end Unicode
