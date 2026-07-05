module
public import UnicodeBasicCommon.Types.Script
public import UnicodeBasicCommon.Types.Script.Common
public import UnicodeBasicCommon.Types.Script.IsValidCodePoint

namespace Unicode

@[expose] public section

/-!
  ## Scripts With Unknown ##
-/

/-- Script identifier type that also admits `Zzzz` (`Unknown`). -/
public structure MaybeUnknownScript where
  public code : UInt32
  public is_valid : Script.IsValidMaybeUnknownCodePoint code := by decide
deriving DecidableEq, Hashable

namespace MaybeUnknownScript

/-- Widen an assigned script to a maybe-unknown script. -/
@[inline]
public def ofScript (s : Script) : MaybeUnknownScript :=
  ⟨s.code, Or.inl s.is_valid⟩

/-- Default value is `Zzzz` (`Unknown`) -/
public instance : Inhabited MaybeUnknownScript where
  default := {
    code := Script.CodePoints.unknown
    is_valid := by decide
  }

/-- String abbreviation of script -/
@[inline]
public def toAbbrev (s : MaybeUnknownScript) → String := toAbbrevImpl s.code

/-- Get script from abbreviation, including `Zzzz`. -/
@[inline]
public def ofAbbrev? (abbr : String.Slice) : Option MaybeUnknownScript :=
  match Script.ofAbbrevAux abbr with
  | some code =>
    if h_val : Script.IsValidMaybeUnknownCodePoint code then
      some ⟨code, h_val⟩
    else
      none
  | none => none

@[inline, inherit_doc ofAbbrev?]
public def ofAbbrev! (abbr : String.Slice) : MaybeUnknownScript := ofAbbrev? abbr |>.get!

end MaybeUnknownScript

end

end Unicode
