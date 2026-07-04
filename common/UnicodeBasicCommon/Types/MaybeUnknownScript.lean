module
public import UnicodeBasicCommon.Types.Script
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
public def toAbbrev : MaybeUnknownScript → String
  | ⟨c, _⟩ =>
    let c0 := Char.ofUInt8 (c >>> 24).toUInt8
    let c1 := Char.ofUInt8 (c >>> 16).toUInt8
    let c2 := Char.ofUInt8 (c >>> 8).toUInt8
    let c3 := Char.ofUInt8 c.toUInt8
    String.ofList [c0, c1, c2, c3]

/-- Get script from abbreviation, including `Zzzz`. -/
@[inline]
public def ofAbbrev? (abbr : String.Slice) : Option MaybeUnknownScript :=
  if abbr.utf8ByteSize = 4 then
    let code := Script.ofAbbrevAux abbr
    if h : Script.IsValidMaybeUnknownCodePoint code then
      some ⟨code, h⟩
    else
      none
  else
    none

@[inline, inherit_doc ofAbbrev?]
public def ofAbbrev! (abbr : String.Slice) : MaybeUnknownScript := ofAbbrev? abbr |>.get!

end MaybeUnknownScript

end

end Unicode
