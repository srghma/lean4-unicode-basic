module
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
public def toAbbrev : Script → String
  | ⟨c, _⟩ =>
    let c0 := Char.ofUInt8 (c >>> 24).toUInt8
    let c1 := Char.ofUInt8 (c >>> 16).toUInt8
    let c2 := Char.ofUInt8 (c >>> 8).toUInt8
    let c3 := Char.ofUInt8 c.toUInt8
    String.ofList [c0, c1, c2, c3]

@[inline]
public def ofAbbrevAux (abbr : String.Slice) : UInt32 :=
  let b := abbr.copy.toUTF8
  if b.size = 4 then
    (UInt32.ofNat (b.get! 0).toNat <<< 24) |||
    (UInt32.ofNat (b.get! 1).toNat <<< 16) |||
    (UInt32.ofNat (b.get! 2).toNat <<< 8) |||
    UInt32.ofNat (b.get! 3).toNat
  else
    0

/-- Get script from abbreviation -/
@[inline]
public def ofAbbrev? (abbr : String.Slice) : Option Script :=
  if abbr.utf8ByteSize = 4 then
    let code := ofAbbrevAux abbr
    if h : Script.IsValidCodePoint code then
      some ⟨code, h⟩
    else
      none
  else
    none

@[inline, inherit_doc ofAbbrev?]
public def ofAbbrev! (abbr : String.Slice) : Script := ofAbbrev? abbr |>.get!

end Script

end Unicode
