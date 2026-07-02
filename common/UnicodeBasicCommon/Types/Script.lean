module

namespace Unicode

/-!
  ## Scripts ##
-/

/-- Check if valid script identifier -/
@[inline]
public def Script.isValid (c : UInt32) : Bool :=
  let c0 := (c >>> 24).toUInt8
  let c1 := (c >>> 16).toUInt8
  let c2 := (c >>> 8).toUInt8
  let c3 := c.toUInt8
  (c0 ≤ 'Z'.toUInt8 && 'A'.toUInt8 ≤ c0)
    && (c1 ≤ 'z'.toUInt8 && 'a'.toUInt8 ≤ c1)
      && (c2 ≤ 'z'.toUInt8 && 'a'.toUInt8 ≤ c2)
        && (c3 ≤ 'z'.toUInt8 && 'a'.toUInt8 ≤ c3)

/-- Script identifier type -/
public structure Script where
  public code : UInt32
  public is_valid : Script.isValid code
deriving DecidableEq, Hashable

namespace Script

/-- Default value is `Zzzz` (`Unknown`) -/
public instance : Inhabited Script where
  default := {
    code := (((('Z'.val <<< 8 ||| 'z'.val) <<< 8) ||| 'z'.val) <<< 8) ||| 'z'.val
    is_valid := by decide
  }

-- TODO: implementation through cpp is not needed. Lean code is ok. Remove cpp.

/-- String abbreviation of script -/
-- @[inline]
@[extern "unicode_basic_common__script_to_abbrev"]
public def toAbbrev : Script → String
  | ⟨c, _⟩ =>
    let c0 := Char.ofUInt8 (c >>> 24).toUInt8
    let c1 := Char.ofUInt8 (c >>> 16).toUInt8
    let c2 := Char.ofUInt8 (c >>> 8).toUInt8
    let c3 := Char.ofUInt8 c.toUInt8
    String.ofList [c0, c1, c2, c3]

-- @[inline]
@[extern "unicode_basic_common__script_of_abbrev"]
public def ofAbbrevAux (abbr : String) : UInt32 :=
  let b := abbr.toUTF8
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
    let code := ofAbbrevAux abbr.toString
    if h : Script.isValid code then
      some ⟨code, h⟩
    else
      none
  else
    none

@[inline, inherit_doc ofAbbrev?]
public def ofAbbrev! (abbr : String.Slice) : Script := ofAbbrev? abbr |>.get!

end Script

end Unicode
