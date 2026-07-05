module
public import UnicodeBasicCommon.Types.Script.IsValidCodePoint
import Aesop

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

/-- Get script from abbreviation -/
@[inline]
public def ofAbbrev? (abbr : String.Slice) : Option Script :=
  let b := abbr.str.toUTF8
  let off := abbr.startInclusive.offset.byteIdx
  let stop := abbr.endExclusive.offset.byteIdx
  -- Ensure the slice is exactly 4 bytes AND within the bounds of the source string
  if h : off + 4 = stop ∧ stop <= b.size then
    -- Indices are safe because off + 4 = stop and stop <= b.size
    let b0 := b[off]'(by omega)
    let b1 := b[off + 1]'(by omega)
    let b2 := b[off + 2]'(by omega)
    let b3 := b[off + 3]'(by omega)

    let code : UInt32 :=
      (b0.toUInt32 <<< 24) |||
      (b1.toUInt32 <<< 16) |||
      (b2.toUInt32 <<< 8)  |||
       b3.toUInt32

    if h_val : IsValidCodePoint code then
      some ⟨code, h_val⟩
    else
      none
  else
    none

@[inline, inherit_doc ofAbbrev?]
public def ofAbbrev! (abbr : String.Slice) : Script := ofAbbrev? abbr |>.get!

end Script

end Unicode
