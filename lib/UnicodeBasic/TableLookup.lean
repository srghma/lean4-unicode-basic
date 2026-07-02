/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.CharacterDatabase
public import UnicodeBasicCommon.Hangul
public import UnicodeBasicCommon.Types.BidiBracketType
public import UnicodeBasicCommon.Types.BidiClass
public import UnicodeBasicCommon.Types.Bounds
public import UnicodeBasicCommon.Types.DecompositionMapping
public import UnicodeBasicCommon.Types.EastAsianWidth
public import UnicodeBasicCommon.Types.GeneralCategory
public import UnicodeBasicCommon.Types.Hex
public import UnicodeBasicCommon.Types.LineBreak
public import UnicodeBasicCommon.Types.NumericType
public import UnicodeBasicCommon.Types.VerticalOrientation
public import UnicodeBasic.Types.GraphemeClusterBreak
public import UnicodeBasic.Types.SentenceBreak
public import UnicodeBasic.Types.WordBreak
public import UnicodeBasicCommon.Types.Script
public import UnicodeBasic.TableLookupTables

namespace Unicode

/-- Binary search -/
@[specialize]
private partial def find (c : UInt32) (t : USize → UInt32) (lo hi : USize) : USize :=
  let mid := (lo + hi) / 2
  if lo = mid then
    lo
  else if c < t mid then
    find c t lo mid
  else
    find c t mid hi

private def decodeGeneralCategory (c raw : UInt32) : GC :=
  let gcMask : UInt32 := GC.univ
  let gc : GC := raw &&& gcMask
  let parityBit := (raw >>> 31) &&& 1
  if gc == GC.LC then
    if (c &&& 1) == parityBit then GC.Lu else GC.Ll
  else if gc == GC.PG then
    if (c &&& 1) == parityBit then GC.Ps else GC.Pe
  else if gc == GC.PQ then
    if (c &&& 1) == parityBit then GC.Pi else GC.Pf
  else
    gc

private def lookupPropRange (c : UInt32) (table : Array (UInt32 × UInt32)) : Bool :=
  if table.size == 0 || c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v

-- protected abbrev oUpper : UInt64 := 0x100000000
-- protected abbrev oLower : UInt64 := 0x200000000
-- protected abbrev oAlpha : UInt64 := 0x400000000
-- protected abbrev oMath  : UInt64 := 0x800000000

-- protected def lookupProp (c : UInt32) : UInt64 :=
--   let gcTable : Array (UInt32 × UInt32 × UInt32) := TableLookupTables.GeneralCategory.table
--   let gc :=
--     if c < gcTable[0]!.1 then GC.Cn else
--       match gcTable[find c (fun i => gcTable[i]!.1) 0 gcTable.usize]! with
--       | (_, stop, raw) => if c ≤ stop then decodeGeneralCategory c raw else GC.Cn
--   let bits := gc.toUInt64
--   let bits := if lookupPropRange c TableLookupTables.OtherUppercase.table then bits ||| oUpper else bits
--   let bits := if lookupPropRange c TableLookupTables.OtherLowercase.table then bits ||| oLower else bits
--   let bits := if lookupPropRange c TableLookupTables.OtherAlphabetic.table then bits ||| oAlpha else bits
--   if lookupPropRange c TableLookupTables.OtherMath.table then bits ||| oMath else bits

-- protected def lookupCase (c : UInt32) : UInt64 :=
--   let table : Array (UInt32 × UInt32 × UInt32 × UInt32 × UInt32) := TableLookupTables.CaseMapping.table
--   if c < table[0]!.1 then 0 else
--     match table[find c (fun i => table[i]!.1) 0 table.usize]! with
--     | (_, stop, upper, lower, title) =>
--       if c ≤ stop then
--         upper.toUInt64 ||| ((lower.toUInt64 ||| (title.toUInt64 <<< (21 : UInt64))) <<< (21 : UInt64))
--       else
--         0

/-- Get bidirectional class using the generated lookup table.

  Unicode property: `Bidi_Class` -/
public def lookupBidiClass (c : UInt32) : BidiClass :=
  let table := table
  if c < table[0]!.1 then .L else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (_, v, bc) => if c ≤ v then bc else .L
where
  table : Array (UInt32 × UInt32 × BidiClass) := TableLookupTables.BidiClass.table

/-- Get the bidi mirroring glyph for a code point, if it exists.

  Unicode property: `Bidi_Mirroring_Glyph`
-/
public def lookupBidiMirroringGlyph? (c : UInt32) : Option UInt32 :=
  let table := table
  if table.size == 0 || c < table[0]!.1 then none else
    let d := table[find c (fun i => table[i]!.1) 0 table.usize]!
    if c = d.1 then some d.2 else none
where
  table : Array (UInt32 × UInt32) := TableLookupTables.BidiMirroringGlyph.table

/-!
  ## Bidi Brackets ##
-/

/-- Structure for `BidiBrackets.txt` table rows. -/
public structure BidiBracket where
  pairedBracket : UInt32
  bracketType : BidiBracketType
deriving Inhabited, Repr

/-- Get bidi bracket data for a code point.

  Unicode properties:
    `Bidi_Paired_Bracket`
    `Bidi_Paired_Bracket_Type`
-/
public def lookupBidiBracket? (c : UInt32) : Option BidiBracket :=
  let table := table
  if table.size == 0 || c < table[0]!.1 then none else
    let d := table[find c (fun i => table[i]!.1) 0 table.usize]!
    if c = d.1 then some { pairedBracket := d.2.1, bracketType := d.2.2 } else none
where
  table : Array (UInt32 × UInt32 × BidiBracketType) := TableLookupTables.BidiBrackets.table

/-- Get the bidi paired bracket for a code point. -/
public def lookupBidiPairedBracket? (c : UInt32) : Option UInt32 :=
  (lookupBidiBracket? c).map BidiBracket.pairedBracket

/-- Get the bidi paired bracket type for a code point. -/
public def lookupBidiPairedBracketType? (c : UInt32) : Option BidiBracketType :=
  (lookupBidiBracket? c).map BidiBracket.bracketType

/-- Get block name for a code point.

  Unicode property: `Block`
-/
public def lookupBlockName (c : UInt32) : String :=
  let table := table
  if table.size == 0 || c < table[0]!.1 then "No_Block" else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, stop, name) => if c ≤ stop then name else "No_Block"
where
  table : Array (UInt32 × UInt32 × String) := TableLookupTables.BlockName.table

/-- Get East Asian width for a code point.

  Unicode property: `East_Asian_Width`
-/
public def lookupEastAsianWidth (c : UInt32) : EastAsianWidth :=
  let table := table
  if table.size == 0 || c < table[0]!.1 then .neutral else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, stop, v) => if c ≤ stop then v else .neutral
where
  table : Array (UInt32 × UInt32 × EastAsianWidth) := TableLookupTables.EastAsianWidth.table

/-- Get vertical orientation for a code point.

  Unicode property: `Vertical_Orientation`
-/
public def lookupVerticalOrientation (c : UInt32) : VerticalOrientation :=
  let table := table
  if table.size == 0 || c < table[0]!.1 then .rotated else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, stop, v) => if c ≤ stop then v else .rotated
where
  table : Array (UInt32 × UInt32 × VerticalOrientation) := TableLookupTables.VerticalOrientation.table

/-- Get canonical combining class using lookup table

  Unicode property: `Canonical_Combining_Class` -/
public def lookupCanonicalCombiningClass (c : UInt32) : Nat :=
  let t := table
  if c < t[0]!.1 then 0 else
    match t[find c (fun i => t[i]!.1) 0 t.size.toUSize]! with
    | (_, v, n) => if c ≤ v then n else 0
where
  table : Array (UInt32 × UInt32 × Nat) := TableLookupTables.CanonicalCombiningClass.table

/-- Get canonical decomposition mapping using lookup table

  Unicode properties:
    `Decomposition_Mapping`
    `Decomposition_Type=Canonical` -/
public def lookupCanonicalDecompositionMapping (c : UInt32) : List UInt32 :=
  -- Hangul syllables
  if Hangul.Syllable.base ≤ c && c ≤ Hangul.Syllable.last then
    let s := Hangul.getSyllable! c
    match s.getTChar? with
    | some t => [s.getLChar.val, s.getVChar.val, t.val]
    | none => [s.getLChar.val, s.getVChar.val]
  else
    let table := table
    if c < table[0]!.1 then [c] else
      match table[find c (fun i => table[i]!.1) 0 table.usize]! with
      | (v, l) => if c == v then l else [c]
where
  table : Array (UInt32 × List UInt32) := TableLookupTables.CanonicalDecompositionMapping.table

/-- Get simple case mappings of a code point using lookup table

  Unicode properties:
    `Simple_Lowercase_Mapping`
    `Simple_Uppercase_Mapping`
    `Simple_Titlecase_Mapping` -/
public def lookupCaseMapping (c : UInt32) : UInt32 × UInt32 × UInt32 :=
  let table : Array (UInt32 × UInt32 × UInt32 × UInt32 × UInt32) := TableLookupTables.CaseMapping.table
  if c < table[0]!.1 then (c, c, c) else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, stop, upper, lower, title) =>
      if c ≤ stop then (upper, lower, title) else (c, c, c)

/-- Get decomposition mapping using lookup table

  Unicode properties:
    `Decomposition_Mapping`
    `Decomposition_Type` -/
public def lookupDecompositionMapping? (c : UInt32) : Option DecompositionMapping :=
  -- Hangul syllables
  if Hangul.Syllable.base ≤ c && c ≤ Hangul.Syllable.last then
    let s := Hangul.getSyllable! c
    match s.getTChar? with
    | some t => some ⟨none, [s.getLVChar, t]⟩
    | none => some ⟨none, [s.getLChar, s.getVChar]⟩
  else
    let table := table
    if c < table[0]!.1 then none else
      match table[find c (fun i => table[i]!.1) 0 table.usize]! with
      | (v, t, l) =>
        if c == v then
          some <| .mk t (l.map fun c => Char.ofNat c.toNat).toList
        else
          none
where
  table : Array (UInt32 × Option CompatibilityTag × Array UInt32) := TableLookupTables.DecompositionMapping.table

/-- Get general category of a code point using lookup table

  Unicode property: `General_Category` -/
@[inline]
public def lookupGC (c : UInt32) : GC :=
  let table : Array (UInt32 × UInt32 × UInt32) := TableLookupTables.GeneralCategory.table
  if c < table[0]!.1 then GC.Cn else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, stop, raw) => if c ≤ stop then decodeGeneralCategory c raw else GC.Cn

/-- Get name of a code point using lookup table

  Unicode property: `Name` -/
public def lookupName (c : UInt32) : String :=
  let table := table
  if c < table[0]!.1 then unreachable! else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (_, v, d) =>
      if c ≤ v then
        if "<".isPrefixOf d then
          if d == "<Control>" then
            s!"<control-{toHexStringRaw c}>"
          else if d == "<Private Use>" then
            s!"<private-use-{toHexStringRaw c}>"
          else if d == "<Reserved>" then
            s!"<reserved-{toHexStringRaw c}>"
          else if d == "<Surrogate>" then
            s!"<surrogate-{toHexStringRaw c}>"
          else if d == "<CJK Unified Ideograph>" then
            "CJK UNIFIED IDEOGRAPH-" ++ toHexStringRaw c
          else if d == "<CJK Compatibility Ideograph>" then
            "CJK COMPATIBILITY IDEOGRAPH-" ++ toHexStringRaw c
          else if d == "<Hangul Syllable>" then
            "HANGUL SYLLABLE " ++ (Hangul.getSyllable! c).getShortName
          else if d == "<Khitan Small Script Character>" then
            "KHITAN SMALL SCRIPT CHARACTER-" ++ toHexStringRaw c
          else if d == "<Nushu Character>" then
            "NUSHU CHARACTER-" ++ toHexStringRaw c
          else if d == "<Tangut Component>" then
            let i := if c.toNat < 0x18B00 then
                -- Tangut Component
                toString <| c.toNat - 0x18800 + 1
              else
                -- Tangut Component Supplement
                toString <| c.toNat - 0x18D80 + 769
            let i :=
              if i.length == 1 then "00" ++ i
              else if i.length == 2 then "0" ++ i
              else i
            "TANGUT COMPONENT-" ++ i
          else if d == "<Tangut Ideograph>" then
            "TANGUT IDEOGRAPH-" ++ toHexStringRaw c
          else panic! s!"unknown name range {d}"
        else d
      else s!"<noncharacter-{toHexStringRaw c}>"
where
  table : Array (UInt32 × UInt32 × String) := TableLookupTables.Name.table

/-- Get numeric value of a code point using lookup table.

  Keep this definition in sync with the generated
  `UnicodeBasic.TableLookupTables.NumericValue` module.

  Unicode properties:
    `Numeric_Type`
    `Numeric_Value` -/
public def lookupNumericValue (c : UInt32) : Option NumericType :=
  let table := table
  if c < table[0]!.1 then none else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (c₀, _, .decimal _) =>
      let val := c.toNat - c₀.toNat
      if h : val < 10 then
        some <| NumericType.decimal ⟨val, h⟩
      else
        none
    | (c₀, c₁, .digit i) =>
      if c ≤ c₁ then
        let val := c.toNat - c₀.toNat + i.val
        if h : val < 10 then
          some <| NumericType.digit ⟨val, h⟩
        else
          panic! "invalid `Numeric_Value` table"
      else
        none
    | ⟨v, _, n⟩ =>
      if c == v then some n else none
where
  table : Array (UInt32 × UInt32 × NumericType) := TableLookupTables.NumericValue.table

/-- Get other properties using lookup table

  Unicode properties: `Other_Alphabetic`, `Other_Lowercase`, `Other_Uppercase`, `Other_Math` -/
public def lookupOther (c : UInt32) : UInt32 :=
  let bits := (0 : UInt32)
  let bits := if lookupPropRange c TableLookupTables.OtherUppercase.table then bits ||| 1 else bits
  let bits := if lookupPropRange c TableLookupTables.OtherLowercase.table then bits ||| 2 else bits
  let bits := if lookupPropRange c TableLookupTables.OtherAlphabetic.table then bits ||| 4 else bits
  if lookupPropRange c TableLookupTables.OtherMath.table then bits ||| 8 else bits

/-! Properties -/

/-- Check if code point is alphabetic using lookup table

  Unicode property: `Alphabetic` -/
@[inline]
public def lookupAlphabetic (c : UInt32) : Bool :=
  lookupPropRange c TableLookupTables.Alphabetic.table

/-- Check if code point is bidi mirrored using lookup table

  Unicode property: `Bidi_Mirrored`
-/
public def lookupBidiMirrored (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.size.toUSize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.BidiMirrored.table

/-- Check if code point is a cased letter using lookup table

  Unicode property: `Cased` -/
@[inline]
public def lookupCased (c : UInt32 ) : Bool :=
  lookupPropRange c TableLookupTables.Cased.table

/-- Check if code point is ignorable using lookup table

  Unicode property: `Default_Ignorable_Code_Point` -/
@[inline]
public def lookupDefaultIgnorableCodePoint (c : UInt32 ) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.DefaultIgnorableCodePoint.table

/-- Check if code point is a lowercase letter using lookup table

  Unicode property: `Lowercase` -/
@[inline]
public def lookupLowercase (c : UInt32) : Bool :=
  lookupPropRange c TableLookupTables.Lowercase.table


/-- Check if code point is a mathematical symbol using lookup table

  Unicode property: `Math` -/
@[inline]
public def lookupMath (c : UInt32) : Bool :=
  lookupPropRange c TableLookupTables.Math.table

/-- Check if code point is a noncharcter code point

  Unicode property: `Noncharacter_Code_Point` -/
@[inline]
public def lookupNoncharacterCodePoint (c : UInt32) : Bool :=
  (c ≤ 0xFDEF && 0xFDD0 ≤ c) || (c ≤ Unicode.max && c &&& 0xFFFE == 0xFFFE)

/-- Check if code point is a titlecase letter using lookup table

  Unicode property: `Titlecase` -/
@[inline]
public def lookupTitlecase (c : UInt32) : Bool :=
  lookupGC c == GC.Lt

/-- Check if code point is a uppercase letter using lookup table

  Unicode property: `Uppercase` -/
@[inline]
public def lookupUppercase (c : UInt32) : Bool :=
  lookupPropRange c TableLookupTables.Uppercase.table

/-- Check if code point is a white space character using lookup table

  Unicode property: `White_Space` -/
public def lookupWhiteSpace (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.WhiteSpace.table

/-- Get the script of a code point using lookup table

  Unicode property: `Script` -/
@[inline]
public def lookupScript (c : UInt32) : Script :=
  let table : Array (UInt32 × UInt32 × Script) := TableLookupTables.Script.table
  if c < table[0]!.1 then default else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, stop, script) => if c ≤ stop then script else default


/-- Get the name of a script

  Unicode property: `Script` -/
public def lookupScriptName (s : Script) : Option String :=
  let table := table
  if s.code < table[0]!.1 then none else
    match table[find s.code (fun i => table[i]!.1) 0 table.usize]! with
    | (c, v) => if s.code = c then some v else none
where
  table : Array (UInt32 × String) := TableLookupTables.ScriptName.table

/-- Get script extensions of a code point using lookup table

  Unicode property: `Script_Extensions` -/
public def lookupScriptExtensions (c : UInt32) : Array Script :=
  let table := table
  if c < table[0]!.1 then #[] else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v, scs) =>
      if c ≤ v then
        scs.filterMap fun code =>
          if h : Script.isValid code then
            some { code, is_valid := h }
          else
            none
      else
        #[]
where
  table : Array (UInt32 × UInt32 × Array UInt32) := TableLookupTables.ScriptExtensions.table

/-- Check if code point has ID_Start property using lookup table -/
public def lookupIDStart (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.IdStart.table

/-- Check if code point has ID_Continue property using lookup table -/
public def lookupIDContinue (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.IdContinue.table

/-- Check if code point has XID_Start property using lookup table -/
public def lookupXIDStart (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.XidStart.table

/-- Check if code point has XID_Continue property using lookup table -/
public def lookupXIDContinue (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.XidContinue.table

/-- Check if code point has Dash property using lookup table -/
public def lookupDash (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.Dash.table

/-- Check if code point has Hyphen property using lookup table -/
public def lookupHyphen (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.Hyphen.table

/-- Check if code point has Quotation_Mark property using lookup table -/
public def lookupQuotationMark (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.QuotationMark.table

/-- Check if code point has Terminal_Punctuation property using lookup table -/
public def lookupTerminalPunctuation (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.TerminalPunctuation.table

/-- Check if code point has Extender property using lookup table -/
public def lookupExtender (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.Extender.table

/-- Check if code point has Regional_Indicator property using lookup table -/
public def lookupRegionalIndicator (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.RegionalIndicator.table

/-- Get case folding of a code point using lookup table -/
public def lookupCaseFolding (c : UInt32) : Array UInt32 :=
  let table := table
  if c < table[0]!.1 then #[] else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (v, m) => if c == v then m else #[]
where
  table : Array (UInt32 × Array UInt32) := TableLookupTables.CaseFolding.table

/-- Get simple case folding of a code point using lookup table -/
public def lookupSimpleCaseFolding (c : UInt32) : UInt32 :=
  let table := table
  if c < table[0]!.1 then c else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (v, m) => if c == v then m else c
where
  table : Array (UInt32 × UInt32) := TableLookupTables.SimpleCaseFolding.table

/-- Get grapheme cluster break property using lookup table -/
public def lookupGraphemeClusterBreak (c : UInt32) : GraphemeClusterBreak :=
  let table := table
  if c < table[0]!.1 then .other else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v, b) => if c ≤ v then b else .other
where
  table : Array (UInt32 × UInt32 × GraphemeClusterBreak) := TableLookupTables.GraphemeBreak.table

/-- Get word break property using lookup table -/
public def lookupWordBreak (c : UInt32) : WordBreak :=
  let table := table
  if c < table[0]!.1 then .other else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v, b) => if c ≤ v then b else .other
where
  table : Array (UInt32 × UInt32 × WordBreak) := TableLookupTables.WordBreak.table

/-- Check if code point has Diacritic property using lookup table -/
public def lookupDiacritic (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.Diacritic.table

/-- Check if code point has Sentence_Terminal property using lookup table -/
public def lookupSentenceTerminal (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.SentenceTerminal.table

/-- Check if code point has Pattern_Syntax property using lookup table -/
public def lookupPatternSyntax (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.PatternSyntax.table

/-- Check if code point has Pattern_White_Space property using lookup table -/
public def lookupPatternWhiteSpace (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.PatternWhiteSpace.table

/-- Check if code point has Emoji property using lookup table -/
public def lookupEmoji (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.Emoji.table

/-- Check if code point has Emoji_Presentation property using lookup table -/
public def lookupEmojiPresentation (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.EmojiPresentation.table

/-- Check if code point has Emoji_Modifier property using lookup table -/
public def lookupEmojiModifier (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.EmojiModifier.table

/-- Check if code point has Emoji_Modifier_Base property using lookup table -/
public def lookupEmojiModifierBase (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.EmojiModifierBase.table

/-- Check if code point has Emoji_Component property using lookup table -/
public def lookupEmojiComponent (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.EmojiComponent.table

/-- Check if code point has Extended_Pictographic property using lookup table -/
public def lookupExtendedPictographic (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.ExtendedPictographic.table

/-- Get sentence break property using lookup table -/
public def lookupSentenceBreak (c : UInt32) : SentenceBreak :=
  let table := table
  if c < table[0]!.1 then .other else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v, b) => if c ≤ v then b else .other
where
  table : Array (UInt32 × UInt32 × SentenceBreak) := TableLookupTables.SentenceBreak.table

/-- Get line break property using lookup table -/
public def lookupLineBreak (c : UInt32) : LineBreak :=
  let table := table
  if c < table[0]!.1 then .unknown else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v, b) => if c ≤ v then b else .unknown
where
  table : Array (UInt32 × UInt32 × LineBreak) := TableLookupTables.LineBreak.table

/-- Check if code point has Grapheme_Base property using lookup table -/
public def lookupGraphemeBase (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.GraphemeBase.table

/-- Check if code point has Grapheme_Extend property using lookup table -/
public def lookupGraphemeExtend (c : UInt32) : Bool :=
  let table := table
  if c < table[0]!.1 then false else
    match table[find c (fun i => table[i]!.1) 0 table.usize]! with
    | (_, v) => c ≤ v
where
  table : Array (UInt32 × UInt32) := TableLookupTables.GraphemeExtend.table
