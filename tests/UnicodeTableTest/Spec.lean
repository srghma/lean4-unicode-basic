/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import Spec.Tree
import UnicodeBasic
import UnicodeData

open Unicode

namespace UnicodeTableTest

def testAlphabetic (d : UnicodeData) : Bool :=
  let v :=
    if d.gc ∈ [.Lu, .Ll, .Lt, .Lm, .Lo, .Nl] then true
    else PropList.isOtherAlphabetic d.code
  v == lookupAlphabetic d.code

def testBidiClass (d : UnicodeData) : Bool :=
  lookupDerivedBidiClass d.code == lookupBidiClass d.code

def testBidiClassRegressions : Bool :=
  lookupBidiClass 0x0590 == .R
    && lookupBidiClass 0x0600 == .AN
    && lookupBidiClass 0x0860 == .AL
    && lookupBidiClass 0x10D40 == .AN
    && lookupBidiClass 0xFFFF == .BN

def testBidiMirrored (d : UnicodeData) : Bool :=
  d.bidiMirrored == lookupBidiMirrored d.code

def testCanonicalCombiningClass (d : UnicodeData) : Bool :=
  d.cc == lookupCanonicalCombiningClass d.code

partial def testCanonicalDecompositionMapping (d : UnicodeData) : Bool :=
  let m := lookupCanonicalDecompositionMapping d.code
  let l := match d.decomp with
    | some ⟨none, l⟩ => mapping (l.map Char.val)
    | _ => [d.code]
  m == l
where
  mapping : List UInt32 → List UInt32
  | [] => unreachable!
  | c :: cs =>
    let d := getUnicodeData! c
    match d.decomp with
    | some ⟨none, l⟩ => mapping <| l.map Char.val ++ cs
    | _ => c :: cs

def testCased (d : UnicodeData) : Bool :=
  let v :=
    match d.gc with
    | .Lu | .Ll | .Lt => true
    | _ =>
      PropList.isOtherLowercase d.code
        || PropList.isOtherUppercase d.code
  v == lookupCased d.code

def testCaseMapping (d : UnicodeData) : Bool :=
  let (mu, ml, mt) := lookupCaseMapping d.code
  mu == (d.uppercase.map Char.val).getD d.code
    && ml == (d.lowercase.map Char.val).getD d.code
      && mt == (d.titlecase.map Char.val).getD d.code

def testDecompositionMapping (d : UnicodeData) : Bool :=
  d.decomp == lookupDecompositionMapping? d.code

def testDefautlIgnorableCodePoint (d : UnicodeData) : Bool :=
  let v :=
    d.gc == .Cf
      || PropList.isOtherDefaultIgnorableCodePoint d.code
        || PropList.isVariationSelector d.code
  let v := v
    && !(0xFFF9 ≤ d.code && d.code ≤ 0xFFFB)
      && !(0x13430 ≤ d.code && d.code ≤ 0x1343F)
        && !PropList.isWhiteSpace d.code
          && !PropList.isPrependedConcatenationMark d.code
  v == lookupDefaultIgnorableCodePoint d.code

def testGeneralCategory (d : UnicodeData) : Bool :=
  d.gc == lookupGC d.code

def testLowercase (d : UnicodeData) : Bool :=
  let v :=
    match d.gc with
    | .Ll => true
    | _ => PropList.isOtherLowercase d.code
  v == lookupLowercase d.code

def testMath (d : UnicodeData) : Bool :=
  let v :=
    match d.gc with
    | .Sm => true
    | _ => PropList.isOtherMath d.code
  v == lookupMath d.code

def testName (d : UnicodeData) : Bool :=
  d.name == lookupName d.code

def testBlockName : Bool :=
  getBlockName 'A' == "Basic Latin"
    && getBlockName '(' == "Basic Latin"

def testEastAsianWidth : Bool :=
  getEastAsianWidth 'A' == EastAsianWidth.narrow
    && getEastAsianWidth '中' == EastAsianWidth.wide

def testVerticalOrientation : Bool :=
  getVerticalOrientation 'A' == VerticalOrientation.rotated
    && getVerticalOrientation '中' == VerticalOrientation.upright

def testBidiPairedBracket : Bool :=
  getBidiPairedBracket? '(' == some (')'.val)
    && getBidiPairedBracketType? '(' == some BidiBracketType.openBracket
    && getBidiPairedBracket? ')' == some ('('.val)
    && getBidiPairedBracketType? ')' == some BidiBracketType.closeBracket

def testBidiMirroringGlyph : Bool :=
  getBidiMirroringGlyph? '(' == some (')'.val)
    && getBidiMirroringGlyph? '<' == some ('>'.val)

def testNoncharacterCodePoint (d : UnicodeData) : Bool :=
  PropList.isNoncharacterCodePoint d.code == lookupNoncharacterCodePoint d.code

def testDash (d : UnicodeData) : Bool :=
  PropList.isDash d.code == lookupDash d.code

def testHyphen (d : UnicodeData) : Bool :=
  PropList.isHyphen d.code == lookupHyphen d.code

def testQuotationMark (d : UnicodeData) : Bool :=
  PropList.isQuotationMark d.code == lookupQuotationMark d.code

def testTerminalPunctuation (d : UnicodeData) : Bool :=
  PropList.isTerminalPunctuation d.code == lookupTerminalPunctuation d.code

def testExtender (d : UnicodeData) : Bool :=
  PropList.isExtender d.code == lookupExtender d.code

def testRegionalIndicator (d : UnicodeData) : Bool :=
  PropList.isRegionalIndicator d.code == lookupRegionalIndicator d.code

def testIDStart (d : UnicodeData) : Bool :=
  DerivedCoreProperties.isIDStart d.code == lookupIDStart d.code

def testIDContinue (d : UnicodeData) : Bool :=
  DerivedCoreProperties.isIDContinue d.code == lookupIDContinue d.code

def testXIDStart (d : UnicodeData) : Bool :=
  DerivedCoreProperties.isXIDStart d.code == lookupXIDStart d.code

def testXIDContinue (d : UnicodeData) : Bool :=
  DerivedCoreProperties.isXIDContinue d.code == lookupXIDContinue d.code

def testDiacritic (d : UnicodeData) : Bool :=
  PropList.isDiacritic d.code == lookupDiacritic d.code

def testSentenceTerminal (d : UnicodeData) : Bool :=
  PropList.isSentenceTerminal d.code == lookupSentenceTerminal d.code

def testPatternSyntax (d : UnicodeData) : Bool :=
  PropList.isPatternSyntax d.code == lookupPatternSyntax d.code

def testPatternWhiteSpace (d : UnicodeData) : Bool :=
  PropList.isPatternWhiteSpace d.code == lookupPatternWhiteSpace d.code

def testEmoji (d : UnicodeData) : Bool :=
  EmojiData.isEmoji d.code == lookupEmoji d.code

def testEmojiPresentation (d : UnicodeData) : Bool :=
  EmojiData.isEmojiPresentation d.code == lookupEmojiPresentation d.code

def testEmojiModifier (d : UnicodeData) : Bool :=
  EmojiData.isEmojiModifier d.code == lookupEmojiModifier d.code

def testEmojiModifierBase (d : UnicodeData) : Bool :=
  EmojiData.isEmojiModifierBase d.code == lookupEmojiModifierBase d.code

def testEmojiComponent (d : UnicodeData) : Bool :=
  EmojiData.isEmojiComponent d.code == lookupEmojiComponent d.code

def testExtendedPictographic (d : UnicodeData) : Bool :=
  EmojiData.isExtendedPictographic d.code == lookupExtendedPictographic d.code

def testGraphemeBase (d : UnicodeData) : Bool :=
  DerivedCoreProperties.isGraphemeBase d.code == lookupGraphemeBase d.code

def testGraphemeExtend (d : UnicodeData) : Bool :=
  DerivedCoreProperties.isGraphemeExtend d.code == lookupGraphemeExtend d.code

def testNumericValue (d : UnicodeData) : Bool :=
  d.numeric == lookupNumericValue d.code

def testNumericValueRegressions : Bool :=
  lookupNumericValue 0x4E00 == some (.numeric 1 none)
    && lookupNumericValue 0x4E03 == some (.numeric 7 none)
    && lookupNumericValue 0x4E07 == some (.numeric 10000 none)
    && lookupNumericValue 0x5146 == some (.numeric 1000000000000 none)

def testTitlecase (d : UnicodeData) : Bool :=
  let v :=
    match d.gc with
    | .Lt => true
    | _ => false
  v == lookupTitlecase d.code

def testUppercase (d : UnicodeData) : Bool :=
  let v :=
    match d.gc with
    | .Lu => true
    | _ => PropList.isOtherUppercase d.code
  v == lookupUppercase d.code

def testWhiteSpace (d : UnicodeData) : Bool :=
  PropList.isWhiteSpace d.code == lookupWhiteSpace d.code

def itPropertyForData (name : String) (f : UnicodeData → Bool) : Spec.Spec := do
  Spec.it name do
    let stream : UnicodeDataStream := {}
    let mut failedCodes : Array UInt32 := #[]
    for d in stream do
      if !f d then
        failedCodes := failedCodes.push d.code
    if !failedCodes.isEmpty then
      Spec.Assert.fail s!"UnicodeTableTest.{name} failed, {failedCodes.size} codes: {failedCodes}"

def itPropertySimple (name : String) (b : Bool) : Spec.Spec := do
  Spec.it name do
    if !b then
      Spec.Assert.fail s!"UnicodeTableTest.{name} failed"

public def spec : Spec.Spec := do
  Spec.describe "UnicodeTableTest" do
    itPropertyForData "Bidi_Class" testBidiClass
    itPropertySimple "Bidi_Class regressions" testBidiClassRegressions
    itPropertySimple "Block" testBlockName
    itPropertySimple "East_Asian_Width" testEastAsianWidth
    itPropertySimple "Vertical_Orientation" testVerticalOrientation
    itPropertySimple "Bidi_Mirroring_Glyph" testBidiMirroringGlyph
    itPropertyForData "Alphabetic" testAlphabetic
    itPropertySimple "Bidi_Paired_Bracket" testBidiPairedBracket
    itPropertyForData "Bidi_Mirrored" testBidiMirrored
    itPropertyForData "Canonical_Combining_Class" testCanonicalCombiningClass
    itPropertyForData "Canonical_Decomposition_Mapping" testCanonicalDecompositionMapping
    itPropertyForData "Case_Mapping" testCaseMapping
    itPropertyForData "Cased" testCased
    itPropertyForData "Decomposition_Mapping" testDecompositionMapping
    itPropertyForData "Default_Ignorable_Code_Point" testDefautlIgnorableCodePoint
    itPropertyForData "Dash" testDash
    itPropertyForData "Diacritic" testDiacritic
    itPropertyForData "Emoji" testEmoji
    itPropertyForData "Emoji_Component" testEmojiComponent
    itPropertyForData "Emoji_Modifier" testEmojiModifier
    itPropertyForData "Emoji_Modifier_Base" testEmojiModifierBase
    itPropertyForData "Emoji_Presentation" testEmojiPresentation
    itPropertyForData "Extended_Pictographic" testExtendedPictographic
    itPropertyForData "Extender" testExtender
    itPropertyForData "Grapheme_Base" testGraphemeBase
    itPropertyForData "Grapheme_Extend" testGraphemeExtend
    itPropertyForData "Hyphen" testHyphen
    itPropertyForData "ID_Continue" testIDContinue
    itPropertyForData "ID_Start" testIDStart
    itPropertyForData "Lowercase" testLowercase
    itPropertyForData "Math" testMath
    itPropertyForData "Name" testName
    itPropertyForData "Noncharacter_Code_Point" testNoncharacterCodePoint
    itPropertyForData "Pattern_Syntax" testPatternSyntax
    itPropertyForData "Pattern_White_Space" testPatternWhiteSpace
    itPropertyForData "Quotation_Mark" testQuotationMark
    itPropertyForData "Regional_Indicator" testRegionalIndicator
    itPropertyForData "Sentence_Terminal" testSentenceTerminal
    itPropertyForData "Terminal_Punctuation" testTerminalPunctuation
    itPropertyForData "Titlecase" testTitlecase
    itPropertyForData "Uppercase" testUppercase
    itPropertyForData "XID_Continue" testXIDContinue
    itPropertyForData "XID_Start" testXIDStart
    itPropertyForData "Numeric_Value" testNumericValue
    itPropertySimple "Numeric_Value regressions" testNumericValueRegressions
    itPropertyForData "General_Category" testGeneralCategory
    itPropertyForData "White_Space" testWhiteSpace
