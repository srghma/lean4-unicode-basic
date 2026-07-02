/-
Copyright © 2023-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.GeneralApi.Script

/-!
  This file uses lookup tables:
  `Script`.
-/

namespace Unicode

/-- Check if character is Adlam -/
public def isAdlam (char : Char) : Bool := isScript (Script.ofAbbrev! "Adlm") char
/-- Check if character is Ahom -/
public def isAhom (char : Char) : Bool := isScript (Script.ofAbbrev! "Ahom") char
/-- Check if character is Anatolian_Hieroglyphs -/
public def isAnatolianHieroglyphs (char : Char) : Bool := isScript (Script.ofAbbrev! "Hluw") char
/-- Check if character is Arabic -/
public def isArabic (char : Char) : Bool := isScript (Script.ofAbbrev! "Arab") char
/-- Check if character is Armenian -/
public def isArmenian (char : Char) : Bool := isScript (Script.ofAbbrev! "Armn") char
/-- Check if character is Avestan -/
public def isAvestan (char : Char) : Bool := isScript (Script.ofAbbrev! "Avst") char
/-- Check if character is Balinese -/
public def isBalinese (char : Char) : Bool := isScript (Script.ofAbbrev! "Bali") char
/-- Check if character is Bamum -/
public def isBamum (char : Char) : Bool := isScript (Script.ofAbbrev! "Bamu") char
/-- Check if character is Bassa_Vah -/
public def isBassaVah (char : Char) : Bool := isScript (Script.ofAbbrev! "Bass") char
/-- Check if character is Batak -/
public def isBatak (char : Char) : Bool := isScript (Script.ofAbbrev! "Batk") char
/-- Check if character is Bengali -/
public def isBengali (char : Char) : Bool := isScript (Script.ofAbbrev! "Beng") char
/-- Check if character is Beria_Erfe -/
public def isBeriaErfe (char : Char) : Bool := isScript (Script.ofAbbrev! "Berf") char
/-- Check if character is Bhaiksuki -/
public def isBhaiksuki (char : Char) : Bool := isScript (Script.ofAbbrev! "Bhks") char
/-- Check if character is Bopomofo -/
public def isBopomofo (char : Char) : Bool := isScript (Script.ofAbbrev! "Bopo") char
/-- Check if character is Brahmi -/
public def isBrahmi (char : Char) : Bool := isScript (Script.ofAbbrev! "Brah") char
/-- Check if character is Braille -/
public def isBraille (char : Char) : Bool := isScript (Script.ofAbbrev! "Brai") char
/-- Check if character is Buginese -/
public def isBuginese (char : Char) : Bool := isScript (Script.ofAbbrev! "Bugi") char
/-- Check if character is Buhid -/
public def isBuhid (char : Char) : Bool := isScript (Script.ofAbbrev! "Buhd") char
/-- Check if character is Canadian_Aboriginal -/
public def isCanadianAboriginal (char : Char) : Bool := isScript (Script.ofAbbrev! "Cans") char
/-- Check if character is Carian -/
public def isCarian (char : Char) : Bool := isScript (Script.ofAbbrev! "Cari") char
/-- Check if character is Caucasian_Albanian -/
public def isCaucasianAlbanian (char : Char) : Bool := isScript (Script.ofAbbrev! "Aghb") char
/-- Check if character is Chakma -/
public def isChakma (char : Char) : Bool := isScript (Script.ofAbbrev! "Cakm") char
/-- Check if character is Cham -/
public def isCham (char : Char) : Bool := isScript (Script.ofAbbrev! "Cham") char
/-- Check if character is Cherokee -/
public def isCherokee (char : Char) : Bool := isScript (Script.ofAbbrev! "Cher") char
/-- Check if character is Chorasmian -/
public def isChorasmian (char : Char) : Bool := isScript (Script.ofAbbrev! "Chrs") char
/-- Check if character is Common -/
public def isCommon (char : Char) : Bool := isScript (Script.ofAbbrev! "Zyyy") char
/-- Check if character is Coptic -/
public def isCoptic (char : Char) : Bool := isScript (Script.ofAbbrev! "Copt") char
/-- Check if character is Cuneiform -/
public def isCuneiform (char : Char) : Bool := isScript (Script.ofAbbrev! "Xsux") char
/-- Check if character is Cypriot -/
public def isCypriot (char : Char) : Bool := isScript (Script.ofAbbrev! "Cprt") char
/-- Check if character is Cypro_Minoan -/
public def isCyproMinoan (char : Char) : Bool := isScript (Script.ofAbbrev! "Cpmn") char
/-- Check if character is Cyrillic -/
public def isCyrillic (char : Char) : Bool := isScript (Script.ofAbbrev! "Cyrl") char
/-- Check if character is Deseret -/
public def isDeseret (char : Char) : Bool := isScript (Script.ofAbbrev! "Dsrt") char
/-- Check if character is Devanagari -/
public def isDevanagari (char : Char) : Bool := isScript (Script.ofAbbrev! "Deva") char
/-- Check if character is Dives_Akuru -/
public def isDivesAkuru (char : Char) : Bool := isScript (Script.ofAbbrev! "Diak") char
/-- Check if character is Dogra -/
public def isDogra (char : Char) : Bool := isScript (Script.ofAbbrev! "Dogr") char
/-- Check if character is Duployan -/
public def isDuployan (char : Char) : Bool := isScript (Script.ofAbbrev! "Dupl") char
/-- Check if character is Egyptian_Hieroglyphs -/
public def isEgyptianHieroglyphs (char : Char) : Bool := isScript (Script.ofAbbrev! "Egyp") char
/-- Check if character is Elbasan -/
public def isElbasan (char : Char) : Bool := isScript (Script.ofAbbrev! "Elba") char
/-- Check if character is Elymaic -/
public def isElymaic (char : Char) : Bool := isScript (Script.ofAbbrev! "Elym") char
/-- Check if character is Ethiopic -/
public def isEthiopic (char : Char) : Bool := isScript (Script.ofAbbrev! "Ethi") char
/-- Check if character is Garay -/
public def isGaray (char : Char) : Bool := isScript (Script.ofAbbrev! "Gara") char
/-- Check if character is Georgian -/
public def isGeorgian (char : Char) : Bool := isScript (Script.ofAbbrev! "Geor") char
/-- Check if character is Glagolitic -/
public def isGlagolitic (char : Char) : Bool := isScript (Script.ofAbbrev! "Glag") char
/-- Check if character is Gothic -/
public def isGothic (char : Char) : Bool := isScript (Script.ofAbbrev! "Goth") char
/-- Check if character is Grantha -/
public def isGrantha (char : Char) : Bool := isScript (Script.ofAbbrev! "Gran") char
/-- Check if character is Greek -/
public def isGreek (char : Char) : Bool := isScript (Script.ofAbbrev! "Grek") char
/-- Check if character is Gujarati -/
public def isGujarati (char : Char) : Bool := isScript (Script.ofAbbrev! "Gujr") char
/-- Check if character is Gunjala_Gondi -/
public def isGunjalaGondi (char : Char) : Bool := isScript (Script.ofAbbrev! "Gong") char
/-- Check if character is Gurmukhi -/
public def isGurmukhi (char : Char) : Bool := isScript (Script.ofAbbrev! "Guru") char
/-- Check if character is Gurung_Khema -/
public def isGurungKhema (char : Char) : Bool := isScript (Script.ofAbbrev! "Gukh") char
/-- Check if character is Han -/
public def isHan (char : Char) : Bool := isScript (Script.ofAbbrev! "Hani") char
/-- Check if character is Hangul -/
public def isHangul (char : Char) : Bool := isScript (Script.ofAbbrev! "Hang") char
/-- Check if character is Hanifi_Rohingya -/
public def isHanifiRohingya (char : Char) : Bool := isScript (Script.ofAbbrev! "Rohg") char
/-- Check if character is Hanunoo -/
public def isHanunoo (char : Char) : Bool := isScript (Script.ofAbbrev! "Hano") char
/-- Check if character is Hatran -/
public def isHatran (char : Char) : Bool := isScript (Script.ofAbbrev! "Hatr") char
/-- Check if character is Hebrew -/
public def isHebrew (char : Char) : Bool := isScript (Script.ofAbbrev! "Hebr") char
/-- Check if character is Hiragana -/
public def isHiragana (char : Char) : Bool := isScript (Script.ofAbbrev! "Hira") char
/-- Check if character is Imperial_Aramaic -/
public def isImperialAramaic (char : Char) : Bool := isScript (Script.ofAbbrev! "Armi") char
/-- Check if character is Inherited -/
public def isInherited (char : Char) : Bool := isScript (Script.ofAbbrev! "Zinh") char
/-- Check if character is Inscriptional_Pahlavi -/
public def isInscriptionalPahlavi (char : Char) : Bool := isScript (Script.ofAbbrev! "Phli") char
/-- Check if character is Inscriptional_Parthian -/
public def isInscriptionalParthian (char : Char) : Bool := isScript (Script.ofAbbrev! "Prti") char
/-- Check if character is Javanese -/
public def isJavanese (char : Char) : Bool := isScript (Script.ofAbbrev! "Java") char
/-- Check if character is Kaithi -/
public def isKaithi (char : Char) : Bool := isScript (Script.ofAbbrev! "Kthi") char
/-- Check if character is Kannada -/
public def isKannada (char : Char) : Bool := isScript (Script.ofAbbrev! "Knda") char
/-- Check if character is Katakana -/
public def isKatakana (char : Char) : Bool := isScript (Script.ofAbbrev! "Kana") char
/-- Check if character is Katakana_Or_Hiragana -/
public def isKatakanaOrHiragana (char : Char) : Bool := isScript (Script.ofAbbrev! "Hrkt") char
/-- Check if character is Kawi -/
public def isKawi (char : Char) : Bool := isScript (Script.ofAbbrev! "Kawi") char
/-- Check if character is Kayah_Li -/
public def isKayahLi (char : Char) : Bool := isScript (Script.ofAbbrev! "Kali") char
/-- Check if character is Kharoshthi -/
public def isKharoshthi (char : Char) : Bool := isScript (Script.ofAbbrev! "Khar") char
/-- Check if character is Khitan_Small_Script -/
public def isKhitanSmallScript (char : Char) : Bool := isScript (Script.ofAbbrev! "Kits") char
/-- Check if character is Khmer -/
public def isKhmer (char : Char) : Bool := isScript (Script.ofAbbrev! "Khmr") char
/-- Check if character is Khojki -/
public def isKhojki (char : Char) : Bool := isScript (Script.ofAbbrev! "Khoj") char
/-- Check if character is Khudawadi -/
public def isKhudawadi (char : Char) : Bool := isScript (Script.ofAbbrev! "Sind") char
/-- Check if character is Kirat_Rai -/
public def isKiratRai (char : Char) : Bool := isScript (Script.ofAbbrev! "Krai") char
/-- Check if character is Lao -/
public def isLao (char : Char) : Bool := isScript (Script.ofAbbrev! "Laoo") char
/-- Check if character is Latin -/
public def isLatin (char : Char) : Bool := isScript (Script.ofAbbrev! "Latn") char
/-- Check if character is Lepcha -/
public def isLepcha (char : Char) : Bool := isScript (Script.ofAbbrev! "Lepc") char
/-- Check if character is Limbu -/
public def isLimbu (char : Char) : Bool := isScript (Script.ofAbbrev! "Limb") char
/-- Check if character is Linear_A -/
public def isLinearA (char : Char) : Bool := isScript (Script.ofAbbrev! "Lina") char
/-- Check if character is Linear_B -/
public def isLinearB (char : Char) : Bool := isScript (Script.ofAbbrev! "Linb") char
/-- Check if character is Lisu -/
public def isLisu (char : Char) : Bool := isScript (Script.ofAbbrev! "Lisu") char
/-- Check if character is Lycian -/
public def isLycian (char : Char) : Bool := isScript (Script.ofAbbrev! "Lyci") char
/-- Check if character is Lydian -/
public def isLydian (char : Char) : Bool := isScript (Script.ofAbbrev! "Lydi") char
/-- Check if character is Mahajani -/
public def isMahajani (char : Char) : Bool := isScript (Script.ofAbbrev! "Mahj") char
/-- Check if character is Makasar -/
public def isMakasar (char : Char) : Bool := isScript (Script.ofAbbrev! "Maka") char
/-- Check if character is Malayalam -/
public def isMalayalam (char : Char) : Bool := isScript (Script.ofAbbrev! "Mlym") char
/-- Check if character is Mandaic -/
public def isMandaic (char : Char) : Bool := isScript (Script.ofAbbrev! "Mand") char
/-- Check if character is Manichaean -/
public def isManichaean (char : Char) : Bool := isScript (Script.ofAbbrev! "Mani") char
/-- Check if character is Marchen -/
public def isMarchen (char : Char) : Bool := isScript (Script.ofAbbrev! "Marc") char
/-- Check if character is Masaram_Gondi -/
public def isMasaramGondi (char : Char) : Bool := isScript (Script.ofAbbrev! "Gonm") char
/-- Check if character is Medefaidrin -/
public def isMedefaidrin (char : Char) : Bool := isScript (Script.ofAbbrev! "Medf") char
/-- Check if character is Meetei_Mayek -/
public def isMeeteiMayek (char : Char) : Bool := isScript (Script.ofAbbrev! "Mtei") char
/-- Check if character is Mende_Kikakui -/
public def isMendeKikakui (char : Char) : Bool := isScript (Script.ofAbbrev! "Mend") char
/-- Check if character is Meroitic_Cursive -/
public def isMeroiticCursive (char : Char) : Bool := isScript (Script.ofAbbrev! "Merc") char
/-- Check if character is Meroitic_Hieroglyphs -/
public def isMeroiticHieroglyphs (char : Char) : Bool := isScript (Script.ofAbbrev! "Mero") char
/-- Check if character is Miao -/
public def isMiao (char : Char) : Bool := isScript (Script.ofAbbrev! "Plrd") char
/-- Check if character is Modi -/
public def isModi (char : Char) : Bool := isScript (Script.ofAbbrev! "Modi") char
/-- Check if character is Mongolian -/
public def isMongolian (char : Char) : Bool := isScript (Script.ofAbbrev! "Mong") char
/-- Check if character is Mro -/
public def isMro (char : Char) : Bool := isScript (Script.ofAbbrev! "Mroo") char
/-- Check if character is Multani -/
public def isMultani (char : Char) : Bool := isScript (Script.ofAbbrev! "Mult") char
/-- Check if character is Myanmar -/
public def isMyanmar (char : Char) : Bool := isScript (Script.ofAbbrev! "Mymr") char
/-- Check if character is Nabataean -/
public def isNabataean (char : Char) : Bool := isScript (Script.ofAbbrev! "Nbat") char
/-- Check if character is Nag_Mundari -/
public def isNagMundari (char : Char) : Bool := isScript (Script.ofAbbrev! "Nagm") char
/-- Check if character is Nandinagari -/
public def isNandinagari (char : Char) : Bool := isScript (Script.ofAbbrev! "Nand") char
/-- Check if character is New_Tai_Lue -/
public def isNewTaiLue (char : Char) : Bool := isScript (Script.ofAbbrev! "Talu") char
/-- Check if character is Newa -/
public def isNewa (char : Char) : Bool := isScript (Script.ofAbbrev! "Newa") char
/-- Check if character is Nko -/
public def isNko (char : Char) : Bool := isScript (Script.ofAbbrev! "Nkoo") char
/-- Check if character is Nushu -/
public def isNushu (char : Char) : Bool := isScript (Script.ofAbbrev! "Nshu") char
/-- Check if character is Nyiakeng_Puachue_Hmong -/
public def isNyiakengPuachueHmong (char : Char) : Bool := isScript (Script.ofAbbrev! "Hmnp") char
/-- Check if character is Ogham -/
public def isOgham (char : Char) : Bool := isScript (Script.ofAbbrev! "Ogam") char
/-- Check if character is Ol_Chiki -/
public def isOlChiki (char : Char) : Bool := isScript (Script.ofAbbrev! "Olck") char
/-- Check if character is Ol_Onal -/
public def isOlOnal (char : Char) : Bool := isScript (Script.ofAbbrev! "Onao") char
/-- Check if character is Old_Hungarian -/
public def isOldHungarian (char : Char) : Bool := isScript (Script.ofAbbrev! "Hung") char
/-- Check if character is Old_Italic -/
public def isOldItalic (char : Char) : Bool := isScript (Script.ofAbbrev! "Ital") char
/-- Check if character is Old_North_Arabian -/
public def isOldNorthArabian (char : Char) : Bool := isScript (Script.ofAbbrev! "Narb") char
/-- Check if character is Old_Permic -/
public def isOldPermic (char : Char) : Bool := isScript (Script.ofAbbrev! "Perm") char
/-- Check if character is Old_Persian -/
public def isOldPersian (char : Char) : Bool := isScript (Script.ofAbbrev! "Xpeo") char
/-- Check if character is Old_Sogdian -/
public def isOldSogdian (char : Char) : Bool := isScript (Script.ofAbbrev! "Sogo") char
/-- Check if character is Old_South_Arabian -/
public def isOldSouthArabian (char : Char) : Bool := isScript (Script.ofAbbrev! "Sarb") char
/-- Check if character is Old_Turkic -/
public def isOldTurkic (char : Char) : Bool := isScript (Script.ofAbbrev! "Orkh") char
/-- Check if character is Old_Uyghur -/
public def isOldUyghur (char : Char) : Bool := isScript (Script.ofAbbrev! "Ougr") char
/-- Check if character is Oriya -/
public def isOriya (char : Char) : Bool := isScript (Script.ofAbbrev! "Orya") char
/-- Check if character is Osage -/
public def isOsage (char : Char) : Bool := isScript (Script.ofAbbrev! "Osge") char
/-- Check if character is Osmanya -/
public def isOsmanya (char : Char) : Bool := isScript (Script.ofAbbrev! "Osma") char
/-- Check if character is Pahawh_Hmong -/
public def isPahawhHmong (char : Char) : Bool := isScript (Script.ofAbbrev! "Hmng") char
/-- Check if character is Palmyrene -/
public def isPalmyrene (char : Char) : Bool := isScript (Script.ofAbbrev! "Palm") char
/-- Check if character is Pau_Cin_Hau -/
public def isPauCinHau (char : Char) : Bool := isScript (Script.ofAbbrev! "Pauc") char
/-- Check if character is Phags_Pa -/
public def isPhagsPa (char : Char) : Bool := isScript (Script.ofAbbrev! "Phag") char
/-- Check if character is Phoenician -/
public def isPhoenician (char : Char) : Bool := isScript (Script.ofAbbrev! "Phnx") char
/-- Check if character is Psalter_Pahlavi -/
public def isPsalterPahlavi (char : Char) : Bool := isScript (Script.ofAbbrev! "Phlp") char
/-- Check if character is Rejang -/
public def isRejang (char : Char) : Bool := isScript (Script.ofAbbrev! "Rjng") char
/-- Check if character is Runic -/
public def isRunic (char : Char) : Bool := isScript (Script.ofAbbrev! "Runr") char
/-- Check if character is Samaritan -/
public def isSamaritan (char : Char) : Bool := isScript (Script.ofAbbrev! "Samr") char
/-- Check if character is Saurashtra -/
public def isSaurashtra (char : Char) : Bool := isScript (Script.ofAbbrev! "Saur") char
/-- Check if character is Sharada -/
public def isSharada (char : Char) : Bool := isScript (Script.ofAbbrev! "Shrd") char
/-- Check if character is Shavian -/
public def isShavian (char : Char) : Bool := isScript (Script.ofAbbrev! "Shaw") char
/-- Check if character is Siddham -/
public def isSiddham (char : Char) : Bool := isScript (Script.ofAbbrev! "Sidd") char
/-- Check if character is Sidetic -/
public def isSidetic (char : Char) : Bool := isScript (Script.ofAbbrev! "Sidt") char
/-- Check if character is SignWriting -/
public def isSignWriting (char : Char) : Bool := isScript (Script.ofAbbrev! "Sgnw") char
/-- Check if character is Sinhala -/
public def isSinhala (char : Char) : Bool := isScript (Script.ofAbbrev! "Sinh") char
/-- Check if character is Sogdian -/
public def isSogdian (char : Char) : Bool := isScript (Script.ofAbbrev! "Sogd") char
/-- Check if character is Sora_Sompeng -/
public def isSoraSompeng (char : Char) : Bool := isScript (Script.ofAbbrev! "Sora") char
/-- Check if character is Soyombo -/
public def isSoyombo (char : Char) : Bool := isScript (Script.ofAbbrev! "Soyo") char
/-- Check if character is Sundanese -/
public def isSundanese (char : Char) : Bool := isScript (Script.ofAbbrev! "Sund") char
/-- Check if character is Sunuwar -/
public def isSunuwar (char : Char) : Bool := isScript (Script.ofAbbrev! "Sunu") char
/-- Check if character is Syloti_Nagri -/
public def isSylotiNagri (char : Char) : Bool := isScript (Script.ofAbbrev! "Sylo") char
/-- Check if character is Syriac -/
public def isSyriac (char : Char) : Bool := isScript (Script.ofAbbrev! "Syrc") char
/-- Check if character is Tagalog -/
public def isTagalog (char : Char) : Bool := isScript (Script.ofAbbrev! "Tglg") char
/-- Check if character is Tagbanwa -/
public def isTagbanwa (char : Char) : Bool := isScript (Script.ofAbbrev! "Tagb") char
/-- Check if character is Tai_Le -/
public def isTaiLe (char : Char) : Bool := isScript (Script.ofAbbrev! "Tale") char
/-- Check if character is Tai_Tham -/
public def isTaiTham (char : Char) : Bool := isScript (Script.ofAbbrev! "Lana") char
/-- Check if character is Tai_Viet -/
public def isTaiViet (char : Char) : Bool := isScript (Script.ofAbbrev! "Tavt") char
/-- Check if character is Tai_Yo -/
public def isTaiYo (char : Char) : Bool := isScript (Script.ofAbbrev! "Tayo") char
/-- Check if character is Takri -/
public def isTakri (char : Char) : Bool := isScript (Script.ofAbbrev! "Takr") char
/-- Check if character is Tamil -/
public def isTamil (char : Char) : Bool := isScript (Script.ofAbbrev! "Taml") char
/-- Check if character is Tangsa -/
public def isTangsa (char : Char) : Bool := isScript (Script.ofAbbrev! "Tnsa") char
/-- Check if character is Tangut -/
public def isTangut (char : Char) : Bool := isScript (Script.ofAbbrev! "Tang") char
/-- Check if character is Telugu -/
public def isTelugu (char : Char) : Bool := isScript (Script.ofAbbrev! "Telu") char
/-- Check if character is Thaana -/
public def isThaana (char : Char) : Bool := isScript (Script.ofAbbrev! "Thaa") char
/-- Check if character is Thai -/
public def isThai (char : Char) : Bool := isScript (Script.ofAbbrev! "Thai") char
/-- Check if character is Tibetan -/
public def isTibetan (char : Char) : Bool := isScript (Script.ofAbbrev! "Tibt") char
/-- Check if character is Tifinagh -/
public def isTifinagh (char : Char) : Bool := isScript (Script.ofAbbrev! "Tfng") char
/-- Check if character is Tirhuta -/
public def isTirhuta (char : Char) : Bool := isScript (Script.ofAbbrev! "Tirh") char
/-- Check if character is Todhri -/
public def isTodhri (char : Char) : Bool := isScript (Script.ofAbbrev! "Todr") char
/-- Check if character is Tolong_Siki -/
public def isTolongSiki (char : Char) : Bool := isScript (Script.ofAbbrev! "Tols") char
/-- Check if character is Toto -/
public def isToto (char : Char) : Bool := isScript (Script.ofAbbrev! "Toto") char
/-- Check if character is Tulu_Tigalari -/
public def isTuluTigalari (char : Char) : Bool := isScript (Script.ofAbbrev! "Tutg") char
/-- Check if character is Ugaritic -/
public def isUgaritic (char : Char) : Bool := isScript (Script.ofAbbrev! "Ugar") char
/-- Check if character is Unknown -/
public def isUnknown (char : Char) : Bool := isScript (Script.ofAbbrev! "Zzzz") char
/-- Check if character is Vai -/
public def isVai (char : Char) : Bool := isScript (Script.ofAbbrev! "Vaii") char
/-- Check if character is Vithkuqi -/
public def isVithkuqi (char : Char) : Bool := isScript (Script.ofAbbrev! "Vith") char
/-- Check if character is Wancho -/
public def isWancho (char : Char) : Bool := isScript (Script.ofAbbrev! "Wcho") char
/-- Check if character is Warang_Citi -/
public def isWarangCiti (char : Char) : Bool := isScript (Script.ofAbbrev! "Wara") char
/-- Check if character is Yezidi -/
public def isYezidi (char : Char) : Bool := isScript (Script.ofAbbrev! "Yezi") char
/-- Check if character is Yi -/
public def isYi (char : Char) : Bool := isScript (Script.ofAbbrev! "Yiii") char
/-- Check if character is Zanabazar_Square -/
public def isZanabazarSquare (char : Char) : Bool := isScript (Script.ofAbbrev! "Zanb") char

end Unicode
