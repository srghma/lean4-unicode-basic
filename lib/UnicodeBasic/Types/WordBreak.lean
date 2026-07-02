module

namespace Unicode

/-- Word break property

  Unicode property: `Word_Break` -/
public inductive WordBreak
| public other
| public doubleQuote
| public singleQuote
| public hebrewLetter
| public cr
| public lf
| public newline
| public extend
| public regionalIndicator
| public katakana
| public aLetter
| public midLetter
| public midNum
| public midNumLet
| public numeric
| public extendNumLet
| public wSegSpace
| public zwj
| public format
deriving Inhabited, DecidableEq, Repr

public instance : ToString WordBreak where
  toString
  | .other => "Other"
  | .doubleQuote => "Double_Quote"
  | .singleQuote => "Single_Quote"
  | .hebrewLetter => "Hebrew_Letter"
  | .cr => "CR"
  | .lf => "LF"
  | .newline => "Newline"
  | .extend => "Extend"
  | .regionalIndicator => "Regional_Indicator"
  | .katakana => "Katakana"
  | .aLetter => "ALetter"
  | .midLetter => "MidLetter"
  | .midNum => "MidNum"
  | .midNumLet => "MidNumLet"
  | .numeric => "Numeric"
  | .extendNumLet => "ExtendNumLet"
  | .wSegSpace => "WSegSpace"
  | .zwj => "ZWJ"
  | .format => "Format"

public def WordBreak.ofAbbrev? (abbr : String.Slice) : Option WordBreak :=
  if abbr == "XX" || abbr == "Other" then some other
  else if abbr == "DQ" || abbr == "Double_Quote" then some doubleQuote
  else if abbr == "SQ" || abbr == "Single_Quote" then some singleQuote
  else if abbr == "HL" || abbr == "Hebrew_Letter" then some hebrewLetter
  else if abbr == "CR" then some cr
  else if abbr == "LF" then some lf
  else if abbr == "NL" || abbr == "Newline" then some newline
  else if abbr == "EX" || abbr == "Extend" then some extend
  else if abbr == "RI" || abbr == "Regional_Indicator" then some regionalIndicator
  else if abbr == "KA" || abbr == "Katakana" then some katakana
  else if abbr == "LE" || abbr == "ALetter" then some aLetter
  else if abbr == "ML" || abbr == "MidLetter" then some midLetter
  else if abbr == "MN" || abbr == "MidNum" then some midNum
  else if abbr == "MB" || abbr == "MidNumLet" then some midNumLet
  else if abbr == "NU" || abbr == "Numeric" then some numeric
  else if abbr == "EX" || abbr == "ExtendNumLet" then some extendNumLet
  else if abbr == "WS" || abbr == "WSegSpace" then some wSegSpace
  else if abbr == "ZWJ" then some zwj
  else if abbr == "FO" || abbr == "Format" then some format
  else none

@[inherit_doc WordBreak.ofAbbrev?]
public def WordBreak.ofAbbrev! (abbr : String.Slice) : WordBreak :=
  match ofAbbrev? abbr with
  | some b => b
  | none => panic! s!"invalid word break abbreviation {abbr.copy}"

end Unicode
