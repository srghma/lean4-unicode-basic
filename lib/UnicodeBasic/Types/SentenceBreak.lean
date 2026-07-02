module

namespace Unicode

/-- Sentence break property

  Unicode property: `Sentence_Break` -/
public inductive SentenceBreak
| public other
| public aTerm
| public cr
| public close
| public extend
| public format
| public lf
| public lower
| public numeric
| public oLetter
| public sContinue
| public sTerm
| public sep
| public sp
| public upper
deriving Inhabited, DecidableEq, Repr

public def SentenceBreak.ofAbbrev? (abbr : String.Slice) : Option SentenceBreak :=
  if abbr == "XX" || abbr == "Other" then some other
  else if abbr == "AT" || abbr == "ATerm" then some aTerm
  else if abbr == "CR" then some cr
  else if abbr == "CL" || abbr == "Close" then some close
  else if abbr == "EX" || abbr == "Extend" then some extend
  else if abbr == "FO" || abbr == "Format" then some format
  else if abbr == "LF" then some lf
  else if abbr == "LO" || abbr == "Lower" then some lower
  else if abbr == "NU" || abbr == "Numeric" then some numeric
  else if abbr == "LE" || abbr == "OLetter" then some oLetter
  else if abbr == "SC" || abbr == "SContinue" then some sContinue
  else if abbr == "ST" || abbr == "STerm" then some sTerm
  else if abbr == "SE" || abbr == "Sep" then some sep
  else if abbr == "SP" || abbr == "Sp" then some sp
  else if abbr == "UP" || abbr == "Upper" then some upper
  else none

@[inherit_doc SentenceBreak.ofAbbrev?]
public def SentenceBreak.ofAbbrev! (abbr : String.Slice) : SentenceBreak :=
  match ofAbbrev? abbr with
  | some b => b
  | none => panic! s!"invalid sentence break abbreviation {abbr.copy}"

end Unicode
