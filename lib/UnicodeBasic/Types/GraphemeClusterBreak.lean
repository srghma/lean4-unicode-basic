module

namespace Unicode

/-- Grapheme cluster break property

  Unicode property: `Grapheme_Cluster_Break` -/
public inductive GraphemeClusterBreak
| public other
| public control
| public cr
| public extend
| public lf
| public spacingMark
| public prepend
| public regionalIndicator
| public l
| public v
| public t
| public lv
| public lvt
| public zwj
deriving Inhabited, DecidableEq, Repr

public instance : ToString GraphemeClusterBreak where
  toString
  | .other => "Other"
  | .control => "Control"
  | .cr => "CR"
  | .extend => "Extend"
  | .lf => "LF"
  | .spacingMark => "SpacingMark"
  | .prepend => "Prepend"
  | .regionalIndicator => "Regional_Indicator"
  | .l => "L"
  | .v => "V"
  | .t => "T"
  | .lv => "LV"
  | .lvt => "LVT"
  | .zwj => "ZWJ"

public def GraphemeClusterBreak.ofAbbrev? (abbr : String.Slice) : Option GraphemeClusterBreak :=
  if abbr == "XX" || abbr == "Other" then some other
  else if abbr == "CN" || abbr == "Control" then some control
  else if abbr == "CR" then some cr
  else if abbr == "EX" || abbr == "Extend" then some extend
  else if abbr == "LF" then some lf
  else if abbr == "SM" || abbr == "SpacingMark" then some spacingMark
  else if abbr == "PP" || abbr == "Prepend" then some prepend
  else if abbr == "RI" || abbr == "Regional_Indicator" then some regionalIndicator
  else if abbr == "L" then some l
  else if abbr == "V" then some v
  else if abbr == "T" then some t
  else if abbr == "LV" then some lv
  else if abbr == "LVT" then some lvt
  else if abbr == "ZWJ" then some zwj
  else none

@[inherit_doc GraphemeClusterBreak.ofAbbrev?]
public def GraphemeClusterBreak.ofAbbrev! (abbr : String.Slice) : GraphemeClusterBreak :=
  match ofAbbrev? abbr with
  | some b => b
  | none => panic! s!"invalid grapheme cluster break abbreviation {abbr.copy}"

end Unicode
