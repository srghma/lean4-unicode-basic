module

namespace Unicode

/-!
  ## Decomposition Mapping ##
-/

/-- Compatibility format tag

  Unicode properties: `Decomposition_Type`, `Decomposition_Mapping` -/
public inductive CompatibilityTag
/-- Font variant -/
| public font
/-- No-break version of a space or hyphen -/
| public noBreak
/-- Initial presentation form (Arabic) -/
| public initial
/-- Medial presentation form (Arabic) -/
| public medial
/-- Final presentation form (Arabic) -/
| public final
/-- Isolated presentation form (Arabic) -/
| public isolated
/-- Encircled form -/
| public circle
/-- Superscript form -/
| public super
/-- Subscript form -/
| public sub
/-- Vertical layout presentation form -/
| public vertical
/-- Wide (or zenkaku) compatibility character -/
| public wide
/-- Narrow (or hankaku) compatibility character -/
| public narrow
/-- Small variant form (CNS compatibility) -/
| public small
/-- CJK squared font variant -/
| public square
/-- Vulgar fraction form -/
| public fraction
/-- Otherwise unspecified compatibility character -/
| public compat
deriving Inhabited, DecidableEq, Repr

public instance : ToString CompatibilityTag where
  toString
  | .font => "<font>"
  | .noBreak => "<noBreak>"
  | .initial => "<initial>"
  | .medial => "<medial>"
  | .final => "<final>"
  | .isolated => "<isolated>"
  | .circle => "<circle>"
  | .super => "<super>"
  | .sub => "<sub>"
  | .vertical => "<vertical>"
  | .wide => "<wide>"
  | .narrow => "<narrow>"
  | .small => "<small>"
  | .square => "<square>"
  | .fraction => "<fraction>"
  | .compat => "<compat>"

/-- Decomposition maping

  Unicode properties: `Decomposition_Type`, `Decomposition_Mapping` -/
public structure DecompositionMapping where
  /-- Compatibility format tag -/
  public tag : Option CompatibilityTag
  /-- Decomposition mapping -/
  public mapping : List Char
deriving Inhabited, DecidableEq, Repr

end Unicode
