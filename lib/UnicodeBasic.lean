/-
Copyright © 2023-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
prelude
public import UnicodeBasicCommon.Types.BidiBracketType
public import UnicodeBasicCommon.Types.Bounds
public import UnicodeBasicCommon.Types.DecompositionMapping
public import UnicodeBasicCommon.Types.EastAsianWidth
public import UnicodeBasicCommon.Types.GeneralCategory
public import UnicodeBasicCommon.Types.NumericType
public import UnicodeBasicCommon.Types.Script
public import UnicodeBasicCommon.Types.VerticalOrientation
public import UnicodeBasic.Hangul
public import UnicodeBasic.Bidi
public import UnicodeBasic.Segmentation

public import UnicodeBasic.GeneralApi.Core
public import UnicodeBasic.GeneralApi.Script
public import UnicodeBasic.GeneralApi.ScriptPredicates
public import UnicodeBasic.GeneralApi.Bidi
public import UnicodeBasic.GeneralApi.GeneralCategory
public import UnicodeBasic.GeneralApi.Case
public import UnicodeBasic.GeneralApi.Decomposition
public import UnicodeBasic.GeneralApi.Numeric
public import UnicodeBasic.GeneralApi.BinaryProperties
public import UnicodeBasic.GeneralApi.Emoji
public import UnicodeBasic.GeneralApi.DerivedProperties


/-!
  # General API #

  As a general rule, for a given a Unicode property called `Unicode_Property`,
  for example:

  - If the property is boolean valued then the implementation is called
    `Unicode.isUnicodeProperty`.

  - Otherwise, the implementation is called `Unicode.getUnicodeProperty`.

  - If the value is not of standard type (`Bool`, `Char`, `Nat`, `Int`, etc.)
    or a combination thereof (e.g. `Bool × Option Nat`) then the value type is
    defined in `UnicodeBasic.Types`.

  - If the input type needs disambiguation (e.g. `Char` vs `String`) the type
    name may be appended to the name as in `Unicode.isUnicodePropertyChar` or
    in `Unicode.getUnicodePropertyString`.

  - If the output type is `Option _` then the suffix `?` may be appended to
    indicate that this is a partial function. In this case, a companion
    function with the suffix `!` may be implemented. This function will
    perform the same calculation as the original but it assumes the user has
    checked that the input is in the domain, the function may panic if this
    is not the case.

  Unicode general categories are encoded using the type `GC`. This type has
  a boolean algebra structure with inclusion `⊆`, meet/intersection `&&&`,
  join/union `|||` and complement `~~~`. The relation `∈` is provided to
  check whether a character belongs to a given category. For example,
  `c ∈ (GC.L &&& ~~~GC.Lt) ||| GC.Z` checks whether `c` is a either a
  non-titlecase letter or a separator.
-/
