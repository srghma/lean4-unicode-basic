/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasicCommon.Types.BidiBracketType
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.BidiBrackets

namespace Unicode

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
