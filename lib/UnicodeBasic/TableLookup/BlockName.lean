/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.BlockName

namespace Unicode

/-- Get block name for a code point.

  Unicode property: `Block`
-/
public def lookupBlockName (c : UInt32) : String :=
  if h : TableLookupTables.BlockName.BetweenOrEqStartEnd c then
    (TableLookupTables.BlockName.getInsideSparseRangeValueTable c h).getD "No_Block"
  else
    "No_Block"
