/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.OtherAlphabetic

namespace Unicode

/-- Check if code point has Other_Alphabetic property using lookup table -/
@[inline]
public def lookupOtherAlphabetic (c : UInt32) : Bool :=
  lookupPropRange c TableLookupTables.OtherAlphabetic.table
