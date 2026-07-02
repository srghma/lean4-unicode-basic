/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.OtherLowercase

namespace Unicode

/-- Check if code point has Other_Lowercase property using lookup table -/
@[inline]
public def lookupOtherLowercase (c : UInt32) : Bool :=
  lookupPropRange c TableLookupTables.OtherLowercase.table
