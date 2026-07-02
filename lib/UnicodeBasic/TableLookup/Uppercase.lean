/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.Uppercase

namespace Unicode

/-- Check if code point is a uppercase letter using lookup table

  Unicode property: `Uppercase` -/
@[inline]
public def lookupUppercase (c : UInt32) : Bool :=
  lookupPropRange c TableLookupTables.Uppercase.table
