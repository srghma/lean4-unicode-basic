/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.CanonicalCombiningClass

namespace Unicode

/-- Get canonical combining class using lookup table

  Unicode property: `Canonical_Combining_Class` -/
public def lookupCanonicalCombiningClass (c : UInt32) : Nat :=
  let t := table
  if c < t[0]!.1 then 0 else
    match t[find c (fun i => t[i]!.1) 0 t.size.toUSize]! with
    | (_, v, n) => if c ≤ v then n else 0
where
  table : Array (UInt32 × UInt32 × Nat) := TableLookupTables.CanonicalCombiningClass.table
