/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.TableLookupCommon
public import UnicodeBasic.TableLookupTables.BidiMirroringGlyph

namespace Unicode

/-- Get the bidi mirroring glyph for a code point, if it exists.

  Unicode property: `Bidi_Mirroring_Glyph`
-/
public def lookupBidiMirroringGlyph? (c : UInt32) : Option UInt32 :=
  let table := table
  if table.size == 0 || c < table[0]!.1 then none else
    let d := table[find c (fun i => table[i]!.1) 0 table.usize]!
    if c = d.1 then some d.2 else none
where
  table : Array (UInt32 × UInt32) := TableLookupTables.BidiMirroringGlyph.table
