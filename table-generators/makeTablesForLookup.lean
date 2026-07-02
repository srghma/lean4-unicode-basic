/-
Copyright © 2024-2025 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
import MakeTablesForLookup.Common
import UnicodeData
import UnicodeBasicCommon.CharacterDatabase
import UnicodeBasicCommon.Types.Script

open Unicode

def compressProp (arr : Array (UInt32 × UInt32)) (noOverlap : Bool := true) : Id <| Array (UInt32 × UInt32) :=
  if h : arr.size > 0 then do
    let mut res := #[]
    let mut top := arr[0]
    for a in arr[1:] do
      if noOverlap && a.1 ≤ top.2 then
        panic! "overlap!"
      else if a.1 ≤ top.2 + 1 then
        top := (top.1, max a.2 top.2)
      else
        res := res.push top
        top := a
    return res.push top
  else #[]

def compressData [BEq α] (arr : Array (UInt32 × UInt32 × α)) (noOverlap : Bool := true) : Id <| Array (UInt32 × UInt32 × α) :=
  if h : arr.size > 0 then do
    let mut res := #[]
    let mut top := arr[0]
    for a in arr[1:] do
      if noOverlap && a.1 ≤ top.2.1 then
        panic! "overlap!"
      else if a.2.2 == top.2.2 && a.1 ≤ top.2.1 + 1 then
        top := (top.1, max a.2.1 top.2.1, top.2.2)
      else
        res := res.push top
        top := a
    return res.push top
  else #[]

def mergeProp (d : Array (Array (UInt32 × UInt32))) (noOverlap : Bool := true) : Array (UInt32 × UInt32) :=
  let data := d.flatten.qsort fun a b => a.1 < b.1
  compressProp data noOverlap

def mergeData [BEq α] (d : Array (Array (UInt32 × UInt32 × α))) (noOverlap : Bool := true) : Array (UInt32 × UInt32 × α) :=
  let data := d.flatten.qsort fun a b => a.1 < b.1
  compressData data noOverlap

def statsData (array : Array (UInt32 × UInt32 × α)) : Id <| Nat × Nat := do
  let mut ct := 0
  for (c₀, c₁, _) in array do
    ct := ct + (c₁.toNat - c₀.toNat)
  return (array.size, ct)

def statsProp (array : Array (UInt32 × UInt32)) : Id <| Nat × Nat := do
  let mut ct := 0
  for (c₀, c₁) in array do
    ct := ct + (c₁.toNat - c₀.toNat)
  return (array.size, ct)

inductive TableDensity where
  | dense
  | sparse
deriving Repr, BEq, Inhabited

inductive TableLayoutShape where
  | pair
  | key
  | range
deriving Repr, BEq, Inhabited

inductive WhyInvalid where
  | unsorted
  | overlap
  | duplicateKey
  | reversedRange
  | tooManyValueFields
deriving Repr, BEq, Inhabited

structure TableLayoutKind where
  density : TableDensity
  kind : TableLayoutShape
  isInvalid : Option WhyInvalid := none
deriving Repr, BEq, Inhabited

structure TableLayoutReport where
  kind : TableLayoutKind
  start : UInt32
  stop : UInt32
  rowCount : Nat := 0
  codePointCount : Nat := 0
  valueFieldCount : Nat := 0
deriving Repr, Inhabited

inductive TableSize where
  | rows (rowCount : Nat)
  | ranges (rowCount : Nat) (spanCount : Nat)
deriving Repr

inductive TableLayoutSource where
  | none
  | pair (pairs : Array (UInt32 × UInt32))
  | range (ranges : Array (UInt32 × UInt32)) (valueFieldCount : Nat)
  | key (keys : Array UInt32)

structure GeneratedTable where
  name : String
  size : TableSize
  rawLayout : TableLayoutSource
  layout : TableLayoutSource
  sorted : Bool := false
  render : String

private def TableDensity.label : TableDensity → String
  | .dense => "dense"
  | .sparse => "sparse"

private def TableLayoutShape.label : TableLayoutShape → String
  | .pair => "pair"
  | .key => "key"
  | .range => "range"

private def WhyInvalid.label : WhyInvalid → String
  | .unsorted => "unsorted"
  | .overlap => "overlap"
  | .duplicateKey => "duplicate key"
  | .reversedRange => "reversed range"
  | .tooManyValueFields => "too many value fields"

private def TableLayoutKind.label : TableLayoutKind → String
  | k =>
    let base :=
      match k.density, k.kind with
      | .dense, .pair => "dense pair"
      | .sparse, .pair => "sparse pair"
      | .dense, .key => "dense key"
      | .sparse, .key => "sparse key"
      | .dense, .range => "dense range"
      | .sparse, .range => "sparse range"
    match k.isInvalid with
    | none => base
    | some why => s!"invalid {base} ({why.label})"

private def ansi (code : String) (s : String) : String :=
  let esc := String.singleton (Char.ofNat 27)
  s!"{esc}[{code}m{s}{esc}[0m"

private def bold (s : String) : String := ansi "1" s
private def dim (s : String) : String := ansi "2" s
private def blue (s : String) : String := ansi "34" s
private def cyan (s : String) : String := ansi "36" s
private def green (s : String) : String := ansi "32" s
private def yellow (s : String) : String := ansi "33" s
private def magenta (s : String) : String := ansi "35" s
private def red (s : String) : String := ansi "31" s

private def TableLayoutKind.coloredLabel : TableLayoutKind → String
  | k =>
    match k.isInvalid with
    | some _ => red k.label
    | none =>
      match k.density with
      | .dense =>
        match k.kind with
        | .pair => green "dense pair"
        | .key => cyan "dense key"
        | .range => green "dense range"
      | .sparse =>
        match k.kind with
        | .pair => yellow "sparse pair"
        | .key => magenta "sparse key"
        | .range => yellow "sparse range"

private def TableLayoutKind.emoji : TableLayoutKind → String
  | k =>
    match k.isInvalid with
    | some _ => "🔴"
    | none =>
      match k.density with
      | .dense => "🟢"
      | .sparse => "🟡"

private def TableLayoutReport.describe (r : TableLayoutReport) : String :=
  s!"Layout: {r.kind.label} [{toHexStringRaw r.start}..{toHexStringRaw r.stop}]; rows={r.rowCount}; codePoints={r.codePointCount}; valueFields={r.valueFieldCount}"

private def TableSize.describe : TableSize → String
  | .rows rowCount => s!"Size: {rowCount}"
  | .ranges rowCount spanCount => s!"Size: {rowCount} + {spanCount}"

private def analyzeRangeTable (ranges : Array (UInt32 × UInt32)) (valueFieldCount : Nat) : TableLayoutReport :=
  match ranges[0]? with
  | none => panic! "range table cannot be empty"
  | some (start, firstStop) =>
    let codePointCount := Id.run do
      let mut total := 0
      for (c₀, c₁) in ranges do
        total := total + (c₁.toNat + 1 - c₀.toNat)
      return total
    let (density, invalid?) := Id.run do
      if valueFieldCount > 1 then
        return (.sparse, some WhyInvalid.tooManyValueFields)
      let mut prevEnd := firstStop
      if start > firstStop then
        return (.sparse, some WhyInvalid.reversedRange)
      for (c₀, c₁) in ranges[1:] do
        if c₀ > c₁ then
          return (.sparse, some WhyInvalid.reversedRange)
        if c₀ < prevEnd + 1 then
          if c₀ ≤ prevEnd then
            return (.sparse, some WhyInvalid.overlap)
          return (.sparse, some WhyInvalid.unsorted)
        if c₀ != prevEnd + 1 then
          return (.sparse, none)
        prevEnd := c₁
      return (.dense, none)
    {
      kind := { density, kind := .range, isInvalid := invalid? },
      start,
      stop := ranges[ranges.size - 1]!.2,
      rowCount := ranges.size,
      codePointCount,
      valueFieldCount
    }

private def analyzeKeyTable (keys : Array UInt32) : TableLayoutReport :=
  match keys[0]? with
  | none => panic! "key table cannot be empty"
  | some start =>
    let stop := keys[keys.size - 1]!
    let (density, invalid?) := Id.run do
      let mut prev := start
      for k in keys[1:] do
        if k < prev then
          return (.sparse, some WhyInvalid.unsorted)
        if k == prev then
          return (.sparse, some WhyInvalid.duplicateKey)
        if k != prev + 1 then
          return (.sparse, none)
        prev := k
      return (.dense, none)
    {
      kind := { density, kind := .key, isInvalid := invalid? },
      start,
      stop,
      rowCount := keys.size,
      codePointCount := keys.size,
      valueFieldCount := 0
    }

private def analyzePairTable (pairs : Array (UInt32 × UInt32)) : TableLayoutReport :=
  match pairs[0]? with
  | none => panic! "pair table cannot be empty"
  | some (start, _) =>
    let stop := pairs[pairs.size - 1]!.1
    let (density, invalid?) := Id.run do
      let mut prev := start
      for (k, _) in pairs[1:] do
        if k < prev then
          return (.sparse, some WhyInvalid.unsorted)
        if k == prev then
          return (.sparse, some WhyInvalid.duplicateKey)
        if k != prev + 1 then
          return (.sparse, none)
        prev := k
      return (.dense, none)
    {
      kind := { density, kind := .pair, isInvalid := invalid? },
      start,
      stop,
      rowCount := pairs.size,
      codePointCount := pairs.size,
      valueFieldCount := 0
    }

private def analyzeLayoutSource : TableLayoutSource → TableLayoutReport
  | .none => panic! "layout source cannot be empty"
  | .pair pairs => analyzePairTable pairs
  | .range ranges valueFieldCount => analyzeRangeTable ranges valueFieldCount
  | .key keys => analyzeKeyTable keys

private def isSortedPairTable (table : Array (UInt32 × UInt32)) : Bool :=
  Id.run do
    match table[0]? with
    | none => true
    | some (_, first) =>
      let mut prev := first
      for (c₀, _) in table[1:] do
        if c₀ < prev then
          return false
        prev := c₀
      return true

private def isSortedKeyTable (table : Array (UInt32 × α)) : Bool :=
  Id.run do
    match table[0]? with
    | none => true
    | some (first, _) =>
      let mut prev := first
      for (c, _) in table[1:] do
        if c ≤ prev then
          return false
        prev := c
      return true

private def isSortedRangeTable (table : Array (UInt32 × UInt32 × α)) : Bool :=
  Id.run do
    match table[0]? with
    | none => true
    | some (firstStart, firstEnd, _) =>
      let mut prevStart := firstStart
      let mut prevEnd := firstEnd
      for (c₀, c₁, _) in table[1:] do
        if c₀ < prevStart || (c₀ == prevStart && c₁ < prevEnd) then
          return false
        prevStart := c₀
        prevEnd := c₁
      return true

private def sortPairTable (table : Array (UInt32 × UInt32)) : Array (UInt32 × UInt32) :=
  table.qsort fun (a0, _) (b0, _) => a0 < b0

private def sortKeyTable (table : Array (UInt32 × α)) : Array (UInt32 × α) :=
  table.qsort fun (a0, _) (b0, _) => a0 < b0

private def sortRangeTable (table : Array (UInt32 × UInt32 × α)) : Array (UInt32 × UInt32 × α) :=
  table.qsort fun (a0, a1, _) (b0, b1, _) =>
    a0 < b0 || (a0 == b0 && a1 < b1)

def mkBidiClass : IO <| Array (UInt32 × UInt32 × BidiClass) := do
  let mut t := #[]
  let mut start : UInt32 := 0
  let mut last := lookupDerivedBidiClass 0
  for i in [1:Unicode.max.toNat + 1] do
    let c : UInt32 := UInt32.ofNat i
    let bc := lookupDerivedBidiClass c
    if bc == last then
      continue
    else
      t := t.push (start, c - 1, last)
      start := c
      last := bc
  return t.push (start, Unicode.max, last)

def mkBidiMirrored : IO <| Array (UInt32 × UInt32) := do
  let mut t := #[]
  for d in UnicodeData.data do
    if d.bidiMirrored then
      match t.back? with
      | some (c₀, c₁) =>
        if d.code == c₁ + 1 then
          t := t.pop.push (c₀, d.code)
        else
          t := t.push (d.code, d.code)
      | none =>
        t := t.push (d.code, d.code)
  return t

def mkBidiMirroringGlyph : Array (UInt32 × UInt32) := Id.run do
  let mut t := #[]
  let txt : String := BidiMirroring.txt
  let stream := UCDStream.ofString txt
  for record in stream do
    t := t.push (ofHexString! record[0]!, ofHexString! record[1]!)
  return t

def mkBidiBrackets : Array (UInt32 × UInt32 × BidiBracketType) := Id.run do
  let mut t := #[]
  let txt : String := BidiBrackets.txt
  let stream := UCDStream.ofString txt
  for record in stream do
    t := t.push (
      ofHexString! record[0]!,
      ofHexString! record[1]!,
      BidiBracketType.ofAbbrev! record[2]!
    )
  return t

def mkBlockName : Array (UInt32 × UInt32 × String) := Id.run do
  let mut t := #[]
  let txt : String := Blocks.txt
  let stream := UCDStream.ofString txt
  for record in stream do
    let (c₀, c₁) : UInt32 × UInt32 :=
      match record[0]!.split ".." |>.toList with
      | [c] => (ofHexString! c, ofHexString! c)
      | [c₀, c₁] => (ofHexString! c₀, ofHexString! c₁)
      | _ => panic! "invalid record in Blocks.txt"
    t := t.push (c₀, c₁, record[1]!.toString)
  return t

def mkEastAsianWidth : Array (UInt32 × UInt32 × EastAsianWidth) := Id.run do
  let mut t := #[]
  let txt : String := EastAsianWidth.txt -- from DerivedEastAsianWidth.lean
  let stream := UCDStream.ofString txt
  for record in stream do
    let (c₀, c₁) : UInt32 × UInt32 :=
      match record[0]!.split ".." |>.toList with
      | [c] => (ofHexString! c, ofHexString! c)
      | [c₀, c₁] => (ofHexString! c₀, ofHexString! c₁)
      | _ => panic! "invalid record in DerivedEastAsianWidth.txt"
    t := t.push (c₀, c₁, EastAsianWidth.ofAbbrev! record[1]!)
  return t

def mkVerticalOrientation : Array (UInt32 × UInt32 × VerticalOrientation) := Id.run do
  let mut t := #[]
  let txt : String := VerticalOrientation.txt
  let stream := UCDStream.ofString txt
  for record in stream do
    let (c₀, c₁) : UInt32 × UInt32 :=
      match record[0]!.split ".." |>.toList with
      | [c] => (ofHexString! c, ofHexString! c)
      | [c₀, c₁] => (ofHexString! c₀, ofHexString! c₁)
      | _ => panic! "invalid record in VerticalOrientation.txt"
    t := t.push (c₀, c₁, VerticalOrientation.ofAbbrev! record[1]!)
  return t

def mkCanonicalCombiningClass : IO <| Array (UInt32 × UInt32 × Nat) := do
  let mut t := #[]
  for d in UnicodeData.data do
    if d.cc > 0 then
      match t.back? with
      | some (c₀, c₁, cc) =>
        if t.size != 0 && d.code == c₁ + 1 && d.cc == cc then
          t := t.pop.push (c₀, c₁+1, cc)
        else
          t := t.push (d.code, d.code, d.cc)
      | none =>
        t := t.push (d.code, d.code, d.cc)
  return t

partial def mkCanonicalDecompositionMapping : IO <| Array (UInt32 × List Char) := do
  let mut t := #[]
  for data in UnicodeData.data do
    match data.decomp with
    | some ⟨none, l⟩ =>
      t := t.push (data.code, fullDecomposition l)
    | _ => continue
  return t
where
  fullDecomposition : List Char → List Char
  | [] => unreachable!
  | h :: t =>
    match (getUnicodeData h).decomp with
    | some ⟨none, l⟩ => fullDecomposition (l ++ t)
    | _ => h :: t

def mkCaseMapping : IO <| Array (UInt32 × UInt32 × UInt32 × UInt32 × UInt32) := do
  let mut t := #[]
  for data in UnicodeData.data do
    match data with
    | ⟨_, _, _, _, _, _, _, _, none, none, none⟩ => continue
    | ⟨c, _, _, _, _, _, _, _, um, lm, tm⟩ =>
      let uc := match um with | some uc => uc.val | _ => c
      let lc := match lm with | some lc => lc.val | _ => c
      let tc := match tm with | some tc => tc.val | _ => uc
      match t.back? with
      | some (c₀,c₁,m) =>
        if (c == c₁ + 1) && (m == (uc, lc, tc)) then
          t := t.pop.push (c₀, c, m)
        else
          t := t.push (c, c, uc, lc, tc)
      | _ =>
          t := t.push (c, c, uc, lc, tc)
  return t

def mkDecompositionMapping : IO <| Array (UInt32 × String) := do
  let mut t := #[]
  for data in UnicodeData.data do
    match data.decomp with
    | some ⟨none, l⟩ =>
      t := t.push (data.code, ";" ++ ";".intercalate (l.map (toHexStringRaw <| Char.val .)))
    | some ⟨some k, l⟩ =>
      t := t.push (data.code, s!"{k};" ++ ";".intercalate (l.map (toHexStringRaw <| Char.val ·)))
    | _ => continue
  return t

def Unicode.GC.PB : GC := (0x80000000 : UInt32)
def Unicode.GC.LC0 : GC := .LC
def Unicode.GC.LC1 : GC := .LC ||| .PB
def Unicode.GC.PG0 : GC := .PG
def Unicode.GC.PG1 : GC := .PG ||| .PB
def Unicode.GC.PQ0 : GC := .PQ
def Unicode.GC.PQ1 : GC := .PQ ||| .PB

def mkGC : IO <| Array (UInt32 × UInt32 × UInt32) := do
  let mut t := #[(0,0,GC.Cc)]
  for i in [1:UnicodeData.data.size] do
    let data := UnicodeData.data[i]!
    let c := data.code
    let k := data.gc
    if data.name.takeEnd 8 == ", First>" then
      t := t.push (c, c, k)
    else if data.name.takeEnd 7 == ", Last>" then
      let (c₀, _, k₀) := t.back!
      t := t.pop.push (c₀, c, k₀)
    else
      let (c₀, c₁, k₀) := t.back!
      if c == c₁ + 1 then
        if k == k₀ then
          t := t.pop.push (c₀, c, k)
        else if k == .Lu then
          if c &&& 1 == 0 then
            if k₀ == .LC0 || (c₀ == c₁ && k₀ == .Ll) then
              t := t.pop.push (c₀, c, .LC0)
            else
              t := t.push (c, c, k)
          else
            if k₀ == .LC1 || (c₀ == c₁ && k₀ == .Ll) then
              t := t.pop.push (c₀, c, .LC1)
            else
              t := t.push (c, c, k)
        else if k == .Ll then
          if c &&& 1 == 0 then
            if k₀ == .LC1 || (c₀ == c₁ && k₀ == .Lu) then
              t := t.pop.push (c₀, c, .LC1)
            else
              t := t.push (c, c, k)
          else
            if k₀ == .LC0 || (c₀ == c₁ && k₀ == .Lu) then
              t := t.pop.push (c₀, c, .LC0)
            else
              t := t.push (c, c, k)
        else if k == .Ps then
          if c &&& 1 == 0 then
            if k₀ == .PG0 || (c₀ == c₁ && k₀ == .Pe) then
              t := t.pop.push (c₀, c, .PG0)
            else
              t := t.push (c, c, k)
          else
            if k₀ == .PG1 || (c₀ == c₁ && k₀ == .Pe) then
              t := t.pop.push (c₀, c, .PG1)
            else
              t := t.push (c, c, k)
        else if k == .Pe then
          if c &&& 1 == 0 then
            if k₀ == .PG1 || (c₀ == c₁ && k₀ == .Ps) then
              t := t.pop.push (c₀, c, .PG1)
            else
              t := t.push (c, c, k)
          else
            if k₀ == .PG0 || (c₀ == c₁ && k₀ == .Ps) then
              t := t.pop.push (c₀, c, .PG0)
            else
              t := t.push (c, c, k)
        else if k == .Pi then
          if c &&& 1 == 0 then
            if k₀ == .PQ0 || (c₀ == c₁ && k₀ == .Pf) then
              t := t.pop.push (c₀, c, .PQ0)
            else
              t := t.push (c, c, k)
          else
            if k₀ == .PQ1 || (c₀ == c₁ && k₀ == .Pf) then
              t := t.pop.push (c₀, c, .PQ1)
            else
              t := t.push (c, c, k)
        else if k == .Pf then
          if c &&& 1 == 0 then
            if k₀ == .PQ1 || (c₀ == c₁ && k₀ == .Pi) then
              t := t.pop.push (c₀, c, .PQ1)
            else
              t := t.push (c, c, k)
          else
            if k₀ == .PQ0 || (c₀ == c₁ && k₀ == .Pi) then
              t := t.pop.push (c₀, c, .PQ0)
            else
              t := t.push (c, c, k)
        else
          t := t.push (c, c, k)
      else
        t := t.push (c, c, k)
  return t

def mkGeneralCategory : IO <| Array (UInt32 × UInt32 × GC) := do
  let mut t := #[(0,0,.Cc)]
  for i in [1:UnicodeData.data.size] do
    let data := UnicodeData.data[i]!
    let c := data.code
    let k := data.gc
    if data.name.takeEnd 8 == ", First>" then
      t := t.push (c, c, k)
    else if data.name.takeEnd 7 == ", Last>" then
      match t.back! with
      | (c₀, _, k) =>
        t := t.pop.push (c₀, c, k)
    else
      let k :=
        if k == .Lu && (c &&& 1) == 0 && UnicodeData.data[i+1]!.code == c+1 then
          if UnicodeData.data[i+1]!.gc == .Ll
          then .LC
          else k
        else if k == .Ll && (c &&& 1) != 0 && UnicodeData.data[i-1]!.code == c-1 then
          if UnicodeData.data[i-1]!.gc == .Lu
          then .LC
          else k
        else if k == .Ps && (c &&& 1) == 0 && UnicodeData.data[i+1]!.code == c+1 then
          if UnicodeData.data[i+1]!.gc == .Pe
          then .PG
          else k
        else if k == .Pe && (c &&& 1) != 0 && UnicodeData.data[i-1]!.code == c-1 then
          if UnicodeData.data[i-1]!.gc == .Ps
          then .PG
          else k
        else if k == .Pi && (c &&& 1) == 0 && UnicodeData.data[i+1]!.code == c+1 then
          if UnicodeData.data[i+1]!.gc == .Pf
          then .PQ
          else k
        else if k == .Pf && (c &&& 1) != 0 && UnicodeData.data[i-1]!.code == c-1 then
          if UnicodeData.data[i-1]!.gc == .Pi
          then .PQ
          else k
        else k
      match t.back! with
      | (c₀, c₁, k₁) =>
        if c == c₁ + 1 && k == k₁ then
          t := t.pop.push (c₀, c, k)
        else
          t := t.push (c, c, k)
  return t

def mkNoncharacterCodePoint : Array (UInt32 × UInt32) :=
  PropList.data.noncharacterCodePoint.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkName : IO <| Array (UInt32 × UInt32 × String) := do
  let mut t := #[(0,0,"<Control>")]
  for i in [1:UnicodeData.data.size] do
    let data := UnicodeData.data[i]!
    let c := data.code
    let n := data.name.copy
    if n.takeEnd 8 == ", First>" then
      if "<CJK Ideograph".isPrefixOf n then
        t := t.push (c, c, "<CJK Unified Ideograph>")
      else if "<Tangut Ideograph".isPrefixOf n then
        t := t.push (c, c, "<Tangut Ideograph>")
      else if n.takeEnd 17 == "Surrogate, First>" then
        match t.back! with
        | (c₀, c₁, n₀) =>
          if c == c₁ + 1 && n₀ == "<Surrogate>" then
            t := t.pop.push (c₀, c, "<Surrogate>")
          else
            t := t.push (c, c, "<Surrogate>")
      else if n.takeEnd 19 == "Private Use, First>" then
        t := t.push (c, c, "<Private Use>")
      else
        t := t.push (c, c, (n.dropEnd 8).copy ++ ">")
    else if n.takeEnd 7 == ", Last>" then
      match t.back! with
      | (c₀, _, n₀) =>
        t := t.pop.push (c₀, c, n₀)
    else if n == "<control>" then
      match t.back! with
      | (c₀, _, n₀) =>
        if n₀ == "<Control>" then
          t := t.pop.push (c₀, c, n₀)
        else
          t := t.push (c, c, "<Control>")
    else if "CJK COMPATIBILITY IDEOGRAPH-".isPrefixOf n then
      match t.back! with
      | (c₀, c₁, n) =>
        if c == c₁ + 1 && n == "<CJK Compatibility Ideograph>" then
          t := t.pop.push (c₀, c, n)
        else
          t := t.push (c, c, "<CJK Compatibility Ideograph>")
    else if "KHITAN SMALL SCRIPT CHARACTER-".isPrefixOf n then
      match t.back! with
      | (c₀, c₁, n) =>
        if c == c₁ + 1 && n == "<Khitan Small Script Character>" then
          t := t.pop.push (c₀, c, n)
        else
          t := t.push (c, c, "<Khitan Small Script Character>")
    else if "NUSHU CHARACTER-".isPrefixOf n then
      match t.back! with
      | (c₀, c₁, n) =>
        if c == c₁ + 1 && n == "<Nushu Character>" then
          t := t.pop.push (c₀, c, n)
        else
          t := t.push (c, c, "<Nushu Character>")
    else if "TANGUT COMPONENT-".isPrefixOf n then
      match t.back! with
      | (c₀, c₁, n) =>
        if c == c₁ + 1 && n == "<Tangut Component>" then
          t := t.pop.push (c₀, c, n)
        else
          t := t.push (c, c, "<Tangut Component>")
    else
      match t.back! with
      | (c₀, c₁, n₀) =>
        if c == c₁ + 1 && n == n₀ then
          t := t.pop.push (c₀, c, n)
        else
          t := t.push (c, c, n)
  return mergeData #[t, mkNoncharacterCodePoint.map fun (c₀, c₁) => (c₀, c₁, "<Reserved>")]

def mkNumericValue : IO <| Array (UInt32 × UInt32 × NumericType) := do
  let mut t := #[]
  for c in [0:Unicode.max.toNat + 1] do
    let c := UInt32.ofNat c
    match getUnicodeData? c with
    | none => continue
    | some d =>
      match d.numeric with
      | some (.decimal 0) =>
        t := t.push (d.code, d.code + 9, NumericType.decimal 0)
      | some (.digit v) =>
        match t.back! with
        | (c₀, c₁, n@(NumericType.digit x)) =>
          let last := x.val + c₁.toNat - c₀.toNat
          if d.code == c₁ + 1 && v.val == last + 1 then
            t := t.pop.push (c₀, d.code, n)
          else
            t := t.push (d.code, d.code, .digit v)
        | _ =>
          t := t.push (d.code, d.code, .digit v)
      | some n@(.numeric _ _) =>
        t := t.push (d.code, d.code, n)
      | _ => continue
  return t

def mkOtherAlphabetic : Array (UInt32 × UInt32) :=
  PropList.data.otherAlphabetic.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkOtherLowercase : Array (UInt32 × UInt32) :=
  PropList.data.otherLowercase.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkOtherMath : Array (UInt32 × UInt32) :=
  PropList.data.otherMath.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkOtherUppercase : Array (UInt32 × UInt32) :=
  PropList.data.otherUppercase.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkOtherDefaultIgnorableCodePoint : Array (UInt32 × UInt32) :=
  PropList.data.otherDefaultIgnorableCodePoint.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkPrependedConcatenationMark : Array (UInt32 × UInt32) :=
  PropList.data.prependedConcatenationMark.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkVariationSelector : Array (UInt32 × UInt32) :=
  PropList.data.variationSelector.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkOther : Array (UInt32 × UInt32 × UInt32) :=
  let ol := mkOtherLowercase |>.map fun (c₀, c₁) => (c₀, c₁, 1)
  let ou := mkOtherUppercase |>.map fun (c₀, c₁) => (c₀, c₁, 2)
  let oa := mkOtherAlphabetic |>.filterMap fun (c₀, c₁) =>
    if c₀ ∈ #[0x0345, 0x24B6, 0x24D0, 0x1F130, 0x1F150, 0x1F170]
    then none
    else some (c₀, c₁, 3)
  let om := mkOtherMath |>.map fun (c₀, c₁) => (c₀, c₁, 4)
  mergeData #[ol, ou, oa, om]

def mkAlphabetic : IO <| Array (UInt32 × UInt32) := do
  let mut t := #[]
  for (c₀, c₁, gc) in ← mkGeneralCategory do
    if gc ⊆ .LC ||| .Ll ||| .Lu ||| .Lt ||| .Lm ||| .Lo ||| .Nl then
      match t.back? with
      | some (a₀, a₁) =>
        if c₀ == a₁ + 1 then
          t := t.pop.push (a₀, c₁)
        else
          t := t.push (c₀, c₁)
      | none =>
        t := t.push (c₀, c₁)
    else continue
  return mergeProp #[t, mkOtherAlphabetic]

def mkCased : IO <| Array (UInt32 × UInt32) := do
  let t := (← mkGeneralCategory).filterMap fun d =>
    if d.2.2 ∈ [.LC, .Ll, .Lu, .Lt] then
      some (d.1, d.2.1)
    else
      none
  return mergeProp #[t, mkOtherLowercase, mkOtherUppercase]

def mkDefaultIgnorableCodePoint : IO <| Array (UInt32 × UInt32) := do
  let t := (← mkGeneralCategory).filterMap fun d =>
    if d.2.2 = .Cf then some (d.1, d.2.1) else none
  let t ← t.flatMapM fun (c₀, c₁) => do
    let mut u := #[]
    for c in [c₀.toNat:c₁.toNat+1] do
      let c := c.toUInt32
      if 0xFFF9 ≤ c && c ≤ 0xFFFB then continue
      if 0x13430 ≤ c && c ≤ 0x1343F then continue
      if PropList.isPrependedConcatenationMark c then continue
      if PropList.isWhiteSpace c then continue
      match u.back? with
      | some (a, b) =>
        if c = b+1 then
          u := u.pop.push (a, c)
        else
          u := u.push (c, c)
      | none =>
        u := u.push (c, c)
    return u
  return mergeProp #[t, mkOtherDefaultIgnorableCodePoint, mkVariationSelector]

def mkMath : IO <| Array (UInt32 × UInt32) := do
  let t := (← mkGeneralCategory).filterMap fun
    | (c₀, c₁, .Sm) => some (c₀, c₁)
    | _ => none
  return mergeProp #[t, mkOtherMath]

def mkLowercase : IO <| Array (UInt32 × UInt32) := do
  let mut t := #[]
  for (c₀, c₁, gc) in ← mkGeneralCategory do
    if gc = .Ll then
      t := t.push (c₀, c₁)
    else if gc = .LC then
      for c in [c₀.toNat:c₁.toNat+1] do
        if c % 2 != 0 then t := t.push (c.toUInt32, c.toUInt32)
    else continue
  return mergeProp #[t, mkOtherLowercase]

def mkTitlecase : IO <| Array (UInt32 × UInt32) := do
  let mut t := #[]
  for (c₀, c₁, gc) in ← mkGeneralCategory do
    if gc = .Lt then
      t := t.push (c₀, c₁)
    else continue
  return t

def mkUppercase : IO <| Array (UInt32 × UInt32) := do
  let mut t := #[]
  for (c₀, c₁, gc) in ← mkGeneralCategory do
    if gc = .Lu then
      t := t.push (c₀, c₁)
    else if gc = .LC then
      for c in [c₀.toNat:c₁.toNat+1] do
        if c % 2 == 0 then t := t.push (c.toUInt32, c.toUInt32)
    else continue
  return mergeProp #[t, mkOtherUppercase]

def mkWhiteSpace : Array (UInt32 × UInt32) :=
  PropList.data.whiteSpace.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkScriptName : Array (UInt32 × String) := Id.run do
  let mut t : Array (UInt32 × String) := #[]
  let txt : String := PropertyValueAliases.txt
  let stream := UCDStream.ofString txt
  for record in stream do
    if record[0]! == "sc" then
      let s := Script.ofAbbrev! record[1]!.copy
      t := t.push (s.code, record[2]!.toString)
  return Array.qsort t fun (a, _) (b, _) => a < b

def mkIDStart : Array (UInt32 × UInt32) :=
  (DerivedCoreProperties.data.idStart).map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkIDContinue : Array (UInt32 × UInt32) :=
  (DerivedCoreProperties.data.idContinue).map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkXIDStart : Array (UInt32 × UInt32) :=
  (DerivedCoreProperties.data.xidStart).map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkXIDContinue : Array (UInt32 × UInt32) :=
  (DerivedCoreProperties.data.xidContinue).map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkDash : Array (UInt32 × UInt32) :=
  PropList.data.dash.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkHyphen : Array (UInt32 × UInt32) :=
  PropList.data.hyphen.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkQuotationMark : Array (UInt32 × UInt32) :=
  PropList.data.quotationMark.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkTerminalPunctuation : Array (UInt32 × UInt32) :=
  PropList.data.terminalPunctuation.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)


def mkExtender : Array (UInt32 × UInt32) :=
  PropList.data.extender.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkRegionalIndicator : Array (UInt32 × UInt32) :=
  PropList.data.regionalIndicator.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkCaseFolding : Array (UInt32 × String) :=
  (CaseFolding.data).filterMap fun e =>
    if e.status == 'C' || e.status == 'F' then
      let m := ";".intercalate (e.mapping.map toHexStringRaw).toList
      some (e.code, m)
    else
      none

def mkSimpleCaseFolding : Array (UInt32 × UInt32) :=
  (CaseFolding.data).filterMap fun e =>
    if (e.status == 'C' || e.status == 'S') && e.mapping.size == 1 then
      some (e.code, e.mapping[0]!)
    else
      none

def mkGraphemeBreak : Array (UInt32 × UInt32 × String) :=
  (BreakProperties.data.graphemeBreak).map fun (c₀, c₁, v) =>
    (c₀, match c₁ with | some c => c | none => c₀, v)

def mkWordBreak : Array (UInt32 × UInt32 × String) :=
  (BreakProperties.data.wordBreak).map fun (c₀, c₁, v) =>
    (c₀, match c₁ with | some c => c | none => c₀, v)


def mkDiacritic : Array (UInt32 × UInt32) :=
  PropList.data.diacritic.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkSentenceTerminal : Array (UInt32 × UInt32) :=
  PropList.data.sentenceTerminal.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkPatternSyntax : Array (UInt32 × UInt32) :=
  PropList.data.patternSyntax.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkPatternWhiteSpace : Array (UInt32 × UInt32) :=
  PropList.data.patternWhiteSpace.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)


def mkEmoji : Array (UInt32 × UInt32) :=
  (EmojiData.data.emoji).map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkEmojiPresentation : Array (UInt32 × UInt32) :=
  (EmojiData.data.emojiPresentation).map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkEmojiModifier : Array (UInt32 × UInt32) :=
  (EmojiData.data.emojiModifier).map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkEmojiModifierBase : Array (UInt32 × UInt32) :=
  (EmojiData.data.emojiModifierBase).map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkEmojiComponent : Array (UInt32 × UInt32) :=
  (EmojiData.data.emojiComponent).map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkExtendedPictographic : Array (UInt32 × UInt32) :=
  (EmojiData.data.extendedPictographic).map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkSentenceBreak : Array (UInt32 × UInt32 × String) :=
  (BreakProperties.data.sentenceBreak).map fun (c₀, c₁, v) =>
    (c₀, match c₁ with | some c => c | none => c₀, v)

def mkLineBreak : Array (UInt32 × UInt32 × String) :=
  (BreakProperties.data.lineBreak).map fun (c₀, c₁, v) =>
    (c₀, match c₁ with | some c => c | none => c₀, v)

def mkGraphemeBase : Array (UInt32 × UInt32) :=
  (DerivedCoreProperties.data.graphemeBase).map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkGraphemeExtend : Array (UInt32 × UInt32) :=
  (DerivedCoreProperties.data.graphemeExtend).map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)

def mkScript : Array (UInt32 × UInt32 × String) := Id.run do
  let t := Scripts.data.toArray.flatMap fun (sc, ranges) =>
    let sc := Script.ofAbbrev! <| (PropertyValueAliases.getShortName! "sc" sc).copy
    ranges.map fun (c₀, c₁) => (c₀, c₁, sc.toAbbrev)
  return t.qsort fun (a, _, _) (b, _, _) => a < b

def mkScriptExtensions : Array (UInt32 × String) := Id.run do
  let mut t_arr : Array (UInt32 × UInt32 × String) := #[]
  let txt : String := ScriptExtensions.txt
  let stream := UCDStream.ofString txt
  for record in stream do
    let r0 : String.Slice := record[0]!
    let (c₀, c₁) : UInt32 × UInt32 :=
      match r0.split ".." |>.toList with
      | [c] => (ofHexString! c, ofHexString! c)
      | [c₀, c₁] => (ofHexString! c₀, ofHexString! c₁)
      | _ => panic! "invalid record in ScriptExtensions.txt"
    let r1 : String.Slice := record[1]!
    let val : String := r1.copy
    t_arr := t_arr.push (c₀, c₁, val)
  let t_res := compressData t_arr
  return t_res.map fun (c₀, c₁, val) => (c₀, (toHexStringRaw c₁) ++ ";" ++ val)


private def lines (rows : Array String) : String :=
  String.intercalate "\n" rows.toList ++ "\n"

private def propText (table : Array (UInt32 × UInt32)) : String :=
  lines <| table.map fun (c₀, c₁) =>
    if c₀ == c₁ then
      toHexStringRaw c₀ ++ ";"
    else
      toHexStringRaw c₀ ++ ";" ++ toHexStringRaw c₁

private def rangeText [ToString α] (table : Array (UInt32 × UInt32 × α)) : String :=
  lines <| table.map fun (c₀, c₁, v) =>
    if c₀ == c₁ then
      ";".intercalate [toHexStringRaw c₀, "", toString v]
    else
      ";".intercalate [toHexStringRaw c₀, toHexStringRaw c₁, toString v]

private def pairText (table : Array (UInt32 × UInt32)) : String :=
  lines <| table.map fun (c, v) =>
    toHexStringRaw c ++ ";" ++ toHexStringRaw v

private def stringPairText (table : Array (UInt32 × String)) : String :=
  lines <| table.map fun (c, v) =>
    toHexStringRaw c ++ ";" ++ v

private def renderGeneratedTable (table : GeneratedTable) : String :=
  table.render

private def spaces (n : Nat) : String :=
  String.ofList <| List.replicate n ' '

private def printIndented (indent : Nat) (s : String) : IO Unit :=
  IO.println <| spaces indent ++ s

private def logGeneratedTable (table : GeneratedTable) : IO Unit := do
  printIndented 0 s!"{bold "🧾"} {bold <| blue table.name}"
  printIndented 2 s!"{dim "size"} {bold ":"} {table.size.describe}"
  if table.sorted then
    let before := analyzeLayoutSource table.rawLayout
    let after := analyzeLayoutSource table.layout
    printIndented 2 s!"{dim "sort"} {bold ":"} yes"
    if before.kind != after.kind then
      printIndented 4 s!"{dim "structure"} {bold ":"} {before.kind.label} -> {after.kind.label}"
  let report := analyzeLayoutSource table.layout
  printIndented 2 s!"{dim "layout"} {bold ":"}"
  printIndented 4 s!"{report.kind.emoji} {report.kind.kind.label}"
  printIndented 6 s!"{dim "density"} {bold ":"} {report.kind.density.label}"
  printIndented 6 s!"{dim "value fields"} {bold ":"} {report.valueFieldCount}"
  match report.kind.isInvalid with
  | some why => printIndented 6 s!"{dim "invalid"} {bold ":"} {why.label}"
  | none => pure ()
  printIndented 6 s!"{dim "bounds"} {bold ":"} [{toHexStringRaw report.start}..{toHexStringRaw report.stop}]"
  printIndented 6 s!"{dim "rows"} {bold ":"} {report.rowCount}"
  printIndented 6 s!"{dim "code points"} {bold ":"} {report.codePointCount}"

private def mkPropTable (name : String) (table : Array (UInt32 × UInt32)) : GeneratedTable :=
  let (rowCount, spanCount) := statsProp table
  let sorted := sortPairTable table
  {
    name,
    size := .ranges rowCount spanCount,
    rawLayout := .range table 0,
    layout := .range sorted 0,
    sorted := !isSortedPairTable table,
    render := propText sorted
  }

private def mkPairTable (name : String) (table : Array (UInt32 × UInt32)) : GeneratedTable :=
  let sorted := sortPairTable table
  {
    name,
    size := .rows table.size,
    rawLayout := .pair table,
    layout := .pair sorted,
    sorted := !isSortedPairTable table,
    render := pairText sorted
  }

private def mkRangeTable (name : String) (table : Array (UInt32 × UInt32 × α)) (render : Array (UInt32 × UInt32 × α) → String) : GeneratedTable :=
  let (rowCount, spanCount) := statsData table
  let sorted := sortRangeTable table
  {
    name,
    size := .ranges rowCount spanCount,
    rawLayout := .range (table.map fun (c₀, c₁, _) => (c₀, c₁)) 1,
    layout := .range (sorted.map fun (c₀, c₁, _) => (c₀, c₁)) 1,
    sorted := !isSortedRangeTable table,
    render := render sorted
  }

private def mkKeyTable (name : String) (table : Array (UInt32 × α)) (render : Array (UInt32 × α) → String) : GeneratedTable :=
  let sorted := sortKeyTable table
  {
    name,
    size := .rows table.size,
    rawLayout := .key <| table.map Prod.fst,
    layout := .key <| sorted.map Prod.fst,
    sorted := !isSortedKeyTable table,
    render := render sorted
  }

private def buildTable (name : String) : IO GeneratedTable := do
  match name with
  | "Alphabetic" =>
    let table ← mkAlphabetic
    return mkPropTable name table
  | "Bidi_Class" =>
    let table ← mkBidiClass
    return mkRangeTable name table (fun table =>
      rangeText <| table.map fun (c₀, c₁, v) => (c₀, c₁, v.toAbbrev))
  | "Bidi_Mirroring_Glyph" =>
    let table := mkBidiMirroringGlyph
    return mkPairTable name table
  | "Bidi_Brackets" =>
    let table := mkBidiBrackets
    return mkKeyTable name table (fun table =>
      lines <| table.map fun (c, paired, bracketType) =>
        ";".intercalate [toHexStringRaw c, toHexStringRaw paired, toString bracketType]
    )
  | "Block_Name" =>
    let table := mkBlockName
    return mkRangeTable name table rangeText
  | "East_Asian_Width" =>
    let table := mkEastAsianWidth
    return mkRangeTable name table rangeText
  | "Vertical_Orientation" =>
    let table := mkVerticalOrientation
    return mkRangeTable name table rangeText
  | "Canonical_Combining_Class" =>
    let table ← mkCanonicalCombiningClass
    return mkRangeTable name table rangeText
  | "Canonical_Decomposition_Mapping" =>
    let table ← mkCanonicalDecompositionMapping
    return mkKeyTable name table (fun table =>
      lines <| table.map fun (c, l) =>
        toHexStringRaw c ++ ";" ++ ";".intercalate (l.map fun c => toHexStringRaw c.val)
    )
  | "Case_Mapping" =>
    let table ← mkCaseMapping
    return mkRangeTable name table (fun table =>
      lines <| table.map fun (c₀, c₁, uc, lc, tc) =>
        let base :=
          if c₀ == c₁ then
            toHexStringRaw c₀ ++ ";" ++
              (if c₀ == uc then ";" else ";" ++ toHexStringRaw uc) ++
              (if c₀ == lc then ";" else ";" ++ toHexStringRaw lc)
          else
            ";".intercalate <| [c₀, c₁, uc, lc].map toHexStringRaw
        if uc == tc then base ++ ";" else base ++ ";" ++ toHexStringRaw tc)
  | "Cased" =>
    let table ← mkCased
    return mkPropTable name table
  | "Decomposition_Mapping" =>
    let table ← mkDecompositionMapping
    return mkKeyTable name table (fun table =>
      lines <| table.map fun (c, s) => toHexStringRaw c ++ ";" ++ s
    )
  | "Default_Ignorable_Code_Point" =>
    let table ← mkDefaultIgnorableCodePoint
    return mkPropTable name table
  | "General_Category" =>
    let table ← mkGC
    return mkRangeTable name table rangeText
  | "Lowercase" =>
    let table ← mkLowercase
    return mkPropTable name table
  | "Math" =>
    let table ← mkMath
    return mkPropTable name table
  | "Name" =>
    let table ← mkName
    return mkRangeTable name table rangeText
  | "Numeric_Value" =>
    let table ← mkNumericValue
    return mkRangeTable name table (fun table =>
      lines <| table.map fun (c₀, c₁, n) =>
        match n with
        | .decimal _ => ";".intercalate [toHexStringRaw c₀, toHexStringRaw c₁, "decimal"]
        | .digit v =>
          if c₀ == c₁ then
            ";".intercalate [toHexStringRaw c₀, "", s!"digit {v.val}"]
          else
            let last := v.val + c₁.toNat - c₀.toNat
            ";".intercalate [toHexStringRaw c₀, toHexStringRaw c₁, s!"digit {v.val}-{last}"]
        | .numeric v none => ";".intercalate [toHexStringRaw c₀, "", s!"numeric {v}"]
        | .numeric v (some d) => ";".intercalate [toHexStringRaw c₀, "", s!"numeric {v}/{d}"])
  | "Other_Alphabetic" =>
    let table := mkOtherAlphabetic
    return mkPropTable name table
  | "Other_Lowercase" =>
    let table := mkOtherLowercase
    return mkPropTable name table
  | "Other_Math" =>
    let table := mkOtherMath
    return mkPropTable name table
  | "Other_Uppercase" =>
    let table := mkOtherUppercase
    return mkPropTable name table
  | "Bidi_Mirrored" =>
    let table ← mkBidiMirrored
    return mkPropTable name table
  | "White_Space" =>
    let table := mkWhiteSpace
    return mkPropTable name table
  | "Script" =>
    let table := mkScript
    return mkRangeTable name table rangeText
  | "Script_Name" =>
    let table := mkScriptName
    return mkKeyTable name table stringPairText
  | "Script_Extensions" =>
    let table := mkScriptExtensions
    return mkKeyTable name table (fun table =>
      lines <| table.map fun (c₀, data) => toHexStringRaw c₀ ++ ";" ++ data
    )
  | "ID_Start" => pureProp name mkIDStart
  | "ID_Continue" => pureProp name mkIDContinue
  | "XID_Start" => pureProp name mkXIDStart
  | "XID_Continue" => pureProp name mkXIDContinue
  | "Dash" => pureProp name mkDash
  | "Hyphen" => pureProp name mkHyphen
  | "Quotation_Mark" => pureProp name mkQuotationMark
  | "Terminal_Punctuation" => pureProp name mkTerminalPunctuation
  | "Extender" => pureProp name mkExtender
  | "Regional_Indicator" => pureProp name mkRegionalIndicator
  | "Case_Folding" =>
    let table := mkCaseFolding
    return mkKeyTable name table (fun table =>
      lines <| table.map fun (c, m) => toHexStringRaw c ++ ";" ++ m
    )
  | "Simple_Case_Folding" =>
    let table := mkSimpleCaseFolding
    return mkPairTable name table
  | "Grapheme_Break" =>
    let table := mkGraphemeBreak
    return mkRangeTable name table rangeText
  | "Word_Break" =>
    let table := mkWordBreak
    return mkRangeTable name table rangeText
  | "Diacritic" => pureProp name mkDiacritic
  | "Sentence_Terminal" => pureProp name mkSentenceTerminal
  | "Pattern_Syntax" => pureProp name mkPatternSyntax
  | "Pattern_White_Space" => pureProp name mkPatternWhiteSpace
  | "Emoji" => pureProp name mkEmoji
  | "Emoji_Presentation" => pureProp name mkEmojiPresentation
  | "Emoji_Modifier" => pureProp name mkEmojiModifier
  | "Emoji_Modifier_Base" => pureProp name mkEmojiModifierBase
  | "Emoji_Component" => pureProp name mkEmojiComponent
  | "Extended_Pictographic" => pureProp name mkExtendedPictographic
  | "Sentence_Break" =>
    let table := mkSentenceBreak
    return mkRangeTable name table rangeText
  | "Line_Break" =>
    let table := mkLineBreak
    return mkRangeTable name table rangeText
  | "Grapheme_Base" => pureProp name mkGraphemeBase
  | "Grapheme_Extend" => pureProp name mkGraphemeExtend
  | "Uppercase" =>
    let table ← mkUppercase
    return mkPropTable name table
  | other => throw <| IO.userError s!"unknown lookup table {other}"
where
  pureProp (name : String) (table : Array (UInt32 × UInt32)) : IO GeneratedTable :=
    return mkPropTable name table

private def tableText (name : String) : IO String := do
  let table ← buildTable name
  logGeneratedTable table
  return renderGeneratedTable table

public def main (_args : List String) : IO UInt32 := do
  MakeTablesForLookup.generateFromTexts (fun spec => tableText spec.fileName) (".." / "lib" / "UnicodeBasic" / "TableLookupTables")
  return 0
