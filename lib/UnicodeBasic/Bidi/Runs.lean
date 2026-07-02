/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeBasic.Bidi.Explicit

/-!
  This file uses lookup tables:
  `BidiBrackets`.
-/

namespace Unicode.BidiInternal

public def resolveWeak (sot : BidiClass) (seq : Array Nat) (items0 : Array Item) : Array Item := Id.run do
  let mut items := items0
  for pos in [:seq.size] do
    let i := seq[pos]!
    if items[i]!.cls == .nonspacingMark then
      items := setClass items i (match previousInSeq? items seq pos with | some bc => bc | none => sot)
  for pos in [:seq.size] do
    let i := seq[pos]!
    if items[i]!.cls == .europeanNumber then
      let prev := prevStrongOrSot items seq pos sot
      if prev == .arabicLetter then
        items := setClass items i .arabicNumber
  for pos in [:seq.size] do
    let i := seq[pos]!
    if items[i]!.cls == .arabicLetter then
      items := setClass items i .rightToLeft
  if seq.size ≥ 3 then
    for pos in [1:seq.size - 1] do
      let i := seq[pos]!
      let bc := items[i]!.cls
      let prev := items[seq[pos - 1]!]!.cls
      let next := items[seq[pos + 1]!]!.cls
      if bc == .europeanSeparator && prev == .europeanNumber && next == .europeanNumber then
        items := setClass items i .europeanNumber
      else if bc == .commonSeparator && prev == .europeanNumber && next == .europeanNumber then
        items := setClass items i .europeanNumber
      else if bc == .commonSeparator && prev == .arabicNumber && next == .arabicNumber then
        items := setClass items i .arabicNumber
  for pos in [:seq.size] do
    let i := seq[pos]!
    if items[i]!.cls == .europeanTerminator then
      let mut hasEN := false
      let mut j := pos
      while j > 0 && items[seq[j - 1]!]!.cls == .europeanTerminator do
        j := j - 1
      if j > 0 && items[seq[j - 1]!]!.cls == .europeanNumber then
        hasEN := true
      j := pos + 1
      while j < seq.size && items[seq[j]!]!.cls == .europeanTerminator do
        j := j + 1
      if j < seq.size && items[seq[j]!]!.cls == .europeanNumber then
        hasEN := true
      if hasEN then
        items := setClass items i .europeanNumber
  for pos in [:seq.size] do
    let i := seq[pos]!
    match items[i]!.cls with
    | .europeanSeparator | .commonSeparator | .europeanTerminator =>
        items := setClass items i .otherNeutral
    | _ => pure ()
  for pos in [:seq.size] do
    let i := seq[pos]!
    if items[i]!.cls == .europeanNumber then
      let prev := prevStrongOrSot items seq pos sot
      if prev == .leftToRight then
        items := setClass items i .leftToRight
  return items

public structure BracketOpen where
  pos : Nat
  pair : UInt32
deriving Inhabited

public def canonicalBracketKey (c : UInt32) : UInt32 :=
  if c == 0x2329 then 0x3008
  else if c == 0x232A then 0x3009
  else c

public def strongOrNumberForBracket : BidiClass → Option BidiClass
  | .leftToRight => some .leftToRight
  | .rightToLeft | .arabicNumber | .europeanNumber => some .rightToLeft
  | _ => none

public def prevBracketContextOrSot (items : Array Item) (seq : Array Nat) (pos : Nat) (sot : BidiClass) : BidiClass := Id.run do
  let mut j := pos
  while j > 0 do
    j := j - 1
    match strongOrNumberForBracket items[seq[j]!]!.cls with
    | some bc => return bc
    | none => pure ()
  return sot

public def resolveBrackets (sot : BidiClass) (seq : Array Nat) (items0 : Array Item) : Array Item := Id.run do
  let mut items := items0
  let mut pairs : Array (Nat × Nat) := #[]
  let mut stack : Array BracketOpen := #[]
  let mut overflowBrackets := 0
  let mut overflowSeen := false
  for pos in [:seq.size] do
    let i := seq[pos]!
    if items[i]!.cls == .otherNeutral then
      match items[i]!.code with
      | none => pure ()
      | some c =>
          match lookupBidiPairedBracketType? c with
          | some .openBracket =>
              if stack.size < 63 then
                match lookupBidiPairedBracket? c with
                | some p => stack := stack.push { pos, pair := canonicalBracketKey p }
                | none => pure ()
              else
                overflowBrackets := overflowBrackets + 1
                overflowSeen := true
          | some .closeBracket =>
              if overflowBrackets > 0 then
                overflowBrackets := overflowBrackets - 1
              else
                let mut found : Option Nat := none
                let mut j := stack.size
                while j > 0 && found.isNone do
                  j := j - 1
                  if stack[j]!.pair == canonicalBracketKey c then
                    found := some j
                match found with
                | none => pure ()
                | some foundAt =>
                    pairs := pairs.push (stack[foundAt]!.pos, pos)
                    stack := stack.extract 0 foundAt
          | none => pure ()
  let processPairs := if overflowSeen then #[] else pairs
  for p in processPairs.qsort (fun a b => if a.1 == b.1 then a.2 < b.2 else a.1 < b.1) do
    let openPos := p.1
    let closePos := p.2
    let embedding := dirClassOfLevel (items[seq[openPos]!]!.level.getD 0)
    let opposite := if embedding == .leftToRight then BidiClass.rightToLeft else BidiClass.leftToRight
    let mut hasEmbedding := false
    let mut hasOpposite := false
    let mut k := openPos + 1
    while k < closePos do
      match strongOrNumberForBracket items[seq[k]!]!.cls with
      | some bc =>
          if bc == embedding then
            hasEmbedding := true
          else if bc == opposite then
            hasOpposite := true
      | none => pure ()
      k := k + 1
    if hasEmbedding || hasOpposite then
      let resolved :=
        if hasEmbedding then
          embedding
        else
          let before := prevBracketContextOrSot items seq openPos sot
          if before == opposite then opposite else embedding
      items := setClass items seq[openPos]! resolved
      items := setClass items seq[closePos]! resolved
      let mut openNsm := openPos + 1
      while openNsm < seq.size && items[seq[openNsm]!]!.orig == .nonspacingMark do
        items := setClass items seq[openNsm]! resolved
        openNsm := openNsm + 1
      let mut closeNsm := closePos + 1
      while closeNsm < seq.size && items[seq[closeNsm]!]!.orig == .nonspacingMark do
        items := setClass items seq[closeNsm]! resolved
        closeNsm := closeNsm + 1
      let mut j := openPos + 1
      while j < closePos do
        let ix := seq[j]!
        if items[ix]!.cls == .nonspacingMark then
          items := setClass items ix resolved
        j := j + 1
  return items

public def resolveNeutrals (sot eot : BidiClass) (seq : Array Nat) (items0 : Array Item) : Array Item := Id.run do
  let mut items := items0
  let mut pos := 0
  while pos < seq.size do
    let i := seq[pos]!
    if !isNeutralItem items[i]! then
      pos := pos + 1
    else
      let start := pos
      while pos < seq.size && isNeutralItem items[seq[pos]!]! do
        pos := pos + 1
      let stop := pos
      let before := if start == 0 then sot else items[seq[start - 1]!]!.cls
      let after := if stop == seq.size then eot else items[seq[stop]!]!.cls
      let beforeDir := if before == .leftToRight then .leftToRight else if before == .rightToLeft || isNumber before then .rightToLeft else sot
      let afterDir := if after == .leftToRight then .leftToRight else if after == .rightToLeft || isNumber after then .rightToLeft else eot
      let resolved := if beforeDir == afterDir then beforeDir else dirClassOfLevel (items[i]!.level.getD 0)
      for j in [start:stop] do
        items := setClass items seq[j]! resolved
  return items

public def contiguousLevelRuns (items : Array Item) : Array (Array Nat) := Id.run do
  let mut runs := #[]
  let mut i := 0
  while i < items.size do
    match items[i]!.level with
    | none => i := i + 1
    | some level =>
        let mut run := #[]
        while i < items.size && (items[i]!.level.isNone || items[i]!.level == some level) do
          if items[i]!.level == some level then
            run := run.push i
          i := i + 1
        runs := runs.push run
  return runs

public def previousRunLevel (runs : Array (Array Nat)) (r : Nat) (items : Array Item) (paragraph : Nat) : Nat :=
  let first := items[runs[r]![0]!]!
  match first.matchingInitiator with
  | some i => items[i]!.level.getD paragraph
  | none => if r == 0 then paragraph else items[runs[r - 1]![0]!]!.level.getD paragraph

public def nextRunLevel (runs : Array (Array Nat)) (r : Nat) (items : Array Item) (paragraph : Nat) : Nat := Id.run do
  let last := items[runs[r]![runs[r]!.size - 1]!]!
  if isIsolateInitiator last.orig then
    let mut found : Option Nat := none
    for item in items do
      if item.matchingInitiator == some last.index then
        found := item.level
    return found.getD paragraph
  else
    return if r + 1 < runs.size then items[runs[r + 1]![0]!]!.level.getD paragraph else paragraph

public def findMatchingPdiIndex (items : Array Item) (initiator : Nat) : Option Nat := Id.run do
  for item in items do
    if item.matchingInitiator == some initiator then
      return some item.index
  return none

public def findRunStartingAt (runs : Array (Array Nat)) (i : Nat) : Option Nat := Id.run do
  for r in [:runs.size] do
    if runs[r]!.size > 0 && runs[r]![0]! == i then
      return some r
  return none

public def resolveRuns (paragraph : Nat) (items0 : Array Item) : Array Item := Id.run do
  let runs := contiguousLevelRuns items0
  let mut items := items0
  let mut visited : Array Bool := Array.replicate runs.size false
  for r in [:runs.size] do
    if visited[r]! then
      continue
    let seq := runs[r]!
    if seq.size == 0 then
      continue
    let mut seq := seq
    let mut lastRun := r
    visited := visited.set! r true
    let mut keepGoing := true
    while keepGoing do
      keepGoing := false
      let lastIndex := runs[lastRun]![runs[lastRun]!.size - 1]!
      let lastItem := items[lastIndex]!
      if isIsolateInitiator lastItem.orig && lastItem.matchedIsolate then
        match findMatchingPdiIndex items lastItem.index with
        | some pdi =>
            match findRunStartingAt runs pdi with
            | some nextRun =>
                if nextRun != lastRun && !visited[nextRun]! then
                  seq := seq ++ runs[nextRun]!
                  visited := visited.set! nextRun true
                  lastRun := nextRun
                  keepGoing := true
            | none => pure ()
        | none => pure ()
    let level := items[seq[0]!]!.level.getD paragraph
    let sot := dirClassOfLevel (max level (previousRunLevel runs r items paragraph))
    let eot := dirClassOfLevel (max (items[seq[seq.size - 1]!]!.level.getD paragraph) (nextRunLevel runs lastRun items paragraph))
    items := resolveWeak sot seq items
    items := resolveBrackets sot seq items
    items := resolveNeutrals sot eot seq items
  return items

public def resolveImplicitLevels (items0 : Array Item) : Array Item := Id.run do
  let mut items := items0
  for i in [:items.size] do
    match items[i]!.level with
    | none => pure ()
    | some level =>
        if isIsolateInitiator items[i]!.orig && !items[i]!.matchedIsolate then
          pure ()
        else
          let bc := items[i]!.cls
          let level :=
            if level % 2 == 0 then
              if bc == .rightToLeft then level + 1
              else if bc == .arabicNumber || bc == .europeanNumber then level + 2
              else level
            else
              if bc == .leftToRight || bc == .arabicNumber || bc == .europeanNumber then level + 1
              else level
          items := items.set! i { items[i]! with level := some level }
  return items

end Unicode.BidiInternal
