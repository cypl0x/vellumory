/-! Dependency-free JSON for the worker protocol.

Importing any `Lean.*` module makes the compiler emit `lean_initialize()`
for the whole Lean environment (Lean/Compiler/LCNF/EmitC.lean, `usesLeanAPI`):
measured 33 ms of startup and about 65 MB of resident memory per worker.
This module depends only on `Init`, so `main` initializes just the runtime.

The API mirrors the subset of `Lean.Json` the worker used. Objects keep
insertion order; on duplicate keys the last one wins, as with `Lean.Json`.
Numbers are IEEE doubles: integers up to 2^53 are exact. Non-integral
numbers print like `Lean.Json` (six decimals); non-finite numbers print as
`null`. -/

namespace Vellumory

inductive Json where
  | null
  | bool (value : Bool)
  | num (value : Float)
  | str (value : String)
  | arr (elements : Array Json)
  | obj (fields : Array (String × Json))
  deriving Inhabited

namespace Json

partial def beq : Json → Json → Bool
  | null, null => true
  | bool a, bool b => a == b
  | num a, num b => a == b
  | str a, str b => a == b
  | arr a, arr b => a.size == b.size && (a.zip b).all fun (x, y) => beq x y
  | obj a, obj b => a.size == b.size && (a.zip b).all fun ((k, x), (l, y)) => k == l && beq x y
  | _, _ => false

instance : BEq Json := ⟨beq⟩

def mkObj (fields : List (String × Json)) : Json := obj fields.toArray

def isNull : Json → Bool
  | null => true
  | _ => false

def getObjVal? (json : Json) (key : String) : Except String Json :=
  match json with
  | obj fields =>
    match fields.findRev? (·.1 == key) with
    | some (_, value) => .ok value
    | none => .error s!"property not found: {key}"
  | _ => .error "object expected"

/-- Missing keys read as `null`, so optional fields decode to `none`. -/
def getObjValD (json : Json) (key : String) : Json :=
  (json.getObjVal? key).toOption.getD null

def setObjVal! (json : Json) (key : String) (value : Json) : Json :=
  match json with
  | obj fields => obj ((fields.filter (·.1 != key)).push (key, value))
  | _ => json

/-! ## Printing -/

private def hexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)

def escape (s : String) (out : String := "") : String := Id.run do
  let mut out := out.push '"'
  for c in s.toList do
    out := match c with
      | '"' => out ++ "\\\""
      | '\\' => out ++ "\\\\"
      | '\n' => out ++ "\\n"
      | '\r' => out ++ "\\r"
      | '\t' => out ++ "\\t"
      | c => if c.toNat < 0x20 then
               (out ++ "\\u00").push (hexDigit (c.toNat / 16)) |>.push (hexDigit (c.toNat % 16))
             else out.push c
  return out.push '"'

def numberString (f : Float) : String :=
  if f.isNaN || f.isInf then "null"
  else if f == f.floor && f.abs < 9007199254740992.0 then
    if f < 0 then "-" ++ toString (-f).toUInt64.toNat
    else toString f.toUInt64.toNat
  else toString f

partial def render (json : Json) (out : String) : String :=
  match json with
  | null => out ++ "null"
  | bool true => out ++ "true"
  | bool false => out ++ "false"
  | num n => out ++ numberString n
  | str s => escape s out
  | arr elements => Id.run do
    let mut out := out.push '['
    for h : i in [0:elements.size] do
      if i > 0 then out := out.push ','
      out := render elements[i] out
    return out.push ']'
  | obj fields => Id.run do
    let mut out := out.push '{'
    for h : i in [0:fields.size] do
      if i > 0 then out := out.push ','
      out := escape fields[i].1 out
      out := render fields[i].2 (out.push ':')
    return out.push '}'

/-- Compact, single-line rendering: never contains a raw newline. -/
def compress (json : Json) : String := render json ""

instance : ToString Json := ⟨compress⟩

/-! ## Parsing (RFC 8259) over UTF-8 bytes -/

private structure Parser where
  bytes : ByteArray
  pos : Nat
  depth : Nat

private abbrev P := StateT Parser (Except String)

private def peek : P (Option UInt8) := do
  let s ← get
  return if h : s.pos < s.bytes.size then some s.bytes[s.pos] else none

private def advance : P Unit := modify fun s => { s with pos := s.pos + 1 }

private def fail (message : String) : P α := do
  throw s!"{message} at byte {(← get).pos}"

private partial def skipSpace : P Unit := do
  match ← peek with
  | some 32 | some 9 | some 10 | some 13 => advance; skipSpace
  | _ => pure ()

private def expectByte (b : UInt8) : P Unit := do
  if (← peek) == some b then advance else fail s!"expected '{Char.ofNat b.toNat}'"

private def literal (text : String) (value : Json) : P Json := do
  for b in text.toUTF8 do expectByte b
  return value

private def hexValue (b : UInt8) : Option Nat :=
  if 48 ≤ b && b ≤ 57 then some (b - 48).toNat
  else if 97 ≤ b && b ≤ 102 then some (b - 87).toNat
  else if 65 ≤ b && b ≤ 70 then some (b - 55).toNat
  else none

private def hex4 : P Nat := do
  let mut n := 0
  for _ in [0:4] do
    let some b ← peek | fail "truncated \\u escape"
    let some v := hexValue b | fail "invalid \\u escape"
    advance
    n := n * 16 + v
  return n

private partial def parseString : P String := do
  expectByte 34
  let mut out : ByteArray := .empty
  repeat
    let some b ← peek | fail "unterminated string"
    advance
    if b == 34 then break
    if b < 0x20 then fail "control character in string"
    if b != 92 then
      out := out.push b
      continue
    let some e ← peek | fail "unterminated escape"
    advance
    match e with
    | 34 => out := out.push 34
    | 92 => out := out.push 92
    | 47 => out := out.push 47
    | 98 => out := out.push 8
    | 102 => out := out.push 12
    | 110 => out := out.push 10
    | 114 => out := out.push 13
    | 116 => out := out.push 9
    | 117 =>
      let mut code ← hex4
      if 0xD800 ≤ code && code < 0xDC00 then
        -- A high surrogate must be followed by a low one.
        expectByte 92; expectByte 117
        let low ← hex4
        unless 0xDC00 ≤ low && low < 0xE000 do fail "invalid surrogate pair"
        code := 0x10000 + (code - 0xD800) * 0x400 + (low - 0xDC00)
      else if 0xDC00 ≤ code && code < 0xE000 then fail "unpaired low surrogate"
      out := out ++ (String.singleton (Char.ofNat code)).toUTF8
    | _ => fail "invalid escape"
  match String.fromUTF8? out with
  | some s => return s
  | none => fail "invalid UTF-8 in string"

private partial def digits (acc : Nat) (count : Nat) : P (Nat × Nat) := do
  match ← peek with
  | some b => if 48 ≤ b && b ≤ 57 then advance; digits (acc * 10 + (b - 48).toNat) (count + 1)
              else return (acc, count)
  | none => return (acc, count)

private def parseNumber : P Json := do
  let negative := (← peek) == some 45
  if negative then advance
  let (whole, wholeDigits) ← digits 0 0
  if wholeDigits == 0 then fail "invalid number"
  let mut mantissa := whole
  let mut exponent : Int := 0
  if (← peek) == some 46 then
    advance
    let (fraction, count) ← digits 0 0
    if count == 0 then fail "digits expected after decimal point"
    mantissa := whole * 10 ^ count + fraction
    exponent := -count
  if (← peek) == some 101 || (← peek) == some 69 then
    advance
    let sign : Int ← match ← peek with
      | some 45 => advance; pure (-1)
      | some 43 => advance; pure 1
      | _ => pure 1
    let (e, count) ← digits 0 0
    if count == 0 then fail "digits expected in exponent"
    if e > 100000 then fail "exponent out of range"
    exponent := exponent + sign * e
  let magnitude := if exponent ≥ 0 then Float.ofScientific mantissa false exponent.toNat
                   else Float.ofScientific mantissa true (-exponent).toNat
  return num (if negative then -magnitude else magnitude)

private partial def parseValue : P Json := do
  skipSpace
  let s ← get
  if s.depth > 512 then fail "nesting deeper than 512"
  let result ← match ← peek with
    | some 123 => parseObject
    | some 91 => parseArray
    | some 34 => str <$> parseString
    | some 116 => literal "true" (bool true)
    | some 102 => literal "false" (bool false)
    | some 110 => literal "null" null
    | some b => if b == 45 || (48 ≤ b && b ≤ 57) then parseNumber else fail "unexpected character"
    | none => fail "unexpected end of input"
  skipSpace
  return result
where
  nested (body : P Json) : P Json := do
    modify fun s => { s with depth := s.depth + 1 }
    let value ← body
    modify fun s => { s with depth := s.depth - 1 }
    return value
  parseArray : P Json := nested do
    expectByte 91
    skipSpace
    let mut elements := #[]
    if (← peek) == some 93 then advance; return arr elements
    repeat
      elements := elements.push (← parseValue)
      match ← peek with
      | some 44 => advance
      | some 93 => advance; break
      | _ => fail "expected ',' or ']'"
    return arr elements
  parseObject : P Json := nested do
    expectByte 123
    skipSpace
    let mut fields := #[]
    if (← peek) == some 125 then advance; return obj fields
    repeat
      skipSpace
      let key ← parseString
      skipSpace
      expectByte 58
      fields := fields.push (key, ← parseValue)
      match ← peek with
      | some 44 => advance
      | some 125 => advance; break
      | _ => fail "expected ',' or '}'"
    return obj fields

def parse (text : String) : Except String Json := do
  let (value, state) ← parseValue.run { bytes := text.toUTF8, pos := 0, depth := 0 }
  if state.pos != state.bytes.size then throw s!"trailing data at byte {state.pos}"
  return value

end Json

/-! ## Conversions -/

class ToJson (α : Type) where
  toJson : α → Json

class FromJson (α : Type) where
  fromJson? : Json → Except String α

export ToJson (toJson)
export FromJson (fromJson?)

instance : ToJson Json := ⟨id⟩
instance : FromJson Json := ⟨.ok⟩
instance : ToJson Bool := ⟨.bool⟩
instance : ToJson String := ⟨.str⟩
instance : ToJson Float := ⟨.num⟩
instance : ToJson Nat := ⟨fun n => .num n.toFloat⟩
instance [ToJson α] : ToJson (Option α) := ⟨fun | some a => toJson a | none => .null⟩
instance [ToJson α] : ToJson (Array α) := ⟨fun xs => .arr (xs.map toJson)⟩
instance [ToJson α] : ToJson (List α) := ⟨fun xs => .arr (xs.toArray.map toJson)⟩
instance [ToJson α] [ToJson β] : ToJson (α × β) := ⟨fun (a, b) => .arr #[toJson a, toJson b]⟩

instance : FromJson Bool := ⟨fun | .bool b => .ok b | _ => .error "boolean expected"⟩
instance : FromJson String := ⟨fun | .str s => .ok s | _ => .error "string expected"⟩
instance : FromJson Float := ⟨fun | .num n => .ok n | _ => .error "number expected"⟩
instance : FromJson Nat where
  fromJson?
    | .num n => if n ≥ 0 && n == n.floor && n < 9007199254740992.0 then .ok n.toUInt64.toNat
                else .error "natural number expected"
    | _ => .error "natural number expected"
instance [FromJson α] : FromJson (Option α) := ⟨fun | .null => .ok none | j => some <$> fromJson? j⟩
instance [FromJson α] : FromJson (Array α) := ⟨fun | .arr xs => xs.mapM fromJson? | _ => .error "array expected"⟩
instance [FromJson α] : FromJson (List α) where
  fromJson? | .arr xs => (·.toList) <$> xs.mapM fromJson? | _ => .error "array expected"

def Json.getObjValAs? (json : Json) (α : Type) [FromJson α] (key : String) : Except String α :=
  fromJson? (json.getObjValD key)

end Vellumory
