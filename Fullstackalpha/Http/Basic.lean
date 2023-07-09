namespace Fullstackalpha.Http

inductive Method 
 | GET 

def Method.asString : Method → String
 | .GET => "GET"

structure Header where
  name : String
  value : String
deriving Repr

instance : ToString Header where
  toString := fun h => s!"{h.name}:{h.value}"

abbrev ParseError := String

abbrev Port := Nat

def Port.asString : Port → String := fun p => String.mk (Nat.toDigits 10 p)

def isNotWhitespace : Char → Bool := fun c => ¬ (c = '\u0009' ∨ c = '\u000a' ∨ c = '\u000d' ∨ c = '\u0020')