namespace Fullstackalpha.Http

inductive Method 
 | GET 

def Method.asString : Method → String
 | .GET => "GET"

abbrev Port := Nat

def Port.asString : Port → String := fun p => String.mk (Nat.toDigits 10 p)