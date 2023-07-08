import Fullstackalpha.Http.Basic

import Lean.Data.Parsec

open Lean.Parsec

namespace Fullstackalpha.Http

structure StatusCode where
  code : Nat
  reason : String

def StatusCode.fromNat : Nat -> String
  | 200 => "200 OK"
  | _ => "500 Unknown status code"

def StatusCode.asString (c : StatusCode) : String := s!"{c.code} {c.reason}"

instance : Repr StatusCode where
  reprPrec := fun c _ => c.asString
  

structure HttpResponse where
  statusCode : StatusCode
  body : Option String

def HttpResponse.render (resp : HttpResponse) : String := 
  let statusLine := "HTTP/1.1 " ++ resp.statusCode.asString
  let contentLength := "Content-length:" ++ (resp.body.map (λ b => Nat.repr b.length)).getD "0" 
  statusLine ++ contentLength ++ (resp.body.map (λ b => "\r\n\r\n" ++ b)).getD ""  ++ "\r\n\r\n"

def isNotWhitespace : Char → Bool := fun c => ¬ (c = '\u0009' ∨ c = '\u000a' ∨ c = '\u000d' ∨ c = '\u0020')

def isNotReturn : Char → Bool := fun c => ¬ (c = '\n' ∨ c = '\r')

def statusCode : Lean.Parsec Statuscode := do
  let c1 ← digit
  let c2 ← digit
  let c3 ← digit
  ws
  let reason ← many (satisfy isNotWhitespace)
  pure {
    code := 
  }


def httpResponse : Lean.Parsec HttpResponse := do
  let d ← (pstring "HTTP/" *> digit *> pchar '.' *> digit)
  let uri ← (ws *> many (satisfy isNotWhitespace))
  pure {
    method := meth
    uri := String.mk (uri.toList)
  }

def HttpResponse.parse (s : String) : Except ParseError HttpResponse :=
  match httpResponse s.mkIterator with
  | Lean.Parsec.ParseResult.success _ res => Except.ok res
  | Lean.Parsec.ParseResult.error it err  => Except.error s!"offset {repr it.i.byteIdx}: {err}"