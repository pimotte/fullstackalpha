import Fullstackalpha.Http.Basic

import Lean.Data.Parsec
import Lean.Data.Json.Parser

open Lean.Parsec

namespace Fullstackalpha.Http

structure StatusCode where
  code : Nat
  reason : String

def StatusCode.fromNat : Nat -> StatusCode
  | 200 => {code := 200, reason := "OK"}
  | 500 => {code := 500, reason := "Internal Server Error"}
  | _ => {code := 500, reason := "Unknown Status Code"}

def StatusCode.asString (c : StatusCode) : String := s!"{c.code} {c.reason}"

instance : ToString StatusCode where
  toString := StatusCode.asString

structure HttpResponse where
  statusCode : StatusCode
  body : Option String

def HttpResponse.render (resp : HttpResponse) : String := 
  let statusLine := "HTTP/1.1 " ++ resp.statusCode.asString
  let contentLength := "Content-length:" ++ (resp.body.map (λ b => Nat.repr b.length)).getD "0" 
  statusLine ++ contentLength ++ (resp.body.map (λ b => "\r\n\r\n" ++ b)).getD ""  ++ "\r\n\r\n"

def isNotReturn : Char → Bool := fun c => ¬ (c = '\u000a' ∨ c = '\u000d')

def statusCode : Lean.Parsec StatusCode := do
  let code ← Lean.Json.Parser.natMaybeZero
  ws
  let reason ← many (satisfy isNotWhitespace)
  return {
    code := code
    reason := .mk reason.toList
  }



def header : Lean.Parsec Header := do
  let name ← many (satisfy fun c => ¬ (c = ':' ∨ c = '\u000a' ∨ c = '\u000d')) 
  dbg_trace s!"Before name : {String.mk name.toList}"
  skipChar ':'
  dbg_trace "After :"
  ws
  let value ← many (satisfy isNotReturn)
  skipString "\r\n"
  dbg_trace s!"Header value {String.mk value.toList}"
  return {
    name := .mk name.toList
    value := .mk value.toList
  }


def httpResponse : Lean.Parsec HttpResponse := do
  let d ← (pstring "HTTP/" *> digit *> pchar '.' *> digit)
  ws
  let status ← statusCode
  ws
  dbg_trace s!"Status parsed {status}"
  let headers ← many (attempt header) <* pstring "\r\n"
  let body ← many anyChar <* eof
  pure {
    statusCode := status
    body := some (String.mk body.toList)
  }



def HttpResponse.parse (s : String) : Except ParseError HttpResponse :=
  match httpResponse s.mkIterator with
  | Lean.Parsec.ParseResult.success _ res => Except.ok res
  | Lean.Parsec.ParseResult.error it err  => Except.error s!"offset {repr it.i.byteIdx}: {err}\r\n Remaining: {it.remainingToString}"