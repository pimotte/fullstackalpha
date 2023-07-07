import Socket

import Lean.Data.Parsec

namespace Fullstackalpha.Http.HttpRequest

inductive Method 
 | GET 

structure HttpRequest where
  method : Method
  uri : String

abbrev ParseError := String

open Lean.Parsec

def isNotWhitespace : Char → Bool := fun c => ¬ (c = '\u0009' ∨ c = '\u000a' ∨ c = '\u000d' ∨ c = '\u0020')

def method : Lean.Parsec Method := do
  let res ← pstring "GET"
  ws
  return Method.GET

def httpRequest : Lean.Parsec HttpRequest := do
  let meth ← method
  let uri ← many (satisfy isNotWhitespace)
  pure {
    method := meth
    uri := String.mk (uri.toList)
  }

def parse (s : String) : Except ParseError HttpRequest :=
  match httpRequest s.mkIterator with
  | Lean.Parsec.ParseResult.success _ res => Except.ok res
  | Lean.Parsec.ParseResult.error it err  => Except.error s!"offset {repr it.i.byteIdx}: {err}"

def receiveRequest (socket : Socket.Socket) : ExceptT ParseError IO HttpRequest := do
  let request ← socket.recv 8192
  let decoded := String.fromUTF8Unchecked request
  parse decoded
