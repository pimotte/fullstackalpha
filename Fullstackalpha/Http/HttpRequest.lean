import Socket

import Lean.Data.Parsec
import Fullstackalpha.Http.Basic

namespace Fullstackalpha.Http

structure HttpRequest where
  method : Method
  uri : String
  headers : List Header

open Lean.Parsec



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
    headers := []
  }

def parse (s : String) : Except ParseError HttpRequest :=
  match httpRequest s.mkIterator with
  | Lean.Parsec.ParseResult.success _ res => Except.ok res
  | Lean.Parsec.ParseResult.error it err  => Except.error s!"offset {repr it.i.byteIdx}: {err}"

def receiveRequest (socket : Socket.Socket) : ExceptT ParseError IO HttpRequest := do
  let request ← socket.recv 8192
  let decoded := String.fromUTF8Unchecked request
  parse decoded

def HttpRequest.render (req : HttpRequest) : String := 
  let requestLine := s!"{req.method.asString} {req.uri} HTTP/1.1\r\n"
  let headers := req.headers.map (fun h => s!"{h.name}: {h.value}\r\n")
  requestLine ++ headers.foldr (. ++ .) "" ++ "\r\n"