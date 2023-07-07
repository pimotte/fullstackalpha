import Socket

import Fullstackalpha.Http.HttpRequest

import Lean.Data.Json.FromToJson


namespace Fullstackalpha.Http.Server

open Fullstackalpha.Http.HttpRequest

open Socket

structure Handler (α : Type) where
  uri : String
  handler [Lean.ToJson α] : HttpRequest → α

structure Config where
  handlers : {α : Type} → String → Handler α

def testHandler : HttpRequest → String := fun _ => "test"

-- #check (⟨ [⟨ "blah", testHandler ⟩]⟩ : Config)

def run : IO Unit := do
  -- configure local SockAddr
  let localAddr ← SockAddr.mk "localhost" "8080" AddressFamily.inet SockType.stream
  IO.println s!"Local Addr: {localAddr}"

  -- bind a socket to local address
  let socket ← Socket.mk AddressFamily.inet SockType.stream
  socket.bind localAddr
  IO.println "Socket Bound."

  -- listen to HTTP requests
  socket.listen 5
  IO.println s!"Listening at http://localhost:8080."

  -- serving
  repeat do
    let (remoteAddr, socket') ← socket.accept
    let t ← IO.asTask do
      let requestExcept ← receiveRequest socket'
      match requestExcept with
      | .error e => IO.println s!"Error: {e}"
      | .ok request =>
        IO.println s!"URI: {request.uri}"
        let strSend := 
          "HTTP/1.1 200 OK" ++
          "Content-Length:5" ++
          "\r\n\r\n" ++
          "Hello" ++
          "\r\n\r\n"
        let bytesSend ← socket'.send strSend.toUTF8
        socket'.close


    IO.println s!"Incoming: {remoteAddr}"
    