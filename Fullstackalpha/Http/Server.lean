import Socket

import Fullstackalpha.Http.HttpRequest
import Fullstackalpha.Http.HttpResponse

import Lean.Data.Json.FromToJson


namespace Fullstackalpha.Http



open Socket


structure Config where
  handler : HttpRequest → HttpResponse
  port : Nat

def testHandler : HttpRequest → String := fun _ => "test"

-- #check (⟨ [⟨ "blah", testHandler ⟩]⟩ : Config)

def run (conf : Config) : IO Unit := do
  let port := String.mk (Nat.toDigits 10 conf.port)
  -- configure local SockAddr
  let localAddr ← SockAddr.mk "localhost" port AddressFamily.inet SockType.stream
  IO.println s!"Local Addr: {localAddr}"

  -- bind a socket to local address
  let socket ← Socket.mk AddressFamily.inet SockType.stream
  socket.bind localAddr
  IO.println "Socket Bound."

  -- listen to HTTP requests
  socket.listen 5
  IO.println s!"Listening at http://localhost:{port}."

  -- serving
  repeat do
    let (remoteAddr, socket') ← socket.accept
    let t ← IO.asTask do
      let requestExcept ← receiveRequest socket'
      match requestExcept with
      | .error e => IO.println s!"Error: {e}"
      | .ok request =>
        IO.println s!"URI: {request.uri}"
        let result := conf.handler request
  
        let strSend := result.render
        let bytesSend ← socket'.send strSend.toUTF8
        socket'.close


    IO.println s!"Incoming: {remoteAddr}"
    