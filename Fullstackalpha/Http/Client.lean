import Socket

import Fullstackalpha.Http.HttpRequest
import Fullstackalpha.Http.HttpResponse

open Socket

namespace Fullstackalpha.Http

structure ClientConfig where
  host : String
  port : Port

def perform (conf : ClientConfig) (req : HttpRequest) : IO HttpResponse := do
  -- configure remote SockAddr
  IO.println s!"Before new sock"
  let remoteAddr ← SockAddr.mk conf.host conf.port.asString AddressFamily.inet SockType.stream
  IO.println s!"Remote Addr: {remoteAddr}"

  -- connect to remote
  let socket ← Socket.mk AddressFamily.inet SockType.stream
  socket.connect remoteAddr
  IO.println "Connected!"

  -- send HTTP request
  let strSend := req.render
  IO.println s!"Raw request: {strSend}"
  let bytesSend ← socket.send strSend.toUTF8
  IO.println s!"Sent {bytesSend} bytes!\n"

  -- get HTTP response and return it
  let bytesRecv ← socket.recv 8192
  IO.println s!"Post Recv!\n"
  let rawResponse := String.fromUTF8Unchecked bytesRecv
  IO.println s!"Parsed response, raw: {rawResponse}"
  match Fullstackalpha.Http.HttpResponse.parse rawResponse with
  | .ok resp => pure resp
  | .error e => pure {
    statusCode := .fromNat 500,
    body := some e
  }