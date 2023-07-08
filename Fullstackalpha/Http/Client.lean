
import Socket

import Fullstackalpha.Http.HttpRequest
import Fullstackalpha.Http.HttpResponse

open Socket

namespace Fullstackalpha.Http

structure ClientConfig where
  host : String
  port : Port

def perform (conf : ClientConfig) (req : HttpRequest) : IO String := do
  -- configure remote SockAddr
  let remoteAddr ← SockAddr.mk conf.host conf.port.asString AddressFamily.inet SockType.stream
  IO.println s!"Remote Addr: {remoteAddr}"

  -- connect to remote
  let socket ← Socket.mk AddressFamily.inet SockType.stream
  socket.connect remoteAddr
  IO.println "Connected!"

  -- send HTTP request
  let strSend := req.render
  let bytesSend ← socket.send strSend.toUTF8
  IO.println s!"Send {bytesSend} bytes!\n"

  -- get HTTP response and return it
  let bytesRecv ← socket.recv 8192
  return String.fromUTF8Unchecked bytesRecv