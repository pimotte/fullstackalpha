import Fullstackalpha.Http.Basic

namespace Fullstackalpha.Http

inductive StatusCode
  | OK

def StatusCode.asString : StatusCode -> String
  | .OK => "200 OK"
instance : Repr StatusCode where
  reprPrec := fun c _ => c.asString
  

structure HttpResponse where
  statusCode : StatusCode
  body : Option String

def HttpResponse.render (resp : HttpResponse) : String := 
  let statusLine := "HTTP/1.1 " ++ resp.statusCode.asString
  let contentLength := "Content-length:" ++ (resp.body.map (λ b => Nat.repr b.length)).getD "0" 
  statusLine ++ contentLength ++ (resp.body.map (λ b => "\r\n\r\n" ++ b)).getD ""  ++ "\r\n\r\n"