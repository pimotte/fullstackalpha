import Fullstackalpha.Http.Server
import Fullstackalpha.Http.Client

import Lean.Data.Json.FromToJson
import Lean.Data.Json.Parser

open Fullstackalpha.Http

structure Post where
  userId : Nat
  id : Nat
  title : String
  body : String
deriving Lean.FromJson

/--
  Entry
-/
def handler : HttpRequest → IO HttpResponse := fun req => do
  let config : ClientConfig := {
    host := "jsonplaceholder.typicode.com",
    port := 80
  }
  let request : HttpRequest := {
    method := .GET
    uri := "http://jsonplaceholder.typicode.com/posts/1"
    headers := [{
      name := "Host",
      value := config.host
    }]
  }
  IO.println "Before request in Main.handler"
  let response ← perform config request
  IO.println "After request in Main.handler"

  match Lean.Json.parse (response.body.getD "{}") with
  | .error e => pure {
    statusCode := Fullstackalpha.Http.StatusCode.fromNat 200
    body := s!"Parse failed {e}"
  }
  | .ok json =>
    match (Lean.fromJson? json : Except String Post) with
    | .error e2 => pure {
        statusCode := Fullstackalpha.Http.StatusCode.fromNat 200
        body := s!"Parse failed {e2}"
      }
    | .ok post =>
    pure {
      statusCode := Fullstackalpha.Http.StatusCode.fromNat 200
      body := s!"Hello, world from {req.uri}. Title: {post.title}"
    }

def conf : Fullstackalpha.Http.Config := 
  {
    port := 8080
    handler := handler
  }

def main : IO Unit := Fullstackalpha.Http.run conf
    