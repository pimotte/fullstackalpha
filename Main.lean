import Fullstackalpha.Http.Server
import Fullstackalpha.Http.Client

open Fullstackalpha.Http

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
  pure {
    statusCode := Fullstackalpha.Http.StatusCode.fromNat 200
    body := s!"Hello, world from {req.uri}. Result {response.render}"
  }

def conf : Fullstackalpha.Http.Config := 
  {
    port := 8080
    handler := handler
  }

def main : IO Unit := Fullstackalpha.Http.run conf
    