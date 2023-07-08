import Fullstackalpha.Http.Server

/--
  Entry
-/
def conf : Fullstackalpha.Http.Config := 
  {
    port := 8080
    handler := fun req => {
      statusCode := .OK
      body := s!"Hello, world from {req.uri}"
    }
  }

def main : IO Unit := Fullstackalpha.Http.run conf
    