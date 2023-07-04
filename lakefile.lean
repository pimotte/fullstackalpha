import Lake
open Lake DSL

require Socket from git "https://github.com/xubaiw/lean4-socket.git"

package «fullstackalpha» {
  -- add package configuration options here
}

lean_lib «Fullstackalpha» {
  -- add library configuration options here
}

@[default_target]
lean_exe «fullstackalpha» {
  root := `Main
}
