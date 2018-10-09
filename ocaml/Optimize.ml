(* Let's do a simple CSE pass,
ideally expressed as a visitor with a separate visit() function? *)

let fnapp2sym = Hashtbl.create (module (String, expr list))

let cse = function
  | FnApp (fname, args) ->
