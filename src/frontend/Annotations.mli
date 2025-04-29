val find_unrecognized :
     (string -> Middle.UnsizedType.t -> [`Fine | `Unknown | `WrongType])
  -> Ast.untyped_program
  -> Warnings.t list
