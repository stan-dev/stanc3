type annotation_classifier =
  string -> Middle.UnsizedType.t -> [`Fine | `Unknown | `WrongType]

val find_unrecognized :
  annotation_classifier -> Ast.untyped_program -> Warnings.t list
