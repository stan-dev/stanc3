open Middle

val trans_prog : Program.Typed.t -> Program.Typed.t
val is_opencl_var : string -> bool
val use_opencl : bool ref

val validate_sized :
     string
  -> 'a
  -> 'b Program.transformation option
  -> Expr.Typed.t SizedType.t
  -> (Expr.Typed.Meta.t, 'a) Stmt.Fixed.t list
