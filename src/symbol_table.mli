(* Symbol table interface to implement var map *)

module type SYMBOL =
  sig 
  type 'a table
  val initialize : unit -> 'a table
  val enter : 'a table -> string -> 'a -> unit  
  val look : 'a table -> string -> 'a option
  val begin_scope : 'a table -> unit
  val end_scope : 'a table -> unit

end
