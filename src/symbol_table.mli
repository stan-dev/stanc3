(* Symbol table interface to implement var map *)

module type SYMBOL = sig
  type 'a state
  
  val initialize : ('a -> bool) -> 'a state

  val enter : 'a state-> string -> 'a -> unit

  val look : 'a state -> string -> 'a option

  val begin_scope : 'a state -> unit

  val end_scope : 'a state -> unit
  
  val is_primitive : 'a state -> string -> 'a -> bool
  
end