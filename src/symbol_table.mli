(* Symbol table interface to implement var map *)

module type SYMBOL = sig
  type 'a state
  
  val initialize : unit -> 'a state

  val enter : 'a state-> string -> 'a -> unit

  val look : 'a state -> string -> 'a option

  val begin_scope : 'a state -> unit

  val end_scope : 'a state -> unit
  
end