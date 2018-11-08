(* Symbol table interface to implement var map *)

module type SYMBOL = sig
  type 'a state
  
  val initialize : unit -> 'a state

  val enter : 'a state-> string -> 'a -> unit

  val look : 'a state -> string -> 'a option

  val begin_scope : 'a state -> unit

  val end_scope : 'a state -> unit
  
  val set_read_only : 'a state -> string -> unit
  
  val get_read_only : 'a state -> string -> bool

  (* TODO: the following is very ugly, but we seem to need something like it to
   reproduce the (strange) behaviour in the current Stan that local variables
   have a block level that is determined by what has been assigned to them
   rather than by where they were declared. I'm not sure that behaviour makes
   sense unless we use static analysis as well to make sure these assignments
   actually get evaluated in that phase. *)
  val set_global : 'a state -> string -> unit
  
  val get_global : 'a state -> string -> bool
  
  val unsafe_remove : 'a state -> string -> unit
  
  val unsafe_add :  'a state-> string -> 'a -> unit
  
end