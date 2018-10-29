(* Symbol table to implement var map *)

module type SYMBOL =
  sig 
  type 'a table
  val initialize : unit -> 'a table
  val enter : 'a table * string * 'a -> unit  
  val look : 'a table * string -> 'a option(*
  val begin_scope : 'a table -> unit
  val end_scope : 'a table -> unit *)

end

module Symbol : SYMBOL =
struct 
  type 'a table = ((string, 'a) Hashtbl.t) * ((string list) ref)
  let initialize _ = (Hashtbl.create 123456, ref []) 
  let enter (tab, str, ty) =  Hashtbl.add (fst tab) str ty ; (snd tab) := str :: !(snd tab)
  let look (tab, str) = Hashtbl.find_opt (fst tab) str
(*
  val begin_scope : a table -> unit
  val end_scope : a table -> unit *)
end