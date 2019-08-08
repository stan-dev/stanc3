type 'a entry_exit

val entry : 'a entry_exit -> 'a 
val exit : 'a entry_exit -> 'a 


module type S = sig
  type t
  type property

  module Label : Label.S
  module F : Flowgraph.S with type t := t and module Label := Label
  module L : Lattice.S with type t := t and type property := property

  module TF :
    Transfer_function.S with type t := t and type property := property

  type flowgraph_info =
    { flowgraph: (Label.t * Label.t) list
    ; initials: Label.Set.t
    ; associations: t Label.Map.t }

  val solve :
    flowgraph_info -> t -> t Label.Map.t * property entry_exit Label.Map.t

  val solve_t : t -> t Label.Map.t * property entry_exit Label.Map.t
end

module Make
    (F : Flowgraph.S)
    (L : Lattice.S with type t := F.t)
    (TF : Transfer_function.S
          with type t := F.t
           and type property := L.property) :
  S
  with type t := F.t
   and module Label := F.Label
   and module F := F
   and type property := L.property
   and module L := L
   and module TF := TF