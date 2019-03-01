(** The API for a monotone framework *)
open Core_kernel

module type FLOWGRAPH = sig
  type labels

  include Base__.Hashtbl_intf.Key with type t = labels

  val initials : labels Set.Poly.t
  val list_nodes : unit -> labels List.t
  val list_edges : unit -> (labels * labels) List.t
  val predecessors : labels -> labels Set.Poly.t
  val sucessors : labels -> labels Set.Poly.t
end

module type LATTICE = sig
  type properties

  val bottom : properties
  val leq : properties -> properties -> bool

  val extremal : properties
  (**  An extremal value, which might not be the top element *)

  val lub : properties -> properties -> properties
end

module type TRANSFER_FUNCTION = sig
  type labels
  type properties

  val transfer_function : labels -> properties -> properties
end

module type MONOTONE_FRAMEWORK = functor
  (F : FLOWGRAPH)
  (L : LATTICE)
  (T :
     TRANSFER_FUNCTION
     with type labels = F.labels
      and type properties = L.properties)
  -> sig
  val mfp :
       unit
    -> (T.labels, T.properties) Hashtbl.t * (T.labels, T.properties) Hashtbl.t
end
