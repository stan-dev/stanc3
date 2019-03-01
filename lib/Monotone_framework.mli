(** The API for a monotone framework *)
open Core_kernel

module type FLOWGRAPH = sig
  type labels

  val extremals : unit -> labels Set.Poly.t
  val predecessors : labels -> labels Set.Poly.t
  val sucessors : labels -> labels Set.Poly.t
end

module type LATTICE = sig
  type properties

  val bottom : properties
  val equal : properties -> properties -> bool

  val extremal : properties
  (**  An extremal value, which might not be the top element *)

  val lub : properties -> properties -> properties
end

module type TRANSFER_FUNCTION = functor (F : FLOWGRAPH) (L : LATTICE) -> sig
  val transfer_function : F.labels -> L.properties -> L.properties
end

module type MONOTONE_FRAMEWORK = functor
  (F : FLOWGRAPH)
  (L : LATTICE)
  (T : TRANSFER_FUNCTION)
  -> sig
  val mfp : unit -> F.labels
end
