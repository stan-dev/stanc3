(** The API for a monotone framework, as described in Nielson, Nielson, and Hankin.
    This gives a modular way of implementing many static analyses. *)
open Core_kernel

(** The API for a data flowgraph, needed for the mfp algorithm
    in the monotone framework *)
module type FLOWGRAPH = sig
  type labels

  include Base__.Hashtbl_intf.Key with type t = labels

  val initials : labels Set.Poly.t
  val nodes : labels Set.Poly.t
  val edges : (labels * labels) Set.Poly.t
  val sucessors : labels -> labels Set.Poly.t
end

(** The minimal data we need to use a type in forming a lattice of various kinds *)
module type PREFLATSET = sig
  type vals

  val ( = ) : vals -> vals -> bool
end

(** The data we need to form a powerset lattice *)
module type PREPOWERSET = sig
  type vals

  val extreme : vals Set.Poly.t
end

(** The API for a complete (possibly non-distributive) lattice,
    needed for the mfp algorithm in the monotone framework *)
module type LATTICE = sig
  type properties

  val bottom : properties
  val leq : properties -> properties -> bool

  val extreme : properties
  (**  An extremal value, which might not be the top element *)

  val lub : properties -> properties -> properties
end

(** The API for a transfer function, needed for the mfp algorithm
    in the monotone framework.
    This describes how output properties are computed from input
    properties at a given node in the flow graph. *)
module type TRANSFER_FUNCTION = sig
  type labels
  type properties

  val transfer_function : labels -> properties -> properties
end

(** The API for a monotone framework. mfp computes the minimal fixed
    point of the equations/inequalities defined between property lattice
    elements at the entry and exit of different flowgraph nodes, where these
    equations/inequalities are generated from the transfer function.
    Returns a hash table of the (input_properties, output_properties) for
    each node l in the flow graph. *)
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
