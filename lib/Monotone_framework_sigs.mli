(** The API for a monotone framework, as described in 2.3-2.4 of
    Nielson, Nielson, and Hankin or 9.3 of Aho et al..
    This gives a modular way of implementing many static analyses. *)
open Core_kernel

(** The API for a flowgraph, needed for the mfp algorithm
    in the monotone framework.
    Assumed invariants: successors contains all graph nodes as keys
                        initials is a subset of the graph nodes *)
module type FLOWGRAPH = sig
  type labels

  include Base__.Hashtbl_intf.Key with type t = labels

  val initials : labels Set.Poly.t
  val successors : (labels, labels Set.Poly.t) Map.Poly.t
end

(** The minimal data we need to use a type in forming a lattice of various kinds *)
module type TYPE = sig
  type vals
end

(** The data we need to form a powerset lattice *)
module type INITIALTYPE = sig
  type vals

  val initial : vals Set.Poly.t
end

(** The data we need to form e.g. an available xpressions lattice*)
module type TOTALTYPE = sig
  type vals

  val total : vals Set.Poly.t
end

(** The data we need to form a dual powerset lattice *)
module type INITIALTOTALTYPE = sig
  include INITIALTYPE
  include TOTALTYPE with type vals := vals
end

(** The API for a complete (possibly non-distributive) lattice,
    needed for the mfp algorithm in the monotone framework *)
module type LATTICE = sig
  type properties

  val bottom : properties
  val leq : properties -> properties -> bool

  val initial : properties
  (**  An initial value, which might not be the top element.
       The idea is that this is the property that you start with
       (you assume to be true at the start of your analysis). *)

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
    each node l in the flow graph.
    The analysis performed is always a forward analysis. 
    For a reverse analysis, supply the reverse flow graph.*)
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
    -> (T.labels, T.properties) Map.Poly.t
       * (T.labels, T.properties) Map.Poly.t
end
