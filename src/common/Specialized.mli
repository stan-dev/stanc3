open Core_kernel



module type S = sig
  type t [@@deriving compare, sexp]

  module Meta : Meta.S
  include Pretty.S with type t := t
  include Comparator.S with type t := t

  include
    Comparable.S
    with type t := t
     and type comparator_witness := comparator_witness
end

module type Parameterized = sig 
  type 'a t [@@deriving compare, sexp]
  include Pretty.S1 with type 'a t := 'a t 
end 


module Make (P : Parameterized) (Meta : Meta.S) :
  S with type t = Meta.t P.t and module Meta := Meta


module type Parameterized2  = sig 
  type ('a,'b) t [@@deriving compare, sexp]
  include Pretty.S2 with type ('a,'b) t := ('a,'b) t 
end 

module Make2 (P :Parameterized2) (First : S) (Meta : Meta.S) :
  S with type t = (First.Meta.t, Meta.t) P.t and module Meta := Meta
