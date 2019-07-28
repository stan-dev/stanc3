open Core_kernel


module type Parameterized  = sig 
  type 'a t [@@deriving compare, sexp]
  include Pretty.S1 with type 'a t := 'a t 
end 

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

module Make (P : Parameterized) (Meta : Meta.S) :
  S with type t = Meta.t P.t and module Meta := Meta = struct

  module Basic = struct
    
    type t = Meta.t P.t

    let pp ppf x = P.pp Meta.pp ppf x
    let compare x y = P.compare Meta.compare x y
    let sexp_of_t x = P.sexp_of_t Meta.sexp_of_t x
    let t_of_sexp x = P.t_of_sexp Meta.t_of_sexp x

    include Comparator.Make (struct
      type nonrec t = t

      let compare = compare
      let sexp_of_t = sexp_of_t
    end)
  end

  include Basic
  include Comparable.Make_using_comparator (Basic)
end


module type Parameterized2  = sig 
  type ('a,'b) t [@@deriving compare, sexp]
  include Pretty.S2 with type ('a,'b) t := ('a,'b) t 
end 

module Make2 (P: Parameterized2) (First : S) (Meta : Meta.S) :
  S with type t = (First.Meta.t, Meta.t) P.t and module Meta := Meta =
struct

  module Basic = struct
    
    type t = (First.Meta.t, Meta.t) P.t

    let pp ppf x = P.pp First.Meta.pp Meta.pp ppf x
    let compare x y = P.compare First.Meta.compare Meta.compare x y
    let sexp_of_t x = P.sexp_of_t First.Meta.sexp_of_t Meta.sexp_of_t x
    let t_of_sexp x = P.t_of_sexp First.Meta.t_of_sexp Meta.t_of_sexp x

    include Comparator.Make (struct
      type nonrec t = t

      let compare = compare
      let sexp_of_t = sexp_of_t
    end)
  end

  include Basic
  include Comparable.Make_using_comparator (Basic)
end
