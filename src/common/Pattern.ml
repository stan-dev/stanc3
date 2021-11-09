(** A [Pattern] defines the signature of modules that may be fixed with
[Fixed.Make] and [Fixed.Make2].

These signatures ensure that all the operations we want to support on our
top level intermediate representations can be defined by the [Fixed.Make]
functors.
*)

module type S = sig
  type 'a t [@@deriving compare, fold, hash, map, sexp]

  include Foldable.S with type 'a t := 'a t
  include Pretty.S1 with type 'a t := 'a t
end

module type S2 = sig
  type ('a, 'b) t [@@deriving compare, fold, hash, map, sexp]

  include Foldable.S2 with type ('a, 'b) t := ('a, 'b) t
  include Pretty.S2 with type ('a, 'b) t := ('a, 'b) t
end
