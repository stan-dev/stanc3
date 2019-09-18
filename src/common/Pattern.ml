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
