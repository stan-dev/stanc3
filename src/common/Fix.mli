open Core_kernel

module type S = sig
  module Pattern : Pattern.S

  type 'a t = {pattern: 'a t Pattern.t; meta: 'a}
  [@@deriving compare, map, fold, hash, sexp]

  include Foldable.S with type 'a t := 'a t
  include Pretty.S1 with type 'a t := 'a t

  module Make_traversable (A : Applicative.S) :
    Traversable.S with module A := A and type 'a t := 'a t

  module Make_traversable2 (A : Applicative.S2) :
    Traversable.S2 with module A := A and type 'a t := 'a t

  val pattern : 'a t -> 'a t Pattern.t
  val meta : 'a t -> 'a
  val fix : 'a -> 'a t Pattern.t -> 'a t
  val with_meta : 'a -> 'a t -> 'a t 
  val map_pattern : f:('a -> 'a t Pattern.t -> 'a t ) -> 'a t -> 'a t

  val fold_left_pattern :
    f:('a -> 'b -> 'b t Pattern.t -> 'a) -> init:'a -> 'b t -> 'a

  val fold_right_pattern :
    f:('b -> 'b t Pattern.t -> 'a -> 'a) -> init:'a -> 'b t -> 'a

  val fold_map_pattern :
       (module Monoid.S with type t = 'a)
    -> f:('b -> 'b t Pattern.t -> 'a)
    -> ?init:'a
    -> 'b t
    -> 'a

  val any_pattern :
    pred:('a -> 'a t Pattern.t -> bool) -> ?init:bool -> 'a t -> bool

  val all_pattern :
    pred:('a -> 'a t Pattern.t -> bool) -> ?init:bool -> 'a t -> bool
end

module type S2 = sig
  module First : S
  module Pattern : Pattern.S2

  type ('a, 'b) t = {pattern: ('a First.t, ('a, 'b) t) Pattern.t; meta: 'b}
  [@@deriving compare, map, fold, hash, sexp]

  include Bifoldable.S with type ('a, 'b) t := ('a, 'b) t
  include Pretty.S2 with type ('a, 'b) t := ('a, 'b) t

  module Make_traversable (A : Applicative.S) :
    Bitraversable.S with module A := A and type ('a, 'b) t := ('a, 'b) t

  module Make_traversable2 (A : Applicative.S2) :
    Bitraversable.S2 with module A := A and type ('a, 'b) t := ('a, 'b) t

  val pattern : ('a, 'b) t -> ('a First.t, ('a, 'b) t) Pattern.t
  val meta : ('a, 'b) t -> 'b
  val fix : 'b -> ('a First.t, ('a, 'b) t) Pattern.t -> ('a, 'b) t
  val with_meta : 'b -> ('a,'b) t -> ('a,'b) t
  val map_pattern :
       f:('a -> 'a First.t First.Pattern.t -> 'a First.t )
    -> g:(   'b
          -> ('a First.t, ('a, 'b) t) Pattern.t
          -> ('a, 'b) t)
    -> ('a, 'b) t
    -> ('a, 'b) t

  val fold_left_pattern :
       f:('a -> 'b -> 'b First.t First.Pattern.t -> 'a)
    -> g:('a -> 'c -> ('b First.t, ('b, 'c) t) Pattern.t -> 'a)
    -> init:'a
    -> ('b, 'c) t
    -> 'a

  val fold_right_pattern :
       f:('b -> 'b First.t First.Pattern.t -> 'a -> 'a)
    -> g:('c -> ('b First.t, ('b, 'c) t) Pattern.t -> 'a -> 'a)
    -> init:'a
    -> ('b, 'c) t
    -> 'a

  val fold_map_pattern :
       (module Monoid.S with type t = 'a)
    -> f:('b -> 'b First.t First.Pattern.t -> 'a)
    -> g:('c -> ('b First.t, ('b, 'c) t) Pattern.t -> 'a)
    -> ?init:'a
    -> ('b, 'c) t
    -> 'a

  val any_pattern :
       pred_first:('a -> 'a First.t First.Pattern.t -> bool)
    -> pred_second:('b -> ('a First.t, ('a, 'b) t) Pattern.t -> bool)
    -> ?init:bool
    -> ('a, 'b) t
    -> bool

  val all_pattern :
       pred_first:('a -> 'a First.t First.Pattern.t -> bool)
    -> pred_second:('b -> ('a First.t, ('a, 'b) t) Pattern.t -> bool)
    -> ?init:bool
    -> ('a, 'b) t
    -> bool
end

module Make (Pattern : Pattern.S) : S with module Pattern := Pattern

module Make2 (First : S) (Pattern : Pattern.S2) :
  S2 with module First := First and module Pattern := Pattern
