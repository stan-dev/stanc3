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
  val map_pattern : f:('a -> 'a t Pattern.t -> 'a t Pattern.t) -> 'a t -> 'a t

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

module Make (Pattern : Pattern.S) : S with module Pattern := Pattern = struct
  

  module Basic = struct
    type 'a t = {pattern: 'a t Pattern.t; meta: 'a}
    [@@deriving compare, hash, sexp]

    let rec map f {pattern; meta} =
      {pattern= Pattern.map (map f) pattern; meta= f meta}

    let rec fold f init {pattern; meta} =
      Pattern.fold_left ~f:(fold f) ~init:(f init meta) pattern
  end

  include Basic
  include Foldable.Make (Basic)

  let pattern {pattern; _} = pattern
  let meta {meta; _} = meta
  let fix meta pattern = {pattern; meta}

  let with_meta meta {pattern;_} = {pattern;meta}

  let rec pp f ppf {pattern; meta} =
    Fmt.pf ppf {|%a%a|} f meta (Pattern.pp (pp f)) pattern

  module Make_traversable (A : Applicative.S) :
    Traversable.S with module A := A and type 'a t := 'a t = struct
    module TraversablePattern = Pattern.Make_traversable (A)

    let rec traverse {pattern; meta} ~f =
      A.map2 ~f:fix (f meta)
        (TraversablePattern.traverse ~f:(traverse ~f) pattern)
  end

  module Make_traversable2 (A : Applicative.S2) :
    Traversable.S2 with module A := A and type 'a t := 'a t = struct
    module TraversablePattern = Pattern.Make_traversable2 (A)

    let rec traverse {pattern; meta} ~f =
      A.map2 ~f:fix (f meta)
        (TraversablePattern.traverse ~f:(traverse ~f) pattern)
  end

  let rec map_pattern ~f {pattern; meta} =
    {pattern= f meta @@ Pattern.map (map_pattern ~f) pattern; meta}

  let rec fold_left_pattern ~f ~init {pattern; meta} =
    Pattern.fold_left
      ~f:(fun accu x -> fold_left_pattern ~f ~init:accu x)
      ~init:(f init meta pattern) pattern

  let rec fold_right_pattern ~f ~init {pattern; meta} =
    f meta pattern
    @@ Pattern.fold_right
         ~f:(fun x accu -> fold_right_pattern ~f ~init:accu x)
         ~init pattern

  let fold_map_pattern (type a) (module M : Monoid.S with type t = a) ~f
      ?init:(empty = M.empty) x =
    fold_right_pattern
      ~f:(fun meta pattern accu -> M.combine accu @@ f meta pattern)
      ~init:empty x

  let any_pattern ~pred ?init x =
    fold_map_pattern (module Monoid.Bool_or) ~f:pred ?init x

  let all_pattern ~pred ?init x =
    fold_map_pattern (module Monoid.Bool_and) ~f:pred ?init x
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
       f:('a -> 'a First.t First.Pattern.t -> 'a First.t First.Pattern.t)
    -> g:(   'b
          -> ('a First.t, ('a, 'b) t) Pattern.t
          -> ('a First.t, ('a, 'b) t) Pattern.t)
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

module Make2 (First : S) (Pattern : Pattern.S2) :
  S2 with module First := First and module Pattern := Pattern = struct
  
  

  type ('a, 'b) t = {pattern: ('a First.t, ('a, 'b) t) Pattern.t; meta: 'b}
  [@@deriving compare, hash, sexp]

  let rec map f g {pattern; meta} =
    {pattern= Pattern.map (First.map f) (map f g) pattern; meta= g meta}

  let rec fold f g init {pattern; meta} =
    Pattern.fold_left
      ~f:(fun accu x -> First.fold_left ~f ~init:accu x)
      ~g:(fold f g) ~init:(g init meta) pattern

  include Bifoldable.Make (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let fold = fold
  end)

  let rec pp f g ppf {pattern; meta} =
    Fmt.pf ppf {|%a%a|} g meta (Pattern.pp (First.pp f) (pp f g)) pattern

  let pattern {pattern; _} = pattern
  let meta {meta; _} = meta
  let fix meta pattern = {pattern; meta}

  let with_meta meta {pattern;_} = {pattern;meta}
  
  module Make_traversable (A : Applicative.S) :
    Bitraversable.S with type ('a, 'b) t := ('a, 'b) t and module A := A =
  struct
    module Traversable_pattern = Pattern.Make_traversable (A)
    module Traversable_first = First.Make_traversable (A)

    let rec traverse {pattern; meta} ~f ~g =
      A.map2 ~f:fix (g meta)
        (Traversable_pattern.traverse
           ~f:(Traversable_first.traverse ~f)
           ~g:(traverse ~f ~g) pattern)
  end

  module Make_traversable2 (A : Applicative.S2) :
    Bitraversable.S2 with type ('a, 'b) t := ('a, 'b) t and module A := A =
  struct
    module Traversable_pattern = Pattern.Make_traversable2 (A)
    module Traversable_first = First.Make_traversable2 (A)

    let rec traverse {pattern; meta} ~f ~g =
      A.map2 ~f:fix (g meta)
        (Traversable_pattern.traverse
           ~f:(Traversable_first.traverse ~f)
           ~g:(traverse ~f ~g) pattern)
  end

  let rec map_pattern ~f ~g {pattern; meta} =
    { pattern=
        g meta
        @@ Pattern.map (First.map_pattern ~f) (map_pattern ~f ~g) pattern
    ; meta }

  let rec fold_left_pattern ~f ~g ~init {pattern; meta} =
    Pattern.fold_left
      ~f:(fun accu x -> First.fold_left_pattern ~f ~init:accu x)
      ~g:(fun accu x -> fold_left_pattern ~f ~g ~init:accu x)
      ~init:(g init meta pattern) pattern

  let rec fold_right_pattern ~f ~g ~init {pattern; meta} =
    Pattern.fold_right
      ~f:(fun x accu -> First.fold_right_pattern ~f ~init:accu x)
      ~g:(fun x accu -> fold_right_pattern ~f ~g ~init:accu x)
      ~init pattern
    |> g meta pattern

  let fold_map_pattern (type a) (module M : Monoid.S with type t = a) ~f ~g
      ?init:(empty = M.empty) x =
    fold_right_pattern
      ~f:(fun meta_first pattern_first accu ->
        M.combine accu @@ f meta_first pattern_first )
      ~g:(fun meta pattern accu -> M.combine accu @@ g meta pattern)
      ~init:empty x

  let any_pattern ~pred_first ~pred_second ?init x =
    fold_map_pattern
      (module Monoid.Bool_or)
      ~f:pred_first ~g:pred_second ?init x

  let all_pattern ~pred_first ~pred_second ?init x =
    fold_map_pattern
      (module Monoid.Bool_and)
      ~f:pred_first ~g:pred_second ?init x
end
