open Core_kernel

module type S = sig
  module Pattern : Pattern.S

  (** The fixed-point of `Pattern.t` *)
  type 'a t = {meta: 'a; pattern: 'a t Pattern.t}
  [@@deriving compare, map, fold, hash, sexp]

  val fix : 'a * 'a t Pattern.t -> 'a t
  val unfix : 'a t -> 'a * 'a t Pattern.t
  val pattern : 'a t -> 'a t Pattern.t
  val meta : 'a t -> 'a

  include Foldable.S with type 'a t := 'a t
  include Pretty.S1 with type 'a t := 'a t

  val fold_pattern : f:('a * 'r Pattern.t -> 'r) -> 'a t -> 'r
  (** 
  `fold_pattern` traverses the data structure from the bottom up
  replacing the original meta data of type `'a` with some result type `'r`
  and combines the result values on the way back up. For example, given
  a pattern with the following type:

  `type 'a pattern = Leaf of string | Branch of 'a * 'a`

  we can use `fold_pattern` to calculate the maximum depth of fixed type:

  ```
  let max_depth t = 
    let f = function 
        | meta , Leaf _ -> 1 
        | meta , Branch(left_depth,right_depth) -> 1 + (max left_depth right_depth)
    in
    fold_pattern ~f t
  ```

  Note that the function 'f' supplied to `fold_pattern` accepts a tuple 
  rather than our type `'a t` since the type of the pattern has been transformed
  to `'r t Pattern.t` and is no longer 'compatible' with the original type of 
  meta data `'a`.
  *)

  val rewrite_bottom_up : f:('a t -> 'a t) -> 'a t -> 'a t
  (** `rewrite_bottom_up` specializes `fold_pattern` so that the result type 
  `'r` is equal to the type of our fixed-point data structure i.e. `'r = 'a t`. 
  This also means that the function `f` can be written with our fixed-point type 
  `'a t` as its arguement. *)

  val fold_pattern_ctxt : f:('a t -> 'a * 'r Pattern.t -> 'r) -> 'a t -> 'r
  (** `fold_pattern_ctxt` generalized `fold_pattern` so that the function `f`
  receives the 'parent' (or context) of the element of the data structure it
  is currently operating on. *)

  val rewrite_ctxt : f:('a t -> 'a t -> 'a t) -> 'a t -> 'a t
  (** `rewrite_ctxt` specializes `fold_pattern_ctxt` so that the result type 
  `'r` is equal to the type of our fixed-point data structure. *)

  val unfold : f:('r -> 'a * 'r Pattern.t) -> 'r -> 'a t
  (** `unfold` builds a fixed-point data structure from the top down. Starting
  with a user-supplied seed of type `'r`, `unfold` recursively applies the 
  function `f` yielding a tuple of meta-data and pattern with elements of 
  type `'r` to which `f` is further applied. 
  
  The `unfold` terminates when a pattern element which does not carry an `'r` is 
  reached.

  As with `fold_pattern` the function `f` returns a tuple of meta-data and 
  pattern rather than our record type since, in general, `'r =/= 'a t`. *)

  val rewrite_top_down : f:('a t -> 'a t) -> 'a t -> 'a t
  (** `rewrite_top_down` specializes `unfold` by requiring that `'r = 'a t`. 
  As a consequence the function `f` accepts our record type `'a t` as its 
  argument. *)

  val unfold_partial :
    f:('r -> 'a * ('a t, 'r) Either.t Pattern.t) -> 'r -> 'a t
  (** `unfold_partial` generalizes `unfold` by allowing the `unfold` to 
  terminate at any point, rather than only when a pattern element without
  a type `'r` is reached.

  This early termination is encoded with `Either.t`; comparing the functions `f`
  appearing in `unfold` and `unfold_partial`:

  ```
  'r -> 'a *        'r           Pattern.t 
  'r -> 'a * ('a t, 'r) Either.t Pattern.t  
  ```
  
  In the second case (`unfold_partial`) the function returns a tuple of 
  meta-data and _either_ a fixed-point value `'a t` to which no further 
  unfolding is applied _or_ a seed value `'r` to which `unfold_partial` is 
  recursively applied. *)

  val rewrite_partial : f:('a t -> ('a t, 'a t) Either.t) -> 'a t -> 'a t
  (** `rewrite_partial` specializes `unfold_partial` by requiring that 
  `'r = 'a t`. *)
end

module Make (Pattern : Pattern.S) : S with module Pattern := Pattern = struct
  type 'a t = {meta: 'a; pattern: 'a t Pattern.t}
  [@@deriving compare, map, fold, hash, sexp]

  let fix (meta, pattern) = {meta; pattern}
  let unfix {meta; pattern} = (meta, pattern)
  let pattern {pattern; _} = pattern
  let meta {meta; _} = meta

  let rec pp f ppf {pattern; meta} =
    Fmt.pf ppf {|%a%a|} f meta (Pattern.pp (pp f)) pattern

  include Foldable.Make (struct type nonrec 'a t = 'a t

                                let fold = fold
  end)

  let rec fold_pattern ~f x =
    let meta, pattern = unfix x in
    let pattern' = Pattern.map (fold_pattern ~f) pattern in
    f (meta, pattern')

  let rewrite_bottom_up ~f x = fold_pattern ~f:(Fn.compose f fix) x

  let rec fold_pattern_ctxt ~f t =
    let meta, pattern = unfix t in
    let pattern' = Pattern.map (fold_pattern_ctxt ~f) pattern in
    f t (meta, pattern')

  let rewrite_ctxt ~f t =
    fold_pattern_ctxt ~f:(fun ctxt proj -> f ctxt @@ fix proj) t

  let rec unfold ~f x =
    let meta, pattern = f x in
    let pattern' = Pattern.map (unfold ~f) pattern in
    fix (meta, pattern')

  let rewrite_top_down ~f x = unfold ~f:(Fn.compose unfix f) x

  let rec unfold_partial ~f x =
    let meta, pattern = f x in
    let g x = Either.value_map ~first:Fn.id ~second:(unfold_partial ~f) x in
    let pattern' = Pattern.map g pattern in
    fix (meta, pattern')

  let rewrite_partial ~f x =
    let align t =
      let meta, pattern = unfix t in
      let pattern' = Pattern.map Either.first pattern in
      (meta, pattern')
    in
    let sequence t = Either.value_map ~first:align ~second:align t in
    unfold_partial ~f:(Fn.compose sequence f) x
end

module type S2 = sig
  module First : S
  module Pattern : Pattern.S2

  type ('a, 'b) t [@@deriving compare, map, fold, hash, sexp]

  include Foldable.S2 with type ('a, 'b) t := ('a, 'b) t
  include Pretty.S2 with type ('a, 'b) t := ('a, 'b) t

  type ('a, 'b, 'r) algebra = 'b * ('a, 'r) Pattern.t -> 'r

  val fold_pattern :
       f:('a * 'r1 First.Pattern.t -> 'r1)
    -> g:('b * ('r1, 'r2) Pattern.t -> 'r2)
    -> ('a, 'b) t
    -> 'r2

  val transform_bottom_up :
       ('a First.t -> 'a First.t)
    -> (('a, 'b) t -> ('a, 'b) t)
    -> ('a, 'b) t
    -> ('a, 'b) t
  (* type ('a, 'b, 'r1, 'r2) r_algebra =
    ('a, 'b) t -> 'b * ('r1, 'r2) Pattern.t -> 'r2

  val para :
       ('a, 'r1) First.r_algebra
    -> ('a, 'b, 'r1, 'r2) r_algebra
    -> ('a, 'b) t
    -> 'r2

  val transform_with_context :
       ('a First.t -> 'a First.t -> 'a First.t)
    -> (('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t)
    -> ('a, 'b) t
    -> ('a, 'b) t *)
end
