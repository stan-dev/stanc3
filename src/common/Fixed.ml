(** This module defines the signatures and [Make] functors for the 'fixed point'
    (or two-level) type we use for our intermediate representations
*)

open Core_kernel

(** The fixed-point of [Pattern.t] annotated with some meta-data *)
module type S = sig
  module Pattern : Pattern.S

  type 'a t = {pattern: 'a t Pattern.t; meta: 'a}
  [@@deriving compare, map, fold, hash, sexp]

  include Foldable.S with type 'a t := 'a t
  include Pretty.S1 with type 'a t := 'a t

  val fold_pattern : f:('a * 'r Pattern.t -> 'r) -> 'a t -> 'r
  (**
  [fold_pattern] traverses the data structure from the bottom up
  replacing the original meta data of type ['a] with some result type ['r]
  and combines the result values on the way back up. For example, given
  a pattern with the following type:

  [type 'a pattern = Leaf of string | Branch of 'a * 'a]

  we can use [fold_pattern] to calculate the maximum depth of data structure
  obtained by [fixing] the pattern type:

  {[
  let max_depth t =
    let f = function
        | meta , Leaf _ -> 1
        | meta , Branch(left_depth,right_depth) ->
            1 + (max left_depth right_depth)
    in
    fold_pattern ~f t
  ]}

  Note that the function 'f' supplied to [fold_pattern] accepts a tuple
  rather than our type ['a t] since the type of the pattern has been transformed
  to ['r t Pattern.t] and is no longer 'compatible' with the original type of
  meta data ['a]. That is, a tuple [('a * 'b t Pattern.t)] has two type
  variables where as  ['a t] has one.
  *)

  val rewrite_bottom_up : f:('a t -> 'a t) -> 'a t -> 'a t
  (** [rewrite_bottom_up] specializes [fold_pattern] so that the result type
  ['r] is equal to the type of our fixed-point data structure i.e. ['r = 'a t].
  This also means that the function [f] can be written with our fixed-point type
  ['a t] as its argument. *)

  val unfold_pattern : f:('r -> 'a * 'r Pattern.t) -> 'r -> 'a t
  (** [unfold] builds a fixed-point data structure from the top down. Starting
  with a user-supplied seed of type ['r], [unfold] recursively applies the
  function [f] yielding a tuple of meta-data and pattern with elements of
  type ['r] to which [f] is further applied.

  The [unfold] terminates when a pattern element which does not carry an ['r] is
  reached.

  As with [fold_pattern] the function [f] returns a tuple of meta-data and
  pattern rather than our record type since, in general, ['r =/= 'a t]. *)

  val rewrite_top_down : f:('a t -> 'a t) -> 'a t -> 'a t
  (** [rewrite_top_down] specializes [unfold] by requiring that [r = 'a t].
  As a consequence the function [f] accepts our record type ['a t] as its
  argument. *)
end

(** Functor  which creates the fixed-point of the type defined in the [Pattern]
module argument
*)
module Make (Pattern : Pattern.S) : S with module Pattern := Pattern = struct
  type 'a t = {pattern: 'a t Pattern.t; meta: 'a}
  [@@deriving compare, map, fold, hash, sexp]

  let rec pp f ppf {pattern; meta} =
    Fmt.pf ppf {|%a%a|} f meta (Pattern.pp (pp f)) pattern

  include Foldable.Make (struct type nonrec 'a t = 'a t

                                let fold = fold
  end)

  let rec fold_pattern ~f {meta; pattern} =
    let pattern' = Pattern.map (fold_pattern ~f) pattern in
    f (meta, pattern')

  (** For clarity this is written explicitly but is equivalent to
  {[fold_pattern ~f:(Fn.compose f fix) t]}
  *)
  let rec rewrite_bottom_up ~f t =
    let x = {t with pattern= Pattern.map (rewrite_bottom_up ~f) t.pattern} in
    f x

  let rec unfold_pattern ~f x =
    let meta, pattern = f x in
    {meta; pattern= Pattern.map (unfold_pattern ~f) pattern}

  (** For clarity this is written explicitly but it is equivalent to
  {[unfold ~f:(Fn.compose unfix f) x]}
  *)
  let rec rewrite_top_down ~f x =
    let t = f x in
    {t with pattern= Pattern.map (rewrite_top_down ~f) t.pattern}
end

(** Nested fixed-point type where an element of the [Pattern] is itself
a fixed-point type. We use this to represent statements which contain
expressions.
*)
module type S2 = sig
  module First : S
  module Pattern : Pattern.S2

  type ('a, 'b) t = {pattern: ('a First.t, ('a, 'b) t) Pattern.t; meta: 'b}
  [@@deriving compare, map, fold, hash, sexp]

  include Foldable.S2 with type ('a, 'b) t := ('a, 'b) t
  include Pretty.S2 with type ('a, 'b) t := ('a, 'b) t

  val fold_pattern :
       f:('a * 'r1 First.Pattern.t -> 'r1)
    -> g:('b * ('r1, 'r2) Pattern.t -> 'r2)
    -> ('a, 'b) t
    -> 'r2
  (**
  [fold_pattern] traverses the data structure from the bottom up
  replacing the original meta data of [First] with type ['a] with some result
  type ['r1] and the meta data at this level withtype ['b] to another result
  type ['r2] and combines the result values on the way back up.
  *)

  val rewrite_bottom_up :
       f:('a First.t -> 'a First.t)
    -> g:(('a, 'b) t -> ('a, 'b) t)
    -> ('a, 'b) t
    -> ('a, 'b) t
  (** [rewrite_bottom_up] specializes [fold_pattern] so that the result type
  ['r1] is equal to the type of the nested fixed-point type
  i.e. ['r1 = 'a First.t] and the result type ['r2] is equal to the top-level
  fixed-point type i.e. ['r2 = ('a,'b) t].

  This also means that the function [f] can be written with our nested
  fixed-point type  ['a First.t] as its argument and [g] can be written with
  [('a,'b) t] as its argument.
  *)

  val unfold_pattern :
       f:('r1 -> 'a * 'r1 First.Pattern.t)
    -> g:('r2 -> 'b * ('r1, 'r2) Pattern.t)
    -> 'r2
    -> ('a, 'b) t
  (** [unfold_pattern] takes a seed value of type ['r2] and uses the function
  [g] to generate a tuple of meta-data and a pattern with types ['r1] and ['r2].
  The functions proceeds by recursively applying [g] to the contained values of
  type ['r2] and [f] to values of type ['r1] finishing when the pattern contains
  no values of type ['r1] or ['r2].
  *)

  val rewrite_top_down :
       f:('a First.t -> 'a First.t)
    -> g:(('a, 'b) t -> ('a, 'b) t)
    -> ('a, 'b) t
    -> ('a, 'b) t
  (** [rewrite_top_down] specializes [unfold_pattern] in a manner analogous to
  how [rewrite_bottom_up] specializes [fold_pattern] *)
end

module Make2 (First : S) (Pattern : Pattern.S2) :
  S2 with module First := First and module Pattern := Pattern = struct
  type ('a, 'b) t = {pattern: ('a First.t, ('a, 'b) t) Pattern.t; meta: 'b}
  [@@deriving map, fold, compare, hash, sexp]

  let rec pp f g ppf {pattern; meta} =
    Fmt.pf ppf {|%a%a|} g meta (Pattern.pp (First.pp f) (pp f g)) pattern

  include Foldable.Make2 (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let fold = fold
  end)

  let rec fold_pattern ~f ~g {meta; pattern} =
    let pattern' =
      Pattern.map (First.fold_pattern ~f) (fold_pattern ~f ~g) pattern
    in
    g (meta, pattern')

  (** {[fold_pattern (Fn.compose f First.fix) (Fn.compose g fix) x]} *)
  let rec rewrite_bottom_up ~f ~g t =
    g
      { t with
        pattern=
          Pattern.map
            (First.rewrite_bottom_up ~f)
            (rewrite_bottom_up ~f ~g) t.pattern }

  let rec unfold_pattern ~f ~g x =
    let meta, pattern = g x in
    let pattern' =
      Pattern.map (First.unfold_pattern ~f) (unfold_pattern ~f ~g) pattern
    in
    {meta; pattern= pattern'}

  (** {[unfold_pattern (Fn.compose First.unfix f) (Fn.compose unfix g) x]} *)
  let rec rewrite_top_down ~f ~g x =
    let t = g x in
    { t with
      pattern=
        Pattern.map
          (First.rewrite_top_down ~f)
          (rewrite_top_down ~f ~g) t.pattern }
end
