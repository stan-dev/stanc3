module Fn = Core_kernel.Fn
open Helpers

module type Basic = sig
  module Pattern : Functor.S

  type t

  val inj : t Pattern.t -> t
end

module type S = sig
  include Basic

  val inj_succ : (t Pattern.t -> t) -> t Pattern.t Pattern.t -> t
  val inj2 : t Pattern.t Pattern.t -> t
  val inj3 : t Pattern.t Pattern.t Pattern.t -> t
  val inj4 : t Pattern.t Pattern.t Pattern.t Pattern.t -> t
  val inj5 : t Pattern.t Pattern.t Pattern.t Pattern.t Pattern.t -> t
end

module type Basic1 = sig
  module Pattern : Functor.S

  type 'a t

  val inj : 'a * 'a t Pattern.t -> 'a t
end

module type S1 = sig
  include Basic1

  val fix : 'a -> 'a t Pattern.t -> 'a t
  val inj_succ : ('b -> 'a t) -> 'a * 'b Pattern.t -> 'a t
  val inj2 : 'a * ('a * 'a t Pattern.t) Pattern.t -> 'a t
  val inj3 : 'a * ('a * ('a * 'a t Pattern.t) Pattern.t) Pattern.t -> 'a t

  val inj4 :
       'a * ('a * ('a * ('a * 'a t Pattern.t) Pattern.t) Pattern.t) Pattern.t
    -> 'a t

  val inj5 :
       'a
       * ( 'a
         * ('a * ('a * ('a * 'a t Pattern.t) Pattern.t) Pattern.t) Pattern.t )
         Pattern.t
    -> 'a t
end

module type Basic2 = sig
  module First : sig
    include Basic1
    include S1 with type 'a t := 'a t and module Pattern := Pattern
  end

  module Pattern : Bifunctor.S

  type ('a, 'b) t

  val inj : 'b * ('a First.t, ('a, 'b) t) Pattern.t -> ('a, 'b) t
end

module type S2 = sig
  include Basic2

  val fix : 'b -> ('a First.t, ('a, 'b) t) Pattern.t -> ('a, 'b) t

  val inj_succ :
    ('c -> ('a, 'b) t) -> 'b * ('a First.t, 'c) Pattern.t -> ('a, 'b) t

  val inj2 :
       'b * ('a First.t, 'b * ('a First.t, ('a, 'b) t) Pattern.t) Pattern.t
    -> ('a, 'b) t

  val inj3 :
       'b
       * ( 'a First.t
         , 'b * ('a First.t, 'b * ('a First.t, ('a, 'b) t) Pattern.t) Pattern.t
         )
         Pattern.t
    -> ('a, 'b) t

  val inj4 :
       'b
       * ( 'a First.t
         , 'b
           * ( 'a First.t
             , 'b
               * ( 'a First.t
                 , 'b * ('a First.t, ('a, 'b) t) Pattern.t )
                 Pattern.t )
             Pattern.t )
         Pattern.t
    -> ('a, 'b) t

  val inj5 :
       'b
       * ( 'a First.t
         , 'b
           * ( 'a First.t
             , 'b
               * ( 'a First.t
                 , 'b
                   * ( 'a First.t
                     , 'b * ('a First.t, ('a, 'b) t) Pattern.t )
                     Pattern.t )
                 Pattern.t )
             Pattern.t )
         Pattern.t
    -> ('a, 'b) t
end

module Make (X : Basic) :
  S with type t := X.t and module Pattern := X.Pattern = struct
  open X

  let inj = inj
  let inj_succ inj_pred x = inj (Pattern.map inj_pred x)
  let inj2 = inj_succ inj
  let inj3 = inj_succ inj2
  let inj4 = inj_succ inj3
  let inj5 = inj_succ inj4
end

module Make1 (X : Basic1) :
  S1 with type 'a t := 'a X.t and module Pattern := X.Pattern = struct
  open X

  let inj = inj
  let fix meta pattern = inj (meta, pattern)
  let inj_succ inj_pred x = inj @@ on_snd (Pattern.map inj_pred) x
  let inj2 x = inj_succ inj x
  let inj3 x = inj_succ inj2 x
  let inj4 x = inj_succ inj3 x
  let inj5 x = inj_succ inj4 x
end

module Make2 (X : Basic2) :
  S2
  with type ('a, 'b) t := ('a, 'b) X.t
   and module First := X.First
   and module Pattern := X.Pattern = struct
  open X

  let inj = inj
  let fix meta pattern = inj (meta, pattern)
  let inj_succ inj_pred x = inj @@ on_snd (Pattern.map Fn.id inj_pred) x
  let inj2 x = inj_succ inj x
  let inj3 x = inj_succ inj2 x
  let inj4 x = inj_succ inj3 x
  let inj5 x = inj_succ inj4 x
end
