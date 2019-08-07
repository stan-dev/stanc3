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

module Make (X : Basic) : S with type t := X.t and module Pattern := X.Pattern

module Make1 (X : Basic1) :
  S1 with type 'a t := 'a X.t and module Pattern := X.Pattern

module Make2 (X : Basic2) :
  S2
  with type ('a, 'b) t := ('a, 'b) X.t
   and module First := X.First
   and module Pattern := X.Pattern
