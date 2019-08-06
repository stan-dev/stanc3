module type Basic = sig
  module Pattern : Functor.S

  type t

  val proj : t -> t Pattern.t
end

module type S = sig
  include Basic

  val proj_succ : (t -> t Pattern.t) -> t -> t Pattern.t Pattern.t
  val proj2 : t -> t Pattern.t Pattern.t
  val proj3 : t -> t Pattern.t Pattern.t Pattern.t
  val proj4 : t -> t Pattern.t Pattern.t Pattern.t Pattern.t
  val proj5 : t -> t Pattern.t Pattern.t Pattern.t Pattern.t Pattern.t
end

module type Basic1 = sig
  module Pattern : Functor.S

  type 'a t

  val proj : 'a t -> 'a * 'a t Pattern.t
end

module type S1 = sig
  include Basic1



  val proj_succ : ('a t -> 'b) -> 'a t -> 'a * 'b Pattern.t
  val proj2 : 'a t -> 'a * ('a * 'a t Pattern.t) Pattern.t
  val proj3 : 'a t -> 'a * ('a * ('a * 'a t Pattern.t) Pattern.t) Pattern.t

  val proj4 :
       'a t
    -> 'a * ('a * ('a * ('a * 'a t Pattern.t) Pattern.t) Pattern.t) Pattern.t

  val proj5 :
       'a t
    -> 'a
       * ( 'a
         * ('a * ('a * ('a * 'a t Pattern.t) Pattern.t) Pattern.t) Pattern.t )
         Pattern.t

  val pattern : 'a t -> 'a t Pattern.t
  val meta : 'a t -> 'a
  val pattern_succ : ('a t -> 'b) -> 'a t ->  'b Pattern.t 
  val pattern2 : 'a t -> 'a t Pattern.t Pattern.t
  val pattern3 : 'a t -> 'a t Pattern.t Pattern.t Pattern.t
  val pattern4 : 'a t -> 'a t Pattern.t Pattern.t Pattern.t Pattern.t
  val pattern5 : 'a t -> 'a t Pattern.t Pattern.t Pattern.t Pattern.t Pattern.t

end

module type Basic2 = sig
  module First : sig
    include Basic1
    include S1 with type 'a t := 'a t and module Pattern := Pattern
  end

  module Pattern : Bifunctor.S

  type ('a, 'b) t

  val proj : ('a, 'b) t -> 'b * ('a First.t, ('a, 'b) t) Pattern.t
end

module type S2 = sig
  include Basic2
  
  val proj_succ :
    (('a, 'b) t -> 'c) -> ('a, 'b) t -> 'b * ('a First.t, 'c) Pattern.t

  val proj2 :
       ('a, 'b) t
    -> 'b * ('a First.t, 'b * ('a First.t, ('a, 'b) t) Pattern.t) Pattern.t

  val proj3 :
       ('a, 'b) t
    -> 'b
       * ( 'a First.t
         , 'b * ('a First.t, 'b * ('a First.t, ('a, 'b) t) Pattern.t) Pattern.t
         )
         Pattern.t

  val proj4 :
       ('a, 'b) t
    -> 'b
       * ( 'a First.t
         , 'b
           * ( 'a First.t
             , 'b
               * ( 'a First.t
                 , 'b * ('a First.t, ('a, 'b) t) Pattern.t )
                 Pattern.t )
             Pattern.t )
         Pattern.t

  val proj5 :
       ('a, 'b) t
    -> 'b
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

  val pattern : ('a, 'b) t -> ('a First.t, ('a, 'b) t) Pattern.t
  val meta : ('a, 'b) t -> 'b

  val pattern_succ :
    (('a, 'b) t -> 'c) -> ('a, 'b) t ->  ('a First.t, 'c) Pattern.t

  val pattern2 : ('a, 'b) t -> ('a First.t, ('a First.t, ('a, 'b) t) Pattern.t) Pattern.t
  val pattern3 : ('a, 'b) t -> ('a First.t, ('a First.t, ('a First.t, ('a, 'b) t) Pattern.t) Pattern.t) Pattern.t
  val pattern4 : ('a, 'b) t -> ('a First.t, ('a First.t, ('a First.t, ('a First.t, ('a, 'b) t) Pattern.t) Pattern.t) Pattern.t) Pattern.t
  val pattern5 : ('a, 'b) t -> ('a First.t, ('a First.t, ('a First.t, ('a First.t, ('a First.t, ('a, 'b) t) Pattern.t) Pattern.t) Pattern.t) Pattern.t) Pattern.t
end

module Make (X : Basic) : S with type t := X.t and module Pattern := X.Pattern

module Make1 (X : Basic1) :
  S1 with type 'a t := 'a X.t and module Pattern := X.Pattern

module Make2 (X : Basic2) :
  S2
  with type ('a, 'b) t := ('a, 'b) X.t
   and module First := X.First
   and module Pattern := X.Pattern
