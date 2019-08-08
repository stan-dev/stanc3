module Fn = Core_kernel.Fn
module Tuple = Core_kernel.Tuple
open Helpers

module type Basic = sig
  module Pattern : Functor.S

  type t

  val inj : t Pattern.t -> t
  val proj : t -> t Pattern.t
end

module type S = sig
  module Pattern : Functor.S

  type t
  type 'a algebra = 'a Pattern.t -> 'a

  val cata : ('a Pattern.t -> 'a) -> t -> 'a
  val transform_bottom_up : (t -> t) -> t -> t

  
  type 'a r_algebra = t -> 'a Pattern.t -> 'a

  val para : (t -> 'a Pattern.t -> 'a) -> t -> 'a
  val transform_with_context : (t -> t -> t) -> t -> t
  
end

module Make (X : Basic) :
  S with type t := X.t and module Pattern := X.Pattern = struct
  open X

  type 'a algebra = 'a Pattern.t -> 'a

  let rec cata f x = proj x |> Pattern.map (cata f) |> f
  let transform_bottom_up f x = cata (Fn.compose f inj) x

  type 'a r_algebra = t -> 'a Pattern.t -> 'a

  let rec para f t = proj t |> Pattern.map (para f) |> f t

  let transform_with_context f x =
    para (fun ctxt projected -> f ctxt @@ inj projected) x


end

module type Basic1 = sig
  module Pattern : Functor.S

  type 'a t

  val inj : 'a * 'a t Pattern.t -> 'a t
  val proj : 'a t -> 'a * 'a t Pattern.t
end

module type S1 = sig
  module Pattern : Functor.S

  type 'a t
  type ('a, 'r) algebra = 'a * 'r Pattern.t -> 'r

  val cata : ('a, 'r) algebra -> 'a t -> 'r
  val transform_bottom_up : ('a t -> 'a t) -> 'a t -> 'a t


  type ('a, 'r) r_algebra = 'a t -> 'a * 'r Pattern.t -> 'r

  val para : ('a, 'r) r_algebra -> 'a t -> 'r
  val transform_with_context : ('a t -> 'a t -> 'a t) -> 'a t -> 'a t


end

module Make1 (X : Basic1) :
  S1 with type 'a t := 'a X.t and module Pattern := X.Pattern = struct
  open X

  type ('a, 'r) algebra = 'a * 'r Pattern.t -> 'r

  let rec cata f x = proj x |> on_snd (Pattern.map (cata f)) |> f
  let transform_bottom_up f x = cata (Fn.compose f inj) x

  type ('a, 'r) r_algebra = 'a t -> 'a * 'r Pattern.t -> 'r

  let rec para f t = proj t |> on_snd (Pattern.map (para f)) |> f t
  let transform_with_context f = para (fun ctxt proj -> f ctxt @@ inj proj)

end

module type Basic2 = sig
  type ('a, 'b) t

  module Pattern : Bifunctor.S

  module First : sig
    include Basic1
    include S1 with type 'a t := 'a t and module Pattern := Pattern
  end

  val inj : 'b * ('a First.t, ('a, 'b) t) Pattern.t -> ('a, 'b) t
  val proj : ('a, 'b) t -> 'b * ('a First.t, ('a, 'b) t) Pattern.t
end

module type S2 = sig
  type ('a, 'b) t

  module First : S1
  module Pattern : Bifunctor.S

  type ('a, 'b, 'r) algebra = 'b * ('a, 'r) Pattern.t -> 'r

  val cata :
    ('a, 'r1) First.algebra -> ('r1, 'b, 'r2) algebra -> ('a, 'b) t -> 'r2

  val transform_bottom_up :
       ('a First.t -> 'a First.t)
    -> (('a, 'b) t -> ('a, 'b) t)
    -> ('a, 'b) t
    -> ('a, 'b) t

 

  type ('a, 'b, 'r1, 'r2) r_algebra =
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
    -> ('a, 'b) t

 
end

module Make2 (X : Basic2) :
  S2
  with type ('a, 'b) t := ('a, 'b) X.t
   and module Pattern := X.Pattern
   and module First := X.First = struct
  open X

  type ('a, 'b, 'r) algebra = 'b * ('a, 'r) Pattern.t -> 'r

  let rec cata f g x =
    proj x |> on_snd (Pattern.map (First.cata f) (cata f g)) |> g

  let transform_bottom_up f g x =
    cata (Fn.compose f First.inj) (Fn.compose g inj) x

 
  type ('a, 'b, 'r1, 'r2) r_algebra =
    ('a, 'b) t -> 'b * ('r1, 'r2) Pattern.t -> 'r2

  let rec para f g x =
    proj x |> on_snd (Pattern.map (First.para f) (para f g)) |> g x

  let transform_with_context f g =
    para
      (fun ctxt proj -> f ctxt @@ First.inj proj)
      (fun ctxt proj -> g ctxt @@ inj proj)
 
end
