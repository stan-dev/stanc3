open Core_kernel

module type S = sig
  type ('a, 'b) t

  module A : Applicative.S

  val traverse :
    ('a, 'b) t -> f:('a -> 'c A.t) -> g:('b -> 'd A.t) -> ('c, 'd) t A.t
end

module type S2 = sig
  type ('a, 'b) t

  module A : Applicative.S2

  val traverse :
       ('a, 'b) t
    -> f:('a -> ('c, 'e) A.t)
    -> g:('b -> ('d, 'e) A.t)
    -> (('c, 'd) t, 'e) A.t
end
