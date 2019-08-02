module type S = sig
  type ('a, 'b) t

  val map : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
end
