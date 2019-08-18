module type S = sig
  type t

  val leq : t -> t -> bool
end
