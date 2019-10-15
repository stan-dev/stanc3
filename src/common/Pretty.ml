(** Named signatures for types that can be pretty-printed.
*)

module type S = sig
  type t

  val pp : Format.formatter -> t -> unit
end

module type S1 = sig
  type 'a t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type S2 = sig
  type ('a, 'b) t

  val pp :
       (Format.formatter -> 'a -> unit)
    -> (Format.formatter -> 'b -> unit)
    -> Format.formatter
    -> ('a, 'b) t
    -> unit
end
