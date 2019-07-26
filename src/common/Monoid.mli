module type S = sig
  type t

  val empty : t
  val combine : t -> t -> t
end

module Bool_or : S with type t = bool
module Bool_and : S with type t = bool

module Make_option_first (X : sig
  type t
end) : S with type t = X.t option

module Make_option_last (X : sig
  type t
end) : S with type t = X.t option
