module type S = sig
  type t

  val empty : t
  val combine : t -> t -> t
end

module Bool_or : S with type t = bool = struct
  type t = bool

  let empty = false
  let combine a b = a || b
end

module Bool_and : S with type t = bool = struct
  type t = bool

  let empty = true
  let combine a b = a && b
end

module Make_option_first (X : sig
  type t
end) : S with type t = X.t option = struct
  type t = X.t option

  let empty = None
  let combine a b = match (a, b) with Some _, _ -> a | _ -> b
end

module Make_option_last (X : sig
  type t
end) : S with type t = X.t option = struct
  type t = X.t option

  let empty = None
  let combine a b = match (a, b) with _, Some _ -> b | _ -> a
end
