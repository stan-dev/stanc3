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
