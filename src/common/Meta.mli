(** Signature of all meta data used to annotate IRs *)
module type S = sig
  type t [@@deriving compare, sexp, hash]

  include Pretty.S with type t := t
end
