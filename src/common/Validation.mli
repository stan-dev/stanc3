module type Infix = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type S = sig
  type error
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val pure : 'a -> 'a t
  val apply : 'a t -> f:('a -> 'b) t -> 'b t
  val apply_const : 'a t -> 'b t -> 'b t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val liftA2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val liftA3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val sequence : 'a t list -> 'a list t
  val ok : 'a -> 'a t
  val error : error -> _ t
  val is_error : 'a t -> bool
  val is_success : 'a t -> bool
  val get_errors_opt : 'a t -> error list option
  val get_first_error_opt : 'a t -> error option
  val get_success_opt : 'a t -> 'a option

  val get_with :
    'a t -> with_ok:('a -> 'b) -> with_errors:(error list -> 'b) -> 'b

  val to_result : 'a t -> ('a, error list) result

  module Validation_infix : Infix with type 'a t := 'a t
  include Infix with type 'a t := 'a t
end

module Make (X : sig
  type t
end) : S with type error := X.t
