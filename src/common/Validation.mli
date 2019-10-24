open Core_kernel

module type S = sig
  module Error : sig
    type t
  end

  module Warning : sig
    type t
  end

  type 'a t

  include Applicative.S with type 'a t := 'a t
  include Monad.S with type 'a t := 'a t

  val apply_const : 'a t -> 'b t -> 'b t
  val liftA2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val liftA3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val ok : 'a -> 'a t
  val error : ?warn:Warning.t -> Error.t -> _ t
  val warn : warn:Warning.t -> 'a -> 'a t
  val is_error : 'a t -> bool
  val is_warning : 'a t -> bool
  val is_success : 'a t -> bool
  val get_errors_opt : 'a t -> (Error.t list * Warning.t list) option
  val get_first_error_opt : 'a t -> Error.t option
  val get_success_opt : 'a t -> ('a * Warning.t list) option

  val to_result :
    'a t -> ('a * Warning.t list, Error.t list * Warning.t list) result
end

module Make (Warning : sig
  type t
end) (Error : sig
  type t
end) : S with module Warning := Warning and module Error := Error
