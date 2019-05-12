include Validation_intf
include Core_kernel

module Make (X : sig
  type t
end) : S with type error := X.t = struct
  type errors = NonEmpty of X.t * X.t list

  let append xs ys =
    match (xs, ys) with
    | NonEmpty (x, []), NonEmpty (y, []) -> NonEmpty (x, [y])
    | NonEmpty (x, xs), NonEmpty (y, ys) -> NonEmpty (x, xs @ (y :: ys))

  type 'a t = ('a, errors) result

  let map x ~f = match x with Ok x -> Ok (f x) | Error x -> Error x
  let pure x = Ok x

  let apply x ~f =
    match (f, x) with
    | Ok f, Ok x -> Ok (f x)
    | Error e, Ok _ -> Error e
    | Ok _, Error e -> Error e
    | Error e1, Error e2 -> Error (append e1 e2)

  let apply_const a b = apply b ~f:(map a ~f:(fun _ x -> x))
  let bind x ~f = match x with Ok x -> f x | Error e -> Error e
  let liftA2 f x y = apply y ~f:(apply x ~f:(pure f))
  let liftA3 f x y z = apply z ~f:(apply y ~f:(apply x ~f:(pure f)))
  let consA next rest = liftA2 List.cons next rest
  let sequence ts = List.fold_right ~init:(pure []) ~f:consA ts

  module Validation_infix = struct
    let ( >>= ) x f = bind x ~f
    let ( <*> ) x f = apply x ~f
    let ( *> ) x y = apply_const x y
  end

  include Validation_infix

  let ok x = pure x
  let error x = Error (NonEmpty (x, []))
  let is_error = function Error _ -> true | _ -> false
  let is_success = function Ok _ -> true | _ -> false

  let get_errors_opt = function
    | Error (NonEmpty (x, xs)) -> Some (x :: xs)
    | _ -> None

  let get_first_error_opt = function
    | Error (NonEmpty (x, _)) -> Some x
    | _ -> None

  let get_success_opt = function Ok x -> Some x | _ -> None

  let get_success x ~with_error =
    match x with Ok x -> x | Error (NonEmpty (x, _)) -> with_error x
end
