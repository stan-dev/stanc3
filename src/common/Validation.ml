include Core_kernel

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

(**
  It is common need in a compiler to check for certain errors e.g. syntax 
  errors, type errors, other semantic errors, etc. Some typical ways of doing 
  this include exceptions or representing success/failure with `Result.t`. 
  In both of these approaches, the computation will 'fail' when we reach the 
  first error condition. 
  
  In some situations, it may be preferable to report _all_ of these errors to a 
  user in one go. `Validation` gives us this ability*.

  The implementation below is an example of an OCaml `functor` i.e. a module 
  that is _parametrized_ by another module. Our `Valiation.Make` functor is 
  parameterized over another module containing the type of errors.
  
  When applied, the functor returns a module with signature `Validation.S` but 
  with the underlying error type fixed at the type `t` contained in the module `X`.
  As an example, we use this functor with the module `Semantic_error` 
  (defined in `frontend`) so the type `error` is made equal to `Semantic_error.t`
  
  Under the hood, `Validation` is just a `Result.t` with a `NonEmpty` list of 
  `error`s. The main workhorse of the module is the `apply` function. 
  The function accepts a value lifted into our result type `'a t` and a 
  _function_ lifted into the result type `('a -> 'b) t`. 
  
  ```
  let apply x ~f = 
    match (f,x) with
  ```

  `apply` 'unwraps' the function and the value and performs different actions 
  depending on whether an error has occured. Because we have two arguments each 
  of which can be in two states, we have to check for four conditions:
  
  The first condition is that both the function `f` and the value `x` are `Ok`; 
  in this case we can simply apply the function to the value and lift is back 
  into the result type:
  
  ```
    | Ok f , Ok x -> Ok (f x)
  ```
  
  The second situation is the the function is an `Error`; in this case we can't
  apply the function so we simply return the same error:
  
  ```
    | Error e , Ok _ -> Error e
  ```
  
  The third situation is that the function `f` is `Ok` but he value `x` is an 
  `Error`; again, we can't apply the function so we return the error:
  
  ```
    | Ok _ , Error e -> Error e
  ```
  
  The final situation is that both `f` and `x` are errors; because our error 
  type is a `NonEmpty` list, we can combine them using the `append` function and
  track both:
  
  ```
    | Error e1 , Error e2 -> Error(append e1 e2)
  ```
  
  The module hides the implementation of `'a t` but exposes the functions `ok` 
  and `error` to construct this type.
  
  * This is true so long as we restrict our usage to `apply` (and related) 
  functions i.e. the _applicative_ interface.As soon as we use `bind` 
  (i.e. the _monadic_ interface) we lose this ability. Technically, `Validation` 
  doesn't constitute a monad since it violates the law:
  
  ```
  liftM2 f m1 m2 === apply f a1 s2
  ```
*)
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

  module Validation_infix = struct let ( >>= ) x f = bind x ~f end
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

  let get_with x ~with_ok ~with_errors =
    match x with
    | Ok x -> with_ok x
    | Error (NonEmpty (x, xs)) -> with_errors @@ (x :: xs)

  let to_result x =
    match x with Ok x -> Ok x | Error (NonEmpty (x, xs)) -> Error (x :: xs)
end
