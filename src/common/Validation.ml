include Core_kernel

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
module Make (Warning : sig
  type t
end) (Error : sig
  type t
end) : S with module Warning := Warning and module Error := Error = struct
  type 'a t =
    ('a * Warning.t list, Error.t NonEmptyList.t * Warning.t list) result

  module Basic = struct
    type nonrec 'a t = 'a t

    let map_ x ~f =
      match x with
      | Result.Ok (x, ws) -> Ok (f x, ws)
      | Error (x, ws) -> Error (x, ws)

    let map = `Custom map_

    let apply f x =
      match (f, x) with
      | Result.Ok (f, ws), Result.Ok (x, vs) ->
          Result.Ok (f x, List.append ws vs)
      | Ok (_, ws), Error (e, vs) -> Error (e, List.append ws vs)
      | Error (e, ws), Ok (_, vs) -> Error (e, List.append ws vs)
      | Error (es, ws), Error (fs, vs) ->
          Error (NonEmptyList.append es fs, List.append ws vs)

    let bind x ~f =
      match x with
      | Result.Ok (x, ws) -> (
        match f x with
        | Result.Ok (x, vs) -> Result.Ok (x, List.append ws vs)
        | Error (e, vs) -> Error (e, List.append ws vs) )
      | Error (e, ws) -> Error (e, ws)

    let return x = Result.Ok (x, [])
  end

  include Applicative.Make (Basic)
  include Monad.Make (Basic)

  let apply_const a b = apply (map a ~f:(fun _ x -> x)) b
  let liftA2 f x y = apply (apply (return f) x) y
  let liftA3 f x y z = apply (apply (apply (return f) x) y) z
  let ok x = return x

  let error ?warn err : 'a t =
    Result.Error
      ( NonEmptyList.singleton err
      , Option.value_map ~default:[] ~f:(fun x -> [x]) warn )

  let warn ~warn x : 'a t = Result.Ok (x, [warn])
  let is_error = function Result.Error _ -> true | _ -> false
  let is_warning = function Result.Ok (_, _ :: _) -> true | _ -> false
  let is_success = function Result.Ok (_, []) -> true | _ -> false

  let get_errors_opt = function
    | Result.Error (es, ws) -> Some (NonEmptyList.to_list es, ws)
    | _ -> None

  let get_first_error_opt = function
    | Result.Error (es, _) -> Some (NonEmptyList.hd es)
    | _ -> None

  let get_success_opt = function
    | Result.Ok (x, ws) -> Some (x, ws)
    | _ -> None

  let to_result x =
    match x with
    | Result.Ok (x, ws) -> Result.Ok (x, ws)
    | Error (es, ws) -> Error (NonEmptyList.to_list es, ws)
end
