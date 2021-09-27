open Core_kernel
open Middle

(** Origin blocks, to keep track of where variables are declared *)
type originblock =
  | MathLibrary
  | Functions
  | Data
  | TData
  | Param
  | TParam
  | Model
  | GQuant
[@@deriving sexp]

type varinfo = {origin: originblock; global: bool; readonly: bool}
[@@deriving sexp]

type info =
  { type_: UnsizedType.t
  ; kind:
      [ `Variable of varinfo
      | `UserDeclared of Location_span.t
      | `StanMath
      | `UserDefined ] }
[@@deriving sexp]

type t = info list String.Map.t

let create () =
  let functions =
    Hashtbl.to_alist Stan_math_signatures.stan_math_signatures
    |> List.map ~f:(fun (key, values) ->
           ( key
           , List.map
               ~f:(fun (rt, args, mem) ->
                 let type_ =
                   UnsizedType.UFun (args, rt, Fun_kind.FnPlain, mem)
                 in
                 {type_; kind= `StanMath} )
               values ) )
    |> String.Map.of_alist_exn
  in
  functions

let add env key type_ kind = Map.add_multi env ~key ~data:{type_; kind}

let add_all_raw env key data =
  let env = Map.remove env key in
  List.fold ~init:env ~f:(fun env data -> Map.add_multi env ~key ~data) data

let find env key = Map.find_multi env key
let mem env key = Map.mem env key
let iter env f = Map.iter env ~f
