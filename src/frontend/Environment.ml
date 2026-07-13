open Core
open Core.Poly
open Middle

type originblock =
  | MathLibrary
  | Functions
  | Data
  | TData
  | Param
  | TParam
  | Model
  | GQuant

let block_name = function
  | MathLibrary -> "Stan Math Library"
  | Functions -> "functions block"
  | Data -> "data block"
  | TData -> "transformed data block"
  | Param -> "parameters block"
  | TParam -> "transformed parameters block"
  | Model -> "model block"
  | GQuant -> "generated quantities block"

type varinfo =
  {origin: originblock; global: bool; readonly: bool; location: Location_span.t}

type info =
  { type_: UnsizedType.t
  ; kind:
      [ `Variable of varinfo
      | `UserDeclared of Location_span.t
      | `StanMath
      | `UserDefined of Location_span.t ] }

let location = function
  | {kind= `Variable {location; _}; _}
   |{kind= `UserDeclared location; _}
   |{kind= `UserDefined location; _} ->
      Some location
  | {kind= `StanMath; _} -> None

type t = info list String.Map.t

let stan_math_environment =
  Lazy.map Stan_math_signatures.signatures_alist ~f:(fun signatures ->
      List.map signatures ~f:(fun (key, values) ->
          ( key
          , List.map values ~f:(fun s ->
                {type_= UnsizedType.UFun s; kind= `StanMath}) ))
      |> String.Map.of_alist_exn)

let add env name type_ kind = Map.add_multi env ~key:name ~data:{type_; kind}
let set_raw env key data = Map.set env ~key ~data
let find env key = Map.find_multi env key
let mem env key = Map.mem env key
let iteri env f = Map.iteri env ~f:(fun ~key ~data -> f key data)

let nearest_ident env name =
  let open Stdlib.Option.Syntax in
  let max_dist s =
    let length = String.length s in
    let length =
      (* Special case: if a function is a *_lpdf, for example, don't give it
         extra typo leeway to avoid some poor suggestions *)
      if Fun_kind.suffix_from_name s <> FnPlain then
        String.length (fst @@ String.rsplit2_exn ~on:'_' s)
      else length in
    Int.min (length / 2) 5 in
  let iter f = Map.iter_keys ~f env in
  let suggestions = Stdlib.String.spellcheck ~max_dist iter name in
  let other_suffixes =
    Utils.(
      distribution_suffices
      @ List.map ~f:(fun n -> "_" ^ n) cumulative_distribution_suffices_w_rng)
    |> List.map ~f:(fun suffix -> name ^ suffix)
    |> List.filter ~f:(Map.mem env) in
  let* key = Option.first_some (List.hd suggestions) (List.hd other_suffixes) in
  let+ values = Map.find env key in
  (key, List.map ~f:location values)
