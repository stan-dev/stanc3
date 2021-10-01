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

module Distance = struct
  (*  Wagner–Fischer algorithm for edit distance
  Adapted from psuedocode on Wikipedia 
  https://en.wikipedia.org/wiki/Levenshtein_distance
  Some horribly, horribly iterative code, but it's quick
  and only for error messaging 
  *)
  let dist s t =
    let m = String.length s in
    let n = String.length t in
    let v0 = ref @@ Array.init (n + 1) ~f:Fn.id in
    let v1 = ref @@ Array.create ~len:(n + 1) 0 in
    for i = 0 to m - 1 do
      !v1.(0) <- i + 1 ;
      for j = 0 to n - 1 do
        let deletion_cost = !v0.(j + 1) + 1 in
        let insertion_cost = !v1.(j) + 1 in
        let substitution_cost =
          if s.[i] = t.[j] then !v0.(j) else !v0.(j) + 1
        in
        !v1.(j + 1)
        <- Int.min deletion_cost (Int.min insertion_cost substitution_cost)
      done ;
      (* swap *)
      let temp = !v1 in
      v1 := !v0 ;
      v0 := temp
    done ;
    !v0.(n)

  let find_min lst name =
    let rec loop lst (celt, cmin) =
      match lst with
      | [] -> (celt, cmin)
      | candidate :: lst ->
          let edist = dist name candidate in
          if edist < cmin then loop lst (candidate, edist)
          else loop lst (celt, cmin)
    in
    loop lst (name, String.length name)
end

let nearest_ident env name =
  try
    (* catch any errors in distance and just ignore them, no big deal *)
    let suggestion, distance = Distance.find_min (Map.keys env) name in
    if distance < 3 && name <> suggestion then Some suggestion else None
  with _ -> None
