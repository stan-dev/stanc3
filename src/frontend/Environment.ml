open Core_kernel
open Core_kernel.Poly
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
           , List.map values ~f:(fun (rt, args, mem) ->
                 let type_ =
                   UnsizedType.UFun
                     (args, rt, Fun_kind.suffix_from_name key, mem) in
                 {type_; kind= `StanMath} ) ) )
    |> String.Map.of_alist_exn in
  functions

let add env key type_ kind = Map.add_multi env ~key ~data:{type_; kind}
let set_raw env key data = Map.set env ~key ~data
let find env key = Map.find_multi env key
let mem env key = Map.mem env key
let iter env f = Map.iter env ~f

module Distance = struct
  (**  Wagner–Fischer algorithm for edit distance
  Adapted from psuedocode on
  {{:https://en.wikipedia.org/wiki/Levenshtein_distance}Wikipedia}
  Some horribly, horribly iterative code, but it's quick
  and only for error messaging
  *)
  let dist s t =
    let m = String.length s in
    let n = String.length t in
    let previous_row = ref @@ Array.init (n + 1) ~f:Fn.id in
    let current_row = ref @@ Array.create ~len:(n + 1) 0 in
    for i = 0 to m - 1 do
      !current_row.(0) <- i + 1 ;
      for j = 0 to n - 1 do
        let deletion_cost = !previous_row.(j + 1) + 1 in
        let insertion_cost = !current_row.(j) + 1 in
        let substitution_cost =
          if s.[i] = t.[j] then !previous_row.(j) else !previous_row.(j) + 1
        in
        !current_row.(j + 1) <-
          Int.min deletion_cost (Int.min insertion_cost substitution_cost)
      done ;
      (* swap *)
      let temp = !current_row in
      current_row := !previous_row ;
      previous_row := temp
    done ;
    !previous_row.(n)

  (** Find the closest entry to [name] in [lst] with
    edit distance less than [?max].
    Does a rather naive pairwise search, but only checks
    if [|len a - len b| < max].
  *)
  let find_min ?max:(limit = 3) lst name =
    let n = String.length name in
    let rec loop lst (celt, cmin) =
      match lst with
      | [] -> (celt, cmin)
      | candidate :: lst ->
          let m = String.length candidate in
          (* skip if the lengths make it impossible for edit distance to satisfy maximum *)
          if m - n > limit || n - m > limit then loop lst (celt, cmin)
          else
            let edist = dist name candidate in
            if edist < cmin then loop lst (candidate, edist)
            else loop lst (celt, cmin) in
    (* don't provide suggestions for length-1 names. Always ends up suggesting 'e' *)
    if n = 1 then None
    else
      let suggestion, _ = loop lst (name, limit) in
      (* if [name = suggestion], that implies that nothing was found which had an
         edit distance less than the limit (because name is the inital thing given
         to [loop]), so we return None *)
      if name <> suggestion then Some suggestion else None
end

let max_distance = 3

let nearest_ident env name =
  try
    (* catch any errors in distance and just ignore them, no big deal *)
    Distance.find_min ~max:max_distance (Map.keys env) name
  with _ -> None
