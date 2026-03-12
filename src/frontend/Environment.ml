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
  let functions =
    Stan_math_signatures.get_stan_math_signatures_alist ()
    |> List.map ~f:(fun (key, values) ->
        ( key
        , List.map values ~f:(fun s ->
              {type_= UnsizedType.UFun s; kind= `StanMath}) ))
    |> String.Map.of_alist_exn in
  functions

let add env name type_ kind = Map.add_multi env ~key:name ~data:{type_; kind}
let set_raw env key data = Map.set env ~key ~data
let find env key = Map.find_multi env key
let mem env key = Map.mem env key
let iteri env f = Map.iteri env ~f:(fun ~key ~data -> f key data)

module Distance = struct
  (** Wagner–Fischer algorithm for edit distance Adapted from pseudocode on
      {{:https://en.wikipedia.org/wiki/Levenshtein_distance}Wikipedia} Some
      horribly, horribly iterative code, but it's quick and only for error
      messaging *)
  let dist s t =
    let m = String.length s in
    let n = String.length t in
    let previous_row = ref @@ Array.init (n + 1) ~f:Fn.id in
    let current_row = ref @@ Array.create ~len:(n + 1) 0 in
    for i = 0 to m - 1 do
      !current_row.(0) <- i + 1;
      for j = 0 to n - 1 do
        let deletion_cost = !previous_row.(j + 1) + 1 in
        let insertion_cost = !current_row.(j) + 1 in
        let substitution_cost =
          if s.[i] = t.[j] then !previous_row.(j) else !previous_row.(j) + 1
        in
        !current_row.(j + 1) <-
          Int.min deletion_cost (Int.min insertion_cost substitution_cost)
      done;
      (* swap *)
      let temp = !current_row in
      current_row := !previous_row;
      previous_row := temp
    done;
    !previous_row.(n)

  (** Find the closest entry to [name] in [lst] with edit distance less than
      [?max]. Does a rather naive pairwise search, but only checks if
      [|len a - len b| < max]. *)
  let find_min ?max:(limit = 3) lst name =
    let n = String.length name in
    let rec loop lst (celt, cmin) =
      match lst with
      | [] -> (celt, cmin)
      | candidate :: lst ->
          let m = String.length candidate in
          (* skip if the lengths make it impossible for edit distance to satisfy
             maximum *)
          if m - n > limit || n - m > limit then loop lst (celt, cmin)
          else
            let edist = dist name candidate in
            if edist < cmin then loop lst (candidate, edist)
            else loop lst (celt, cmin) in
    (* don't provide suggestions for length-1 names. Always ends up suggesting
       'e' *)
    if n = 1 then None
    else
      let suggestion, _ = loop lst (name, limit) in
      (* if [name = suggestion], that implies that nothing was found which had
         an edit distance less than the limit (because name is the initial thing
         given to [loop]), so we return None *)
      if name <> suggestion then Some suggestion else None
end

let max_distance_for_length l = if l < 10 then 3 else 5

let nearest_ident env name =
  let open Common.Let_syntax.Option in
  let* key =
    try
      (* catch any errors in distance and just ignore them, no big deal *)
      Option.first_some
        (Distance.find_min
           ~max:(max_distance_for_length (String.length name))
           (Map.keys env) name)
        (Utils.(
           distribution_suffices
           @ List.map
               ~f:(fun n -> "_" ^ n)
               cumulative_distribution_suffices_w_rng)
        |> List.map ~f:(fun suffix -> name ^ suffix)
        |> List.filter ~f:(Map.mem env)
        |> List.hd)
    with _ -> None in
  let+ values = Map.find env key in
  (key, List.map ~f:location values)
