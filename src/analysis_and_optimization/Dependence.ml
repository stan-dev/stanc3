open Std
open Dataflow_types
open Accesses

(** Compares the steps at one position of two paths. Two tuple fields compare
    like two constant subscripts. *)
let compare_steps (first : step) (second : step) : Index_form.comparison =
  match (first, second) with
  | Field first_field, Field second_field ->
      if first_field = second_field then Always_same else Never_same
  | Subscript first, Subscript second ->
      Index_form.compare_subscripts ~first ~second
  | Field _, Subscript _ | Subscript _, Field _ -> Cannot_tell

(** The killer path can be shorter than the killed path, so [m[1]] overwrites
    [m[1, 2]]. At each position of the killer path, the two steps must select
    the same element in every iteration, as [[1]] and [[1]] do. Steps that
    select the same element only within one iteration, as [[n]] and [[n]] do,
    count only when both writes run in the same iteration. *)
let rec must_overwrite ~(same_iteration : bool) ~(killer : step list)
    ~(killed : step list) : bool =
  match (killer, killed) with
  | [], _ -> true
  | _ :: _, [] -> false
  | killer_step :: killer_rest, killed_step :: killed_rest -> (
      let rest_overwritten () =
        must_overwrite ~same_iteration ~killer:killer_rest ~killed:killed_rest
      in
      match compare_steps killer_step killed_step with
      | Always_same -> rest_overwritten ()
      | Same_at_distance {distance= 0; _} ->
          same_iteration && rest_overwritten ()
      | Never_same | Same_at_distance _ | Cannot_tell -> false)

type t = Independent | Distances of int option list

(** Sets the distance at [loop] in [distances], one entry per loop of [loops].
    The result is [None] when [distances] already holds a different distance at
    [loop], because the two positions then never select the same element
    together. *)
let fix_distance ~(loops : label list) (distances : int option list)
    ~(loop : label) ~(distance : int) : int option list option =
  Option.all
    (List.map2 loops distances ~f:(fun common_loop known_distance ->
         match known_distance with
         | _ when common_loop <> loop -> Some known_distance
         | Some known when known <> distance -> None
         | Some _ | None -> Some (Some distance)))

let compare_paths ~(loops : label list) (source : access) (sink : access) : t =
  let all_unknown = Distances (List.map loops ~f:(fun _ -> None)) in
  if List.compare_lengths source.path sink.path <> 0 then all_unknown
  else
    List.fold_left2 source.path sink.path ~init:all_unknown
      ~f:(fun dependence source_step sink_step ->
        match dependence with
        | Independent -> Independent
        | Distances distances -> (
            match compare_steps source_step sink_step with
            | Never_same -> Independent
            | Always_same | Cannot_tell -> dependence
            | Same_at_distance {loop; distance} -> (
                match fix_distance ~loops distances ~loop ~distance with
                | Some fixed -> Distances fixed
                | None -> Independent)))

let is_plausible ~(source_first_in_text : bool) (dependence : t) : bool =
  let rec source_may_run_first = function
    | [] -> source_first_in_text
    | Some distance :: inner ->
        if distance > 0 then true
        else if distance < 0 then false
        else source_may_run_first inner
    (* An unknown distance allows an earlier iteration of this loop. *)
    | None :: _ -> true in
  match dependence with
  | Independent -> false
  | Distances distances -> source_may_run_first distances

(** Prints the distance at one loop as [[n:1]], with the loop variable that
    [loop_name] gives for the loop label. *)
let pp_distance ~(loop_name : label -> string) ppf
    ((loop, distance) : label * int option) =
  Fmt.pf ppf "[%s:%a]" (loop_name loop)
    Fmt.(option ~none:(any "*") int)
    distance

let pp ~(loop_name : label -> string) ~(loops : label list) ppf = function
  | Independent -> Fmt.string ppf "independent"
  | Distances distances when List.for_all distances ~f:Option.is_none ->
      Fmt.string ppf "unknown"
  | Distances distances ->
      Fmt.pf ppf "distances %a"
        Fmt.(list ~sep:nop (pp_distance ~loop_name))
        (List.map2 loops distances ~f:(fun loop distance -> (loop, distance)))
