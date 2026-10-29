open Std
open Dataflow_types
open Accesses

(** Compares the steps at one position of two paths. Two tuple fields compare
    like two constant subscripts. *)
let compare_steps (first : step) (second : step) : Affine_subscript.comparison =
  match (first, second) with
  | Field first_field, Field second_field ->
      if first_field = second_field then Always_same else Never_same
  | Subscript first, Subscript second ->
      Affine_subscript.compare_subscripts first second
  | Field _, Subscript _ | Subscript _, Field _ -> Cannot_tell

(** The killer path can be shorter than the killed path, so [m[1]] overwrites
    [m[1, 2]]. At each position of the killer path, the two steps must select
    the same element in every iteration, as [[1]] and [[1]] do. Steps that
    select the same element only within one iteration, as [[n]] and [[n]] do,
    count only when both writes run in the same iteration. *)
let rec is_overwritten ~(is_same_iteration : bool) (killer : step list)
    (killed : step list) : bool =
  match (killer, killed) with
  | [], _ -> true
  | _ :: _, [] -> false
  | killer_step :: killer_rest, killed_step :: killed_rest -> (
      let is_rest_overwritten () =
        is_overwritten ~is_same_iteration killer_rest killed_rest in
      match compare_steps killer_step killed_step with
      | Always_same -> is_rest_overwritten ()
      | Same_at_distance {distance= 0; _} ->
          is_same_iteration && is_rest_overwritten ()
      | Never_same | Same_at_distance _ | Cannot_tell -> false)

type t = Independent | Distance_vector of int option list

(** Sets the distance at [loop_id] in [distances], one entry per loop of
    [loop_ids]. The result is [None] when [distances] already holds a different
    distance at [loop_id], because the two positions then never select the same
    element together. *)
let set_distance ~(loop_ids : label list) (distances : int option list)
    ~(loop_id : label) ~(distance : int) : int option list option =
  Option.all
    (List.map2 loop_ids distances ~f:(fun common_loop_id known_distance ->
         match known_distance with
         | _ when common_loop_id <> loop_id -> Some known_distance
         | Some known when known <> distance -> None
         | Some _ | None -> Some (Some distance)))

let test_dependence ~(common_loop_ids : label list) (source : access)
    (sink : access) : t =
  let all_unknown =
    Distance_vector (List.map common_loop_ids ~f:(fun _ -> None)) in
  if List.compare_lengths source.path sink.path <> 0 then all_unknown
  else
    List.fold_left2 source.path sink.path ~init:all_unknown
      ~f:(fun dependence source_step sink_step ->
        match dependence with
        | Independent -> Independent
        | Distance_vector distances -> (
            match compare_steps source_step sink_step with
            | Never_same -> Independent
            | Always_same | Cannot_tell -> dependence
            | Same_at_distance {loop_id; distance} -> (
                match
                  set_distance ~loop_ids:common_loop_ids distances ~loop_id
                    ~distance
                with
                | Some distances -> Distance_vector distances
                | None -> Independent)))

let is_lexicographically_positive ~(is_loop_independent_possible : bool)
    (dependence : t) : bool =
  match dependence with
  | Independent -> false
  | Distance_vector distances -> (
      (* The first distance that is not zero decides. An unknown distance allows
         an earlier iteration of that loop. *)
      match
        List.find_map distances ~f:(function
          | Some 0 -> None
          | distance -> Some distance)
      with
      | None -> is_loop_independent_possible
      | Some (Some distance) -> distance > 0
      | Some None -> true)

(** Prints the distance at one loop as [[n:1]], with the loop variable that
    [loop_name] gives for the loop label. *)
let pp_distance ~(loop_name : label -> string) ppf
    ((loop_id, distance) : label * int option) =
  Fmt.pf ppf "[%s:%a]" (loop_name loop_id)
    Fmt.(option ~none:(any "*") int)
    distance

let pp ~(loop_name : label -> string) ~(common_loop_ids : label list) ppf =
  function
  | Independent -> Fmt.string ppf "independent"
  | Distance_vector distances when List.for_all distances ~f:Option.is_none ->
      Fmt.string ppf "unknown"
  | Distance_vector distances ->
      Fmt.pf ppf "distances %a"
        Fmt.(list ~sep:nop (pp_distance ~loop_name))
        (List.map2 common_loop_ids distances ~f:(fun loop_id distance ->
             (loop_id, distance)))
