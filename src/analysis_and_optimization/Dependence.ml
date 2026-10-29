open Std
open Middle
open Dataflow_types
open Accesses

type level = {loop: loop; distance: int option}
type t = Independent | Unknown | Distances of level list

(** The strong SIV test for one position. When [source] and [sink] are
    [Sequential] with the same loop term and symbol, and a loop in [loops] sets
    the loop variable, source iteration [i] selects [i + source_term.const] and
    sink iteration [j] selects [j + sink_term.const]. These match when the
    distance [j - i] is [source_term.const - sink_term.const]. The sign flips
    for a [Descending] loop term. The other loops get an unknown distance. Any
    other pair gives [Unknown]. *)
let test_strong_siv ~(loops : loop list) (source : index step)
    (sink : index step) : t =
  match (source, sink) with
  | ( Subscript (Single (Sequential source_term))
    , Subscript (Single (Sequential sink_term)) )
    when Option.equal equal_loop_term source_term.loop_term sink_term.loop_term
         && Option.equal Expr.Typed.equal source_term.symbol sink_term.symbol
    -> (
      let sets_loopvar loopvar = function
        | For {var; _} -> String.equal var loopvar
        | While _ -> false in
      match source_term.loop_term with
      | Some {loopvar; direction}
        when List.exists loops ~f:(sets_loopvar loopvar) ->
          (* A descending loop term flips the sign of the distance. *)
          let offset_difference = source_term.const - sink_term.const in
          let iteration_distance =
            match direction with
            | Ascending -> offset_difference
            | Descending -> -offset_difference in
          Distances
            (List.map loops ~f:(fun loop ->
                 { loop
                 ; distance=
                     (if sets_loopvar loopvar loop then Some iteration_distance
                      else None) }))
      | Some _ | None -> (* No common loop sets the loop variable. *) Unknown)
  | Subscript _, _ | Field _, _ -> Unknown

(** Compares the [source] and [sink] steps at one position. The ZIV test gives
    [Independent] for different constants or fields and [Unknown] for equal
    ones. Other steps go to {!test_strong_siv}. *)
let test_subscript ~(loops : loop list) (source : index step)
    (sink : index step) : t =
  match compare_steps source sink with
  (* The ZIV test separates different constants and different fields. *)
  | Different -> Independent
  (* Equal constants or equal fields do not constrain the iteration distance. *)
  | Same_every_iteration -> Unknown
  | Same_in_one_iteration | May_differ -> test_strong_siv ~loops source sink

(** Merges two level lists over the same common loops in the same order. The
    result is [None] when the known distances at one loop differ. *)
let merge_levels (path_levels : level list) (position_levels : level list) :
    level list option =
  let merge_level path_level position_level =
    match (path_level.distance, position_level.distance) with
    | Some path_distance, Some position_distance
      when path_distance <> position_distance ->
        None
    | _ ->
        Some
          { path_level with
            distance=
              Option.first_some path_level.distance position_level.distance }
  in
  Option.all (List.map2 path_levels position_levels ~f:merge_level)

(** Combines the result of the earlier positions with the result of one more
    position. [Unknown] adds nothing. [Independent] or a conflict gives
    [Independent]. *)
let merge_path_constraints (path_constraints : t) (position_constraints : t) : t
    =
  match (path_constraints, position_constraints) with
  | Independent, _ | _, Independent -> Independent
  | Unknown, constraints | constraints, Unknown -> constraints
  | Distances path_levels, Distances position_levels -> (
      match merge_levels path_levels position_levels with
      | Some agreed_levels -> Distances agreed_levels
      | None -> Independent)

let compare_paths ~(loops : loop list) (source : index access)
    (sink : index access) : t =
  if List.compare_lengths source.path sink.path <> 0 then Unknown
  else
    List.fold_left2 source.path sink.path ~init:Unknown
      ~f:(fun path_constraints source_step sink_step ->
        let position_constraints = test_subscript ~loops source_step sink_step in
        merge_path_constraints path_constraints position_constraints)

let is_plausible ~(source_first_in_text : bool) (dependence : t) : bool =
  let rec source_may_run_first = function
    | [] -> source_first_in_text
    | {distance= Some distance; _} :: inner ->
        if distance > 0 then true
        else if distance < 0 then false
        else source_may_run_first inner
    (* An unknown distance allows an earlier iteration of this loop. *)
    | {distance= None; _} :: _ -> true in
  match dependence with
  | Independent -> false
  | Unknown -> true
  | Distances levels -> source_may_run_first levels

(** Prints one level as [[n:1]], or as [[while:*]] for a [while] loop. *)
let pp_level ppf {loop; distance} =
  Fmt.pf ppf "[%s:%a]"
    (match loop with For {var; _} -> var | While _ -> "while")
    Fmt.(option ~none:(any "*") int)
    distance

let pp ppf = function
  | Independent -> Fmt.string ppf "independent"
  | Unknown -> Fmt.string ppf "unknown"
  | Distances levels ->
      Fmt.pf ppf "distances %a" Fmt.(list ~sep:nop pp_level) levels
