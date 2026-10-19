open Std
open Middle
open Dataflow_types
open Accesses

type loop = Dataflow_types.loop = {id: label; var: string option}

(* After the first loop that differs, no loop is around both accesses. *)
let rec common_loops (source_loops : loop list) (sink_loops : loop list) :
    loop list =
  match (source_loops, sink_loops) with
  | source :: sources, sink :: sinks when source.id = sink.id ->
      source :: common_loops sources sinks
  | _ -> []

type level = {loop: loop; distance: int option}
type t = Independent | Unknown | Distances of level list

let test_strong_siv ~(loops : loop list) (source : index step)
    (sink : index step) : t =
  match (source, sink) with
  | ( Subscript (Single (Sequential source_term))
    , Subscript (Single (Sequential sink_term)) )
    when Option.equal equal_loop_term source_term.loop_term sink_term.loop_term
         && Option.equal Expr.Typed.equal source_term.symbol sink_term.symbol
    -> (
      match source_term.loop_term with
      | Some {loopvar; direction}
        when List.exists loops ~f:(fun (loop : loop) ->
                 Option.equal String.equal loop.var (Some loopvar)) ->
          (* i + source_offset equals j + sink_offset when j - i = source_offset
             - sink_offset. A descending index flips the sign. *)
          let offset_difference = source_term.const - sink_term.const in
          let iteration_distance =
            match direction with
            | Ascending -> offset_difference
            | Descending -> -offset_difference in
          Distances
            (List.map loops ~f:(fun loop ->
                 { loop
                 ; distance=
                     (if Option.equal String.equal loop.var (Some loopvar) then
                        Some iteration_distance
                      else None) }))
      | Some _ | None -> (* No common loop sets the loop variable. *) Unknown)
  | Subscript _, _ | Field _, _ -> Unknown

let test_subscript ~(loops : loop list) (source : index step)
    (sink : index step) : t =
  match compare_steps source sink with
  (* The ZIV test separates different constants and different fields. *)
  | Different -> Independent
  (* Equal constants or equal fields constrain no iteration distance. *)
  | Same_every_iteration -> Unknown
  | Same_in_one_iteration | May_differ -> test_strong_siv ~loops source sink

(* Both level lists hold the same common loops in the same order. *)
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

let pp_level ppf {loop; distance} =
  Fmt.pf ppf "[%s:%a]"
    (Option.value loop.var ~default:"while")
    Fmt.(option ~none:(any "*") int)
    distance

let pp ppf = function
  | Independent -> Fmt.string ppf "independent"
  | Unknown -> Fmt.string ppf "unknown"
  | Distances levels ->
      Fmt.pf ppf "distances %a" Fmt.(list ~sep:nop pp_level) levels
