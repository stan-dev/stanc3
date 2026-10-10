open Std
open Middle
open Dataflow_types
open Accesses

type loop = {id: label; var: string option}

let common_loops (source_loops : loop list) (sink_loops : loop list) : loop list
    =
  (* Once the lists diverge, deeper loops belong to different nesting branches
     and cannot enclose both accesses. *)
  let rec common_prefix = function
    | source :: sources, sink :: sinks when source.id = sink.id ->
        source :: common_prefix (sources, sinks)
    | _ -> [] in
  common_prefix (source_loops, sink_loops)

type component = {loop: loop; distance: int option}
type t = Independent | Unknown | Constrained of component list

(* Equal invariant indices impose no constraint on iteration distance. *)
let test_ziv ~(offset_difference : int) : t =
  if offset_difference = 0 then Unknown else Independent

let test_strong_siv ~(loops : loop list) ~(loopvar : string)
    ~(iteration_distance : int) : t =
  Constrained
    (List.map loops ~f:(fun loop ->
         { loop
         ; distance=
             (if Option.equal String.equal loop.var (Some loopvar) then
                Some iteration_distance
              else None) }))

let test_subscript ~(loops : loop list) (source : index step)
    (sink : index step) : t =
  match (source, sink) with
  | ( Subscript (Single (Sequential source_term))
    , Subscript (Single (Sequential sink_term)) )
    when Option.equal String.equal source_term.loopvar sink_term.loopvar
         && Option.equal Expr.Typed.equal source_term.symbol sink_term.symbol
    -> (
      let offset_difference = source_term.const - sink_term.const in
      match source_term.loopvar with
      | None -> test_ziv ~offset_difference
      | Some loopvar
        when List.exists loops ~f:(fun loop ->
                 Option.equal String.equal loop.var (Some loopvar)) ->
          (* Matching i + source_offset with j + sink_offset requires j - i =
             source_offset - sink_offset. Coefficient one makes the offset
             difference an integer iteration distance. *)
          let iteration_distance = offset_difference in
          test_strong_siv ~loops ~loopvar ~iteration_distance
      | Some _ -> (* The index variable belongs to no shared loop. *) Unknown)
  | Field source_field, Field sink_field ->
      if source_field = sink_field then Unknown else Independent
  | Subscript _, _ | Field _, _ -> Unknown

(* Both component lists describe the same shared loops in the same order. *)
let merge_components (path_components : component list)
    (position_components : component list) : component list option =
  let merge_component path_component position_component =
    match (path_component.distance, position_component.distance) with
    | Some path_distance, Some position_distance
      when path_distance <> position_distance ->
        None
    | _ ->
        Some
          { path_component with
            distance=
              Option.first_some path_component.distance
                position_component.distance } in
  Option.all (List.map2 path_components position_components ~f:merge_component)

let merge_path_constraints (path_constraints : t) (position_constraints : t) : t
    =
  match (path_constraints, position_constraints) with
  | Independent, _ | _, Independent -> Independent
  | Unknown, constraints | constraints, Unknown -> constraints
  | Constrained path_components, Constrained position_components -> (
      match merge_components path_components position_components with
      | Some agreed_components -> Constrained agreed_components
      | None -> Independent)

let compare_paths ~(loops : loop list) (source : index access)
    (sink : index access) : t =
  if List.compare_lengths source.path sink.path <> 0 then Unknown
  else
    List.fold_left2 source.path sink.path ~init:Unknown
      ~f:(fun path_constraints source_step sink_step ->
        let position_constraints = test_subscript ~loops source_step sink_step in
        merge_path_constraints path_constraints position_constraints)

let may_exist ~(source_first_in_text : bool) (dependence : t) : bool =
  let rec source_may_run_first = function
    | [] -> source_first_in_text
    | {distance= Some distance; _} :: inner ->
        if distance > 0 then true
        else if distance < 0 then false
        else source_may_run_first inner
    (* An earlier outer iteration remains possible regardless of inner
       distances. *)
    | {distance= None; _} :: _ -> true in
  match dependence with
  | Independent -> false
  | Unknown -> true
  | Constrained components -> source_may_run_first components

let pp_component ppf {loop; distance} =
  Fmt.pf ppf "[%s:%a]"
    (Option.value loop.var ~default:"while")
    Fmt.(option ~none:(any "*") int)
    distance

let pp ppf = function
  | Independent -> Fmt.string ppf "independent"
  | Unknown -> Fmt.string ppf "unknown"
  | Constrained components ->
      Fmt.pf ppf "constrained %a" Fmt.(list ~sep:nop pp_component) components
