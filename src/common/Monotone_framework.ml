open Core_kernel

type 'a entry_exit = {entry: 'a; exit: 'a}

let entry {entry; _} = entry
let exit {exit; _} = exit

module type S = sig
  type t
  type property

  module Label : Label.S
  module F : Flowgraph.S with type t := t and module Label := Label
  module L : Lattice.S with type t := t and type property := property

  module TF :
    Transfer_function.S with type t := t and type property := property

  type flowgraph_info =
    { flowgraph: (Label.t * Label.t) list
    ; initials: Label.Set.t
    ; associations: t Label.Map.t }

  val solve :
    flowgraph_info -> t -> t Label.Map.t * property entry_exit Label.Map.t

  val solve_t : t -> t Label.Map.t * property entry_exit Label.Map.t
end

module Make
    (F : Flowgraph.S)
    (L : Lattice.S with type t := F.t)
    (TF : Transfer_function.S
          with type t := F.t
           and type property := L.property) :
  S
  with type t := F.t
   and module Label := F.Label
   and module F := F
   and type property := L.property
   and module L := L
   and module TF := TF = struct
  type flowgraph_info =
    { flowgraph: (F.Label.t * F.Label.t) list
    ; initials: F.Label.Set.t
    ; associations: F.t F.Label.Map.t }

  (** Add elements to the worklist (see Step 2, page 75, Nielsen et al.) *)
  let rec enqueue label accu = function
    | [] -> accu
    | (l, l') :: rest when l = label -> enqueue label ((l, l') :: accu) rest
    | _ :: rest -> enqueue label accu rest

  let update_analysis analysis label prop =
    F.Label.Map.update analysis label ~f:(fun prop_opt ->
        L.lub (Option.value_exn prop_opt) prop )

  let rec solve_helper all_props flow assocs analysis = function
    | [] -> analysis
    | (l1, l2) :: rest ->
        let t = F.Label.Map.find_exn assocs l1 in
        let props1 = TF.apply all_props t @@ F.Label.Map.find_exn analysis l1
        and props2 = F.Label.Map.find_exn analysis l2 in
        if not (L.leq props1 props2) then
          let analysis' = update_analysis analysis l2 props1
          and w = enqueue l2 rest flow in
          solve_helper all_props flow assocs analysis' w
        else solve_helper all_props flow assocs analysis rest

  let result_of_analysis all_props initials flow assocs analysis =
    let all_labels =
      Set.to_list initials :: List.map ~f:(fun (l, l') -> [l; l']) flow
      |> List.concat
    in
    List.fold_left ~init:F.Label.Map.empty all_labels ~f:(fun accu lbl ->
        let t = F.Label.Map.find_exn assocs lbl
        and entry = F.Label.Map.find_exn analysis lbl in
        let exit = TF.apply all_props t entry in
        let data = {entry; exit} in
        match F.Label.Map.add accu ~key:lbl ~data with
        | `Duplicate -> accu
        | `Ok accu' -> accu' )

  let initialize initials extremal_value least_value flow =
    let all_labels =
      Set.to_list initials :: List.map ~f:(fun (l, l') -> [l; l']) flow
      |> List.concat
    in
    List.fold_left ~init:F.Label.Map.empty
      ~f:(fun accu label ->
        let data =
          if F.Label.Set.mem initials label then extremal_value
          else least_value
        in
        match F.Label.Map.add accu ~key:label ~data with
        | `Duplicate -> accu
        | `Ok accu' -> accu' )
      all_labels

  let solve {flowgraph; initials; associations} x =
    let extremal_value = L.extremal_value_of x
    and least_value = L.least_element_of x
    and all_props = TF.all_properties_of x in
    let init = initialize initials extremal_value least_value flowgraph
    and worklist = flowgraph in
    let analysis =
      solve_helper all_props flowgraph associations init worklist
    in
    ( associations
    , result_of_analysis all_props initials flowgraph associations analysis )

  let solve_t x =
    solve
      { flowgraph= F.flow_of_t x
      ; initials= F.extremal_labels_of_t x
      ; associations= F.associations_of_t x }
      x
end
