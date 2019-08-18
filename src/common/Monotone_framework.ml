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

  val solve :
    F.flowgraph_info -> t -> t Label.Map.t * property entry_exit Label.Map.t

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

  (** Initialize the analysis
    ```
    for all l in F or E do
        if l in E then Analysis[l] = extremal_value
        else Analysis[l] = least_value
    ```
  *)
  let initialize initials extremal_value least_value flow =    
    let all_labels =
      Set.to_list initials :: List.map ~f:(fun (l, l') -> [l; l']) flow
      |> List.concat
    and f accu label = 
      let data = 
        if F.Label.Set.mem initials label then extremal_value
        else least_value in
      match F.Label.Map.add accu ~key:label ~data with
        | `Duplicate -> accu
        | `Ok accu' -> accu'  in    
    List.fold_left ~init:F.Label.Map.empty ~f  all_labels


  (** Add elements to the worklist *)
  let enqueue ~label ~init worklist = 
    let rec aux accu = function 
    | [] -> accu 
    | (l,l') :: rest when l = label -> aux ((l,l')::accu) rest 
    | _ :: rest -> aux accu rest in 
    aux init worklist
  
  (** Update the analysis for the label setting the property
  to the least-upper-bound of the current value and the provided property 
  *)
  let update_analysis analysis label prop =
    F.Label.Map.update analysis label 
      ~f:(fun prop_opt ->
            L.lub (Option.value_exn prop_opt) prop 
        )

  (** Step 2: Iteration (updating worklist and analyis) *) 
  let iterate all_props flow assocs analysis worklist = 
    let rec aux analysis = function 
    | [] -> analysis
    | (l1,l2) :: rest -> 
        let t = F.Label.Map.find_exn assocs l1 in
        let props1 = TF.apply all_props t @@ F.Label.Map.find_exn analysis l1
        and props2 = F.Label.Map.find_exn analysis l2 in
        if not (L.leq props1 props2) then
          let analysis' = update_analysis analysis l2 props1
          and w = enqueue ~label:l2 ~init:rest flow in
          aux analysis' w
        else 
          aux analysis rest 
    in 
    aux analysis worklist

  (** Step 3: Presenting the result *)
  let result_of_analysis all_props initials flow assocs analysis =
    let all_labels =
      Set.to_list initials :: List.map ~f:(fun (l, l') -> [l; l']) flow
      |> List.concat
    and f accu lbl = 
      let entry = F.Label.Map.find_exn analysis lbl
      and t = F.Label.Map.find_exn assocs lbl in
      let exit = TF.apply all_props t entry in 
      let data = {entry; exit} in
      match F.Label.Map.add accu ~key:lbl ~data with
      | `Duplicate -> accu
      | `Ok accu' -> accu' 
    in 
    List.fold_left ~init:F.Label.Map.empty ~f all_labels



  (**  Initialise and solve data-flow equations find the least fixed point 
  solution. This version assumes that relevant flowgraph information has 
  been calculated to allow reuse across analyses. See `solve_t` for a 
  version which computes this directly from a value of `t`. 
  *)
  let solve {F.flowgraph; initials; associations} x =
    let extremal_value = L.extremal_value_of x
    and least_value = L.least_element_of x
    and all_props = TF.all_properties_of x in
    
    let init = initialize initials extremal_value least_value flowgraph    
    and worklist = flowgraph in
    let analysis =
      iterate all_props flowgraph associations init worklist
    in
    ( associations
    , result_of_analysis all_props initials flowgraph associations analysis )

  let solve_t x = solve (F.flowgraph_info_of_t x) x
end
