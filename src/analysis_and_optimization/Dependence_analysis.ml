open Std
open Middle
open Dataflow_types
open Mir_utils
open Dataflow_utils
open Monotone_framework_sigs
open Monotone_framework

(***********************************)
(* Dependency analysis & interface *)
(***********************************)

type reaching_definition = Dependence.location * label option

type node_dep_info =
  { predecessors: label Set.Poly.t
  ; parents: label Set.Poly.t
  ; reaching_defn_entry: reaching_definition Set.Poly.t
  ; reaching_defn_exit: reaching_definition Set.Poly.t
  ; accesses: Dependence.accesses
  ; loop_ids: label list
  ; meta: Location_span.t }

let reaching_defn_lookup (definitions : reaching_definition Set.Poly.t)
    (read : Dependence.location) : label Set.Poly.t =
  Set.Poly.fold definitions ~init:Set.Poly.empty
    ~f:(fun ((write : Dependence.location), writer_id) writer_labels ->
      match writer_id with
      | Some statement_id
        when Dependence.Location.same_var write read
             && Dependence.may_overlap write.path read.path ->
          Set.Poly.add statement_id writer_labels
      | _ -> writer_labels)

let writers_of_reads definitions ~blockers reads =
  List.fold_left reads ~init:Set.Poly.empty ~f:(fun writers read ->
      if Set.Poly.mem (VVar read.Dependence.variable) blockers then writers
      else Set.Poly.union writers (reaching_defn_lookup definitions read))

let node_immediate_dependencies
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    ?(blockers : vexpr Set.Poly.t = Set.Poly.empty) (label : label) :
    label Set.Poly.t =
  let _, info = LabelMap.find label statement_map in
  let writers =
    writers_of_reads info.reaching_defn_entry ~blockers info.accesses.reads
  in
  Set.Poly.union info.parents writers

(* This is doing an explicit graph traversal with edges defined by
   node_immediate_dependencies. *)
let rec node_dependencies_rec
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    ?(blockers : vexpr Set.Poly.t = Set.Poly.empty) (label : label)
    (visited : label Set.Poly.t) : label Set.Poly.t =
  if Set.Poly.mem label visited then visited
  else
    let visited' = Set.Poly.add label visited in
    let deps = node_immediate_dependencies statement_map ~blockers label in
    Set.Poly.fold deps ~init:visited' ~f:(node_dependencies_rec statement_map)

let node_dependencies
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    (label : label) : label Set.Poly.t =
  node_dependencies_rec statement_map label Set.Poly.empty

let node_vars_dependencies
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    ?(blockers : vexpr Set.Poly.t = Set.Poly.empty) (vars : vexpr Set.Poly.t)
    (label : label) : label Set.Poly.t =
  let _, info = LabelMap.find label statement_map in
  let reads =
    Set.Poly.fold vars ~init:[] ~f:(fun (VVar variable) selected_reads ->
        let matching_reads =
          List.filter info.accesses.reads ~f:(fun read ->
              String.equal read.Dependence.variable variable) in
        match matching_reads with
        | [] -> Dependence.{variable; path= []} :: selected_reads
        | _ -> matching_reads @ selected_reads) in
  let writers = writers_of_reads info.reaching_defn_entry ~blockers reads in
  Set.Poly.union info.parents writers
  |> Set.Poly.fold ~init:Set.Poly.empty
       ~f:(node_dependencies_rec statement_map ~blockers)

(* The strategy here is to write an update function on the whole dependency
   graph in terms of node_immediate_dependencies, and then to find a
   fixed-point. Since it's updating the dependencies for the whole graph at a
   time, it should be more efficient than doing a graph traversal for each
   node. *)
let all_node_dependencies
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t) :
    label Set.Poly.t LabelMap.t =
  let immediate_map =
    LabelMap.mapi statement_map ~f:(fun label _ ->
        node_immediate_dependencies statement_map label) in
  let step_node label m =
    let immediate = LabelMap.find label immediate_map in
    let updated =
      Set.Poly.union
        (Set.Poly.union_map immediate ~f:(fun label -> LabelMap.find label m))
        immediate in
    Set.Poly.remove label updated in
  let step_map m = LabelMap.mapi m ~f:(fun label _ -> step_node label m) in
  let map_equal = LabelMap.equal ~cmp:Set.Poly.equal in
  let rec step_until_fixed m =
    let m' = step_map m in
    if map_equal m m' then m else step_until_fixed m' in
  step_until_fixed immediate_map

let all_labels
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int) : int Set.Poly.t =
  let step set =
    Set.Poly.union set
      (Set.Poly.union_map set ~f:(fun l -> LabelMap.find l Flowgraph.successors))
  in
  let rec step_fix set =
    let next = step set in
    if Set.Poly.equal set next then set else step_fix next in
  step_fix Flowgraph.initials

let prog_rhs_variables
    (flowgraph_to_mir : Stmt.Located.Non_recursive.t LabelMap.t)
    (labels : int Set.Poly.t) : string Set.Poly.t =
  let label_vars label =
    Set.Poly.map
      ~f:(fun (VVar s, _) -> s)
      (stmt_rhs_var_set (LabelMap.find label flowgraph_to_mir).pattern) in
  Set.Poly.union_map labels ~f:label_vars

let stmt_uninitialized_variables (exceptions : string Set.Poly.t)
    (stmt : Stmt.Located.t) : (Location_span.t * string) Set.Poly.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_stmt ~flatten_loops:true stmt in
  let (module Flowgraph) = flowgraph in
  let labels = all_labels (module Flowgraph) in
  let all_variables = prog_rhs_variables flowgraph_to_mir labels in
  let initialized_vars_map =
    initialized_vars_mfp all_variables (module Flowgraph) flowgraph_to_mir in
  let uninitialized =
    LabelMap.fold initialized_vars_map ~init:Set.Poly.empty
      ~f:(fun ~key:label ~data:inits acc ->
        let stmt = LabelMap.find label flowgraph_to_mir in
        let rhs =
          Set.Poly.map
            ~f:(fun (VVar s, Expr.Typed.Meta.{loc; _}) -> (loc, s))
            (stmt_rhs_var_set stmt.pattern) in
        let uninitialized (_, var) = not (Set.Poly.mem var inits.entry) in
        let uninitialized_set = Set.Poly.filter ~f:uninitialized rhs in
        Set.Poly.union acc uninitialized_set) in
  Set.Poly.filter uninitialized ~f:(fun (_, v) ->
      not (Set.Poly.mem v exceptions))

let mir_uninitialized_variables (mir : Program.Typed.t) :
    (Location_span.t * string) Set.Poly.t =
  let flag_variables = List.map ~f:Flag_vars.to_string Flag_vars.enumerate in
  let function_names = function_names mir in
  let data_vars = data_set ~exclude_transformed:true mir in
  let globals =
    Set.Poly.union function_names (Set.Poly.of_list flag_variables) in
  let parameters =
    Set.Poly.of_list
      (List.map ~f:fst3
         (List.filter
            ~f:(fun (_, _, Program.{out_block; _}) -> out_block = Parameters)
            mir.output_vars)) in
  let globals_data = Set.Poly.union globals data_vars in
  let check_against_data b =
    stmt_uninitialized_variables globals_data
      {pattern= SList b; meta= Location_span.empty} in
  let check_against_data_params b =
    let globals_data_prep = Set.Poly.union globals_data parameters in
    stmt_uninitialized_variables globals_data_prep
      (* prepend prepare_data to detect bad transformed data usages *)
      {pattern= SList (mir.prepare_data @ b); meta= Location_span.empty} in
  Set.Poly.union_list
    [ (* prepare_data scope: data *)
      check_against_data mir.prepare_data
      (* log_prob scope: data, prep declarations *)
    ; check_against_data_params mir.log_prob
    ; check_against_data_params mir.reverse_mode_log_prob
      (* gen quant scope: data, prep declarations *)
    ; check_against_data_params mir.generate_quantities
      (* functions scope: arguments *)
    ; Set.Poly.union_list
        (List.map mir.functions_block ~f:(fun Program.{fdbody; fdargs; _} ->
             let arg_vars =
               Set.Poly.of_list
                 (List.map fdargs ~f:(fun (_, arg_name, _) -> arg_name)) in
             Option.value_map fdbody ~default:Set.Poly.empty ~f:(fun fdbody ->
                 stmt_uninitialized_variables
                   (Set.Poly.union arg_vars globals)
                   fdbody))) ]

let build_dep_info_map (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t =
  let statement_map =
    build_statement_map
      (fun Stmt.{pattern; _} -> pattern)
      (fun Stmt.{meta; _} -> meta)
      stmt in
  let exits, preds, controls_by_statement = build_cf_graphs statement_map in
  let accesses_by_statement =
    LabelMap.map statement_map ~f:(fun (pattern, _) ->
        Dependence.collect_accesses pattern) in
  let written_vars =
    LabelMap.fold accesses_by_statement ~init:Set.Poly.empty
      ~f:(fun ~key:_ ~data:statement_accesses collected_variables ->
        List.fold_left statement_accesses.Dependence.writes
          ~init:collected_variables ~f:(fun variables write_location ->
            Set.Poly.add write_location.Dependence.variable variables)) in
  let inverse_flowgraph =
    (module struct
      type labels = int

      let initials = exits
      let successors = preds
    end : FLOWGRAPH
      with type labels = int) in
  let rd_map =
    reaching_definitions_mfp mir
      (Monotone_framework.reverse inverse_flowgraph)
      accesses_by_statement written_vars in
  LabelMap.mapi statement_map ~f:(fun label (stmt, meta) ->
      let definitions = LabelMap.find label rd_map in
      let statement_controls = LabelMap.find label controls_by_statement in
      ( stmt
      , { predecessors= LabelMap.find label preds
        ; parents= control_parent_ids statement_controls
        ; reaching_defn_entry= definitions.entry
        ; reaching_defn_exit= definitions.exit
        ; accesses= LabelMap.find label accesses_by_statement
        ; loop_ids= enclosing_loop_ids label stmt statement_controls
        ; meta } ))

let log_prob_build_dep_info_map (mir : Program.Typed.t) :
    ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t =
  let log_prob_stmt =
    Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dep_info_map mir log_prob_stmt

let log_prob_dependency_graph (mir : Program.Typed.t) :
    label Set.Poly.t LabelMap.t =
  let dep_info_map = log_prob_build_dep_info_map mir in
  all_node_dependencies dep_info_map

let%test_module "writers_of_reads" =
  (module struct
    let print_writers ~blocked source =
      let open Frontend in
      let ast = Parse.parse_program (`Code source) |> fst |> Result.get_ok in
      let typed = Typechecker.check_program ast |> Result.get_ok |> fst in
      let statements =
        log_prob_build_dep_info_map (Ast_to_Mir.trans_prog "" typed) in
      let lines = String.split_lines source |> Array.of_list in
      let describe line =
        Fmt.str "line %d: %s" line (String.trim lines.(line - 1)) in
      LabelMap.iter statements ~f:(fun ~key:_ ~data:(pattern, info) ->
          match pattern with
          | Stmt.Pattern.NRFunApp (CompilerInternal FnPrint, _) ->
              Fmt.pr "%s@." (describe info.meta.begin_loc.line_num);
              List.iter blocked ~f:(fun variables ->
                  let blockers =
                    Set.Poly.of_list (List.map variables ~f:(fun v -> VVar v))
                  in
                  let writers =
                    writers_of_reads info.reaching_defn_entry ~blockers
                      info.accesses.reads in
                  Fmt.pr "  blocked [%s]" (String.concat ~sep:", " variables);
                  if Set.Poly.is_empty writers then Fmt.pr " -> no writers@."
                  else (
                    Fmt.pr " -> writers@.";
                    Set.Poly.iter writers ~f:(fun id ->
                        let _, writer = LabelMap.find id statements in
                        Fmt.pr "    %s@."
                          (describe writer.meta.begin_loc.line_num))))
          | _ -> ())

    let%expect_test
        "writers_of_reads excludes blocked variables from the writers of \
         print(x, y)" =
      print_writers
        ~blocked:[[]; ["x"]; ["x"; "y"]]
        {|model {
            int x = 3;
            int y = 5;
            print(x, y);
          }|};
      [%expect
        {|
        line 4: print(x, y);
          blocked [] -> writers
            line 2: int x = 3;
            line 3: int y = 5;
          blocked [x] -> writers
            line 3: int y = 5;
          blocked [x, y] -> no writers
        |}]
  end)
