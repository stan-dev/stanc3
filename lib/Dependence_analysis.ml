open Core_kernel
open Mir
open Dataflow_types
open Mir_utils
open Dataflow_utils
open Monotone_framework_sigs
open Monotone_framework

(***********************************)
(* Dependency analysis & interface *)
(***********************************)

type node_dep_info =
  { predecessors: label Set.Poly.t
  ; parents: label Set.Poly.t
  ; reaching_defn_entry: reaching_defn Set.Poly.t
  ; reaching_defn_exit: reaching_defn Set.Poly.t }

(**
   Find all of the reaching definitions of a variable in an RD set
*)
let reaching_defn_lookup (rds : reaching_defn Set.Poly.t) (var : vexpr) :
    label Set.Poly.t =
  Set.Poly.map (Set.Poly.filter rds ~f:(fun (var', _) -> var' = var)) ~f:snd

let node_immediate_dependencies
    (statement_map :
      (label, (expr_typed_located, label) statement * node_dep_info) Map.Poly.t)
    (label : label) : label Set.Poly.t =
  let stmt, info = Map.Poly.find_exn statement_map label in
  let rhs_set = stmt_rhs_var_set stmt in
  let rhs_deps =
    union_map rhs_set ~f:(reaching_defn_lookup info.reaching_defn_entry)
  in
  Set.Poly.union info.parents rhs_deps

(*
   This is doing an explicit graph traversal with edges defined by
   node_immediate_dependencies.
*)
let node_dependencies
    (statement_map :
      (label, (expr_typed_located, label) statement * node_dep_info) Map.Poly.t)
    (label : label) : label Set.Poly.t =
  let rec label_dependencies_rec visited label =
    if Set.Poly.mem visited label then Set.Poly.empty
    else
      let deps = node_immediate_dependencies statement_map label in
      let visited' = Set.Poly.add visited label in
      Set.Poly.fold deps ~init:visited' ~f:label_dependencies_rec
  in
  label_dependencies_rec Set.Poly.empty label

(*
   The strategy here is to write an update function on the whole dependency graph in terms
   of node_immediate_dependencies, and then to find a fixed-point. Since it's updating the
   dependencies for the whole graph at a time, it should be more efficient than doing a
   graph traversal for each node.
*)
let all_node_dependencies
    (statement_map :
      (label, (expr_typed_located, label) statement * node_dep_info) Map.Poly.t)
    : (label, label Set.Poly.t) Map.Poly.t =
  let immediate_map =
    Map.mapi statement_map ~f:(fun ~key:label ~data:_ ->
        node_immediate_dependencies statement_map label )
  in
  let step_node label m =
    let immediate = Map.find_exn immediate_map label in
    let updated =
      Set.union
        (union_map immediate ~f:(fun label -> Map.find_exn m label))
        immediate
    in
    Set.remove updated label
  in
  let step_map m =
    Map.mapi m ~f:(fun ~key:label ~data:_ -> step_node label m)
  in
  let map_equal = Map.Poly.equal Set.Poly.equal in
  let rec step_until_fixed m =
    let m' = step_map m in
    if map_equal m m' then m else step_until_fixed m'
  in
  step_until_fixed immediate_map

let reaching_defns
    (statement_map :
      (label, (expr_typed_located, label) statement * 'm) Map.Poly.t) :
    (label, reaching_defn Set.Poly.t entry_exit) Map.Poly.t =
  Map.Poly.mapi statement_map ~f:(fun ~key:_ ~data:_ ->
      (* TODO: figure out how to call RDs *)
      {entry= Set.Poly.empty; exit= Set.Poly.empty} )

let build_dep_info_map
    (statement_map :
      (label, (expr_typed_located, label) statement * 'm) Map.Poly.t) :
    (label, (expr_typed_located, label) statement * node_dep_info) Map.Poly.t =
  let _, preds, parents = build_cf_graphs statement_map in
  let rd_map = reaching_defns statement_map in
  Map.Poly.mapi statement_map ~f:(fun ~key:label ~data:(stmt, _) ->
      let rds = Map.find_exn rd_map label in
      ( stmt
      , { predecessors= Map.find_exn preds label
        ; parents= Map.find_exn parents label
        ; reaching_defn_entry= rds.entry
        ; reaching_defn_exit= rds.exit } ) )

let mir_reaching_definitions (mir : typed_prog) (stmt : stmt_loc) :
    (label, reaching_defn Set.Poly.t entry_exit) Map.Poly.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_stmt stmt
  in
  let (module Flowgraph) = flowgraph in
  let rd_map =
    reaching_definitions_mfp mir (module Flowgraph) flowgraph_to_mir
  in
  let to_rd_set set =
    Set.Poly.map set ~f:(fun (s, label_opt) ->
        (VVar s, Option.value label_opt ~default:0) )
  in
  Map.Poly.map rd_map ~f:(fun {entry; exit} ->
      {entry= to_rd_set entry; exit= to_rd_set exit} )

let log_prob_build_dep_info_map (mir : Mir.typed_prog) :
    (label, (expr_typed_located, label) statement * node_dep_info) Map.Poly.t =
  let log_prob_stmt = {sloc= Mir.no_span; stmt= SList mir.log_prob} in
  let statement_map =
    build_statement_map (fun s -> s.stmt) (fun s -> s.sloc) log_prob_stmt
  in
  let _, preds, parents = build_cf_graphs statement_map in
  let rd_map = mir_reaching_definitions mir log_prob_stmt in
  Map.Poly.mapi statement_map ~f:(fun ~key:label ~data:(stmt, _) ->
      let rds = Map.find_exn rd_map label in
      ( stmt
      , { predecessors= Map.find_exn preds label
        ; parents= Map.find_exn parents label
        ; reaching_defn_entry= rds.entry
        ; reaching_defn_exit= rds.exit } ) )

let stmt_map_dependency_graph
    (statement_map :
      (label, (expr_typed_located, label) statement * 'm) Map.Poly.t) :
    (label, label Set.Poly.t) Map.Poly.t =
  let dep_info_map = build_dep_info_map statement_map in
  all_node_dependencies dep_info_map

let log_prob_dependency_graph (mir : Mir.typed_prog) :
    (label, label Set.Poly.t) Map.Poly.t =
  let dep_info_map = log_prob_build_dep_info_map mir in
  all_node_dependencies dep_info_map

let example1_program =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        model
        {                                // 1
          int i                          // 2: 3
              = 0;                       //    4
          if (i < 0)                     // 5
          {                              // 6
            print(i);                    // 7
          } else
          {                              // 8
            for (j in 1:10)              // 9
            {                            // 10
              if (j > 9)                 // 11
              {                          // 12
                break;                   // 13
              }
              if (j > 8 && i < -1)       // 14
              {                          // 15
                continue;                // 16
              }
              if (j > 5)                 // 17
              {                          // 18
                continue;                // 19
              } else
              {                          // 20
                print("Badger", i + j);  // 21
              }
              print("Fin");              // 22
            }
          }
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)

let%expect_test "Dependency graph example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps = log_prob_dependency_graph example1_program in
  print_s [%sexp (deps : (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 ()) (5 (4)) (6 (4 5)) (7 (4 5)) (8 (4 5))
       (9 (4 5 11 13)) (10 (4 5 9 11 13)) (11 (4 5 9 13)) (12 (4 5 9 11 13))
       (13 (4 5 9 11)) (14 (4 5 9 11 13)) (15 (4 5 9 11 13 14))
       (16 (4 5 9 11 13 14)) (17 (4 5 9 11 13 14 16)) (18 (4 5 9 11 13 14 16 17))
       (19 (4 5 9 11 13 14 16 17)) (20 (4 5 9 11 13 14 16 17))
       (21 (4 5 9 11 13 14 16 17)) (22 (4 5 9 11 13 14 16 17 19)))
    |}]
