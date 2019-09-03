open Core_kernel
open Frontend
open Analysis_and_optimization.Dependence_analysis
(*open Analysis_and_optimization.Monotone_framework_sigs*)
open Analysis_and_optimization.Monotone_framework
open Middle
open Analysis_and_optimization.Dataflow_types
open Analysis_and_optimization.Dataflow_utils

let semantic_check_program ast =
  Option.value_exn
    (Result.ok
       (Semantic_check.semantic_check_program
          (Option.value_exn (Result.ok ast))))

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
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

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

let%expect_test "Reaching defns example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps =
    Map.Poly.map (log_prob_build_dep_info_map example1_program)
      ~f:(fun (_, x) ->
        ( reaching_defn_lookup x.reaching_defn_entry (VVar "j")
        , reaching_defn_lookup x.reaching_defn_exit (VVar "j") ) )
  in
  print_s
    [%sexp (deps : (label, label Set.Poly.t * label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((1 (() ())) (2 (() ())) (3 (() ())) (4 (() ())) (5 (() ())) (6 (() ()))
       (7 (() ())) (8 (() ())) (9 ((9) (9))) (10 ((9) (9))) (11 ((9) (9)))
       (12 ((9) (9))) (13 ((9) (9))) (14 ((9) (9))) (15 ((9) (9))) (16 ((9) (9)))
       (17 ((9) (9))) (18 ((9) (9))) (19 ((9) (9))) (20 ((9) (9))) (21 ((9) (9)))
       (22 ((9) (9))))
    |}]

let%expect_test "Reaching defns example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps =
    Map.Poly.map (log_prob_build_dep_info_map example1_program)
      ~f:(fun (_, x) -> (x.reaching_defn_entry, x.reaching_defn_exit) )
  in
  print_s
    [%sexp
      ( deps
        : ( label
          , reaching_defn Set.Poly.t * reaching_defn Set.Poly.t )
          Map.Poly.t )] ;
  [%expect
    {|
      ((1 (() ())) (2 (() ())) (3 (() (((VVar i) 3))))
       (4 ((((VVar i) 3)) (((VVar i) 4)))) (5 ((((VVar i) 4)) (((VVar i) 4))))
       (6 ((((VVar i) 4)) (((VVar i) 4)))) (7 ((((VVar i) 4)) (((VVar i) 4))))
       (8 ((((VVar i) 4)) (((VVar i) 4))))
       (9 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (10 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (11 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (12 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (13 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (14 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (15 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (16 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (17 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (18 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (19 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (20 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (21 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (22 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9)))))
    |}]

let%expect_test "Variable dependency example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps =
    node_vars_dependencies
      (log_prob_build_dep_info_map example1_program)
      (Set.Poly.singleton (VVar "j"))
      17
  in
  print_s [%sexp (deps : label Set.Poly.t)] ;
  [%expect {|
      (4 5 9 11 13 14 16)
    |}]

let uninitialized_var_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        parameters {
          real x;
        }
        model
        {
          int i;
          int z = 0;
          print(i);
          print(z);
          print(x);
          if (z == 1) {
            i = 1;
          } else {}
          print(i);
          if (z == 2) {
            i = 1;
          } else {
            i = 2;
          }
          print(i);
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let uninitialized_variables (mir : typed_prog) (stmt : stmt_loc) =
    (*: (label * string) Set.Poly.t =*)
  let flowgraph, flowgraph_to_mir =
    forward_flowgraph_of_stmt stmt
  in
  let (module Flowgraph) = flowgraph in
  let initialized_vars_map =
    initialized_vars_mfp (module Flowgraph) flowgraph_to_mir
  in
  initialized_vars_map
(*
  let uninitialized =
    Map.Poly.fold initialized_vars_map ~init:Set.Poly.empty ~f:(fun ~key:label ~data:inits acc ->
      let rhs = Set.Poly.map ~f:(fun (VVar s) -> (label, s)) (stmt_rhs_var_set (Map.Poly.find_exn flowgraph_to_mir label).stmtn) in
      let uninitialized (_, var) = not (Set.Poly.mem inits.entry var) in
      let uninitialized_set = Set.Poly.filter ~f:uninitialized rhs in
      Set.Poly.union acc uninitialized_set)
  in
  let input_var_names = Set.Poly.of_list (List.map mir.input_vars ~f:(fun (v, _) -> v)) in
  let output_var_names = Set.Poly.of_list (List.map mir.output_vars ~f:(fun (v, _) -> v)) in
  let global = Set.Poly.union input_var_names output_var_names in
  Set.Poly.filter uninitialized ~f:(fun (_, v) -> not (Set.Poly.mem global v))
*)

let%expect_test "Uninitialized variables example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let prog = uninitialized_var_example in
  let s = {stmt= SList prog.log_prob; smeta= no_span} in
  let deps = mir_uninitialized_variables prog s in
  print_s [%sexp (deps : (label * string) Set.Poly.t)] ;
  [%expect
    {|
      gen i 20
      no gen 19
      gen i 18
      no gen 17
      no gen 16
      no gen 16
      no gen 15
      no gen 14
      gen i 13
      no gen 15
      no gen 12
      no gen 11
      no gen 11
      no gen 10
      no gen 9
      no gen 8
      gen z 7
      no gen 8
      no gen 6
      no gen 5
      no gen 4
      gen x 3
      no gen 4
      no gen 2
      no gen 1
      no gen 1
      no gen 2
      gen x 3
      no gen 4
      no gen 5
      no gen 6
      gen z 7
      no gen 8
      no gen 9
      no gen 10
      no gen 11
      no gen 12
      gen i 13
      no gen 14
      no gen 15
      no gen 16
      no gen 17
      gen i 18
      no gen 19
      gen i 20
      no gen 21
      ((8 i) (9 z) (11 z) (15 i) (16 z) (21 i))
    |}]

let%expect_test "Uninitialized variables mfp" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let prog = uninitialized_var_example in
  let s = {stmt= SList prog.log_prob; smeta= no_span} in
  let deps = uninitialized_variables prog s in
  let deps_ = Map.Poly.map deps ~f:(fun ee -> ee.entry) in
  print_s [%sexp (deps_ : (int, string Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      gen i 20
      no gen 19
      gen i 18
      no gen 17
      no gen 16
      no gen 16
      no gen 15
      no gen 14
      gen i 13
      no gen 15
      no gen 12
      no gen 11
      no gen 11
      no gen 10
      no gen 9
      no gen 8
      gen z 7
      no gen 8
      no gen 6
      no gen 5
      no gen 4
      gen x 3
      no gen 4
      no gen 2
      no gen 1
      no gen 1
      no gen 2
      gen x 3
      no gen 4
      no gen 5
      no gen 6
      gen z 7
      no gen 8
      no gen 9
      no gen 10
      no gen 11
      no gen 12
      gen i 13
      no gen 14
      no gen 15
      no gen 16
      no gen 17
      gen i 18
      no gen 19
      gen i 20
      no gen 21
      ((1 ()) (2 ()) (3 ()) (4 ()) (5 ()) (6 ()) (7 ()) (8 ()) (9 ()) (10 ())
       (11 ()) (12 ()) (13 ()) (14 ()) (15 ()) (16 ()) (17 ()) (18 ()) (19 ())
       (20 ()) (21 ()))
    |}]


let example4_statement_map =
  let s = {stmt= SList uninitialized_var_example.log_prob; smeta= no_span} in
  build_statement_map (fun s -> s.stmt) (fun s -> s.smeta) s

let%expect_test "Statement label map example 4" =
  print_s
    [%sexp
      ( example4_statement_map
        : ( label
          , (expr_typed_located, label) statement * Middle.location_span )
          Map.Poly.t )] ;
  [%expect
    {|
      ((1
        ((SList (2 3 4))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (2
        ((Decl (decl_adtype AutoDiffable) (decl_id x) (decl_type (Sized SReal)))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 3) (col_num 17) (included_from ()))))))
       (3
        ((Assignment (x UReal ())
          (FunApp CompilerInternal FnReadParam__ ((Lit Str x) (Lit Str scalar))))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 3) (col_num 17) (included_from ()))))))
       (4
        ((Block (5 6 7 8 9 10 11 15 16 21))
         ((begin_loc
           ((filename string) (line_num 7) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 7) (col_num 16) (included_from ()))))))
       (5
        ((Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type (Sized SInt)))
         ((begin_loc
           ((filename string) (line_num 7) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 7) (col_num 16) (included_from ()))))))
       (6
        ((Decl (decl_adtype AutoDiffable) (decl_id z) (decl_type (Sized SInt)))
         ((begin_loc
           ((filename string) (line_num 8) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 8) (col_num 20) (included_from ()))))))
       (7
        ((Assignment (z UInt ()) (Lit Int 0))
         ((begin_loc
           ((filename string) (line_num 8) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 8) (col_num 20) (included_from ()))))))
       (8
        ((NRFunApp CompilerInternal FnPrint__ ((Var i)))
         ((begin_loc
           ((filename string) (line_num 9) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 9) (col_num 19) (included_from ()))))))
       (9
        ((NRFunApp CompilerInternal FnPrint__ ((Var z)))
         ((begin_loc
           ((filename string) (line_num 10) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 10) (col_num 19) (included_from ()))))))
       (10
        ((NRFunApp CompilerInternal FnPrint__ ((Var x)))
         ((begin_loc
           ((filename string) (line_num 11) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 11) (col_num 19) (included_from ()))))))
       (11
        ((IfElse (FunApp StanLib Equals__ ((Var z) (Lit Int 1))) 12 (14))
         ((begin_loc
           ((filename string) (line_num 12) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 14) (col_num 19) (included_from ()))))))
       (12
        ((Block (13))
         ((begin_loc
           ((filename string) (line_num 12) (col_num 22) (included_from ())))
          (end_loc
           ((filename string) (line_num 14) (col_num 11) (included_from ()))))))
       (13
        ((Assignment (i UInt ()) (Lit Int 1))
         ((begin_loc
           ((filename string) (line_num 13) (col_num 12) (included_from ())))
          (end_loc
           ((filename string) (line_num 13) (col_num 18) (included_from ()))))))
       (14
        ((Block ())
         ((begin_loc
           ((filename string) (line_num 14) (col_num 17) (included_from ())))
          (end_loc
           ((filename string) (line_num 14) (col_num 19) (included_from ()))))))
       (15
        ((NRFunApp CompilerInternal FnPrint__ ((Var i)))
         ((begin_loc
           ((filename string) (line_num 15) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 15) (col_num 19) (included_from ()))))))
       (16
        ((IfElse (FunApp StanLib Equals__ ((Var z) (Lit Int 2))) 17 (19))
         ((begin_loc
           ((filename string) (line_num 16) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 20) (col_num 11) (included_from ()))))))
       (17
        ((Block (18))
         ((begin_loc
           ((filename string) (line_num 16) (col_num 22) (included_from ())))
          (end_loc
           ((filename string) (line_num 18) (col_num 11) (included_from ()))))))
       (18
        ((Assignment (i UInt ()) (Lit Int 1))
         ((begin_loc
           ((filename string) (line_num 17) (col_num 12) (included_from ())))
          (end_loc
           ((filename string) (line_num 17) (col_num 18) (included_from ()))))))
       (19
        ((Block (20))
         ((begin_loc
           ((filename string) (line_num 18) (col_num 17) (included_from ())))
          (end_loc
           ((filename string) (line_num 20) (col_num 11) (included_from ()))))))
       (20
        ((Assignment (i UInt ()) (Lit Int 2))
         ((begin_loc
           ((filename string) (line_num 19) (col_num 12) (included_from ())))
          (end_loc
           ((filename string) (line_num 19) (col_num 18) (included_from ()))))))
       (21
        ((NRFunApp CompilerInternal FnPrint__ ((Var i)))
         ((begin_loc
           ((filename string) (line_num 21) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 21) (col_num 19) (included_from ())))))))
    |}]
