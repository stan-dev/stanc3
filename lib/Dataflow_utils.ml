open Core_kernel
open Mir
open Dataflow_types
open Mir_utils

(** Union maps, preserving the left element in a collision *)
let union_maps_left (m1 : ('a, 'b) Map.Poly.t) (m2 : ('a, 'b) Map.Poly.t) :
    ('a, 'b) Map.Poly.t =
  let f ~key:_ opt =
    match opt with
    | `Left v -> Some v
    | `Right v -> Some v
    | `Both (v1, _) -> Some v1
  in
  Map.Poly.merge m1 m2 ~f

(**
   Like a forward traversal, but branches accumulate two different states that are
   recombined with join.
*)
let branching_traverse_statement (stmt : ('e, 'a) statement)
    ~(join : 'f -> 'f -> 'f) ~init:(state : 'f) ~(f : 'f -> 'a -> 'f * 'c) :
    'f * ('e, 'c) statement =
  match stmt with
  | IfElse (pred, then_s, else_s_opt) ->
      let s', c = f state then_s in
      Option.value_map else_s_opt
        ~default:(s', IfElse (pred, c, None))
        ~f:(fun else_s ->
          let s'', c' = f state else_s in
          (join s' s'', IfElse (pred, c, Some c')) )
  | _ as s -> fwd_traverse_statement s ~init:state ~f

(** Like a branching traversal, but doesn't return an updated statement.
*)
let branching_fold_statement (stmt : ('e, 'a) statement)
    ~(join : 'f -> 'f -> 'f) ~init:(state : 'f) ~(f : 'f -> 'a -> 'f) : 'f =
  fst
    (branching_traverse_statement stmt ~join ~init:state ~f:(fun s a ->
         (f s a, ()) ))

(**
   See interface file
*)
let build_statement_map (extract : 's -> ('e, 's) statement)
    (metadata : 's -> 'm) (stmt : 's) :
    (label, ('e, label) statement * 'm) Map.Poly.t =
  let rec build_statement_map_rec (next_label : label)
      (map : (label, ('e, label) statement * 'm) Map.Poly.t) (stmt : 's) :
      (label * (label, ('e, label) statement * 'm) Map.Poly.t) * label =
    let this_label = next_label in
    let next_label' = next_label + 1 in
    let f (label, map) stmt = build_statement_map_rec label map stmt in
    let (next_label'', map), built =
      fwd_traverse_statement (extract stmt) ~init:(next_label', map) ~f
    in
    ( ( next_label''
      , union_maps_left map
          (Map.Poly.singleton this_label (built, metadata stmt)) )
    , this_label )
  in
  let (_, map), _ = build_statement_map_rec 1 Map.Poly.empty stmt in
  map

(* TODO: this currently does not seem to be labelling inside function bodies.
   Could we also do that? *)

(**
   See interface file
*)
let rec build_recursive_statement (rebuild : ('e, 's) statement -> 'm -> 's)
    (statement_map : (label, ('e, label) statement * 'm) Map.Poly.t)
    (label : label) : 's =
  let stmt_ints, meta = Map.Poly.find_exn statement_map label in
  let build_stmt = build_recursive_statement rebuild statement_map in
  let stmt = map_statement (fun x -> x) build_stmt stmt_ints in
  rebuild stmt meta

(** Represents the state required to build control flow information during an MIR
    traversal, where
     * breaks is the set of Breaks seen since the beginning of their loop
     * continues is the set of Continues seen since the beginning of their loop
     * returns is the set of Returns seen since the beginning of their function definition
     * exits is the set of nodes that could have been the last one to execute before this
       node
*)
type cf_state =
  { breaks: label Set.Poly.t
  ; continues: label Set.Poly.t
  ; returns: label Set.Poly.t
  ; exits: label Set.Poly.t }

(** Represents the control flow information at each node in the control graph, where
     * predecessors points to the nodes which could have executed before this node
     * parents points to the adjacent nodes which directly influence the execution of this
       node
*)
type cf_edges = {predecessors: label Set.Poly.t; parents: label Set.Poly.t}

(** Join the state of a controlflow traversal across different branches of execution such
    as over if/else branch. *)
let join_cf_states (state1 : cf_state) (state2 : cf_state) : cf_state =
  { breaks= Set.Poly.union state1.breaks state2.breaks
  ; continues= Set.Poly.union state1.continues state2.continues
  ; returns= Set.Poly.union state1.returns state2.returns
  ; exits= Set.Poly.union state1.exits state2.exits }

(** Check if the statement controls the execution of its substatements. *)
let is_ctrl_flow (stmt : ('e, 's) statement) : bool =
  match stmt with
  | IfElse _ -> true
  | While _ -> true
  | For _ -> true
  | FunDef _ -> true
  | _ -> false

(**
   Simultaneously builds the controlflow parent graph, the predecessor graph and the exit
   set of a statement. It's advantageous to build them together because they both rely on
   some of the same Break, Continue and Return bookkeeping.
*)
let build_cf_graphs
    (statement_map :
      (label, (expr_typed_located, label) statement * 'm) Map.Poly.t) :
    label Set.Poly.t
    * (label, label Set.Poly.t) Map.Poly.t
    * (label, label Set.Poly.t) Map.Poly.t =
  let rec build_cf_graph_rec (cf_parent : label option)
      ((in_state, in_map) : cf_state * (label, cf_edges) Map.Poly.t)
      (label : label) : cf_state * (label, cf_edges) Map.Poly.t =
    let stmt, _ = Map.Poly.find_exn statement_map label in
    (* Only control flow nodes should pass themselves down as parents *)
    let child_cf = if is_ctrl_flow stmt then Some label else cf_parent in
    let join (state1, map1) (state2, map2) =
      (join_cf_states state1 state2, union_maps_left map1 map2)
    in
    (* The accumulated state after traversing substatements *)
    let substmt_state_unlooped, substmt_map_unlooped =
      branching_fold_statement stmt ~join
        ~init:({in_state with exits= Set.Poly.singleton label}, in_map)
        ~f:(build_cf_graph_rec child_cf)
    in
    (* If the statement is a loop, we need to include the loop body exits predecessors
         of the loop body *)
    let substmt_state, substmt_map, predecessors =
      let looped_state passthrough_possible =
        (* Loop statements are preceded by:
           1. The statements that come before the loop
           2. The natural exit points of the loop body
           3. Continue statements in the loop body
           This comment mangling brought to you by the autoformatter
        *)
        let loop_predecessors =
          Set.Poly.union_list
            [ (*1*) in_state.exits; (*2*) substmt_state_unlooped.exits
            ; (*3*)
              Set.Poly.diff substmt_state_unlooped.continues in_state.continues
            ]
        in
        (* Loop exits are:
           1. The loop node itself, since the last action of a typical loop execution is
              to check if there are any iterations remaining
           2. The statements that come before the loop, in case the loop didn't execute
           3. Break statements in the loop body, since broken loops don't execute the
              loop statement
        *)
        let loop_exits =
          Set.Poly.union_list
            [ (*1*) Set.Poly.singleton label
            ; (*2*)
              (if passthrough_possible then in_state.exits else Set.Poly.empty)
            ; (*3*) Set.Poly.diff substmt_state_unlooped.breaks in_state.breaks
            ]
        in
        ( {substmt_state_unlooped with exits= loop_exits}
        , substmt_map_unlooped
        , loop_predecessors )
      in
      match stmt with
      | For
          { lower= {texpr= Lit (Int, l_str); _}
          ; upper= {texpr= Lit (Int, u_str); _}; _ } ->
          (* TODO: Is it safe to use int_of_string here? *)
          let l = int_of_string l_str in
          let u = int_of_string u_str in
          looped_state (l > u)
      | For _ -> looped_state true
      | While ({texpr= Lit (Int, cond_str); _}, _) ->
          looped_state (int_of_string cond_str = 0)
      | While _ -> looped_state true
      | _ -> (substmt_state_unlooped, substmt_map_unlooped, in_state.exits)
    in
    (* Some statements interact with the break/return/continue states
       E.g., loops nullify breaks and continues in their body, but are still affected by
       breaks and input continues*)
    let breaks_out, returns_out, continues_out, extra_cf_deps =
      match stmt with
      | Break ->
          ( Set.Poly.add substmt_state.breaks label
          , substmt_state.returns
          , substmt_state.continues
          , Set.Poly.empty )
      | Return _ ->
          ( substmt_state.breaks
          , Set.Poly.add substmt_state.returns label
          , substmt_state.continues
          , Set.Poly.empty )
      | Continue ->
          ( substmt_state.breaks
          , substmt_state.returns
          , Set.Poly.add substmt_state.continues label
          , Set.Poly.empty )
      | While _ | For _ ->
          ( in_state.breaks
          , substmt_state.returns
          , in_state.continues
          , Set.Poly.union substmt_state.breaks substmt_state.returns )
      | _ ->
          ( substmt_state.breaks
          , substmt_state.returns
          , substmt_state.continues
          , Set.Poly.empty )
    in
    let cf_parents =
      Set.Poly.union_list
        [ Option.value_map cf_parent ~default:Set.Poly.empty
            ~f:Set.Poly.singleton
        ; in_state.returns; in_state.continues; extra_cf_deps ]
    in
    ( { breaks= breaks_out
      ; continues= continues_out
      ; returns= returns_out
      ; exits= substmt_state.exits }
    , Map.Poly.add_exn substmt_map ~key:label
        ~data:{parents= cf_parents; predecessors} )
  in
  let state, edges =
    build_cf_graph_rec None
      ( { breaks= Set.Poly.empty
        ; continues= Set.Poly.empty
        ; returns= Set.Poly.empty
        ; exits= Set.Poly.empty }
      , Map.Poly.empty )
      1
  in
  ( state.exits
  , Map.Poly.map edges ~f:(fun e -> e.predecessors)
  , Map.Poly.map edges ~f:(fun e -> e.parents) )

(** See interface file *)
let build_cf_graph
    (statement_map :
      (label, (expr_typed_located, label) statement * 'm) Map.Poly.t) :
    (label, label Set.Poly.t) Map.Poly.t =
  let _, _, cf_graph = build_cf_graphs statement_map in
  cf_graph

(** See interface file *)
let build_predecessor_graph
    (statement_map :
      (label, (expr_typed_located, label) statement * 'm) Map.Poly.t) :
    label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t =
  let exits, pred_graph, _ = build_cf_graphs statement_map in
  (exits, pred_graph)

(***********************************)
(* Tests                           *)
(***********************************)

let%expect_test "Loop passthrough" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        model {
          if (1) {
            if (1) {
              for (j in 1:2) {
                print("Badger", j);
              }
            } else {
              for (j in 2:1) {
                print("Badger", j);
              }
            }
          } else {
            if (1) {
              while (1) {
                print("Badger");
              }
            } else {
              while (0) {
                print("Badger");
              }
            }
          }
        }
      |}
  in
  let mir =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let block = Mir.Block mir.log_prob in
  let statement_map =
    build_statement_map
      (fun s -> s.stmt)
      (fun s -> s.sloc)
      {stmt= block; sloc= Mir.no_span}
  in
  let exits, _ = build_predecessor_graph statement_map in
  print_s [%sexp (exits : label Set.Poly.t)] ;
  [%expect {|
      (6 9 10 16 19 20)
    |}]

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
  let mir =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let block = Mir.Block mir.log_prob in
  {stmt= block; sloc= Mir.no_span}

let example1_statement_map =
  build_statement_map (fun s -> s.stmt) (fun s -> s.sloc) example1_program

let%expect_test "Statement label map example" =
  print_s
    [%sexp
      ( Map.Poly.map example1_statement_map ~f:fst
        : (label, (expr_typed_located, label) statement) Map.Poly.t )] ;
  [%expect
    {|
      ((1 (Block (2 5))) (2 (SList (3 4)))
       (3 (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type UInt)))
       (4
        (Assignment
         ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var i))
          (texpr_adlevel DataOnly))
         ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 0))
          (texpr_adlevel DataOnly))))
       (5
        (IfElse
         ((texpr_type UInt) (texpr_loc <opaque>)
          (texpr
           (FunApp Less__
            (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var i))
              (texpr_adlevel DataOnly))
             ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 0))
              (texpr_adlevel DataOnly)))))
          (texpr_adlevel DataOnly))
         6 (8)))
       (6 (Block (7)))
       (7
        (NRFunApp print
         (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var i))
           (texpr_adlevel DataOnly)))))
       (8 (Block (9)))
       (9
        (For (loopvar j)
         (lower
          ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 1))
           (texpr_adlevel DataOnly)))
         (upper
          ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 10))
           (texpr_adlevel DataOnly)))
         (body 10)))
       (10 (Block (11 14 17 22)))
       (11
        (IfElse
         ((texpr_type UInt) (texpr_loc <opaque>)
          (texpr
           (FunApp Greater__
            (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var j))
              (texpr_adlevel DataOnly))
             ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 9))
              (texpr_adlevel DataOnly)))))
          (texpr_adlevel DataOnly))
         12 ()))
       (12 (Block (13))) (13 Break)
       (14
        (IfElse
         ((texpr_type UInt) (texpr_loc <opaque>)
          (texpr
           (FunApp And__
            (((texpr_type UInt) (texpr_loc <opaque>)
              (texpr
               (FunApp Greater__
                (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var j))
                  (texpr_adlevel DataOnly))
                 ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 8))
                  (texpr_adlevel DataOnly)))))
              (texpr_adlevel DataOnly))
             ((texpr_type UInt) (texpr_loc <opaque>)
              (texpr
               (FunApp Less__
                (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var i))
                  (texpr_adlevel DataOnly))
                 ((texpr_type UInt) (texpr_loc <opaque>)
                  (texpr
                   (FunApp PMinus__
                    (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 1))
                      (texpr_adlevel DataOnly)))))
                  (texpr_adlevel DataOnly)))))
              (texpr_adlevel DataOnly)))))
          (texpr_adlevel DataOnly))
         15 ()))
       (15 (Block (16))) (16 Continue)
       (17
        (IfElse
         ((texpr_type UInt) (texpr_loc <opaque>)
          (texpr
           (FunApp Greater__
            (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var j))
              (texpr_adlevel DataOnly))
             ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 5))
              (texpr_adlevel DataOnly)))))
          (texpr_adlevel DataOnly))
         18 (20)))
       (18 (Block (19))) (19 Continue) (20 (Block (21)))
       (21
        (NRFunApp print
         (((texpr_type UReal) (texpr_loc <opaque>) (texpr (Lit Str Badger))
           (texpr_adlevel DataOnly))
          ((texpr_type UInt) (texpr_loc <opaque>)
           (texpr
            (FunApp Plus__
             (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var i))
               (texpr_adlevel DataOnly))
              ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var j))
               (texpr_adlevel DataOnly)))))
           (texpr_adlevel DataOnly)))))
       (22
        (NRFunApp print
         (((texpr_type UReal) (texpr_loc <opaque>) (texpr (Lit Str Fin))
           (texpr_adlevel DataOnly))))))
    |}]

let%expect_test "Predecessor graph example" =
  let exits, preds = build_predecessor_graph example1_statement_map in
  print_s
    [%sexp
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((7 9 13)
       ((1 ()) (2 (1)) (3 (2)) (4 (3)) (5 (4)) (6 (5)) (7 (6)) (8 (5))
        (9 (8 16 19 22)) (10 (9)) (11 (10)) (12 (11)) (13 (12)) (14 (13)) (15 (14))
        (16 (15)) (17 (16)) (18 (17)) (19 (18)) (20 (17)) (21 (20)) (22 (19 21))))
    |}]

let%expect_test "Controlflow graph example" =
  let cf = build_cf_graph example1_statement_map in
  print_s [%sexp (cf : (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 ()) (5 ()) (6 (5)) (7 (5)) (8 (5)) (9 (5 13))
       (10 (9)) (11 (9)) (12 (11)) (13 (11)) (14 (9)) (15 (14)) (16 (14))
       (17 (9 16)) (18 (16 17)) (19 (16 17)) (20 (16 17)) (21 (16 17))
       (22 (9 16 19)))
    |}]

(* We don't actually expect to be called this way, we instead expect to be called on each
   function body separately *)
let example2_program =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        real f() {
          if (3>2) {
            print("hello");
            return 2;
          }
          return 22;
          return 14;
        }
        void g() {
          print("bye");
        }
      }
      model {
        print(f());
      }
      |}
  in
  let mir =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let blocks =
    Mir.SList
      [ {stmt= SList mir.functions_block; sloc= Mir.no_span}
      ; {stmt= Block mir.log_prob; sloc= Mir.no_span} ]
  in
  {stmt= blocks; sloc= Mir.no_span}

let example2_statement_map =
  build_statement_map (fun s -> s.stmt) (fun s -> s.sloc) example2_program

let%expect_test "Statement label map example 2" =
  print_s
    [%sexp
      ( example2_statement_map
        : ( label
          , (expr_typed_located, label) statement * Ast.location_span )
          Map.Poly.t )] ;
  [%expect
    {|
      ((1
        ((SList (2 14))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (2
        ((SList (3 11))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (3
        ((FunDef (fdrt (UReal)) (fdname f) (fdargs ()) (fdbody 4))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (included_from ())))
          (end_loc
           ((filename string) (line_num 10) (col_num 9) (included_from ()))))))
       (4
        ((Block (5 9 10))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 17) (included_from ())))
          (end_loc
           ((filename string) (line_num 10) (col_num 9) (included_from ()))))))
       (5
        ((IfElse
          ((texpr_type UInt) (texpr_loc <opaque>)
           (texpr
            (FunApp Greater__
             (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 3))
               (texpr_adlevel DataOnly))
              ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 2))
               (texpr_adlevel DataOnly)))))
           (texpr_adlevel DataOnly))
          6 ())
         ((begin_loc
           ((filename string) (line_num 4) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 7) (col_num 11) (included_from ()))))))
       (6
        ((Block (7 8))
         ((begin_loc
           ((filename string) (line_num 4) (col_num 19) (included_from ())))
          (end_loc
           ((filename string) (line_num 7) (col_num 11) (included_from ()))))))
       (7
        ((NRFunApp print
          (((texpr_type UReal) (texpr_loc <opaque>) (texpr (Lit Str hello))
            (texpr_adlevel DataOnly))))
         ((begin_loc
           ((filename string) (line_num 5) (col_num 12) (included_from ())))
          (end_loc
           ((filename string) (line_num 5) (col_num 27) (included_from ()))))))
       (8
        ((Return
          (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 2))
            (texpr_adlevel DataOnly))))
         ((begin_loc
           ((filename string) (line_num 6) (col_num 12) (included_from ())))
          (end_loc
           ((filename string) (line_num 6) (col_num 21) (included_from ()))))))
       (9
        ((Return
          (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 22))
            (texpr_adlevel DataOnly))))
         ((begin_loc
           ((filename string) (line_num 8) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 8) (col_num 20) (included_from ()))))))
       (10
        ((Return
          (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 14))
            (texpr_adlevel DataOnly))))
         ((begin_loc
           ((filename string) (line_num 9) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 9) (col_num 20) (included_from ()))))))
       (11
        ((FunDef (fdrt ()) (fdname g) (fdargs ()) (fdbody 12))
         ((begin_loc
           ((filename string) (line_num 11) (col_num 8) (included_from ())))
          (end_loc
           ((filename string) (line_num 13) (col_num 9) (included_from ()))))))
       (12
        ((Block (13))
         ((begin_loc
           ((filename string) (line_num 11) (col_num 17) (included_from ())))
          (end_loc
           ((filename string) (line_num 13) (col_num 9) (included_from ()))))))
       (13
        ((NRFunApp print
          (((texpr_type UReal) (texpr_loc <opaque>) (texpr (Lit Str bye))
            (texpr_adlevel DataOnly))))
         ((begin_loc
           ((filename string) (line_num 12) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 12) (col_num 23) (included_from ()))))))
       (14
        ((Block (15))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (15
        ((NRFunApp print
          (((texpr_type UReal) (texpr_loc <opaque>) (texpr (FunApp f ()))
            (texpr_adlevel DataOnly))))
         ((begin_loc
           ((filename string) (line_num 16) (col_num 8) (included_from ())))
          (end_loc
           ((filename string) (line_num 16) (col_num 19) (included_from ())))))))
    |}]

let%expect_test "Controlflow graph example 2" =
  let cf = build_cf_graph example2_statement_map in
  print_s [%sexp (cf : (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 (3)) (5 (3)) (6 (5)) (7 (5)) (8 (5)) (9 (3 8))
       (10 (3 8 9)) (11 (8 9 10)) (12 (8 9 10 11)) (13 (8 9 10 11)) (14 (8 9 10))
       (15 (8 9 10)))
    |}]

let%expect_test "Predecessor graph example 2" =
  let exits, preds = build_predecessor_graph example2_statement_map in
  print_s
    [%sexp
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((15)
       ((1 ()) (2 (1)) (3 (2)) (4 (3)) (5 (4)) (6 (5)) (7 (6)) (8 (7)) (9 (8))
        (10 (9)) (11 (10)) (12 (11)) (13 (12)) (14 (13)) (15 (14))))
    |}]

(* TODO: this predecessor graph is wrong! *)

let%test "Reconstructed recursive statement" =
  let stmt =
    build_recursive_statement
      (fun stmt meta -> {stmt; sloc= meta})
      example1_statement_map 1
  in
  stmt = example1_program

let example3_program =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        while (42);
        print("exit");
      }
      |}
  in
  let mir =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let blocks =
    Mir.SList
      [ {stmt= SList mir.functions_block; sloc= Mir.no_span}
      ; {stmt= Block mir.log_prob; sloc= Mir.no_span} ]
  in
  {stmt= blocks; sloc= Mir.no_span}

let example3_statement_map =
  build_statement_map (fun s -> s.stmt) (fun s -> s.sloc) example3_program

let%expect_test "Statement label map example 3" =
  print_s
    [%sexp
      ( example3_statement_map
        : ( label
          , (expr_typed_located, label) statement * Ast.location_span )
          Map.Poly.t )] ;
  [%expect
    {|
      ((1
        ((SList (2 3))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (2
        ((SList ())
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (3
        ((Block (4 6))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (4
        ((While
          ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 42))
           (texpr_adlevel DataOnly))
          5)
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (included_from ())))
          (end_loc
           ((filename string) (line_num 3) (col_num 19) (included_from ()))))))
       (5
        (Skip
         ((begin_loc
           ((filename string) (line_num 3) (col_num 18) (included_from ())))
          (end_loc
           ((filename string) (line_num 3) (col_num 19) (included_from ()))))))
       (6
        ((NRFunApp print
          (((texpr_type UReal) (texpr_loc <opaque>) (texpr (Lit Str exit))
            (texpr_adlevel DataOnly))))
         ((begin_loc
           ((filename string) (line_num 4) (col_num 8) (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 22) (included_from ())))))))
    |}]

let%expect_test "Controlflow graph example 3" =
  let cf = build_cf_graph example3_statement_map in
  print_s [%sexp (cf : (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect {|
      ((1 ()) (2 ()) (3 ()) (4 ()) (5 (4)) (6 ()))
    |}]

let%expect_test "Predecessor graph example 3" =
  (* TODO: this is still wrong. The correct answer is
      ((6) ((1 ()) (2 (1)) (3 (2)) (4 (3 5)) (5 (4)) (6 (5))))
  Similarly for for-loops.
  ) *)
  let exits, preds = build_predecessor_graph example3_statement_map in
  print_s
    [%sexp
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((6) ((1 ()) (2 (1)) (3 (2)) (4 (3 5)) (5 (4)) (6 (4))))
    |}]

let example4_program =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        for (i in 1:6) {
          continue;
          ;
        }
      }
      |}
  in
  let mir =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let blocks =
    Mir.SList
      [ {stmt= SList mir.functions_block; sloc= Mir.no_span}
      ; {stmt= Block mir.log_prob; sloc= Mir.no_span} ]
  in
  {stmt= blocks; sloc= Mir.no_span}

let example4_statement_map =
  build_statement_map (fun s -> s.stmt) (fun s -> s.sloc) example4_program

let%expect_test "Statement label map example 4" =
  print_s
    [%sexp
      ( example4_statement_map
        : ( label
          , (expr_typed_located, label) statement * Ast.location_span )
          Map.Poly.t )] ;
  [%expect
    {|
      ((1
        ((SList (2 3))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (2
        ((SList ())
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (3
        ((Block (4))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (4
        ((For (loopvar i)
          (lower
           ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 1))
            (texpr_adlevel DataOnly)))
          (upper
           ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 6))
            (texpr_adlevel DataOnly)))
          (body 5))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (included_from ())))
          (end_loc ((filename string) (line_num 6) (col_num 9) (included_from ()))))))
       (5
        ((Block (6 7))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 23) (included_from ())))
          (end_loc ((filename string) (line_num 6) (col_num 9) (included_from ()))))))
       (6
        (Continue
         ((begin_loc
           ((filename string) (line_num 4) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 19) (included_from ()))))))
       (7
        (Skip
         ((begin_loc
           ((filename string) (line_num 5) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 5) (col_num 11) (included_from ())))))))
    |}]

let%expect_test "Controlflow graph example 4" =
  let cf = build_cf_graph example4_statement_map in
  print_s [%sexp (cf : (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 ()) (5 (4)) (6 (4)) (7 (4 6)))
    |}]

let%expect_test "Predecessor graph example 4" =
  let exits, preds = build_predecessor_graph example4_statement_map in
  (* TODO: this is still wrong. The correct answer is
  ( (7) ( (1 ()) (2 (1)) (3 (2)) (4 (3 6)) (5 (4)) (6 (5)) (7 ()) ) )
  or a very conservative approximation
  ( (7) ( (1 ()) (2 (1)) (3 (2)) (4 (3 6 7)) (5 (4)) (6 (5)) (7 (6)) ) )
   *)
  print_s
    [%sexp
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((4) ((1 ()) (2 (1)) (3 (2)) (4 (3 6 7)) (5 (4)) (6 (5)) (7 (6))))
    |}]

let example5_program =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        for (i in 1:6) {
          break;
          ;
        }
        ;
      }
      |}
  in
  let mir =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let blocks =
    Mir.SList
      [ {stmt= SList mir.functions_block; sloc= Mir.no_span}
      ; {stmt= Block mir.log_prob; sloc= Mir.no_span} ]
  in
  {stmt= blocks; sloc= Mir.no_span}

let example5_statement_map =
  build_statement_map (fun s -> s.stmt) (fun s -> s.sloc) example5_program

let%expect_test "Statement label map example 5" =
  print_s
    [%sexp
      ( example5_statement_map
        : ( label
          , (expr_typed_located, label) statement * Ast.location_span )
          Map.Poly.t )] ;
  [%expect
    {|
      ((1
        ((SList (2 3))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (2
        ((SList ())
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (3
        ((Block (4 8))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (4
        ((For (loopvar i)
          (lower
           ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 1))
            (texpr_adlevel DataOnly)))
          (upper
           ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 6))
            (texpr_adlevel DataOnly)))
          (body 5))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (included_from ())))
          (end_loc ((filename string) (line_num 6) (col_num 9) (included_from ()))))))
       (5
        ((Block (6 7))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 23) (included_from ())))
          (end_loc ((filename string) (line_num 6) (col_num 9) (included_from ()))))))
       (6
        (Break
         ((begin_loc
           ((filename string) (line_num 4) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 16) (included_from ()))))))
       (7
        (Skip
         ((begin_loc
           ((filename string) (line_num 5) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 5) (col_num 11) (included_from ()))))))
       (8
        (Skip
         ((begin_loc
           ((filename string) (line_num 7) (col_num 8) (included_from ())))
          (end_loc ((filename string) (line_num 7) (col_num 9) (included_from ())))))))
    |}]

let%expect_test "Controlflow graph example 5" =
  let cf = build_cf_graph example5_statement_map in
  print_s [%sexp (cf : (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 (6)) (5 (4)) (6 (4)) (7 (4)) (8 ()))
    |}]

let%expect_test "Predecessor graph example 5" =
  let exits, preds = build_predecessor_graph example5_statement_map in
  (* TODO: this is still very very conservative (e.g. I'd hope for 
  (8) ((1 ())) (2 (1)) (3 (2)) (4 (3)) (5 (4)) (6 (5)) (7 ()) (8 (6))
  but maybe that's too much to ask for
  ) *)
  print_s
    [%sexp
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((8) ((1 ()) (2 (1)) (3 (2)) (4 (3 7)) (5 (4)) (6 (5)) (7 (6)) (8 (4 6))))
    |}]
