open Core_kernel
open Mir
open Dataflow_types

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
   A traversal that simultaneously accumulates a state (type 'f) and replaces the
   substatement values from ('a to 'c). Traversal is done in-order but ignores branching,
   e.g., and if's then block is followed by the else block rather than branching.
*)
let fwd_traverse_statement (stmt : (expr_typed_located, 'a) statement)
    ~init:(state : 'f) ~(f : 'f -> 'a -> 'f * 'c) :
    'f * (expr_typed_located, 'c) statement =
  match stmt with
  | IfElse (pred, then_s, else_s_opt) ->
      let s', c = f state then_s in
      Option.value_map else_s_opt
        ~default:(s', IfElse (pred, c, None))
        ~f:(fun else_s ->
          let s'', c' = f s' else_s in
          (s'', IfElse (pred, c, Some c')) )
  | While (pred, body) ->
      let s', c = f state body in
      (s', While (pred, c))
  | For vars ->
      let s', c = f state vars.body in
      (s', For {vars with body= c})
  | Block stmts ->
      let s', ls =
        List.fold_left stmts
          ~f:(fun (s, l) stmt ->
            let s', c = f s stmt in
            (s', List.cons c l) )
          ~init:(state, [])
      in
      (s', Block (List.rev ls))
  | SList stmts ->
      let s', ls =
        List.fold_left stmts
          ~f:(fun (s, l) stmt ->
            let s', c = f s stmt in
            (s', List.cons c l) )
          ~init:(state, [])
      in
      (s', SList (List.rev ls))
  | FunDef vars ->
      let s', c = f state vars.fdbody in
      (s', FunDef {vars with fdbody= c})
  | Assignment _ as s -> (state, s)
  | TargetPE _ as s -> (state, s)
  | NRFunApp _ as s -> (state, s)
  | Check _ as s -> (state, s)
  | Break as s -> (state, s)
  | Continue as s -> (state, s)
  | Return _ as s -> (state, s)
  | Skip as s -> (state, s)
  | Decl _ as s -> (state, s)

(**
   Like a forward traversal, but branches accumulate two different states that are
   recombined with join.
*)
let branching_traverse_statement (stmt : (expr_typed_located, 'a) statement)
    ~(join : 'f -> 'f -> 'f) ~init:(state : 'f) ~(f : 'f -> 'a -> 'f * 'c) :
    'f * (expr_typed_located, 'c) statement =
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
let branching_fold_statement (stmt : (expr_typed_located, 'a) statement)
    ~(join : 'f -> 'f -> 'f) ~init:(state : 'f) ~(f : 'f -> 'a -> 'f) : 'f =
  fst
    (branching_traverse_statement stmt ~join ~init:state ~f:(fun s a ->
         (f s a, ()) ))

(**
   See interface file
*)
let build_statement_map (extract : 's -> (expr_typed_located, 's) statement)
    (metadata : 's -> 'm) (stmt : 's) :
    (label, (expr_typed_located, label) statement * 'm) Map.Poly.t =
  let rec build_statement_map_rec (next_label : label)
      (map : (label, (expr_typed_located, label) statement * 'm) Map.Poly.t)
      (stmt : 's) :
      (label * (label, (expr_typed_located, label) statement * 'm) Map.Poly.t)
      * label =
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
let rec build_recursive_statement
    (rebuild : (expr_typed_located, 's) statement -> 'm -> 's)
    (statement_map :
      (label, (expr_typed_located, label) statement * 'm) Map.Poly.t)
    (label : label) : 's =
  let stmt_ints, meta = Map.Poly.find_exn statement_map label in
  let build_stmt = build_recursive_statement rebuild statement_map in
  let stmt = map_statement (fun x -> x) build_stmt stmt_ints in
  rebuild stmt meta

(**
   See interface file
*)
let build_cf_graph
    (statement_map :
      (label, (expr_typed_located, label) statement * 'm) Map.Poly.t) :
    (label, label Set.Poly.t) Map.Poly.t =
  let rec build_cf_graph_rec (cf_parent : label option)
      ((breaks_in, returns_in, continues_in, map_in) :
        label Set.Poly.t
        * label Set.Poly.t
        * label Set.Poly.t
        * (label, label Set.Poly.t) Map.Poly.t) (label : label) :
      label Set.Poly.t
      * label Set.Poly.t
      * label Set.Poly.t
      * (label, label Set.Poly.t) Map.Poly.t =
    let stmt, _ = Map.Poly.find_exn statement_map label in
    (* Only control flow nodes should pass themselves down as parents *)
    let is_ctrl_flow =
      match stmt with
      | IfElse _ -> true
      | While _ -> true
      | For _ -> true
      | FunDef _ -> true
      | _ -> false
    in
    let child_cf = if is_ctrl_flow then Some label else cf_parent in
    let join_state (breaks1, returns1, continues1, map_sofar1)
        (breaks2, returns2, continues2, map_sofar2) =
      ( Set.Poly.union breaks1 breaks2
      , Set.Poly.union returns1 returns2
      , Set.Poly.union continues1 continues2
      , union_maps_left map_sofar1 map_sofar2 )
    in
    (* The accumulated state after traversing substatements *)
    let breaks_subexpr, returns_subexpr, continues_subexpr, map_subexpr =
      branching_fold_statement stmt ~join:join_state
        ~init:(breaks_in, returns_in, continues_in, map_in)
        ~f:(build_cf_graph_rec child_cf)
    in
    (* Some statements interact with the break/return/continue states
       E.g., loops nullify breaks and continues in their body, but are still affected by
       breaks and input continues*)
    let breaks_out, returns_out, continues_out, extra_cf_deps =
      match stmt with
      | Break ->
          ( Set.Poly.add breaks_subexpr label
          , returns_subexpr
          , continues_subexpr
          , Set.Poly.empty )
      | Return _ ->
          ( breaks_subexpr
          , Set.Poly.add returns_subexpr label
          , continues_subexpr
          , Set.Poly.empty )
      | Continue ->
          ( breaks_subexpr
          , returns_subexpr
          , Set.Poly.add continues_subexpr label
          , Set.Poly.empty )
      | While _ -> (breaks_in, returns_subexpr, continues_in, breaks_subexpr)
      | For _ -> (breaks_in, returns_subexpr, continues_in, breaks_subexpr)
      | _ ->
          (breaks_subexpr, returns_subexpr, continues_subexpr, Set.Poly.empty)
    in
    let cf_parent_set =
      Option.value_map cf_parent ~default:Set.Poly.empty ~f:Set.Poly.singleton
    in
    let cf_deps =
      Set.Poly.union
        (Set.Poly.union continues_in
           (Set.Poly.union returns_subexpr extra_cf_deps))
        cf_parent_set
    in
    ( breaks_out
    , returns_out
    , continues_out
    , union_maps_left map_subexpr (Map.Poly.singleton label cf_deps) )
  in
  let _, _, _, map =
    build_cf_graph_rec None
      (Set.Poly.empty, Set.Poly.empty, Set.Poly.empty, Map.Poly.empty)
      1
  in
  map

(**
   See interface file
*)
let build_predecessor_graph
    (statement_map :
      (label, (expr_typed_located, label) statement * 'm) Map.Poly.t) :
    label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t =
  let rec build_pred_graph_rec
      ((preds, map_in) :
        label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)
      (label : label) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t
      =
    let stmt, _ = Map.Poly.find_exn statement_map label in
    let join_state (preds1, map1) (preds2, map2) =
      (Set.Poly.union preds1 preds2, union_maps_left map1 map2)
    in
    let exits, map_subexpr =
      branching_fold_statement stmt ~join:join_state
        ~init:(Set.Poly.singleton label, map_in)
        ~f:build_pred_graph_rec
    in
    let looping_predecessors () =
      let exits, map_subexpr =
        branching_fold_statement stmt ~join:join_state
          ~init:(Set.Poly.add exits label, map_in)
          ~f:build_pred_graph_rec
      in
      (Set.Poly.union preds exits, map_subexpr)
    in
    let exits', map_subexpr' =
      match stmt with
      | For _ -> looping_predecessors ()
      | While _ -> looping_predecessors ()
      | _ -> (exits, map_subexpr)
    in
    (exits', union_maps_left map_subexpr' (Map.Poly.singleton label preds))
  in
  build_pred_graph_rec (Set.Poly.empty, Map.Poly.empty) 1

(***********************************)
(* Tests                           *)
(***********************************)

let example1_program =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        model {
          int i = 0;
          if (i < 0) {
            print(i);
          } else {
            for (j in 1:10) {
              if (j > 9) {
                break;
              }
              if (j > 8 && i < -1) {
                continue;
              }
              if (j > 5) {
                continue;
              } else {
                print("Badger", i + j);
              }
              print("Fin");
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
      ( example1_statement_map
        : ( label
          , (expr_typed_located, label) statement * Ast.location_span )
          Map.Poly.t )] ;
  [%expect
    {|
      ((1
        ((Block (2 5))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (2
        ((SList (3 4))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 3) (col_num 20) (included_from ()))))))
       (3
        ((Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type UInt))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 3) (col_num 20) (included_from ()))))))
       (4
        ((Assignment
          ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var i))
           (texpr_adlevel DataOnly))
          ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 0))
           (texpr_adlevel DataOnly)))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 3) (col_num 20) (included_from ()))))))
       (5
        ((IfElse
          ((texpr_type UInt) (texpr_loc <opaque>)
           (texpr
            (FunApp Less__
             (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var i))
               (texpr_adlevel DataOnly))
              ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 0))
               (texpr_adlevel DataOnly)))))
           (texpr_adlevel DataOnly))
          6 (8))
         ((begin_loc
           ((filename string) (line_num 4) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 21) (col_num 11) (included_from ()))))))
       (6
        ((Block (7))
         ((begin_loc
           ((filename string) (line_num 4) (col_num 21) (included_from ())))
          (end_loc
           ((filename string) (line_num 6) (col_num 11) (included_from ()))))))
       (7
        ((NRFunApp print
          (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var i))
            (texpr_adlevel DataOnly))))
         ((begin_loc
           ((filename string) (line_num 5) (col_num 12) (included_from ())))
          (end_loc
           ((filename string) (line_num 5) (col_num 21) (included_from ()))))))
       (8
        ((Block (9))
         ((begin_loc
           ((filename string) (line_num 6) (col_num 17) (included_from ())))
          (end_loc
           ((filename string) (line_num 21) (col_num 11) (included_from ()))))))
       (9
        ((For (loopvar j)
          (lower
           ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 1))
            (texpr_adlevel DataOnly)))
          (upper
           ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 10))
            (texpr_adlevel DataOnly)))
          (body 10))
         ((begin_loc
           ((filename string) (line_num 7) (col_num 12) (included_from ())))
          (end_loc
           ((filename string) (line_num 20) (col_num 13) (included_from ()))))))
       (10
        ((Block (11 14 17 22))
         ((begin_loc
           ((filename string) (line_num 7) (col_num 28) (included_from ())))
          (end_loc
           ((filename string) (line_num 20) (col_num 13) (included_from ()))))))
       (11
        ((IfElse
          ((texpr_type UInt) (texpr_loc <opaque>)
           (texpr
            (FunApp Greater__
             (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var j))
               (texpr_adlevel DataOnly))
              ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 9))
               (texpr_adlevel DataOnly)))))
           (texpr_adlevel DataOnly))
          12 ())
         ((begin_loc
           ((filename string) (line_num 8) (col_num 14) (included_from ())))
          (end_loc
           ((filename string) (line_num 10) (col_num 15) (included_from ()))))))
       (12
        ((Block (13))
         ((begin_loc
           ((filename string) (line_num 8) (col_num 25) (included_from ())))
          (end_loc
           ((filename string) (line_num 10) (col_num 15) (included_from ()))))))
       (13
        (Break
         ((begin_loc
           ((filename string) (line_num 9) (col_num 16) (included_from ())))
          (end_loc
           ((filename string) (line_num 9) (col_num 22) (included_from ()))))))
       (14
        ((IfElse
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
          15 ())
         ((begin_loc
           ((filename string) (line_num 11) (col_num 14) (included_from ())))
          (end_loc
           ((filename string) (line_num 13) (col_num 15) (included_from ()))))))
       (15
        ((Block (16))
         ((begin_loc
           ((filename string) (line_num 11) (col_num 35) (included_from ())))
          (end_loc
           ((filename string) (line_num 13) (col_num 15) (included_from ()))))))
       (16
        (Continue
         ((begin_loc
           ((filename string) (line_num 12) (col_num 16) (included_from ())))
          (end_loc
           ((filename string) (line_num 12) (col_num 25) (included_from ()))))))
       (17
        ((IfElse
          ((texpr_type UInt) (texpr_loc <opaque>)
           (texpr
            (FunApp Greater__
             (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var j))
               (texpr_adlevel DataOnly))
              ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 5))
               (texpr_adlevel DataOnly)))))
           (texpr_adlevel DataOnly))
          18 (20))
         ((begin_loc
           ((filename string) (line_num 14) (col_num 14) (included_from ())))
          (end_loc
           ((filename string) (line_num 18) (col_num 15) (included_from ()))))))
       (18
        ((Block (19))
         ((begin_loc
           ((filename string) (line_num 14) (col_num 25) (included_from ())))
          (end_loc
           ((filename string) (line_num 16) (col_num 15) (included_from ()))))))
       (19
        (Continue
         ((begin_loc
           ((filename string) (line_num 15) (col_num 16) (included_from ())))
          (end_loc
           ((filename string) (line_num 15) (col_num 25) (included_from ()))))))
       (20
        ((Block (21))
         ((begin_loc
           ((filename string) (line_num 16) (col_num 21) (included_from ())))
          (end_loc
           ((filename string) (line_num 18) (col_num 15) (included_from ()))))))
       (21
        ((NRFunApp print
          (((texpr_type UReal) (texpr_loc <opaque>) (texpr (Lit Str Badger))
            (texpr_adlevel DataOnly))
           ((texpr_type UInt) (texpr_loc <opaque>)
            (texpr
             (FunApp Plus__
              (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var i))
                (texpr_adlevel DataOnly))
               ((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var j))
                (texpr_adlevel DataOnly)))))
            (texpr_adlevel DataOnly))))
         ((begin_loc
           ((filename string) (line_num 17) (col_num 16) (included_from ())))
          (end_loc
           ((filename string) (line_num 17) (col_num 39) (included_from ()))))))
       (22
        ((NRFunApp print
          (((texpr_type UReal) (texpr_loc <opaque>) (texpr (Lit Str Fin))
            (texpr_adlevel DataOnly))))
         ((begin_loc
           ((filename string) (line_num 19) (col_num 14) (included_from ())))
          (end_loc
           ((filename string) (line_num 19) (col_num 27) (included_from ())))))))
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

let%expect_test "Predecessor graph example" =
  let exits, preds = build_predecessor_graph example1_statement_map in
  print_s
    [%sexp
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((7 8 22)
       ((1 ()) (2 (1)) (3 (2)) (4 (3)) (5 (4)) (6 (5)) (7 (6)) (8 (5)) (9 (8))
        (10 (9 22)) (11 (10)) (12 (11)) (13 (12)) (14 (13)) (15 (14)) (16 (15))
        (17 (16)) (18 (17)) (19 (18)) (20 (17)) (21 (20)) (22 (19 21))))
    |}]

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
      ((1 (8 9 10)) (2 (8 9 10)) (3 (8 9 10)) (4 (3 8 9 10)) (5 (3 8)) (6 (5 8))
       (7 (5)) (8 (5)) (9 (3 8)) (10 (3 8 9)) (11 (8 9 10)) (12 (8 9 10 11))
       (13 (8 9 10 11)) (14 (8 9 10)) (15 (8 9 10)))
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
