(* Code for optimization passes on the MIR *)
open Core_kernel
open Mir

let create_function_inline_map l =
  (* We only add the first definition for each function to the inline map.
   This will make sure we do not inline recursive functions. *)
  let f accum {stmt; _} =
    match stmt with
    | FunDef {fdname; fdargs; fdbody; fdrt} -> (
      match
        Map.add accum ~key:fdname
          ~data:(fdrt, List.map ~f:(fun (_, name, _) -> name) fdargs, fdbody)
      with
      | `Ok m -> m
      | `Duplicate -> accum )
    | _ -> Errors.fatal_error ()
  in
  Map.filter
    ~f:(fun (_, _, v) -> v.stmt <> Skip)
    (List.fold l ~init:Map.Poly.empty ~f)

let replace_fresh_local_vars s' =
  let f m = function
    | Decl {decl_adtype; decl_type; decl_id} ->
        let fresh_name = Util.gensym () in
        ( Decl {decl_adtype; decl_id= fresh_name; decl_type}
        , Map.Poly.set m ~key:decl_id ~data:(Var fresh_name) )
    | x -> (x, m)
  in
  let s, m = map_rec_state_stmt_loc f Map.Poly.empty s' in
  Partial_evaluator.subst_stmt m s

let subst_args_stmt args es =
  let m = Map.Poly.of_alist_exn (List.zip_exn args es) in
  Partial_evaluator.subst_stmt m

let handle_early_returns opt_var b =
  let f = function
    | Return opt_ret -> (
      match (opt_var, opt_ret) with
      | None, Some _ | Some _, None -> Errors.fatal_error ()
      | None, None -> Break
      | Some v, Some e ->
          SList
            [{stmt= Assignment (Var v, e); sloc= ""}; {stmt= Break; sloc= ""}]
      )
    | x -> x
  in
  For
    { loopvar= Var (Util.gensym ())
    ; lower= Lit (Int, "1")
    ; upper= Lit (Int, "1")
    ; body= map_rec_stmt_loc f b }

let map_no_loc l = List.map ~f:(fun s -> {stmt= s; sloc= ""}) l
let slist_no_loc l = SList (map_no_loc l)

let slist_concat_no_loc l stmt =
  match l with [] -> stmt | l -> slist_no_loc (l @ [stmt])

let rec inline_function_statement adt fim {stmt; sloc} =
  { stmt=
      ( match stmt with
      | Assignment (e1, e2) ->
          let sl1, e1 = inline_function_expression adt fim e1 in
          let sl2, e2 = inline_function_expression adt fim e2 in
          slist_concat_no_loc (sl2 @ sl1) (Assignment (e1, e2))
      | TargetPE e ->
          let s, e = inline_function_expression adt fim e in
          slist_concat_no_loc s (TargetPE e)
      | NRFunApp (s, es) ->
          let se_list = List.map ~f:(inline_function_expression adt fim) es in
          (* function arguments are evaluated from right to left in C++, so we need to reverse *)
          let s_list = List.concat (List.rev (List.map ~f:fst se_list)) in
          let es = List.map ~f:snd se_list in
          slist_concat_no_loc s_list
            ( match Map.find fim s with
            | None -> NRFunApp (s, es)
            | Some (_, args, b) ->
                let b = replace_fresh_local_vars b in
                let b = handle_early_returns None b in
                (subst_args_stmt args es {stmt= b; sloc= ""}).stmt )
      | Check (f, l) ->
          let se_list = List.map ~f:(inline_function_expression adt fim) l in
          let s_list = List.concat (List.rev (List.map ~f:fst se_list)) in
          let es = List.map ~f:snd se_list in
          slist_concat_no_loc s_list (Check (f, es))
      | Return e -> (
        match e with
        | None -> Return None
        | Some e ->
            let s, e = inline_function_expression adt fim e in
            slist_concat_no_loc s (Return (Some e)) )
      | IfElse (e, s1, s2) ->
          let s, e = inline_function_expression adt fim e in
          slist_concat_no_loc s
            (IfElse
               ( e
               , inline_function_statement adt fim s1
               , Option.map ~f:(inline_function_statement adt fim) s2 ))
      | While (e, s) ->
          let s', e = inline_function_expression adt fim e in
          slist_concat_no_loc s'
            (While
               ( e
               , match s' with
                 | [] -> inline_function_statement adt fim s
                 | _ ->
                     { stmt=
                         SList
                           ( [inline_function_statement adt fim s]
                           @ map_no_loc s' )
                     ; sloc= "" } ))
      | For {loopvar; lower; upper; body} ->
          let s_lower, lower = inline_function_expression adt fim lower in
          let s_upper, upper = inline_function_expression adt fim upper in
          slist_concat_no_loc (s_lower @ s_upper)
            (For
               { loopvar
               ; lower
               ; upper
               ; body=
                   ( match s_upper with
                   | [] -> inline_function_statement adt fim body
                   | _ ->
                       { stmt=
                           SList
                             ( [inline_function_statement adt fim body]
                             @ map_no_loc s_upper )
                       ; sloc= "" } ) })
      | Block l -> Block (List.map l ~f:(inline_function_statement adt fim))
      | SList l -> SList (List.map l ~f:(inline_function_statement adt fim))
      | FunDef {fdrt; fdname; fdargs; fdbody} ->
          FunDef
            { fdrt
            ; fdname
            ; fdargs
            ; fdbody= inline_function_statement adt fim fdbody }
      | Decl r -> Decl r
      | Skip -> Skip
      | Break -> Break
      | Continue -> Continue )
  ; sloc }

and inline_function_expression adt fim e =
  match e with
  | Var x -> ([], Var x)
  | Lit (t, v) -> ([], Lit (t, v))
  | FunApp (s, es) -> (
      let se_list = List.map ~f:(inline_function_expression adt fim) es in
      let s_list = List.concat (List.rev (List.map ~f:fst se_list)) in
      let es = List.map ~f:snd se_list in
      match Map.find fim s with
      | None -> (s_list, FunApp (s, es))
      | Some (rt, args, b) ->
          let b = replace_fresh_local_vars b in
          let x = Util.gensym () in
          let b = handle_early_returns (Some x) b in
          ( s_list
            @ [ Decl
                  {decl_adtype= adt; decl_id= x; decl_type= Option.value_exn rt}
              ; (subst_args_stmt args es {stmt= b; sloc= ""}).stmt ]
          , Var x ) )
  | BinOp (e1, op, e2) ->
      let sl1, e1 = inline_function_expression adt fim e1 in
      let sl2, e2 = inline_function_expression adt fim e2 in
      (* TODO: really, || and && should be lazy here. *)
      (sl1 @ sl2, BinOp (e1, op, e2))
  | TernaryIf (e1, e2, e3) ->
      let sl1, e1 = inline_function_expression adt fim e1 in
      let sl2, e2 = inline_function_expression adt fim e2 in
      let sl3, e3 = inline_function_expression adt fim e3 in
      ( sl1
        @ [ IfElse
              ( e1
              , {stmt= slist_no_loc sl2; sloc= ""}
              , Some {stmt= slist_no_loc sl3; sloc= ""} ) ]
      , TernaryIf (e1, e2, e3) )
  | Indexed (e, i_list) ->
      let sl, e = inline_function_expression adt fim e in
      let si_list = List.map ~f:(inline_function_index adt fim) i_list in
      let i_list = List.map ~f:snd si_list in
      let s_list = List.concat (List.rev (List.map ~f:fst si_list)) in
      (s_list @ sl, Indexed (e, i_list))

and inline_function_index adt fim i =
  match i with
  | All -> ([], All)
  | Single e ->
      let sl, e = inline_function_expression adt fim e in
      (sl, Single e)
  | Upfrom e ->
      let sl, e = inline_function_expression adt fim e in
      (sl, Upfrom e)
  | Downfrom e ->
      let sl, e = inline_function_expression adt fim e in
      (sl, Downfrom e)
  | Between (e1, e2) ->
      let sl1, e1 = inline_function_expression adt fim e1 in
      let sl2, e2 = inline_function_expression adt fim e2 in
      (sl1 @ sl2, Between (e1, e2))
  | MultiIndex e ->
      let sl, e = inline_function_expression adt fim e in
      (sl, MultiIndex e)

let function_inlining (mir : stmt_loc prog) =
  let function_inline_map = create_function_inline_map mir.functions_block in
  let inline_function_statements adt =
    List.map ~f:(inline_function_statement adt function_inline_map)
  in
  { functions_block= inline_function_statements Ast.DataOnly mir.functions_block
  ; data_vars= mir.data_vars
  ; tdata_vars= mir.tdata_vars
  ; prepare_data= inline_function_statements Ast.DataOnly mir.prepare_data
  ; params= mir.params
  ; tparams= mir.tparams
  ; prepare_params=
      inline_function_statements Ast.AutoDiffable mir.prepare_params
  ; log_prob= inline_function_statements Ast.AutoDiffable mir.log_prob
  ; gen_quant_vars= mir.gen_quant_vars
  ; generate_quantities=
      inline_function_statements Ast.DataOnly mir.generate_quantities
  ; prog_name= mir.prog_name
  ; prog_path= mir.prog_path }

let rec contains_top_break_or_continue {stmt; _} =
  match stmt with
  | Break | Continue -> true
  | Assignment (_, _)
   |TargetPE _
   |NRFunApp (_, _)
   |Check _ | Return _ | FunDef _ | Decl _
   |While (_, _)
   |For _ | Skip ->
      false
  | Block l | SList l -> List.exists l ~f:contains_top_break_or_continue
  | IfElse (_, b1, b2) -> (
      contains_top_break_or_continue b1
      ||
      match b2 with
      | None -> false
      | Some b -> contains_top_break_or_continue b )

let unroll_loops_statement =
  let f stmt =
    match stmt with
    | For {loopvar; lower; upper; body} -> (
      match (contains_top_break_or_continue body, lower, upper) with
      | false, Lit (Int, low), Lit (Int, up) ->
          let range =
            List.map
              ~f:(fun i -> Lit (Int, Int.to_string i))
              (List.range ~start:`inclusive ~stop:`inclusive
                 (Int.of_string low) (Int.of_string up))
          in
          let loopvar_str =
            match loopvar with Var s -> s | _ -> Errors.fatal_error ()
          in
          let stmts =
            List.map
              ~f:(fun i ->
                subst_args_stmt [loopvar_str] [i] {stmt= body.stmt; sloc= ""}
                )
              range
          in
          SList stmts
      | _ -> stmt )
    | _ -> stmt
  in
  map_rec_stmt_loc f

let loop_unrolling = map_prog unroll_loops_statement

let collapse_lists_statement =
  let rec collapse_lists l =
    match l with
    | [] -> []
    | {stmt= SList l'; _} :: rest -> l' @ collapse_lists rest
    | x :: rest -> x :: collapse_lists rest
  in
  let f = function
    | Block l -> Block (collapse_lists l)
    | SList l -> SList (collapse_lists l)
    | x -> x
  in
  map_rec_stmt_loc f

let list_collapsing (mir : stmt_loc prog) =
  map_prog collapse_lists_statement mir

let statement_of_program mir =
  { stmt=
      SList
        [ {stmt= SList mir.functions_block; sloc= ""}
        ; {stmt= SList mir.prepare_data; sloc= ""}
        ; {stmt= SList mir.prepare_params; sloc= ""}
        ; {stmt= Block mir.log_prob; sloc= ""}
        ; {stmt= SList mir.generate_quantities; sloc= ""} ]
  ; sloc= "" }

let update_program_statement_blocks (mir : stmt_loc prog) (s : stmt_loc) =
  let l =
    match s.stmt with
    | SList l ->
        List.map
          ~f:(fun x ->
            match x.stmt with
            | SList l | Block l -> l
            | _ -> raise_s [%sexp (x : stmt_loc)] )
          l
    | _ -> raise_s [%sexp (s : stmt_loc)]
  in
  { mir with
    functions_block= List.nth_exn l 0
  ; prepare_data= List.nth_exn l 1
  ; prepare_params= List.nth_exn l 2
  ; log_prob= List.nth_exn l 3
  ; generate_quantities= List.nth_exn l 4 }

let propagation
    (propagation_transfer :
         (int, Mir.stmt_loc_num) Map.Poly.t
      -> (module
          Monotone_framework_sigs.TRANSFER_FUNCTION
            with type labels = int
             and type properties = (string, Mir.expr) Map.Poly.t option))
    (mir : stmt_loc prog) =
  let s = statement_of_program mir in
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_stmt s
  in
  let (module Flowgraph) = flowgraph in
  let values =
    Monotone_framework.propagation_mfp mir
      (module Flowgraph)
      flowgraph_to_mir propagation_transfer
  in
  let propagate_stmt =
    map_rec_stmt_loc_num flowgraph_to_mir (fun i ->
        Partial_evaluator.subst_stmt_base
          (Option.value ~default:Map.Poly.empty (Map.find_exn values i).entry)
    )
  in
  let s = propagate_stmt (Map.find_exn flowgraph_to_mir 1) in
  update_program_statement_blocks mir s

let constant_propagation =
  propagation Monotone_framework.constant_propagation_transfer

(* TODO: implement separate constant folding phase;
   this will be very clean once we have a recursive map over expressions *)

let expression_propagation =
  propagation Monotone_framework.expression_propagation_transfer

let copy_propagation = propagation Monotone_framework.copy_propagation_transfer

let rec can_side_effect_expr (e : expr) =
  match e with
  | Var _ | Lit (_, _) -> false
  | FunApp (f, es) ->
      String.suffix f 3 = "_lp" || List.exists ~f:can_side_effect_expr es
  | BinOp (e1, _, e2) -> can_side_effect_expr e1 || can_side_effect_expr e2
  | TernaryIf (e1, e2, e3) -> List.exists ~f:can_side_effect_expr [e1; e2; e3]
  | Indexed (e, is) ->
      can_side_effect_expr e || List.exists ~f:can_side_effect_idx is

and can_side_effect_idx (i : idx) =
  match i with
  | All -> false
  | Single e | Upfrom e | Downfrom e | MultiIndex e -> can_side_effect_expr e
  | Between (e1, e2) -> can_side_effect_expr e1 || can_side_effect_expr e2

let is_skip_break_continue s =
  match s with Skip | Break | Continue -> true | _ -> false

(* TODO: could also implement partial dead code elimination *)
let dead_code_elimination (mir : stmt_loc prog) =
  (* TODO: think about whether we should treat function bodies as local scopes in the statement
   from the POV of a live variables analysis.
   (Obviously, this shouldn't be the case for the purposes of reaching definitions,
   constant propagation, expressions analyses. But I do think that's the right way to
   go about live variables. *)
  let s = statement_of_program mir in
  let rev_flowgraph, flowgraph_to_mir =
    Monotone_framework.inverse_flowgraph_of_stmt s
  in
  let (module Rev_Flowgraph) = rev_flowgraph in
  let live_variables =
    Monotone_framework.live_variables_mfp mir
      (module Rev_Flowgraph)
      flowgraph_to_mir
  in
  let dead_code_elim_stmt_base i stmt =
    (* NOTE: entry in the reverse flowgraph, so exit in the forward flowgraph *)
    let live_variables_s =
      (Map.find_exn live_variables i).Monotone_framework_sigs.entry
    in
    match stmt with
    | Assignment (Var x, rhs) ->
        if Set.Poly.mem live_variables_s x || can_side_effect_expr rhs then
          stmt
        else Skip
    | Assignment (Indexed (Var x, is), rhs) ->
        if
          Set.Poly.mem live_variables_s x
          || can_side_effect_expr rhs
          || List.exists ~f:can_side_effect_idx is
        then stmt
        else Skip
    | Assignment _ -> Errors.fatal_error ()
    (* NOTE: we never get rid of declarations as we might not be able to remove an assignment to a variable
           due to side effects. *)
    | Decl _ | TargetPE _
     |NRFunApp (_, _)
     |Check _ | Break | Continue | Return _ | Skip ->
        stmt
    | IfElse (e, b1, b2) -> (
        if
          (* TODO: check if e has side effects, like print, reject, then don't optimize? *)
          (not (can_side_effect_expr e))
          && b1.stmt = Skip
          && ( Option.map ~f:(fun x -> x.stmt) b2 = Some Skip
             || Option.map ~f:(fun x -> x.stmt) b2 = None )
        then Skip
        else
          match e with
          | Lit (Int, "0") | Lit (Real, "0.0") -> (
            match b2 with Some x -> x.stmt | None -> Skip )
          | Lit (_, _) -> b1.stmt
          | _ -> IfElse (e, b1, b2) )
    | While (e, b) -> (
      match e with
      | Lit (Int, "0") | Lit (Real, "0.0") -> Skip
      | _ -> While (e, b) )
    | For {loopvar; lower; upper; body} ->
        (* TODO: check if e has side effects, like print, reject, then don't optimize? *)
        if
          (not (can_side_effect_expr lower))
          && (not (can_side_effect_expr upper))
          && is_skip_break_continue body.stmt
        then Skip
        else For {loopvar; lower; upper; body}
    | Block l ->
        let l' = List.filter ~f:(fun x -> x.stmt <> Skip) l in
        if List.length l' = 0 then Skip else Block l'
    | SList l ->
        let l' = List.filter ~f:(fun x -> x.stmt <> Skip) l in
        SList l'
    (* TODO: do dead code elimination in function body too! *)
    | FunDef x -> FunDef x
  in
  let dead_code_elim_stmt =
    map_rec_stmt_loc_num flowgraph_to_mir dead_code_elim_stmt_base
  in
  let s = dead_code_elim_stmt (Map.find_exn flowgraph_to_mir 1) in
  let mir = update_program_statement_blocks mir s in
  mir

(* TODO: implement SlicStan style optimizer for choosing best program block for each statement. *)
(* TODO: implement lazy code motion. Make sure to apply it separately to each program block, rather than to the program as a whole. *)
(* TODO: or maybe combine the previous two *)

(* TODO: add tests *)

let%expect_test "map_rec_stmt_loc" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        print(24);
        if (13) {
          print(244);
          if (24) {
            print(24);
          }
        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let f = function
    | NRFunApp ("print", [s]) -> NRFunApp ("print", [s; s])
    | x -> x
  in
  let mir = map_prog (map_rec_stmt_loc f) mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block ()) (data_vars ()) (tdata_vars ()) (prepare_data ())
       (params ()) (tparams ()) (prepare_params ())
       (log_prob
        (((sloc <opaque>) (stmt (NRFunApp print ((Lit Int 24) (Lit Int 24)))))
         ((sloc <opaque>)
          (stmt
           (IfElse (Lit Int 13)
            ((sloc <opaque>)
             (stmt
              (Block
               (((sloc <opaque>)
                 (stmt (NRFunApp print ((Lit Int 244) (Lit Int 244)))))
                ((sloc <opaque>)
                 (stmt
                  (IfElse (Lit Int 24)
                   ((sloc <opaque>)
                    (stmt
                     (Block
                      (((sloc <opaque>)
                        (stmt (NRFunApp print ((Lit Int 24) (Lit Int 24)))))))))
                   ())))))))
            ())))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "map_rec_stmt_loc" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        print(24);
        if (13) {
          print(244);
          if (24) {
            print(24);
          }
        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let f i = function
    | NRFunApp ("print", [s]) -> (NRFunApp ("print", [s; s]), i + 1)
    | x -> (x, i)
  in
  let mir_num =
    (map_rec_state_stmt_loc f 0) {stmt= SList mir.log_prob; sloc= ""}
  in
  print_s [%sexp (mir_num : stmt_loc * int)] ;
  [%expect
    {|
      (((sloc <opaque>)
        (stmt
         (SList
          (((sloc <opaque>) (stmt (NRFunApp print ((Lit Int 24) (Lit Int 24)))))
           ((sloc <opaque>)
            (stmt
             (IfElse (Lit Int 13)
              ((sloc <opaque>)
               (stmt
                (Block
                 (((sloc <opaque>)
                   (stmt (NRFunApp print ((Lit Int 244) (Lit Int 244)))))
                  ((sloc <opaque>)
                   (stmt
                    (IfElse (Lit Int 24)
                     ((sloc <opaque>)
                      (stmt
                       (Block
                        (((sloc <opaque>)
                          (stmt (NRFunApp print ((Lit Int 24) (Lit Int 24)))))))))
                     ())))))))
              ())))))))
       3) |}]

let%expect_test "inline functions" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        void f(int x, matrix y) {
          print(x);
          print(y);
        }
        real g(int z) {
          return z^2;
        }
      }
      model {
        f(3, [[3,2],[4,6]]);
        reject(g(53));
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block
        (((sloc <opaque>)
          (stmt
           (FunDef (fdrt ()) (fdname f)
            (fdargs ((AutoDiffable x UInt) (AutoDiffable y UMatrix)))
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>) (stmt (NRFunApp print ((Var x)))))
                 ((sloc <opaque>) (stmt (NRFunApp print ((Var y)))))))))))))
         ((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UReal)) (fdname g) (fdargs ((AutoDiffable z UInt)))
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>)
                  (stmt (Return ((FunApp Pow ((Var z) (Lit Int 2)))))))))))))))))
       (data_vars ()) (tdata_vars ()) (prepare_data ()) (params ()) (tparams ())
       (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (For (loopvar (Var sym1__)) (lower (Lit Int 1)) (upper (Lit Int 1))
            (body
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>) (stmt (NRFunApp print ((Lit Int 3)))))
                 ((sloc <opaque>)
                  (stmt
                   (NRFunApp print
                    ((FunApp make_rowvec
                      ((FunApp make_rowvec ((Lit Int 3) (Lit Int 2)))
                       (FunApp make_rowvec ((Lit Int 4) (Lit Int 6)))))))))))))))))
         ((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt
               (Decl (decl_adtype AutoDiffable) (decl_id sym2__) (decl_type UReal))))
             ((sloc <opaque>)
              (stmt
               (For (loopvar (Var sym3__)) (lower (Lit Int 1)) (upper (Lit Int 1))
                (body
                 ((sloc <opaque>)
                  (stmt
                   (Block
                    (((sloc <opaque>)
                      (stmt
                       (SList
                        (((sloc <opaque>)
                          (stmt
                           (Assignment (Var sym2__)
                            (FunApp Pow ((Lit Int 53) (Lit Int 2))))))
                         ((sloc <opaque>) (stmt Break))))))))))))))
             ((sloc <opaque>) (stmt (NRFunApp reject ((Var sym2__)))))))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "list collapsing" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        void f(int x, matrix y) {
          print(x);
          print(y);
        }
        real g(int z) {
          return z^2;
        }
      }
      model {
        f(3, [[3,2],[4,6]]);
        reject(g(53));
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  let mir = list_collapsing mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
    ((functions_block
      (((sloc <opaque>)
        (stmt
         (FunDef (fdrt ()) (fdname f)
          (fdargs ((AutoDiffable x UInt) (AutoDiffable y UMatrix)))
          (fdbody
           ((sloc <opaque>)
            (stmt
             (Block
              (((sloc <opaque>) (stmt (NRFunApp print ((Var x)))))
               ((sloc <opaque>) (stmt (NRFunApp print ((Var y)))))))))))))
       ((sloc <opaque>)
        (stmt
         (FunDef (fdrt (UReal)) (fdname g) (fdargs ((AutoDiffable z UInt)))
          (fdbody
           ((sloc <opaque>)
            (stmt
             (Block
              (((sloc <opaque>)
                (stmt (Return ((FunApp Pow ((Var z) (Lit Int 2)))))))))))))))))
     (data_vars ()) (tdata_vars ()) (prepare_data ()) (params ()) (tparams ())
     (prepare_params ())
     (log_prob
      (((sloc <opaque>)
        (stmt
         (For (loopvar (Var sym4__)) (lower (Lit Int 1)) (upper (Lit Int 1))
          (body
           ((sloc <opaque>)
            (stmt
             (Block
              (((sloc <opaque>) (stmt (NRFunApp print ((Lit Int 3)))))
               ((sloc <opaque>)
                (stmt
                 (NRFunApp print
                  ((FunApp make_rowvec
                    ((FunApp make_rowvec ((Lit Int 3) (Lit Int 2)))
                     (FunApp make_rowvec ((Lit Int 4) (Lit Int 6)))))))))))))))))
       ((sloc <opaque>)
        (stmt
         (SList
          (((sloc <opaque>)
            (stmt
             (Decl (decl_adtype AutoDiffable) (decl_id sym5__) (decl_type UReal))))
           ((sloc <opaque>)
            (stmt
             (For (loopvar (Var sym6__)) (lower (Lit Int 1)) (upper (Lit Int 1))
              (body
               ((sloc <opaque>)
                (stmt
                 (Block
                  (((sloc <opaque>)
                    (stmt
                     (Assignment (Var sym5__)
                      (FunApp Pow ((Lit Int 53) (Lit Int 2))))))
                   ((sloc <opaque>) (stmt Break))))))))))
           ((sloc <opaque>) (stmt (NRFunApp reject ((Var sym5__)))))))))))
     (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path ""))
    |}]

let%expect_test "do not inline recursive functions" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        real g(int z);
        real g(int z) {
          return z^2;
        }
      }
      model {
        reject(g(53));
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block
        (((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UReal)) (fdname g) (fdargs ((AutoDiffable z UInt)))
            (fdbody ((sloc <opaque>) (stmt Skip))))))
         ((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UReal)) (fdname g) (fdargs ((AutoDiffable z UInt)))
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>)
                  (stmt (Return ((FunApp Pow ((Var z) (Lit Int 2)))))))))))))))))
       (data_vars ()) (tdata_vars ()) (prepare_data ()) (params ()) (tparams ())
       (prepare_params ())
       (log_prob
        (((sloc <opaque>) (stmt (NRFunApp reject ((FunApp g ((Lit Int 53)))))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "inline function in for loop" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        int f(int z) {
          print("f");
          return 42;
        }
        int g(int z) {
          print("g");
          return z + 24;
        }
      }
      model {
        for (i in f(2) : g(3)) print("body");
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block
        (((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UInt)) (fdname f) (fdargs ((AutoDiffable z UInt)))
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str f)))))
                 ((sloc <opaque>) (stmt (Return ((Lit Int 42)))))))))))))
         ((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UInt)) (fdname g) (fdargs ((AutoDiffable z UInt)))
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str g)))))
                 ((sloc <opaque>)
                  (stmt (Return ((BinOp (Var z) Plus (Lit Int 24))))))))))))))))
       (data_vars ()) (tdata_vars ()) (prepare_data ()) (params ()) (tparams ())
       (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt
               (Decl (decl_adtype AutoDiffable) (decl_id sym7__) (decl_type UInt))))
             ((sloc <opaque>)
              (stmt
               (For (loopvar (Var sym8__)) (lower (Lit Int 1)) (upper (Lit Int 1))
                (body
                 ((sloc <opaque>)
                  (stmt
                   (Block
                    (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str f)))))
                     ((sloc <opaque>)
                      (stmt
                       (SList
                        (((sloc <opaque>)
                          (stmt (Assignment (Var sym7__) (Lit Int 42))))
                         ((sloc <opaque>) (stmt Break))))))))))))))
             ((sloc <opaque>)
              (stmt
               (Decl (decl_adtype AutoDiffable) (decl_id sym9__) (decl_type UInt))))
             ((sloc <opaque>)
              (stmt
               (For (loopvar (Var sym10__)) (lower (Lit Int 1)) (upper (Lit Int 1))
                (body
                 ((sloc <opaque>)
                  (stmt
                   (Block
                    (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str g)))))
                     ((sloc <opaque>)
                      (stmt
                       (SList
                        (((sloc <opaque>)
                          (stmt
                           (Assignment (Var sym9__)
                            (BinOp (Lit Int 3) Plus (Lit Int 24)))))
                         ((sloc <opaque>) (stmt Break))))))))))))))
             ((sloc <opaque>)
              (stmt
               (For (loopvar (Var i)) (lower (Var sym7__)) (upper (Var sym9__))
                (body
                 ((sloc <opaque>)
                  (stmt
                   (SList
                    (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str body)))))
                     ((sloc <opaque>)
                      (stmt
                       (Decl (decl_adtype AutoDiffable) (decl_id sym9__)
                        (decl_type UInt))))
                     ((sloc <opaque>)
                      (stmt
                       (For (loopvar (Var sym10__)) (lower (Lit Int 1))
                        (upper (Lit Int 1))
                        (body
                         ((sloc <opaque>)
                          (stmt
                           (Block
                            (((sloc <opaque>)
                              (stmt (NRFunApp print ((Lit Str g)))))
                             ((sloc <opaque>)
                              (stmt
                               (SList
                                (((sloc <opaque>)
                                  (stmt
                                   (Assignment (Var sym9__)
                                    (BinOp (Lit Int 3) Plus (Lit Int 24)))))
                                 ((sloc <opaque>) (stmt Break))))))))))))))))))))))))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "inline function in while loop" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        int f(int z) {
          print("f");
          return 42;
        }
        int g(int z) {
          print("g");
          return z + 24;
        }
      }
      model {
        while (g(3)) print("body");
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block
        (((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UInt)) (fdname f) (fdargs ((AutoDiffable z UInt)))
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str f)))))
                 ((sloc <opaque>) (stmt (Return ((Lit Int 42)))))))))))))
         ((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UInt)) (fdname g) (fdargs ((AutoDiffable z UInt)))
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str g)))))
                 ((sloc <opaque>)
                  (stmt (Return ((BinOp (Var z) Plus (Lit Int 24))))))))))))))))
       (data_vars ()) (tdata_vars ()) (prepare_data ()) (params ()) (tparams ())
       (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt
               (Decl (decl_adtype AutoDiffable) (decl_id sym11__) (decl_type UInt))))
             ((sloc <opaque>)
              (stmt
               (For (loopvar (Var sym12__)) (lower (Lit Int 1)) (upper (Lit Int 1))
                (body
                 ((sloc <opaque>)
                  (stmt
                   (Block
                    (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str g)))))
                     ((sloc <opaque>)
                      (stmt
                       (SList
                        (((sloc <opaque>)
                          (stmt
                           (Assignment (Var sym11__)
                            (BinOp (Lit Int 3) Plus (Lit Int 24)))))
                         ((sloc <opaque>) (stmt Break))))))))))))))
             ((sloc <opaque>)
              (stmt
               (While (Var sym11__)
                ((sloc <opaque>)
                 (stmt
                  (SList
                   (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str body)))))
                    ((sloc <opaque>)
                     (stmt
                      (Decl (decl_adtype AutoDiffable) (decl_id sym11__)
                       (decl_type UInt))))
                    ((sloc <opaque>)
                     (stmt
                      (For (loopvar (Var sym12__)) (lower (Lit Int 1))
                       (upper (Lit Int 1))
                       (body
                        ((sloc <opaque>)
                         (stmt
                          (Block
                           (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str g)))))
                            ((sloc <opaque>)
                             (stmt
                              (SList
                               (((sloc <opaque>)
                                 (stmt
                                  (Assignment (Var sym11__)
                                   (BinOp (Lit Int 3) Plus (Lit Int 24)))))
                                ((sloc <opaque>) (stmt Break)))))))))))))))))))))))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "inline function in if then else" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        int f(int z) {
          print("f");
          return 42;
        }
        int g(int z) {
          print("g");
          return z + 24;
        }
      }
      model {
        if (g(3)) print("body");
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block
        (((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UInt)) (fdname f) (fdargs ((AutoDiffable z UInt)))
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str f)))))
                 ((sloc <opaque>) (stmt (Return ((Lit Int 42)))))))))))))
         ((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UInt)) (fdname g) (fdargs ((AutoDiffable z UInt)))
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str g)))))
                 ((sloc <opaque>)
                  (stmt (Return ((BinOp (Var z) Plus (Lit Int 24))))))))))))))))
       (data_vars ()) (tdata_vars ()) (prepare_data ()) (params ()) (tparams ())
       (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt
               (Decl (decl_adtype AutoDiffable) (decl_id sym13__) (decl_type UInt))))
             ((sloc <opaque>)
              (stmt
               (For (loopvar (Var sym14__)) (lower (Lit Int 1)) (upper (Lit Int 1))
                (body
                 ((sloc <opaque>)
                  (stmt
                   (Block
                    (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str g)))))
                     ((sloc <opaque>)
                      (stmt
                       (SList
                        (((sloc <opaque>)
                          (stmt
                           (Assignment (Var sym13__)
                            (BinOp (Lit Int 3) Plus (Lit Int 24)))))
                         ((sloc <opaque>) (stmt Break))))))))))))))
             ((sloc <opaque>)
              (stmt
               (IfElse (Var sym13__)
                ((sloc <opaque>) (stmt (NRFunApp print ((Lit Str body))))) ())))))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path ""))

    |}]

let%expect_test "inline function in ternary if " =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        int f(int z) {
          print("f");
          return 42;
        }
        int g(int z) {
          print("g");
          return z + 24;
        }
        int h(int z) {
          print("h");
          return z + 4;
        }
      }
      model {
        print(f(2) ? g(3) : h(4));
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block
        (((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UInt)) (fdname f) (fdargs ((AutoDiffable z UInt)))
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str f)))))
                 ((sloc <opaque>) (stmt (Return ((Lit Int 42)))))))))))))
         ((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UInt)) (fdname g) (fdargs ((AutoDiffable z UInt)))
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str g)))))
                 ((sloc <opaque>)
                  (stmt (Return ((BinOp (Var z) Plus (Lit Int 24))))))))))))))
         ((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UInt)) (fdname h) (fdargs ((AutoDiffable z UInt)))
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str h)))))
                 ((sloc <opaque>)
                  (stmt (Return ((BinOp (Var z) Plus (Lit Int 4))))))))))))))))
       (data_vars ()) (tdata_vars ()) (prepare_data ()) (params ()) (tparams ())
       (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt
               (Decl (decl_adtype AutoDiffable) (decl_id sym15__) (decl_type UInt))))
             ((sloc <opaque>)
              (stmt
               (For (loopvar (Var sym16__)) (lower (Lit Int 1)) (upper (Lit Int 1))
                (body
                 ((sloc <opaque>)
                  (stmt
                   (Block
                    (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str f)))))
                     ((sloc <opaque>)
                      (stmt
                       (SList
                        (((sloc <opaque>)
                          (stmt (Assignment (Var sym15__) (Lit Int 42))))
                         ((sloc <opaque>) (stmt Break))))))))))))))
             ((sloc <opaque>)
              (stmt
               (IfElse (Var sym15__)
                ((sloc <opaque>)
                 (stmt
                  (SList
                   (((sloc <opaque>)
                     (stmt
                      (Decl (decl_adtype AutoDiffable) (decl_id sym17__)
                       (decl_type UInt))))
                    ((sloc <opaque>)
                     (stmt
                      (For (loopvar (Var sym18__)) (lower (Lit Int 1))
                       (upper (Lit Int 1))
                       (body
                        ((sloc <opaque>)
                         (stmt
                          (Block
                           (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str g)))))
                            ((sloc <opaque>)
                             (stmt
                              (SList
                               (((sloc <opaque>)
                                 (stmt
                                  (Assignment (Var sym17__)
                                   (BinOp (Lit Int 3) Plus (Lit Int 24)))))
                                ((sloc <opaque>) (stmt Break))))))))))))))))))
                (((sloc <opaque>)
                  (stmt
                   (SList
                    (((sloc <opaque>)
                      (stmt
                       (Decl (decl_adtype AutoDiffable) (decl_id sym19__)
                        (decl_type UInt))))
                     ((sloc <opaque>)
                      (stmt
                       (For (loopvar (Var sym20__)) (lower (Lit Int 1))
                        (upper (Lit Int 1))
                        (body
                         ((sloc <opaque>)
                          (stmt
                           (Block
                            (((sloc <opaque>)
                              (stmt (NRFunApp print ((Lit Str h)))))
                             ((sloc <opaque>)
                              (stmt
                               (SList
                                (((sloc <opaque>)
                                  (stmt
                                   (Assignment (Var sym19__)
                                    (BinOp (Lit Int 4) Plus (Lit Int 4)))))
                                 ((sloc <opaque>) (stmt Break))))))))))))))))))))))
             ((sloc <opaque>)
              (stmt
               (NRFunApp print
                ((TernaryIf (Var sym15__) (Var sym17__) (Var sym19__))))))))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "inline function in ternary if " =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        int f(int z) {
          if (2) {
            print("f");
            return 42;
          }
          return 6;
        }
      }
      model {
        print(f(2));
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block
        (((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UInt)) (fdname f) (fdargs ((AutoDiffable z UInt)))
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>)
                  (stmt
                   (IfElse (Lit Int 2)
                    ((sloc <opaque>)
                     (stmt
                      (Block
                       (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str f)))))
                        ((sloc <opaque>) (stmt (Return ((Lit Int 42)))))))))
                    ())))
                 ((sloc <opaque>) (stmt (Return ((Lit Int 6)))))))))))))))
       (data_vars ()) (tdata_vars ()) (prepare_data ()) (params ()) (tparams ())
       (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt
               (Decl (decl_adtype AutoDiffable) (decl_id sym21__) (decl_type UInt))))
             ((sloc <opaque>)
              (stmt
               (For (loopvar (Var sym22__)) (lower (Lit Int 1)) (upper (Lit Int 1))
                (body
                 ((sloc <opaque>)
                  (stmt
                   (Block
                    (((sloc <opaque>)
                      (stmt
                       (IfElse (Lit Int 2)
                        ((sloc <opaque>)
                         (stmt
                          (Block
                           (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str f)))))
                            ((sloc <opaque>)
                             (stmt
                              (SList
                               (((sloc <opaque>)
                                 (stmt (Assignment (Var sym21__) (Lit Int 42))))
                                ((sloc <opaque>) (stmt Break))))))))))
                        ())))
                     ((sloc <opaque>)
                      (stmt
                       (SList
                        (((sloc <opaque>)
                          (stmt (Assignment (Var sym21__) (Lit Int 6))))
                         ((sloc <opaque>) (stmt Break))))))))))))))
             ((sloc <opaque>) (stmt (NRFunApp print ((Var sym21__)))))))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "unroll nested loop" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|      model {
                for (i in 1:2)
                  for (j in 3:4)
                    print(i, j);
                   }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = loop_unrolling mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block ()) (data_vars ()) (tdata_vars ()) (prepare_data ())
       (params ()) (tparams ()) (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt
               (SList
                (((sloc <opaque>)
                  (stmt (NRFunApp print ((Lit Int 1) (Lit Int 3)))))
                 ((sloc <opaque>)
                  (stmt (NRFunApp print ((Lit Int 1) (Lit Int 4)))))))))
             ((sloc <opaque>)
              (stmt
               (SList
                (((sloc <opaque>)
                  (stmt (NRFunApp print ((Lit Int 2) (Lit Int 3)))))
                 ((sloc <opaque>)
                  (stmt (NRFunApp print ((Lit Int 2) (Lit Int 4)))))))))))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "unroll nested loop with break" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|      model {
                for (i in 1:2)
                  for (j in 3:4) {
                    print(i);
                    break;
                  }
              }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = loop_unrolling mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block ()) (data_vars ()) (tdata_vars ()) (prepare_data ())
       (params ()) (tparams ()) (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt
               (For (loopvar (Var j)) (lower (Lit Int 3)) (upper (Lit Int 4))
                (body
                 ((sloc <opaque>)
                  (stmt
                   (Block
                    (((sloc <opaque>) (stmt (NRFunApp print ((Lit Int 1)))))
                     ((sloc <opaque>) (stmt Break))))))))))
             ((sloc <opaque>)
              (stmt
               (For (loopvar (Var j)) (lower (Lit Int 3)) (upper (Lit Int 4))
                (body
                 ((sloc <opaque>)
                  (stmt
                   (Block
                    (((sloc <opaque>) (stmt (NRFunApp print ((Lit Int 2)))))
                     ((sloc <opaque>) (stmt Break))))))))))))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "constant propagation" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      transformed data {
        int i;
        i = 42;
        int j;
        j = 2 + i;
      }
      model {
        for (x in 1:i) {
          print(i + j);
        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = constant_propagation mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
    ((functions_block ()) (data_vars ())
     (tdata_vars
      ((i
        ((tvident i) (tvtype SInt) (tvtrans Identity)
         (tvloc "file string, line 3, columns 8-14")))
       (j
        ((tvident j) (tvtype SInt) (tvtrans Identity)
         (tvloc "file string, line 5, columns 8-14")))))
     (prepare_data
      (((sloc <opaque>) (stmt (Assignment (Var i) (Lit Int 42))))
       ((sloc <opaque>)
        (stmt (Assignment (Var j) (BinOp (Lit Int 2) Plus (Lit Int 42)))))))
     (params ()) (tparams ()) (prepare_params ())
     (log_prob
      (((sloc <opaque>)
        (stmt
         (For (loopvar (Var x)) (lower (Lit Int 1)) (upper (Lit Int 42))
          (body
           ((sloc <opaque>)
            (stmt
             (Block
              (((sloc <opaque>)
                (stmt (NRFunApp print ((BinOp (Lit Int 42) Plus (Lit Int 44))))))))))))))))
     (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "constant propagation, local scope" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      transformed data {
        int i;
        i = 42;
        {
          int j;
          j = 2;
        }
      }
      model {
        int j;
        for (x in 1:i) {
          print(i + j);
        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = constant_propagation mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
    ((functions_block ()) (data_vars ())
     (tdata_vars
      ((i
        ((tvident i) (tvtype SInt) (tvtrans Identity)
         (tvloc "file string, line 3, columns 8-14")))))
     (prepare_data
      (((sloc <opaque>) (stmt (Assignment (Var i) (Lit Int 42))))
       ((sloc <opaque>)
        (stmt
         (Block
          (((sloc <opaque>)
            (stmt
             (SList
              (((sloc <opaque>)
                (stmt
                 (Decl (decl_adtype AutoDiffable) (decl_id j) (decl_type UInt))))
               ((sloc <opaque>) (stmt Skip))))))
           ((sloc <opaque>) (stmt (Assignment (Var j) (Lit Int 2))))))))))
     (params ()) (tparams ()) (prepare_params ())
     (log_prob
      (((sloc <opaque>)
        (stmt
         (SList
          (((sloc <opaque>)
            (stmt (Decl (decl_adtype AutoDiffable) (decl_id j) (decl_type UInt))))
           ((sloc <opaque>) (stmt Skip))))))
       ((sloc <opaque>)
        (stmt
         (For (loopvar (Var x)) (lower (Lit Int 1)) (upper (Lit Int 42))
          (body
           ((sloc <opaque>)
            (stmt
             (Block
              (((sloc <opaque>)
                (stmt (NRFunApp print ((BinOp (Lit Int 42) Plus (Var j))))))))))))))))
     (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "constant propagation, model block local scope" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int i;
        i = 42;
        int j;
        j = 2;
      }
      generated quantities {
        int i;
        int j;
        for (x in 1:i) {
          print(i + j);
        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = constant_propagation mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
    ((functions_block ()) (data_vars ()) (tdata_vars ()) (prepare_data ())
     (params ()) (tparams ()) (prepare_params ())
     (log_prob
      (((sloc <opaque>)
        (stmt
         (SList
          (((sloc <opaque>)
            (stmt (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type UInt))))
           ((sloc <opaque>) (stmt Skip))))))
       ((sloc <opaque>) (stmt (Assignment (Var i) (Lit Int 42))))
       ((sloc <opaque>)
        (stmt
         (SList
          (((sloc <opaque>)
            (stmt (Decl (decl_adtype AutoDiffable) (decl_id j) (decl_type UInt))))
           ((sloc <opaque>) (stmt Skip))))))
       ((sloc <opaque>) (stmt (Assignment (Var j) (Lit Int 2))))))
     (gen_quant_vars
      ((i
        ((tvident i) (tvtype SInt) (tvtrans Identity)
         (tvloc "file string, line 9, columns 8-14")))
       (j
        ((tvident j) (tvtype SInt) (tvtrans Identity)
         (tvloc "file string, line 10, columns 8-14")))))
     (generate_quantities
      (((sloc <opaque>)
        (stmt
         (For (loopvar (Var x)) (lower (Lit Int 1)) (upper (Lit Int 42))
          (body
           ((sloc <opaque>)
            (stmt
             (Block
              (((sloc <opaque>)
                (stmt (NRFunApp print ((BinOp (Lit Int 42) Plus (Lit Int 2))))))))))))))))
     (prog_name "") (prog_path "")) |}]

let%expect_test "expression propagation" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      transformed data {
        int i;
        int j;
        j = 2 + i;
      }
      model {
        for (x in 1:i) {
          print(i + j);
        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = expression_propagation mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block ()) (data_vars ())
       (tdata_vars
        ((i
          ((tvident i) (tvtype SInt) (tvtrans Identity)
           (tvloc "file string, line 3, columns 8-14")))
         (j
          ((tvident j) (tvtype SInt) (tvtrans Identity)
           (tvloc "file string, line 4, columns 8-14")))))
       (prepare_data
        (((sloc <opaque>)
          (stmt (Assignment (Var j) (BinOp (Lit Int 2) Plus (Var i)))))))
       (params ()) (tparams ()) (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (For (loopvar (Var x)) (lower (Lit Int 1)) (upper (Var i))
            (body
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>)
                  (stmt
                   (NRFunApp print
                    ((BinOp (Var i) Plus (BinOp (Lit Int 2) Plus (Var i)))))))))))))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "copy propagation" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      transformed data {
        int i;
        int j;
        j = i;
        int k;
        k = 2 * j;
      }
      model {
        for (x in 1:i) {
          print(i + j + k);
        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = copy_propagation mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block ()) (data_vars ())
       (tdata_vars
        ((i
          ((tvident i) (tvtype SInt) (tvtrans Identity)
           (tvloc "file string, line 3, columns 8-14")))
         (j
          ((tvident j) (tvtype SInt) (tvtrans Identity)
           (tvloc "file string, line 4, columns 8-14")))
         (k
          ((tvident k) (tvtype SInt) (tvtrans Identity)
           (tvloc "file string, line 6, columns 8-14")))))
       (prepare_data
        (((sloc <opaque>) (stmt (Assignment (Var j) (Var i))))
         ((sloc <opaque>)
          (stmt (Assignment (Var k) (BinOp (Lit Int 2) Times (Var i)))))))
       (params ()) (tparams ()) (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (For (loopvar (Var x)) (lower (Lit Int 1)) (upper (Var i))
            (body
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>)
                  (stmt
                   (NRFunApp print
                    ((BinOp (BinOp (Var i) Plus (Var i)) Plus (Var k))))))))))))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "dead code elimination" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      transformed data {
        int i[2];
        i[1] = 2;
        i = {3, 2};
        int j[2];
        j = {3, 2};
        j[1] = 2;
      }
      model {
        print(i);
        print(j);
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block ()) (data_vars ())
       (tdata_vars
        ((i
          ((tvident i) (tvtype (SArray SInt (Lit Int 2))) (tvtrans Identity)
           (tvloc "file string, line 3, columns 8-17")))
         (j
          ((tvident j) (tvtype (SArray SInt (Lit Int 2))) (tvtrans Identity)
           (tvloc "file string, line 6, columns 8-17")))))
       (prepare_data
        (((sloc <opaque>)
          (stmt (Assignment (Var i) (FunApp make_array ((Lit Int 3) (Lit Int 2))))))
         ((sloc <opaque>)
          (stmt (Assignment (Var j) (FunApp make_array ((Lit Int 3) (Lit Int 2))))))
         ((sloc <opaque>)
          (stmt (Assignment (Indexed (Var j) ((Single (Lit Int 1)))) (Lit Int 2))))))
       (params ()) (tparams ()) (prepare_params ())
       (log_prob
        (((sloc <opaque>) (stmt (NRFunApp print ((Var i)))))
         ((sloc <opaque>) (stmt (NRFunApp print ((Var j)))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "dead code elimination decl" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int i;
        i = 4;
      }
      generated quantities {
        {
          int i;
          print(i);
        }
      }
      |}
  in
  (* TODO: this doesn't work yet with global variables i. Ask Sean to
     not remove top decls? *)
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block ()) (data_vars ()) (tdata_vars ()) (prepare_data ())
       (params ()) (tparams ()) (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type UInt))))))))))
       (gen_quant_vars ())
       (generate_quantities
        (((sloc <opaque>)
          (stmt
           (Block
            (((sloc <opaque>)
              (stmt
               (SList
                (((sloc <opaque>)
                  (stmt
                   (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type UInt))))))))
             ((sloc <opaque>) (stmt (NRFunApp print ((Var i)))))))))))
       (prog_name "") (prog_path "")) |}]

let%expect_test "dead code elimination functions" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        real f() {
          int x;
          x = 23;
          return 24;
        }
      }
      model {
        print(42);
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block
        (((sloc <opaque>)
          (stmt
           (FunDef (fdrt (UReal)) (fdname f) (fdargs ())
            (fdbody
             ((sloc <opaque>)
              (stmt
               (Block
                (((sloc <opaque>)
                  (stmt
                   (SList
                    (((sloc <opaque>)
                      (stmt
                       (Decl (decl_adtype AutoDiffable) (decl_id x)
                        (decl_type UInt))))))))
                 ((sloc <opaque>) (stmt (Return ((Lit Int 24)))))))))))))))
       (data_vars ()) (tdata_vars ()) (prepare_data ()) (params ()) (tparams ())
       (prepare_params ())
       (log_prob (((sloc <opaque>) (stmt (NRFunApp print ((Lit Int 42)))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "dead code elimination, for loop" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int i;
        print(i);
        for (j in 3:5);
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block ()) (data_vars ()) (tdata_vars ()) (prepare_data ())
       (params ()) (tparams ()) (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type UInt))))))))
         ((sloc <opaque>) (stmt (NRFunApp print ((Var i)))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "dead code elimination, while loop" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int i;
        print(i);
        while (0) {
          print(13);
        };
        while (1) {
        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block ()) (data_vars ()) (tdata_vars ()) (prepare_data ())
       (params ()) (tparams ()) (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type UInt))))))))
         ((sloc <opaque>) (stmt (NRFunApp print ((Var i)))))
         ((sloc <opaque>) (stmt (While (Lit Int 1) ((sloc <opaque>) (stmt Skip)))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "dead code elimination, if then" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int i;
        print(i);
        if (1) {
          print("hello");
        } else {
          print("goodbye");
        }
        if (0) {
          print("hello");
        } else {
          print("goodbye");
        }
        if (i) {

        } else {

        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block ()) (data_vars ()) (tdata_vars ()) (prepare_data ())
       (params ()) (tparams ()) (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type UInt))))))))
         ((sloc <opaque>) (stmt (NRFunApp print ((Var i)))))
         ((sloc <opaque>)
          (stmt
           (Block (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str hello)))))))))
         ((sloc <opaque>)
          (stmt
           (Block (((sloc <opaque>) (stmt (NRFunApp print ((Lit Str goodbye)))))))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

let%expect_test "dead code elimination, nested" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int i;
        print(i);
        for (j in 3:5) {
          for (k in 34:2);
        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  print_s [%sexp (mir : Mir.stmt_loc Mir.prog)] ;
  [%expect
    {|
      ((functions_block ()) (data_vars ()) (tdata_vars ()) (prepare_data ())
       (params ()) (tparams ()) (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type UInt))))))))
         ((sloc <opaque>) (stmt (NRFunApp print ((Var i)))))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]

(* Let's do a simple CSE pass,
ideally expressed as a visitor with a separate visit() function? *)
(*
open Core_kernel
open Mir

let _counter = ref 0;;
let gensym =
  fun () ->
    _counter := (!_counter) + 1;
    String.concat ["sym"; (string_of_int !_counter)];;

let rec count_subtrees_rec fnapp2sym e =
  match e with
  | FnApp(_, args) ->
    let _ = List.map args ~f:(count_subtrees_rec fnapp2sym) in
    Hashtbl.Poly.update fnapp2sym e ~f:(function Some(i) -> i+1 | None -> 1)
  | _ -> ()

let count_subtrees e = let fnapp2sym = Hashtbl.Poly.create () in
  count_subtrees_rec fnapp2sym e;
  fnapp2sym

let%expect_test "count_subtrees" =
  let dict = count_subtrees (FnApp("plus",
                                   [FnApp("plus", [Lit(Int, "2"); Lit(Int, "3")]);
                                    FnApp("plus", [Lit(Int, "2"); Lit(Int, "3")])])) in
  print_s [%sexp (Hashtbl.Poly.to_alist dict : (expr * int) list)];
  [%expect{|
    (((FnApp plus ((Lit Int 2) (Lit Int 3))) 2)
     ((FnApp plus
       ((FnApp plus ((Lit Int 2) (Lit Int 3)))
        (FnApp plus ((Lit Int 2) (Lit Int 3)))))
      1)) |}]

let filter_dups fnapp2sym =
  let dups = Hashtbl.Poly.filter fnapp2sym ~f:(fun x -> x > 1) in
  Hashtbl.Poly.map dups ~f:(fun _ -> gensym ())

let add_assigns dups e =
  if Hashtbl.Poly.is_empty dups then e
  else
    Hashtbl.Poly.fold dups ~init:[e] ~f:(fun ~key:ast_node ~data:var l ->
        AssignExpr(var, ast_node) :: l)
    |> ExprList

let%expect_test "add_assigns" =
  let ast_node = (FnApp("plus", [Lit(Int, "2")])) in
  let dups = Hashtbl.Poly.of_alist_exn [(Var "hi"), "lo"] in
  print_s [%sexp ((add_assigns dups ast_node) : expr)];
  [%expect{| (ExprList ((AssignExpr lo (Var hi)) (FnApp plus ((Lit Int 2))))) |}]

let rec replace_usages dups e = match Hashtbl.Poly.find dups e with
  | Some(sym) -> Var sym
  | None -> (match e with
      | FnApp(fname, args) -> FnApp(fname, List.map args
                                      ~f:(replace_usages dups))
      | x -> x)

let%expect_test "replace_usages" =
  let e = (FnApp("p", [Lit(Int, "2")])) in
  let dups = Hashtbl.Poly.of_alist_exn [e, "sup"] in
  print_s [%sexp ((replace_usages dups e) : expr)];
  [%expect{| (Var sup) |}]

let cse e = let dups = filter_dups (count_subtrees e) in
  add_assigns dups (replace_usages dups e)

let optimize e =
  e |> Peep.run_peephole_opts |> cse

let%expect_test _ =
  print_s [%sexp
    ((optimize
        (FnApp("plus",
               [FnApp("log", [FnApp("minus", [Lit(Int, "1"); (Var "hi")])]);
                FnApp("log", [FnApp("minus", [Lit(Int, "1"); (Var "hi")])])])))
     : expr)];
  [%expect{|
      (ExprList
       ((AssignExpr sym1 (FnApp log1m ((Var hi))))
        (FnApp plus ((Var sym1) (Var sym1))))) |}]
*)

(* XXX
   Todos to make code gen as good as Stan 2 codegen
   1. Turn all autodiff types on vardecls in data block to data type
   2. CSE
   3. loop-invariant code motion
*)
