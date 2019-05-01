(* Code for optimization passes on the MIR
open Core_kernel
open Mir

let create_function_inline_map {stmt; _} =
  (* We only add the first definition for each function to the inline map.
   This will make sure we do not inline recursive functions. *)
  let f accum {stmt; _} =
    match stmt with
    | FunDef {fdname; fdargs; fdbody; fdrt} -> (
      match
        Map.add accum ~key:fdname
          ~data:
            (fdrt, List.map ~f:(fun (_, name, _) -> name) fdargs, fdbody.stmt)
      with
      | `Ok m -> m
      | `Duplicate -> accum )
    | _ -> Errors.fatal_error ()
  in
  match stmt with
  | Assignment (_, _)
   |TargetPE _
   |NRFunApp (_, _)
   |Block _ | Check _ | Break | Continue | Return _
   |IfElse (_, _, _)
   |While (_, _)
   |For _ | Decl _ | FunDef _ ->
      raise_s [%sexp (stmt : stmt_loc statement)]
  | SList l ->
      Map.filter
        ~f:(fun (_, _, v) -> v <> Skip)
        (List.fold l ~init:Map.Poly.empty ~f)
  | Skip -> Map.Poly.empty

let rec replace_fresh_local_vars stmt =
  match stmt with
  | Decl {decl_adtype; decl_type; _} ->
      Decl {decl_adtype; decl_id= Util.gensym (); decl_type}
  | SList l ->
      SList
        (List.map
           ~f:(fun {stmt; sloc} -> {stmt= replace_fresh_local_vars stmt; sloc})
           l)
  | Block l ->
      Block
        (List.map
           ~f:(fun {stmt; sloc} -> {stmt= replace_fresh_local_vars stmt; sloc})
           l)
  | x -> x

let rec subst_args_expr args es e =
  let arg_map = Map.Poly.of_alist_exn (List.zip_exn args es) in
  let f = subst_args_expr args es in
  match e with
  | Var v -> ( match Map.find arg_map v with None -> Var v | Some e -> e )
  | Lit (t, v) -> Lit (t, v)
  | FunApp (s, e_list) -> FunApp (s, List.map ~f e_list)
  (* Note: when we add higher order functions, we will need to do something here. *)
  | BinOp (e1, op, e2) -> BinOp (f e1, op, f e2)
  | TernaryIf (e1, e2, e3) -> TernaryIf (f e1, f e2, f e3)
  | Indexed (e, is) -> Indexed (f e, List.map ~f:(subst_args_idx args es) is)

and subst_args_idx args es i =
  let f = subst_args_expr args es in
  match i with
  | All -> All
  | Single e -> Single (f e)
  | Upfrom e -> Upfrom (f e)
  | Downfrom e -> Downfrom (f e)
  | Between (e1, e2) -> Between (f e1, f e2)
  | MultiIndex e -> MultiIndex (f e)

let rec subst_args_stmt args es b =
  let f = subst_args_expr args es in
  let g {stmt; sloc} = {stmt= subst_args_stmt args es stmt; sloc} in
  match b with
  | Assignment (e1, e2) -> Assignment (f e1, f e2)
  | TargetPE e -> TargetPE (f e)
  | NRFunApp (s, e_list) -> NRFunApp (s, List.map e_list ~f)
  | Check (s, e_list) -> Check (s, List.map e_list ~f)
  | Return opt_e -> Return (Option.map opt_e ~f)
  | IfElse (e, b1, b2) -> IfElse (f e, g b1, Option.map ~f:g b2)
  | While (e, b) -> While (f e, g b)
  | For {loopvar; lower; upper; body} ->
      For {loopvar; lower= f lower; upper= f upper; body= g body}
  | Block sl -> Block (List.map ~f:g sl)
  | SList sl -> SList (List.map ~f:g sl)
  | FunDef {fdrt; fdname; fdargs; fdbody} ->
      FunDef {fdrt; fdname; fdargs; fdbody= g fdbody}
  | x -> x

let rec handle_early_returns_help opt_var b =
  match b with
  | Return opt_ret -> (
    match (opt_var, opt_ret) with
    | None, Some _ | Some _, None -> Errors.fatal_error ()
    | None, None -> Break
    | Some v, Some e ->
        SList [{stmt= Assignment (Var v, e); sloc= ""}; {stmt= Break; sloc= ""}]
    )
  | IfElse (e, b1, b2) ->
      IfElse
        ( e
        , {stmt= handle_early_returns_help opt_var b1.stmt; sloc= b1.sloc}
        , match b2 with
          | None -> None
          | Some b2 ->
              Some
                {stmt= handle_early_returns_help opt_var b2.stmt; sloc= b2.sloc}
        )
  | While (e, body) ->
      While
        ( e
        , {stmt= handle_early_returns_help opt_var body.stmt; sloc= body.sloc}
        )
  | For {loopvar; lower; upper; body} ->
      For
        { loopvar
        ; lower
        ; upper
        ; body=
            {stmt= handle_early_returns_help opt_var body.stmt; sloc= body.sloc}
        }
  | Block l ->
      Block
        (List.map
           ~f:(fun {stmt; sloc} ->
             {stmt= handle_early_returns_help opt_var stmt; sloc} )
           l)
  | SList l ->
      SList
        (List.map
           ~f:(fun {stmt; sloc} ->
             {stmt= handle_early_returns_help opt_var stmt; sloc} )
           l)
  | x -> x

let handle_early_returns opt_var b =
  For
    { loopvar= Var (Util.gensym ())
    ; lower= Lit (Int, "1")
    ; upper= Lit (Int, "1")
    ; body= {sloc= ""; stmt= handle_early_returns_help opt_var b} }

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
                subst_args_stmt args es b )
      | Check {ccfunname; ccvid; cctype; ccargs} ->
          let se_list =
            List.map ~f:(inline_function_expression adt fim) ccargs
          in
          let s_list = List.concat (List.rev (List.map ~f:fst se_list)) in
          let es = List.map ~f:snd se_list in
          slist_concat_no_loc s_list
            (Check {ccfunname; ccvid; cctype; ccargs= es})
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
              ; subst_args_stmt args es b ]
          , Var x ) )
  | BinOp (e1, op, e2) ->
      let sl1, e1 = inline_function_expression adt fim e1 in
      let sl2, e2 = inline_function_expression adt fim e2 in
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
  let function_inline_map = create_function_inline_map mir.functionsb in
  { functionsb=
      inline_function_statement Ast.DataOnly function_inline_map mir.functionsb
  ; datavars= mir.datavars
  ; tdatab=
      ( fst mir.tdatab
      , inline_function_statement Ast.DataOnly function_inline_map
          (snd mir.tdatab) )
  ; modelb=
      ( fst mir.modelb
      , inline_function_statement Ast.AutoDiffable function_inline_map
          (snd mir.modelb) )
  ; gqb=
      ( fst mir.gqb
      , inline_function_statement Ast.DataOnly function_inline_map
          (snd mir.gqb) )
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

let rec unroll_loops_statement {stmt; sloc} =
  { stmt=
      ( match stmt with
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
                  { stmt=
                      subst_args_stmt [loopvar_str] [i]
                        (unroll_loops_statement body).stmt
                  ; sloc= "" } )
                range
            in
            SList stmts
        | _ -> For {loopvar; lower; upper; body= unroll_loops_statement body} )
      | IfElse (e, b1, b2) ->
          IfElse
            ( e
            , unroll_loops_statement b1
            , Option.map ~f:unroll_loops_statement b2 )
      | While (e, b) -> While (e, unroll_loops_statement b)
      | Block l -> Block (List.map ~f:unroll_loops_statement l)
      | SList l -> SList (List.map ~f:unroll_loops_statement l)
      | FunDef {fdrt; fdname; fdargs; fdbody} ->
          FunDef {fdrt; fdname; fdargs; fdbody= unroll_loops_statement fdbody}
      | Check x -> Check x
      | Break -> Break
      | Assignment (x, y) -> Assignment (x, y)
      | TargetPE x -> TargetPE x
      | NRFunApp (x, y) -> NRFunApp (x, y)
      | Continue -> Continue
      | Return x -> Return x
      | Skip -> Skip
      | Decl x -> Decl x )
  ; sloc }

let loop_unrolling (mir : stmt_loc prog) =
  { functionsb= unroll_loops_statement mir.functionsb
  ; datavars= mir.datavars
  ; tdatab= (fst mir.tdatab, unroll_loops_statement (snd mir.tdatab))
  ; modelb= (fst mir.modelb, unroll_loops_statement (snd mir.modelb))
  ; gqb= (fst mir.gqb, unroll_loops_statement (snd mir.gqb))
  ; prog_name= mir.prog_name
  ; prog_path= mir.prog_path }

let rec collapse_lists_statement {stmt; sloc} =
  let rec collapse_lists l =
    match l with
    | [] -> []
    | {stmt= SList l'; _} :: rest -> l' @ collapse_lists rest
    | x :: rest -> x :: collapse_lists rest
  in
  { stmt=
      ( match stmt with
      | Assignment (x, y) -> Assignment (x, y)
      | TargetPE x -> TargetPE x
      | NRFunApp (x, y) -> NRFunApp (x, y)
      | Check x -> Check x
      | Break -> Break
      | Continue -> Continue
      | Return x -> Return x
      | Skip -> Skip
      | IfElse (x, y, z) ->
          IfElse
            ( x
            , collapse_lists_statement y
            , Option.map ~f:collapse_lists_statement z )
      | While (x, y) -> While (x, collapse_lists_statement y)
      | For {loopvar; lower; upper; body} ->
          For {loopvar; lower; upper; body= collapse_lists_statement body}
      | Block l ->
          Block (List.map ~f:collapse_lists_statement (collapse_lists l))
      | SList l ->
          SList (List.map ~f:collapse_lists_statement (collapse_lists l))
      | Decl x -> Decl x
      | FunDef {fdrt; fdname; fdargs; fdbody} ->
          FunDef {fdrt; fdname; fdargs; fdbody= collapse_lists_statement fdbody}
      )
  ; sloc }

let list_collapsing (mir : stmt_loc prog) =
  { functionsb= collapse_lists_statement mir.functionsb
  ; datavars= mir.datavars
  ; tdatab= (fst mir.tdatab, collapse_lists_statement (snd mir.tdatab))
  ; modelb= (fst mir.modelb, collapse_lists_statement (snd mir.modelb))
  ; gqb= (fst mir.gqb, collapse_lists_statement (snd mir.gqb))
  ; prog_name= mir.prog_name
  ; prog_path= mir.prog_path }

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
      ((functionsb
        ((sloc <opaque>)
         (stmt
          (SList
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
                     (stmt (Return ((FunApp Pow ((Var z) (Lit Int 2))))))))))))))))))))
       (datavars ()) (tdatab (() ((sloc <opaque>) (stmt (SList ())))))
       (modelb
        (()
         ((sloc <opaque>)
          (stmt
           (SList
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
                   (Decl (decl_adtype AutoDiffable) (decl_id sym2__)
                    (decl_type UReal))))
                 ((sloc <opaque>)
                  (stmt
                   (For (loopvar (Var sym3__)) (lower (Lit Int 1))
                    (upper (Lit Int 1))
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
                 ((sloc <opaque>) (stmt (NRFunApp reject ((Var sym2__)))))))))))))))
       (gqb (() ((sloc <opaque>) (stmt (SList ()))))) (prog_name "")
       (prog_path "")) |}]

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
    ((functionsb
      ((sloc <opaque>)
       (stmt
        (SList
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
                   (stmt (Return ((FunApp Pow ((Var z) (Lit Int 2))))))))))))))))))))
     (datavars ()) (tdatab (() ((sloc <opaque>) (stmt (SList ())))))
     (modelb
      (()
       ((sloc <opaque>)
        (stmt
         (SList
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
     (gqb (() ((sloc <opaque>) (stmt (SList ()))))) (prog_name "")
     (prog_path ""))
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
      ((functionsb
        ((sloc <opaque>)
         (stmt
          (SList
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
                     (stmt (Return ((FunApp Pow ((Var z) (Lit Int 2))))))))))))))))))))
       (datavars ()) (tdatab (() ((sloc <opaque>) (stmt (SList ())))))
       (modelb
        (()
         ((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>) (stmt (NRFunApp reject ((FunApp g ((Lit Int 53)))))))))))))
       (gqb (() ((sloc <opaque>) (stmt (SList ()))))) (prog_name "")
       (prog_path "")) |}]

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
      ((functionsb
        ((sloc <opaque>)
         (stmt
          (SList
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
                     (stmt (Return ((BinOp (Var z) Plus (Lit Int 24)))))))))))))))))))
       (datavars ()) (tdatab (() ((sloc <opaque>) (stmt (SList ())))))
       (modelb
        (()
         ((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt
               (SList
                (((sloc <opaque>)
                  (stmt
                   (Decl (decl_adtype AutoDiffable) (decl_id sym7__)
                    (decl_type UInt))))
                 ((sloc <opaque>)
                  (stmt
                   (For (loopvar (Var sym8__)) (lower (Lit Int 1))
                    (upper (Lit Int 1))
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
                                     ((sloc <opaque>) (stmt Break))))))))))))))))))))))))))))))))
       (gqb (() ((sloc <opaque>) (stmt (SList ()))))) (prog_name "")
       (prog_path "")) |}]

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
      ((functionsb
        ((sloc <opaque>)
         (stmt
          (SList
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
                     (stmt (Return ((BinOp (Var z) Plus (Lit Int 24)))))))))))))))))))
       (datavars ()) (tdatab (() ((sloc <opaque>) (stmt (SList ())))))
       (modelb
        (()
         ((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt
               (SList
                (((sloc <opaque>)
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
                               (((sloc <opaque>)
                                 (stmt (NRFunApp print ((Lit Str g)))))
                                ((sloc <opaque>)
                                 (stmt
                                  (SList
                                   (((sloc <opaque>)
                                     (stmt
                                      (Assignment (Var sym11__)
                                       (BinOp (Lit Int 3) Plus (Lit Int 24)))))
                                    ((sloc <opaque>) (stmt Break)))))))))))))))))))))))))))))))
       (gqb (() ((sloc <opaque>) (stmt (SList ()))))) (prog_name "")
       (prog_path "")) |}]

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
      ((functionsb
        ((sloc <opaque>)
         (stmt
          (SList
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
                     (stmt (Return ((BinOp (Var z) Plus (Lit Int 24)))))))))))))))))))
       (datavars ()) (tdatab (() ((sloc <opaque>) (stmt (SList ())))))
       (modelb
        (()
         ((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt
               (SList
                (((sloc <opaque>)
                  (stmt
                   (Decl (decl_adtype AutoDiffable) (decl_id sym13__)
                    (decl_type UInt))))
                 ((sloc <opaque>)
                  (stmt
                   (For (loopvar (Var sym14__)) (lower (Lit Int 1))
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
                               (Assignment (Var sym13__)
                                (BinOp (Lit Int 3) Plus (Lit Int 24)))))
                             ((sloc <opaque>) (stmt Break))))))))))))))
                 ((sloc <opaque>)
                  (stmt
                   (IfElse (Var sym13__)
                    ((sloc <opaque>) (stmt (NRFunApp print ((Lit Str body))))) ())))))))))))))
       (gqb (() ((sloc <opaque>) (stmt (SList ()))))) (prog_name "")
       (prog_path ""))

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
      ((functionsb
        ((sloc <opaque>)
         (stmt
          (SList
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
                     (stmt (Return ((BinOp (Var z) Plus (Lit Int 4)))))))))))))))))))
       (datavars ()) (tdatab (() ((sloc <opaque>) (stmt (SList ())))))
       (modelb
        (()
         ((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt
               (SList
                (((sloc <opaque>)
                  (stmt
                   (Decl (decl_adtype AutoDiffable) (decl_id sym15__)
                    (decl_type UInt))))
                 ((sloc <opaque>)
                  (stmt
                   (For (loopvar (Var sym16__)) (lower (Lit Int 1))
                    (upper (Lit Int 1))
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
                               (((sloc <opaque>)
                                 (stmt (NRFunApp print ((Lit Str g)))))
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
                    ((TernaryIf (Var sym15__) (Var sym17__) (Var sym19__))))))))))))))))
       (gqb (() ((sloc <opaque>) (stmt (SList ()))))) (prog_name "")
       (prog_path "")) |}]

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
      ((functionsb
        ((sloc <opaque>)
         (stmt
          (SList
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
                    ((sloc <opaque>) (stmt (Return ((Lit Int 6))))))))))))))))))
       (datavars ()) (tdatab (() ((sloc <opaque>) (stmt (SList ())))))
       (modelb
        (()
         ((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt
               (SList
                (((sloc <opaque>)
                  (stmt
                   (Decl (decl_adtype AutoDiffable) (decl_id sym21__)
                    (decl_type UInt))))
                 ((sloc <opaque>)
                  (stmt
                   (For (loopvar (Var sym22__)) (lower (Lit Int 1))
                    (upper (Lit Int 1))
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
                               (((sloc <opaque>)
                                 (stmt (NRFunApp print ((Lit Str f)))))
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
                 ((sloc <opaque>) (stmt (NRFunApp print ((Var sym21__)))))))))))))))
       (gqb (() ((sloc <opaque>) (stmt (SList ()))))) (prog_name "")
       (prog_path "")) |}]

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
      ((functionsb ((sloc <opaque>) (stmt Skip))) (datavars ())
       (tdatab (() ((sloc <opaque>) (stmt (SList ())))))
       (modelb
        (()
         ((sloc <opaque>)
          (stmt
           (SList
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
                      (stmt (NRFunApp print ((Lit Int 2) (Lit Int 4)))))))))))))))))))
       (gqb (() ((sloc <opaque>) (stmt (SList ()))))) (prog_name "")
       (prog_path "")) |}]

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
      ((functionsb ((sloc <opaque>) (stmt Skip))) (datavars ())
       (tdatab (() ((sloc <opaque>) (stmt (SList ())))))
       (modelb
        (()
         ((sloc <opaque>)
          (stmt
           (SList
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
                         ((sloc <opaque>) (stmt Break))))))))))))))))))))
       (gqb (() ((sloc <opaque>) (stmt (SList ()))))) (prog_name "")
       (prog_path "")) |}]

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
*)
