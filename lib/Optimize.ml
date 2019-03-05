(* Code for optimization passes on the MIR *)
open Core_kernel
open Mir

let create_function_inline_map {stmt; _} =
  (* We only add the first definition for each function to the inline map.
   This will make sure we do not inline recursive functions. *)
  let f accum {stmt; _} =
    match stmt with
    | FunDef {fdname; fdargs; fdbody; _} -> (
      match
        Map.add accum ~key:fdname
          ~data:(List.map ~f:(fun (_, name, _) -> name) fdargs, fdbody.stmt)
      with
      | `Ok m -> m
      | `Duplicate -> accum )
    | _ -> Errors.fatal_error ()
  in
  match stmt with
  | Assignment (_, _)
   |TargetPE _
   |NRFunApp (_, _)
   |Check _ | Break | Continue | Return _ | Skip
   |IfElse (_, _, _)
   |While (_, _)
   |For _ | Block _ | Decl _ | FunDef _ ->
      Errors.fatal_error ()
  | SList l -> List.fold l ~init:Map.Poly.empty ~f

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
  | Check {ccfunname; ccvid; cctype; ccargs} ->
      Check {ccfunname; ccvid; cctype; ccargs= List.map ccargs ~f}
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

(* TODO *)

let rec handle_early_returns opt_var b =
  For
    { loopvar= Var (Util.gensym ())
    ; lower= Lit (Int, "1")
    ; upper= Lit (Int, "1")
    ; body=
        { sloc= ""
        ; stmt=
            ( match b with
            | Return opt_ret -> (
              match (opt_var, opt_ret) with
              | None, Some _ | Some _, None -> Errors.fatal_error ()
              | None, None -> Break
              | Some v, Some e -> Assignment (Var v, e) )
            | IfElse (e, b1, b2) ->
                IfElse
                  ( e
                  , {stmt= handle_early_returns opt_var b1.stmt; sloc= b1.sloc}
                  , match b2 with
                    | None -> None
                    | Some b2 ->
                        Some
                          { stmt= handle_early_returns opt_var b2.stmt
                          ; sloc= b2.sloc } )
            | While (e, body) ->
                While
                  ( e
                  , { stmt= handle_early_returns opt_var body.stmt
                    ; sloc= body.sloc } )
            | For {loopvar; lower; upper; body} ->
                For
                  { loopvar
                  ; lower
                  ; upper
                  ; body=
                      { stmt= handle_early_returns opt_var body.stmt
                      ; sloc= body.sloc } }
            | Block l ->
                Block
                  (List.map
                     ~f:(fun {stmt; sloc} ->
                       {stmt= handle_early_returns opt_var stmt; sloc} )
                     l)
            | SList l ->
                SList
                  (List.map
                     ~f:(fun {stmt; sloc} ->
                       {stmt= handle_early_returns opt_var stmt; sloc} )
                     l)
            | x -> x ) } }

let map_no_loc l = List.map ~f:(fun s -> {stmt= s; sloc= ""}) l
let slist_no_loc l = SList (map_no_loc l)

(* TODO: declare variables we are binding results to*)
let rec inline_function_statement fim {stmt; sloc} =
  { stmt=
      ( match stmt with
      | Assignment (e1, e2) ->
          let sl1, e1 = inline_function_expression fim e1 in
          let sl2, e2 = inline_function_expression fim e2 in
          slist_no_loc (sl1 @ sl2 @ [Assignment (e1, e2)])
      | TargetPE e ->
          let s, e = inline_function_expression fim e in
          slist_no_loc (s @ [TargetPE e])
      | NRFunApp (s, es) ->
          let se_list = List.map ~f:(inline_function_expression fim) es in
          let s_list = List.concat (List.map ~f:fst se_list) in
          let es = List.map ~f:snd se_list in
          slist_no_loc
            ( s_list
            @ [ ( match Map.find fim s with
                | None -> NRFunApp (s, es)
                | Some (args, b) ->
                    let b = replace_fresh_local_vars b in
                    let b = handle_early_returns None b in
                    subst_args_stmt args es b ) ] )
      | Check {ccfunname; ccvid; cctype; ccargs} ->
          let se_list = List.map ~f:(inline_function_expression fim) ccargs in
          let s_list = List.concat (List.map ~f:fst se_list) in
          let es = List.map ~f:snd se_list in
          slist_no_loc (s_list @ [Check {ccfunname; ccvid; cctype; ccargs= es}])
      | Return e -> (
        match e with
        | None -> Return None
        | Some e ->
            let s, e = inline_function_expression fim e in
            slist_no_loc (s @ [Return (Some e)]) )
      | IfElse (e, s1, s2) ->
          let s, e = inline_function_expression fim e in
          slist_no_loc
            ( s
            @ [ IfElse
                  ( e
                  , inline_function_statement fim s1
                  , Option.map ~f:(inline_function_statement fim) s2 ) ] )
      | While (e, s) ->
          let s', e = inline_function_expression fim e in
          slist_no_loc
            ( s'
            @ [ While
                  ( e
                  , { stmt=
                        SList
                          ([inline_function_statement fim s] @ map_no_loc s')
                    ; sloc= "" } ) ] )
      | For {loopvar; lower; upper; body} ->
          let s_lower, lower = inline_function_expression fim lower in
          let s_upper, upper = inline_function_expression fim upper in
          slist_no_loc
            ( s_lower @ s_upper
            @ [ For
                  { loopvar
                  ; lower
                  ; upper
                  ; body=
                      { stmt=
                          SList
                            ( [inline_function_statement fim body]
                            @ map_no_loc s_upper )
                      ; sloc= "" } } ] )
      | Block l -> Block (List.map l ~f:(inline_function_statement fim))
      | SList l -> SList (List.map l ~f:(inline_function_statement fim))
      | FunDef {fdrt; fdname; fdargs; fdbody} ->
          FunDef
            {fdrt; fdname; fdargs; fdbody= inline_function_statement fim fdbody}
      | Decl r -> Decl r
      | Skip -> Skip
      | Break -> Break
      | Continue -> Continue )
  ; sloc }

and inline_function_expression fim e =
  match e with
  | Var x -> ([], Var x)
  | Lit (t, v) -> ([], Lit (t, v))
  | FunApp (s, es) -> (
      let se_list = List.map ~f:(inline_function_expression fim) es in
      let s_list = List.concat (List.map ~f:fst se_list) in
      let es = List.map ~f:snd se_list in
      match Map.find fim s with
      | None -> (s_list, FunApp (s, es))
      | Some (args, b) ->
          let b = replace_fresh_local_vars b in
          let x = Util.gensym () in
          let b = handle_early_returns (Some x) b in
          (s_list @ [subst_args_stmt args es b], Var x) )
  | BinOp (e1, op, e2) ->
      let sl1, e1 = inline_function_expression fim e1 in
      let sl2, e2 = inline_function_expression fim e2 in
      (sl1 @ sl2, BinOp (e1, op, e2))
  | TernaryIf (e1, e2, e3) ->
      let sl1, e1 = inline_function_expression fim e1 in
      let sl2, e2 = inline_function_expression fim e2 in
      let sl3, e3 = inline_function_expression fim e3 in
      ( sl1
        @ [ IfElse
              ( e1
              , {stmt= slist_no_loc sl2; sloc= ""}
              , Some {stmt= slist_no_loc sl3; sloc= ""} ) ]
      , TernaryIf (e1, e2, e3) )
  | Indexed (e, i_list) ->
      let sl, e = inline_function_expression fim e in
      let si_list = List.map ~f:(inline_function_index fim) i_list in
      let i_list = List.map ~f:snd si_list in
      let s_list = List.concat (List.map ~f:fst si_list) in
      (sl @ s_list, Indexed (e, i_list))

and inline_function_index fim i =
  match i with
  | All -> ([], All)
  | Single e ->
      let sl, e = inline_function_expression fim e in
      (sl, Single e)
  | Upfrom e ->
      let sl, e = inline_function_expression fim e in
      (sl, Upfrom e)
  | Downfrom e ->
      let sl, e = inline_function_expression fim e in
      (sl, Downfrom e)
  | Between (e1, e2) ->
      let sl1, e1 = inline_function_expression fim e1 in
      let sl2, e2 = inline_function_expression fim e2 in
      (sl1 @ sl2, Between (e1, e2))
  | MultiIndex e ->
      let sl, e = inline_function_expression fim e in
      (sl, MultiIndex e)

let function_inlining (mir : stmt_loc prog) =
  let function_inline_map = create_function_inline_map mir.functionsb in
  { functionsb= inline_function_statement function_inline_map mir.functionsb
  ; datavars= mir.datavars
  ; tdatab=
      ( fst mir.tdatab
      , inline_function_statement function_inline_map (snd mir.tdatab) )
  ; modelb=
      ( fst mir.modelb
      , inline_function_statement function_inline_map (snd mir.modelb) )
  ; gqb=
      (fst mir.gqb, inline_function_statement function_inline_map (snd mir.gqb))
  ; prog_name= mir.prog_name
  ; prog_path= mir.prog_path }

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
