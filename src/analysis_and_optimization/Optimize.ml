(* Code for optimization passes on the MIR *)
open Core_kernel
open Common
open Middle
open Mir_utils

(**
   Apply the transformation to each function body and to the rest of the program as one
   block.
*)
let transform_program (mir : Program.Typed.t)
    (transform : Stmt.Located.t -> Stmt.Located.t) : Program.Typed.t =
  let packed_prog_body =
    transform
      { pattern=
          SList
            (List.map
               ~f:(fun x ->
                 Stmt.Fixed.{pattern= SList x; meta= Location_span.empty} )
               [ mir.prepare_data; mir.transform_inits; mir.log_prob
               ; mir.generate_quantities ])
      ; meta= Location_span.empty }
  in
  let transformed_prog_body = transform packed_prog_body in
  let transformed_functions =
    List.map mir.functions_block ~f:(fun fs ->
        {fs with fdbody= transform fs.fdbody} )
  in
  match transformed_prog_body with
  | { pattern=
        SList
          [ {pattern= SList prepare_data'; _}
          ; {pattern= SList transform_inits'; _}
          ; {pattern= SList log_prob'; _}
          ; {pattern= SList generate_quantities'; _} ]; _ } ->
      { mir with
        functions_block= transformed_functions
      ; prepare_data= prepare_data'
      ; transform_inits= transform_inits'
      ; log_prob= log_prob'
      ; generate_quantities= generate_quantities' }
  | _ ->
      raise
        (Failure "Something went wrong with program transformation packing!")

(**
   Apply the transformation to each function body and to each program block separately.
*)
let transform_program_blockwise (mir : Program.Typed.t)
    (transform : Stmt.Located.t -> Stmt.Located.t) : Program.Typed.t =
  let transform' s =
    match transform {pattern= SList s; meta= Location_span.empty} with
    | {pattern= SList l; _} -> l
    | _ ->
        raise
          (Failure "Something went wrong with program transformation packing!")
  in
  let transformed_functions =
    List.map mir.functions_block ~f:(fun fs ->
        {fs with fdbody= transform fs.fdbody} )
  in
  { mir with
    functions_block= transformed_functions
  ; prepare_data= transform' mir.prepare_data
  ; transform_inits= transform' mir.transform_inits
  ; log_prob= transform' mir.log_prob
  ; generate_quantities= transform' mir.generate_quantities }

let map_no_loc l =
  List.map ~f:(fun s -> Stmt.Fixed.{pattern= s; meta= Location_span.empty}) l

let slist_no_loc l = Stmt.Fixed.Pattern.SList (map_no_loc l)
let block_no_loc l = Stmt.Fixed.Pattern.Block (map_no_loc l)

let slist_concat_no_loc l stmt =
  match l with [] -> stmt | l -> slist_no_loc (l @ [stmt])

let replace_fresh_local_vars s' =
  let f m = function
    | Stmt.Fixed.Pattern.Decl {decl_adtype; decl_type; decl_id} ->
        let fresh_name = Gensym.generate () in
        ( Stmt.Fixed.Pattern.Decl {decl_adtype; decl_id= fresh_name; decl_type}
        , Map.Poly.set m ~key:decl_id
            ~data:
              Expr.Fixed.
                { pattern= Var fresh_name
                ; meta=
                    Expr.Typed.Meta.
                      { type_= Type.to_unsized decl_type
                      ; adlevel= decl_adtype
                      ; loc= Location_span.empty } } )
    | Assignment ((var_name, ut, l), e) ->
        let var_name =
          match Map.Poly.find m var_name with
          | None -> var_name
          | Some Expr.Fixed.({pattern= Var var_name; _}) -> var_name
          | Some e -> raise_s [%sexp (e : Expr.Typed.t)]
        in
        (Stmt.Fixed.Pattern.Assignment ((var_name, ut, l), e), m)
    | x -> (x, m)
  in
  let s, m = map_rec_state_stmt_loc f Map.Poly.empty s' in
  subst_stmt m s

let replace_fresh_local_vars_triple (d_list, s_list, e) =
  let s =
    slist_no_loc
      ([slist_no_loc d_list] @ [slist_no_loc s_list] @ [Return (Some e)])
  in
  let s =
    (replace_fresh_local_vars {pattern= s; meta= Location_span.empty}).pattern
  in
  match s with
  | SList
      [ {pattern= SList d_list; _}
      ; {pattern= SList s_list; _}
      ; {pattern= Return (Some e); _} ] ->
      ( List.map ~f:(fun x -> x.pattern) d_list
      , List.map ~f:(fun x -> x.pattern) s_list
      , e )
  | _ ->
      raise_s [%sexp (s : (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t)]

let subst_args_stmt args es =
  let m = Map.Poly.of_alist_exn (List.zip_exn args es) in
  subst_stmt m

(* TODO: only handle early returns if that's necessary *)
let handle_early_returns opt_triple b =
  let f = function
    | Stmt.Fixed.Pattern.Return opt_ret -> (
      match (opt_triple, opt_ret) with
      | None, None -> Stmt.Fixed.Pattern.Break
      | Some (Some _, _, name), Some e ->
          SList
            [ Stmt.Fixed.
                { pattern= Assignment ((name, Expr.Typed.type_of e, []), e)
                ; meta= Location_span.empty }
            ; {pattern= Break; meta= Location_span.empty} ]
      | _, _ -> raise_s [%sexp ("" : string)] )
    | x -> x
  in
  Stmt.Fixed.Pattern.For
    { loopvar= Gensym.generate ()
    ; lower=
        Expr.Fixed.
          { pattern= Lit (Int, "1")
          ; meta=
              Expr.Typed.Meta.
                {type_= UInt; adlevel= DataOnly; loc= Location_span.empty} }
    ; upper=
        { pattern= Lit (Int, "1")
        ; meta= {type_= UInt; adlevel= DataOnly; loc= Location_span.empty} }
    ; body= map_rec_stmt_loc f b }

let rec inline_function_expression adt fim (Expr.Fixed.({pattern; _}) as e) =
  match pattern with
  | Var _ -> ([], [], e)
  | Lit (_, _) -> ([], [], e)
  | FunApp (t, s, es) -> (
      let dse_list = List.map ~f:(inline_function_expression adt fim) es in
      (* function arguments are evaluated from right to left in C++, so we need to reverse *)
      let d_list =
        List.concat (List.rev (List.map ~f:(function x, _, _ -> x) dse_list))
      in
      let s_list =
        List.concat (List.rev (List.map ~f:(function _, x, _ -> x) dse_list))
      in
      let es = List.map ~f:(function _, _, x -> x) dse_list in
      match Map.find fim s with
      | None -> (d_list, s_list, {e with pattern= FunApp (t, s, es)})
      | Some (rt, args, b) ->
          let x = Gensym.generate () in
          let b = handle_early_returns (Some (rt, adt, x)) b in
          let d_list2, s_list2, e =
            replace_fresh_local_vars_triple
              ( [ Decl
                    { decl_adtype= adt
                    ; decl_id= x
                    ; decl_type= Option.value_exn rt } ]
              , [ (subst_args_stmt args es
                     {pattern= b; meta= Location_span.empty})
                    .pattern ]
              , { pattern= Var x
                ; meta=
                    Expr.Typed.Meta.
                      { type_= Type.to_unsized (Option.value_exn rt)
                      ; adlevel= adt
                      ; loc= Location_span.empty } } )
          in
          let d_list = d_list @ d_list2 in
          let s_list = s_list @ s_list2 in
          (d_list, s_list, e) )
  | TernaryIf (e1, e2, e3) ->
      let dl1, sl1, e1 = inline_function_expression adt fim e1 in
      let dl2, sl2, e2 = inline_function_expression adt fim e2 in
      let dl3, sl3, e3 = inline_function_expression adt fim e3 in
      ( dl1 @ dl2 @ dl3
      , sl1
        @ [ Stmt.Fixed.(
              Pattern.IfElse
                ( e1
                , {pattern= block_no_loc sl2; meta= Location_span.empty}
                , Some {pattern= block_no_loc sl3; meta= Location_span.empty}
                )) ]
      , {e with pattern= TernaryIf (e1, e2, e3)} )
  | Indexed (e', i_list) ->
      let dl, sl, e' = inline_function_expression adt fim e' in
      let dsi_list = List.map ~f:(inline_function_index adt fim) i_list in
      let d_list =
        List.concat (List.rev (List.map ~f:(function x, _, _ -> x) dsi_list))
      in
      let s_list =
        List.concat (List.rev (List.map ~f:(function _, x, _ -> x) dsi_list))
      in
      let i_list = List.map ~f:(function _, _, x -> x) dsi_list in
      (d_list @ dl, s_list @ sl, {e with pattern= Indexed (e', i_list)})
  | EAnd (e1, e2) ->
      let dl1, sl1, e1 = inline_function_expression adt fim e1 in
      let dl2, sl2, e2 = inline_function_expression adt fim e2 in
      let sl2 =
        [ Stmt.Fixed.(
            Pattern.IfElse
              ( e1
              , {pattern= Block (map_no_loc sl2); meta= Location_span.empty}
              , None )) ]
      in
      (dl1 @ dl2, sl1 @ sl2, {e with pattern= EAnd (e1, e2)})
  | EOr (e1, e2) ->
      let dl1, sl1, e1 = inline_function_expression adt fim e1 in
      let dl2, sl2, e2 = inline_function_expression adt fim e2 in
      let sl2 =
        [ Stmt.Fixed.(
            Pattern.IfElse
              ( e1
              , {pattern= Skip; meta= Location_span.empty}
              , Some
                  {pattern= Block (map_no_loc sl2); meta= Location_span.empty}
              )) ]
      in
      (dl1 @ dl2, sl1 @ sl2, {e with pattern= EOr (e1, e2)})

and inline_function_index adt fim i =
  match i with
  | All -> ([], [], All)
  | Single e ->
      let dl, sl, e = inline_function_expression adt fim e in
      (dl, sl, Single e)
  | Upfrom e ->
      let dl, sl, e = inline_function_expression adt fim e in
      (dl, sl, Upfrom e)
  | Between (e1, e2) ->
      let dl1, sl1, e1 = inline_function_expression adt fim e1 in
      let dl2, sl2, e2 = inline_function_expression adt fim e2 in
      (dl1 @ dl2, sl1 @ sl2, Between (e1, e2))
  | MultiIndex e ->
      let dl, sl, e = inline_function_expression adt fim e in
      (dl, sl, MultiIndex e)

let rec inline_function_statement adt fim Stmt.Fixed.({pattern; meta}) =
  Stmt.Fixed.
    { pattern=
        ( match pattern with
        | Assignment ((x, ut, l), e2) ->
            let e1 =
              {e2 with pattern= Indexed ({e2 with pattern= Var x}, l)}
            in
            (* This inner e2 is wrong. We are giving the wrong type to Var x. But it doens't really matter as we discard it later. *)
            let dl1, sl1, e1 = inline_function_expression adt fim e1 in
            let dl2, sl2, e2 = inline_function_expression adt fim e2 in
            let x, l =
              match e1.pattern with
              | Var x -> (x, [])
              | Indexed ({pattern= Var x; _}, l) -> (x, l)
              | _ as w ->
                  raise_s [%sexp (w : Expr.Typed.t Expr.Fixed.Pattern.t)]
            in
            slist_concat_no_loc
              (dl2 @ dl1 @ sl2 @ sl1)
              (Assignment ((x, ut, l), e2))
        | TargetPE e ->
            let d, s, e = inline_function_expression adt fim e in
            slist_concat_no_loc (d @ s) (TargetPE e)
        | NRFunApp (t, s, es) ->
            let dse_list =
              List.map ~f:(inline_function_expression adt fim) es
            in
            (* function arguments are evaluated from right to left in C++, so we need to reverse *)
            let d_list =
              List.concat
                (List.rev (List.map ~f:(function x, _, _ -> x) dse_list))
            in
            let s_list =
              List.concat
                (List.rev (List.map ~f:(function _, x, _ -> x) dse_list))
            in
            let es = List.map ~f:(function _, _, x -> x) dse_list in
            slist_concat_no_loc (d_list @ s_list)
              ( match Map.find fim s with
              | None -> NRFunApp (t, s, es)
              | Some (_, args, b) ->
                  let b = replace_fresh_local_vars b in
                  let b = handle_early_returns None b in
                  (subst_args_stmt args es
                     {pattern= b; meta= Location_span.empty})
                    .pattern )
        | Return e -> (
          match e with
          | None -> Return None
          | Some e ->
              let d, s, e = inline_function_expression adt fim e in
              slist_concat_no_loc (d @ s) (Return (Some e)) )
        | IfElse (e, s1, s2) ->
            let d, s, e = inline_function_expression adt fim e in
            slist_concat_no_loc (d @ s)
              (IfElse
                 ( e
                 , inline_function_statement adt fim s1
                 , Option.map ~f:(inline_function_statement adt fim) s2 ))
        | While (e, s) ->
            let d', s', e = inline_function_expression adt fim e in
            slist_concat_no_loc (d' @ s')
              (While
                 ( e
                 , match s' with
                   | [] -> inline_function_statement adt fim s
                   | _ ->
                       { pattern=
                           Block
                             ( [inline_function_statement adt fim s]
                             @ map_no_loc s' )
                       ; meta= Location_span.empty } ))
        | For {loopvar; lower; upper; body} ->
            let d_lower, s_lower, lower =
              inline_function_expression adt fim lower
            in
            let d_upper, s_upper, upper =
              inline_function_expression adt fim upper
            in
            slist_concat_no_loc
              (d_lower @ d_upper @ s_lower @ s_upper)
              (For
                 { loopvar
                 ; lower
                 ; upper
                 ; body=
                     ( match s_upper with
                     | [] -> inline_function_statement adt fim body
                     | _ ->
                         { pattern=
                             Block
                               ( [inline_function_statement adt fim body]
                               @ map_no_loc s_upper )
                         ; meta= Location_span.empty } ) })
        | Block l -> Block (List.map l ~f:(inline_function_statement adt fim))
        | SList l -> SList (List.map l ~f:(inline_function_statement adt fim))
        | Decl r -> Decl r
        | Skip -> Skip
        | Break -> Break
        | Continue -> Continue )
    ; meta }

let create_function_inline_map adt l =
  (* We only add the first definition for each function to the inline map.
   This will make sure we do not inline recursive functions. *)
  let f accum fundef =
    match fundef with Program.({fdname; fdargs; fdbody; fdrt; _}) -> (
      match
        Map.add accum ~key:fdname
          ~data:
            ( Option.map ~f:(fun x -> Type.Unsized x) fdrt
            , List.map ~f:(fun (_, name, _) -> name) fdargs
            , inline_function_statement adt accum fdbody )
      with
      | `Ok m -> m
      | `Duplicate -> accum )
  in
  Map.filter
    ~f:(fun (_, _, v) -> v.pattern <> Skip)
    (List.fold l ~init:Map.Poly.empty ~f)

let function_inlining (mir : Program.Typed.t) =
  let dataonly_inline_map =
    create_function_inline_map UnsizedType.DataOnly mir.functions_block
  in
  let autodiff_inline_map =
    create_function_inline_map UnsizedType.AutoDiffable mir.functions_block
  in
  let dataonly_inline_function_statements =
    List.map
      ~f:(inline_function_statement UnsizedType.DataOnly dataonly_inline_map)
  in
  let autodiffable_inline_function_statements =
    List.map
      ~f:
        (inline_function_statement UnsizedType.AutoDiffable autodiff_inline_map)
  in
  { mir with
    prepare_data= dataonly_inline_function_statements mir.prepare_data
  ; transform_inits=
      autodiffable_inline_function_statements mir.transform_inits
  ; log_prob= autodiffable_inline_function_statements mir.log_prob
  ; generate_quantities=
      dataonly_inline_function_statements mir.generate_quantities }

let rec contains_top_break_or_continue Stmt.Fixed.({pattern; _}) =
  match pattern with
  | Break | Continue -> true
  | Assignment (_, _)
   |TargetPE _
   |NRFunApp (_, _, _)
   |Return _ | Decl _
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

let unroll_static_loops_statement =
  let f stmt =
    match stmt with
    | Stmt.Fixed.Pattern.For {loopvar; lower; upper; body} -> (
        let lower = Partial_evaluator.eval_expr lower in
        let upper = Partial_evaluator.eval_expr upper in
        match
          (contains_top_break_or_continue body, lower.pattern, upper.pattern)
        with
        | false, Lit (Int, low), Lit (Int, up) ->
            let range =
              List.map
                ~f:(fun i ->
                  Expr.Fixed.
                    { pattern= Lit (Int, Int.to_string i)
                    ; meta=
                        Expr.Typed.Meta.
                          { type_= UInt
                          ; loc= Location_span.empty
                          ; adlevel= DataOnly } } )
                (List.range ~start:`inclusive ~stop:`inclusive
                   (Int.of_string low) (Int.of_string up))
            in
            let stmts =
              List.map
                ~f:(fun i ->
                  subst_args_stmt [loopvar] [i]
                    {pattern= body.pattern; meta= Location_span.empty} )
                range
            in
            Stmt.Fixed.Pattern.SList stmts
        | _ -> stmt )
    | _ -> stmt
  in
  top_down_map_rec_stmt_loc f

let static_loop_unrolling mir =
  transform_program_blockwise mir unroll_static_loops_statement

let unroll_loop_one_step_statement =
  let f stmt =
    match stmt with
    | Stmt.Fixed.Pattern.For {loopvar; lower; upper; body} ->
        if contains_top_break_or_continue body then stmt
        else
          IfElse
            ( Expr.Fixed.
                {lower with pattern= FunApp (StanLib, "Leq__", [lower; upper])}
            , { pattern=
                  Block
                    [ subst_args_stmt [loopvar] [lower]
                        {pattern= body.pattern; meta= Location_span.empty}
                    ; { body with
                        pattern=
                          For
                            { loopvar
                            ; lower=
                                { lower with
                                  pattern=
                                    FunApp
                                      ( StanLib
                                      , "Plus__"
                                      , [lower; Expr.Helpers.loop_bottom] ) }
                            ; upper
                            ; body } } ]
              ; meta= Location_span.empty }
            , None )
    | While (e, body) ->
        if contains_top_break_or_continue body then stmt
        else
          IfElse
            ( e
            , { pattern= Block [body; {body with pattern= While (e, body)}]
              ; meta= Location_span.empty }
            , None )
    | _ -> stmt
  in
  map_rec_stmt_loc f

let one_step_loop_unrolling mir =
  transform_program_blockwise mir unroll_loop_one_step_statement

let collapse_lists_statement =
  let rec collapse_lists l =
    match l with
    | [] -> []
    | Stmt.Fixed.({pattern= SList l'; _}) :: rest -> l' @ collapse_lists rest
    | x :: rest -> x :: collapse_lists rest
  in
  let f = function
    | Stmt.Fixed.Pattern.Block l -> Stmt.Fixed.Pattern.Block (collapse_lists l)
    | SList l -> SList (collapse_lists l)
    | x -> x
  in
  map_rec_stmt_loc f

let list_collapsing (mir : Program.Typed.t) =
  transform_program_blockwise mir collapse_lists_statement

let propagation
    (propagation_transfer :
         (int, Stmt.Located.Non_recursive.t) Map.Poly.t
      -> (module
          Monotone_framework_sigs.TRANSFER_FUNCTION
            with type labels = int
             and type properties = (string, Middle.Expr.Typed.t) Map.Poly.t
                                   option)) (mir : Program.Typed.t) =
  let transform s =
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
          subst_stmt_base
            (Option.value ~default:Map.Poly.empty (Map.find_exn values i).entry)
      )
    in
    propagate_stmt (Map.find_exn flowgraph_to_mir 1)
  in
  transform_program mir transform

let constant_propagation =
  propagation Monotone_framework.constant_propagation_transfer

let rec can_side_effect_expr (e : Expr.Typed.t) =
  match e.pattern with
  | Var _ | Lit (_, _) -> false
  | FunApp (t, f, es) ->
      String.suffix f 3 = "_lp"
      || List.exists ~f:can_side_effect_expr es
      || (t = CompilerInternal && f = Internal_fun.to_string FnReadParam)
      || (t = CompilerInternal && f = Internal_fun.to_string FnWriteParam)
      || (t = CompilerInternal && f = Internal_fun.to_string FnUnconstrain)
  | TernaryIf (e1, e2, e3) -> List.exists ~f:can_side_effect_expr [e1; e2; e3]
  | Indexed (e, is) ->
      can_side_effect_expr e || List.exists ~f:can_side_effect_idx is
  | EAnd (e1, e2) | EOr (e1, e2) -> List.exists ~f:can_side_effect_expr [e1; e2]

and can_side_effect_idx (i : Expr.Typed.t Index.t) =
  match i with
  | All -> false
  | Single e | Upfrom e | MultiIndex e -> can_side_effect_expr e
  | Between (e1, e2) -> can_side_effect_expr e1 || can_side_effect_expr e2

let expression_propagation =
  propagation
    (Monotone_framework.expression_propagation_transfer can_side_effect_expr)

let copy_propagation = propagation Monotone_framework.copy_propagation_transfer

let is_skip_break_continue s =
  match s with
  | Stmt.Fixed.Pattern.Skip | Break | Continue -> true
  | _ -> false

(* TODO: could also implement partial dead code elimination *)
let dead_code_elimination (mir : Program.Typed.t) =
  (* TODO: think about whether we should treat function bodies as local scopes in the statement
   from the POV of a live variables analysis.
   (Obviously, this shouldn't be the case for the purposes of reaching definitions,
   constant propagation, expressions analyses. But I do think that's the right way to
   go about live variables. *)
  let transform s =
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
      | Stmt.Fixed.Pattern.Assignment ((x, _, []), rhs) ->
          if Set.Poly.mem live_variables_s x || can_side_effect_expr rhs then
            stmt
          else Skip
      | Assignment ((x, _, is), rhs) ->
          if
            Set.Poly.mem live_variables_s x
            || can_side_effect_expr rhs
            || List.exists ~f:can_side_effect_idx is
          then stmt
          else Skip
      (* NOTE: we never get rid of declarations as we might not be able to
        remove an assignment to a variable
           due to side effects. *)
      (* TODO: maybe we should revisit that. *)
      | Decl _ | TargetPE _
       |NRFunApp (_, _, _)
       |Break | Continue | Return _ | Skip ->
          stmt
      | IfElse (e, b1, b2) -> (
          if
            (* TODO: check if e has side effects, like print, reject, then don't optimize? *)
            (not (can_side_effect_expr e))
            && b1.Stmt.Fixed.pattern = Skip
            && ( Option.map ~f:(fun Stmt.Fixed.({pattern; _}) -> pattern) b2
                 = Some Skip
               || Option.map ~f:(fun Stmt.Fixed.({pattern; _}) -> pattern) b2
                  = None )
          then Skip
          else
            match e.pattern with
            | Lit (Int, "0") | Lit (Real, "0.0") -> (
              match b2 with Some x -> x.pattern | None -> Skip )
            | Lit (_, _) -> b1.pattern
            | _ -> IfElse (e, b1, b2) )
      | While (e, b) -> (
          if (not (can_side_effect_expr e)) && b.pattern = Break then Skip
          else
            match e.pattern with
            | Lit (Int, "0") | Lit (Real, "0.0") -> Skip
            | _ -> While (e, b) )
      | For {loopvar; lower; upper; body} ->
          if
            (not (can_side_effect_expr lower))
            && (not (can_side_effect_expr upper))
            && is_skip_break_continue body.pattern
          then Skip
          else For {loopvar; lower; upper; body}
      | Block l ->
          let l' = List.filter ~f:(fun x -> x.pattern <> Skip) l in
          if List.length l' = 0 then Skip else Block l'
      | SList l ->
          let l' = List.filter ~f:(fun x -> x.pattern <> Skip) l in
          SList l'
    in
    let dead_code_elim_stmt =
      map_rec_stmt_loc_num flowgraph_to_mir dead_code_elim_stmt_base
    in
    dead_code_elim_stmt (Map.find_exn flowgraph_to_mir 1)
  in
  transform_program mir transform

let partial_evaluation = Partial_evaluator.eval_prog

let lazy_code_motion (mir : Program.Typed.t) =
  (* TODO: clean up this code. It is not very pretty. *)
  (* TODO: make lazy code motion operate on transformed parameters and models blocks
     simultaneously *)
  let preprocess_flowgraph =
    let preprocess_flowgraph_base
        (stmt : (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t) =
      match stmt with
      | IfElse (e, b1, Some b2) ->
          Stmt.Fixed.(
            Pattern.IfElse
              ( e
              , { pattern=
                    Block [b1; {pattern= Skip; meta= Location_span.empty}]
                ; meta= Location_span.empty }
              , Some
                  { pattern=
                      Block [b2; {pattern= Skip; meta= Location_span.empty}]
                  ; meta= Location_span.empty } ))
      | IfElse (e, b, None) ->
          IfElse
            ( e
            , { pattern= Block [b; {pattern= Skip; meta= Location_span.empty}]
              ; meta= Location_span.empty }
            , Some {pattern= Skip; meta= Location_span.empty} )
      | While (e, b) ->
          While
            ( e
            , { pattern= Block [b; {pattern= Skip; meta= Location_span.empty}]
              ; meta= Location_span.empty } )
      | For {loopvar; lower; upper; body= b} ->
          For
            { loopvar
            ; lower
            ; upper
            ; body=
                { pattern= Block [b; {pattern= Skip; meta= Location_span.empty}]
                ; meta= Location_span.empty } }
      | _ -> stmt
    in
    map_rec_stmt_loc preprocess_flowgraph_base
  in
  let transform s =
    let rev_flowgraph, flowgraph_to_mir =
      Monotone_framework.inverse_flowgraph_of_stmt s
    in
    let fwd_flowgraph = Monotone_framework.reverse rev_flowgraph in
    let latest_expr, used_not_latest_expressions_mfp =
      Monotone_framework.lazy_expressions_mfp fwd_flowgraph rev_flowgraph
        flowgraph_to_mir
    in
    let expression_map =
      Set.fold (Monotone_framework.used_expressions_stmt s.pattern)
        ~init:Expr.Typed.Map.empty ~f:(fun accum e ->
          match e.pattern with
          | Lit (_, _) -> accum
          | _ when can_side_effect_expr e -> accum
          | _ -> Map.set accum ~key:e ~data:(Gensym.generate ()) )
    in
    (* TODO: it'd be more efficient to just not accumulate constants in the static analysis *)
    let declarations_list =
      Map.fold expression_map ~init:[] ~f:(fun ~key ~data accum ->
          Stmt.Fixed.
            { pattern=
                Pattern.Decl
                  { decl_adtype= Expr.Typed.adlevel_of key
                  ; decl_id= data
                  ; decl_type= Type.Unsized (Expr.Typed.type_of key) }
            ; meta= Location_span.empty }
          :: accum )
    in
    let lazy_code_motion_base i stmt =
      let latest_and_used_after_i =
        Set.inter
          (Map.find_exn latest_expr i)
          (Map.find_exn used_not_latest_expressions_mfp i).entry
      in
      let to_assign_in_s =
        Set.filter
          ~f:(fun x -> Map.mem expression_map x)
          latest_and_used_after_i
      in
      let to_assign_in_s = Set.to_list to_assign_in_s in
      let to_assign_in_s =
        List.sort
          ~compare:(fun e e' -> compare_int (expr_depth e) (expr_depth e'))
          to_assign_in_s
      in
      (* TODO: is this sort doing anything or are they already stored in the right order by
         chance? It appears to not do anything. *)
      let assignments_to_add_to_s =
        List.map
          ~f:(fun e ->
            Stmt.Fixed.
              { pattern=
                  Assignment
                    ((Map.find_exn expression_map e, e.meta.type_, []), e)
              ; meta= Location_span.empty } )
          to_assign_in_s
      in
      let expr_subst_stmt_except_initial_assign m =
        let f stmt =
          match stmt with
          | Stmt.Fixed.Pattern.Assignment ((x, _, []), e')
            when Map.mem m e'
                 && Expr.Typed.equal {e' with pattern= Var x}
                      (Map.find_exn m e') ->
              expr_subst_stmt_base (Map.remove m e') stmt
          | _ -> expr_subst_stmt_base m stmt
        in
        map_rec_stmt_loc f
      in
      let f =
        expr_subst_stmt_except_initial_assign
          (Map.filter_keys
             ~f:(fun key ->
               Set.mem latest_and_used_after_i key
               || Set.mem (Map.find_exn used_not_latest_expressions_mfp i).exit
                    key )
             (Map.mapi expression_map ~f:(fun ~key ~data ->
                  {key with pattern= Var data} )))
      in
      if List.length assignments_to_add_to_s = 0 then
        (f Stmt.Fixed.{pattern= stmt; meta= Location_span.empty}).pattern
      else
        SList
          (List.map ~f
             ( assignments_to_add_to_s
             @ [{pattern= stmt; meta= Location_span.empty}] ))
    in
    let lazy_code_motion_stmt =
      map_rec_stmt_loc_num flowgraph_to_mir lazy_code_motion_base
    in
    Stmt.Fixed.
      { pattern=
          SList
            ( declarations_list
            @ [lazy_code_motion_stmt (Map.find_exn flowgraph_to_mir 1)] )
      ; meta= Location_span.empty }
  in
  transform_program_blockwise mir (fun x -> transform (preprocess_flowgraph x))

let block_fixing mir =
  transform_program_blockwise mir
    (map_rec_stmt_loc (fun stmt ->
         match stmt with
         | IfElse
             ( e
             , {pattern= SList l; meta}
             , Some {pattern= SList l'; meta= smeta'} ) ->
             IfElse
               ( e
               , {pattern= Block l; meta}
               , Some {pattern= Block l'; meta= smeta'} )
         | IfElse (e, {pattern= SList l; meta}, b) ->
             IfElse (e, {pattern= Block l; meta}, b)
         | IfElse (e, b, Some {pattern= SList l'; meta= smeta'}) ->
             IfElse (e, b, Some {pattern= Block l'; meta= smeta'})
         | While (e, {pattern= SList l; meta}) ->
             While (e, {pattern= Block l; meta})
         | For {loopvar; lower; upper; body= {pattern= SList l; meta}} ->
             For {loopvar; lower; upper; body= {pattern= Block l; meta}}
         | _ -> stmt ))

(* TODO: implement SlicStan style optimizer for choosing best program block for each statement. *)
(* TODO: add optimization pass to move declarations down as much as possible and introduce as
   tight as possible local scopes *)
(* TODO: add tests *)
(* TODO: add pass to get rid of redundant declarations? *)

let optimize_ad_levels (mir : Program.Typed.t) =
  let transform s =
    let rev_flowgraph, flowgraph_to_mir =
      Monotone_framework.inverse_flowgraph_of_stmt s
    in
    let fwd_flowgraph = Monotone_framework.reverse rev_flowgraph in
    let (module Rev_Flowgraph) = rev_flowgraph in
    let (module Fwd_Flowgraph) = fwd_flowgraph in
    let initial_ad_variables =
      Set.Poly.of_list
        (List.filter_map
           ~f:(fun (v, Program.({out_block; _})) ->
             match out_block with Parameters -> Some v | _ -> None )
           mir.output_vars)
    in
    let ad_levels =
      Monotone_framework.autodiff_level_mfp
        (module Fwd_Flowgraph)
        (module Rev_Flowgraph)
        flowgraph_to_mir initial_ad_variables
    in
    let insert_constraint_variables vars =
      Set.Poly.union vars (Set.Poly.map ~f:(fun x -> x ^ "_in__") vars)
    in
    let optimize_ad_levels_stmt_base i stmt =
      let autodiffable_variables =
        insert_constraint_variables (Map.find_exn ad_levels i).exit
      in
      match
        Stmt.Fixed.Pattern.map
          (update_expr_ad_levels autodiffable_variables)
          (fun x -> x)
          stmt
      with
      | Decl {decl_id; decl_type; _}
        when Set.mem autodiffable_variables decl_id ->
          Stmt.Fixed.Pattern.Decl
            {decl_adtype= AutoDiffable; decl_id; decl_type}
      | Decl {decl_id; decl_type; _} ->
          Decl {decl_adtype= DataOnly; decl_id; decl_type}
      | s -> s
    in
    let optimize_ad_levels_stmt =
      map_rec_stmt_loc_num flowgraph_to_mir optimize_ad_levels_stmt_base
    in
    optimize_ad_levels_stmt (Map.find_exn flowgraph_to_mir 1)
  in
  transform_program_blockwise mir transform

(* Apparently you need to completely copy/paste type definitions between
   ml and mli files?*)
type optimization_settings =
  { function_inlining: bool
  ; static_loop_unrolling: bool
  ; one_step_loop_unrolling: bool
  ; list_collapsing: bool
  ; block_fixing: bool
  ; constant_propagation: bool
  ; expression_propagation: bool
  ; copy_propagation: bool
  ; dead_code_elimination: bool
  ; partial_evaluation: bool
  ; lazy_code_motion: bool
  ; optimize_ad_levels: bool }

let optimization_suite settings mir =
  let maybe_optimizations =
    [ (* Phase order. See phase-ordering-nodes.org for details *)
      (* Book section A *)
      (* Book section B *)
      (* Book: Procedure integration *)
      (function_inlining, settings.function_inlining)
      (* Book: Sparse conditional constant propagation *)
    ; (constant_propagation, settings.constant_propagation)
      (* Book section C *)
      (* Book: Local and global copy propagation *)
    ; (copy_propagation, settings.copy_propagation)
      (* Book: Sparse conditional constant propagation *)
    ; (constant_propagation, settings.constant_propagation)
      (* Book: Dead-code elimination *)
    ; (dead_code_elimination, settings.dead_code_elimination)
      (* Matthijs: Before lazy code motion to get loop-invariant code motion *)
    ; (one_step_loop_unrolling, settings.one_step_loop_unrolling)
      (* Matthjis: expression_propagation < partial_evaluation *)
    ; (expression_propagation, settings.expression_propagation)
      (* Matthjis: partial_evaluation < lazy_code_motion *)
    ; (partial_evaluation, settings.partial_evaluation)
      (* Book: Loop-invariant code motion *)
    ; (lazy_code_motion, settings.lazy_code_motion)
      (* Matthijs: lazy_code_motion < copy_propagation TODO: Check if this is necessary *)
    ; (copy_propagation, settings.copy_propagation)
      (* Matthijs: Constant propagation before static loop unrolling *)
    ; (constant_propagation, settings.constant_propagation)
      (* Book: Loop simplification *)
    ; (static_loop_unrolling, settings.static_loop_unrolling)
      (* Book: Dead-code elimination *)
      (* Matthijs: Everything < Dead-code elimination *)
    ; (dead_code_elimination, settings.dead_code_elimination)
      (* Book: Machine idioms and instruction combining *)
    ; (list_collapsing, settings.list_collapsing)
      (* Book: Machine idioms and instruction combining *)
    ; (optimize_ad_levels, settings.optimize_ad_levels)
      (* Book: Machine idioms and instruction combining *)
      (* Matthijs: Everything < block_fixing *)
    ; (block_fixing, settings.block_fixing) ]
  in
  let optimizations =
    List.filter_map maybe_optimizations ~f:(fun (fn, flag) ->
        if flag then Some fn else None )
  in
  List.fold optimizations ~init:mir ~f:(fun mir opt -> opt mir)
