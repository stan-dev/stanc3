(* Code for optimization passes on the MIR *)
open Core_kernel
open Middle
open Mir_utils

(**
   Apply the transformation to each function body and to the rest of the program as one
   block.
*)
let transform_program (mir : typed_prog) (transform : stmt_loc -> stmt_loc) :
    typed_prog =
  let packed_prog_body =
    transform
      { stmt=
          SList
            (List.map
               ~f:(fun x -> {stmt= SList x; smeta= Middle.no_span})
               [ mir.prepare_data; mir.transform_inits; mir.log_prob
               ; mir.generate_quantities ])
      ; smeta= Middle.no_span }
  in
  let transformed_prog_body = transform packed_prog_body in
  let transformed_functions =
    List.map mir.functions_block ~f:(fun fs ->
        {fs with fdbody= transform fs.fdbody} )
  in
  match transformed_prog_body with
  | { stmt=
        SList
          [ {stmt= SList prepare_data'; _}
          ; {stmt= SList transform_inits'; _}
          ; {stmt= SList log_prob'; _}
          ; {stmt= SList generate_quantities'; _} ]; _ } ->
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
let transform_program_blockwise (mir : typed_prog)
    (transform : stmt_loc -> stmt_loc) : typed_prog =
  let transform' s =
    match transform {stmt= SList s; smeta= Middle.no_span} with
    | {stmt= SList l; _} -> l
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

let map_no_loc l = List.map ~f:(fun s -> {stmt= s; smeta= Middle.no_span}) l
let slist_no_loc l = SList (map_no_loc l)
let block_no_loc l = Block (map_no_loc l)

let slist_concat_no_loc l stmt =
  match l with [] -> stmt | l -> slist_no_loc (l @ [stmt])

let replace_fresh_local_vars s' =
  let f m = function
    | Decl {decl_adtype; decl_type; decl_id} ->
        let fresh_name = gensym () in
        ( Decl {decl_adtype; decl_id= fresh_name; decl_type}
        , Map.Poly.set m ~key:decl_id
            ~data:
              { expr= Var fresh_name
              ; emeta=
                  { mtype= remove_possible_size decl_type
                  ; madlevel= decl_adtype
                  ; mloc= Middle.no_span } } )
    | Assignment ((var_name, ut, l), e) ->
        let var_name =
          match Map.Poly.find m var_name with
          | None -> var_name
          | Some {expr= Var var_name; _} -> var_name
          | Some e -> raise_s [%sexp (e : expr_typed_located)]
        in
        (Assignment ((var_name, ut, l), e), m)
    | x -> (x, m)
  in
  let s, m = map_rec_state_stmt_loc f Map.Poly.empty s' in
  subst_stmt m s

let replace_fresh_local_vars_triple (d_list, s_list, e) =
  let s =
    slist_no_loc
      ([slist_no_loc d_list] @ [slist_no_loc s_list] @ [Return (Some e)])
  in
  let s = (replace_fresh_local_vars {stmt= s; smeta= no_span}).stmt in
  match s with
  | SList
      [ {stmt= SList d_list; _}
      ; {stmt= SList s_list; _}
      ; {stmt= Return (Some e); _} ] ->
      ( List.map ~f:(fun x -> x.stmt) d_list
      , List.map ~f:(fun x -> x.stmt) s_list
      , e )
  | _ ->
      raise_s
        [%sexp
          ( s
            : ( mtype_loc_ad with_expr
              , (mtype_loc_ad, location_span) stmt_with )
              statement )]

let subst_args_stmt args es =
  let m = Map.Poly.of_alist_exn (List.zip_exn args es) in
  subst_stmt m

(* TODO: only handle early returns if that's necessary *)
let handle_early_returns opt_triple b =
  let f = function
    | Return opt_ret -> (
      match (opt_triple, opt_ret) with
      | None, None -> Break
      | Some (Some _, _, name), Some e ->
          SList
            [ { stmt= Assignment ((name, e.emeta.mtype, []), e)
              ; smeta= Middle.no_span }
            ; {stmt= Break; smeta= Middle.no_span} ]
      | _, _ -> raise_s [%sexp ("" : string)] )
    | x -> x
  in
  For
    { loopvar= gensym ()
    ; lower=
        { expr= Lit (Int, "1")
        ; emeta= {mtype= UInt; madlevel= DataOnly; mloc= Middle.no_span} }
    ; upper=
        { expr= Lit (Int, "1")
        ; emeta= {mtype= UInt; madlevel= DataOnly; mloc= Middle.no_span} }
    ; body= map_rec_stmt_loc f b }

let rec inline_function_expression adt fim e =
  match e.expr with
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
      | None -> (d_list, s_list, {e with expr= FunApp (t, s, es)})
      | Some (rt, args, b) ->
          let x = gensym () in
          let b = handle_early_returns (Some (rt, adt, x)) b in
          let d_list2, s_list2, e =
            replace_fresh_local_vars_triple
              ( [ Decl
                    { decl_adtype= adt
                    ; decl_id= x
                    ; decl_type= Option.value_exn rt } ]
              , [ (subst_args_stmt args es {stmt= b; smeta= Middle.no_span})
                    .stmt ]
              , { expr= Var x
                ; emeta=
                    { mtype= remove_possible_size (Option.value_exn rt)
                    ; madlevel= adt
                    ; mloc= Middle.no_span } } )
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
        @ [ IfElse
              ( e1
              , {stmt= block_no_loc sl2; smeta= Middle.no_span}
              , Some {stmt= block_no_loc sl3; smeta= Middle.no_span} ) ]
      , {e with expr= TernaryIf (e1, e2, e3)} )
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
      (d_list @ dl, s_list @ sl, {e with expr= Indexed (e', i_list)})
  | EAnd (e1, e2) ->
      let dl1, sl1, e1 = inline_function_expression adt fim e1 in
      let dl2, sl2, e2 = inline_function_expression adt fim e2 in
      let sl2 =
        [IfElse (e1, {stmt= Block (map_no_loc sl2); smeta= no_span}, None)]
      in
      (dl1 @ dl2, sl1 @ sl2, {e with expr= EAnd (e1, e2)})
  | EOr (e1, e2) ->
      let dl1, sl1, e1 = inline_function_expression adt fim e1 in
      let dl2, sl2, e2 = inline_function_expression adt fim e2 in
      let sl2 =
        [ IfElse
            ( e1
            , {stmt= Skip; smeta= no_span}
            , Some {stmt= Block (map_no_loc sl2); smeta= no_span} ) ]
      in
      (dl1 @ dl2, sl1 @ sl2, {e with expr= EOr (e1, e2)})

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

let rec inline_function_statement adt fim {stmt; smeta} =
  { stmt=
      ( match stmt with
      | Assignment ((x, ut, l), e2) ->
          let e1 = {e2 with expr= Indexed ({e2 with expr= Var x}, l)} in
          (* This inner e2 is wrong. We are giving the wrong type to Var x. But it doens't really matter as we discard it later. *)
          let dl1, sl1, e1 = inline_function_expression adt fim e1 in
          let dl2, sl2, e2 = inline_function_expression adt fim e2 in
          let x, l =
            match e1.expr with
            | Var x -> (x, [])
            | Indexed ({expr= Var x; _}, l) -> (x, l)
            | _ as w -> raise_s [%sexp (w : mtype_loc_ad with_expr expr)]
          in
          slist_concat_no_loc
            (dl2 @ dl1 @ sl2 @ sl1)
            (Assignment ((x, ut, l), e2))
      | TargetPE e ->
          let d, s, e = inline_function_expression adt fim e in
          slist_concat_no_loc (d @ s) (TargetPE e)
      | NRFunApp (t, s, es) ->
          let dse_list = List.map ~f:(inline_function_expression adt fim) es in
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
                (subst_args_stmt args es {stmt= b; smeta= Middle.no_span}).stmt
            )
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
                     { stmt=
                         Block
                           ( [inline_function_statement adt fim s]
                           @ map_no_loc s' )
                     ; smeta= Middle.no_span } ))
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
                       { stmt=
                           Block
                             ( [inline_function_statement adt fim body]
                             @ map_no_loc s_upper )
                       ; smeta= Middle.no_span } ) })
      | Block l -> Block (List.map l ~f:(inline_function_statement adt fim))
      | SList l -> SList (List.map l ~f:(inline_function_statement adt fim))
      | Decl r -> Decl r
      | Skip -> Skip
      | Break -> Break
      | Continue -> Continue )
  ; smeta }

let create_function_inline_map adt l =
  (* We only add the first definition for each function to the inline map.
   This will make sure we do not inline recursive functions. *)
  let f accum fundef =
    match fundef with {fdname; fdargs; fdbody; fdrt; _} -> (
      match
        Map.add accum ~key:fdname
          ~data:
            ( Option.map ~f:(fun x -> Unsized x) fdrt
            , List.map ~f:(fun (_, name, _) -> name) fdargs
            , inline_function_statement adt accum fdbody )
      with
      | `Ok m -> m
      | `Duplicate -> accum )
  in
  Map.filter
    ~f:(fun (_, _, v) -> v.stmt <> Skip)
    (List.fold l ~init:Map.Poly.empty ~f)

let function_inlining (mir : typed_prog) =
  let dataonly_inline_map =
    create_function_inline_map DataOnly mir.functions_block
  in
  let autodiff_inline_map =
    create_function_inline_map AutoDiffable mir.functions_block
  in
  let dataonly_inline_function_statements =
    List.map ~f:(inline_function_statement DataOnly dataonly_inline_map)
  in
  let autodiffable_inline_function_statements =
    List.map ~f:(inline_function_statement AutoDiffable autodiff_inline_map)
  in
  { mir with
    prepare_data= dataonly_inline_function_statements mir.prepare_data
  ; transform_inits=
      autodiffable_inline_function_statements mir.transform_inits
  ; log_prob= autodiffable_inline_function_statements mir.log_prob
  ; generate_quantities=
      dataonly_inline_function_statements mir.generate_quantities }

let rec contains_top_break_or_continue {stmt; _} =
  match stmt with
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
    | For {loopvar; lower; upper; body} -> (
        let lower = Partial_evaluator.eval_expr lower in
        let upper = Partial_evaluator.eval_expr upper in
        match
          (contains_top_break_or_continue body, lower.expr, upper.expr)
        with
        | false, Lit (Int, low), Lit (Int, up) ->
            let range =
              List.map
                ~f:(fun i ->
                  { expr= Lit (Int, Int.to_string i)
                  ; emeta=
                      {mtype= UInt; mloc= Middle.no_span; madlevel= DataOnly}
                  } )
                (List.range ~start:`inclusive ~stop:`inclusive
                   (Int.of_string low) (Int.of_string up))
            in
            let stmts =
              List.map
                ~f:(fun i ->
                  subst_args_stmt [loopvar] [i]
                    {stmt= body.stmt; smeta= Middle.no_span} )
                range
            in
            SList stmts
        | _ -> stmt )
    | _ -> stmt
  in
  top_down_map_rec_stmt_loc f

let static_loop_unrolling mir =
  transform_program_blockwise mir unroll_static_loops_statement

let unroll_loop_one_step_statement =
  let f stmt =
    match stmt with
    | For {loopvar; lower; upper; body} ->
        if contains_top_break_or_continue body then stmt
        else
          IfElse
            ( {lower with expr= FunApp (StanLib, "Leq__", [lower; upper])}
            , { stmt=
                  Block
                    [ subst_args_stmt [loopvar] [lower]
                        {stmt= body.stmt; smeta= Middle.no_span}
                    ; { body with
                        stmt=
                          For
                            { loopvar
                            ; lower=
                                { lower with
                                  expr=
                                    FunApp
                                      (StanLib, "Plus__", [lower; loop_bottom])
                                }
                            ; upper
                            ; body } } ]
              ; smeta= no_span }
            , None )
    | While (e, body) ->
        if contains_top_break_or_continue body then stmt
        else
          IfElse
            ( e
            , { stmt= Block [body; {body with stmt= While (e, body)}]
              ; smeta= no_span }
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
    | {stmt= SList l'; _} :: rest -> l' @ collapse_lists rest
    | x :: rest -> x :: collapse_lists rest
  in
  let f = function
    | Block l -> Block (collapse_lists l)
    | SList l -> SList (collapse_lists l)
    | x -> x
  in
  map_rec_stmt_loc f

let list_collapsing (mir : typed_prog) =
  transform_program_blockwise mir collapse_lists_statement

let propagation
    (propagation_transfer :
         (int, Middle.stmt_loc_num) Map.Poly.t
      -> (module
          Monotone_framework_sigs.TRANSFER_FUNCTION
            with type labels = int
             and type properties = ( string
                                   , Middle.expr_typed_located )
                                   Map.Poly.t
                                   option)) (mir : typed_prog) =
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

let expression_propagation =
  propagation Monotone_framework.expression_propagation_transfer

let copy_propagation = propagation Monotone_framework.copy_propagation_transfer

let rec can_side_effect_expr (e : expr_typed_located) =
  match e.expr with
  | Var _ | Lit (_, _) -> false
  | FunApp (_, f, es) ->
      String.suffix f 3 = "_lp" || List.exists ~f:can_side_effect_expr es
  | TernaryIf (e1, e2, e3) -> List.exists ~f:can_side_effect_expr [e1; e2; e3]
  | Indexed (e, is) ->
      can_side_effect_expr e || List.exists ~f:can_side_effect_idx is
  | EAnd (e1, e2) | EOr (e1, e2) -> List.exists ~f:can_side_effect_expr [e1; e2]

and can_side_effect_idx (i : expr_typed_located index) =
  match i with
  | All -> false
  | Single e | Upfrom e | MultiIndex e -> can_side_effect_expr e
  | Between (e1, e2) -> can_side_effect_expr e1 || can_side_effect_expr e2

let is_skip_break_continue s =
  match s with Skip | Break | Continue -> true | _ -> false

(* TODO: could also implement partial dead code elimination *)
let dead_code_elimination (mir : typed_prog) =
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
      | Assignment ((x, _, []), rhs) ->
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
            && b1.stmt = Skip
            && ( Option.map ~f:(fun x -> x.stmt) b2 = Some Skip
               || Option.map ~f:(fun x -> x.stmt) b2 = None )
          then Skip
          else
            match e.expr with
            | Lit (Int, "0") | Lit (Real, "0.0") -> (
              match b2 with Some x -> x.stmt | None -> Skip )
            | Lit (_, _) -> b1.stmt
            | _ -> IfElse (e, b1, b2) )
      | While (e, b) -> (
          if (not (can_side_effect_expr e)) && b.stmt = Break then Skip
          else
            match e.expr with
            | Lit (Int, "0") | Lit (Real, "0.0") -> Skip
            | _ -> While (e, b) )
      | For {loopvar; lower; upper; body} ->
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
    in
    let dead_code_elim_stmt =
      map_rec_stmt_loc_num flowgraph_to_mir dead_code_elim_stmt_base
    in
    dead_code_elim_stmt (Map.find_exn flowgraph_to_mir 1)
  in
  transform_program mir transform

let partial_evaluation = Partial_evaluator.eval_prog

let lazy_code_motion (mir : typed_prog) =
  (* TODO: clean up this code. It is not very pretty. *)
  (* TODO: make lazy code motion operate on transformed parameters and models blocks
     simultaneously *)
  let preprocess_flowgraph =
    let preprocess_flowgraph_base
        (stmt : (expr_typed_located, stmt_loc) statement) =
      match stmt with
      | IfElse (e, b1, Some b2) ->
          IfElse
            ( e
            , {stmt= Block [b1; {stmt= Skip; smeta= no_span}]; smeta= no_span}
            , Some
                {stmt= Block [b2; {stmt= Skip; smeta= no_span}]; smeta= no_span}
            )
      | IfElse (e, b, None) ->
          IfElse
            ( e
            , {stmt= Block [b; {stmt= Skip; smeta= no_span}]; smeta= no_span}
            , Some {stmt= Skip; smeta= no_span} )
      | While (e, b) ->
          While
            (e, {stmt= Block [b; {stmt= Skip; smeta= no_span}]; smeta= no_span})
      | For {loopvar; lower; upper; body= b} ->
          For
            { loopvar
            ; lower
            ; upper
            ; body=
                {stmt= Block [b; {stmt= Skip; smeta= no_span}]; smeta= no_span}
            }
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
      Set.fold (Monotone_framework.used_expressions_stmt s.stmt)
        ~init:ExprMap.empty ~f:(fun accum e ->
          match e.expr with
          | Lit (_, _) -> accum
          | _ when can_side_effect_expr e -> accum
          | _ -> Map.set accum ~key:e ~data:(gensym ()) )
    in
    (* TODO: it'd be more efficient to just not accumulate constants in the static analysis *)
    let declarations_list =
      Map.fold expression_map ~init:[] ~f:(fun ~key ~data accum ->
          { stmt=
              Middle.Decl
                { decl_adtype= key.emeta.madlevel
                ; decl_id= data
                ; decl_type= Unsized key.emeta.mtype }
          ; smeta= Middle.no_span }
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
            { stmt=
                Assignment
                  ((Map.find_exn expression_map e, e.emeta.mtype, []), e)
            ; smeta= Middle.no_span } )
          to_assign_in_s
      in
      let expr_subst_stmt_except_initial_assign m =
        let f stmt =
          match stmt with
          | Assignment ((x, _, []), e')
            when Map.mem m e'
                 && Middle.compare_expr_typed_located {e' with expr= Var x}
                      (Map.find_exn m e')
                    = 0 ->
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
                  {key with expr= Var data} )))
      in
      if List.length assignments_to_add_to_s = 0 then
        (f {stmt; smeta= Middle.no_span}).stmt
      else
        SList
          (List.map ~f
             (assignments_to_add_to_s @ [{stmt; smeta= Middle.no_span}]))
    in
    let lazy_code_motion_stmt =
      map_rec_stmt_loc_num flowgraph_to_mir lazy_code_motion_base
    in
    { stmt=
        SList
          ( declarations_list
          @ [lazy_code_motion_stmt (Map.find_exn flowgraph_to_mir 1)] )
    ; smeta= Middle.no_span }
  in
  transform_program_blockwise mir (fun x -> transform (preprocess_flowgraph x))

let block_fixing mir =
  transform_program_blockwise mir
    (map_rec_stmt_loc (fun stmt ->
         match stmt with
         | IfElse
             (e, {stmt= SList l; smeta}, Some {stmt= SList l'; smeta= smeta'})
           ->
             IfElse
               (e, {stmt= Block l; smeta}, Some {stmt= Block l'; smeta= smeta'})
         | IfElse (e, {stmt= SList l; smeta}, b) ->
             IfElse (e, {stmt= Block l; smeta}, b)
         | IfElse (e, b, Some {stmt= SList l'; smeta= smeta'}) ->
             IfElse (e, b, Some {stmt= Block l'; smeta= smeta'})
         | While (e, {stmt= SList l; smeta}) ->
             While (e, {stmt= Block l; smeta})
         | For {loopvar; lower; upper; body= {stmt= SList l; smeta}} ->
             For {loopvar; lower; upper; body= {stmt= Block l; smeta}}
         | _ -> stmt ))

(* TODO: implement SlicStan style optimizer for choosing best program block for each statement. *)
(* TODO: add optimization pass to move declarations down as much as possible and introduce as
   tight as possible local scopes *)
(* TODO: add tests *)
(* TODO: add pass to get rid of redundant declarations? *)

let optimize_ad_levels mir =
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
           ~f:(fun (v, {out_block; _}) ->
             match out_block with Parameters -> Some v | _ -> None )
           mir.output_vars)
    in
    let ad_levels =
      Monotone_framework.autodiff_level_mfp
        (module Fwd_Flowgraph)
        (module Rev_Flowgraph)
        flowgraph_to_mir initial_ad_variables
    in
    let optimize_ad_levels_stmt_base i stmt =
      let autodiffable_variables = (Map.find_exn ad_levels i).exit in
      match
        map_statement
          (update_expr_ad_levels autodiffable_variables)
          (fun x -> x)
          stmt
      with
      | Decl {decl_id; decl_type; _}
        when Set.mem autodiffable_variables decl_id ->
          Decl {decl_adtype= AutoDiffable; decl_id; decl_type}
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
