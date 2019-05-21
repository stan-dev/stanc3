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
    | x -> (x, m)
  in
  let s, m = map_rec_state_stmt_loc f Map.Poly.empty s' in
  let _ = print_s[%sexp (m : (string, mtype_loc_ad with_expr) Map.Poly.t) ] in
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
  | _ -> Errors.fatal_error ()

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
            [ {stmt= Assignment ((name, []), e); smeta= Middle.no_span}
            ; {stmt= Break; smeta= Middle.no_span} ]
      | _, _ -> Errors.fatal_error () )
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
  | Downfrom e ->
      let dl, sl, e = inline_function_expression adt fim e in
      (dl, sl, Downfrom e)
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
      | Assignment ((x, l), e2) ->
          let e1 = {e2 with expr= Indexed ({e2 with expr= Var x}, l)} in
          (* This inner e2 is wrong. We are giving the wrong type to Var x. But it doens't really matter as we discard it later. *)
          let dl1, sl1, e1 = inline_function_expression adt fim e1 in
          let dl2, sl2, e2 = inline_function_expression adt fim e2 in
          let x, l =
            match e1.expr with
            | Var x -> (x, [])
            | Indexed ({expr= Var x; _}, l) -> (x, l)
            | _ -> Errors.fatal_error ()
          in
          slist_concat_no_loc (dl2 @ dl1 @ sl2 @ sl1) (Assignment ((x, l), e2))
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
      match (contains_top_break_or_continue body, lower.expr, upper.expr) with
      | false, Lit (Int, low), Lit (Int, up) ->
          let range =
            List.map
              ~f:(fun i ->
                { expr= Lit (Int, Int.to_string i)
                ; emeta= {mtype= UInt; mloc= Middle.no_span; madlevel= DataOnly}
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
  map_rec_stmt_loc f

let static_loop_unrolling mir = transform_program_blockwise mir
unroll_static_loops_statement

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
  | Single e | Upfrom e | Downfrom e | MultiIndex e -> can_side_effect_expr e
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
      | Assignment ((x, []), rhs) ->
          if Set.Poly.mem live_variables_s x || can_side_effect_expr rhs then
            stmt
          else Skip
      | Assignment ((x, is), rhs) ->
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
        match e.expr with
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
            { stmt= Assignment ((Map.find_exn expression_map e, []), e)
            ; smeta= Middle.no_span } )
          to_assign_in_s
      in
      let expr_subst_stmt_except_initial_assign m =
        let f stmt =
          match stmt with
          | Assignment ((x, []), e')
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

let%expect_test "map_rec_stmt_loc" =
  let _ = gensym_reset_danger_use_cautiously () in
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
    | NRFunApp (CompilerInternal, "FnPrint__", [s]) ->
        NRFunApp (CompilerInternal, "FnPrint__", [s; s])
    | x -> x
  in
  let mir = map_prog (fun x -> x) (map_rec_stmt_loc f) mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        FnPrint__(24, 24);
        if(13) {
          FnPrint__(244, 244);
          if(24) {
            FnPrint__(24, 24);
            }
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "map_rec_state_stmt_loc" =
  let _ = gensym_reset_danger_use_cautiously () in
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
    | NRFunApp (CompilerInternal, "FnPrint__", [s]) ->
        (NRFunApp (CompilerInternal, "FnPrint__", [s; s]), i + 1)
    | x -> (x, i)
  in
  let mir_stmt, num =
    (map_rec_state_stmt_loc f 0)
      {stmt= SList mir.log_prob; smeta= Middle.no_span}
  in
  let mir = {mir with log_prob= [mir_stmt]} in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  print_endline (string_of_int num) ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        FnPrint__(24, 24);
        if(13) {
          FnPrint__(244, 244);
          if(24) {
            FnPrint__(24, 24);
            }
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      }

      3 |}]

let%expect_test "inline functions" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      ()
      ((sym2__
        ((expr (Var sym4__))
         (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable))))))
      functions {
        void f(int x, matrix y)
          {
          FnPrint__(x);
          FnPrint__(y);
          }

        real g(int z)
          {
          return Pow__(z, 2);
          }

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        for(sym1__ in 1:1) {
          FnPrint__(3);
          FnPrint__(FnMakeRowVec__(FnMakeRowVec__(3, 2), FnMakeRowVec__(4, 6)));
          }
        real sym4__;
        for(sym3__ in 1:1) {
          sym2__ = Pow__(53, 2);
          break;
          }
        FnReject__(sym4__);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline functions 2" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        void f() {
        }
        void g() {
          f();
        }
      }
      generated quantities {
        g();
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      ()
      ()
      ()
      functions {
        void f()
          {

          }

        void g()
          {
          f();
          }

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {

      }

      generate_quantities {
        if(emit_generated_quantities__) {
          for(sym3__ in 1:1) {
            for(sym1__ in 1:1) {

              }
            }
          }
      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "list collapsing" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  print_s [%sexp (mir : Middle.typed_prog)] ;
  [%expect
    {|
    ()
    ((sym2__
      ((expr (Var sym4__))
       (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable))))))
    ((functions_block
      (((fdrt ()) (fdname f)
        (fdargs ((AutoDiffable x UInt) (AutoDiffable y UMatrix)))
        (fdbody
         ((stmt
           (Block
            (((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var x))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var y))
                  (emeta
                   ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>)))))
          (smeta <opaque>)))
        (fdloc
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (included_from ())))
          (end_loc
           ((filename string) (line_num 6) (col_num 9) (included_from ()))))))
       ((fdrt (UReal)) (fdname g) (fdargs ((AutoDiffable z UInt)))
        (fdbody
         ((stmt
           (Block
            (((stmt
               (Return
                (((expr
                   (FunApp StanLib Pow__
                    (((expr (Var z))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Lit Int 2))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>)))))
          (smeta <opaque>)))
        (fdloc
         ((begin_loc
           ((filename string) (line_num 7) (col_num 8) (included_from ())))
          (end_loc
           ((filename string) (line_num 9) (col_num 9) (included_from ()))))))))
     (input_vars ()) (prepare_data ())
     (log_prob
      (((stmt
         (Block
          (((stmt
             (For (loopvar sym1__)
              (lower
               ((expr (Lit Int 1))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
              (upper
               ((expr (Lit Int 1))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
              (body
               ((stmt
                 (Block
                  (((stmt
                     (NRFunApp CompilerInternal FnPrint__
                      (((expr (Lit Int 3))
                        (emeta
                         ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                    (smeta <opaque>))
                   ((stmt
                     (NRFunApp CompilerInternal FnPrint__
                      (((expr
                         (FunApp CompilerInternal FnMakeRowVec__
                          (((expr
                             (FunApp CompilerInternal FnMakeRowVec__
                              (((expr (Lit Int 3))
                                (emeta
                                 ((mtype UInt) (mloc <opaque>)
                                  (madlevel DataOnly))))
                               ((expr (Lit Int 2))
                                (emeta
                                 ((mtype UInt) (mloc <opaque>)
                                  (madlevel DataOnly)))))))
                            (emeta
                             ((mtype URowVector) (mloc <opaque>)
                              (madlevel DataOnly))))
                           ((expr
                             (FunApp CompilerInternal FnMakeRowVec__
                              (((expr (Lit Int 4))
                                (emeta
                                 ((mtype UInt) (mloc <opaque>)
                                  (madlevel DataOnly))))
                               ((expr (Lit Int 6))
                                (emeta
                                 ((mtype UInt) (mloc <opaque>)
                                  (madlevel DataOnly)))))))
                            (emeta
                             ((mtype URowVector) (mloc <opaque>)
                              (madlevel DataOnly)))))))
                        (emeta
                         ((mtype UMatrix) (mloc <opaque>) (madlevel DataOnly)))))))
                    (smeta <opaque>)))))
                (smeta <opaque>)))))
            (smeta <opaque>))
           ((stmt
             (Decl (decl_adtype AutoDiffable) (decl_id sym4__)
              (decl_type (Unsized UReal))))
            (smeta <opaque>))
           ((stmt
             (For (loopvar sym3__)
              (lower
               ((expr (Lit Int 1))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
              (upper
               ((expr (Lit Int 1))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
              (body
               ((stmt
                 (Block
                  (((stmt
                     (Assignment (sym2__ ())
                      ((expr
                        (FunApp StanLib Pow__
                         (((expr (Lit Int 53))
                           (emeta
                            ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                          ((expr (Lit Int 2))
                           (emeta
                            ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                       (emeta
                        ((mtype UReal) (mloc <opaque>) (madlevel DataOnly))))))
                    (smeta <opaque>))
                   ((stmt Break) (smeta <opaque>)))))
                (smeta <opaque>)))))
            (smeta <opaque>))
           ((stmt
             (NRFunApp CompilerInternal FnReject__
              (((expr (Var sym4__))
                (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
            (smeta <opaque>)))))
        (smeta <opaque>))))
     (generate_quantities ()) (transform_inits ()) (output_vars ())
     (prog_name "") (prog_path ""))
    |}]

let%expect_test "do not inline recursive functions" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {
        real g(int z)
          ;

        real g(int z)
          {
          return Pow__(z, 2);
          }

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        FnReject__(g(53));
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function in for loop" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      ((sym1__
        ((expr (Var sym3__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      ((sym4__
        ((expr (Var sym6__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      functions {
        int f(int z)
          {
          FnPrint__("f");
          return 42;
          }

        int g(int z)
          {
          FnPrint__("g");
          return Plus__(z, 24);
          }

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        int sym3__;
        int sym6__;
        for(sym2__ in 1:1) {
          FnPrint__("f");
          sym1__ = 42;
          break;
          }
        for(sym5__ in 1:1) {
          FnPrint__("g");
          sym4__ = Plus__(3, 24);
          break;
          }
        for(i in sym3__:sym6__) {
          {
          FnPrint__("body");
          }
          for(sym5__ in 1:1) {
            FnPrint__("g");
            sym4__ = Plus__(3, 24);
            break;
            }
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

(* TODO: check test results from here *)

let%expect_test "inline function in for loop 2" =
  let _ = gensym_reset_danger_use_cautiously () in
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
          return f(z) + 24;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      ((sym1__
        ((expr (Var sym3__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
      ((sym4__
        ((expr (Var sym6__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      ((sym7__
        ((expr (Var sym9__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      ((sym10__
        ((expr (Var sym12__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable)))))
       (sym6__
        ((expr (Var sym13__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      functions {
        int f(int z)
          {
          FnPrint__("f");
          return 42;
          }

        int g(int z)
          {
          FnPrint__("g");
          return Plus__(f(z), 24);
          }

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        int sym9__;
        int sym12__;
        for(sym8__ in 1:1) {
          FnPrint__("f");
          sym7__ = 42;
          break;
          }
        for(sym11__ in 1:1) {
          FnPrint__("g");
          int sym13__;
          for(sym5__ in 1:1) {
            FnPrint__("f");
            sym4__ = 42;
            break;
            }
          sym10__ = Plus__(sym13__, 24);
          break;
          }
        for(i in sym9__:sym12__) {
          {
          FnPrint__("body");
          }
          for(sym11__ in 1:1) {
            FnPrint__("g");
            int sym13__;
            for(sym5__ in 1:1) {
              FnPrint__("f");
              sym4__ = 42;
              break;
              }
            sym10__ = Plus__(sym13__, 24);
            break;
            }
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function in while loop" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      ((sym1__
        ((expr (Var sym3__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      functions {
        int f(int z)
          {
          FnPrint__("f");
          return 42;
          }

        int g(int z)
          {
          FnPrint__("g");
          return Plus__(z, 24);
          }

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        int sym3__;
        for(sym2__ in 1:1) {
          FnPrint__("g");
          sym1__ = Plus__(3, 24);
          break;
          }
        while(sym3__) {
          FnPrint__("body");
          for(sym2__ in 1:1) {
            FnPrint__("g");
            sym1__ = Plus__(3, 24);
            break;
            }
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function in if then else" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      ((sym1__
        ((expr (Var sym3__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      functions {
        int f(int z)
          {
          FnPrint__("f");
          return 42;
          }

        int g(int z)
          {
          FnPrint__("g");
          return Plus__(z, 24);
          }

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        int sym3__;
        for(sym2__ in 1:1) {
          FnPrint__("g");
          sym1__ = Plus__(3, 24);
          break;
          }
        if(sym3__) FnPrint__("body");
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      }

    |}]

let%expect_test "inline function in ternary if " =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      ((sym1__
        ((expr (Var sym3__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      ((sym4__
        ((expr (Var sym6__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      ((sym7__
        ((expr (Var sym9__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      functions {
        int f(int z)
          {
          FnPrint__("f");
          return 42;
          }

        int g(int z)
          {
          FnPrint__("g");
          return Plus__(z, 24);
          }

        int h(int z)
          {
          FnPrint__("h");
          return Plus__(z, 4);
          }

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        int sym3__;
        int sym6__;
        int sym9__;
        for(sym2__ in 1:1) {
          FnPrint__("f");
          sym1__ = 42;
          break;
          }
        if(sym3__) {
          for(sym5__ in 1:1) {
            FnPrint__("g");
            sym4__ = Plus__(3, 24);
            break;
            }
          }
         else {
          for(sym8__ in 1:1) {
            FnPrint__("h");
            sym7__ = Plus__(4, 4);
            break;
            }
          }
        FnPrint__(sym3__ ?sym6__: sym9__);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function multiple returns " =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      ((sym1__
        ((expr (Var sym3__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      functions {
        int f(int z)
          {
          if(2) {
            FnPrint__("f");
            return 42;
            }
          return 6;
          }

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        int sym3__;
        for(sym2__ in 1:1) {
          if(2) {
            FnPrint__("f");
            sym1__ = 42;
            break;
            }
          sym1__ = 6;
          break;
          }
        FnPrint__(sym3__);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function indices " =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        int f(int z) {
          print(z);
          return 42;
        }
      }
      model {
        int a[2, 2];
        print(a[f(1), f(2)]);
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      ((sym1__
        ((expr (Var sym3__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      ((sym4__
        ((expr (Var sym6__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      functions {
        int f(int z)
          {
          FnPrint__(z);
          return 42;
          }

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        array[array[int, 2], 2] a;
        int sym6__;
        int sym3__;
        for(sym5__ in 1:1) {
          FnPrint__(2);
          sym4__ = 42;
          break;
          }
        for(sym2__ in 1:1) {
          FnPrint__(1);
          sym1__ = 42;
          break;
          }
        FnPrint__(a[sym3__, sym6__]);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function and " =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        int f(int z) {
          print(z);
          return 42;
        }
      }
      model {
        print(f(1) && f(2));
      }
      |}
  in
  (* TODO: these declarations are still in the wrong place *)
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      ((sym1__
        ((expr (Var sym3__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      ((sym4__
        ((expr (Var sym6__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      functions {
        int f(int z)
          {
          FnPrint__(z);
          return 42;
          }

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        int sym3__;
        int sym6__;
        for(sym2__ in 1:1) {
          FnPrint__(1);
          sym1__ = 42;
          break;
          }
        if(sym3__) {
          for(sym5__ in 1:1) {
            FnPrint__(2);
            sym4__ = 42;
            break;
            }
          }
        FnPrint__(sym3__ && sym6__);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function or " =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        int f(int z) {
          print(z);
          return 42;
        }
      }
      model {
        print(f(1) || f(2));
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      ((sym1__
        ((expr (Var sym3__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      ((sym4__
        ((expr (Var sym6__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
      functions {
        int f(int z)
          {
          FnPrint__(z);
          return 42;
          }

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        int sym3__;
        int sym6__;
        for(sym2__ in 1:1) {
          FnPrint__(1);
          sym1__ = 42;
          break;
          }
        if(sym3__) ;
         else {
          for(sym5__ in 1:1) {
            FnPrint__(2);
            sym4__ = 42;
            break;
            }
          }
        FnPrint__(sym3__ || sym6__);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "unroll nested loop" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  let mir = static_loop_unrolling mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        {
        {
        FnPrint__(1, 3);
        }
        {
        FnPrint__(1, 4);
        }
        }
        {
        {
        FnPrint__(2, 3);
        }
        {
        FnPrint__(2, 4);
        }
        }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "unroll nested loop with break" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  let mir = static_loop_unrolling mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        {
        for(j in 3:4) {
          FnPrint__(1);
          break;
          }
        }
        {
        for(j in 3:4) {
          FnPrint__(2);
          break;
          }
        }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "constant propagation" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
    functions {

    }

    input_vars {

    }

    prepare_data {
      data int i;
      i = 42;
      data int j;
      j = Plus__(2, 42);
    }

    log_prob {
      {
      for(x in 1:42) {
        FnPrint__(Plus__(42, 44));
        }
      }
    }

    generate_quantities {

    }

    transform_inits {

    }

    output_vars {

    } |}]

let%expect_test "constant propagation, local scope" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
    functions {

    }

    input_vars {

    }

    prepare_data {
      data int i;
      i = 42;
      {
      data int j;
      j = 2;
      }
    }

    log_prob {
      {
      int j;
      for(x in 1:42) {
        FnPrint__(Plus__(42, j));
        }
      }
    }

    generate_quantities {

    }

    transform_inits {

    }

    output_vars {

    } |}]

let%expect_test "constant propagation, model block local scope" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
    functions {

    }

    input_vars {

    }

    prepare_data {

    }

    log_prob {
      {
      int i;
      i = 42;
      int j;
      j = 2;
      }
    }

    generate_quantities {
      if(emit_generated_quantities__) {
        data int i;
        data int j;
        for(x in 1:i) {
          FnPrint__(Plus__(i, j));
          }
        FnWriteParam__(i);
        FnWriteParam__(j);
        }
    }

    transform_inits {

    }

    output_vars {
      generated_quantities int i;
      generated_quantities int j;
    } |}]

let%expect_test "expression propagation" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {
        data int i;
        data int j;
        j = Plus__(2, i);
      }

      log_prob {
        {
        for(x in 1:i) {
          FnPrint__(Plus__(i, Plus__(2, i)));
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "copy propagation" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {
        data int i;
        data int j;
        j = i;
        data int k;
        k = Times__(2, i);
      }

      log_prob {
        {
        for(x in 1:i) {
          FnPrint__(Plus__(Plus__(i, i), k));
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "dead code elimination" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {
        data array[int, 2] i;
        i = FnMakeArray__(3, 2);
        data array[int, 2] j;
        j = FnMakeArray__(3, 2);
        j[1] = 2;
      }

      log_prob {
        {
        FnPrint__(i);
        FnPrint__(j);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "dead code elimination decl" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        int i;
        }
      }

      generate_quantities {
        if(emit_generated_quantities__) {
          data int i;
          FnPrint__(i);
          }
      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "dead code elimination, for loop" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        int i;
        FnPrint__(i);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "dead code elimination, while loop" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        int i;
        FnPrint__(i);
        while(1) ;
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "dead code elimination, if then" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        int i;
        FnPrint__(i);
        {
        FnPrint__("hello");
        }
        {
        FnPrint__("goodbye");
        }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "dead code elimination, nested" =
  let _ = gensym_reset_danger_use_cautiously () in
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        int i;
        FnPrint__(i);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "partial evaluation" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        if (1 > 2) {
          int i;
          print(1+2);
          print(i + (1+2));
          print(log(1-i));
        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = partial_evaluation mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        if(0) {
          int i;
          FnPrint__(3);
          FnPrint__(Plus__(i, 3));
          FnPrint__(log1m(i));
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "try partially evaluate" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        real x;
        real y;
        vector[2] a;
        vector[2] b;
        print(log(exp(x)-exp(y)));
        print(log(exp(a)-exp(b)));
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = partial_evaluation mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        real x;
        real y;
        vector[2] a;
        vector[2] b;
        FnPrint__(log_diff_exp(x, y));
        FnPrint__(log(Minus__(exp(a), exp(b))));
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "partially evaluate with equality check" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        vector[2] x;
        vector[2] y;
        print(dot_product(x, x));
        print(dot_product(x, y));
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = partial_evaluation mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        vector[2] x;
        vector[2] y;
        FnPrint__(dot_self(x));
        FnPrint__(dot_product(x, y));
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "partially evaluate glm" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        matrix[2,3] x;
        int y[2];
        vector[2] y_real;
        vector[3] beta;
        vector[2] alpha;
        real sigma;
        print(bernoulli_lpmf(y| inv_logit(alpha + x * beta)));
        print(bernoulli_logit_lpmf(y| alpha + x * beta));
        print(bernoulli_lpmf(y| inv_logit(x * beta + alpha)));
        print(bernoulli_logit_lpmf(y| x * beta + alpha));
        print(bernoulli_lpmf(y| inv_logit(x * beta)));
        print(bernoulli_logit_lpmf(y| x * beta));
        print(neg_binomial_2_lpmf(y| exp(alpha + x * beta), sigma));
        print(neg_binomial_2_log_lpmf(y| alpha + x * beta, sigma));
        print(neg_binomial_2_lpmf(y| exp(x * beta + alpha), sigma));
        print(neg_binomial_2_log_lpmf(y| x * beta + alpha, sigma));
        print(neg_binomial_2_lpmf(y| exp(x * beta), sigma));
        print(neg_binomial_2_log_lpmf(y| x * beta, sigma));
        print(normal_lpdf(y_real| alpha + x * beta, sigma));
        print(normal_lpdf(y_real| x * beta + alpha, sigma));
        print(normal_lpdf(y_real| x * beta, sigma));
        print(poisson_lpmf(y| exp(alpha + x * beta)));
        print(poisson_log_lpmf(y| alpha + x * beta));
        print(poisson_lpmf(y| exp(x * beta + alpha)));
        print(poisson_log_lpmf(y| x * beta + alpha));
        print(poisson_lpmf(y| exp(x * beta)));
        print(poisson_log_lpmf(y| x * beta));
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = partial_evaluation mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
        matrix[2, 3] x;
        array[int, 2] y;
        vector[2] y_real;
        vector[3] beta;
        vector[2] alpha;
        real sigma;
        FnPrint__(bernoulli_logit_glm_lpmf(y, x, alpha, beta));
        FnPrint__(bernoulli_logit_glm_lpmf(y, x, alpha, beta));
        FnPrint__(bernoulli_logit_glm_lpmf(y, x, alpha, beta));
        FnPrint__(bernoulli_logit_glm_lpmf(y, x, alpha, beta));
        FnPrint__(bernoulli_logit_glm_lpmf(y, x, 0, beta));
        FnPrint__(bernoulli_logit_glm_lpmf(y, x, 0, beta));
        FnPrint__(neg_binomial_2_log_glm_lpmf(y, x, alpha, beta, sigma));
        FnPrint__(neg_binomial_2_log_glm_lpmf(y, x, alpha, beta, sigma));
        FnPrint__(neg_binomial_2_log_glm_lpmf(y, x, alpha, beta, sigma));
        FnPrint__(neg_binomial_2_log_glm_lpmf(y, x, alpha, beta, sigma));
        FnPrint__(neg_binomial_2_log_glm_lpmf(y, x, 0, beta, sigma));
        FnPrint__(neg_binomial_2_log_glm_lpmf(y, x, 0, beta, sigma));
        FnPrint__(normal_id_glm_lpdf(y_real, x, alpha, beta, sigma));
        FnPrint__(normal_id_glm_lpdf(y_real, x, alpha, beta, sigma));
        FnPrint__(normal_id_glm_lpdf(y_real, x, 0, beta, sigma));
        FnPrint__(poisson_log_glm_lpmf(y, x, alpha, beta));
        FnPrint__(poisson_log_glm_lpmf(y, x, alpha, beta));
        FnPrint__(poisson_log_glm_lpmf(y, x, alpha, beta));
        FnPrint__(poisson_log_glm_lpmf(y, x, alpha, beta));
        FnPrint__(poisson_log_glm_lpmf(y, x, 0, beta));
        FnPrint__(poisson_log_glm_lpmf(y, x, 0, beta));
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        print({3.0});
        print({3.0});
        print({3.0});
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
    functions {

    }

    input_vars {

    }

    prepare_data {

    }

    log_prob {
      data [real] sym1__;
      {
      sym1__ = FnMakeArray__(3.0);
      FnPrint__(sym1__);
      FnPrint__(sym1__);
      FnPrint__(sym1__);
      }
    }

    generate_quantities {

    }

    transform_inits {

    }

    output_vars {

    } |}]

let%expect_test "lazy code motion, 2" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        for (i in 1:2)
          print(3 + 4);
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym2__;
        data int sym1__;
        {
        for(i in 1:2) {
          {
          FnPrint__(Plus__(3, 4));
          }
          ;
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 3" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        print(3);
        print(3 + 5);
        print((3 + 5) + 7);
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym2__;
        data int sym1__;
        {
        FnPrint__(3);
        sym1__ = Plus__(3, 5);
        FnPrint__(sym1__);
        FnPrint__(Plus__(sym1__, 7));
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 4" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int b;
        int c;
        int x;
        int y;
        b = 1;
        if (1) {
          ;
          ;
          ;
        } else {
          x = b + c;
          ;
        }
        y = b + c;
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  (* TODO: make sure that these
     temporaries do not get assigned level DataOnly unless appropriate *)
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym1__;
        {
        int b;
        int c;
        int x;
        int y;
        b = 1;
        if(1) {
          {
          ;
          ;
          ;
          }
          sym1__ = Plus__(b, c);
          ;
          }
         else {
          {
          sym1__ = Plus__(b, c);
          x = sym1__;
          ;
          }
          ;
          }
        y = sym1__;
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 5" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int b;
        int c;
        int x;
        int y;
        b = 1;
        if (1) {
          ;
          ;
          ;
        } else {
          if (2) x = b + c;
          ;
        }
        y = b + c;
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym1__;
        {
        int b;
        int c;
        int x;
        int y;
        b = 1;
        if(1) {
          {
          ;
          ;
          ;
          }
          sym1__ = Plus__(b, c);
          ;
          }
         else {
          {
          if(2) {
            sym1__ = Plus__(b, c);
            x = sym1__;
            ;
            }
           else sym1__ = Plus__(b, c);
                ;
          ;
          }
          ;
          }
        y = sym1__;
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 6" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int x;
        int y;
        if (2)
          x = 1 + 2;
        y = 4 + 3;
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym2__;
        data int sym1__;
        {
        int x;
        int y;
        if(2) {
          x = Plus__(1, 2);
          ;
          }
         else ;
        y = Plus__(4, 3);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 7" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int a;
        int b;
        int c;
        int x;
        int y;
        int z;
        if (1) {
          a = c;
          x = a + b;
        } else ;
        if (2) {
          if (3) {
            ;
            while (4) y = a + b;
            ;
          } else {
              ;
              while (5) ;
              y = a + b;
            }
            z = a + b;
          } else ;
          ;
        }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym2__;
        data int sym1__;
        {
        int a;
        int b;
        int c;
        int x;
        int y;
        int z;
        if(1) {
          {
          a = c;
          x = Plus__(a, b);
          }
          ;
          }
         else {
          ;
          ;
          }
        if(2) {
          {
          if(3) {
            {
            sym2__ = Plus__(a, b);
            ;
            while(4) {
              y = sym2__;
              ;
              }
            ;
            }
            ;
            }
           else {
            {
            ;
            while(5) {
              ;
              ;
              }
            sym2__ = Plus__(a, b);
            y = sym2__;
            }
            ;
            }
          z = sym2__;
          }
          ;
          }
         else {
          ;
          ;
          }
        ;
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 8, _lp functions not optimized" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        int foo_lp(int x) { target += 1; return 24; }
        int foo(int x) { return 24; }
      }
      model {
        print(foo(foo_lp(1)));
        print(foo(foo_lp(1)));
        print(foo(foo(1)));
        print(foo(foo(1)));
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {
        int foo_lp(int x)
          {
          target += 1;
          return 24;
          }

        int foo(int x)
          {
          return 24;
          }

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym1__;
        {
        FnPrint__(foo(foo_lp(1)));
        FnPrint__(foo(foo_lp(1)));
        sym1__ = foo(foo(1));
        FnPrint__(sym1__);
        FnPrint__(sym1__);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 9" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int x;
        while (x * 2) print("hello") ;
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym1__;
        {
        int x;
        while(Times__(x, 2)) {
          FnPrint__("hello");
          ;
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 10" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int x;
        x = 3;
        print(x * 2);
        x = 2;
        print(x * 2);
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym1__;
        {
        int x;
        x = 3;
        FnPrint__(Times__(x, 2));
        x = 2;
        FnPrint__(Times__(x, 2));
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 11" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        {
          int x;
          print(x * 2);
        }
        {
          int x;
          print(x * 2);
        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym1__;
        {
        {
        int x;
        FnPrint__(Times__(x, 2));
        }
        {
        int x;
        FnPrint__(Times__(x, 2));
        }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 12" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int x;
        for (i in 1:6) {
          print(x + 42);
          continue;
          x = 3;
        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym2__;
        data int sym1__;
        {
        int x;
        for(i in 1:6) {
          {
          FnPrint__(Plus__(x, 42));
          continue;
          x = 3;
          }
          ;
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "cool example: expression propagation + partial evaluation + \
                 lazy code motion + dead code elimination" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        real x;
        int y;
        real theta;
        for (i in 1:100000) {
          theta = inv_logit(x);
          target += bernoulli_lpmf(y| theta);
        }
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = expression_propagation mir in
  let mir = partial_evaluation mir in
  let mir = one_step_loop_unrolling mir in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  let mir = dead_code_elimination mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        real sym5__;
        real sym4__;
        data int sym3__;
        data int sym2__;
        data int sym1__;
        {
        real x;
        int y;
        real theta;
        if(Leq__(1, 100000)) {
          {
          {
          sym3__ = Plus__(1, 1);
          sym4__ = bernoulli_logit_lpmf(y, x);
          target += sym4__;
          }
          for(i in sym3__:100000) {
            {
            target += sym4__;
            }
            }
          }
          }
         else ;
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "block fixing" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir =
    { mir with
      Middle.log_prob=
        [ { stmt=
              IfElse
                ( zero
                , { stmt= While (zero, {stmt= SList []; smeta= no_span})
                  ; smeta= no_span }
                , None )
          ; smeta= no_span } ] }
  in
  let mir = block_fixing mir in
  print_s [%sexp (mir : Middle.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (IfElse
            ((expr (Lit Int 0))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
            ((stmt
              (While
               ((expr (Lit Int 0))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
               ((stmt (Block ())) (smeta <opaque>))))
             (smeta <opaque>))
            ()))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "one-step loop unrolling" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      transformed data {
        int x;
        for (i in x:6) print("hello");
        while (1<2) print("goodbye");
        for (i in 1:1) for (j in 2:2) print("nested");
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = one_step_loop_unrolling mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {
        data int x;
        if(Leq__(x, 6)) {
          {
          FnPrint__("hello");
          }
          for(i in Plus__(x, 1):6) {
            FnPrint__("hello");
            }
          }
        if(Less__(1, 2)) {
          FnPrint__("goodbye");
          while(Less__(1, 2)) FnPrint__("goodbye");
          }
        if(Leq__(1, 1)) {
          {
          if(Leq__(2, 2)) {
            {
            FnPrint__("nested");
            }
            for(j in Plus__(2, 1):2) {
              FnPrint__("nested");
              }
            }
          }
          for(i in Plus__(1, 1):1) {
            if(Leq__(2, 2)) {
              {
              FnPrint__("nested");
              }
              for(j in Plus__(2, 1):2) {
                FnPrint__("nested");
                }
              }
            }
          }
      }

      log_prob {

      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]


let%expect_test "replace fresh variables" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      transformed data {
        real x;
        print(x);
        int y;
        for (i in 1:4) {
          real c;
          print(c);
          real d[2];
          reject(d, c);
          reject(d, c);
          print(x);
        }
        print(x);
        print(y);
      }
      |}
  in
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = transform_program mir replace_fresh_local_vars in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      ((c
        ((expr (Var sym3__))
         (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))
       (d
        ((expr (Var sym4__))
         (emeta ((mtype (UArray UReal)) (mloc <opaque>) (madlevel DataOnly)))))
       (x
        ((expr (Var sym1__))
         (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))
       (y
        ((expr (Var sym2__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
      ((sym1__
        ((expr (Var sym5__))
         (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))
       (sym2__
        ((expr (Var sym6__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
       (sym3__
        ((expr (Var sym7__))
         (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))
       (sym4__
        ((expr (Var sym8__))
         (emeta ((mtype (UArray UReal)) (mloc <opaque>) (madlevel DataOnly))))))
      functions {

      }

      input_vars {

      }

      prepare_data {
        data real sym5__;
        FnPrint__(sym5__);
        data int sym6__;
        for(i in 1:4) {
          data real sym7__;
          FnPrint__(sym7__);
          data array[real, 2] sym8__;
          FnReject__(sym8__, sym7__);
          FnReject__(sym8__, sym7__);
          FnPrint__(sym5__);
          }
        FnPrint__(sym5__);
        FnPrint__(sym6__);
      }

      log_prob {

      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "replace fresh variables 2" =
  let _ = gensym_reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      transformed data {
              {
        int x;
        int y;
        for(z in 1:1) {
          print(1);
          x = 42;
          break;
          }
        if(x) ;
         else {
          for(z in 1:1) {
            print(2);
            y = 42;
            break;
            }
          }
        print(x || y);
        }
      }
      |}
  in
  (* TODO: this is still doing the wrong thing. This is related to the bug in function inlining.
     It seems that substitution is not happening for variables that appeared at the top level.
     It seems like the bug is in substitution  *)
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = map_prog (fun x -> x) replace_fresh_local_vars mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      ((x
        ((expr (Var sym1__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
       (y
        ((expr (Var sym2__))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
      functions {

      }

      input_vars {

      }

      prepare_data {
        {
        data int sym1__;
        data int sym2__;
        for(z in 1:1) {
          FnPrint__(1);
          x = 42;
          break;
          }
        if(sym1__) ;
         else {
          for(z in 1:1) {
            FnPrint__(2);
            y = 42;
            break;
            }
          }
        FnPrint__(sym1__ || sym2__);
        }
      }

      log_prob {

      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

