(* Code for optimization passes on the MIR *)
open Core_kernel
open Mir
open Mir_utils

let replace_fresh_local_vars s' =
  let f m = function
    | Decl {decl_adtype; decl_type; decl_id} ->
        let fresh_name = Util.gensym () in
        ( Decl {decl_adtype; decl_id= fresh_name; decl_type}
        , Map.Poly.set m ~key:decl_id
            ~data:
              { expr= Var fresh_name
              ; emeta=
                  { mtype= remove_possible_size decl_type
                  ; madlevel= decl_adtype
                  ; mloc= Mir.no_span } } )
    | x -> (x, m)
  in
  let s, m = map_rec_state_stmt_loc f Map.Poly.empty s' in
  subst_stmt m s

let subst_args_stmt args es =
  let m = Map.Poly.of_alist_exn (List.zip_exn args es) in
  subst_stmt m

let handle_early_returns opt_triple b =
  let f = function
    | Return opt_ret -> (
      match (opt_triple, opt_ret) with
      | None, None -> Break
      | Some (Some _, _, name), Some e ->
          SList
            [ {stmt= Assignment ((name, []), e); smeta= Mir.no_span}
            ; {stmt= Break; smeta= Mir.no_span} ]
      | _, _ -> Errors.fatal_error () )
    | x -> x
  in
  For
    { loopvar= Util.gensym ()
    ; lower=
        { expr= Lit (Int, "1")
        ; emeta= {mtype= UInt; madlevel= DataOnly; mloc= Mir.no_span} }
    ; upper=
        { expr= Lit (Int, "1")
        ; emeta= {mtype= UInt; madlevel= DataOnly; mloc= Mir.no_span} }
    ; body= map_rec_stmt_loc f b }

let map_no_loc l = List.map ~f:(fun s -> {stmt= s; smeta= Mir.no_span}) l
let slist_no_loc l = SList (map_no_loc l)

let slist_concat_no_loc l stmt =
  match l with [] -> stmt | l -> slist_no_loc (l @ [stmt])

let _ = replace_fresh_local_vars

let rec inline_function_expression adt fim e =
  match e.expr with
  | Var _ -> ([], e)
  | Lit (_, _) -> ([], e)
  | FunApp (t, s, es) -> (
      let se_list = List.map ~f:(inline_function_expression adt fim) es in
      let s_list = List.concat (List.rev (List.map ~f:fst se_list)) in
      let es = List.map ~f:snd se_list in
      match Map.find fim s with
      | None ->
      (s_list, {e with expr= FunApp (t, s, es)})
      | Some (rt, args, b) ->
          let b = b (* TODO: replace fresh variables here *)
          in
          let x = Util.gensym () in
          let b = handle_early_returns (Some (rt, adt, x)) b in
          ( s_list
            @ [ Decl
                  {decl_adtype= adt; decl_id= x; decl_type= Option.value_exn rt}
              ; (subst_args_stmt args es {stmt= b; smeta= Mir.no_span}).stmt ]
          , { expr= Var x
            ; emeta=
                { mtype= remove_possible_size (Option.value_exn rt)
                ; madlevel= adt
                ; mloc= Mir.no_span } } ) )
  | TernaryIf (e1, e2, e3) ->
      let sl1, e1 = inline_function_expression adt fim e1 in
      let sl2, e2 = inline_function_expression adt fim e2 in
      let sl3, e3 = inline_function_expression adt fim e3 in
      ( sl1
        @ [ IfElse
              ( e1
              , {stmt= slist_no_loc sl2; smeta= Mir.no_span}
              , Some {stmt= slist_no_loc sl3; smeta= Mir.no_span} ) ]
      , {e with expr= TernaryIf (e1, e2, e3)} )
  | Indexed (e, i_list) ->
      let sl, e = inline_function_expression adt fim e in
      let si_list = List.map ~f:(inline_function_index adt fim) i_list in
      let s_list = List.concat (List.rev (List.map ~f:fst si_list)) in
      (s_list @ sl, e)
  | EAnd (_, _) ->
      Errors.fatal_error ~msg:"Not yet implemented" () (* TODO!! *)
  | EOr (_, _) -> Errors.fatal_error ~msg:"Not yet implemented" ()

(* TODO!! *)
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

let rec inline_function_statement adt fim {stmt; smeta} =
  { stmt=
      ( match stmt with
      | Assignment ((x, l), e2) ->
          let e1 = {e2 with expr= Indexed ({e2 with expr= Var x}, l)} in
          let sl1, e1 = inline_function_expression adt fim e1 in
          let sl2, e2 = inline_function_expression adt fim e2 in
          let x, l =
            match e1.expr with
            | Var x -> (x, [])
            | Indexed ({expr= Var x; _}, l) -> (x, l)
            | _ -> Errors.fatal_error ()
          in
          slist_concat_no_loc (sl2 @ sl1) (Assignment ((x, l), e2))
      | TargetPE e ->
          let s, e = inline_function_expression adt fim e in
          slist_concat_no_loc s (TargetPE e)
      | NRFunApp (t, s, es) ->
          let se_list = List.map ~f:(inline_function_expression adt fim) es in
          (* function arguments are evaluated from right to left in C++, so we need to reverse *)
          let s_list = List.concat (List.rev (List.map ~f:fst se_list)) in
          let es = List.map ~f:snd se_list in
          slist_concat_no_loc s_list
            ( match Map.find fim s with
            | None -> NRFunApp (t, s, es)
            | Some (_, args, b) ->
                let b = b in (* TODO: replace fresh variables here *)
                let b = handle_early_returns None b in
                (subst_args_stmt args es {stmt= b; smeta= Mir.no_span}).stmt )
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
                           @ map_no_loc (Option.value ~default:[] (List.tl s'))
                           )
                     ; smeta= Mir.no_span } ))
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
                             @ map_no_loc
                                 (Option.value ~default:[] (List.tl s_upper))
                             )
                       ; smeta= Mir.no_span } ) })
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

let unroll_loops_statement =
  let f stmt =
    match stmt with
    | For {loopvar; lower; upper; body} -> (
      match (contains_top_break_or_continue body, lower.expr, upper.expr) with
      | false, Lit (Int, low), Lit (Int, up) ->
          let range =
            List.map
              ~f:(fun i ->
                { expr= Lit (Int, Int.to_string i)
                ; emeta= {mtype= UInt; mloc= Mir.no_span; madlevel= DataOnly}
                } )
              (List.range ~start:`inclusive ~stop:`inclusive
                 (Int.of_string low) (Int.of_string up))
          in
          let stmts =
            List.map
              ~f:(fun i ->
                subst_args_stmt [loopvar] [i]
                  {stmt= body.stmt; smeta= Mir.no_span} )
              range
          in
          SList stmts
      | _ -> stmt )
    | _ -> stmt
  in
  map_rec_stmt_loc f

let loop_unrolling = map_prog (fun x -> x) unroll_loops_statement

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

(*
let statement_of_program mir =
  { stmt=
      SList
        (List.map
           ~f:(fun x -> {stmt= SList x; smeta= Mir.no_span})
           [ mir.functions_block; mir.prepare_data; mir.prepare_params
           ; mir.log_prob; mir.generate_quantities ])
  ; smeta= Mir.no_span }

let update_program_statement_blocks (mir : typed_prog) (s : stmt_loc) =
  let l =
    match s.stmt with
    | SList l ->
        List.map
          ~f:(fun x ->
            match x.stmt with
            | SList l -> l
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
             and type properties = (string, Mir.expr_typed_located) Map.Poly.t
                                   option)) (mir : typed_prog) =
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
        subst_stmt_base
          (Option.value ~default:Map.Poly.empty (Map.find_exn values i).entry)
    )
  in
  let s = propagate_stmt (Map.find_exn flowgraph_to_mir 1) in
  update_program_statement_blocks mir s
*)

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
               ~f:(fun x -> {stmt= SList x; smeta= Mir.no_span})
               [ mir.prepare_data; mir.transform_inits; mir.log_prob
               ; mir.generate_quantities ])
      ; smeta= Mir.no_span }
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
    match transform {stmt= SList s; smeta= Mir.no_span} with
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

let list_collapsing (mir : typed_prog) =
  transform_program_blockwise mir collapse_lists_statement

let propagation
    (propagation_transfer :
         (int, Mir.stmt_loc_num) Map.Poly.t
      -> (module
          Monotone_framework_sigs.TRANSFER_FUNCTION
            with type labels = int
             and type properties = (string, Mir.expr_typed_located) Map.Poly.t
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
      (* TODO: double check that internal functions cannot side effect*)
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
          | _ -> Map.set accum ~key:e ~data:(Util.gensym ()) )
    in
    (* TODO: it'd be more efficient to just not accumulate constants in the static analysis *)
    let declarations_list =
      Map.fold expression_map ~init:[] ~f:(fun ~key ~data accum ->
          { stmt=
              Mir.Decl
                { decl_adtype= key.emeta.madlevel
                ; decl_id= data
                ; decl_type= Unsized key.emeta.mtype }
          ; smeta= Mir.no_span }
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
            ; smeta= Mir.no_span } )
          to_assign_in_s
      in
      let expr_subst_stmt_except_initial_assign m =
        let f stmt =
          match stmt with
          | Assignment ((x, []), e')
            when Map.mem m e'
                 && Mir.compare_expr_typed_located {e' with expr= Var x}
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
        (f {stmt; smeta= Mir.no_span}).stmt
      else
        SList
          (List.map ~f (assignments_to_add_to_s @ [{stmt; smeta= Mir.no_span}]))
    in
    let lazy_code_motion_stmt =
      map_rec_stmt_loc_num flowgraph_to_mir lazy_code_motion_base
    in
    { stmt=
        SList
          ( declarations_list
          @ [lazy_code_motion_stmt (Map.find_exn flowgraph_to_mir 1)] )
    ; smeta= Mir.no_span }
  in
  transform_program_blockwise mir (fun x -> transform (preprocess_flowgraph x))

let block_fixing =
  map_prog
    (fun x -> x)
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
  pp_typed_prog Format.std_formatter mir ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        FnPrint__(24, 24);
        if(13) {
          FnPrint__(244, 244);
          if(24) {
            FnPrint__(24, 24);
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars { |}]

let%expect_test "map_rec_state_stmt_loc" =
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
    (map_rec_state_stmt_loc f 0) {stmt= SList mir.log_prob; smeta= Mir.no_span}
  in
  let mir = {mir with log_prob= [mir_stmt]} in
  pp_typed_prog Format.std_formatter mir ;
  print_endline (string_of_int num) ;
  [%expect
    {|
      }
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        FnPrint__(24, 24);
        if(13) {
          FnPrint__(244, 244);
          if(24) {
            FnPrint__(24, 24);
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {
        3 |}]

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
  pp_typed_prog Format.std_formatter mir ;
  [%expect
    {|
      }
      functions {
        void f(int x, matrix y) {
          FnPrint__(x);
          FnPrint__(y);
        }
        real g(int z) {
          return Pow__(z, 2);
        }
      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        for(sym1__ in 1:1) {
          FnPrint__(3);
          FnPrint__(FnMakeRowVec__(FnMakeRowVec__(3, 2), FnMakeRowVec__(4, 6)));
        }
        real sym2__;
        for(sym3__ in 1:1) {
          sym2__[] = Pow__(53, 2);
          break;
        }
        FnReject__(sym2__);
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars { |}]

let%expect_test "inline functions 2" =
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
  pp_typed_prog Format.std_formatter mir ;
  [%expect
    {|
      }
      functions {
        void f() {

        }
        void g() {
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
        for(sym6__ in 1:1) {
          for(sym4__ in 1:1) {

          }
        }
      }

      transform_inits {

      }

      output_vars { |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
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
             (For (loopvar sym7__)
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
             (Decl (decl_adtype AutoDiffable) (decl_id sym8__)
              (decl_type (Unsized UReal))))
            (smeta <opaque>))
           ((stmt
             (For (loopvar sym9__)
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
                     (Assignment (sym8__ ())
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
              (((expr (Var sym8__))
                (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
            (smeta <opaque>)))))
        (smeta <opaque>))))
     (generate_quantities ()) (transform_inits ()) (output_vars ())
     (prog_name "") (prog_path ""))
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
  pp_typed_prog Format.std_formatter mir ;
  [%expect
    {|
      }
      functions {
        real g(int z) {
          skip;
        }
        real g(int z) {
          return Pow__(z, 2);
        }
      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        FnReject__(g(53));
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars { |}]

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
  pp_typed_prog Format.std_formatter mir ;
  [%expect
    {|
      }
      functions {
        int f(int z) {
          FnPrint__("f");
          return 42;
        }
        int g(int z) {
          FnPrint__("g");
          return Plus__(z, 24);
        }
      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        int sym10__;
        for(sym11__ in 1:1) {
          FnPrint__("f");
          sym10__[] = 42;
          break;
        }
        int sym12__;
        for(sym13__ in 1:1) {
          FnPrint__("g");
          sym12__[] = Plus__(3, 24);
          break;
        }
        for(i in sym10__:sym12__) {
          FnPrint__("body");
          for(sym13__ in 1:1) {
            FnPrint__("g");
            sym12__[] = Plus__(3, 24);
            break;
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars { |}]

(* TODO: check test results from here *)

let%expect_test "inline function in for loop 2" =
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
  pp_typed_prog Format.std_formatter mir ;
  [%expect
    {|
      }
      functions {
        int f(int z) {
          FnPrint__("f");
          return 42;
        }
        int g(int z) {
          FnPrint__("g");
          return Plus__(f(z), 24);
        }
      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        int sym18__;
        for(sym19__ in 1:1) {
          FnPrint__("f");
          sym18__[] = 42;
          break;
        }
        int sym20__;
        for(sym21__ in 1:1) {
          FnPrint__("g");
          int sym16__;
          for(sym17__ in 1:1) {
            FnPrint__("f");
            sym16__[] = 42;
            break;
          }
          sym20__[] = Plus__(sym16__, 24);
          break;
        }
        for(i in sym18__:sym20__) {
          FnPrint__("body");
          for(sym21__ in 1:1) {
            FnPrint__("g");
            int sym16__;
            for(sym17__ in 1:1) {
              FnPrint__("f");
              sym16__[] = 42;
              break;
            }
            sym20__[] = Plus__(sym16__, 24);
            break;
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars { |}]

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
  pp_typed_prog Format.std_formatter mir ;
  [%expect
    {|
      }
      functions {
        int f(int z) {
          FnPrint__("f");
          return 42;
        }
        int g(int z) {
          FnPrint__("g");
          return Plus__(z, 24);
        }
      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        int sym22__;
        for(sym23__ in 1:1) {
          FnPrint__("g");
          sym22__[] = Plus__(3, 24);
          break;
        }
        while(sym22__) {
          FnPrint__("body");
          for(sym23__ in 1:1) {
            FnPrint__("g");
            sym22__[] = Plus__(3, 24);
            break;
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars { |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block
        (((fdrt (UInt)) (fdname f) (fdargs ((AutoDiffable z UInt)))
          (fdbody
           ((stmt
             (Block
              (((stmt
                 (NRFunApp CompilerInternal FnPrint__
                  (((expr (Lit Str f))
                    (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>))
               ((stmt
                 (Return
                  (((expr (Lit Int 42))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>)))))
            (smeta <opaque>)))
          (fdloc
           ((begin_loc
             ((filename string) (line_num 3) (col_num 8) (included_from ())))
            (end_loc
             ((filename string) (line_num 6) (col_num 9) (included_from ()))))))
         ((fdrt (UInt)) (fdname g) (fdargs ((AutoDiffable z UInt)))
          (fdbody
           ((stmt
             (Block
              (((stmt
                 (NRFunApp CompilerInternal FnPrint__
                  (((expr (Lit Str g))
                    (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>))
               ((stmt
                 (Return
                  (((expr
                     (FunApp StanLib Plus__
                      (((expr (Var z))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                       ((expr (Lit Int 24))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>)))))
            (smeta <opaque>)))
          (fdloc
           ((begin_loc
             ((filename string) (line_num 7) (col_num 8) (included_from ())))
            (end_loc
             ((filename string) (line_num 10) (col_num 9) (included_from ()))))))))
       (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (SList
                (((stmt
                   (Decl (decl_adtype AutoDiffable) (decl_id sym24__)
                    (decl_type (Unsized UInt))))
                  (smeta <opaque>))
                 ((stmt
                   (For (loopvar sym25__)
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
                            (((expr (Lit Str g))
                              (emeta
                               ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
                          (smeta <opaque>))
                         ((stmt
                           (SList
                            (((stmt
                               (Assignment (sym24__ ())
                                ((expr
                                  (FunApp StanLib Plus__
                                   (((expr (Lit Int 3))
                                     (emeta
                                      ((mtype UInt) (mloc <opaque>)
                                       (madlevel DataOnly))))
                                    ((expr (Lit Int 24))
                                     (emeta
                                      ((mtype UInt) (mloc <opaque>)
                                       (madlevel DataOnly)))))))
                                 (emeta
                                  ((mtype UInt) (mloc <opaque>)
                                   (madlevel DataOnly))))))
                              (smeta <opaque>))
                             ((stmt Break) (smeta <opaque>)))))
                          (smeta <opaque>)))))
                      (smeta <opaque>)))))
                  (smeta <opaque>))
                 ((stmt
                   (IfElse
                    ((expr (Var sym24__))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))
                    ((stmt
                      (NRFunApp CompilerInternal FnPrint__
                       (((expr (Lit Str body))
                         (emeta
                          ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
                     (smeta <opaque>))
                    ()))
                  (smeta <opaque>)))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path ""))

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block
        (((fdrt (UInt)) (fdname f) (fdargs ((AutoDiffable z UInt)))
          (fdbody
           ((stmt
             (Block
              (((stmt
                 (NRFunApp CompilerInternal FnPrint__
                  (((expr (Lit Str f))
                    (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>))
               ((stmt
                 (Return
                  (((expr (Lit Int 42))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>)))))
            (smeta <opaque>)))
          (fdloc
           ((begin_loc
             ((filename string) (line_num 3) (col_num 8) (included_from ())))
            (end_loc
             ((filename string) (line_num 6) (col_num 9) (included_from ()))))))
         ((fdrt (UInt)) (fdname g) (fdargs ((AutoDiffable z UInt)))
          (fdbody
           ((stmt
             (Block
              (((stmt
                 (NRFunApp CompilerInternal FnPrint__
                  (((expr (Lit Str g))
                    (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>))
               ((stmt
                 (Return
                  (((expr
                     (FunApp StanLib Plus__
                      (((expr (Var z))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                       ((expr (Lit Int 24))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>)))))
            (smeta <opaque>)))
          (fdloc
           ((begin_loc
             ((filename string) (line_num 7) (col_num 8) (included_from ())))
            (end_loc
             ((filename string) (line_num 10) (col_num 9) (included_from ()))))))
         ((fdrt (UInt)) (fdname h) (fdargs ((AutoDiffable z UInt)))
          (fdbody
           ((stmt
             (Block
              (((stmt
                 (NRFunApp CompilerInternal FnPrint__
                  (((expr (Lit Str h))
                    (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>))
               ((stmt
                 (Return
                  (((expr
                     (FunApp StanLib Plus__
                      (((expr (Var z))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                       ((expr (Lit Int 4))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>)))))
            (smeta <opaque>)))
          (fdloc
           ((begin_loc
             ((filename string) (line_num 11) (col_num 8) (included_from ())))
            (end_loc
             ((filename string) (line_num 14) (col_num 9) (included_from ()))))))))
       (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (SList
                (((stmt
                   (Decl (decl_adtype AutoDiffable) (decl_id sym26__)
                    (decl_type (Unsized UInt))))
                  (smeta <opaque>))
                 ((stmt
                   (For (loopvar sym27__)
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
                            (((expr (Lit Str f))
                              (emeta
                               ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
                          (smeta <opaque>))
                         ((stmt
                           (SList
                            (((stmt
                               (Assignment (sym26__ ())
                                ((expr (Lit Int 42))
                                 (emeta
                                  ((mtype UInt) (mloc <opaque>)
                                   (madlevel DataOnly))))))
                              (smeta <opaque>))
                             ((stmt Break) (smeta <opaque>)))))
                          (smeta <opaque>)))))
                      (smeta <opaque>)))))
                  (smeta <opaque>))
                 ((stmt
                   (IfElse
                    ((expr (Var sym26__))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))
                    ((stmt
                      (SList
                       (((stmt
                          (Decl (decl_adtype AutoDiffable) (decl_id sym28__)
                           (decl_type (Unsized UInt))))
                         (smeta <opaque>))
                        ((stmt
                          (For (loopvar sym29__)
                           (lower
                            ((expr (Lit Int 1))
                             (emeta
                              ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                           (upper
                            ((expr (Lit Int 1))
                             (emeta
                              ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                           (body
                            ((stmt
                              (Block
                               (((stmt
                                  (NRFunApp CompilerInternal FnPrint__
                                   (((expr (Lit Str g))
                                     (emeta
                                      ((mtype UReal) (mloc <opaque>)
                                       (madlevel DataOnly)))))))
                                 (smeta <opaque>))
                                ((stmt
                                  (SList
                                   (((stmt
                                      (Assignment (sym28__ ())
                                       ((expr
                                         (FunApp StanLib Plus__
                                          (((expr (Lit Int 3))
                                            (emeta
                                             ((mtype UInt) (mloc <opaque>)
                                              (madlevel DataOnly))))
                                           ((expr (Lit Int 24))
                                            (emeta
                                             ((mtype UInt) (mloc <opaque>)
                                              (madlevel DataOnly)))))))
                                        (emeta
                                         ((mtype UInt) (mloc <opaque>)
                                          (madlevel DataOnly))))))
                                     (smeta <opaque>))
                                    ((stmt Break) (smeta <opaque>)))))
                                 (smeta <opaque>)))))
                             (smeta <opaque>)))))
                         (smeta <opaque>)))))
                     (smeta <opaque>))
                    (((stmt
                       (SList
                        (((stmt
                           (Decl (decl_adtype AutoDiffable) (decl_id sym30__)
                            (decl_type (Unsized UInt))))
                          (smeta <opaque>))
                         ((stmt
                           (For (loopvar sym31__)
                            (lower
                             ((expr (Lit Int 1))
                              (emeta
                               ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                            (upper
                             ((expr (Lit Int 1))
                              (emeta
                               ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                            (body
                             ((stmt
                               (Block
                                (((stmt
                                   (NRFunApp CompilerInternal FnPrint__
                                    (((expr (Lit Str h))
                                      (emeta
                                       ((mtype UReal) (mloc <opaque>)
                                        (madlevel DataOnly)))))))
                                  (smeta <opaque>))
                                 ((stmt
                                   (SList
                                    (((stmt
                                       (Assignment (sym30__ ())
                                        ((expr
                                          (FunApp StanLib Plus__
                                           (((expr (Lit Int 4))
                                             (emeta
                                              ((mtype UInt) (mloc <opaque>)
                                               (madlevel DataOnly))))
                                            ((expr (Lit Int 4))
                                             (emeta
                                              ((mtype UInt) (mloc <opaque>)
                                               (madlevel DataOnly)))))))
                                         (emeta
                                          ((mtype UInt) (mloc <opaque>)
                                           (madlevel DataOnly))))))
                                      (smeta <opaque>))
                                     ((stmt Break) (smeta <opaque>)))))
                                  (smeta <opaque>)))))
                              (smeta <opaque>)))))
                          (smeta <opaque>)))))
                      (smeta <opaque>)))))
                  (smeta <opaque>))
                 ((stmt
                   (NRFunApp CompilerInternal FnPrint__
                    (((expr
                       (TernaryIf
                        ((expr (Var sym26__))
                         (emeta
                          ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))
                        ((expr (Var sym28__))
                         (emeta
                          ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))
                        ((expr (Var sym30__))
                         (emeta
                          ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable))))))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                  (smeta <opaque>)))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block
        (((fdrt (UInt)) (fdname f) (fdargs ((AutoDiffable z UInt)))
          (fdbody
           ((stmt
             (Block
              (((stmt
                 (IfElse
                  ((expr (Lit Int 2))
                   (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                  ((stmt
                    (Block
                     (((stmt
                        (NRFunApp CompilerInternal FnPrint__
                         (((expr (Lit Str f))
                           (emeta
                            ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
                       (smeta <opaque>))
                      ((stmt
                        (Return
                         (((expr (Lit Int 42))
                           (emeta
                            ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                       (smeta <opaque>)))))
                   (smeta <opaque>))
                  ()))
                (smeta <opaque>))
               ((stmt
                 (Return
                  (((expr (Lit Int 6))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>)))))
            (smeta <opaque>)))
          (fdloc
           ((begin_loc
             ((filename string) (line_num 3) (col_num 8) (included_from ())))
            (end_loc
             ((filename string) (line_num 9) (col_num 9) (included_from ()))))))))
       (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (SList
                (((stmt
                   (Decl (decl_adtype AutoDiffable) (decl_id sym32__)
                    (decl_type (Unsized UInt))))
                  (smeta <opaque>))
                 ((stmt
                   (For (loopvar sym33__)
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
                           (IfElse
                            ((expr (Lit Int 2))
                             (emeta
                              ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                            ((stmt
                              (Block
                               (((stmt
                                  (NRFunApp CompilerInternal FnPrint__
                                   (((expr (Lit Str f))
                                     (emeta
                                      ((mtype UReal) (mloc <opaque>)
                                       (madlevel DataOnly)))))))
                                 (smeta <opaque>))
                                ((stmt
                                  (SList
                                   (((stmt
                                      (Assignment (sym32__ ())
                                       ((expr (Lit Int 42))
                                        (emeta
                                         ((mtype UInt) (mloc <opaque>)
                                          (madlevel DataOnly))))))
                                     (smeta <opaque>))
                                    ((stmt Break) (smeta <opaque>)))))
                                 (smeta <opaque>)))))
                             (smeta <opaque>))
                            ()))
                          (smeta <opaque>))
                         ((stmt
                           (SList
                            (((stmt
                               (Assignment (sym32__ ())
                                ((expr (Lit Int 6))
                                 (emeta
                                  ((mtype UInt) (mloc <opaque>)
                                   (madlevel DataOnly))))))
                              (smeta <opaque>))
                             ((stmt Break) (smeta <opaque>)))))
                          (smeta <opaque>)))))
                      (smeta <opaque>)))))
                  (smeta <opaque>))
                 ((stmt
                   (NRFunApp CompilerInternal FnPrint__
                    (((expr (Var sym32__))
                      (emeta
                       ((mtype UInt) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (smeta <opaque>)))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (SList
                (((stmt
                   (SList
                    (((stmt
                       (NRFunApp CompilerInternal FnPrint__
                        (((expr (Lit Int 1))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                         ((expr (Lit Int 3))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                      (smeta <opaque>))
                     ((stmt
                       (NRFunApp CompilerInternal FnPrint__
                        (((expr (Lit Int 1))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                         ((expr (Lit Int 4))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                      (smeta <opaque>)))))
                  (smeta <opaque>))
                 ((stmt
                   (SList
                    (((stmt
                       (NRFunApp CompilerInternal FnPrint__
                        (((expr (Lit Int 2))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                         ((expr (Lit Int 3))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                      (smeta <opaque>))
                     ((stmt
                       (NRFunApp CompilerInternal FnPrint__
                        (((expr (Lit Int 2))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                         ((expr (Lit Int 4))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                      (smeta <opaque>)))))
                  (smeta <opaque>)))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (SList
                (((stmt
                   (For (loopvar j)
                    (lower
                     ((expr (Lit Int 3))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                    (upper
                     ((expr (Lit Int 4))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                    (body
                     ((stmt
                       (Block
                        (((stmt
                           (NRFunApp CompilerInternal FnPrint__
                            (((expr (Lit Int 1))
                              (emeta
                               ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                          (smeta <opaque>))
                         ((stmt Break) (smeta <opaque>)))))
                      (smeta <opaque>)))))
                  (smeta <opaque>))
                 ((stmt
                   (For (loopvar j)
                    (lower
                     ((expr (Lit Int 3))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                    (upper
                     ((expr (Lit Int 4))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                    (body
                     ((stmt
                       (Block
                        (((stmt
                           (NRFunApp CompilerInternal FnPrint__
                            (((expr (Lit Int 2))
                              (emeta
                               ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                          (smeta <opaque>))
                         ((stmt Break) (smeta <opaque>)))))
                      (smeta <opaque>)))))
                  (smeta <opaque>)))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
    ((functions_block ()) (input_vars ())
     (prepare_data
      (((stmt (Decl (decl_adtype DataOnly) (decl_id i) (decl_type (Sized SInt))))
        (smeta <opaque>))
       ((stmt
         (Assignment (i ())
          ((expr (Lit Int 42))
           (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
        (smeta <opaque>))
       ((stmt (Decl (decl_adtype DataOnly) (decl_id j) (decl_type (Sized SInt))))
        (smeta <opaque>))
       ((stmt
         (Assignment (j ())
          ((expr
            (FunApp StanLib Plus__
             (((expr (Lit Int 2))
               (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
              ((expr (Lit Int 42))
               (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
           (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
        (smeta <opaque>))))
     (log_prob
      (((stmt
         (Block
          (((stmt
             (For (loopvar x)
              (lower
               ((expr (Lit Int 1))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
              (upper
               ((expr (Lit Int 42))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
              (body
               ((stmt
                 (Block
                  (((stmt
                     (NRFunApp CompilerInternal FnPrint__
                      (((expr
                         (FunApp StanLib Plus__
                          (((expr (Lit Int 42))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                           ((expr (Lit Int 44))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                        (emeta
                         ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                    (smeta <opaque>)))))
                (smeta <opaque>)))))
            (smeta <opaque>)))))
        (smeta <opaque>))))
     (generate_quantities ()) (transform_inits ()) (output_vars ())
     (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
    ((functions_block ()) (input_vars ())
     (prepare_data
      (((stmt (Decl (decl_adtype DataOnly) (decl_id i) (decl_type (Sized SInt))))
        (smeta <opaque>))
       ((stmt
         (Assignment (i ())
          ((expr (Lit Int 42))
           (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
        (smeta <opaque>))
       ((stmt
         (Block
          (((stmt
             (Decl (decl_adtype DataOnly) (decl_id j) (decl_type (Sized SInt))))
            (smeta <opaque>))
           ((stmt
             (Assignment (j ())
              ((expr (Lit Int 2))
               (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
            (smeta <opaque>)))))
        (smeta <opaque>))))
     (log_prob
      (((stmt
         (Block
          (((stmt
             (Decl (decl_adtype AutoDiffable) (decl_id j)
              (decl_type (Sized SInt))))
            (smeta <opaque>))
           ((stmt
             (For (loopvar x)
              (lower
               ((expr (Lit Int 1))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
              (upper
               ((expr (Lit Int 42))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
              (body
               ((stmt
                 (Block
                  (((stmt
                     (NRFunApp CompilerInternal FnPrint__
                      (((expr
                         (FunApp StanLib Plus__
                          (((expr (Lit Int 42))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                           ((expr (Var j))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                        (emeta
                         ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                    (smeta <opaque>)))))
                (smeta <opaque>)))))
            (smeta <opaque>)))))
        (smeta <opaque>))))
     (generate_quantities ()) (transform_inits ()) (output_vars ())
     (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
    ((functions_block ()) (input_vars ()) (prepare_data ())
     (log_prob
      (((stmt
         (Block
          (((stmt
             (Decl (decl_adtype AutoDiffable) (decl_id i)
              (decl_type (Sized SInt))))
            (smeta <opaque>))
           ((stmt
             (Assignment (i ())
              ((expr (Lit Int 42))
               (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
            (smeta <opaque>))
           ((stmt
             (Decl (decl_adtype AutoDiffable) (decl_id j)
              (decl_type (Sized SInt))))
            (smeta <opaque>))
           ((stmt
             (Assignment (j ())
              ((expr (Lit Int 2))
               (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
            (smeta <opaque>)))))
        (smeta <opaque>))))
     (generate_quantities
      (((stmt (Decl (decl_adtype DataOnly) (decl_id i) (decl_type (Sized SInt))))
        (smeta <opaque>))
       ((stmt (Decl (decl_adtype DataOnly) (decl_id j) (decl_type (Sized SInt))))
        (smeta <opaque>))
       ((stmt
         (For (loopvar x)
          (lower
           ((expr (Lit Int 1))
            (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
          (upper
           ((expr (Var i))
            (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
          (body
           ((stmt
             (Block
              (((stmt
                 (NRFunApp CompilerInternal FnPrint__
                  (((expr
                     (FunApp StanLib Plus__
                      (((expr (Var i))
                        (emeta
                         ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                       ((expr (Var j))
                        (emeta
                         ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>)))))
            (smeta <opaque>)))))
        (smeta <opaque>))))
     (transform_inits ())
     (output_vars
      ((i (SInt GeneratedQuantities)) (j (SInt GeneratedQuantities))))
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ())
       (prepare_data
        (((stmt (Decl (decl_adtype DataOnly) (decl_id i) (decl_type (Sized SInt))))
          (smeta <opaque>))
         ((stmt (Decl (decl_adtype DataOnly) (decl_id j) (decl_type (Sized SInt))))
          (smeta <opaque>))
         ((stmt
           (Assignment (j ())
            ((expr
              (FunApp StanLib Plus__
               (((expr (Lit Int 2))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((expr (Var i))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
          (smeta <opaque>))))
       (log_prob
        (((stmt
           (Block
            (((stmt
               (For (loopvar x)
                (lower
                 ((expr (Lit Int 1))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                (upper
                 ((expr (Var i))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                (body
                 ((stmt
                   (Block
                    (((stmt
                       (NRFunApp CompilerInternal FnPrint__
                        (((expr
                           (FunApp StanLib Plus__
                            (((expr (Var i))
                              (emeta
                               ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                             ((expr
                               (FunApp StanLib Plus__
                                (((expr (Lit Int 2))
                                  (emeta
                                   ((mtype UInt) (mloc <opaque>)
                                    (madlevel DataOnly))))
                                 ((expr (Var i))
                                  (emeta
                                   ((mtype UInt) (mloc <opaque>)
                                    (madlevel DataOnly)))))))
                              (emeta
                               ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                      (smeta <opaque>)))))
                  (smeta <opaque>)))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ())
       (prepare_data
        (((stmt (Decl (decl_adtype DataOnly) (decl_id i) (decl_type (Sized SInt))))
          (smeta <opaque>))
         ((stmt (Decl (decl_adtype DataOnly) (decl_id j) (decl_type (Sized SInt))))
          (smeta <opaque>))
         ((stmt
           (Assignment (j ())
            ((expr (Var i))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
          (smeta <opaque>))
         ((stmt (Decl (decl_adtype DataOnly) (decl_id k) (decl_type (Sized SInt))))
          (smeta <opaque>))
         ((stmt
           (Assignment (k ())
            ((expr
              (FunApp StanLib Times__
               (((expr (Lit Int 2))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((expr (Var i))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
          (smeta <opaque>))))
       (log_prob
        (((stmt
           (Block
            (((stmt
               (For (loopvar x)
                (lower
                 ((expr (Lit Int 1))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                (upper
                 ((expr (Var i))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                (body
                 ((stmt
                   (Block
                    (((stmt
                       (NRFunApp CompilerInternal FnPrint__
                        (((expr
                           (FunApp StanLib Plus__
                            (((expr
                               (FunApp StanLib Plus__
                                (((expr (Var i))
                                  (emeta
                                   ((mtype UInt) (mloc <opaque>)
                                    (madlevel DataOnly))))
                                 ((expr (Var i))
                                  (emeta
                                   ((mtype UInt) (mloc <opaque>)
                                    (madlevel DataOnly)))))))
                              (emeta
                               ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                             ((expr (Var k))
                              (emeta
                               ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                      (smeta <opaque>)))))
                  (smeta <opaque>)))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ())
       (prepare_data
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id i)
            (decl_type
             (Sized
              (SArray SInt
               ((expr (Lit Int 2))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
          (smeta <opaque>))
         ((stmt
           (Assignment (i ())
            ((expr
              (FunApp CompilerInternal FnMakeArray__
               (((expr (Lit Int 3))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((expr (Lit Int 2))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
             (emeta ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))))
          (smeta <opaque>))
         ((stmt
           (Decl (decl_adtype DataOnly) (decl_id j)
            (decl_type
             (Sized
              (SArray SInt
               ((expr (Lit Int 2))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
          (smeta <opaque>))
         ((stmt
           (Assignment (j ())
            ((expr
              (FunApp CompilerInternal FnMakeArray__
               (((expr (Lit Int 3))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((expr (Lit Int 2))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
             (emeta ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))))
          (smeta <opaque>))
         ((stmt
           (Assignment
            (j
             ((Single
               ((expr (Lit Int 1))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
            ((expr (Lit Int 2))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
          (smeta <opaque>))))
       (log_prob
        (((stmt
           (Block
            (((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var i))
                  (emeta
                   ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var j))
                  (emeta
                   ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id i)
                (decl_type (Sized SInt))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities
        (((stmt
           (Block
            (((stmt
               (Decl (decl_adtype DataOnly) (decl_id i) (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var i))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (transform_inits ()) (output_vars ()) (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block
        (((fdrt (UReal)) (fdname f) (fdargs ())
          (fdbody
           ((stmt
             (Block
              (((stmt
                 (Decl (decl_adtype AutoDiffable) (decl_id x)
                  (decl_type (Sized SInt))))
                (smeta <opaque>))
               ((stmt
                 (Return
                  (((expr (Lit Int 24))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>)))))
            (smeta <opaque>)))
          (fdloc
           ((begin_loc
             ((filename string) (line_num 3) (col_num 8) (included_from ())))
            (end_loc
             ((filename string) (line_num 7) (col_num 9) (included_from ()))))))))
       (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Lit Int 42))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id i)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var i))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id i)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var i))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (While
                ((expr (Lit Int 1))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((stmt Skip) (smeta <opaque>))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id i)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var i))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (Block
                (((stmt
                   (NRFunApp CompilerInternal FnPrint__
                    (((expr (Lit Str hello))
                      (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
                  (smeta <opaque>)))))
              (smeta <opaque>))
             ((stmt
               (Block
                (((stmt
                   (NRFunApp CompilerInternal FnPrint__
                    (((expr (Lit Str goodbye))
                      (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
                  (smeta <opaque>)))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id i)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var i))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "partial evaluation" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (IfElse
                ((expr (Lit Int 0))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((stmt
                  (Block
                   (((stmt
                      (Decl (decl_adtype AutoDiffable) (decl_id i)
                       (decl_type (Sized SInt))))
                     (smeta <opaque>))
                    ((stmt
                      (NRFunApp CompilerInternal FnPrint__
                       (((expr (Lit Int 3))
                         (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                     (smeta <opaque>))
                    ((stmt
                      (NRFunApp CompilerInternal FnPrint__
                       (((expr
                          (FunApp StanLib Plus__
                           (((expr (Var i))
                             (emeta
                              ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                            ((expr (Lit Int 3))
                             (emeta
                              ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                         (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                     (smeta <opaque>))
                    ((stmt
                      (NRFunApp CompilerInternal FnPrint__
                       (((expr
                          (FunApp StanLib log1m
                           (((expr (Var i))
                             (emeta
                              ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                         (emeta
                          ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
                     (smeta <opaque>)))))
                 (smeta <opaque>))
                ()))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "try partially evaluate" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id x)
                (decl_type (Sized SReal))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id y)
                (decl_type (Sized SReal))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id a)
                (decl_type
                 (Sized
                  (SVector
                   ((expr (Lit Int 2))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id b)
                (decl_type
                 (Sized
                  (SVector
                   ((expr (Lit Int 2))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib log_diff_exp
                    (((expr (Var x))
                      (emeta
                       ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var y))
                      (emeta
                       ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib log
                    (((expr
                       (FunApp StanLib Minus__
                        (((expr
                           (FunApp StanLib exp
                            (((expr (Var a))
                              (emeta
                               ((mtype UVector) (mloc <opaque>)
                                (madlevel AutoDiffable)))))))
                          (emeta
                           ((mtype UVector) (mloc <opaque>)
                            (madlevel AutoDiffable))))
                         ((expr
                           (FunApp StanLib exp
                            (((expr (Var b))
                              (emeta
                               ((mtype UVector) (mloc <opaque>)
                                (madlevel AutoDiffable)))))))
                          (emeta
                           ((mtype UVector) (mloc <opaque>)
                            (madlevel AutoDiffable)))))))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "partially evaluate with equality check" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id x)
                (decl_type
                 (Sized
                  (SVector
                   ((expr (Lit Int 2))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id y)
                (decl_type
                 (Sized
                  (SVector
                   ((expr (Lit Int 2))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib dot_self
                    (((expr (Var x))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib dot_product
                    (((expr (Var x))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var y))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "partially evaluate glm" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id x)
                (decl_type
                 (Sized
                  (SMatrix
                   ((expr (Lit Int 2))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                   ((expr (Lit Int 3))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id y)
                (decl_type
                 (Sized
                  (SArray SInt
                   ((expr (Lit Int 2))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id y_real)
                (decl_type
                 (Sized
                  (SVector
                   ((expr (Lit Int 2))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id beta)
                (decl_type
                 (Sized
                  (SVector
                   ((expr (Lit Int 3))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id alpha)
                (decl_type
                 (Sized
                  (SVector
                   ((expr (Lit Int 2))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id sigma)
                (decl_type (Sized SReal))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib bernoulli_logit_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib bernoulli_logit_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib bernoulli_logit_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib bernoulli_logit_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib bernoulli_logit_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Lit Int 0))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib bernoulli_logit_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Lit Int 0))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib neg_binomial_2_log_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var sigma))
                      (emeta
                       ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib neg_binomial_2_log_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var sigma))
                      (emeta
                       ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib neg_binomial_2_log_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var sigma))
                      (emeta
                       ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib neg_binomial_2_log_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var sigma))
                      (emeta
                       ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib neg_binomial_2_log_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Lit Int 0))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var sigma))
                      (emeta
                       ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib neg_binomial_2_log_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Lit Int 0))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var sigma))
                      (emeta
                       ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib normal_id_glm_lpdf
                    (((expr (Var y_real))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var sigma))
                      (emeta
                       ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib normal_id_glm_lpdf
                    (((expr (Var y_real))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var sigma))
                      (emeta
                       ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib normal_id_glm_lpdf
                    (((expr (Var y_real))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Lit Int 0))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var sigma))
                      (emeta
                       ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib poisson_log_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib poisson_log_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib poisson_log_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib poisson_log_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var alpha))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib poisson_log_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Lit Int 0))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib poisson_log_glm_lpmf
                    (((expr (Var y))
                      (emeta
                       ((mtype (UArray UInt)) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var x))
                      (emeta
                       ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Lit Int 0))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Var beta))
                      (emeta
                       ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "lazy code motion" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
    ((functions_block ()) (input_vars ()) (prepare_data ())
     (log_prob
      (((stmt
         (Decl (decl_adtype DataOnly) (decl_id sym34__)
          (decl_type (Unsized (UArray UReal)))))
        (smeta <opaque>))
       ((stmt
         (Block
          (((stmt
             (Assignment (sym34__ ())
              ((expr
                (FunApp CompilerInternal FnMakeArray__
                 (((expr (Lit Real 3.0))
                   (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
               (emeta
                ((mtype (UArray UReal)) (mloc <opaque>) (madlevel DataOnly))))))
            (smeta <opaque>))
           ((stmt
             (NRFunApp CompilerInternal FnPrint__
              (((expr (Var sym34__))
                (emeta
                 ((mtype (UArray UReal)) (mloc <opaque>) (madlevel DataOnly)))))))
            (smeta <opaque>))
           ((stmt
             (NRFunApp CompilerInternal FnPrint__
              (((expr (Var sym34__))
                (emeta
                 ((mtype (UArray UReal)) (mloc <opaque>) (madlevel DataOnly)))))))
            (smeta <opaque>))
           ((stmt
             (NRFunApp CompilerInternal FnPrint__
              (((expr (Var sym34__))
                (emeta
                 ((mtype (UArray UReal)) (mloc <opaque>) (madlevel DataOnly)))))))
            (smeta <opaque>)))))
        (smeta <opaque>))))
     (generate_quantities ()) (transform_inits ()) (output_vars ())
     (prog_name "") (prog_path "")) |}]

let%expect_test "lazy code motion, 2" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym36__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym35__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Block
            (((stmt
               (For (loopvar i)
                (lower
                 ((expr (Lit Int 1))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                (upper
                 ((expr (Lit Int 2))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                (body
                 ((stmt
                   (Block
                    (((stmt
                       (NRFunApp CompilerInternal FnPrint__
                        (((expr
                           (FunApp StanLib Plus__
                            (((expr (Lit Int 3))
                              (emeta
                               ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                             ((expr (Lit Int 4))
                              (emeta
                               ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                      (smeta <opaque>))
                     ((stmt Skip) (smeta <opaque>)))))
                  (smeta <opaque>)))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "lazy code motion, 3" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym38__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym37__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Block
            (((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Lit Int 3))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (Assignment (sym37__ ())
                ((expr
                  (FunApp StanLib Plus__
                   (((expr (Lit Int 3))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                    ((expr (Lit Int 5))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var sym37__))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib Plus__
                    (((expr (Var sym37__))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Lit Int 7))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "lazy code motion, 4" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym39__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id b)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id c)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id x)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id y)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Assignment (b ())
                ((expr (Lit Int 1))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
              (smeta <opaque>))
             ((stmt
               (IfElse
                ((expr (Lit Int 1))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((stmt
                  (Block
                   (((stmt
                      (Block
                       (((stmt Skip) (smeta <opaque>))
                        ((stmt Skip) (smeta <opaque>))
                        ((stmt Skip) (smeta <opaque>)))))
                     (smeta <opaque>))
                    ((stmt
                      (Assignment (sym39__ ())
                       ((expr
                         (FunApp StanLib Plus__
                          (((expr (Var b))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                           ((expr (Var c))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
                     (smeta <opaque>))
                    ((stmt Skip) (smeta <opaque>)))))
                 (smeta <opaque>))
                (((stmt
                   (Block
                    (((stmt
                       (Block
                        (((stmt
                           (Assignment (sym39__ ())
                            ((expr
                              (FunApp StanLib Plus__
                               (((expr (Var b))
                                 (emeta
                                  ((mtype UInt) (mloc <opaque>)
                                   (madlevel DataOnly))))
                                ((expr (Var c))
                                 (emeta
                                  ((mtype UInt) (mloc <opaque>)
                                   (madlevel DataOnly)))))))
                             (emeta
                              ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
                          (smeta <opaque>))
                         ((stmt
                           (Assignment (x ())
                            ((expr (Var sym39__))
                             (emeta
                              ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
                          (smeta <opaque>))
                         ((stmt Skip) (smeta <opaque>)))))
                      (smeta <opaque>))
                     ((stmt Skip) (smeta <opaque>)))))
                  (smeta <opaque>)))))
              (smeta <opaque>))
             ((stmt
               (Assignment (y ())
                ((expr (Var sym39__))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "lazy code motion, 5" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym40__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id b)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id c)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id x)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id y)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Assignment (b ())
                ((expr (Lit Int 1))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
              (smeta <opaque>))
             ((stmt
               (IfElse
                ((expr (Lit Int 1))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((stmt
                  (Block
                   (((stmt
                      (Block
                       (((stmt Skip) (smeta <opaque>))
                        ((stmt Skip) (smeta <opaque>))
                        ((stmt Skip) (smeta <opaque>)))))
                     (smeta <opaque>))
                    ((stmt
                      (Assignment (sym40__ ())
                       ((expr
                         (FunApp StanLib Plus__
                          (((expr (Var b))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                           ((expr (Var c))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
                     (smeta <opaque>))
                    ((stmt Skip) (smeta <opaque>)))))
                 (smeta <opaque>))
                (((stmt
                   (Block
                    (((stmt
                       (Block
                        (((stmt
                           (IfElse
                            ((expr (Lit Int 2))
                             (emeta
                              ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                            ((stmt
                              (Block
                               (((stmt
                                  (Assignment (sym40__ ())
                                   ((expr
                                     (FunApp StanLib Plus__
                                      (((expr (Var b))
                                        (emeta
                                         ((mtype UInt) (mloc <opaque>)
                                          (madlevel DataOnly))))
                                       ((expr (Var c))
                                        (emeta
                                         ((mtype UInt) (mloc <opaque>)
                                          (madlevel DataOnly)))))))
                                    (emeta
                                     ((mtype UInt) (mloc <opaque>)
                                      (madlevel DataOnly))))))
                                 (smeta <opaque>))
                                ((stmt
                                  (Assignment (x ())
                                   ((expr (Var sym40__))
                                    (emeta
                                     ((mtype UInt) (mloc <opaque>)
                                      (madlevel DataOnly))))))
                                 (smeta <opaque>))
                                ((stmt Skip) (smeta <opaque>)))))
                             (smeta <opaque>))
                            (((stmt
                               (SList
                                (((stmt
                                   (Assignment (sym40__ ())
                                    ((expr
                                      (FunApp StanLib Plus__
                                       (((expr (Var b))
                                         (emeta
                                          ((mtype UInt) (mloc <opaque>)
                                           (madlevel DataOnly))))
                                        ((expr (Var c))
                                         (emeta
                                          ((mtype UInt) (mloc <opaque>)
                                           (madlevel DataOnly)))))))
                                     (emeta
                                      ((mtype UInt) (mloc <opaque>)
                                       (madlevel DataOnly))))))
                                  (smeta <opaque>))
                                 ((stmt Skip) (smeta <opaque>)))))
                              (smeta <opaque>)))))
                          (smeta <opaque>))
                         ((stmt Skip) (smeta <opaque>)))))
                      (smeta <opaque>))
                     ((stmt Skip) (smeta <opaque>)))))
                  (smeta <opaque>)))))
              (smeta <opaque>))
             ((stmt
               (Assignment (y ())
                ((expr (Var sym40__))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "lazy code motion, 6" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym42__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym41__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id x)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id y)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (IfElse
                ((expr (Lit Int 2))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((stmt
                  (Block
                   (((stmt
                      (Assignment (x ())
                       ((expr
                         (FunApp StanLib Plus__
                          (((expr (Lit Int 1))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                           ((expr (Lit Int 2))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
                     (smeta <opaque>))
                    ((stmt Skip) (smeta <opaque>)))))
                 (smeta <opaque>))
                (((stmt Skip) (smeta <opaque>)))))
              (smeta <opaque>))
             ((stmt
               (Assignment (y ())
                ((expr
                  (FunApp StanLib Plus__
                   (((expr (Lit Int 4))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                    ((expr (Lit Int 3))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "lazy code motion, 7" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym44__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym43__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id a)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id b)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id c)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id x)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id y)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id z)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (IfElse
                ((expr (Lit Int 1))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((stmt
                  (Block
                   (((stmt
                      (Block
                       (((stmt
                          (Assignment (a ())
                           ((expr (Var c))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
                         (smeta <opaque>))
                        ((stmt
                          (Assignment (x ())
                           ((expr
                             (FunApp StanLib Plus__
                              (((expr (Var a))
                                (emeta
                                 ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                               ((expr (Var b))
                                (emeta
                                 ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
                         (smeta <opaque>)))))
                     (smeta <opaque>))
                    ((stmt Skip) (smeta <opaque>)))))
                 (smeta <opaque>))
                (((stmt
                   (Block
                    (((stmt Skip) (smeta <opaque>)) ((stmt Skip) (smeta <opaque>)))))
                  (smeta <opaque>)))))
              (smeta <opaque>))
             ((stmt
               (IfElse
                ((expr (Lit Int 2))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((stmt
                  (Block
                   (((stmt
                      (Block
                       (((stmt
                          (IfElse
                           ((expr (Lit Int 3))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                           ((stmt
                             (Block
                              (((stmt
                                 (Block
                                  (((stmt
                                     (Assignment (sym44__ ())
                                      ((expr
                                        (FunApp StanLib Plus__
                                         (((expr (Var a))
                                           (emeta
                                            ((mtype UInt) (mloc <opaque>)
                                             (madlevel DataOnly))))
                                          ((expr (Var b))
                                           (emeta
                                            ((mtype UInt) (mloc <opaque>)
                                             (madlevel DataOnly)))))))
                                       (emeta
                                        ((mtype UInt) (mloc <opaque>)
                                         (madlevel DataOnly))))))
                                    (smeta <opaque>))
                                   ((stmt Skip) (smeta <opaque>))
                                   ((stmt
                                     (While
                                      ((expr (Lit Int 4))
                                       (emeta
                                        ((mtype UInt) (mloc <opaque>)
                                         (madlevel DataOnly))))
                                      ((stmt
                                        (Block
                                         (((stmt
                                            (Assignment (y ())
                                             ((expr (Var sym44__))
                                              (emeta
                                               ((mtype UInt) (mloc <opaque>)
                                                (madlevel DataOnly))))))
                                           (smeta <opaque>))
                                          ((stmt Skip) (smeta <opaque>)))))
                                       (smeta <opaque>))))
                                    (smeta <opaque>))
                                   ((stmt Skip) (smeta <opaque>)))))
                                (smeta <opaque>))
                               ((stmt Skip) (smeta <opaque>)))))
                            (smeta <opaque>))
                           (((stmt
                              (Block
                               (((stmt
                                  (Block
                                   (((stmt Skip) (smeta <opaque>))
                                    ((stmt
                                      (While
                                       ((expr (Lit Int 5))
                                        (emeta
                                         ((mtype UInt) (mloc <opaque>)
                                          (madlevel DataOnly))))
                                       ((stmt
                                         (Block
                                          (((stmt Skip) (smeta <opaque>))
                                           ((stmt Skip) (smeta <opaque>)))))
                                        (smeta <opaque>))))
                                     (smeta <opaque>))
                                    ((stmt
                                      (Assignment (sym44__ ())
                                       ((expr
                                         (FunApp StanLib Plus__
                                          (((expr (Var a))
                                            (emeta
                                             ((mtype UInt) (mloc <opaque>)
                                              (madlevel DataOnly))))
                                           ((expr (Var b))
                                            (emeta
                                             ((mtype UInt) (mloc <opaque>)
                                              (madlevel DataOnly)))))))
                                        (emeta
                                         ((mtype UInt) (mloc <opaque>)
                                          (madlevel DataOnly))))))
                                     (smeta <opaque>))
                                    ((stmt
                                      (Assignment (y ())
                                       ((expr (Var sym44__))
                                        (emeta
                                         ((mtype UInt) (mloc <opaque>)
                                          (madlevel DataOnly))))))
                                     (smeta <opaque>)))))
                                 (smeta <opaque>))
                                ((stmt Skip) (smeta <opaque>)))))
                             (smeta <opaque>)))))
                         (smeta <opaque>))
                        ((stmt
                          (Assignment (z ())
                           ((expr (Var sym44__))
                            (emeta
                             ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
                         (smeta <opaque>)))))
                     (smeta <opaque>))
                    ((stmt Skip) (smeta <opaque>)))))
                 (smeta <opaque>))
                (((stmt
                   (Block
                    (((stmt Skip) (smeta <opaque>)) ((stmt Skip) (smeta <opaque>)))))
                  (smeta <opaque>)))))
              (smeta <opaque>))
             ((stmt Skip) (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "lazy code motion, 8, _lp functions not optimized" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block
        (((fdrt (UInt)) (fdname foo_lp) (fdargs ((AutoDiffable x UInt)))
          (fdbody
           ((stmt
             (SList
              (((stmt
                 (Block
                  (((stmt
                     (TargetPE
                      ((expr (Lit Int 1))
                       (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
                    (smeta <opaque>))
                   ((stmt
                     (Return
                      (((expr (Lit Int 24))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                    (smeta <opaque>)))))
                (smeta <opaque>)))))
            (smeta <opaque>)))
          (fdloc
           ((begin_loc
             ((filename string) (line_num 3) (col_num 8) (included_from ())))
            (end_loc
             ((filename string) (line_num 3) (col_num 53) (included_from ()))))))
         ((fdrt (UInt)) (fdname foo) (fdargs ((AutoDiffable x UInt)))
          (fdbody
           ((stmt
             (SList
              (((stmt
                 (Block
                  (((stmt
                     (Return
                      (((expr (Lit Int 24))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                    (smeta <opaque>)))))
                (smeta <opaque>)))))
            (smeta <opaque>)))
          (fdloc
           ((begin_loc
             ((filename string) (line_num 4) (col_num 8) (included_from ())))
            (end_loc
             ((filename string) (line_num 4) (col_num 37) (included_from ()))))))))
       (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym45__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Block
            (((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp UserDefined foo
                    (((expr
                       (FunApp UserDefined foo_lp
                        (((expr (Lit Int 1))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp UserDefined foo
                    (((expr
                       (FunApp UserDefined foo_lp
                        (((expr (Lit Int 1))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (Assignment (sym45__ ())
                ((expr
                  (FunApp UserDefined foo
                   (((expr
                      (FunApp UserDefined foo
                       (((expr (Lit Int 1))
                         (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var sym45__))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var sym45__))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "lazy code motion, 9" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym46__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id x)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (While
                ((expr
                  (FunApp StanLib Times__
                   (((expr (Var x))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                    ((expr (Lit Int 2))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((stmt
                  (Block
                   (((stmt
                      (NRFunApp CompilerInternal FnPrint__
                       (((expr (Lit Str hello))
                         (emeta
                          ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
                     (smeta <opaque>))
                    ((stmt Skip) (smeta <opaque>)))))
                 (smeta <opaque>))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "lazy code motion, 10" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym47__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id x)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Assignment (x ())
                ((expr (Lit Int 3))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib Times__
                    (((expr (Var x))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Lit Int 2))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (Assignment (x ())
                ((expr (Lit Int 2))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr
                   (FunApp StanLib Times__
                    (((expr (Var x))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Lit Int 2))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "lazy code motion, 11" =
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
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym48__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Block
            (((stmt
               (Block
                (((stmt
                   (Decl (decl_adtype AutoDiffable) (decl_id x)
                    (decl_type (Sized SInt))))
                  (smeta <opaque>))
                 ((stmt
                   (NRFunApp CompilerInternal FnPrint__
                    (((expr
                       (FunApp StanLib Times__
                        (((expr (Var x))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                         ((expr (Lit Int 2))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                  (smeta <opaque>)))))
              (smeta <opaque>))
             ((stmt
               (Block
                (((stmt
                   (Decl (decl_adtype AutoDiffable) (decl_id x)
                    (decl_type (Sized SInt))))
                  (smeta <opaque>))
                 ((stmt
                   (NRFunApp CompilerInternal FnPrint__
                    (((expr
                       (FunApp StanLib Times__
                        (((expr (Var x))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                         ((expr (Lit Int 2))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                  (smeta <opaque>)))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "lazy code motion, 12" =
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
  (* TODO: this isn't doing the right thing yet. 
     I believe the flowgraph for break and continue statements is overly conservative. *)
  let ast = Semantic_check.semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym50__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym49__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id x)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (For (loopvar i)
                (lower
                 ((expr (Lit Int 1))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                (upper
                 ((expr (Lit Int 6))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                (body
                 ((stmt
                   (Block
                    (((stmt
                       (Block
                        (((stmt
                           (NRFunApp CompilerInternal FnPrint__
                            (((expr
                               (FunApp StanLib Plus__
                                (((expr (Var x))
                                  (emeta
                                   ((mtype UInt) (mloc <opaque>)
                                    (madlevel DataOnly))))
                                 ((expr (Lit Int 42))
                                  (emeta
                                   ((mtype UInt) (mloc <opaque>)
                                    (madlevel DataOnly)))))))
                              (emeta
                               ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                          (smeta <opaque>))
                         ((stmt Continue) (smeta <opaque>))
                         ((stmt
                           (Assignment (x ())
                            ((expr (Lit Int 3))
                             (emeta
                              ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
                          (smeta <opaque>)))))
                      (smeta <opaque>))
                     ((stmt Skip) (smeta <opaque>)))))
                  (smeta <opaque>)))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "cool example: expression propagation + partial evaluation + \
                 lazy code motion + dead code elimination" =
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
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  let mir = dead_code_elimination mir in
  print_s [%sexp (mir : Mir.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Decl (decl_adtype AutoDiffable) (decl_id sym53__)
            (decl_type (Unsized UReal))))
          (smeta <opaque>))
         ((stmt
           (Decl (decl_adtype AutoDiffable) (decl_id sym52__)
            (decl_type (Unsized UReal))))
          (smeta <opaque>))
         ((stmt
           (Decl (decl_adtype DataOnly) (decl_id sym51__)
            (decl_type (Unsized UInt))))
          (smeta <opaque>))
         ((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id x)
                (decl_type (Sized SReal))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id y)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id theta)
                (decl_type (Sized SReal))))
              (smeta <opaque>))
             ((stmt
               (For (loopvar i)
                (lower
                 ((expr (Lit Int 1))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                (upper
                 ((expr (Lit Int 100000))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                (body
                 ((stmt
                   (Block
                    (((stmt
                       (Block
                        (((stmt
                           (TargetPE
                            ((expr
                              (FunApp StanLib bernoulli_logit_lpmf
                               (((expr (Var y))
                                 (emeta
                                  ((mtype UInt) (mloc <opaque>)
                                   (madlevel DataOnly))))
                                ((expr (Var x))
                                 (emeta
                                  ((mtype UReal) (mloc <opaque>)
                                   (madlevel AutoDiffable)))))))
                             (emeta
                              ((mtype UReal) (mloc <opaque>)
                               (madlevel AutoDiffable))))))
                          (smeta <opaque>)))))
                      (smeta <opaque>)))))
                  (smeta <opaque>)))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "block fixing" =
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
      Mir.log_prob=
        [ { stmt=
              IfElse
                ( zero
                , { stmt= While (zero, {stmt= SList []; smeta= no_span})
                  ; smeta= no_span }
                , None )
          ; smeta= no_span } ] }
  in
  let mir = block_fixing mir in
  print_s [%sexp (mir : Mir.typed_prog)] ;
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
