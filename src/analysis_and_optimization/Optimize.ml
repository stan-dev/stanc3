(* Code for optimization passes on the MIR *)
open Core_kernel
open Common
open Middle
open Mir_utils

let preserve_stability = false

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
        {fs with fdbody= Option.map ~f:transform fs.fdbody} )
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
    (transform :
      Stmt.Located.t Program.fun_def option -> Stmt.Located.t -> Stmt.Located.t)
    : Program.Typed.t =
  let transform' fd s =
    match transform fd {pattern= SList s; meta= Location_span.empty} with
    | {pattern= SList l; _} -> l
    | _ ->
        raise
          (Failure "Something went wrong with program transformation packing!")
  in
  let transformed_functions =
    List.map mir.functions_block ~f:(fun fs ->
        {fs with fdbody= Option.map ~f:(transform (Some fs)) fs.fdbody} )
  in
  { mir with
    functions_block= transformed_functions
  ; prepare_data= transform' None mir.prepare_data
  ; transform_inits= transform' None mir.transform_inits
  ; log_prob= transform' None mir.log_prob
  ; generate_quantities= transform' None mir.generate_quantities }

let map_no_loc l =
  List.map ~f:(fun s -> Stmt.Fixed.{pattern= s; meta= Location_span.empty}) l

let slist_no_loc l = Stmt.Fixed.Pattern.SList (map_no_loc l)
let block_no_loc l = Stmt.Fixed.Pattern.Block (map_no_loc l)

let slist_concat_no_loc l stmt =
  match l with [] -> stmt | l -> slist_no_loc (l @ [stmt])

let replace_fresh_local_vars s' =
  let f m = function
    | Stmt.Fixed.Pattern.Decl {decl_adtype; decl_type; decl_id} ->
        let new_name =
          match Map.Poly.find m decl_id with
          | Some existing -> existing
          | None -> Gensym.generate ~prefix:"inline_" ()
        in
        ( Stmt.Fixed.Pattern.Decl {decl_adtype; decl_id= new_name; decl_type}
        , Map.Poly.set m ~key:decl_id ~data:new_name )
    | Stmt.Fixed.Pattern.For {loopvar; lower; upper; body} ->
        let new_name =
          match Map.Poly.find m loopvar with
          | Some existing -> existing
          | None -> Gensym.generate ~prefix:"inline_" ()
        in
        ( Stmt.Fixed.Pattern.For {loopvar= new_name; lower; upper; body}
        , Map.Poly.set m ~key:loopvar ~data:new_name )
    | Assignment ((var_name, ut, l), e) ->
        let var_name =
          match Map.Poly.find m var_name with
          | None -> var_name
          | Some var_name -> var_name
        in
        (Stmt.Fixed.Pattern.Assignment ((var_name, ut, l), e), m)
    | x -> (x, m)
  in
  let s, m = map_rec_state_stmt_loc f Map.Poly.empty s' in
  name_subst_stmt m s

let subst_args_stmt args es =
  let m = Map.Poly.of_alist_exn (List.zip_exn args es) in
  subst_stmt m

(* TODO: only handle early returns if that's necessary *)
(* The strategy here is to wrap the function body in a dummy loop, then replace
   returns with breaks. One issue is early return from internal loops - in
   those cases, a break would only break out of the inner loop. The solution is
   a flag variable to indicate whether a 'return' break has been called, and
   then to check if that flag is set after each loop. Then, if a 'return' break
   is called from an inner loop, there's a cascade of breaks all the way out of
   the dummy loop. *)
let handle_early_returns opt_var b =
  let returned = Gensym.generate ~prefix:"inline_" () in
  let f = function
    | Stmt.Fixed.Pattern.Return opt_ret -> (
      match (opt_var, opt_ret) with
      | None, None -> Stmt.Fixed.Pattern.Break
      | Some name, Some e ->
          SList
            [ Stmt.Fixed.
                { pattern=
                    Assignment
                      ( (returned, UInt, [])
                      , Expr.Fixed.
                          { pattern= Lit (Int, "1")
                          ; meta=
                              Expr.Typed.Meta.
                                { type_= UInt
                                ; adlevel= DataOnly
                                ; loc= Location_span.empty } } )
                ; meta= Location_span.empty }
            ; Stmt.Fixed.
                { pattern= Assignment ((name, Expr.Typed.type_of e, []), e)
                ; meta= Location_span.empty }
            ; {pattern= Break; meta= Location_span.empty} ]
      | Some _, None ->
          raise_s
            [%message
              ( "Function should return a value but found an empty return \
                 statement."
                : string )]
      | None, Some _ ->
          raise_s
            [%message
              ( "Expected a void function but found a non-empty return \
                 statement."
                : string )] )
    | Stmt.Fixed.Pattern.For _ as loop ->
        Stmt.Fixed.Pattern.SList
          [ Stmt.Fixed.{pattern= loop; meta= Location_span.empty}
          ; Stmt.Fixed.
              { pattern=
                  IfElse
                    ( Expr.Fixed.
                        { pattern= Var returned
                        ; meta=
                            Expr.Typed.Meta.
                              { type_= UInt
                              ; adlevel= DataOnly
                              ; loc= Location_span.empty } }
                    , {pattern= Break; meta= Location_span.empty}
                    , None )
              ; meta= Location_span.empty } ]
    | x -> x
  in
  Stmt.Fixed.Pattern.SList
    [ Stmt.Fixed.
        { pattern=
            Decl
              {decl_adtype= DataOnly; decl_id= returned; decl_type= Sized SInt}
        ; meta= Location_span.empty }
    ; Stmt.Fixed.
        { pattern=
            Assignment
              ( (returned, UInt, [])
              , Expr.Fixed.
                  { pattern= Lit (Int, "0")
                  ; meta=
                      Expr.Typed.Meta.
                        { type_= UInt
                        ; adlevel= DataOnly
                        ; loc= Location_span.empty } } )
        ; meta= Location_span.empty }
    ; Stmt.Fixed.
        { pattern=
            Stmt.Fixed.Pattern.For
              { loopvar= Gensym.generate ~prefix:"inline_" ()
              ; lower=
                  Expr.Fixed.
                    { pattern= Lit (Int, "1")
                    ; meta=
                        Expr.Typed.Meta.
                          { type_= UInt
                          ; adlevel= DataOnly
                          ; loc= Location_span.empty } }
              ; upper=
                  { pattern= Lit (Int, "1")
                  ; meta=
                      {type_= UInt; adlevel= DataOnly; loc= Location_span.empty}
                  }
              ; body= map_rec_stmt_loc f b }
        ; meta= Location_span.empty } ]

(* Triple is (declaration list, statement list, return expression) *)
let rec inline_function_expression propto adt fim
    (Expr.Fixed.({pattern; _}) as e) =
  match pattern with
  | Var _ -> ([], [], e)
  | Lit (_, _) -> ([], [], e)
  | FunApp (t, s, es) -> (
      let dse_list =
        List.map ~f:(inline_function_expression propto adt fim) es
      in
      (* function arguments are evaluated from right to left in C++, so we need to reverse *)
      let d_list =
        List.concat (List.rev (List.map ~f:(function x, _, _ -> x) dse_list))
      in
      let s_list =
        List.concat (List.rev (List.map ~f:(function _, x, _ -> x) dse_list))
      in
      let es = List.map ~f:(function _, _, x -> x) dse_list in
      let s = if propto then s else Middle.Utils.stdlib_distribution_name s in
      match Map.find fim s with
      | None -> (d_list, s_list, {e with pattern= FunApp (t, s, es)})
      | Some (rt, args, b) ->
          let x = Gensym.generate ~prefix:"inline_" () in
          let handle = handle_early_returns (Some x) in
          let d_list2, s_list2, (e : Expr.Typed.t) =
            ( [ Stmt.Fixed.Pattern.Decl
                  {decl_adtype= adt; decl_id= x; decl_type= Option.value_exn rt}
              ]
              (* We should minimize the code that's having its variables
                   replaced to avoid conflict with the (two) new dummy
                   variables introduced by inlining *)
            , [handle (replace_fresh_local_vars (subst_args_stmt args es b))]
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
      let dl1, sl1, e1 = inline_function_expression propto adt fim e1 in
      let dl2, sl2, e2 = inline_function_expression propto adt fim e2 in
      let dl3, sl3, e3 = inline_function_expression propto adt fim e3 in
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
      let dl, sl, e' = inline_function_expression propto adt fim e' in
      let dsi_list =
        List.map ~f:(inline_function_index propto adt fim) i_list
      in
      let d_list =
        List.concat (List.rev (List.map ~f:(function x, _, _ -> x) dsi_list))
      in
      let s_list =
        List.concat (List.rev (List.map ~f:(function _, x, _ -> x) dsi_list))
      in
      let i_list = List.map ~f:(function _, _, x -> x) dsi_list in
      (d_list @ dl, s_list @ sl, {e with pattern= Indexed (e', i_list)})
  | EAnd (e1, e2) ->
      let dl1, sl1, e1 = inline_function_expression propto adt fim e1 in
      let dl2, sl2, e2 = inline_function_expression propto adt fim e2 in
      let sl2 =
        [ Stmt.Fixed.(
            Pattern.IfElse
              ( e1
              , {pattern= Block (map_no_loc sl2); meta= Location_span.empty}
              , None )) ]
      in
      (dl1 @ dl2, sl1 @ sl2, {e with pattern= EAnd (e1, e2)})
  | EOr (e1, e2) ->
      let dl1, sl1, e1 = inline_function_expression propto adt fim e1 in
      let dl2, sl2, e2 = inline_function_expression propto adt fim e2 in
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

and inline_function_index propto adt fim i =
  match i with
  | All -> ([], [], All)
  | Single e ->
      let dl, sl, e = inline_function_expression propto adt fim e in
      (dl, sl, Single e)
  | Upfrom e ->
      let dl, sl, e = inline_function_expression propto adt fim e in
      (dl, sl, Upfrom e)
  | Between (e1, e2) ->
      let dl1, sl1, e1 = inline_function_expression propto adt fim e1 in
      let dl2, sl2, e2 = inline_function_expression propto adt fim e2 in
      (dl1 @ dl2, sl1 @ sl2, Between (e1, e2))
  | MultiIndex e ->
      let dl, sl, e = inline_function_expression propto adt fim e in
      (dl, sl, MultiIndex e)

let rec inline_function_statement propto adt fim Stmt.Fixed.({pattern; meta}) =
  Stmt.Fixed.
    { pattern=
        ( match pattern with
        | Assignment ((x, ut, l), e2) ->
            let e1 =
              {e2 with pattern= Indexed ({e2 with pattern= Var x}, l)}
            in
            (* This inner e2 is wrong. We are giving the wrong type to Var x. But it doens't really matter as we discard it later. *)
            let dl1, sl1, e1 = inline_function_expression propto adt fim e1 in
            let dl2, sl2, e2 = inline_function_expression propto adt fim e2 in
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
            let d, s, e = inline_function_expression propto adt fim e in
            slist_concat_no_loc (d @ s) (TargetPE e)
        | NRFunApp (t, s, es) ->
            let dse_list =
              List.map ~f:(inline_function_expression propto adt fim) es
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
              let d, s, e = inline_function_expression propto adt fim e in
              slist_concat_no_loc (d @ s) (Return (Some e)) )
        | IfElse (e, s1, s2) ->
            let d, s, e = inline_function_expression propto adt fim e in
            slist_concat_no_loc (d @ s)
              (IfElse
                 ( e
                 , inline_function_statement propto adt fim s1
                 , Option.map ~f:(inline_function_statement propto adt fim) s2
                 ))
        | While (e, s) ->
            let d', s', e = inline_function_expression propto adt fim e in
            slist_concat_no_loc (d' @ s')
              (While
                 ( e
                 , match s' with
                   | [] -> inline_function_statement propto adt fim s
                   | _ ->
                       { pattern=
                           Block
                             ( [inline_function_statement propto adt fim s]
                             @ map_no_loc s' )
                       ; meta= Location_span.empty } ))
        | For {loopvar; lower; upper; body} ->
            let d_lower, s_lower, lower =
              inline_function_expression propto adt fim lower
            in
            let d_upper, s_upper, upper =
              inline_function_expression propto adt fim upper
            in
            slist_concat_no_loc
              (d_lower @ d_upper @ s_lower @ s_upper)
              (For
                 { loopvar
                 ; lower
                 ; upper
                 ; body=
                     ( match s_upper with
                     | [] -> inline_function_statement propto adt fim body
                     | _ ->
                         { pattern=
                             Block
                               ( [inline_function_statement propto adt fim body]
                               @ map_no_loc s_upper )
                         ; meta= Location_span.empty } ) })
        | Block l ->
            Block (List.map l ~f:(inline_function_statement propto adt fim))
        | SList l ->
            SList (List.map l ~f:(inline_function_statement propto adt fim))
        | Decl r -> Decl r
        | Skip -> Skip
        | Break -> Break
        | Continue -> Continue )
    ; meta }

let create_function_inline_map adt l =
  (* We only add the first definition for each function to the inline map.
     This will make sure we do not inline recursive functions.
     We also don't want to add any function declaration (as opposed to
     definitions), because that would replace the function call with a Skip.
  *)
  let f (accum, visited) Program.({fdname; fdargs; fdbody; fdrt; _}) =
    if Set.mem visited fdname then (accum, visited)
    else
      let accum' =
        match fdbody with
        | None -> accum
        | Some fdbody -> (
            let create_data propto =
              ( Option.map ~f:(fun x -> Type.Unsized x) fdrt
              , List.map ~f:(fun (_, name, _) -> name) fdargs
              , inline_function_statement propto adt accum fdbody )
            in
            match Middle.Utils.with_unnormalized_suffix fdname with
            | None -> (
                let data = create_data true in
                match Map.add accum ~key:fdname ~data with
                | `Ok m -> m
                | `Duplicate -> accum )
            | Some fdname' ->
                let data = create_data false in
                let data' = create_data true in
                let m =
                  Map.Poly.of_alist_exn [(fdname, data); (fdname', data')]
                in
                Map.merge_skewed accum m ~combine:(fun ~key:_ f _ -> f) )
      in
      let visited' = Set.add visited fdname in
      (accum', visited')
  in
  let accum, _ = List.fold l ~init:(Map.Poly.empty, Set.Poly.empty) ~f in
  accum

let function_inlining (mir : Program.Typed.t) =
  let dataonly_inline_map =
    create_function_inline_map UnsizedType.DataOnly mir.functions_block
  in
  let autodiff_inline_map =
    create_function_inline_map UnsizedType.AutoDiffable mir.functions_block
  in
  let dataonly_inline_function_statements =
    List.map
      ~f:
        (inline_function_statement true UnsizedType.DataOnly
           dataonly_inline_map)
  in
  let autodiffable_inline_function_statements =
    List.map
      ~f:
        (inline_function_statement true UnsizedType.AutoDiffable
           autodiff_inline_map)
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

let unroll_static_limit = 32

let unroll_static_loops_statement _ =
  let f stmt =
    match stmt with
    | Stmt.Fixed.Pattern.For {loopvar; lower; upper; body} -> (
        let lower = Partial_evaluator.eval_expr lower in
        let upper = Partial_evaluator.eval_expr upper in
        match
          (contains_top_break_or_continue body, lower.pattern, upper.pattern)
        with
        | false, Lit (Int, low_str), Lit (Int, up_str) ->
            let low = Int.of_string low_str in
            let up = Int.of_string up_str in
            if up - low > unroll_static_limit then stmt
            else
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
                  (List.range ~start:`inclusive ~stop:`inclusive low up)
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

let unroll_loop_one_step_statement _ =
  let f stmt =
    match stmt with
    | Stmt.Fixed.Pattern.For {loopvar; lower; upper; body} ->
        if contains_top_break_or_continue body then stmt
        else
          IfElse
            ( Expr.Fixed.
                {lower with pattern= FunApp (StanLib, "Geq__", [upper; lower])}
            , { pattern=
                  (let body_unrolled =
                     subst_args_stmt [loopvar] [lower]
                       {pattern= body.pattern; meta= Location_span.empty}
                   in
                   let (body' : Stmt.Located.t) =
                     { pattern=
                         Stmt.Fixed.Pattern.For
                           { loopvar
                           ; upper
                           ; body
                           ; lower=
                               { lower with
                                 pattern=
                                   FunApp
                                     ( StanLib
                                     , "Plus__"
                                     , [lower; Expr.Helpers.loop_bottom] ) } }
                     ; meta= Location_span.empty }
                   in
                   match body_unrolled.pattern with
                   | Block stmts -> Block (stmts @ [body'])
                   | _ -> Stmt.Fixed.Pattern.Block [body_unrolled; body'])
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

let collapse_lists_statement _ =
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

let rec expr_any pred (e : Expr.Typed.t) =
  match e.pattern with
  | Indexed (e, is) -> expr_any pred e || List.exists ~f:(idx_any pred) is
  | _ -> pred e || Expr.Fixed.Pattern.fold (accum_any pred) false e.pattern

and idx_any pred (i : Expr.Typed.t Index.t) =
  Index.fold (accum_any pred) false i

and accum_any pred b e = b || expr_any pred e

let can_side_effect_top_expr (e : Expr.Typed.t) =
  match e.pattern with
  | FunApp (t, f, _) ->
      String.suffix f 3 = "_lp"
      || (t = CompilerInternal && f = Internal_fun.to_string FnReadParam)
      || (t = CompilerInternal && f = Internal_fun.to_string FnReadData)
      || (t = CompilerInternal && f = Internal_fun.to_string FnWriteParam)
      || (t = CompilerInternal && f = Internal_fun.to_string FnConstrain)
      || (t = CompilerInternal && f = Internal_fun.to_string FnValidateSize)
      || (t = CompilerInternal && f = Internal_fun.to_string FnValidateSize)
      || t = CompilerInternal
         && f = Internal_fun.to_string FnValidateSizeSimplex
      || t = CompilerInternal
         && f = Internal_fun.to_string FnValidateSizeUnitVector
      || (t = CompilerInternal && f = Internal_fun.to_string FnUnconstrain)
  | _ -> false

let cannot_duplicate_expr (e : Expr.Typed.t) =
  let pred e =
    can_side_effect_top_expr e
    || ( match e.pattern with
       | FunApp (_, f, _) -> String.suffix f 4 = "_rng"
       | _ -> false )
    || (preserve_stability && UnsizedType.is_autodiffable e.meta.type_)
  in
  expr_any pred e

let cannot_remove_expr (e : Expr.Typed.t) = expr_any can_side_effect_top_expr e

let expression_propagation mir =
  propagation
    (Monotone_framework.expression_propagation_transfer cannot_duplicate_expr)
    mir

let copy_propagation mir =
  let globals = Monotone_framework.globals mir in
  propagation (Monotone_framework.copy_propagation_transfer globals) mir

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
          if Set.Poly.mem live_variables_s x || cannot_remove_expr rhs then
            stmt
          else Skip
      | Assignment ((x, _, is), rhs) ->
          if
            Set.Poly.mem live_variables_s x
            || cannot_remove_expr rhs
            || List.exists ~f:(idx_any cannot_remove_expr) is
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
            (not (cannot_remove_expr e))
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
          if (not (cannot_remove_expr e)) && b.pattern = Break then Skip
          else
            match e.pattern with
            | Lit (Int, "0") | Lit (Real, "0.0") -> Skip
            | _ -> While (e, b) )
      | For {loopvar; lower; upper; body} ->
          if
            (not (cannot_remove_expr lower))
            && (not (cannot_remove_expr upper))
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
      Monotone_framework.inverse_flowgraph_of_stmt ~blocks_after_body:false s
    in
    let fwd_flowgraph = Monotone_framework.reverse rev_flowgraph in
    let latest_expr, used_not_latest_expressions_mfp =
      Monotone_framework.lazy_expressions_mfp fwd_flowgraph rev_flowgraph
        flowgraph_to_mir
    in
    let expression_map =
      let rec collect_expressions accum (e : Expr.Typed.t) =
        match e.pattern with
        | Lit (_, _) -> accum
        | Var _ -> accum
        | _ when cannot_duplicate_expr e ->
            (* Immovable expressions might have movable subexpressions *)
            Expr.Fixed.Pattern.fold collect_expressions accum e.pattern
        | _ -> Map.set accum ~key:e ~data:(Gensym.generate ~prefix:"lcm_" ())
      in
      Set.fold
        (Monotone_framework.used_expressions_stmt s.pattern)
        ~init:Expr.Typed.Map.empty ~f:collect_expressions
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
        latest_and_used_after_i
        |> Set.filter ~f:(fun x -> Map.mem expression_map x)
        |> Set.to_list
        |> List.sort ~compare:(fun e e' ->
               compare_int (expr_depth e) (expr_depth e') )
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
      let expr_map =
        Map.filter_keys
          ~f:(fun key ->
            Set.mem latest_and_used_after_i key
            || Set.mem (Map.find_exn used_not_latest_expressions_mfp i).exit
                 key )
          (Map.mapi expression_map ~f:(fun ~key ~data ->
               {key with pattern= Var data} ))
      in
      let f = expr_subst_stmt_except_initial_assign expr_map in
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
  let cleanup =
    let cleanup_base
        (stmt : (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t) :
        (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t =
      match stmt with
      | Stmt.Fixed.(Pattern.IfElse
                      ( e
                      , {pattern= Block [b1; {pattern= Skip; _}]; _}
                      , Some {pattern= Block [b2; {pattern= Skip; _}]; _} )) ->
          IfElse (e, b1, Some b2)
      | IfElse
          ( e
          , {pattern= Block [b; {pattern= Skip; _}]; _}
          , Some {pattern= Skip; _} ) ->
          IfElse (e, b, None)
      | While (e, {pattern= Block [b; {pattern= Skip; _}]; _}) -> While (e, b)
      | For
          { loopvar
          ; lower
          ; upper
          ; body= {pattern= Block [b; {pattern= Skip; _}]; _} } ->
          For {loopvar; lower; upper; body= b}
      | _ -> stmt
    in
    map_rec_stmt_loc cleanup_base
  in
  transform_program_blockwise mir (fun _ x ->
      cleanup (transform (preprocess_flowgraph x)) )

let block_fixing mir =
  transform_program_blockwise mir (fun _ x ->
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
        x )

(* TODO: implement SlicStan style optimizer for choosing best program block for each statement. *)
(* TODO: add optimization pass to move declarations down as much as possible and introduce as
   tight as possible local scopes *)
(* TODO: add tests *)
(* TODO: add pass to get rid of redundant declarations? *)

let optimize_ad_levels (mir : Program.Typed.t) =
  let global_initial_ad_variables =
    Set.Poly.of_list
      (List.filter_map
         ~f:(fun (v, Program.({out_block; _})) ->
           match out_block with Parameters -> Some v | _ -> None )
         mir.output_vars)
  in
  let transform fundef_opt s =
    let rev_flowgraph, flowgraph_to_mir =
      Monotone_framework.inverse_flowgraph_of_stmt s
    in
    let fwd_flowgraph = Monotone_framework.reverse rev_flowgraph in
    let (module Rev_Flowgraph) = rev_flowgraph in
    let (module Fwd_Flowgraph) = fwd_flowgraph in
    let initial_ad_variables =
      match (fundef_opt : Stmt.Located.t Program.fun_def option) with
      | None -> global_initial_ad_variables
      | Some {fdargs; _} ->
          Set.Poly.union global_initial_ad_variables
            (Set.Poly.of_list
               (List.filter_map fdargs ~f:(fun (_, name, ut) ->
                    if UnsizedType.is_autodiffable ut then Some name else None
                )))
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

let settings_const b =
  { function_inlining= b
  ; static_loop_unrolling= b
  ; one_step_loop_unrolling= b
  ; list_collapsing= b
  ; block_fixing= b
  ; constant_propagation= b
  ; expression_propagation= b
  ; copy_propagation= b
  ; dead_code_elimination= b
  ; partial_evaluation= b
  ; lazy_code_motion= b
  ; optimize_ad_levels= b }

let all_optimizations : optimization_settings = settings_const true
let no_optimizations : optimization_settings = settings_const false

let optimization_suite ?(settings = all_optimizations) mir =
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
