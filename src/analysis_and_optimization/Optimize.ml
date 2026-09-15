(** Code for optimization passes on the MIR *)

open Std
open Common
open Middle
open Mir_utils
open Dataflow_types

(** Apply the transformation to each function body and to the rest of the
    program as one block. *)
let transform_program (mir : Program.Typed.t)
    (transform : Stmt.Located.t -> Stmt.Located.t) : Program.Typed.t =
  let packed_prog_body =
    transform
      { pattern=
          SList
            (List.map
               ~f:(fun x -> Stmt.{pattern= SList x; meta= Location_span.empty})
               [ mir.prepare_data; mir.transform_inits; mir.log_prob
               ; mir.reverse_mode_log_prob; mir.generate_quantities ])
      ; meta= Location_span.empty } in
  let transformed_prog_body = transform packed_prog_body in
  let transformed_functions =
    List.map mir.functions_block ~f:(fun fs ->
        Program.{fs with fdbody= Option.map ~f:transform fs.fdbody}) in
  match transformed_prog_body with
  | { pattern=
        SList
          [ {pattern= SList prepare_data'; _}
          ; {pattern= SList transform_inits'; _}; {pattern= SList log_prob'; _}
          ; {pattern= SList reverse_mode_log_prob'; _}
          ; {pattern= SList generate_quantities'; _} ]
    ; _ } ->
      { mir with
        functions_block= transformed_functions
      ; prepare_data= prepare_data'
      ; transform_inits= transform_inits'
      ; log_prob= log_prob'
      ; reverse_mode_log_prob= reverse_mode_log_prob'
      ; generate_quantities= generate_quantities' }
  | _ ->
      ICE.internal_error
        "Something went wrong with program transformation packing!"
      [@coverage off]

(** Apply the transformation to each function body and to each program block
    separately. *)
let transform_program_blockwise (mir : Program.Typed.t)
    (transform :
      Stmt.Located.t Program.fun_def option -> Stmt.Located.t -> Stmt.Located.t)
    : Program.Typed.t =
  let transform' fd s =
    match transform fd {pattern= SList s; meta= Location_span.empty} with
    | {pattern= SList l; _} -> l
    | _ ->
        ICE.internal_error
          "Something went wrong with program transformation packing!"
        [@coverage off] in
  let transformed_functions =
    List.map mir.functions_block ~f:(fun fs ->
        Program.{fs with fdbody= Option.map ~f:(transform (Some fs)) fs.fdbody})
  in
  { mir with
    functions_block= transformed_functions
  ; prepare_data= transform' None mir.prepare_data
  ; transform_inits= transform' None mir.transform_inits
  ; log_prob= transform' None mir.log_prob
  ; reverse_mode_log_prob= transform' None mir.reverse_mode_log_prob
  ; generate_quantities= transform' None mir.generate_quantities }

let map_no_loc l =
  List.map ~f:(fun s -> Stmt.{pattern= s; meta= Location_span.empty}) l

let slist_no_loc l = Stmt.Pattern.SList (map_no_loc l)
let block_no_loc l = Stmt.Pattern.Block (map_no_loc l)

let slist_concat_no_loc l stmt =
  match l with [] -> stmt | l -> slist_no_loc (l @ [stmt])

let gen_inline_var (name : string) (id_var : string) =
  Gensym.generate ~prefix:("inline_" ^ name ^ "_" ^ id_var ^ "_") ()

let replace_fresh_local_vars (fname : string) stmt =
  let f (m : string String.Map.t) = function
    | Stmt.Pattern.Decl {decl_adtype; decl_type; decl_id; initialize} ->
        let new_name =
          match String.Map.find_opt decl_id m with
          | Some existing -> existing
          | None -> gen_inline_var fname decl_id in
        ( Stmt.Pattern.Decl
            {decl_adtype; decl_id= new_name; decl_type; initialize}
        , String.Map.add m ~key:decl_id ~data:new_name )
    | Stmt.Pattern.For {loopvar; lower; upper; body} ->
        let new_name =
          match String.Map.find_opt loopvar m with
          | Some existing -> existing
          | None -> gen_inline_var fname loopvar in
        ( Stmt.Pattern.For {loopvar= new_name; lower; upper; body}
        , String.Map.add m ~key:loopvar ~data:new_name )
    | Assignment (lhs, type_, e) ->
        let update_name var_name =
          match String.Map.find_opt var_name m with
          | None -> var_name
          | Some var_name' -> var_name' in
        let lhs' = Stmt.Helpers.map_lhs_variable ~f:update_name lhs in
        (Stmt.Pattern.Assignment (lhs', type_, e), m)
    | x -> (x, m) in
  let s, m = map_rec_state_stmt_loc f String.Map.empty stmt in
  name_subst_stmt m s

let subst_args_stmt args es =
  let m = String.Map.of_list (List.combine args es) in
  subst_stmt m

(** Count the number of returns that happen in a statement *)
let rec count_returns Stmt.{pattern; _} : int =
  Stmt.Pattern.fold Fun.const
    (fun acc -> function
      | Stmt.{pattern= Return _; _} -> acc + 1
      | stmt -> acc + count_returns stmt)
    0 pattern

(* The strategy here is to wrap the function body in a dummy loop, then replace
   returns with breaks. One issue is early return from internal loops - in those
   cases, a break would only break out of the inner loop. The solution is a flag
   variable to indicate whether a 'return' break has been called, and then to
   check if that flag is set after each loop. Then, if a 'return' break is
   called from an inner loop, there's a cascade of breaks all the way out of the
   dummy loop. *)
let handle_early_returns (fname : string) opt_var stmt =
  let returned = gen_inline_var fname "early_ret_check" in
  let generate_inner_breaks num_returns stmt_pattern =
    match stmt_pattern with
    | Stmt.Pattern.Return opt_ret -> (
        match (opt_var, opt_ret) with
        | None, None when num_returns > 1 -> Stmt.Pattern.Break
        | None, None -> Stmt.Pattern.Block []
        | Some name, Some e when num_returns > 1 ->
            SList
              [ Stmt.
                  { pattern=
                      Assignment
                        ( Stmt.Helpers.lvariable returned
                        , UInt
                        , Expr.
                            { pattern= Lit (Int, "1")
                            ; meta=
                                Expr.Typed.Meta.
                                  { type_= UInt
                                  ; adlevel= DataOnly
                                  ; loc= Location_span.empty } } )
                  ; meta= Location_span.empty }
              ; Stmt.
                  { pattern=
                      Assignment
                        (Stmt.Helpers.lvariable name, Expr.Typed.type_of e, e)
                  ; meta= Location_span.empty }
              ; {pattern= Break; meta= Location_span.empty} ]
        | Some name, Some e ->
            Assignment (Stmt.Helpers.lvariable name, Expr.Typed.type_of e, e)
        | Some _, None ->
            ICE.internal_error
              "Function should return a value but found an empty return \
               statement." [@coverage off]
        | None, Some _ ->
            ICE.internal_error
              "Expected a void function but found a non-empty return statement."
            [@coverage off])
    | Stmt.Pattern.For _ as loop when num_returns > 1 ->
        Stmt.Pattern.SList
          [ Stmt.{pattern= loop; meta= Location_span.empty}
          ; Stmt.
              { pattern=
                  IfElse
                    ( Expr.
                        { pattern= Var returned
                        ; meta=
                            Expr.Typed.Meta.
                              { type_= UInt
                              ; adlevel= DataOnly
                              ; loc= Location_span.empty } }
                    , {pattern= Break; meta= Location_span.empty}
                    , None )
              ; meta= Location_span.empty } ]
    | x -> x in
  let num_returns = count_returns stmt in
  if num_returns > 1 then
    Stmt.Pattern.SList
      [ Stmt.
          { pattern=
              Decl
                { decl_adtype= DataOnly
                ; decl_id= returned
                ; decl_type= Sized SInt
                ; initialize= Default }
          ; meta= Location_span.empty }
      ; Stmt.
          { pattern=
              Assignment
                ( Stmt.Helpers.lvariable returned
                , UInt
                , Expr.
                    { pattern= Lit (Int, "0")
                    ; meta=
                        Expr.Typed.Meta.
                          { type_= UInt
                          ; adlevel= DataOnly
                          ; loc= Location_span.empty } } )
          ; meta= Location_span.empty }
      ; Stmt.
          { pattern=
              Stmt.Pattern.For
                { loopvar= gen_inline_var fname "iterator"
                ; lower=
                    Expr.
                      { pattern= Lit (Int, "1")
                      ; meta=
                          Expr.Typed.Meta.
                            { type_= UInt
                            ; adlevel= DataOnly
                            ; loc= Location_span.empty } }
                ; upper=
                    { pattern= Lit (Int, "1")
                    ; meta=
                        { type_= UInt
                        ; adlevel= DataOnly
                        ; loc= Location_span.empty } }
                ; body=
                    map_rec_stmt_loc (generate_inner_breaks num_returns) stmt }
          ; meta= Location_span.empty } ]
  else (map_rec_stmt_loc (generate_inner_breaks num_returns) stmt).pattern

let inline_list f es =
  let dse_list = List.map ~f es in
  (* function arguments are evaluated from right to left in C++, so we need to
     reverse *)
  let d_list =
    List.concat (List.rev (List.map ~f:(function x, _, _ -> x) dse_list)) in
  let s_list =
    List.concat (List.rev (List.map ~f:(function _, x, _ -> x) dse_list)) in
  let es = List.map ~f:(function _, _, x -> x) dse_list in
  (d_list, s_list, es)

let compute_suffix_and_name propto suffix fname =
  let open Fun_kind in
  match suffix with
  | FnLpdf propto' when propto' && propto ->
      ( FnLpdf true
      , with_unnormalized_suffix fname |> Option.value ~default:fname )
  | FnLpdf _ -> (FnLpdf false, fname)
  | FnLpmf propto' when propto' && propto ->
      ( FnLpmf true
      , with_unnormalized_suffix fname |> Option.value ~default:fname )
  | FnLpmf _ -> (FnLpmf false, fname)
  | _ -> (suffix, fname)

(* Triple is (declaration list, statement list, return expression) *)
let rec inline_function_expression propto adt fim (Expr.{pattern; _} as e) =
  match pattern with
  | Var _ -> ([], [], e)
  | Lit (_, _) -> ([], [], e)
  | Promotion (expr, ut, ad) ->
      let d, sl, expr' = inline_function_expression propto adt fim expr in
      (d, sl, {e with pattern= Promotion (expr', ut, ad)})
  | FunApp (kind, es) -> (
      let d_list, s_list, es =
        inline_list (inline_function_expression propto adt fim) es in
      match kind with
      | CompilerInternal _ ->
          (d_list, s_list, {e with pattern= FunApp (kind, es)})
      | StanLib (fname, suffix, mem) ->
          let suffix, _ = compute_suffix_and_name propto suffix fname in
          ( d_list
          , s_list
          , {e with pattern= FunApp (Fun_kind.StanLib (fname, suffix, mem), es)}
          )
      | UserDefined (fname, suffix) -> (
          let suffix, fname' = compute_suffix_and_name propto suffix fname in
          match String.Map.find_opt fname' fim with
          | None ->
              ( d_list
              , s_list
              , { e with
                  pattern= FunApp (Fun_kind.UserDefined (fname, suffix), es) }
              )
          | Some (rt, args, body) ->
              let inline_return_name = gen_inline_var fname "return" in
              let handle =
                handle_early_returns fname (Some inline_return_name) in
              let d_list2, s_list2, (e : Expr.Typed.t) =
                let decl_type =
                  Option.map ~f:unsafe_unsized_to_sized_type rt
                  |> Option.map ~f:(fun s -> Type.Sized s)
                  |> Option.get in
                ( [ Stmt.Pattern.Decl
                      { decl_adtype=
                          UnsizedType.fill_adtype_for_type adt
                            (Type.to_unsized decl_type)
                      ; decl_id= inline_return_name
                      ; decl_type
                      ; initialize= Uninit } ]
                  (* We should minimize the code that's having its variables
                     replaced to avoid conflict with the (two) new dummy
                     variables introduced by inlining *)
                , [ handle
                      (subst_args_stmt args es
                         (replace_fresh_local_vars fname body)) ]
                , { pattern= Var inline_return_name
                  ; meta=
                      Expr.Typed.Meta.
                        { type_= Type.to_unsized decl_type
                        ; adlevel= adt
                        ; loc= Location_span.empty } } ) in
              let d_list = d_list @ d_list2 in
              let s_list = s_list @ s_list2 in
              (d_list, s_list, e)))
  | TernaryIf (e1, e2, e3) ->
      let dl1, sl1, e1 = inline_function_expression propto adt fim e1 in
      let dl2, sl2, e2 = inline_function_expression propto adt fim e2 in
      let dl3, sl3, e3 = inline_function_expression propto adt fim e3 in
      ( dl1 @ dl2 @ dl3
      , sl1
        @ [ Stmt.(
              Pattern.IfElse
                ( e1
                , {pattern= block_no_loc sl2; meta= Location_span.empty}
                , Some {pattern= block_no_loc sl3; meta= Location_span.empty} ))
          ]
      , {e with pattern= TernaryIf (e1, e2, e3)} )
  | Indexed (e', i_list) ->
      let dl, sl, e' = inline_function_expression propto adt fim e' in
      let d_list, s_list, i_list =
        inline_list (inline_function_index propto adt fim) i_list in
      (d_list @ dl, s_list @ sl, {e with pattern= Indexed (e', i_list)})
  | TupleProjection (e', ix) ->
      let dl, sl, e' = inline_function_expression propto adt fim e' in
      (dl, sl, {e with pattern= TupleProjection (e', ix)})
  | EAnd (e1, e2) ->
      let dl1, sl1, e1 = inline_function_expression propto adt fim e1 in
      let dl2, sl2, e2 = inline_function_expression propto adt fim e2 in
      let sl2 =
        [ Stmt.(
            Pattern.IfElse
              ( e1
              , {pattern= Block (map_no_loc sl2); meta= Location_span.empty}
              , None )) ] in
      (dl1 @ dl2, sl1 @ sl2, {e with pattern= EAnd (e1, e2)})
  | EOr (e1, e2) ->
      let dl1, sl1, e1 = inline_function_expression propto adt fim e1 in
      let dl2, sl2, e2 = inline_function_expression propto adt fim e2 in
      let sl2 =
        [ Stmt.(
            Pattern.IfElse
              ( e1
              , {pattern= Skip; meta= Location_span.empty}
              , Some {pattern= Block (map_no_loc sl2); meta= Location_span.empty}
              )) ] in
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

let rec inline_function_statement propto adt fim Stmt.{pattern; meta} =
  Stmt.
    { pattern=
        (match pattern with
        | Assignment (lhs, ut, e2) ->
            let e1 = Stmt.Helpers.expr_of_lvalue lhs ~meta:e2.meta in
            (* This inner e2 is wrong. We are giving the wrong type to Var x.
               But it doesn't really matter as we discard it later. *)
            let dl1, sl1, e1 = inline_function_expression propto adt fim e1 in
            let dl2, sl2, e2 = inline_function_expression propto adt fim e2 in
            let lhs' =
              match Stmt.Helpers.lvalue_of_expr_opt e1 with
              | Some x -> x
              | None ->
                  ICE.internal_error
                    "Internal error in inline optimization: lhs could not be \
                     converted round-trip to expression" [@coverage off] in
            slist_concat_no_loc
              (dl2 @ dl1 @ sl2 @ sl1)
              (Assignment (lhs', ut, e2))
        | TargetPE e ->
            let d, s, e = inline_function_expression propto adt fim e in
            slist_concat_no_loc (d @ s) (TargetPE e)
        | JacobianPE e ->
            let d, s, e = inline_function_expression propto adt fim e in
            slist_concat_no_loc (d @ s) (JacobianPE e)
        | NRFunApp (kind, exprs) ->
            let d_list, s_list, es =
              inline_list (inline_function_expression propto adt fim) exprs
            in
            slist_concat_no_loc (d_list @ s_list)
              (match kind with
              | CompilerInternal _ | StanLib _ -> NRFunApp (kind, es)
              | UserDefined (s, _) -> (
                  match String.Map.find_opt s fim with
                  | None -> NRFunApp (kind, es)
                  | Some (_, args, b) ->
                      let b = replace_fresh_local_vars s b in
                      let b = handle_early_returns s None b in
                      (subst_args_stmt args es
                         {pattern= b; meta= Location_span.empty})
                        .pattern))
        | Return e -> (
            match e with
            | None -> Return None
            | Some expr ->
                let d, s, e = inline_function_expression propto adt fim expr in
                slist_concat_no_loc (d @ s) (Return (Some e)))
        | IfElse (expr, s1, s2) ->
            let d, s, e = inline_function_expression propto adt fim expr in
            slist_concat_no_loc (d @ s)
              (IfElse
                 ( e
                 , inline_function_statement propto adt fim s1
                 , Option.map ~f:(inline_function_statement propto adt fim) s2
                 ))
        | While (expr, stmt) ->
            let d', s', e = inline_function_expression propto adt fim expr in
            slist_concat_no_loc (d' @ s')
              (While
                 ( e
                 , if List.is_empty s' then
                     inline_function_statement propto adt fim stmt
                   else
                     { pattern=
                         Block
                           ([inline_function_statement propto adt fim stmt]
                           @ map_no_loc s')
                     ; meta= Location_span.empty } ))
        | For {loopvar; lower; upper; body} ->
            let d_lower, s_lower, lower =
              inline_function_expression propto adt fim lower in
            let d_upper, s_upper, upper =
              inline_function_expression propto adt fim upper in
            slist_concat_no_loc
              (d_lower @ d_upper @ s_lower @ s_upper)
              (For
                 { loopvar
                 ; lower
                 ; upper
                 ; body=
                     (if List.is_empty s_upper then
                        inline_function_statement propto adt fim body
                      else
                        { pattern=
                            Block
                              ([inline_function_statement propto adt fim body]
                              @ map_no_loc s_upper)
                        ; meta= Location_span.empty }) })
        | Profile (name, l) ->
            Profile
              (name, List.map l ~f:(inline_function_statement propto adt fim))
        | Block l ->
            Block (List.map l ~f:(inline_function_statement propto adt fim))
        | SList l ->
            SList (List.map l ~f:(inline_function_statement propto adt fim))
        | Decl {decl_adtype; decl_id; decl_type; initialize= Assign expr} ->
            let d, s, e = inline_function_expression propto adt fim expr in
            slist_concat_no_loc (d @ s)
              (Decl {decl_adtype; decl_id; decl_type; initialize= Assign e})
        | Decl r -> Decl r
        | Skip -> Skip
        | Break -> Break
        | Continue -> Continue)
    ; meta }

let create_function_inline_map adt l =
  let f accum Program.{fdname; fdargs; fdbody; fdrt; _} =
    match fdbody with
    | None -> accum
    | Some fdbody -> (
        let create_data propto =
          ( Option.map
              ~f:(fun x -> Type.Unsized x)
              (UnsizedType.returntype_to_type_opt fdrt)
          , List.map ~f:(fun (_, name, _) -> name) fdargs
          , inline_function_statement propto adt accum fdbody ) in
        match Fun_kind.with_unnormalized_suffix fdname with
        | None ->
            let data = create_data true in
            if String.Map.mem fdname accum then accum
            else String.Map.add accum ~key:fdname ~data
        | Some fdname' ->
            let data = create_data false in
            let data' = create_data true in
            let m = String.Map.of_list [(fdname, data); (fdname', data')] in
            String.Map.union accum m ~f:(fun _ v1 _ -> Some v1)) in
  List.fold_left l ~init:String.Map.empty ~f

let function_inlining (mir : Program.Typed.t) =
  (* We add only the functions with a single definition to the inline map.
     Overloaded functions cannot be inlined. *)
  let can_inline =
    List.fold_left mir.functions_block ~init:String.Map.empty
      ~f:(fun accum Program.{fdname; _} ->
        String.Map.update accum ~key:fdname ~f:(fun o ->
            Some (Option.value_map o ~default:true ~f:(fun _ -> false)))) in
  let inlineable_functions =
    List.filter mir.functions_block ~f:(fun Program.{fdname; _} ->
        String.Map.find fdname can_inline) in
  let dataonly_inline_map =
    create_function_inline_map UnsizedType.DataOnly inlineable_functions in
  let autodiff_inline_map =
    create_function_inline_map UnsizedType.AutoDiffable inlineable_functions
  in
  let dataonly_inline_function_statements =
    List.map
      ~f:
        (inline_function_statement true UnsizedType.DataOnly dataonly_inline_map)
  in
  let autodiffable_inline_function_statements =
    List.map
      ~f:
        (inline_function_statement true UnsizedType.AutoDiffable
           autodiff_inline_map) in
  { mir with
    transform_inits= autodiffable_inline_function_statements mir.transform_inits
  ; unconstrain_array=
      autodiffable_inline_function_statements mir.unconstrain_array
  ; log_prob= autodiffable_inline_function_statements mir.log_prob
  ; reverse_mode_log_prob=
      autodiffable_inline_function_statements mir.reverse_mode_log_prob
  ; generate_quantities=
      dataonly_inline_function_statements mir.generate_quantities }

let rec contains_top_break_or_continue Stmt.{pattern; _} =
  match pattern with
  | Break | Continue -> true
  | Assignment (_, _, _)
   |TargetPE _ | JacobianPE _
   |NRFunApp (_, _)
   |Return _ | Decl _
   |While (_, _)
   |For _ | Skip ->
      false
  | Profile (_, l) | Block l | SList l ->
      List.exists l ~f:contains_top_break_or_continue
  | IfElse (_, b1, b2) -> (
      contains_top_break_or_continue b1
      ||
      match b2 with
      | None -> false
      | Some b -> contains_top_break_or_continue b)

let unroll_static_limit = 32

let unroll_static_loops_statement _ =
  let f stmt =
    match stmt with
    | Stmt.Pattern.For {loopvar; lower; upper; body} -> (
        let lower = Partial_evaluator.try_eval_expr lower in
        let upper = Partial_evaluator.try_eval_expr upper in
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
                    Expr.
                      { pattern= Lit (Int, Int.to_string i)
                      ; meta=
                          Expr.Typed.Meta.
                            { type_= UInt
                            ; loc= Location_span.empty
                            ; adlevel= DataOnly } })
                  (List.range low (up + 1)) in
              let stmts =
                List.map
                  ~f:(fun i ->
                    subst_args_stmt [loopvar] [i]
                      {pattern= body.pattern; meta= Location_span.empty})
                  range in
              Stmt.Pattern.SList stmts
        | _ -> stmt)
    | _ -> stmt in
  top_down_map_rec_stmt_loc f

let static_loop_unrolling mir =
  transform_program_blockwise mir unroll_static_loops_statement

let unroll_loop_one_step_statement _ =
  let f stmt : (_, Stmt.Located.t) Stmt.Pattern.t =
    match stmt with
    | Stmt.Pattern.For {loopvar; lower; upper; body}
      when not (contains_top_break_or_continue body) -> (
        let lower, lower_decl =
          if cannot_duplicate_expr lower then
            let lower_name = "_" ^ loopvar ^ "_lower" in
            ( Expr.Helpers.variable lower_name
            , [ { Stmt.pattern=
                    Decl
                      { decl_adtype= DataOnly
                      ; decl_id= lower_name
                      ; decl_type= Sized SInt
                      ; initialize= Assign lower }
                ; meta= lower.meta.loc } ] )
          else (lower, []) in
        let unrolled =
          Stmt.Pattern.IfElse
            ( Expr.
                { lower with
                  pattern=
                    FunApp (StanLib ("Geq__", FnPlain, AoS), [upper; lower]) }
            , { Stmt.pattern=
                  (let body_unrolled =
                     subst_args_stmt [loopvar] [lower]
                       {pattern= body.pattern; meta= Location_span.empty} in
                   let (body' : Stmt.Located.t) =
                     { pattern=
                         Stmt.Pattern.For
                           { loopvar
                           ; upper
                           ; body
                           ; lower=
                               { lower with
                                 pattern=
                                   FunApp
                                     ( StanLib ("Plus__", FnPlain, AoS)
                                     , [lower; Expr.Helpers.loop_bottom] ) } }
                     ; meta= Location_span.empty } in
                   match body_unrolled.pattern with
                   | Block stmts -> Block (stmts @ [body'])
                   | _ -> Stmt.Pattern.Block [body_unrolled; body'])
              ; meta= Location_span.empty }
            , None ) in
        match lower_decl with
        | [] -> unrolled
        | decls ->
            Block (decls @ [{pattern= unrolled; meta= Location_span.empty}]))
    | While (e, body) when not (contains_top_break_or_continue body) ->
        IfElse
          ( e
          , { pattern= Block [body; {body with pattern= While (e, body)}]
            ; meta= Location_span.empty }
          , None )
    | _ -> stmt in
  map_rec_stmt_loc f

let one_step_loop_unrolling mir =
  transform_program_blockwise mir unroll_loop_one_step_statement

let can_duplicate_expr e = not (cannot_duplicate_expr e)

(** [*], [/] and [^] have elementwise variants that accept containers. *)
let elementwise_operator = function
  | Operator.Times -> Some Operator.EltTimes
  | Divide -> Some EltDivide
  | Pow -> Some EltPow
  | Plus | PPlus | Minus | PMinus | IntDivide | Modulo | LDivide | EltTimes
   |EltDivide | EltPow | Or | And | Equals | NEquals | Less | Leq | Greater
   |Geq | PNot | Transpose ->
      None

(** What the outer loop needs to know about its (already vectorized) body:
    whether a [break] or [continue] leaves this loop, which leaves the loop
    alone. ([target()] reads are handled by the dependence graph: they make the
    [target +=] increments ordinary ordered writes.) *)
type conflicts = {breaks: bool}

(* ---- Debug report for --debug-loop-vectorization (§7.13) ---- *)

type hoist_outcome =
  | Hoisted
  | Recurrence of Dataflow_types.loop_edge
  | In_cycle of int list
  | Effectful
  | Not_widened of string
  | Loop_bail of string

type loop_report =
  { loc: Location_span.t
  ; loopvar: string
  ; lower: Expr.Typed.t
  ; upper: Expr.Typed.t
  ; graph: Dataflow_types.loop_dependence_graph
  ; blocks: int list list
  ; outcomes: (int * hoist_outcome) list }

let loop_report_log : loop_report list ref = ref []
let loop_reports () = List.rev !loop_report_log

let pp_positions ppf ps =
  Fmt.(list ~sep:(any " ") (fun ppf v -> Fmt.pf ppf "S%d" v)) ppf ps

let pp_hoist_outcome ppf = function
  | Hoisted -> Fmt.string ppf "hoisted"
  | Recurrence e ->
      Fmt.pf ppf "sequential: recurrence, %a" Loop_dependence.pp_loop_edge e
  | In_cycle others ->
      Fmt.pf ppf "sequential: in a dependence cycle with %a" pp_positions others
  | Effectful ->
      Fmt.string ppf
        "sequential: has effects (print, reject or a user-defined function \
         call)"
  | Not_widened reason -> Fmt.pf ppf "sequential: %s" reason
  | Loop_bail reason -> Fmt.pf ppf "loop left alone: %s" reason

(** One loop of the report: the header, one line per leaf statement with its
    outcome, the edges and the pi-blocks in emission order. *)
let pp_loop_report ppf {loc; loopvar; lower; upper; graph; blocks; outcomes} =
  Fmt.pf ppf "loop at %a  (%s in %a:%a)@."
    (Location_span.pp ?printed_filename:None)
    loc loopvar Expr.Typed.pp lower Expr.Typed.pp upper;
  Array.iter graph.nodes ~f:(fun (node : Dataflow_types.loop_node) ->
      let outcome =
        List.find_map outcomes ~f:(fun (p, o) ->
            if p = node.pos then Some o else None) in
      Fmt.pf ppf "  S%d  %a   %a@." node.pos Loop_dependence.pp_stmt_one_line
        node.stmt
        (Fmt.option pp_hoist_outcome)
        outcome);
  Fmt.pf ppf "  %a@.  %a@." Loop_dependence.pp_edges graph
    (Loop_dependence.pp_blocks graph)
    blocks

(* ---- Widening ---- *)

type 'a vector_state =
  | Scalar of 'a (* contains only loop-invariant scalars *)
  | Widened of 'a (* something has been vectorized *)
  | Refused of string (* why it cannot be widened *)
[@@deriving map]

(** [bound + offset] for a loop bound and a symbolic [linear] offset, built with
    [Expr.Helpers.binop] and constant-folded later by partial evaluation;
    [bound] itself when the offset is zero. This is how [a[n + k]] over
    [n in 1:N] becomes [a[(1 + k):(N + k)]]. *)
let shift_bound (bound : Expr.Typed.t) ({const; terms} : Dataflow_types.linear)
    =
  let open Expr.Helpers in
  let add acc c (e : Expr.Typed.t) =
    let scaled = match abs c with 1 -> e | k -> binop (int k) Times e in
    if c > 0 then binop acc Plus scaled else binop acc Minus scaled in
  let acc =
    List.fold_left terms ~init:bound ~f:(fun acc (c, e) -> add acc c e) in
  let acc =
    if const = 0 then acc
    else if const > 0 then binop acc Plus (int const)
    else binop acc Minus (int (-const)) in
  {acc with meta= bound.meta}

let pp_arg_types ppf (args : Expr.Typed.t list) =
  Fmt.(list ~sep:(any ", ") UnsizedType.pp)
    ppf
    (List.map args ~f:(fun (a : Expr.Typed.t) -> a.meta.type_))

(** Rewrite one [For] by pi-block code generation (Allen and Kennedy 1987 §5.2;
    design §7.6). The loop is left alone when the body breaks, reads [target()],
    or writes a variable of its bounds. Otherwise each pi-block of
    [loop_dependence_graph] is emitted in topological order: a singleton,
    acyclic, effect-free block whose statement widens becomes a vector
    statement; every other block stays a sequential loop, and adjacent
    sequential blocks fuse into one loop whose body is the original body
    restricted to their statements in lexical order (always legal: it is the
    original loop minus some statements, and every edge between the fused
    statements is preserved by that order). If nothing hoists, the original loop
    is returned unchanged. Every decision is recorded for the
    [--debug-loop-vectorization] report. *)
let vectorized_for (meta : Stmt.Located.Meta.t) (conflict_info : conflicts)
    loopvar lower upper (body : Stmt.Located.t) : Stmt.Located.t =
  let original = Stmt.{pattern= For {loopvar; lower; upper; body}; meta} in
  let graph = Loop_dependence.loop_dependence_graph ~loopvar body in
  let blocks = Loop_dependence.pi_blocks graph in
  let written = Stmt.Helpers.assigned_or_declared_variables body in
  let report outcomes =
    (* compiler-generated loops (data reads, parameter unpacking) carry no
       source location and are left out of the report *)
    if Stdlib.compare meta Location_span.empty <> 0 then
      loop_report_log :=
        {loc= meta; loopvar; lower; upper; graph; blocks; outcomes}
        :: !loop_report_log in
  let bail reason =
    report
      (List.init ~len:(Array.length graph.nodes) ~f:(fun p ->
           (p, Loop_bail reason)));
    original in
  let written_bounds =
    Set.Poly.inter written
      (Set.Poly.union (expr_var_names_set lower) (expr_var_names_set upper))
  in
  if conflict_info.breaks then bail "break or continue in the loop body"
  else if not (Set.Poly.is_empty written_bounds) then
    bail
      (Fmt.str "loop bound variable %s is written in the body"
         (String.concat ~sep:", " (Set.Poly.to_list written_bounds)))
  else
    let is_loop_invariant_expr e =
      not (Set.Poly.mem loopvar (expr_var_names_set e)) in
    (* the loop variable appears outside an index position, e.g. [c[n] * n] *)
    let rec uses_loopvar_as_value (e : Expr.Typed.t) =
      match e.pattern with
      | Var v -> String.equal v loopvar
      | Lit _ -> false
      | Indexed (base, _) -> uses_loopvar_as_value base
      | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _
       |TupleProjection _ ->
          Expr.Pattern.fold
            (fun acc e -> acc || uses_loopvar_as_value e)
            false e.pattern in
    let rec widen (e : Expr.Typed.t) : Expr.Typed.t vector_state =
      match e.meta.type_ with
      | (UInt | UReal | UComplex) when is_loop_invariant_expr e -> Scalar e
      | UInt | UReal -> widen_inner e
      | UComplex ->
          Refused
            (Fmt.str "complex expression %a varies with the loop" Expr.Typed.pp
               e)
      | UVector | URowVector | UMatrix | UComplexVector | UComplexRowVector
       |UComplexMatrix | UArray _ | UTuple _ | UFun _ | UMathLibraryFunction ->
          if is_loop_invariant_expr e then
            Refused (Fmt.str "%a is a loop-invariant container" Expr.Typed.pp e)
          else
            Refused
              (Fmt.str "%a is a container that varies with the loop"
                 Expr.Typed.pp e)
    and widen_inner (e : Expr.Typed.t) =
      match e.pattern with
      | Var v when String.equal v loopvar ->
          Refused (Fmt.str "loop variable %s is used as a value" v)
      | Var _ | Lit _ -> Scalar e
      | Indexed (base, idcs) when is_loop_invariant_expr base -> (
          match widen_indices idcs with
          | Refused r -> Refused r
          | Scalar _ ->
              Refused
                (Fmt.str "no index of %a varies with the loop" Expr.Typed.pp e)
          | Widened idcs' ->
              let type_ =
                Expr.Helpers.infer_type_of_indexed base.meta.type_ idcs' in
              Widened
                {Expr.pattern= Indexed (base, idcs'); meta= {e.meta with type_}}
          )
      | Indexed (base, _) ->
          Refused
            (Fmt.str "indexed base %a varies with the loop" Expr.Typed.pp base)
      | FunApp (StanLib (name, FnPlain, mem), args) -> (
          match widen_all args with
          | Refused r -> Refused r
          | Scalar args' | Widened args' -> (
              let container_call name =
                match Partial_evaluator.stan_math_return_type name args' with
                | Some
                    (ReturnType
                       ((UVector | URowVector | UArray (UInt | UReal)) as type_))
                  ->
                    Some
                      (Widened
                         Expr.
                           { pattern=
                               FunApp (StanLib (name, FnPlain, mem), args')
                           ; meta= {e.meta with type_} })
                | Some _ | None -> None in
              let elementwise =
                Option.bind
                  (Operator.of_string_opt name)
                  ~f:elementwise_operator
                |> Option.map ~f:Operator.to_string in
              match
                Option.first_some (container_call name)
                  (Option.bind elementwise ~f:container_call)
              with
              | Some widened -> widened
              | None ->
                  Refused
                    (Fmt.str "no Stan Math signature for %s(%a)" name
                       pp_arg_types args')))
      | FunApp
          ( ( StanLib
                (name, (FnRng | FnLpdf _ | FnLpmf _ | FnTarget | FnJacobian), _)
            | UserDefined (name, _) )
          , _ ) ->
          Refused (Fmt.str "%s is not a plain Stan Math function" name)
      | FunApp (CompilerInternal _, _) ->
          Refused (Fmt.str "%a is a compiler-internal call" Expr.Typed.pp e)
      | (TernaryIf _ | EAnd _ | EOr _ | Promotion _ | TupleProjection _)
        when uses_loopvar_as_value e ->
          Refused (Fmt.str "loop variable %s is used as a value" loopvar)
      | TernaryIf _ | EAnd _ | EOr _ | Promotion _ | TupleProjection _ ->
          Refused (Fmt.str "%a cannot be widened" Expr.Typed.pp e)
    and widen_all es =
      (* functions allow widening any and all inputs*)
      List.map ~f:widen es
      |> List.fold_left ~init:(Scalar []) ~f:(fun acc e ->
          match (acc, e) with
          | Refused r, _ | _, Refused r -> Refused r
          | Scalar ls, Scalar e -> Scalar (e :: ls)
          | (Scalar ls | Widened ls), (Scalar e | Widened e) -> Widened (e :: ls))
      |> map_vector_state List.rev
    and widen_indices idxs =
      (* exactly one index varies with the loop; all others are invariant
         scalars *)
      List.fold_left idxs ~init:(Scalar []) ~f:(fun acc i ->
          match (acc, i) with
          | Refused r, _ -> Refused r
          | Widened ls, Index.Single e when is_loop_invariant_expr e ->
              Widened (Index.Single e :: ls)
          | Widened _, Single e ->
              Refused
                (Fmt.str "more than one index varies with the loop (%a)"
                   Expr.Typed.pp e)
          | ( (Widened _ | Scalar _)
            , ((All | Upfrom _ | Between _ | MultiIndex _) as i) ) ->
              Refused
                (Fmt.str "index %a is already a slice" (Index.pp Expr.Typed.pp)
                   i)
          | Scalar ls, Single e -> (
              match
                Loop_dependence.classify_subscript ~loopvar ~written (Single e)
              with
              | Affine {coeff= 1; offset} ->
                  Widened
                    (Between (shift_bound lower offset, shift_bound upper offset)
                    :: ls)
              | Affine {coeff; _} ->
                  Refused
                    (Fmt.str "index %a has stride %d" Expr.Typed.pp e coeff)
              | Invariant _ -> Scalar (Single e :: ls)
              | Varying Written ->
                  Refused
                    (Fmt.str "index %a mentions a variable written in the loop"
                       Expr.Typed.pp e)
              | Varying (Gather | Nonlinear | Slice | Multi_index) -> (
                  match widen e with
                  | Widened e' -> Widened (MultiIndex e' :: ls)
                  | Scalar e -> Scalar (Single e :: ls)
                  | Refused r -> Refused r)))
      |> map_vector_state List.rev in
    let index_exprs idcs = List.concat_map idcs ~f:Index.bounds in
    (* One leaf statement as a vector statement, or the reason it is not. *)
    let rec widen_stmt (s : Stmt.Located.t) : (Stmt.Located.t, string) result =
      let swrap pattern = Ok Stmt.{pattern; meta= s.meta} in
      match s.pattern with
      | TargetPE
          ({ pattern=
               FunApp
                 (StanLib (name, ((FnLpdf _ | FnLpmf _) as suffix), mem), args)
           ; _ } as e) -> (
          if cannot_duplicate_expr e then
            Error "the density call has side effects or draws random numbers"
          else
            match widen_all args with
            | Refused r -> Error r
            | Scalar args' ->
                let funapp =
                  {e with pattern= FunApp (StanLib (name, suffix, mem), args')}
                in
                swrap
                  (TargetPE
                     Expr.Helpers.(
                       binop
                         (binop upper Minus (binop lower Minus one))
                         Times funapp))
            | Widened args'' -> (
                match Partial_evaluator.stan_math_return_type name args'' with
                | Some (ReturnType UReal) ->
                    swrap
                      (TargetPE
                         { e with
                           pattern= FunApp (StanLib (name, suffix, mem), args'')
                         })
                | Some _ | None ->
                    Error
                      (Fmt.str "density %s has no signature for (%a)" name
                         pp_arg_types args'')))
      | TargetPE {pattern= FunApp (UserDefined (name, _), _); _} ->
          Error
            (Fmt.str "user-defined density %s has no container signature" name)
      | TargetPE _ -> Error "target increment is not a density call"
      | Assignment ((LVariable var, idcs), vtype, value) -> (
          if
            cannot_duplicate_expr value
            || List.exists (index_exprs idcs) ~f:cannot_duplicate_expr
          then Error "the assignment has side effects or draws random numbers"
          else
            match widen_indices idcs with
            | Refused r -> Error r
            | Scalar _ ->
                Error
                  (Fmt.str
                     "no index of %s varies with the loop (the same element is \
                      assigned every iteration)"
                     var)
            | Widened idcs' -> (
                match widen value with
                | Refused r -> Error r
                | Scalar _ ->
                    Error
                      "right-hand side is loop-invariant (would need a \
                       broadcast)"
                | Widened e ->
                    let target_type =
                      Expr.Helpers.infer_type_of_indexed vtype idcs' in
                    if UnsizedType.equal e.meta.type_ target_type then
                      swrap (Assignment ((LVariable var, idcs'), vtype, e))
                    else
                      Error
                        (Fmt.str
                           "right-hand side widens to %a but the assigned \
                            slice is %a"
                           UnsizedType.pp e.meta.type_ UnsizedType.pp
                           target_type)))
      | Assignment ((LTupleProjection _, _), _, _) ->
          Error "assignment to a tuple projection"
      | Profile (name, stmts) -> (
          match widen_stmts stmts with
          | Ok stmts' -> swrap (Profile (name, stmts'))
          | Error r -> Error r)
      | Block stmts -> (
          match widen_stmts stmts with
          | Ok stmts' -> swrap (Block stmts')
          | Error r -> Error r)
      | SList stmts -> (
          match widen_stmts stmts with
          | Ok stmts' -> swrap (SList stmts')
          | Error r -> Error r)
      | NRFunApp (CompilerInternal (FnPrint | FnReject | FnFatalError), _) ->
          Error "print, reject or fatal_error statement"
      | NRFunApp _ -> Error "function call statement"
      | Decl {decl_id; _} ->
          Error (Fmt.str "declaration of %s inside the loop" decl_id)
      | IfElse _ -> Error "if statement (no if-conversion)"
      | While _ -> Error "while loop"
      | For _ -> Error "nested loop"
      | JacobianPE _ -> Error "jacobian increment"
      | Return _ -> Error "return statement"
      | Break | Continue -> Error "break or continue"
      | Skip -> Error "empty statement"
    and widen_stmts stmts =
      List.fold_left stmts ~init:(Ok []) ~f:(fun acc s ->
          match acc with
          | Error r -> Error r
          | Ok acc -> (
              match widen_stmt s with
              | Ok s' -> Ok (s' :: acc)
              | Error r -> Error r))
      |> Result.map ~f:List.rev in
    (* pi-block code generation *)
    let classify block =
      match block with
      | [p] when Loop_dependence.is_cyclic graph block -> (
          let self_edge =
            List.find_opt graph.edges ~f:(fun (e : Dataflow_types.loop_edge) ->
                e.src = p && e.dst = p) in
          match self_edge with
          | Some e -> (`Seq [p], [(p, Recurrence e)])
          | None -> (`Seq [p], [(p, In_cycle [])]))
      | [p] when graph.nodes.(p).effects -> (`Seq [p], [(p, Effectful)])
      | [p] -> (
          match widen_stmt graph.nodes.(p).stmt with
          | Ok s -> (`Vec (p, s), [(p, Hoisted)])
          | Error r -> (`Seq [p], [(p, Not_widened r)]))
      | members ->
          ( `Seq members
          , List.map members ~f:(fun p ->
                (p, In_cycle (List.filter members ~f:(fun q -> q <> p)))) )
    in
    let items, outcomes = List.map blocks ~f:classify |> List.split in
    report (List.concat outcomes);
    (* Typed fusion (Kennedy and Allen §6.2.5): a sequential block may move
       earlier past a vector block when no edge runs from that vector block into
       it, so that sequential blocks become adjacent and fuse into as few
       residual loops as possible. Swapping independent neighbours keeps every
       edge's source ahead of its sink. *)
    let nodes_of = function `Vec (p, _) -> [p] | `Seq ps -> ps in
    let independent_of vec seq =
      not
        (List.exists graph.edges ~f:(fun (e : Dataflow_types.loop_edge) ->
             List.exists (nodes_of vec) ~f:(fun v -> v = e.src)
             && List.exists (nodes_of seq) ~f:(fun q -> q = e.dst))) in
    let rec bubble acc = function
      | [] -> List.rev acc
      | (`Seq _ as q) :: rest ->
          (* [acc] is the output so far, most recent first: pop the vector
             blocks [q] may pass, place [q], put them back *)
          let rec skip vecs = function
            | (`Vec _ as v) :: acc' when independent_of v q ->
                skip (v :: vecs) acc'
            | acc' -> (vecs, acc') in
          let vecs, acc' = skip [] acc in
          bubble (List.rev_append vecs (q :: acc')) rest
      | (`Vec _ as v) :: rest -> bubble (v :: acc) rest in
    let rec fuse = function
      | `Seq a :: `Seq b :: rest -> fuse (`Seq (a @ b) :: rest)
      | x :: rest -> x :: fuse rest
      | [] -> [] in
    let items = fuse (bubble [] items) in
    if not (List.exists items ~f:(function `Vec _ -> true | `Seq _ -> false))
    then original
    else
      let emit = function
        | `Vec (_, s) -> s
        | `Seq ps ->
            let stmts =
              List.sort ps ~cmp:Int.compare
              |> List.map ~f:(fun p -> graph.nodes.(p).stmt) in
            let body' =
              match stmts with
              | [s] -> s
              | l -> Stmt.{pattern= Block l; meta= body.meta} in
            Stmt.{pattern= For {loopvar; lower; upper; body= body'}; meta} in
      match List.map items ~f:emit with
      | [s] -> s
      | stmts -> Stmt.{pattern= SList stmts; meta}

(** [break] or [continue] that leaves this loop level (not one nested in an
    inner loop). *)
let rec stmt_breaks (s : Stmt.Located.t) =
  match s.pattern with
  | Break | Continue -> true
  | For _ | While _ -> false
  | Assignment _ | TargetPE _ | JacobianPE _ | NRFunApp _ | Return _ | Skip
   |IfElse _ | Profile _ | Block _ | SList _ | Decl _ ->
      Stmt.Pattern.fold
        (fun acc _ -> acc)
        (fun acc s -> acc || stmt_breaks s)
        false s.pattern

(** Vectorize every loop, innermost first. *)
let rec vectorize_stmt : Stmt.Located.t -> Stmt.Located.t = function
  | {pattern= For {loopvar; lower; upper; body}; meta}
    when can_duplicate_expr lower && can_duplicate_expr upper ->
      let body = vectorize_stmt body in
      let conflict_info = {breaks= stmt_breaks body} in
      vectorized_for meta conflict_info loopvar lower upper body
  | {pattern; meta} ->
      {pattern= Stmt.Pattern.map Fun.id vectorize_stmt pattern; meta}

(* Rewrites e.g. [for (n in 1:N) target += normal_lpdf(y[n] | mu[n], sigma)] to
   [target += normal_lpdf(y[1:N] | mu[1:N], sigma)], and [for (n in 1:N) mu[n] =
   alpha + beta * x[n]] to [mu[1:N] = alpha + beta * x[1:N]]. Widening applies
   to all StanLib functions that re-typecheck at a container return type, with
   the operators [*] and [/] replaced by their elementwise variants [.*] and
   [./] if necessary. Statements of one loop body are hoisted or kept sequential
   per pi-block of the loop dependence graph (design §7.6). *)
let vectorize_loops mir =
  loop_report_log := [];
  Program.map Fun.id vectorize_stmt Fun.id mir

let collapse_lists_statement _ =
  let rec collapse_lists l =
    match l with
    | [] -> []
    | Stmt.{pattern= SList l'; _} :: rest -> l' @ collapse_lists rest
    | x :: rest -> x :: collapse_lists rest in
  let f = function
    | Stmt.Pattern.Block l -> Stmt.Pattern.Block (collapse_lists l)
    | SList l -> SList (collapse_lists l)
    | x -> x in
  map_rec_stmt_loc f

let list_collapsing (mir : Program.Typed.t) =
  transform_program_blockwise mir collapse_lists_statement

let propagation
    (propagation_transfer :
         Stmt.Located.Non_recursive.t LabelMap.t
      -> (module Monotone_framework_sigs.TRANSFER_FUNCTION
            with type labels = int
             and type properties = Expr.Typed.t String.Map.t option))
    (mir : Program.Typed.t) =
  let transform stmt =
    let flowgraph, flowgraph_to_mir =
      Monotone_framework.forward_flowgraph_of_stmt stmt in
    let (module Flowgraph) = flowgraph in
    let values =
      Monotone_framework.propagation_mfp mir
        (module Flowgraph)
        flowgraph_to_mir propagation_transfer in
    let propagate_stmt =
      map_rec_stmt_loc_num flowgraph_to_mir (fun i ->
          subst_stmt_base
            (Option.value ~default:String.Map.empty
               (LabelMap.find i values).entry)) in
    propagate_stmt (LabelMap.find 1 flowgraph_to_mir) in
  transform_program mir transform

let constant_propagation ?(preserve_stability = false) =
  propagation
    (Monotone_framework.constant_propagation_transfer ~preserve_stability)

let expression_propagation ?(preserve_stability = false) mir =
  propagation
    (Monotone_framework.expression_propagation_transfer ~preserve_stability
       (cannot_duplicate_expr ~preserve_stability))
    mir

let copy_propagation mir =
  let globals = Monotone_framework.globals mir in
  propagation (Monotone_framework.copy_propagation_transfer globals) mir

let is_skip_break_continue s =
  match s with Stmt.Pattern.Skip | Break | Continue -> true | _ -> false

(* TODO: could also implement partial dead code elimination *)
let dead_code_elimination (mir : Program.Typed.t) =
  (* TODO: think about whether we should treat function bodies as local scopes
     in the statement from the POV of a live variables analysis. (Obviously,
     this shouldn't be the case for the purposes of reaching definitions,
     constant propagation, expressions analyses. But I do think that's the right
     way to go about live variables. *)
  let transform s =
    let rev_flowgraph, flowgraph_to_mir =
      Monotone_framework.inverse_flowgraph_of_stmt s in
    let (module Rev_Flowgraph) = rev_flowgraph in
    let live_variables =
      Monotone_framework.live_variables_mfp mir
        (module Rev_Flowgraph)
        flowgraph_to_mir in
    let dead_code_elim_stmt_base i stmt =
      (* NOTE: entry in the reverse flowgraph, so exit in the forward
         flowgraph *)
      let live_variables_s =
        (LabelMap.find i live_variables).Monotone_framework_sigs.entry in
      match stmt with
      | Stmt.Pattern.Assignment (lhs, _, rhs) ->
          if
            Set.Poly.mem (Stmt.Helpers.lhs_variable lhs) live_variables_s
            || cannot_remove_expr rhs
            || List.exists
                 ~f:(idx_any cannot_remove_expr)
                 (Stmt.Helpers.lhs_indices lhs)
          then stmt
          else Skip
      (* NOTE: we never get rid of declarations as we might not be able to
         remove an assignment to a variable
            due to side effects. *)
      (* TODO: maybe we should revisit that. *)
      | Decl ({decl_id; initialize= Assign e; _} as decl) ->
          if Set.Poly.mem decl_id live_variables_s || cannot_remove_expr e then
            stmt
          else Decl {decl with initialize= Uninit}
      | Decl _ | TargetPE _ | JacobianPE _
       |NRFunApp (_, _)
       |Break | Continue | Return _ | Skip ->
          stmt
      | IfElse (e, b1, b2) -> (
          if
            (* TODO: check if e has side effects, like print, reject, then don't
               optimize? *)
            (not (cannot_remove_expr e))
            && b1.Stmt.pattern = Skip
            && (Option.map ~f:(fun Stmt.{pattern; _} -> pattern) b2 = Some Skip
               || Option.map ~f:(fun Stmt.{pattern; _} -> pattern) b2 = None)
          then Skip
          else
            match e.pattern with
            | _ when Partial_evaluator.is_int 0 e -> (
                match b2 with Some x -> x.pattern | None -> Skip)
            | Lit (_, _) -> b1.pattern
            | _ -> IfElse (e, b1, b2))
      | While (e, b) -> (
          if (not (cannot_remove_expr e)) && b.pattern = Break then Skip
          else
            match e.pattern with
            | _ when Partial_evaluator.is_int 0 e -> Skip
            | _ -> While (e, b))
      | For {loopvar; lower; upper; body} ->
          if
            (not (cannot_remove_expr lower))
            && (not (cannot_remove_expr upper))
            && is_skip_break_continue body.pattern
          then Skip
          else For {loopvar; lower; upper; body}
      | Profile (name, l) ->
          let l' = List.filter ~f:(fun x -> x.Stmt.pattern <> Skip) l in
          if List.is_empty l' then Skip else Profile (name, l')
      | Block l ->
          let l' = List.filter ~f:(fun x -> x.Stmt.pattern <> Skip) l in
          if List.is_empty l' then Skip else Block l'
      | SList l ->
          let l' = List.filter ~f:(fun x -> x.Stmt.pattern <> Skip) l in
          SList l' in
    let dead_code_elim_stmt =
      map_rec_stmt_loc_num flowgraph_to_mir dead_code_elim_stmt_base in
    dead_code_elim_stmt (LabelMap.find 1 flowgraph_to_mir) in
  transform_program mir transform

let partial_evaluation p =
  transform_program p Partial_evaluator.eval_stmt
  |> Program.map Partial_evaluator.try_eval_expr Fun.id Fun.id

(** Given a name and Stmt, search the statement for the first assignment where
    that name is the assignee. *)
let rec find_assignment_idx (name : string) Stmt.{pattern; _} =
  match pattern with
  | Stmt.Pattern.Assignment (lval, lhs_ut, (rhs : 'a Expr.t)) ->
      let assign_name = Stmt.Helpers.lhs_variable lval in
      let idx_lst = Stmt.Helpers.lhs_indices lval in
      if
        name = assign_name
        && (not (Set.Poly.mem assign_name (expr_var_names_set rhs)))
        && not
             (rhs.meta.adlevel = UnsizedType.DataOnly
             && UnsizedType.is_array lhs_ut)
      then Some idx_lst
      else None
  | _ -> None

(** Given a list of Stmts, find Decls whose objects are fully assigned to in
    their first assignment and mark them as not needing to be initialized. *)
and unenforce_initialize (lst : Stmt.Located.t list) =
  let rec unenforce_initialize_patt (Stmt.{pattern; _} as stmt) sub_lst =
    match pattern with
    | Stmt.Pattern.Decl ({decl_id; initialize= Default; _} as decl_pat) -> (
        match List.hd sub_lst with
        | Some next_stmt -> (
            match find_assignment_idx decl_id next_stmt with
            | Some idxs when Index.every_index_is_all idxs ->
                { stmt with
                  pattern= Stmt.Pattern.Decl {decl_pat with initialize= Uninit}
                }
            | None | Some _ -> stmt)
        | None -> stmt)
    | Block block_lst ->
        {stmt with pattern= Block (unenforce_initialize block_lst)}
    | SList s_lst -> {stmt with pattern= SList (unenforce_initialize s_lst)}
    (*[] here because we do not want to check out of scope *)
    | While (expr, stmt) ->
        {stmt with pattern= While (expr, unenforce_initialize_patt stmt [])}
    | For ({body; _} as pat) ->
        { stmt with
          pattern= For {pat with body= unenforce_initialize_patt body []} }
    | Profile ((pname : string), stmts) ->
        {stmt with pattern= Profile (pname, unenforce_initialize stmts)}
    | IfElse ((expr : 'a Expr.t), true_stmt, op_false_stmt) ->
        let mod_false_stmt =
          Option.map ~f:(fun x -> unenforce_initialize_patt x []) op_false_stmt
        in
        { stmt with
          pattern=
            IfElse (expr, unenforce_initialize_patt true_stmt [], mod_false_stmt)
        }
    | _ -> stmt in
  match List.hd lst with
  | Some stmt -> (
      match List.tl lst with
      | Some sub_lst ->
          List.cons
            (unenforce_initialize_patt stmt sub_lst)
            (unenforce_initialize sub_lst)
      | None -> lst)
  | None -> lst

(** Take the Mir and perform a transform that requires searching across the list
    inside of each piece of the Mir.
    @param mir The mir
    @param transformer a function that takes in and returns a list of Stmts. *)
let transform_mir_blocks (mir : Program.Typed.t)
    (transformer : Stmt.Located.t list -> Stmt.Located.t list) : Program.Typed.t
    =
  let transformed_functions =
    List.map mir.functions_block ~f:(fun fs ->
        let new_body =
          match fs.Program.fdbody with
          | Some (Stmt.{pattern= SList lst; _} as stmt) ->
              Some {stmt with pattern= SList (transformer lst)}
          | Some (Stmt.{pattern= Block lst; _} as stmt) ->
              Some {stmt with pattern= Block (transformer lst)}
          | alt -> alt in
        {fs with fdbody= new_body}) in
  { Program.functions_block= transformed_functions
  ; input_vars= mir.input_vars
  ; prepare_data= transformer mir.prepare_data
  ; log_prob= transformer mir.log_prob
  ; reverse_mode_log_prob= transformer mir.reverse_mode_log_prob
  ; generate_quantities= transformer mir.generate_quantities
  ; transform_inits= transformer mir.transform_inits
  ; unconstrain_array= transformer mir.unconstrain_array
  ; output_vars= mir.output_vars
  ; prog_name= mir.prog_name
  ; prog_path= mir.prog_path }

let allow_uninitialized_decls mir =
  transform_mir_blocks mir unenforce_initialize

let lazy_code_motion ?(preserve_stability = false) (mir : Program.Typed.t) =
  (* TODO: clean up this code. It is not very pretty. *)
  (* TODO: make lazy code motion operate on transformed parameters and models blocks
     simultaneously *)
  let preprocess_flowgraph =
    let preprocess_flowgraph_base
        (stmt : (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t) =
      match stmt with
      | IfElse (e, b1, Some b2) ->
          Stmt.(
            Pattern.IfElse
              ( e
              , { pattern= Block [b1; {pattern= Skip; meta= Location_span.empty}]
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
      | _ -> stmt in
    map_rec_stmt_loc preprocess_flowgraph_base in
  let transform s =
    let rev_flowgraph, flowgraph_to_mir =
      Monotone_framework.inverse_flowgraph_of_stmt ~blocks_after_body:false s
    in
    let fwd_flowgraph = Monotone_framework.reverse rev_flowgraph in
    let latest_expr, used_not_latest_expressions_mfp =
      Monotone_framework.lazy_expressions_mfp fwd_flowgraph rev_flowgraph
        flowgraph_to_mir in
    let expression_map =
      let rec collect_expressions accum (e : Expr.Typed.t) =
        match e.pattern with
        | Lit (_, _) -> accum
        | Var _ -> accum
        | _ when cannot_duplicate_expr ~preserve_stability e ->
            (* Immovable expressions might have movable subexpressions *)
            Expr.Pattern.fold collect_expressions accum e.pattern
        | _ ->
            ExprMap.add accum ~key:e ~data:(Gensym.generate ~prefix:"lcm_" ())
      in
      ExprSet.fold
        (Monotone_framework.used_expressions_stmt s.pattern)
        ~init:ExprMap.empty
        ~f:(Fun.flip collect_expressions) in
    (* TODO: it'd be more efficient to just not accumulate constants in the
       static analysis *)
    let declarations_list =
      ExprMap.fold expression_map ~init:[] ~f:(fun ~key ~data accum ->
          Stmt.
            { pattern=
                Pattern.Decl
                  { decl_adtype= Expr.Typed.adlevel_of key
                  ; decl_id= data
                  ; decl_type= Type.Unsized (Expr.Typed.type_of key)
                  ; initialize= Default }
            ; meta= Location_span.empty }
          :: accum) in
    let lazy_code_motion_base i stmt =
      let latest_and_used_after_i =
        ExprSet.inter
          (LabelMap.find i latest_expr)
          (LabelMap.find i used_not_latest_expressions_mfp).entry in
      let to_assign_in_s =
        latest_and_used_after_i
        |> ExprSet.filter ~f:(fun x -> ExprMap.mem x expression_map)
        |> ExprSet.to_list
        |> List.sort ~cmp:(fun e e' ->
            Int.compare (expr_depth e) (expr_depth e')) in
      (* TODO: is this sort doing anything or are they already stored in the
         right order by chance? It appears to not do anything. *)
      let assignments_to_add_to_s =
        List.map
          ~f:(fun e ->
            Stmt.
              { pattern=
                  Assignment
                    ( Stmt.Helpers.lvariable (ExprMap.find e expression_map)
                    , e.meta.type_
                    , e )
              ; meta= Location_span.empty })
          to_assign_in_s in
      let expr_subst_stmt_except_initial_assign m =
        let f stmt =
          match stmt with
          | Stmt.Pattern.Assignment ((LVariable x, []), _, e')
           |Decl {decl_id= x; initialize= Assign e'; _}
            when ExprMap.mem e' m
                 && Expr.Typed.equal {e' with pattern= Var x}
                      (ExprMap.find e' m) ->
              expr_subst_stmt_base (ExprMap.remove e' m) stmt
          | _ -> expr_subst_stmt_base m stmt in
        map_rec_stmt_loc f in
      let expr_map =
        ExprMap.filter
          ~f:(fun key _ ->
            ExprSet.mem key latest_and_used_after_i
            || ExprSet.mem key
                 (LabelMap.find i used_not_latest_expressions_mfp).exit)
          (ExprMap.mapi expression_map ~f:(fun key data ->
               {key with pattern= Var data})) in
      let f = expr_subst_stmt_except_initial_assign expr_map in
      if List.is_empty assignments_to_add_to_s then
        (f Stmt.{pattern= stmt; meta= Location_span.empty}).pattern
      else
        SList
          (List.map ~f
             (assignments_to_add_to_s
             @ [{pattern= stmt; meta= Location_span.empty}])) in
    let lazy_code_motion_stmt =
      map_rec_stmt_loc_num flowgraph_to_mir lazy_code_motion_base in
    Stmt.
      { pattern=
          SList
            (declarations_list
            @ [lazy_code_motion_stmt (LabelMap.find 1 flowgraph_to_mir)])
      ; meta= Location_span.empty } in
  let cleanup =
    let cleanup_base (stmt : (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t) :
        (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t =
      match stmt with
      | Stmt.(
          Pattern.IfElse
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
      | _ -> stmt in
    map_rec_stmt_loc cleanup_base in
  transform_program_blockwise mir (fun _ x ->
      cleanup (transform (preprocess_flowgraph x)))

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
           | _ -> stmt))
        x)

(* TODO: implement SlicStan style optimizer for choosing best program block for each statement. *)
(* TODO: add optimization pass to move declarations down as much as possible and introduce as
   tight as possible local scopes *)
(* TODO: add tests *)
(* TODO: add pass to get rid of redundant declarations? *)

(** A generic optimization pass for finding a minimal set of variables that are
    generated by some circumstance, and then updating the MIR with that set.
    @param gen_variables:
      the variables that must be added to the set at the given statement
    @param update_expr: update an MIR expression given the variable set
    @param update_stmt:
      Function for updating an MIR statement given the variable set
    @param extra_variables:
      the set of variables that are implied to be in the set by a given variable
      in the set (usually empty, sometimes unrepresented variables like _in__
      variables)
    @param initial_variables: the initial known members of the set of variables
    @param stmt the MIR statement to optimize. *)
let optimize_minimal_variables
    ~(gen_variables :
          Stmt.Located.Non_recursive.t LabelMap.t
       -> int
       -> string Set.Poly.t
       -> string Set.Poly.t)
    ~(update_expr : string Set.Poly.t -> Expr.Typed.t -> Expr.Typed.t)
    ~(update_stmt :
          (Expr.Typed.t, (Expr.Typed.Meta.t, 'a) Stmt.t) Stmt.Pattern.t
       -> string Set.Poly.t
       -> (Expr.Typed.t, (Expr.Typed.Meta.t, 'a) Stmt.t) Stmt.Pattern.t)
    ~(extra_variables : string -> string Set.Poly.t)
    ~(initial_variables : string Set.Poly.t) (stmt : Stmt.Located.t) =
  let rev_flowgraph, flowgraph_to_mir =
    Monotone_framework.inverse_flowgraph_of_stmt stmt in
  let fwd_flowgraph = Monotone_framework.reverse rev_flowgraph in
  let (module Circular_Fwd_Flowgraph) =
    Monotone_framework.make_circular_flowgraph fwd_flowgraph rev_flowgraph in
  let mfp_variables =
    Monotone_framework.minimal_variables_mfp
      (module Circular_Fwd_Flowgraph)
      flowgraph_to_mir initial_variables gen_variables in
  let optimize_min_vars_stmt_base i stmt_pattern =
    let variable_set =
      let exits = (LabelMap.find i mfp_variables).exit in
      Set.Poly.union exits (Set.Poly.union_map exits ~f:extra_variables) in
    let stmt_val =
      Stmt.Pattern.map (update_expr variable_set) (fun x -> x) stmt_pattern
    in
    update_stmt stmt_val variable_set in
  map_rec_stmt_loc_num flowgraph_to_mir optimize_min_vars_stmt_base
    (LabelMap.find 1 flowgraph_to_mir)

(* XXX: This optimization current promotes/demotes entire tuples at once. This
   could be significantly better *)
let optimize_ad_levels (mir : Program.Typed.t) =
  let gen_ad_variables
      (flowgraph_to_mir : Stmt.Located.Non_recursive.t LabelMap.t) (l : int)
      (ad_variables : string Set.Poly.t) =
    let mir_node = (LabelMap.find l flowgraph_to_mir).pattern in
    match mir_node with
    | Assignment (lval, _, e)
      when UnsizedType.is_autodifftype
           @@ Expr.Typed.adlevel_of (update_expr_ad_levels ad_variables e) ->
        Set.Poly.singleton (Stmt.Helpers.lhs_variable lval)
    | _ -> Set.Poly.empty in
  let global_initial_ad_variables =
    Set.Poly.of_list
      (List.filter_map
         ~f:(fun (v, _, Program.{out_block; _}) ->
           match out_block with Parameters -> Some v | _ -> None)
         mir.output_vars) in
  let initial_ad_variables fundef_opt _ =
    match (fundef_opt : Stmt.Located.t Program.fun_def option) with
    | None -> global_initial_ad_variables
    | Some {fdargs; _} ->
        Set.Poly.union global_initial_ad_variables
          (Set.Poly.of_list
             (List.filter_map fdargs ~f:(fun (_, name, ut) ->
                  if UnsizedType.is_autodiffable ut then Some name else None)))
  in
  let extra_variables v = Set.Poly.singleton (v ^ "_in__") in
  let update_stmt stmt_pattern variable_set =
    match stmt_pattern with
    | Stmt.Pattern.Decl ({decl_id; decl_type; _} as decl)
      when Set.Poly.mem decl_id variable_set ->
        Stmt.Pattern.Decl
          { decl with
            decl_adtype=
              UnsizedType.fill_adtype_for_type UnsizedType.AutoDiffable
                (Type.to_unsized decl_type) }
    | Decl ({decl_id; decl_type; _} as decl)
      when not (Set.Poly.mem decl_id variable_set) ->
        let decl_type =
          match decl_type with
          | Sized st -> Type.Sized (SizedType.demote_sizedtype_mem st)
          | u -> u in
        Decl
          { decl with
            decl_type
          ; decl_adtype=
              UnsizedType.fill_adtype_for_type UnsizedType.DataOnly
                (Type.to_unsized decl_type) }
    | Assignment (lval, ty, ({Expr.pattern= Promotion (e, ut, ad); _} as prom))
      when (not (Set.Poly.mem (Stmt.Helpers.lhs_variable lval) variable_set))
           && UnsizedType.has_autodiff ad ->
        (* When a variable has been downcast, we need to remove any promotions
           it was going to recieve or else C++ compilation will fail *)
        Assignment
          ( lval
          , ty
          , { prom with
              pattern=
                Promotion (e, ut, UnsizedType.fill_adtype_for_type DataOnly ty)
            } )
    | s -> s in
  let transform fundef_opt stmt =
    optimize_minimal_variables ~gen_variables:gen_ad_variables
      ~update_expr:update_expr_ad_levels ~update_stmt ~extra_variables
      ~initial_variables:(initial_ad_variables fundef_opt stmt)
      stmt in
  transform_program_blockwise mir transform

(** Deduces whether types can be Structures of Arrays (SoA/fast) or Arrays of
    Structs (AoS/slow). See the docs in Mem_pattern.query_demote_stmt/exprs*
    functions for details on the rules surrounding when demotion from SoA -> AoS
    needs to happen.

    This first does a simple iter over the log_prob portion of the MIR, finding
    the names of all matrices (and arrays of matrices) where either the Stan
    math function does not support SoA or the object is single cell accessed
    within a For or While loop. These are the initial variables given to the
    monotone framework. Then log_prob has all matrix like objects and the
    functions that use them to SoA. After that the Monotone framework is used to
    deduce assignment paths of AoS <-> SoA and vice versa which need to be
    demoted to AoS as well as updating functions and objects after these
    assignment passes that then also need to be AoS.

    @param mir: The program's whole MIR. *)
let optimize_soa (mir : Program.Typed.t) =
  let gen_aos_variables
      (flowgraph_to_mir : Stmt.Located.Non_recursive.t LabelMap.t) (l : int)
      (aos_variables : string Set.Poly.t) =
    let mir_node mir_idx = LabelMap.find mir_idx flowgraph_to_mir in
    Memory_patterns.query_demotable_stmt aos_variables (mir_node l) in
  let initial_variables =
    List.fold_left ~init:Set.Poly.empty
      ~f:(Memory_patterns.query_initial_demotable_stmt false)
      mir.reverse_mode_log_prob in
  let mod_exprs aos_exits mod_expr =
    map_rec_expr (Memory_patterns.modify_expr_pattern aos_exits) mod_expr in
  let modify_stmt_patt stmt_pattern variable_set =
    Memory_patterns.modify_stmt_pattern stmt_pattern variable_set in
  let transform stmt =
    optimize_minimal_variables ~gen_variables:gen_aos_variables
      ~update_expr:mod_exprs ~update_stmt:modify_stmt_patt ~initial_variables
      stmt ~extra_variables:(fun _ -> initial_variables) in
  let transform' s =
    match transform {pattern= SList s; meta= Location_span.empty} with
    | {pattern= SList (l : Stmt.Located.t list); _} -> l
    | _ ->
        ICE.internal_error
          "Something went wrong with program transformation packing!"
        [@coverage off] in
  {mir with reverse_mode_log_prob= transform' mir.reverse_mode_log_prob}

(* Apparently you need to completely copy/paste type definitions between ml and
   mli files?*)
type optimization_settings =
  { function_inlining: bool
  ; static_loop_unrolling: bool
  ; one_step_loop_unrolling: bool
  ; vectorize_loops: bool
  ; list_collapsing: bool
  ; block_fixing: bool
  ; allow_uninitialized_decls: bool
  ; constant_propagation: bool
  ; expression_propagation: bool
  ; copy_propagation: bool
  ; dead_code_elimination: bool
  ; partial_evaluation: bool
  ; lazy_code_motion: bool
  ; optimize_ad_levels: bool
  ; preserve_stability: bool
  ; optimize_soa: bool }

let settings_const b =
  { function_inlining= b
  ; static_loop_unrolling= b
  ; one_step_loop_unrolling= b
  ; vectorize_loops= b
  ; list_collapsing= b
  ; block_fixing= b
  ; allow_uninitialized_decls= b
  ; constant_propagation= b
  ; expression_propagation= b
  ; copy_propagation= b
  ; dead_code_elimination= b
  ; partial_evaluation= b
  ; lazy_code_motion= b
  ; optimize_ad_levels= b
  ; preserve_stability= not b
  ; optimize_soa= b }

let all_optimizations : optimization_settings = settings_const true
let no_optimizations : optimization_settings = settings_const false

type optimization_level = O0 | O1 | Oexperimental

let level_optimizations (lvl : optimization_level) : optimization_settings =
  match lvl with
  | O0 -> no_optimizations
  | O1 ->
      { function_inlining= true
      ; static_loop_unrolling= false
      ; one_step_loop_unrolling= false
      ; vectorize_loops= true
      ; list_collapsing= true
      ; block_fixing= true
      ; constant_propagation= true
      ; expression_propagation= false
      ; copy_propagation= true
      ; dead_code_elimination= true
      ; partial_evaluation= true
      ; lazy_code_motion= false
      ; allow_uninitialized_decls= true
      ; optimize_ad_levels= false
      ; preserve_stability= false
      ; optimize_soa= true }
  | Oexperimental -> all_optimizations

let optimization_suite ?(settings = all_optimizations) mir =
  let preserve_stability = settings.preserve_stability in
  let maybe_optimizations =
    [ (* Phase order. See phase-ordering-nodes.org for details *)
      (* Book section A *)
      (* Book section B *)
      (* Book: Procedure integration *)
      (function_inlining, settings.function_inlining)
      (* Book: Sparse conditional constant propagation *)
    ; (constant_propagation ~preserve_stability, settings.constant_propagation)
      (* Book section C *)
      (* Book: Local and global copy propagation *)
    ; (copy_propagation, settings.copy_propagation)
      (* Book: Sparse conditional constant propagation *)
    ; (constant_propagation ~preserve_stability, settings.constant_propagation)
      (* Book: Dead-code elimination *)
    ; (dead_code_elimination, settings.dead_code_elimination)
      (* Vectorization needs the loops intact, so it runs before one-step
         unrolling. *)
    ; (vectorize_loops, settings.vectorize_loops)
      (* Matthijs: Before lazy code motion to get loop-invariant code motion *)
    ; (one_step_loop_unrolling, settings.one_step_loop_unrolling)
      (* Matthjis: expression_propagation < partial_evaluation *)
    ; ( expression_propagation ~preserve_stability
      , settings.expression_propagation )
      (* Matthjis: partial_evaluation < lazy_code_motion *)
    ; (partial_evaluation, settings.partial_evaluation)
      (* Book: Loop-invariant code motion *)
    ; (lazy_code_motion ~preserve_stability, settings.lazy_code_motion)
      (* Matthijs: lazy_code_motion < copy_propagation TODO: Check if this is
         necessary *)
    ; (copy_propagation, settings.copy_propagation)
      (* Matthijs: Constant propagation before static loop unrolling *)
    ; (constant_propagation ~preserve_stability, settings.constant_propagation)
      (* Book: Loop simplification *)
    ; (static_loop_unrolling, settings.static_loop_unrolling)
      (* Book: Dead-code elimination *)
      (* Matthijs: Everything < Dead-code elimination *)
    ; (dead_code_elimination, settings.dead_code_elimination)
      (* Book: Machine idioms and instruction combining *)
    ; (list_collapsing, settings.list_collapsing)
      (* Book: Machine idioms and instruction combining *)
    ; (optimize_soa, settings.optimize_soa)
    ; (optimize_ad_levels, settings.optimize_ad_levels)
      (* Remove decls immediately assigned to *)
    ; (allow_uninitialized_decls, settings.allow_uninitialized_decls)
      (* Book: Machine idioms and instruction combining *)
      (* Matthijs: Everything < block_fixing *)
    ; (block_fixing, settings.block_fixing) ] in
  let optimizations =
    List.filter_map maybe_optimizations ~f:(fun (fn, flag) ->
        if flag then Some fn else None) in
  List.fold_left optimizations ~init:mir ~f:( |> )
