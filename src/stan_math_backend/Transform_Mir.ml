open Core_kernel
open Middle

let pos = "pos__"

let data_read smeta (decl_id, st) =
  let decl_var =
    { Expr.Fixed.pattern= Var decl_id
    ; meta=
        Expr.Typed.Meta.
          {loc= smeta; type_= SizedType.to_unsized st; adlevel= DataOnly} }
  in
  let swrap stmt = {Stmt.Fixed.pattern= stmt; meta= smeta} in
  let bodyfn var =
    let pos_var = {Expr.Fixed.pattern= Var pos; meta= Expr.Typed.Meta.empty} in
    let readfnapp var =
      let f =
        Expr.Helpers.internal_funapp FnReadData
          [{var with pattern= Lit (Str, decl_id)}]
          var.meta
      in
      let meta = Expr.Typed.Meta.{var.meta with type_= UInt} in
      {Expr.Fixed.pattern= Indexed (f, [Single pos_var]); meta}
    in
    let pos_increment =
      if SizedType.is_scalar st then []
      else
        [ Assignment ((pos, UInt, []), Expr.Helpers.(binop pos_var Plus one))
          |> swrap ]
    in
    SList
      ( Stmt.Helpers.assign_indexed (SizedType.to_unsized st) decl_id smeta
          readfnapp var
      :: pos_increment )
    |> swrap
  in
  let pos_reset =
    Stmt.Fixed.Pattern.Assignment ((pos, UInt, []), Expr.Helpers.loop_bottom)
    |> swrap
  in
  [pos_reset; Stmt.Helpers.for_scalar_inv st bodyfn decl_var smeta]

let rec base_type = function
  | SizedType.SArray (t, _) -> base_type t
  | SVector _ | SRowVector _ | SMatrix _ -> UnsizedType.UReal
  | x -> SizedType.to_unsized x

let rec base_ut_to_string = function
  | UnsizedType.UMatrix -> "matrix"
  | UVector -> "vector"
  | URowVector -> "row_vector"
  | UReal -> "scalar"
  | UInt -> "integer"
  | UArray t -> base_ut_to_string t
  | t ->
      raise_s
        [%message "Another place where it's weird to get " (t : UnsizedType.t)]

let param_read smeta
    ( decl_id
    , Program.({ out_constrained_st= cst
               ; out_unconstrained_st= ucst
               ; out_block; _ }) ) =
  if not (out_block = Parameters) then []
  else
    let decl_id, decl =
      match cst = ucst with
      | true -> (decl_id, [])
      | false ->
          let decl_id = decl_id ^ "_in__" in
          let d =
            Stmt.Fixed.Pattern.Decl
              {decl_adtype= AutoDiffable; decl_id; decl_type= Sized ucst}
          in
          (decl_id, [Stmt.Fixed.fix (smeta, d)])
    in
    let unconstrained_decl_var =
      let meta =
        Expr.Typed.Meta.create ~loc:smeta
          ~type_:SizedType.(to_unsized cst)
          ~adlevel:AutoDiffable ()
      in
      Expr.Fixed.fix (meta, Var decl_id)
    in
    let bodyfn var =
      let readfnapp (var : Expr.Typed.t) =
        Expr.(
          Helpers.internal_funapp FnReadParam
            ( { Fixed.pattern=
                  Lit (Str, base_ut_to_string (SizedType.to_unsized ucst))
              ; meta= Typed.Meta.empty }
            :: SizedType.dims_of ucst )
            Typed.Meta.{var.meta with type_= base_type ucst})
      in
      Stmt.Helpers.assign_indexed (SizedType.to_unsized cst) decl_id smeta
        readfnapp var
    in
    decl @ [Stmt.Helpers.for_eigen ucst bodyfn unconstrained_decl_var smeta]

let escape_name str =
  str
  |> String.substr_replace_all ~pattern:"." ~with_:"_"
  |> String.substr_replace_all ~pattern:"-" ~with_:"_"

let rec add_jacobians stmt =
  let smeta = Stmt.Fixed.meta_of stmt in
  match Stmt.Fixed.pattern_of stmt with
  | Assignment (lhs, {pattern= FunApp (CompilerInternal, f, args); meta= emeta})
    when Internal_fun.of_string_opt f = Some FnConstrain ->
      let var n = Expr.{Fixed.pattern= Var n; meta= Typed.Meta.empty} in
      let assign rhs =
        Stmt.{Fixed.pattern= Assignment (lhs, rhs); meta= smeta}
      in
      { Stmt.Fixed.pattern=
          IfElse
            ( var "jacobian__"
            , assign
                { Expr.Fixed.pattern=
                    FunApp (CompilerInternal, f, args @ [var "lp__"])
                ; meta= emeta }
            , Some
                (assign
                   { Expr.Fixed.pattern= FunApp (CompilerInternal, f, args)
                   ; meta= emeta }) )
      ; meta= smeta }
  | ptn ->
      Stmt.Fixed.{pattern= Pattern.map Fn.id add_jacobians ptn; meta= smeta}

(* Make sure that all if-while-and-for bodies are safely wrapped in a block in such a way that we can insert a location update before.
   The blocks make sure that the program with the inserted location update is still well-formed C++ though.
   *)
let rec ensure_body_in_block stmt =
  let pattern = Stmt.Fixed.pattern_of stmt in
  Stmt.Fixed.
    { stmt with
      pattern=
        ensure_body_in_block_base
        @@ Pattern.map Fn.id ensure_body_in_block pattern }

and ensure_body_in_block_base pattern =
  match pattern with
  | IfElse (_, _, _) | While (_, _) | For _ ->
      Stmt.Fixed.Pattern.map Fn.id in_block pattern
  | _ -> pattern

and in_block stmt =
  let pattern' =
    match Stmt.Fixed.pattern_of stmt with
    | Block l | SList l -> Stmt.Fixed.Pattern.Block l
    | _ -> Block [stmt]
  in
  {stmt with pattern= pattern'}

let flatten_slist stmt =
  match Stmt.Fixed.pattern_of stmt with SList ls -> ls | _ -> [stmt]

let add_reads stmts vars mkread =
  let var_names = String.Map.of_alist_exn vars in
  let add_read_to_decl stmt =
    match Stmt.Fixed.pattern_of stmt with
    | Decl {decl_id; _} when Map.mem var_names decl_id ->
        let meta = Stmt.Fixed.meta_of stmt in
        stmt :: mkread meta (decl_id, Map.find_exn var_names decl_id)
    | _ -> [stmt]
  in
  List.concat_map ~f:add_read_to_decl stmts |> List.concat_map ~f:flatten_slist

let gen_write (decl_id, sizedtype) =
  let bodyfn var =
    { Stmt.Fixed.pattern=
        NRFunApp (CompilerInternal, Internal_fun.to_string FnWriteParam, [var])
    ; meta= Location_span.empty }
  in
  let meta =
    {Expr.Typed.Meta.empty with type_= SizedType.to_unsized sizedtype}
  in
  let expr = Expr.Fixed.fix (meta, Var decl_id) in
  Stmt.Helpers.for_scalar_inv sizedtype bodyfn expr Location_span.empty

let rec contains_var_expr is_vident accum expr =
  accum
  ||
  match Expr.Fixed.pattern_of expr with
  | Var v when is_vident v -> true
  | pattern ->
      Expr.Fixed.Pattern.fold (contains_var_expr is_vident) false pattern

(* When a parameter's unconstrained type and its constrained type are different,
   we generate a new variable "<param_name>_in__" and read into that. We now need
   to change the FnConstrain calls to constrain that variable and assign to the
   actual <param_name> var.
*)
let constrain_in_params outvars stmts =
  let is_target_var = function
    | ( name
      , { Program.out_unconstrained_st
        ; out_constrained_st
        ; out_block= Parameters; _ } )
      when not (out_unconstrained_st = out_constrained_st) ->
        Some name
    | _ -> None
  in
  let target_vars =
    List.filter_map outvars ~f:is_target_var |> String.Set.of_list
  in
  let rec change_constrain_target s =
    match Stmt.Fixed.pattern_of s with
    | Assignment (_, {pattern= FunApp (CompilerInternal, f, args); _})
      when ( Internal_fun.of_string_opt f = Some FnConstrain
           || Internal_fun.of_string_opt f = Some FnUnconstrain )
           && List.exists args
                ~f:(contains_var_expr (Set.mem target_vars) false) ->
        let rec change_var_expr e =
          match Expr.Fixed.pattern_of e with
          | Var vident when Set.mem target_vars vident ->
              {e with pattern= Var (vident ^ "_in__")}
          | pattern ->
              {e with pattern= Expr.Fixed.Pattern.map change_var_expr pattern}
        in
        let rec change_var_stmt s =
          Stmt.Fixed.
            { s with
              pattern= Pattern.map change_var_expr change_var_stmt s.pattern }
        in
        change_var_stmt s
    | pattern ->
        Stmt.Fixed.
          {s with pattern= Pattern.map Fn.id change_constrain_target pattern}
  in
  List.map ~f:change_constrain_target stmts

let fn_name_map =
  String.Map.of_alist_exn [("integrate_ode", "integrate_ode_rk45")]

let rec map_fn_names s =
  let rec map_fn_names_expr e =
    let pattern =
      Expr.Fixed.(
        match pattern_of e with
        | FunApp (k, f, a) when Map.mem fn_name_map f ->
            Pattern.FunApp (k, Map.find_exn fn_name_map f, a)
        | expr -> Pattern.map map_fn_names_expr expr)
    in
    {e with pattern}
  in
  let stmt =
    Stmt.Fixed.(
      match pattern_of s with
      | NRFunApp (k, f, a) when Map.mem fn_name_map f ->
          Pattern.NRFunApp (k, Map.find_exn fn_name_map f, a)
      | stmt -> Pattern.map map_fn_names_expr map_fn_names stmt)
  in
  {s with pattern= stmt}

let rec insert_before f to_insert = function
  | [] -> to_insert
  | hd :: tl ->
      if f hd then to_insert @ (hd :: tl)
      else hd :: insert_before f to_insert tl

let%expect_test "insert before" =
  let l = [1; 2; 3; 4; 5; 6] |> insert_before (( = ) 6) [999] in
  [%sexp (l : int list)] |> print_s ;
  [%expect {| (1 2 3 4 5 999 6) |}]

let make_fill vident st loc =
  let rhs =
    Expr.(
      Helpers.internal_funapp FnNaN []
      @@ Typed.Meta.create ~type_:UReal ~loc ~adlevel:DataOnly ())
  in
  let ut = SizedType.to_unsized st in
  let var =
    Expr.(
      let meta = {Typed.Meta.empty with type_= ut; loc} in
      Fixed.fix (meta, Var vident))
  in
  let bodyfn var =
    Stmt.Fixed.
      { pattern=
          Assignment ((vident, ut, Expr.Helpers.collect_indices var), rhs)
      ; meta= loc }
  in
  Stmt.Helpers.for_scalar st bodyfn var loc

let rec contains_eigen = function
  | UnsizedType.UArray t -> contains_eigen t
  | UMatrix | URowVector | UVector -> true
  | _ -> false

let rec add_fill no_fill_required = function
  | Stmt.Fixed.({pattern= Decl {decl_id; decl_type= Sized st; _}; meta}) as
    decl
    when (not (Set.mem no_fill_required decl_id))
         && Utils.is_user_ident decl_id
         && (contains_eigen (SizedType.to_unsized st) || SizedType.is_scalar st)
    ->
      (* I *think* we only need to initialize eigen types and scalars because we already construct
       std::vectors with 0s.
    *)
      [decl; make_fill decl_id st meta]
  | {pattern= Decl {decl_id; decl_type= Unsized ut; _}; _}
    when (not (Set.mem no_fill_required decl_id))
         && Utils.is_user_ident decl_id
         && contains_eigen ut ->
      raise_s
        [%message
          "Unsized type initialization to NaN not yet implemented - consider \
           adding this to resize_to_match"]
  | {pattern= Block ls; _} as s ->
      [ { s with
          pattern= Block (List.concat_map ~f:(add_fill no_fill_required) ls) }
      ]
  | {pattern= SList ls; _} as s ->
      [ { s with
          pattern= SList (List.concat_map ~f:(add_fill no_fill_required) ls) }
      ]
  | s -> [s]

let trans_prog (p : Program.Typed.t) =
  let p = Program.map Fn.id map_fn_names p in
  let init_pos =
    [ Stmt.Fixed.Pattern.Decl
        {decl_adtype= DataOnly; decl_id= pos; decl_type= Sized SInt}
    ; Assignment ((pos, UInt, []), Expr.Helpers.loop_bottom) ]
    |> List.map ~f:(fun pattern ->
           Stmt.Fixed.{pattern; meta= Location_span.empty} )
  in
  let log_prob = List.map ~f:add_jacobians p.log_prob in
  let get_pname_cst = function
    | name, {Program.out_block= Parameters; out_constrained_st; _} ->
        Some (name, out_constrained_st)
    | _ -> None
  in
  let constrained_params = List.filter_map ~f:get_pname_cst p.output_vars in
  let param_writes, tparam_writes, gq_writes =
    List.map p.output_vars
      ~f:(fun (name, {out_constrained_st= st; out_block; _}) ->
        (out_block, gen_write (name, st)) )
    |> List.partition3_map ~f:(fun (b, x) ->
           match b with
           | Parameters -> `Fst x
           | TransformedParameters -> `Snd x
           | GeneratedQuantities -> `Trd x )
  in
  let data_and_params =
    List.map ~f:fst constrained_params @ List.map ~f:fst p.input_vars
  in
  let add_fills =
    List.concat_map ~f:(add_fill (String.Set.of_list data_and_params))
  in
  let tparam_start stmt =
    match Stmt.Fixed.pattern_of stmt with
    | IfElse (cond, _, _)
      when contains_var_expr (( = ) "emit_transformed_parameters__") false cond
      ->
        true
    | _ -> false
  in
  let gq_start stmt =
    match Stmt.Fixed.pattern_of stmt with
    | IfElse
        ( { pattern=
              FunApp (_, _, [{pattern= Var "emit_generated_quantities__"; _}]); _
          }
        , _
        , _ ) ->
        true
    | _ -> false
  in
  let gq =
    ( add_reads p.generate_quantities p.output_vars param_read
    |> constrain_in_params p.output_vars
    |> insert_before tparam_start param_writes
    |> insert_before gq_start tparam_writes )
    @ gq_writes
    |> add_fills
  in
  let p =
    { p with
      log_prob=
        add_reads log_prob p.output_vars param_read
        |> constrain_in_params p.output_vars
        |> add_fills
    ; prog_name= escape_name p.prog_name
    ; prepare_data=
        init_pos @ add_reads p.prepare_data p.input_vars data_read |> add_fills
    ; transform_inits=
        init_pos
        @ add_reads p.transform_inits constrained_params data_read
        @ List.map ~f:gen_write constrained_params
    ; generate_quantities= gq }
  in
  Program.map Fn.id ensure_body_in_block p
