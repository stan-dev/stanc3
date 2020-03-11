open Core_kernel
open Middle

let use_opencl = ref false

let opencl_triggers =
  String.Map.of_alist_exn
    [ ( "normal_id_glm_lpdf"
      , ( [0; 1]
        , [ (* Array of conditions under which to move to OpenCL *)
            ([1], (* Argument 1 is data *)
                  [(1, UnsizedType.UMatrix)])
          (* Argument 1 is a matrix *)
           ] ) )
    ; ( "bernoulli_logit_glm_lpmf"
      , ([0; 1], [([1], [(1, UnsizedType.UMatrix)])]) )
    ; ( "categorical_logit_glm_lpmf"
      , ([0; 1], [([1], [(1, UnsizedType.UMatrix)])]) )
    ; ( "neg_binomial_2_log_glm_lpmf"
      , ([0; 1], [([1], [(1, UnsizedType.UMatrix)])]) )
    ; ( "ordered_logistic_glm_lpmf"
      , ([0; 1], [([1], [(1, UnsizedType.UMatrix)])]) )
    ; ("poisson_log_glm_lpmf", ([0; 1], [([1], [(1, UnsizedType.UMatrix)])]))
    ]

let opencl_suffix = "_opencl__"

let to_matrix_cl e =
  Expr.Fixed.{e with pattern= FunApp (StanLib, "to_matrix_cl", [e])}

let rec switch_expr_to_opencl available_cl_vars (Expr.Fixed.({pattern; _}) as e)
    =
  let is_avail = List.mem available_cl_vars ~equal:( = ) in
  let to_cl (Expr.Fixed.({pattern; _}) as e) =
    match pattern with
    | Var s when is_avail s ->
        Expr.Fixed.{e with pattern= Var (s ^ opencl_suffix)}
    | _ -> to_matrix_cl e
  in
  let move_cl_args cl_args index arg =
    if List.mem ~equal:( = ) cl_args index then to_cl arg else arg
  in
  let check_type args (i, t) = Expr.Typed.type_of (List.nth_exn args i) = t in
  let check_if_data args ind =
    let Expr.Fixed.({pattern; _}) = List.nth_exn args ind in
    match pattern with Var s when is_avail s -> true | _ -> false
  in
  let req_met args (data_arg, type_arg) =
    List.for_all ~f:(check_if_data args) data_arg
    && List.for_all ~f:(check_type args) type_arg
  in
  let any_req_met args req_args = List.exists ~f:(req_met args) req_args in
  let maybe_map_args args (cl_args, req_args) =
    match any_req_met args req_args with
    | true -> List.mapi args ~f:(move_cl_args cl_args)
    | false -> args
  in
  let trim_propto f = String.substr_replace_all ~pattern:"_propto_" ~with_:"_" f in
  match pattern with
  | FunApp (StanLib, f, args) when Map.mem opencl_triggers (trim_propto f) ->
      let trigger = Map.find_exn opencl_triggers (trim_propto f) in
      {e with pattern= FunApp (StanLib, f, maybe_map_args args trigger)}
  | x ->
      { e with
        pattern=
          Expr.Fixed.Pattern.map (switch_expr_to_opencl available_cl_vars) x }

let rec base_type = function
  | SizedType.SArray (t, _) -> base_type t
  | SVector _ | SRowVector _ | SMatrix _ -> UnsizedType.UReal
  | x -> SizedType.to_unsized x

let pos = "pos__"

let data_read smeta (decl_id, st) =
  let unsized = SizedType.to_unsized st in
  let scalar = base_type st in
  let flat_type = UnsizedType.UArray scalar in
  let decl_var =
    { Expr.Fixed.pattern= Var decl_id
    ; meta= Expr.Typed.Meta.{loc= smeta; type_= unsized; adlevel= DataOnly} }
  in
  let swrap stmt = {Stmt.Fixed.pattern= stmt; meta= smeta} in
  let pos_var = {Expr.Fixed.pattern= Var pos; meta= Expr.Typed.Meta.empty} in
  let readfnapp var =
    Expr.Helpers.internal_funapp FnReadData
      [{var with pattern= Lit (Str, decl_id)}]
      Expr.Typed.Meta.{var.meta with type_= flat_type}
  in
  match unsized with
  | UInt | UReal ->
      [ Assignment
          ( (decl_id, unsized, [])
          , { Expr.Fixed.pattern=
                Indexed (readfnapp decl_var, [Single Expr.Helpers.loop_bottom])
            ; meta= {decl_var.meta with type_= unsized} } )
        |> swrap ]
  | UArray UInt | UArray UReal ->
      [Assignment ((decl_id, flat_type, []), readfnapp decl_var) |> swrap]
  | UFun _ | UMathLibraryFunction ->
      raise_s [%message "Cannot read a function type."]
  | UVector | URowVector | UMatrix | UArray _ ->
      let decl, assign, flat_var =
        let decl_id = decl_id ^ "_flat__" in
        ( Stmt.Fixed.Pattern.Decl
            {decl_adtype= AutoDiffable; decl_id; decl_type= Unsized flat_type}
          |> swrap
        , Assignment ((decl_id, flat_type, []), readfnapp decl_var) |> swrap
        , { Expr.Fixed.pattern= Var decl_id
          ; meta=
              Expr.Typed.Meta.{loc= smeta; type_= flat_type; adlevel= DataOnly}
          } )
      in
      let bodyfn var =
        let pos_increment =
          [ Assignment ((pos, UInt, []), Expr.Helpers.(binop pos_var Plus one))
            |> swrap ]
        in
        let read_indexed _ =
          { Expr.Fixed.pattern= Indexed (flat_var, [Single pos_var])
          ; meta= Expr.Typed.Meta.{flat_var.meta with type_= scalar} }
        in
        SList
          ( Stmt.Helpers.assign_indexed (SizedType.to_unsized st) decl_id smeta
              read_indexed var
          :: pos_increment )
        |> swrap
      in
      let pos_reset =
        Stmt.Fixed.Pattern.Assignment
          ((pos, UInt, []), Expr.Helpers.loop_bottom)
        |> swrap
      in
      [ Block
          [ decl; assign; pos_reset
          ; Stmt.Helpers.for_scalar_inv st bodyfn decl_var smeta ]
        |> swrap ]

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
          (decl_id, [Stmt.Fixed.{meta= smeta; pattern= d}])
    in
    let unconstrained_decl_var =
      let meta =
        Expr.Typed.Meta.create ~loc:smeta
          ~type_:SizedType.(to_unsized cst)
          ~adlevel:AutoDiffable ()
      in
      Expr.Fixed.{meta; pattern= Var decl_id}
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

let rec add_jacobians Stmt.Fixed.({meta= smeta; pattern}) =
  match pattern with
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
let rec ensure_body_in_block (Stmt.Fixed.({pattern; _}) as stmt) =
  let in_block stmt =
    let pattern =
      Stmt.Fixed.(
        match stmt.pattern with
        | Block l | SList l -> Pattern.Block l
        | _ -> Block [stmt])
    in
    {stmt with pattern}
  in
  let ensure_body_in_block_base pattern =
    Stmt.Fixed.Pattern.(
      match pattern with
      | IfElse (_, _, _) | While (_, _) | For _ -> map Fn.id in_block pattern
      | _ -> pattern)
  in
  let pattern =
    ensure_body_in_block_base
      Stmt.Fixed.(Pattern.map Fn.id ensure_body_in_block pattern)
  in
  {stmt with pattern}

let rec flatten_slists_list ls =
  let flatten_slist stmt =
    Stmt.Fixed.(match stmt.pattern with SList ls -> ls | _ -> [stmt])
  in
  let rec flatten_slists_stmt stmt =
    let pattern =
      Stmt.Fixed.(
        match stmt.pattern with
        | Block ls ->
            Pattern.Block
              (List.concat_map
                 ~f:(Fn.compose flatten_slist flatten_slists_stmt)
                 ls)
        | pattern -> Pattern.map Fn.id flatten_slists_stmt pattern)
    in
    {stmt with pattern}
  in
  List.concat_map ls ~f:(fun stmt ->
      Stmt.Fixed.(
        match stmt.pattern with
        | SList ls -> flatten_slists_list ls
        | _ -> [stmt]) )
  |> List.map ~f:flatten_slists_stmt

let%expect_test "Flatten slists" =
  let e pattern = Expr.Fixed.{meta= (); pattern} in
  let s pattern = Stmt.Fixed.{meta= (); pattern} in
  let stmt =
    Stmt.Fixed.Pattern.(
      [ SList
          [ Block
              [ SList
                  [ While (e (Var "hi"), Block [SList [Break |> s] |> s] |> s)
                    |> s ]
                |> s ]
            |> s ]
        |> s ]
      |> flatten_slists_list)
  in
  print_s [%sexp (stmt : (unit, unit) Stmt.Fixed.t list)] ;
  [%expect
    {|
    (((pattern
       (Block
        (((pattern
           (While ((pattern (Var hi)) (meta ()))
            ((pattern (Block (((pattern Break) (meta ()))))) (meta ()))))
          (meta ())))))
      (meta ()))) |}]

let add_reads vars mkread stmts =
  let var_names = String.Map.of_alist_exn vars in
  let add_read_to_decl (Stmt.Fixed.({pattern; meta}) as stmt) =
    match pattern with
    | Decl {decl_id; _} when Map.mem var_names decl_id ->
        stmt :: mkread meta (decl_id, Map.find_exn var_names decl_id)
    | _ -> [stmt]
  in
  List.concat_map ~f:add_read_to_decl stmts

let gen_write (decl_id, sizedtype) =
  let bodyfn var =
    Stmt.Helpers.internal_nrfunapp FnWriteParam [var] Location_span.empty
  in
  let meta =
    {Expr.Typed.Meta.empty with type_= SizedType.to_unsized sizedtype}
  in
  let expr = Expr.Fixed.{meta; pattern= Var decl_id} in
  Stmt.Helpers.for_scalar_inv sizedtype bodyfn expr Location_span.empty

let rec contains_var_expr is_vident accum Expr.Fixed.({pattern; _}) =
  accum
  ||
  match pattern with
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
  let rec change_constrain_target (Stmt.Fixed.({pattern; _}) as s) =
    match pattern with
    | Assignment (_, {pattern= FunApp (CompilerInternal, f, args); _})
      when ( Internal_fun.of_string_opt f = Some FnConstrain
           || Internal_fun.of_string_opt f = Some FnUnconstrain )
           && List.exists args
                ~f:(contains_var_expr (Set.mem target_vars) false) ->
        let rec change_var_expr (Expr.Fixed.({pattern; _}) as e) =
          match pattern with
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
        match e.pattern with
        | FunApp (k, f, a) when Map.mem fn_name_map f ->
            Pattern.FunApp (k, Map.find_exn fn_name_map f, a)
        | expr -> Pattern.map map_fn_names_expr expr)
    in
    {e with pattern}
  in
  let stmt =
    Stmt.Fixed.(
      match s.pattern with
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

let is_opencl_var = String.is_suffix ~suffix:opencl_suffix

let rec collect_vars_expr is_target accum Expr.Fixed.({pattern; _}) =
  Set.union accum
    ( match pattern with
    | Var s when is_target s -> String.Set.of_list [s]
    | x ->
        Expr.Fixed.Pattern.fold
          (collect_vars_expr is_target)
          String.Set.empty x )

let collect_opencl_vars s =
  let rec go accum s =
    Stmt.Fixed.(
      Pattern.fold (collect_vars_expr is_opencl_var) go accum s.pattern)
  in
  go String.Set.empty s

let%expect_test "collect vars expr" =
  let mkvar s = Expr.{Fixed.pattern= Var s; meta= Typed.Meta.empty} in
  let args = List.map ~f:mkvar ["y"; "x_opencl__"; "z"; "w_opencl__"] in
  let fnapp =
    Expr.
      {Fixed.pattern= FunApp (StanLib, "print", args); meta= Typed.Meta.empty}
  in
  Stmt.Fixed.{pattern= TargetPE fnapp; meta= Location_span.empty}
  |> collect_opencl_vars |> String.Set.sexp_of_t |> print_s ;
  [%expect {| (w_opencl__ x_opencl__) |}]

let%expect_test "insert before" =
  let l = [1; 2; 3; 4; 5; 6] |> insert_before (( = ) 6) [999] in
  [%sexp (l : int list)] |> print_s ;
  [%expect {| (1 2 3 4 5 999 6) |}]

let validate_sized decl_id meta transform st =
  let check fn x =
    Stmt.Helpers.internal_nrfunapp fn
      Expr.Helpers.
        [str decl_id; str (Fmt.strf "%a" Expression_gen.pp_expr x); x]
      meta
  in
  let nrfunapp fname args =
    Stmt.Fixed.{pattern= NRFunApp (CompilerInternal, fname, args); meta}
  in
  let rec dims_check = function
    | SizedType.SInt | SReal -> []
    | SArray (st, s) -> check FnValidateSize s :: dims_check st
    | SVector s | SRowVector s ->
        let fn =
          match transform with
          | Some Program.Simplex -> Internal_fun.FnValidateSizeSimplex
          | Some UnitVector -> FnValidateSizeUnitVector
          | _ -> FnValidateSize
        in
        [check fn s]
    | SMatrix (rows, cols) ->
        let validate_rows =
          match transform with
          | Some CholeskyCov ->
              nrfunapp "check_greater_or_equal"
                Expr.Helpers.
                  [ str ("cholesky_factor_cov " ^ decl_id)
                  ; str "num rows (must be greater or equal to num cols)"
                  ; rows; cols ]
          | _ -> check FnValidateSize rows
        in
        [validate_rows; check FnValidateSize cols]
  in
  dims_check st

let rec add_validate_dims outvars stmts =
  let transforms =
    List.filter_map
      ~f:(function
        | decl_id, Program.({out_block= Parameters; out_trans; _}) ->
            Some (decl_id, out_trans)
        | _ -> None)
      outvars
    |> String.Map.of_alist_exn
  in
  let with_size_checks = function
    | Stmt.Fixed.({pattern= Decl {decl_id; decl_type= Sized st; _}; meta}) as
      decl ->
        let tr = Map.find transforms decl_id in
        validate_sized decl_id meta tr st @ [decl]
    | stmt -> [validate_dims_stmt stmt]
  in
  List.concat_map ~f:with_size_checks stmts

and validate_dims_stmt stmt =
  let pattern =
    match stmt.pattern with
    | Stmt.Fixed.Pattern.Block s ->
        Stmt.Fixed.Pattern.Block (add_validate_dims [] s)
    | SList s -> SList (add_validate_dims [] s)
    | While (a, b) -> While (a, validate_dims_stmt b)
    | For f -> For {f with body= validate_dims_stmt f.body}
    | IfElse (p, t, e) ->
        IfElse (p, validate_dims_stmt t, Option.map ~f:validate_dims_stmt e)
    | s -> s
  in
  {stmt with pattern}

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
      Fixed.{meta; pattern= Var vident})
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

let type_needs_fill decl_id ut =
  Utils.is_user_ident decl_id
  && (contains_eigen ut || match ut with UReal -> true | _ -> false)

let rec add_fill no_fill_required = function
  | Stmt.Fixed.({pattern= Decl {decl_id; decl_type= Sized st; _}; meta}) as
    decl
    when (not (Set.mem no_fill_required decl_id))
         && type_needs_fill decl_id (SizedType.to_unsized st) ->
      (* I *think* we only need to initialize eigen types and scalars because we already construct
       std::vectors with 0s.
    *)
      Stmt.Fixed.{pattern= SList [decl; make_fill decl_id st meta]; meta}
  | {pattern; meta} ->
      Stmt.Fixed.
        {pattern= Pattern.map Fn.id (add_fill no_fill_required) pattern; meta}

let map_prog_stmt_lists f (p : ('a, 'b) Program.t) =
  { p with
    Program.prepare_data= f p.prepare_data
  ; log_prob= f p.log_prob
  ; generate_quantities= f p.generate_quantities
  ; transform_inits= f p.transform_inits }

let trans_prog (p : Program.Typed.t) =
  let p = Program.map Fn.id map_fn_names p in
  let init_pos =
    [ Stmt.Fixed.Pattern.Decl
        {decl_adtype= DataOnly; decl_id= pos; decl_type= Sized SInt}
    ; Assignment ((pos, UInt, []), Expr.Helpers.loop_bottom) ]
    |> List.map ~f:(fun pattern ->
           Stmt.Fixed.{pattern; meta= Location_span.empty} )
  in
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
  let tparam_start stmt =
    Stmt.Fixed.(
      match stmt.pattern with
      | IfElse (cond, _, _)
        when contains_var_expr
               (( = ) "emit_transformed_parameters__")
               false cond ->
          true
      | _ -> false)
  in
  let gq_start Stmt.Fixed.({pattern; _}) =
    match pattern with
    | IfElse
        ( { pattern=
              FunApp (_, _, [{pattern= Var "emit_generated_quantities__"; _}]); _
          }
        , _
        , _ ) ->
        true
    | _ -> false
  in
  let translate_to_open_cl stmts =
    if !use_opencl then
      let decl Stmt.Fixed.({pattern; _}) =
        match pattern with Decl d -> Some d.decl_id | _ -> None
      in
      let data_var_idents = List.filter_map ~f:decl p.prepare_data in
      let switch_expr = switch_expr_to_opencl data_var_idents in
      let rec trans_stmt_to_opencl s =
        Stmt.Fixed.
          { s with
            pattern= Pattern.map switch_expr trans_stmt_to_opencl s.pattern }
      in
      List.map stmts ~f:trans_stmt_to_opencl
    else stmts
  in
  let functions_block =
    List.map
      ~f:(fun def -> {def with fdbody= validate_dims_stmt def.fdbody})
      p.functions_block
  in
  let generate_quantities =
    ( p.generate_quantities
    |> add_validate_dims p.output_vars
    |> add_reads p.output_vars param_read
    |> translate_to_open_cl
    |> constrain_in_params p.output_vars
    |> insert_before tparam_start param_writes
    |> insert_before gq_start tparam_writes )
    @ gq_writes
  in
  let log_prob =
    p.log_prob |> List.map ~f:add_jacobians
    |> add_validate_dims p.output_vars
    |> add_reads p.output_vars param_read
    |> constrain_in_params p.output_vars
    |> translate_to_open_cl
  in
  let opencl_vars =
    String.Set.union_list
      (List.concat_map
         ~f:(List.map ~f:collect_opencl_vars)
         [log_prob; generate_quantities])
    |> String.Set.to_list
  in
  let to_matrix_cl_stmts =
    List.concat_map opencl_vars ~f:(fun vident ->
        let vident_sans_opencl =
          String.chop_suffix_exn ~suffix:opencl_suffix vident
        in
        let type_of_input_var =
          match
            List.Assoc.find p.input_vars vident_sans_opencl ~equal:String.equal
          with
          | Some st -> SizedType.to_unsized st
          | None -> UnsizedType.UMatrix
        in
        [ Stmt.Fixed.
            { pattern=
                Decl
                  { decl_adtype= DataOnly
                  ; decl_id= vident
                  ; decl_type= Type.Unsized type_of_input_var }
            ; meta= Location_span.empty }
        ; { pattern=
              Assignment
                ( (vident, type_of_input_var, [])
                , to_matrix_cl
                    { pattern= Var vident_sans_opencl
                    ; meta= Expr.Typed.Meta.empty } )
          ; meta= Location_span.empty } ] )
  in
  let p =
    { p with
      functions_block
    ; log_prob
    ; prog_name= escape_name p.prog_name
    ; prepare_data=
        init_pos
        @ ( add_validate_dims [] p.prepare_data
          |> add_reads p.input_vars data_read )
        @ to_matrix_cl_stmts
    ; transform_inits=
        init_pos
        @ ( add_validate_dims p.output_vars p.transform_inits
          |> add_reads constrained_params data_read )
        @ List.map ~f:gen_write constrained_params
    ; generate_quantities }
  in
  Program.(
    p
    |> map Fn.id ensure_body_in_block
    |> map Fn.id (add_fill (String.Set.of_list data_and_params))
    |> map_prog_stmt_lists flatten_slists_list)
