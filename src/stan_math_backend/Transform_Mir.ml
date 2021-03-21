open Core_kernel
open Middle

let use_opencl = ref false

let opencl_trigger_restrictions =
  String.Map.of_alist_exn
    [ ( "bernoulli_lpmf"
      , [ [ (0, UnsizedType.DataOnly, UnsizedType.UArray UnsizedType.UInt)
          ; (1, UnsizedType.DataOnly, UnsizedType.UReal) ] ] )
    ; ( "bernoulli_logit_glm_lpmf"
      , [ (* Array of conditions under which we do not want to move to OpenCL *)
          [(1, UnsizedType.DataOnly, UnsizedType.URowVector)]
        (* Argument 1 (0-based indexing) is a row vector *)
         ] )
    ; ( "categorical_logit_glm_lpmf"
      , [[(1, UnsizedType.DataOnly, UnsizedType.URowVector)]] )
    ; ( "exponential_lpdf"
      , [ [ (0, UnsizedType.AutoDiffable, UnsizedType.UVector)
          ; (1, UnsizedType.DataOnly, UnsizedType.UReal) ] ] )
    ; ( "neg_binomial_2_log_glm_lpmf"
      , [[(1, UnsizedType.DataOnly, UnsizedType.URowVector)]] )
    ; ( "normal_id_glm_lpdf"
      , [[(1, UnsizedType.DataOnly, UnsizedType.URowVector)]] )
    ; ( "ordered_logistic_glm_lpmf"
      , [[(1, UnsizedType.DataOnly, UnsizedType.URowVector)]] )
    ; ( "poisson_log_glm_lpmf"
      , [[(1, UnsizedType.DataOnly, UnsizedType.URowVector)]] )
    ; ( "std_normal_lpdf"
      , [[(0, UnsizedType.AutoDiffable, UnsizedType.UVector)]] )
    ; ( "uniform_lpdf"
      , [ [ (0, UnsizedType.AutoDiffable, UnsizedType.UVector)
          ; (1, UnsizedType.DataOnly, UnsizedType.UReal)
          ; (1, UnsizedType.DataOnly, UnsizedType.UReal) ] ] ) ]

let opencl_supported_functions =
  [ "bernoulli_lpmf"; "bernoulli_logit_lpmf"; "bernoulli_logit_glm_lpmf"
  ; "beta_lpdf"; "beta_proportion_lpdf"; "binomial_lpmf"
  ; "categorical_logit_glm_lpmf"; "cauchy_lpdf"; "chi_square_lpdf"
  ; "double_exponential_lpdf"; "exp_mod_normal_lpdf"; "exponential_lpdf"
  ; "frechet_lpdf"; "gamma_lpdf"; "gumbel_lpdf"; "inv_chi_square_lpdf"
  ; "inv_gamma_lpdf"; "logistic_lpdf"; "lognormal_lpdf"; "neg_binomial_lpmf"
  ; "neg_binomial_2_lpmf"; "neg_binomial_2_log_lpmf"
  ; "neg_binomial_2_log_glm_lpmf"; "normal_lpdf"; "normal_id_glm_lpdf"
  ; "ordered_logistic_glm_lpmf"; "pareto_lpdf"; "pareto_type_2_lpdf"
  ; "poisson_lpmf"; "poisson_log_lpmf"; "poisson_log_glm_lpmf"; "rayleigh_lpdf"
  ; "scaled_inv_chi_square_lpdf"; "skew_normal_lpdf"; "std_normal_lpdf"
  ; "student_t_lpdf"; "uniform_lpdf"; "weibull_lpdf" ]
  |> String.Set.of_list

let opencl_suffix = "_opencl__"

let to_matrix_cl e =
  Expr.Fixed.{e with pattern= FunApp (StanLib, "to_matrix_cl", [e])}

let rec switch_expr_to_opencl available_cl_vars (Expr.Fixed.({pattern; _}) as e)
    =
  let is_avail = List.mem available_cl_vars ~equal:( = ) in
  let to_cl (Expr.Fixed.({pattern; meta= {Expr.Typed.Meta.type_; _}}) as e) =
    match (pattern, type_) with
    | Var s, _ when is_avail s ->
        Expr.Fixed.{e with pattern= Var (s ^ opencl_suffix)}
    | _, UnsizedType.(UInt | UReal) -> e
    | _, _ -> to_matrix_cl e
  in
  let check_type args (i, ad, t) =
    let arg = List.nth_exn args i in
    Expr.Typed.type_of arg = t
    && UnsizedType.autodifftype_can_convert (Expr.Typed.adlevel_of arg) ad
  in
  let is_restricted args =
    List.exists ~f:(List.for_all ~f:(check_type args))
  in
  let maybe_map_args args req_args =
    match req_args with
    | Some x when is_restricted args x -> args
    | None | Some _ -> List.map args ~f:to_cl
  in
  let is_fn_opencl_supported f =
    Set.mem opencl_supported_functions (Utils.stdlib_distribution_name f)
  in
  match pattern with
  | FunApp (StanLib, f, args) when is_fn_opencl_supported f ->
      let trigger =
        Map.find opencl_trigger_restrictions (Utils.stdlib_distribution_name f)
      in
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

type constrainaction = Check | Constrain | Unconstrain [@@deriving sexp]

let check_constraint_to_string t (c : constrainaction) =
  match t with
  | Program.Ordered -> "ordered"
  | PositiveOrdered -> "positive_ordered"
  | Simplex -> "simplex"
  | UnitVector -> "unit_vector"
  | CholeskyCorr -> "cholesky_factor_corr"
  | CholeskyCov -> "cholesky_factor_cov"
  | Correlation -> "corr_matrix"
  | Covariance -> "cov_matrix"
  | Lower _ -> (
    match c with
    | Check -> "greater_or_equal"
    | Constrain | Unconstrain -> "lb" )
  | Upper _ -> (
    match c with Check -> "less_or_equal" | Constrain | Unconstrain -> "ub" )
  | LowerUpper _ -> (
    match c with
    | Check ->
        raise_s
          [%message "LowerUpper is really two other checks tied together"]
    | Constrain | Unconstrain -> "lub" )
  | Offset _ | Multiplier _ | OffsetMultiplier _ -> (
    match c with Check -> "" | Constrain | Unconstrain -> "offset_multiplier" )
  | Identity -> ""

let constrain_constraint_to_string t (c : constrainaction) =
  match t with
  | Program.CholeskyCorr -> "cholesky_factor_corr"
  | _ -> check_constraint_to_string t c

let default_multiplier = 1
let default_offset = 0

let transform_args = function
  | Program.Offset offset -> [offset; Expr.Helpers.int default_multiplier]
  | Multiplier multiplier -> [Expr.Helpers.int default_offset; multiplier]
  | transform ->
      Program.fold_transformation (fun args arg -> args @ [arg]) [] transform

let param_read smeta
    (decl_id, Program.({out_constrained_st= cst; out_block; out_trans; _})) =
  if not (out_block = Parameters) then []
  else
    let ut = SizedType.to_unsized cst in
    let decl_var =
      Expr.Fixed.
        { pattern= Var decl_id
        ; meta=
            Expr.Typed.Meta.create ~loc:smeta ~type_:ut ~adlevel:AutoDiffable
              () }
    in
    let transform_args = transform_args out_trans in
    (*
      For constrains that return square / lower triangular matrices the C++
      only wants one of the matrix dimensions.
    *)
    let rec constrain_get_dims st =
      match st with
      | SizedType.SInt | SReal -> []
      | SVector d | SRowVector d -> [d]
      | SMatrix (_, dim2) -> [dim2]
      | SArray (t, dim) -> dim :: constrain_get_dims t
    in
    let read_constrain_dims constrain_transform st =
      match constrain_transform with
      | Program.CholeskyCorr | Correlation | Covariance ->
          constrain_get_dims st
      | _ -> SizedType.get_dims st
    in
    (* this is an absolute hack

       I need to unpack the constraint arguments and the dimensions in codegen, but we pack them all together into a fake function FnReadParam as expressions
       So, I'm packing in the number of constraint arguments to read as an int expression
       To avoid this hack, keep internal functions as a variant type instead of a normal funapp
    *)
    let n_args_expression = Expr.Helpers.int (List.length transform_args) in
    let read =
      Expr.(
        Helpers.(
          internal_funapp FnReadParam
            ( Expr.Helpers.str
                (constrain_constraint_to_string out_trans Constrain)
            :: n_args_expression
            :: (transform_args @ read_constrain_dims out_trans cst) ))
          Typed.Meta.{decl_var.meta with type_= ut})
    in
    [ Stmt.Fixed.
        {pattern= Pattern.Assignment ((decl_id, ut, []), read); meta= smeta} ]

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

let gen_write_unconstrained (decl_id, sizedtype) =
  let bodyfn var =
    let var =
      match var.Expr.Fixed.pattern with
      | Indexed ({pattern= Indexed (expr, idcs1); _}, idcs2) ->
          {var with pattern= Indexed (expr, idcs1 @ idcs2)}
      | _ -> var
    in
    Stmt.Helpers.internal_nrfunapp FnWriteParam [var] Location_span.empty
  in
  let meta =
    {Expr.Typed.Meta.empty with type_= SizedType.to_unsized sizedtype}
  in
  let expr = Expr.Fixed.{meta; pattern= Var decl_id} in
  let writefn var =
    Stmt.Helpers.for_scalar_inv
      (SizedType.inner_type sizedtype)
      bodyfn var Location_span.empty
  in
  Stmt.Helpers.for_eigen sizedtype writefn expr Location_span.empty

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
    | Assignment
        (lval, {pattern= FunApp (CompilerInternal, f, var :: args); meta})
      when Internal_fun.of_string_opt f = Some FnConstrain
           && contains_var_expr (Set.mem target_vars) false var ->
        let rec change_var_expr (Expr.Fixed.({pattern; _}) as e) =
          match pattern with
          | Var vident when Set.mem target_vars vident ->
              {e with pattern= Var (vident ^ "_in__")}
          | pattern ->
              {e with pattern= Expr.Fixed.Pattern.map change_var_expr pattern}
        in
        Stmt.Fixed.
          { s with
            pattern=
              Assignment
                ( lval
                , { pattern=
                      FunApp (CompilerInternal, f, change_var_expr var :: args)
                  ; meta } ) }
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
  let get_pname_ust = function
    | ( name
      , { Program.out_block= Parameters
        ; out_unconstrained_st
        ; out_trans= Identity; _ } ) ->
        Some (name, out_unconstrained_st)
    | name, {Program.out_block= Parameters; out_unconstrained_st; _} ->
        Some (name ^ "_free__", out_unconstrained_st)
    | _ -> None
  in
  let constrained_params = List.filter_map ~f:get_pname_cst p.output_vars in
  let free_params = List.filter_map ~f:get_pname_ust p.output_vars in
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
        match pattern with
        | Decl {decl_type= Sized (SInt | SReal); _} -> None
        | Decl {decl_id; _} -> Some decl_id
        | _ -> None
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
  let tparam_writes_cond =
    match tparam_writes with
    | [] -> []
    | _ ->
        [ Stmt.Fixed.
            { pattern=
                IfElse
                  ( Expr.
                      { Fixed.pattern= Var "emit_transformed_parameters__"
                      ; meta= Typed.Meta.empty }
                  , {pattern= SList tparam_writes; meta= Location_span.empty}
                  , None )
            ; meta= Location_span.empty } ]
  in
  let generate_quantities =
    ( p.generate_quantities
    |> add_reads p.output_vars param_read
    |> translate_to_open_cl
    |> constrain_in_params p.output_vars
    |> insert_before tparam_start param_writes
    |> insert_before gq_start tparam_writes_cond )
    @ gq_writes
  in
  let log_prob =
    p.log_prob |> List.map ~f:add_jacobians
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
      log_prob
    ; prog_name= escape_name p.prog_name
    ; prepare_data=
        init_pos
        @ (p.prepare_data |> add_reads p.input_vars data_read)
        @ to_matrix_cl_stmts
    ; transform_inits=
        init_pos
        @ (p.transform_inits |> add_reads constrained_params data_read)
        @ List.map ~f:gen_write_unconstrained free_params
    ; generate_quantities }
  in
  Program.(
    p
    |> map Fn.id ensure_body_in_block
    |> map_prog_stmt_lists flatten_slists_list)
