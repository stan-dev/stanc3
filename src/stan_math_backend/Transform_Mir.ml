open Core_kernel
open Middle
open Mangle

let use_opencl = ref false

let translate_funapps_and_kwrds e =
  let open Expr.Fixed in
  let f ({pattern; _} as expr) =
    match pattern with
    | FunApp (UserDefined (fname, suffix), args) ->
        { expr with
          pattern=
            FunApp (UserDefined (add_prefix_to_kwrds fname, suffix), args) }
    | Var s -> {expr with pattern= Var (add_prefix_to_kwrds s)}
    | _ -> expr
  in
  rewrite_bottom_up ~f e

let rec change_kwrds_stmts s =
  let open Stmt.Fixed.Pattern in
  let pattern =
    match s.Stmt.Fixed.pattern with
    | Decl e -> Decl {e with decl_id= add_prefix_to_kwrds e.decl_id}
    | NRFunApp (UserDefined (s, sfx), e) ->
        NRFunApp (UserDefined (add_prefix_to_kwrds s, sfx), e)
    | Assignment ((s, t, e1), e2) ->
        Assignment ((add_prefix_to_kwrds s, t, e1), e2)
    | For e -> For {e with loopvar= add_prefix_to_kwrds e.loopvar}
    | x -> map Fn.id change_kwrds_stmts x
  in
  {s with pattern}

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
  Expr.Fixed.
    {e with pattern= FunApp (StanLib ("to_matrix_cl", FnPlain, AoS), [e])}

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
  let is_fn_opencl_supported f = Set.mem opencl_supported_functions f in
  match pattern with
  | FunApp (StanLib (f, sfx, mem_pattern), args) when is_fn_opencl_supported f
    ->
      let trigger = Map.find opencl_trigger_restrictions f in
      { e with
        pattern=
          FunApp (StanLib (f, sfx, mem_pattern), maybe_map_args args trigger)
      }
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
      [{var with pattern= Lit (Str, remove_prefix decl_id)}]
      Expr.Typed.Meta.{var.meta with type_= flat_type}
  in
  match unsized with
  | UInt | UReal | UComplex ->
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
            { decl_adtype= AutoDiffable
            ; decl_id
            ; decl_type= Unsized flat_type
            ; initialize= false }
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

(*
  Get the dimension expressions that are expected by constrain/unconstrain
  functions for a sized type.

  For constrains that return square / lower triangular matrices the C++
  only wants one of the matrix dimensions.
*)
let read_constrain_dims constrain_transform st =
  let rec constrain_get_dims st =
    match st with
    | SizedType.SInt | SReal | SComplex -> []
    | SVector (_, d) | SRowVector (_, d) -> [d]
    | SMatrix (_, _, dim2) -> [dim2]
    | SArray (t, dim) -> dim :: constrain_get_dims t
  in
  match constrain_transform with
  | Transformation.CholeskyCorr | Correlation | Covariance ->
      constrain_get_dims st
  | _ -> SizedType.get_dims st

let data_serializer_read loc out_constrained_st =
  let ut = SizedType.to_unsized out_constrained_st in
  let dims = SizedType.get_dims_io out_constrained_st in
  let emeta = Expr.Typed.Meta.create ~loc ~type_:ut ~adlevel:AutoDiffable () in
  Expr.(
    Helpers.(
      internal_funapp FnReadDataSerializer dims
        Typed.Meta.{emeta with type_= ut}))

let param_read smeta
    (decl_id, Program.({out_constrained_st= cst; out_block; out_trans; _})) =
  if not (out_block = Parameters) then []
  else
    let ut = SizedType.to_unsized cst in
    let emeta =
      Expr.Typed.Meta.create ~loc:smeta ~type_:ut ~adlevel:AutoDiffable ()
    in
    let dims = read_constrain_dims out_trans cst in
    let read =
      Expr.(
        Helpers.(
          internal_funapp
            (FnReadParam
               { constrain= out_trans
               ; dims
               ; mem_pattern= SizedType.get_mem_pattern cst })
            []
            Typed.Meta.{emeta with type_= ut}))
    in
    [ Stmt.Fixed.
        {pattern= Pattern.Assignment ((decl_id, ut, []), read); meta= smeta} ]

let escape_name str =
  str
  |> String.substr_replace_all ~pattern:"." ~with_:"_"
  |> String.substr_replace_all ~pattern:"-" ~with_:"_"

(** Make sure that all if-while-and-for bodies are safely wrapped in a block in
  such a way that we can insert a location update before. The blocks make sure
  that the program with the inserted location update is still well-formed C++ though.
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

let gen_write ?(unconstrain = false)
    (decl_id, Program.({out_constrained_st; out_trans; _})) =
  let decl_var =
    { Expr.Fixed.pattern= Var decl_id
    ; meta=
        Expr.Typed.Meta.
          { loc= Location_span.empty
          ; type_= SizedType.to_unsized out_constrained_st
          ; adlevel= DataOnly } }
  in
  Stmt.Helpers.internal_nrfunapp
    (FnWriteParam
       {unconstrain_opt= Option.some_if unconstrain out_trans; var= decl_var})
    [] Location_span.empty

(**
  Generate write instructions for unconstrained types. For scalars,
 *  matrices, vectors, and arrays with one dimension we can write
 *  these directly, but for arrays of arrays/vectors/matrices we
 *  need to use for_scalar_inv to write them in "column major order"
 *)
let gen_unconstrained_write (decl_id, Program.({out_constrained_st; _})) =
  if SizedType.is_recursive_container out_constrained_st then
    let bodyfn var =
      Stmt.Helpers.internal_nrfunapp
        (FnWriteParam {unconstrain_opt= None; var})
        [var] Location_span.empty
    in
    let meta =
      { Expr.Typed.Meta.empty with
        type_= SizedType.to_unsized out_constrained_st }
    in
    let expr = Expr.Fixed.{meta; pattern= Var decl_id} in
    Stmt.Helpers.for_scalar_inv out_constrained_st bodyfn expr
      Location_span.empty
  else
    let decl_var =
      { Expr.Fixed.pattern= Var decl_id
      ; meta=
          Expr.Typed.Meta.
            { loc= Location_span.empty
            ; type_= SizedType.to_unsized out_constrained_st
            ; adlevel= DataOnly } }
    in
    Stmt.Helpers.internal_nrfunapp
      (FnWriteParam {unconstrain_opt= None; var= decl_var})
      [] Location_span.empty

(** Statements to read, unconstrain and assign a parameter then write it back *)
let data_unconstrain_transform smeta (decl_id, outvar) =
  [ Stmt.Fixed.
      { pattern=
          Decl
            { decl_adtype= UnsizedType.AutoDiffable
            ; decl_id
            ; decl_type= Type.Sized outvar.Program.out_constrained_st
            ; initialize= true }
      ; meta= smeta }
  ; (let nonarray_st, array_dims =
       SizedType.get_array_dims outvar.Program.out_constrained_st
     in
     Stmt.Helpers.mk_nested_for (List.rev array_dims)
       (fun loopvars ->
         Stmt.Fixed.
           { meta= smeta
           ; pattern=
               Assignment
                 ( ( decl_id
                   , SizedType.to_unsized nonarray_st
                   , List.map ~f:(fun e -> Index.Single e) (List.rev loopvars)
                   )
                 , data_serializer_read smeta nonarray_st ) } )
       smeta)
  ; gen_write ~unconstrain:true (decl_id, outvar) ]

let rec contains_var_expr is_vident accum Expr.Fixed.({pattern; _}) =
  accum
  ||
  match pattern with
  | Var v when is_vident v -> true
  | pattern ->
      Expr.Fixed.Pattern.fold (contains_var_expr is_vident) false pattern

let fn_name_map =
  String.Map.of_alist_exn [("integrate_ode", "integrate_ode_rk45")]

let rec map_fn_names s =
  let rec map_fn_names_expr e =
    let pattern =
      Expr.Fixed.(
        match e.pattern with
        | FunApp (StanLib (f, sfx, mem_pattern), a) when Map.mem fn_name_map f
          ->
            Pattern.FunApp
              (StanLib (Map.find_exn fn_name_map f, sfx, mem_pattern), a)
        | expr -> Pattern.map map_fn_names_expr expr)
    in
    {e with pattern}
  in
  let stmt =
    Stmt.Fixed.(
      match s.pattern with
      | NRFunApp (StanLib (f, sfx, mem_pattern), a) when Map.mem fn_name_map f
        ->
          Pattern.NRFunApp
            (StanLib (Map.find_exn fn_name_map f, sfx, mem_pattern), a)
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
      { Fixed.pattern= FunApp (StanLib ("print", FnPlain, AoS), args)
      ; meta= Typed.Meta.empty }
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
  (* name mangling of c++ keywords*)
  let rec map_stmt {Stmt.Fixed.pattern; meta} =
    { Stmt.Fixed.pattern=
        Stmt.Fixed.Pattern.map translate_funapps_and_kwrds map_stmt pattern
    ; meta }
  in
  let rename_kwrds (s, e) = (add_prefix_to_kwrds s, e) in
  let rename_fdarg (e1, s, e2) = (e1, add_prefix_to_kwrds s, e2) in
  let rename_func (s : 'a Program.fun_def) =
    { s with
      fdname= add_prefix_to_kwrds s.fdname
    ; fdargs= List.map ~f:rename_fdarg s.fdargs }
  in
  let p =
    Program.(
      { p with
        output_vars= List.map ~f:rename_kwrds p.output_vars
      ; input_vars= List.map ~f:rename_kwrds p.input_vars
      ; functions_block= List.map ~f:rename_func p.functions_block }
      |> map translate_funapps_and_kwrds map_stmt
      |> map Fn.id change_kwrds_stmts)
  in
  let p = Program.map Fn.id map_fn_names p in
  let init_pos =
    [ Stmt.Fixed.Pattern.Decl
        { decl_adtype= DataOnly
        ; decl_id= pos
        ; decl_type= Sized SInt
        ; initialize= true }
    ; Assignment ((pos, UInt, []), Expr.Helpers.loop_bottom) ]
    |> List.map ~f:(fun pattern ->
           Stmt.Fixed.{pattern; meta= Location_span.empty} )
  in
  let param_writes, tparam_writes, gq_writes =
    List.map p.output_vars ~f:(fun (name, outvar) ->
        (outvar.Program.out_block, gen_unconstrained_write (name, outvar)) )
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
              FunApp (_, [{pattern= Var "emit_generated_quantities__"; _}]); _
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
    |> insert_before tparam_start param_writes
    |> insert_before gq_start tparam_writes_cond )
    @ gq_writes
  in
  let log_prob =
    p.log_prob |> add_reads p.output_vars param_read |> translate_to_open_cl
  in
  let opencl_vars =
    String.Set.union_list
      (List.concat_map
         ~f:(List.map ~f:collect_opencl_vars)
         [log_prob; generate_quantities])
    |> String.Set.to_list
  in
  let maybe_add_opencl_events_clear =
    let event_clear_stmt x =
      Stmt.Fixed.
        { pattern= NRFunApp (CompilerInternal (FnReadWriteEventsOpenCL x), [])
        ; meta= Location_span.empty }
    in
    List.map ~f:event_clear_stmt opencl_vars
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
                  ; decl_type= Type.Unsized type_of_input_var
                  ; initialize= true }
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
      log_prob= log_prob @ maybe_add_opencl_events_clear
    ; prog_name= escape_name p.prog_name
    ; prepare_data=
        init_pos
        @ (p.prepare_data |> add_reads p.input_vars data_read)
        @ to_matrix_cl_stmts
    ; transform_inits=
        init_pos
        @ List.concat_map
            ~f:(data_unconstrain_transform Location_span.empty)
            (List.filter
               ~f:(fun (_, ov) -> ov.Program.out_block = Parameters)
               p.output_vars)
    ; generate_quantities }
  in
  Program.(
    p
    |> map Fn.id ensure_body_in_block
    |> map_prog_stmt_lists flatten_slists_list)
