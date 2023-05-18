open Core_kernel
open Middle
open Lower_expr
open Lower_stmt
open Cpp

let rec lower_type_eigen_expr (t : UnsizedType.t) (inner_type : type_) : type_ =
  match t with
  | UInt -> Int
  | UReal | UMatrix | URowVector | UVector | UComplexVector | UComplexMatrix
   |UComplexRowVector | UTuple _ ->
      inner_type
  | UComplex -> Types.complex inner_type
  | UArray t when UnsizedType.contains_tuple t ->
      StdVector (lower_type_eigen_expr t inner_type)
  | UArray t ->
      (* Expressions are not accepted for arrays of Eigen::Matrix *)
      StdVector (lower_type t inner_type)
  | UMathLibraryFunction | UFun _ ->
      Common.FatalError.fatal_error_msg
        [%message "Function types not implemented"]

let arg_type custom_scalar ut =
  let scalar =
    match custom_scalar with None -> stantype_prim ut | Some s -> s in
  lower_type_eigen_expr ut scalar

let lower_arg ~is_possibly_eigen_expr type_ (_, name, ut) =
  (* we add the _arg suffix for any Eigen types *)
  let opt_arg_suffix =
    if is_possibly_eigen_expr && UnsizedType.is_eigen_type ut then
      name ^ "_arg__"
    else name in
  (Types.const_ref type_, opt_arg_suffix)

let requires ut t =
  match ut with
  | UnsizedType.URowVector ->
      [ RequireIs ("stan::is_row_vector", t)
      ; RequireIs ("stan::is_vt_not_complex", t) ]
  | UComplexRowVector ->
      [ RequireIs ("stan::is_row_vector", t)
      ; RequireIs ("stan::is_vt_complex", t) ]
  | UVector ->
      [ RequireIs ("stan::is_col_vector", t)
      ; RequireIs ("stan::is_vt_not_complex", t) ]
  | UComplexVector ->
      [ RequireIs ("stan::is_col_vector", t)
      ; RequireIs ("stan::is_vt_complex", t) ]
  | UMatrix ->
      [ RequireIs ("stan::is_eigen_matrix_dynamic", t)
      ; RequireIs ("stan::is_vt_not_complex", t) ]
  | UComplexMatrix ->
      [ RequireIs ("stan::is_eigen_matrix_dynamic", t)
      ; RequireIs ("stan::is_vt_complex", t) ]
      (* NB: Not unwinding array types due to the way arrays of eigens are printed *)
  | _ -> [RequireIs ("stan::is_stan_scalar", t)]

let return_optional_arg_types (args : Program.fun_arg_decl) =
  let rec template_p start i (ad, typ) =
    match (ad, typ) with
    | _, t when UnsizedType.is_int_type t -> []
    | _, ut when UnsizedType.contains_tuple ut -> (
        let internal, _ = UnsizedType.unwind_array_type ut in
        match internal with
        | UTuple tys ->
            let temps =
              List.map ~f:(fun ty -> (ad, ty)) tys
              |> List.mapi ~f:(template_p (sprintf "%s%d__" start i)) in
            let templates = List.concat temps in
            templates
        | _ ->
            Common.FatalError.fatal_error_msg
              [%message
                "Impossible: type passes UnsizedType.contains_tuple but \
                 unwrapped scalar is not tuple"
                  (typ : UnsizedType.t)
                  (internal : UnsizedType.t)
                  (ad : UnsizedType.autodifftype)] )
    | UnsizedType.DataOnly, ut when not (UnsizedType.is_eigen_type ut) -> []
    | _ ->
        if UnsizedType.is_eigen_type typ then
          [sprintf "stan::base_type_t<%s%d__>" start i]
        else [sprintf "%s%d__" start i] in
  List.mapi args ~f:(fun i (ad, _, ty) -> template_p "T" i (ad, ty))

(** Print template arguments for C++ functions that need templates
  @param args A pack of [Program.fun_arg_decl] containing functions to detect templates.
  @return A list of arguments with template parameter names added.
 *)
let template_parameters (args : Program.fun_arg_decl) =
  let rec template_p start i (ad, typ) =
    match (ad, fst (UnsizedType.unwind_array_type typ)) with
    | _, UInt -> ([], [], arg_type None typ)
    | _, UTuple tys ->
        let temps, reqs, sclrs =
          List.map ~f:(fun ty -> (ad, ty)) tys
          |> List.mapi ~f:(template_p (sprintf "%s%d__" start i))
          |> List.unzip3 in
        let templates = List.concat temps in
        let requires = List.concat reqs in
        let scalar = Tuple sclrs in
        (templates, requires, arg_type (Some scalar) typ)
    | UnsizedType.DataOnly, ut when not (UnsizedType.is_eigen_type ut) ->
        ([], [], arg_type None typ)
    | _ ->
        let template = sprintf "%s%d__" start i in
        ( [template]
        , requires typ template
        , arg_type (Some (TemplateType template)) typ ) in
  List.mapi args ~f:(fun i (ad, _, ty) -> template_p "T" i (ad, ty))

let%expect_test "arg types templated correctly" =
  [(AutoDiffable, "xreal", UReal); (DataOnly, "yint", UInt)]
  |> template_parameters |> List.map ~f:fst3 |> List.concat
  |> String.concat ~sep:"," |> print_endline ;
  [%expect {| T0__ |}]

let%expect_test "arg types tuple template" =
  let templates, reqs, type_ =
    [(AutoDiffable, "xreal", UTuple [UReal; UMatrix])]
    |> template_parameters |> List.unzip3 in
  templates |> List.concat |> String.concat ~sep:"," |> print_endline ;
  reqs |> List.concat |> List.sexp_of_t sexp_of_template_parameter |> print_s ;
  type_
  |> Fmt.to_to_string (Fmt.list ~sep:Fmt.comma Cpp.Printing.pp_type_)
  |> print_endline ;
  [%expect
    {|
    T0__0__,T0__1__
    ((RequireIs stan::is_stan_scalar T0__0__)
     (RequireIs stan::is_eigen_matrix_dynamic T0__1__)
     (RequireIs stan::is_vt_not_complex T0__1__))
    std::tuple<T0__0__, T0__1__> |}]

let%expect_test "arg types tuple template" =
  let templates, reqs, type_ =
    [(AutoDiffable, "xreal", UArray (UTuple [UArray UInt; UMatrix]))]
    |> template_parameters |> List.unzip3 in
  templates |> List.concat |> String.concat ~sep:"," |> print_endline ;
  reqs |> List.concat |> List.sexp_of_t sexp_of_template_parameter |> print_s ;
  type_
  |> Fmt.to_to_string (Fmt.list ~sep:Fmt.comma Cpp.Printing.pp_type_)
  |> print_endline ;
  [%expect
    {|
  T0__1__
  ((RequireIs stan::is_eigen_matrix_dynamic T0__1__)
   (RequireIs stan::is_vt_not_complex T0__1__))
  std::vector<std::tuple<std::vector<int>, T0__1__>> |}]

let lower_promoted_scalar args =
  match args with
  | [] -> Double
  | _ ->
      let rec promote_args_chunked args =
        let chunk_till_empty list_tail =
          match list_tail with [] -> [] | _ -> [promote_args_chunked list_tail]
        in
        match args with
        | [] -> Double
        | hd :: list_tail ->
            TypeTrait ("stan::promote_args_t", hd @ chunk_till_empty list_tail)
      in
      promote_args_chunked
        List.(
          chunks_of ~length:5
            (List.map
               ~f:(fun t -> TemplateType t)
               (concat (return_optional_arg_types args)) ))

(** Pretty-prints a function's return-type, taking into account templated argument
promotion.*)
let lower_returntype arg_types rt =
  let scalar = lower_promoted_scalar arg_types in
  match rt with
  | UnsizedType.ReturnType ut -> lower_type ut scalar
  | Void -> Void

let lower_eigen_args_to_ref arg_types =
  let lower_ref name =
    VariableDefn
      (make_variable_defn ~type_:(Types.const_ref Auto) ~name
         ~init:
           (Assignment
              (Exprs.fun_call "stan::math::to_ref" [Var (name ^ "_arg__")]) )
         () ) in
  List.map ~f:lower_ref
    (List.filter_map
       ~f:(fun (_, name, ut) ->
         if UnsizedType.is_eigen_type ut then Some name else None )
       arg_types )

let typename parameter_name = Typename parameter_name

(** Construct an object with it's needed templates for function signatures.
@param is_possibly_eigen_expr if true, argument can possibly be an unevaluated eigen expression.
@param fdargs A sexp list of strings representing C++ types.
*)
let templates_and_args (is_possibly_eigen_expr : bool)
    (fdargs : Program.fun_arg_decl) :
    string list * template_parameter list * (type_ * string) list =
  let arg_type_templates, require_arg_templates, arg_types =
    template_parameters fdargs |> List.unzip3 in
  ( List.concat arg_type_templates
  , List.concat require_arg_templates
  , List.map2_exn ~f:(lower_arg ~is_possibly_eigen_expr) arg_types fdargs )

(**
Prints boilerplate at start of function. Body of function wrapped in a `try` block.
*)
let lower_fun_body fdargs fdsuffix fdbody =
  let local_scalar =
    Using ("local_scalar_t__", Some (lower_promoted_scalar fdargs)) in
  let to_refs = lower_eigen_args_to_ref fdargs in
  let propto =
    match fdsuffix with
    | Fun_kind.FnLpdf _ | FnTarget -> []
    | FnPlain | FnRng ->
        VariableDefn
          (make_variable_defn ~static:true ~constexpr:true ~type_:Types.bool
             ~name:"propto__" ~init:(Assignment (Literal "true")) () )
        :: Stmts.unused "propto__" in
  let body = lower_statement fdbody in
  (local_scalar :: Decls.current_statement :: to_refs)
  @ propto @ Decls.dummy_var @ Stmts.rethrow_located body

let mk_extra_args templates args =
  List.map
    ~f:(fun (t, v) -> (Ref (TemplateType t), v))
    (List.zip_exn templates args)

let lower_args extra_templates extra args variadic =
  let args, variadic_args =
    match variadic with
    | ReduceSum -> List.split_n args 3
    | VariadicHOF x -> List.split_n args x
    | FixedArgs -> (args, []) in
  let arg_strs =
    args
    @ mk_extra_args extra_templates extra
    @ [(Pointer (TypeLiteral "std::ostream"), "pstream__")]
    @ variadic_args in
  arg_strs

let extra_suffix_args fdsuffix =
  match fdsuffix with
  | Fun_kind.FnTarget -> (["lp__"; "lp_accum__"], ["T_lp__"; "T_lp_accum__"])
  | FnRng -> (["base_rng__"], ["RNG"])
  | FnLpdf _ | FnPlain -> ([], [])

let lower_fun_def (functors : Lower_expr.variadic list)
    Program.{fdrt; fdname; fdsuffix; fdargs; fdbody; _} :
    fun_defn * struct_defn list =
  let extra_arg_names, extra_template_names = extra_suffix_args fdsuffix in
  let template_parameter_and_arg_names is_possibly_eigen_expr variadic_fun_type
      =
    let template_param_names, template_require_checks, args =
      templates_and_args is_possibly_eigen_expr fdargs in
    let template_params =
      List.(map ~f:typename (template_param_names @ extra_template_names))
      @ template_require_checks in
    match (fdsuffix, variadic_fun_type) with
    | (FnLpdf _ | FnTarget), FixedArgs ->
        (Bool "propto__" :: template_params, args)
    | _ -> (template_params, args) in
  let template_params, templated_args =
    template_parameter_and_arg_names true FixedArgs in
  let cpp_arg_gen = lower_args extra_template_names extra_arg_names in
  let cpp_args = cpp_arg_gen templated_args FixedArgs in
  let almost_fn =
    make_fun_defn
      ~templates_init:([template_params], true)
      ~name:fdname
      ~return_type:(lower_returntype fdargs fdrt)
      ~args:cpp_args in
  let fd =
    match Option.map ~f:(lower_fun_body fdargs fdsuffix) fdbody with
    | Some body -> almost_fn ~body ()
    | None -> almost_fn () in
  let register_functor variadic_fun_type =
    let suffix = Lower_expr.functor_suffix_select variadic_fun_type in
    let functor_name = fdname ^ suffix in
    let struct_template =
      match (fdsuffix, variadic_fun_type) with
      | FnLpdf _, ReduceSum -> Some (Bool "propto__")
      | _ -> None in
    let arg_templates, templated_args =
      template_parameter_and_arg_names false variadic_fun_type in
    let cpp_args = cpp_arg_gen templated_args variadic_fun_type in
    let defn_template =
      match fdsuffix with
      | FnLpdf _ | FnTarget -> [TemplateType "propto__"]
      | _ -> [] in
    let defn_args =
      List.map ~f:Exprs.to_var
        (List.map ~f:snd templated_args @ extra_arg_names @ ["pstream__"]) in
    let defn_args =
      match (variadic_fun_type, defn_args) with
      | ReduceSum, slice :: start :: end_ :: rest ->
          slice :: plus_one start :: plus_one end_ :: rest
      | _ -> defn_args in
    let defn_body =
      [Return (Some (Exprs.templated_fun_call fdname defn_template defn_args))]
    in
    let functor_decl =
      make_fun_defn
        ~templates_init:([arg_templates], true)
        ~name:"operator()"
        ~return_type:(lower_returntype fdargs fdrt)
        ~args:cpp_args ~cv_qualifiers:[Const] ~body:defn_body () in
    make_struct_defn ~param:struct_template ~name:functor_name
      ~body:[FunDef functor_decl] () in
  (fd, functors |> List.map ~f:register_functor)

let get_functor_requirements (p : Program.Numbered.t) =
  let open Expr.Fixed in
  let rec find_functors_expr init = function
    | {pattern= FunApp (StanLib (hof, FnPlain, _), args); _} ->
        let f accum = function
          | { pattern= Var name
            ; meta= {Expr.Typed.Meta.type_= UnsizedType.UFun (args, _, _, _); _}
            } ->
              Map.add_multi accum
                ~key:(Utils.stdlib_distribution_name name)
                ~data:(Lower_expr.functor_type hof, List.map ~f:snd args)
          | e -> find_functors_expr accum e in
        List.fold ~init ~f args
    | {pattern; _} -> Pattern.fold find_functors_expr init pattern in
  let rec find_functors_stmt accum stmt =
    Stmt.Fixed.(
      Pattern.fold find_functors_expr find_functors_stmt accum stmt.pattern)
  in
  Program.fold find_functors_expr find_functors_stmt Fn.const String.Map.empty p

let collect_functors_functions (p : Program.Numbered.t) : defn list =
  let functor_required = get_functor_requirements p in
  (* overloaded functions generate only one functor struct per name *)
  let structs = String.Table.create () in
  let matching_argtypes Program.{fdargs; _} arg_types =
    List.equal UnsizedType.equal
      (List.map ~f:(fun (_, _, t) -> t) fdargs)
      arg_types in
  let register_functors (d : _ Program.fun_def) =
    let functors =
      Map.find_multi functor_required d.fdname
      |> List.stable_dedup
      |> List.filter_map ~f:(fun (hof, types) ->
             if matching_argtypes d types then Some hof else None ) in
    let fn, st = lower_fun_def functors d in
    List.iter st ~f:(fun s ->
        (* Side effecting, collates functor structs *)
        Hashtbl.update structs s.struct_name ~f:(function
          | Some x -> {x with body= x.body @ s.body}
          | None -> s ) ) ;
    fn in
  let fun_decls, fun_defns =
    p.functions_block
    |> List.filter_map ~f:(fun d ->
           let fn = register_functors d in
           if Option.is_none d.fdbody then None
           else
             let decl, defn = Cpp.split_fun_decl_defn fn in
             Some (FunDef decl, FunDef defn) )
    |> List.unzip in
  let structs = Hashtbl.data structs |> List.map ~f:(fun s -> Struct s) in
  fun_decls @ structs @ fun_defns

let lower_standalone_fun_def namespace_fun
    Program.{fdname; fdsuffix; fdargs; fdrt; _} =
  let extra, extra_templates =
    match fdsuffix with
    | Fun_kind.FnTarget ->
        (["lp__"; "lp_accum__"], ["double"; "stan::math::accumulator<double>"])
    | FnRng -> (["base_rng__"], ["boost::ecuyer1988"])
    | FnLpdf _ | FnPlain -> ([], []) in
  let args =
    List.map
      ~f:(fun (_, name, ut) ->
        (Types.const_ref (lower_type ut (stantype_prim ut)), name) )
      fdargs in
  let all_args =
    args
    @ mk_extra_args extra_templates extra
    @ [(Pointer (TypeLiteral "std::ostream"), "pstream__ = nullptr")] in
  let mark_function_comment = GlobalComment "[[stan::function]]" in
  let return_type, return_stmt =
    match fdrt with
    | Void -> (Void, fun e -> Expression e)
    | ReturnType ut -> (lower_type ut Double, fun e -> Return (Some e)) in
  let fn_sig = make_fun_defn ~name:fdname ~return_type ~args:all_args in
  let internal_fname = namespace_fun ^ "::" ^ fdname in
  let template =
    match fdsuffix with
    | FnLpdf _ | FnTarget -> [TypeLiteral "false"]
    | FnRng | FnPlain -> [] in
  let call_args =
    List.map ~f:(fun (_, name, _) -> name) fdargs @ extra @ ["pstream__"]
    |> List.map ~f:Exprs.to_var in
  let ret =
    return_stmt (Exprs.templated_fun_call internal_fname template call_args)
  in
  [mark_function_comment; FunDef (fn_sig ~body:[ret] ())]

module Testing = struct
  (* Testing code *)
  open Middle
  open Fmt

  let pp_fun_def_test ppf a =
    let defn, st = lower_fun_def [FixedArgs] a in
    Cpp.Printing.pp_fun_defn ppf defn ;
    cut ppf () ;
    (list ~sep:cut Cpp.Printing.pp_struct_defn) ppf st

  let%expect_test "udf" =
    let with_no_loc stmt =
      Stmt.Fixed.{pattern= stmt; meta= Numbering.no_span_num} in
    let w e = Expr.{Fixed.pattern= e; meta= Typed.Meta.empty} in
    { fdrt= Void
    ; fdname= "sars"
    ; fdsuffix= FnPlain
    ; fdargs= [(DataOnly, "x", UMatrix); (AutoDiffable, "y", URowVector)]
    ; fdbody=
        Stmt.Fixed.Pattern.Return
          (Some
             ( w
             @@ FunApp
                  ( StanLib ("add", FnPlain, AoS)
                  , [w @@ Var "x"; w @@ Lit (Int, "1")] ) ) )
        |> with_no_loc |> List.return |> Stmt.Fixed.Pattern.Block |> with_no_loc
        |> Some
    ; fdloc= Location_span.empty }
    |> str "@[<v>%a" pp_fun_def_test
    |> print_endline ;
    [%expect
      {|
    template <typename T0__, typename T1__,
              stan::require_all_t<stan::is_eigen_matrix_dynamic<T0__>,
                                  stan::is_vt_not_complex<T0__>,
                                  stan::is_row_vector<T1__>,
                                  stan::is_vt_not_complex<T1__>>* = nullptr>
    void sars(const T0__& x_arg__, const T1__& y_arg__, std::ostream* pstream__) {
      using local_scalar_t__ = stan::promote_args_t<stan::base_type_t<T0__>,
                                 stan::base_type_t<T1__>>;
      int current_statement__ = 0;
      const auto& x = stan::math::to_ref(x_arg__);
      const auto& y = stan::math::to_ref(y_arg__);
      static constexpr bool propto__ = true;
      // suppress unused var warning
      (void) propto__;
      local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
      // suppress unused var warning
      (void) DUMMY_VAR__;
      try {
        return stan::math::add(x, 1);
      } catch (const std::exception& e) {
        stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      }
    }
    struct sars_functor__ {
      template <typename T0__, typename T1__,
                stan::require_all_t<stan::is_eigen_matrix_dynamic<T0__>,
                                    stan::is_vt_not_complex<T0__>,
                                    stan::is_row_vector<T1__>,
                                    stan::is_vt_not_complex<T1__>>* = nullptr>
      void
      operator()(const T0__& x, const T1__& y, std::ostream* pstream__) const {
        return sars(x, y, pstream__);
      }
    }; |}]

  let%expect_test "udf-expressions" =
    let with_no_loc stmt =
      Stmt.Fixed.{pattern= stmt; meta= Numbering.no_span_num} in
    let w e = Expr.{Fixed.pattern= e; meta= Typed.Meta.empty} in
    { fdrt= ReturnType UMatrix
    ; fdname= "sars"
    ; fdsuffix= FnPlain
    ; fdargs=
        [ (DataOnly, "x", UMatrix); (AutoDiffable, "y", URowVector)
        ; (AutoDiffable, "z", URowVector); (AutoDiffable, "w", UArray UMatrix)
        ]
    ; fdbody=
        Stmt.Fixed.Pattern.Return
          (Some
             ( w
             @@ FunApp
                  ( StanLib ("add", FnPlain, AoS)
                  , [w @@ Var "x"; w @@ Lit (Int, "1")] ) ) )
        |> with_no_loc |> List.return |> Stmt.Fixed.Pattern.Block |> with_no_loc
        |> Some
    ; fdloc= Location_span.empty }
    |> str "@[<v>%a" pp_fun_def_test
    |> print_endline ;
    [%expect
      {|
    template <typename T0__, typename T1__, typename T2__, typename T3__,
              stan::require_all_t<stan::is_eigen_matrix_dynamic<T0__>,
                                  stan::is_vt_not_complex<T0__>,
                                  stan::is_row_vector<T1__>,
                                  stan::is_vt_not_complex<T1__>,
                                  stan::is_row_vector<T2__>,
                                  stan::is_vt_not_complex<T2__>,
                                  stan::is_stan_scalar<T3__>>* = nullptr>
    Eigen::Matrix<stan::promote_args_t<stan::base_type_t<T0__>,
                    stan::base_type_t<T1__>, stan::base_type_t<T2__>, T3__>,-1,-1>
    sars(const T0__& x_arg__, const T1__& y_arg__, const T2__& z_arg__,
         const std::vector<Eigen::Matrix<T3__,-1,-1>>& w, std::ostream* pstream__) {
      using local_scalar_t__ = stan::promote_args_t<stan::base_type_t<T0__>,
                                 stan::base_type_t<T1__>,
                                 stan::base_type_t<T2__>, T3__>;
      int current_statement__ = 0;
      const auto& x = stan::math::to_ref(x_arg__);
      const auto& y = stan::math::to_ref(y_arg__);
      const auto& z = stan::math::to_ref(z_arg__);
      static constexpr bool propto__ = true;
      // suppress unused var warning
      (void) propto__;
      local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
      // suppress unused var warning
      (void) DUMMY_VAR__;
      try {
        return stan::math::add(x, 1);
      } catch (const std::exception& e) {
        stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      }
    }
    struct sars_functor__ {
      template <typename T0__, typename T1__, typename T2__, typename T3__,
                stan::require_all_t<stan::is_eigen_matrix_dynamic<T0__>,
                                    stan::is_vt_not_complex<T0__>,
                                    stan::is_row_vector<T1__>,
                                    stan::is_vt_not_complex<T1__>,
                                    stan::is_row_vector<T2__>,
                                    stan::is_vt_not_complex<T2__>,
                                    stan::is_stan_scalar<T3__>>* = nullptr>
      Eigen::Matrix<stan::promote_args_t<stan::base_type_t<T0__>,
                      stan::base_type_t<T1__>, stan::base_type_t<T2__>, T3__>,-1,-1>
      operator()(const T0__& x, const T1__& y, const T2__& z,
                 const std::vector<Eigen::Matrix<T3__,-1,-1>>& w, std::ostream*
                 pstream__) const {
        return sars(x, y, z, w, pstream__);
      }
    }; |}]
end
