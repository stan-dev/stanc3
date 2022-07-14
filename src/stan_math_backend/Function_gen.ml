(** Code generation for user defined functions and the relevant functors *)

open Core_kernel
open Core_kernel.Poly
open Middle
open Fmt
open Expression_gen
open Statement_gen

type template_parameter =
  | Typename of string  (** The name of a template typename *)
  | Require of string * string
      (** A C++ type trait and the name which needs to satisfy that *)
  | Bool of string  (** A named boolean template type *)
[@@deriving sexp]

let pp_template_parameter ppf template_parameter =
  match template_parameter with
  | Typename param_name -> pf ppf "typename %s" param_name
  | Require (requirement, param_name) ->
      pf ppf "stan::require_t<%s<%s>>*" requirement param_name
  | Bool param_name -> pf ppf "bool %s" param_name

let pp_requires ~default ppf requires =
  match requires with
  | [] -> ()
  | _ ->
      let pp_require ppf (trait, name) = pf ppf "%s<%s>" trait name in
      pf ppf ",@ stan::require_all_t<@[%a@]>*%s"
        (list ~sep:comma pp_require)
        requires
        (if default then " = nullptr" else "")

(**
   Pretty print a full C++ `template <parameter-list>`
  *)
let pp_template ~default ppf template_parameters =
  match template_parameters with
  | [] -> ()
  | _ ->
      let templates, requires =
        List.partition_map template_parameters ~f:(function
          | Require (trait, name) -> Second (trait, name)
          | Typename name -> First ("typename " ^ name)
          | Bool name -> First ("bool " ^ name) ) in
      pf ppf "template <@[%a%a@]>@ " (list ~sep:comma string) templates
        (pp_requires ~default) requires

type found_functor =
  { struct_template: template_parameter option
  ; arg_templates: template_parameter list
  ; signature: string
  ; defn: string }

let rec pp_unsizedtype_custom_scalar_eigen_exprs ppf (scalar, ut) =
  match ut with
  | UnsizedType.UInt | UReal | UMatrix | URowVector | UVector | UComplexVector
   |UComplexMatrix | UComplexRowVector ->
      string ppf scalar
  | UComplex -> pf ppf "std::complex<%s>" scalar
  | UArray t when UnsizedType.contains_tuple t ->
      (* TUPLE TODO: still not sure about these, need to print with scalar *)
      (* Expressions are not accepted for arrays of Eigen::Matrix *)
      pf ppf "std::vector<%a>" pp_unsizedtype_custom_scalar_eigen_exprs
        (scalar, t)
  | UArray t ->
      (* Expressions are not accepted for arrays of Eigen::Matrix *)
      pf ppf "std::vector<%a>" pp_unsizedtype_custom_scalar (scalar, t)
  | UTuple _ -> pf ppf "std::tuple<%s>" scalar
  | UMathLibraryFunction | UFun _ ->
      Common.FatalError.fatal_error_msg
        [%message "Function types not implemented"]

let pp_arg_type ppf (custom_scalar, ut) =
  let scalar =
    match custom_scalar with None -> stantype_prim_str ut | Some s -> s in
  pf ppf "const %a&" pp_unsizedtype_custom_scalar_eigen_exprs (scalar, ut)

(** Print the type of an object.
  @param ppf A pretty printer
  @param custom_scalar_opt A string representing a types inner scalar value.
  @param name The name of the object
  @param ut The unsized type of the object
 *)
let pp_arg_eigen_suffix ~is_possibly_eigen_expr ppf (type_, (_, name, ut)) =
  (* we add the _arg suffix for any Eigen types *)
  let opt_arg_suffix =
    if UnsizedType.is_eigen_type ut && is_possibly_eigen_expr then
      name ^ "_arg__"
    else name in
  pf ppf "%s %s" type_ opt_arg_suffix

let requires ut t =
  let tmpl = List.hd_exn t in
  match ut with
  | UnsizedType.URowVector ->
      [ Require ("stan::is_row_vector", tmpl)
      ; Require ("stan::is_vt_not_complex", tmpl) ]
  | UComplexRowVector ->
      [ Require ("stan::is_row_vector", tmpl)
      ; Require ("stan::is_vt_complex", tmpl) ]
  | UVector ->
      [ Require ("stan::is_col_vector", tmpl)
      ; Require ("stan::is_vt_not_complex", tmpl) ]
  | UComplexVector ->
      [ Require ("stan::is_col_vector", tmpl)
      ; Require ("stan::is_vt_complex", tmpl) ]
  | UMatrix ->
      [ Require ("stan::is_eigen_matrix_dynamic", tmpl)
      ; Require ("stan::is_vt_not_complex", tmpl) ]
  | UComplexMatrix ->
      [ Require ("stan::is_eigen_matrix_dynamic", tmpl)
      ; Require ("stan::is_vt_complex", tmpl) ]
  (* NB: Not unwinding array types due to the way arrays of eigens are printed *)
  | _ -> [Require ("stan::is_stan_scalar", tmpl)]

(** Print template arguments for C++ functions that need templates
  @param args A pack of [Program.fun_arg_decl] containing functions to detect templates.
  @return A list of arguments with template parameter names added.
 *)
let template_parameters (args : Program.fun_arg_decl) =
  let arg_name = str "%a" pp_arg_type in
  (* TODO also return type to print, like eigen::whatever or T1*)
  let rec template_p start i (ad, typ) =
    match (ad, typ) with
    | _, t when UnsizedType.is_int_type t -> ([], [], arg_name (None, typ))
    | _, ut when UnsizedType.contains_tuple ut -> (
        let internal, _ = UnsizedType.unwind_array_type ut in
        match internal with
        | UTuple tys ->
            let temps, reqs, sclrs =
              List.map ~f:(fun ty -> (ad, ty)) tys
              |> List.mapi ~f:(template_p (sprintf "%s%d__" start i))
              |> List.unzip3 in
            let templates = List.concat temps in
            let requires = List.concat reqs in
            let scalars = String.concat ~sep:", " sclrs in
            (templates, requires, arg_name (Some scalars, typ))
        | _ -> failwith "bad 4" )
    | UnsizedType.DataOnly, ut when not (UnsizedType.is_eigen_type ut) ->
        ([], [], arg_name (None, typ))
    | _ ->
        let template = sprintf "%s%d__" start i in
        ([template], requires typ [template], arg_name (Some template, typ))
  in
  List.mapi args ~f:(fun i (ad, _, ty) -> template_p "T" i (ad, ty))

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
        | _ -> failwith "bad 5" )
    | UnsizedType.DataOnly, ut when not (UnsizedType.is_eigen_type ut) -> []
    | _ ->
        if UnsizedType.is_eigen_type typ then
          [sprintf "stan::base_type_t<%s%d__>" start i]
        else [sprintf "%s%d__" start i] in
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
  type_ |> String.concat ~sep:"," |> print_endline ;
  [%expect
    {|
    T0__0__,T0__1__
    ((Require stan::is_stan_scalar T0__0__)
     (Require stan::is_eigen_matrix_dynamic T0__1__)
     (Require stan::is_vt_not_complex T0__1__))
    const std::tuple<const T0__0__&, const T0__1__&>& |}]

let%expect_test "arg types tuple template" =
  let templates, reqs, type_ =
    [(AutoDiffable, "xreal", UArray (UTuple [UArray UInt; UMatrix]))]
    |> template_parameters |> List.unzip3 in
  templates |> List.concat |> String.concat ~sep:"," |> print_endline ;
  reqs |> List.concat |> List.sexp_of_t sexp_of_template_parameter |> print_s ;
  type_ |> String.concat ~sep:"," |> print_endline ;
  [%expect
    {|
  T0__1__
  ((Require stan::is_eigen_matrix_dynamic T0__1__)
   (Require stan::is_vt_not_complex T0__1__))
  const std::vector<std::tuple<const std::vector<int>&, const T0__1__&>>& |}]

(** Print the code for promoting stan real types
  @param ppf A pretty printer$
  @param args A pack of arguments to detect whether they need to use the promotion rules.
 *)
let pp_promoted_scalar ppf args =
  match args with
  | [] -> pf ppf "double"
  | _ ->
      let rec promote_args_chunked ppf args =
        let chunk_till_empty ppf list_tail =
          match list_tail with
          | [] -> ()
          | _ -> pf ppf ",@ %a" promote_args_chunked list_tail in
        match args with
        | [] -> pf ppf "double"
        | hd :: list_tail ->
            pf ppf "@[stan::promote_args_t<@[%a%a@]>@]" (list ~sep:comma string)
              hd chunk_till_empty list_tail in
      promote_args_chunked ppf
        List.(chunks_of ~length:5 (concat (return_optional_arg_types args)))

(** Pretty-prints a function's return-type, taking into account templated argument
  promotion.*)
let pp_returntype ppf arg_types rt =
  let scalar = str "@[%a@]" pp_promoted_scalar arg_types in
  match rt with
  | Some ut when UnsizedType.is_int_type ut ->
      pf ppf "%a@ " pp_unsizedtype_custom_scalar ("int", ut)
  | Some ut -> pf ppf "%a@ " pp_unsizedtype_custom_scalar (scalar, ut)
  | None -> pf ppf "void@ "

let pp_eigen_arg_to_ref ppf arg_types =
  let pp_ref ppf name =
    pf ppf "@[<hv 8>const auto& %s = stan::math::to_ref(%s);@]" name
      (name ^ "_arg__") in
  pf ppf "@[<v>%a@]@ " (list ~sep:cut pp_ref)
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
    string list * template_parameter list * string list =
  let arg_type_templates, require_arg_templates, arg_types =
    template_parameters fdargs |> List.unzip3 in
  ( List.concat arg_type_templates
  , List.concat require_arg_templates
  , List.map2_exn
      ~f:(fun type_string arg ->
        str "%a" (pp_arg_eigen_suffix ~is_possibly_eigen_expr) (type_string, arg)
        )
      arg_types fdargs )

let mk_extra_args templates args =
  List.map ~f:(fun (t, v) -> t ^ "& " ^ v) (List.zip_exn templates args)

(**
  Prints boilerplate at start of function. Body of function wrapped in a `try` block.
 *)
let pp_fun_body fdargs fdsuffix ppf (Stmt.Fixed.{pattern; _} as fdbody) =
  pf ppf "@[<hv 8>using local_scalar_t__ =@ %a;@]@," pp_promoted_scalar fdargs ;
  pf ppf "int current_statement__ = 0; @ " ;
  if List.exists ~f:(fun (_, _, ut) -> UnsizedType.is_eigen_type ut) fdargs then
    pp_eigen_arg_to_ref ppf fdargs ;
  ( match fdsuffix with
  | Fun_kind.FnLpdf _ | FnTarget -> ()
  | FnPlain | FnRng ->
      pf ppf "%s@ " "static constexpr bool propto__ = true;" ;
      pf ppf "%s@ " "(void) propto__;" ) ;
  pf ppf "%s@ "
    "local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());" ;
  pf ppf "%a" pp_unused "DUMMY_VAR__" ;
  let blocked_fdbody =
    match pattern with
    | SList stmts -> {fdbody with pattern= Block stmts}
    | Block _ -> fdbody
    | _ -> {fdbody with pattern= Block [fdbody]} in
  pp_located_error ppf (pp_statement, blocked_fdbody) ;
  pf ppf "@ "

(**
    Functor to generate a pretty printer for the function signature.
   *)
let gen_pp_sig fdargs fdrt extra_templates extra ppf (name, args, variadic) =
  Format.open_vbox 2 ;
  pp_returntype ppf fdargs fdrt ;
  let args, variadic_args =
    match variadic with
    | `ReduceSum -> List.split_n args 3
    | `VariadicODE -> List.split_n args 2
    | `VariadicDAE -> List.split_n args 3
    | `None -> (args, []) in
  let arg_strs =
    args
    @ mk_extra_args extra_templates extra
    @ ["std::ostream* pstream__"]
    @ variadic_args in
  pf ppf "%s(@[<hov>%a@]) " name (list ~sep:comma string) arg_strs ;
  Format.close_box ()

(** Print the C++ function definition.
  @param ppf A pretty printer
  Refactor this please - one idea might be to have different functions for
   printing user defined distributions vs rngs vs regular functions.
 *)
let pp_fun_def ppf
    ( Program.{fdrt; fdname; fdsuffix; fdargs; fdbody; _}
    , (functors : (string, found_functor list) Hashtbl.t)
    , (forward_decls : (string * template_parameter list) Hash_set.t)
    , (funs_used_in_reduce_sum : String.Set.t)
    , (funs_used_in_variadic_ode : String.Set.t)
    , (funs_used_in_variadic_dae : String.Set.t) ) =
  let extra, template_extra_params =
    match fdsuffix with
    | Fun_kind.FnTarget -> (["lp__"; "lp_accum__"], ["T_lp__"; "T_lp_accum__"])
    | FnRng -> (["base_rng__"], ["RNG"])
    | FnLpdf _ | FnPlain -> ([], []) in
  let template_parameter_and_arg_names is_possibly_eigen_expr variadic_fun_type
      =
    let template_param_names, template_require_checks, args =
      templates_and_args is_possibly_eigen_expr fdargs in
    let template_params =
      List.(map ~f:typename (template_param_names @ template_extra_params))
      @ template_require_checks in
    match (fdsuffix, variadic_fun_type) with
    | (FnLpdf _ | FnTarget), `None -> (Bool "propto__" :: template_params, args)
    | _ -> (template_params, args) in
  let template_params, templated_args =
    template_parameter_and_arg_names true `None in
  let pp_fun_sig = gen_pp_sig fdargs fdrt template_extra_params extra in
  let signature = str "%a" pp_fun_sig (fdname, templated_args, `None) in
  (* We want to print the [* = nullptr] at most once, and preferrably on a forward decl *)
  let template_parameter_default_values =
    Option.is_none fdbody
    || not (Hash_set.mem forward_decls (signature, template_params)) in
  pf ppf "%a%a"
    (pp_template ~default:template_parameter_default_values)
    template_params pp_fun_sig
    (fdname, templated_args, `None) ;
  match fdbody with
  | None ->
      pf ppf ";@ " ;
      (* Side Effect: *)
      Hash_set.add forward_decls (signature, template_params)
  | Some fdbody ->
      pp_block ppf (pp_fun_body fdargs fdsuffix, fdbody) ;
      let register_functor (str_args, args, variadic_fun_type) =
        let suffix =
          match variadic_fun_type with
          | `None -> functor_suffix
          | `ReduceSum -> reduce_sum_functor_suffix
          | `VariadicODE -> variadic_ode_functor_suffix
          | `VariadicDAE -> variadic_dae_functor_suffix in
        let functor_name = fdname ^ suffix in
        let struct_template =
          match (fdsuffix, variadic_fun_type) with
          | FnLpdf _, `ReduceSum -> Some (Bool "propto__")
          | _ -> None in
        let arg_templates, templated_args =
          template_parameter_and_arg_names false variadic_fun_type in
        let op_signature =
          str "%a" pp_fun_sig ("operator()", templated_args, variadic_fun_type)
        in
        let operator_paren_sig =
          str "%a@ const@,{@.  return %a;@.}@." pp_fun_sig
            ( functor_name
              ^ (if struct_template <> None then "<propto__>" else "")
              ^ "::operator()"
            , templated_args
            , variadic_fun_type )
            pp_call_str
            ( ( match fdsuffix with
              | FnLpdf _ | FnTarget -> fdname ^ "<propto__>"
              | _ -> fdname )
            , str_args
              @ List.map ~f:(fun (_, name, _) -> name) args
              @ extra @ ["pstream__"] ) in
        (* Side Effect: *)
        Hashtbl.add_multi functors ~key:functor_name
          ~data:
            { struct_template
            ; arg_templates
            ; signature= op_signature
            ; defn= operator_paren_sig } in
      register_functor ([], fdargs, `None) ;
      if String.Set.mem funs_used_in_reduce_sum fdname then
        (* Produces the reduce_sum functors that has the pstream argument
           as the third and not last argument *)
        match fdargs with
        | (_, slice, _) :: (_, start, _) :: (_, end_, _) :: rest ->
            register_functor
              ([slice; start ^ " + 1"; end_ ^ " + 1"], rest, `ReduceSum)
        | _ ->
            Common.FatalError.fatal_error_msg
              [%message
                "Ill-formed reduce_sum call!" (fdargs : Program.fun_arg_decl)]
      else if String.Set.mem funs_used_in_variadic_ode fdname then
        (* Produces the variadic ode functors that has the pstream argument
           as the third and not last argument *)
        register_functor ([], fdargs, `VariadicODE)
      else if String.Set.mem funs_used_in_variadic_dae fdname then
        (* Produces the variadic DAE functors that has the pstream argument
           as the fourth and not last argument *)
        register_functor ([], fdargs, `VariadicDAE)

let pp_standalone_fun_def namespace_fun ppf
    Program.{fdname; fdsuffix; fdargs; fdbody; fdrt; _} =
  let extra, extra_templates =
    match fdsuffix with
    | Fun_kind.FnTarget ->
        (["lp__"; "lp_accum__"], ["double"; "stan::math::accumulator<double>"])
    | FnRng -> (["base_rng__"], ["boost::ecuyer1988"])
    | FnLpdf _ | FnPlain -> ([], []) in
  let args =
    List.map
      ~f:(fun (_, name, ut) ->
        str "const %a& %s" pp_unsizedtype_custom_scalar
          (stantype_prim_str ut, ut)
          name )
      fdargs in
  let pp_sig_standalone ppf _ =
    let arg_strs =
      args
      @ mk_extra_args extra_templates extra
      @ ["std::ostream* pstream__ = nullptr"] in
    pf ppf "(@[<hov>%a@]) " (list ~sep:comma string) arg_strs in
  let mark_function_comment = "// [[stan::function]]" in
  let return_type = match fdrt with None -> "void" | _ -> "auto" in
  let return_stmt = match fdrt with None -> "" | _ -> "return " in
  match fdbody with
  | None -> pf ppf ";@ "
  | Some _ ->
      pf ppf "@,%s@,%s %s%a @,{@, %s%s::%a;@,}@," mark_function_comment
        return_type fdname pp_sig_standalone "" return_stmt namespace_fun
        pp_call_str
        ( ( match fdsuffix with
          | FnLpdf _ | FnTarget -> fdname ^ "<false>"
          | FnRng | FnPlain -> fdname )
        , List.map ~f:(fun (_, name, _) -> name) fdargs @ extra @ ["pstream__"]
        )

let is_fun_used_with_variadic_fn (variadic_fn_test : string -> bool)
    (p : Program.Numbered.t) =
  let rec find_functors_expr accum Expr.Fixed.{pattern; _} =
    String.Set.union accum
      ( match pattern with
      | FunApp (StanLib (x, FnPlain, _), {pattern= Var f; _} :: _)
        when variadic_fn_test x ->
          String.Set.of_list [Utils.stdlib_distribution_name f]
      | x -> Expr.Fixed.Pattern.fold find_functors_expr accum x ) in
  let rec find_functors_stmt accum stmt =
    Stmt.Fixed.(
      Pattern.fold find_functors_expr find_functors_stmt accum stmt.pattern)
  in
  Program.fold find_functors_expr find_functors_stmt String.Set.empty p

let collect_functors_functions (p : Program.Numbered.t) =
  let (functors : (string, found_functor list) Hashtbl.t) =
    String.Table.create () in
  let forward_decls = Hash_set.Poly.create () in
  let reduce_sum_fns =
    is_fun_used_with_variadic_fn Stan_math_signatures.is_reduce_sum_fn p in
  let variadic_ode_fns =
    is_fun_used_with_variadic_fn Stan_math_signatures.is_variadic_ode_fn p in
  let variadic_dae_fns =
    is_fun_used_with_variadic_fn Stan_math_signatures.is_variadic_dae_fn p in
  let pp_fun_def_with_variadic_fn_list ppf fblock =
    (hovbox ~indent:2 pp_fun_def)
      ppf
      ( fblock
      , functors
      , forward_decls
      , reduce_sum_fns
      , variadic_ode_fns
      , variadic_dae_fns ) in
  ( str "@[<v>%a@]"
      (list ~sep:cut pp_fun_def_with_variadic_fn_list)
      p.functions_block
  , functors )

let pp_functions_functors ppf (p : Program.Numbered.t) =
  let fns_str, functors = collect_functors_functions p in
  let pp_functor_decls ppf tbl =
    Hashtbl.iteri tbl ~f:(fun ~key ~data ->
        pf ppf "@[<v 2>%astruct %s {@,%aconst;@]@,};@."
          (option
             (option (fun ppf template_param ->
                  pf ppf "template <%a>@ " pp_template_parameter template_param )
             ) )
          (Option.map
             ~f:(fun functor_t -> functor_t.struct_template)
             (List.hd data) )
          key
          (list ~sep:(any "const;@,") (fun ppf (template_parameters, sign) ->
               pf ppf "%a@[<h>%a@]"
                 (pp_template ~default:true)
                 template_parameters text sign ) )
          (List.map
             ~f:(fun {arg_templates; signature; _} -> (arg_templates, signature))
             data ) ) in
  let pp_functors ppf functor_tbl =
    Hashtbl.iter functor_tbl ~f:(fun data ->
        List.iter data ~f:(fun {struct_template; defn; arg_templates; _} ->
            pf ppf "%a%a%s@."
              (option (fun ppf template_param ->
                   pf ppf "template <%a>@ " pp_template_parameter template_param )
              )
              struct_template
              (pp_template ~default:false)
              arg_templates defn ) ) in
  pf ppf "%a@ %s@ %a" pp_functor_decls functors fns_str pp_functors functors

(* Testing code *)

let pp_fun_def_w_rs a b =
  pp_fun_def a
    ( b
    , String.Table.create ()
    , Hash_set.Poly.create ()
    , String.Set.empty
    , String.Set.empty
    , String.Set.empty )

let%expect_test "udf" =
  let with_no_loc stmt =
    Stmt.Fixed.{pattern= stmt; meta= Locations.no_span_num} in
  let w e = Expr.{Fixed.pattern= e; meta= Typed.Meta.empty} in
  { fdrt= None
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
  |> str "@[<v>%a" pp_fun_def_w_rs
  |> print_endline ;
  [%expect
    {|
    template <typename T0__, typename T1__,
              stan::require_all_t<stan::is_eigen_matrix_dynamic<T0__>,
                                  stan::is_vt_not_complex<T0__>,
                                  stan::is_row_vector<T1__>,
                                  stan::is_vt_not_complex<T1__>>* = nullptr>
    void
    sars(const T0__& x_arg__, const T1__& y_arg__, std::ostream* pstream__) {
      using local_scalar_t__ =
              stan::promote_args_t<stan::base_type_t<T0__>,
                                   stan::base_type_t<T1__>>;
      int current_statement__ = 0;
      const auto& x = stan::math::to_ref(x_arg__);
      const auto& y = stan::math::to_ref(y_arg__);
      static constexpr bool propto__ = true;
      (void) propto__;
      local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
      (void) DUMMY_VAR__;  // suppress unused var warning
      try {
        return stan::math::add(x, 1);
      } catch (const std::exception& e) {
        stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      }

    } |}]

let%expect_test "udf-expressions" =
  let with_no_loc stmt =
    Stmt.Fixed.{pattern= stmt; meta= Locations.no_span_num} in
  let w e = Expr.{Fixed.pattern= e; meta= Typed.Meta.empty} in
  { fdrt= Some UMatrix
  ; fdname= "sars"
  ; fdsuffix= FnPlain
  ; fdargs=
      [ (DataOnly, "x", UMatrix); (AutoDiffable, "y", URowVector)
      ; (AutoDiffable, "z", URowVector); (AutoDiffable, "w", UArray UMatrix) ]
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
  |> str "@[<v>%a" pp_fun_def_w_rs
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
    Eigen::Matrix<stan::promote_args_t<stan::base_type_t<T0__>, stan::base_type_t<T1__>,
                         stan::base_type_t<T2__>, T3__>, -1, -1>
    sars(const T0__& x_arg__, const T1__& y_arg__, const T2__& z_arg__,
         const std::vector<Eigen::Matrix<T3__, -1, -1>>& w,
         std::ostream* pstream__) {
      using local_scalar_t__ =
              stan::promote_args_t<stan::base_type_t<T0__>,
                                   stan::base_type_t<T1__>,
                                   stan::base_type_t<T2__>, T3__>;
      int current_statement__ = 0;
      const auto& x = stan::math::to_ref(x_arg__);
      const auto& y = stan::math::to_ref(y_arg__);
      const auto& z = stan::math::to_ref(z_arg__);
      static constexpr bool propto__ = true;
      (void) propto__;
      local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
      (void) DUMMY_VAR__;  // suppress unused var warning
      try {
        return stan::math::add(x, 1);
      } catch (const std::exception& e) {
        stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      }

    } |}]
