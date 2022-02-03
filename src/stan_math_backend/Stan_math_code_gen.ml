(** Generate C++ from the MIR.

    This module makes extensive use of the Format[0] module via the Fmt[1] API.
    As such, you'll need to understand the "%a" and "@" notation from [0], especially
    the section headed "Formatted pretty-printing." Then, we use functions like
    [pf] and [str] from the Fmt library[1]. On top of that, the "@" pretty-printing
    specifiers all actually correspond 1-to-1 with commands like [open_box] from
    the Format library[0]. The boxing system is best described in this explainer
    pdf [2], particularly Section 3 ("Format basics"). It's worth noting that someone
    was able to make a decent-looking pretty-printer for a subset of Javascript[3] that
    might serve as a good reference. Good luck!

    [0] Format module doc: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
    [1] Fmt module doc: https://erratique.ch/software/fmt/doc/Fmt.html
    [2] Format Unraveled: https://hal.archives-ouvertes.fr/hal-01503081/file/format-unraveled.pdf
    [3] Javascript pretty-printer https://github.com/Virum/compiler/blob/28e807b842bab5dcf11460c8193dd5b16674951f/JavaScript.ml#L112
*)

open Core_kernel
open Core_kernel.Poly
open Middle
open Fmt
open Expression_gen
open Statement_gen

(* TODO: move to seperate file and code gen more like this with a structured type *)
type template =
  | Typename of string
  | Require of string * string
  | Bool of string

let pp_template ppf template =
  match template with
  | Typename t -> pf ppf "typename %s" t
  | Require (r, t) -> pf ppf "%s<%s>*" r t
  | Bool s -> pf ppf "bool %s" s

let pp_template_defaults ppf template =
  match template with
  | Require _ -> pf ppf "%a = nullptr" pp_template template
  | _ -> pp_template ppf template

let pp_templates ~defaults ppf templates =
  match templates with
  | [] -> ()
  | _ ->
      pf ppf "template <@[%a@]>@ "
        (list ~sep:comma
           (if defaults then pp_template_defaults else pp_template) )
        templates

type found_functor =
  { struct_template: template option
  ; arg_templates: template list
  ; signature: string
  ; defn: string }

let standalone_functions = ref false

let stanc_args_to_print =
  let sans_model_and_hpp_paths x =
    not
      String.(
        is_suffix ~suffix:".stan" x
        && not (is_prefix ~prefix:"--filename-in-msg" x)
        || is_prefix ~prefix:"--o" x) in
  (* Ignore the "--o" arg, the stan file and the binary name (bin/stanc). *)
  Array.to_list Sys.argv |> List.tl_exn
  |> List.filter ~f:sans_model_and_hpp_paths
  |> String.concat ~sep:" "

let pp_unused = fmt "(void) %s;  // suppress unused var warning"

(** Print name of model function.
  @param prog_name Name of the Stan program.
  @param fname Name of the function.
 *)
let pp_function__ ppf (prog_name, fname) =
  pf ppf
    {|@[<v>static constexpr const char* function__ = "%s_namespace::%s";@,%a@]|}
    prog_name fname pp_unused "function__"

(** Print the body of exception handling for functions *)
let pp_located ppf _ =
  pf ppf
    {|stan::lang::rethrow_located(e, locations_array__[current_statement__]);|}

(** Detect if argument requires C++ template *)
let arg_needs_template = function
  | UnsizedType.DataOnly, _, t -> UnsizedType.is_eigen_type t
  | _, _, t when UnsizedType.is_int_type t -> false
  | _ -> true

(** Print template arguments for C++ functions that need templates
  @param args A pack of [Program.fun_arg_decl] containing functions to detect templates.
  @return A list of arguments with template parameter names added.
 *)
let maybe_templated_arg_types (args : Program.fun_arg_decl) =
  List.mapi args ~f:(fun i a ->
      match arg_needs_template a with
      | true -> Some (sprintf "T%d__" i)
      | false -> None )

let maybe_require_templates (names : string option list)
    (args : Program.fun_arg_decl) =
  let require_for_arg arg =
    match trd3 arg with
    | UnsizedType.URowVector -> "stan::require_row_vector_t"
    | UVector -> "stan::require_col_vector_t"
    | UMatrix -> "stan::require_eigen_matrix_dynamic_t"
    (* NB: Not unwinding array types due to the way arrays of eigens are printed *)
    | _ -> "stan::require_stan_scalar_t" in
  List.map2_exn names args ~f:(fun name a ->
      match name with
      | Some t -> Some (Require (require_for_arg a, t))
      | None -> None )

let return_arg_types (args : Program.fun_arg_decl) =
  List.mapi args ~f:(fun i ((_, _, ut) as a) ->
      if UnsizedType.is_eigen_type ut && arg_needs_template a then
        Some (sprintf "stan::value_type_t<T%d__>" i)
      else if arg_needs_template a then Some (sprintf "T%d__" i)
      else None )

let%expect_test "arg types templated correctly" =
  [(AutoDiffable, "xreal", UReal); (DataOnly, "yint", UInt)]
  |> maybe_templated_arg_types |> List.filter_opt |> String.concat ~sep:","
  |> print_endline ;
  [%expect {| T0__ |}]

(** Print the code for promoting stan real types
 @param ppf A pretty printer
 @param args A pack of arguments to detect whether they need to use the promotion rules.
 *)
let pp_promoted_scalar ppf args =
  match args with
  | [] -> pf ppf "double"
  | _ ->
      let rec promote_args_chunked ppf args =
        let go ppf tl =
          match tl with [] -> () | _ -> pf ppf ",@ %a" promote_args_chunked tl
        in
        match args with
        | [] -> pf ppf "double"
        | hd :: tl ->
            pf ppf "@[stan::promote_args_t<@[%a%a@]>@]" (list ~sep:comma string)
              hd go tl in
      promote_args_chunked ppf
        List.(chunks_of ~length:5 (filter_opt (return_arg_types args)))

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

(** [pp_located_error ppf (pp_body_block, body_block, err_msg)] surrounds [body_block]
    with a C++ try-catch that will rethrow the error with the proper source location
    from the [body_block] (required to be a [stmt_loc Block] variant).
  @param ppf A pretty printer.
  @param pp_body_block A pretty printer for the body block
  @param body A C++ scoped body block surrounded by squiggly braces.
  *)
let pp_located_error ppf (pp_body_block, body) =
  pf ppf "@ try %a" pp_body_block body ;
  string ppf " catch (const std::exception& e) " ;
  pp_block ppf (pp_located, ())

(** Print the type of an object.
 @param ppf A pretty printer
 @param custom_scalar_opt A string representing a types inner scalar value.
 @param name The name of the object
 @param ut The unsized type of the object
 *)
let pp_arg ppf (custom_scalar_opt, (_, name, ut)) =
  let scalar =
    match custom_scalar_opt with
    | Some scalar -> scalar
    | None -> stantype_prim_str ut in
  (* we add the _arg suffix for any Eigen types *)
  pf ppf "const %a& %s" pp_unsizedtype_custom_scalar_eigen_exprs (scalar, ut)
    name

let pp_arg_eigen_suffix ppf (custom_scalar_opt, (_, name, ut)) =
  let scalar =
    match custom_scalar_opt with
    | Some scalar -> scalar
    | None -> stantype_prim_str ut in
  (* we add the _arg suffix for any Eigen types *)
  let opt_arg_suffix =
    if UnsizedType.is_eigen_type ut then name ^ "_arg__" else name in
  pf ppf "const %a& %s" pp_unsizedtype_custom_scalar_eigen_exprs (scalar, ut)
    opt_arg_suffix

(** [pp_located_error_b] automatically adds a Block wrapper *)
let pp_located_error_b ppf body_stmts =
  pp_located_error ppf
    ( pp_statement
    , Stmt.Fixed.{pattern= Block body_stmts; meta= Locations.no_span_num} )

let typename t = Typename t

(** Construct an object with it's needed templates for function signatures.
 @param fdargs A sexp list of strings representing C++ types.
  *)
let get_templates_and_args exprs fdargs =
  let argtypetemplates = maybe_templated_arg_types fdargs in
  let requireargtemplates = maybe_require_templates argtypetemplates fdargs in
  ( List.filter_opt argtypetemplates
  , List.filter_opt requireargtemplates
  , if not exprs then
      List.map
        ~f:(fun a -> str "%a" pp_arg a)
        (List.zip_exn argtypetemplates fdargs)
    else
      List.map
        ~f:(fun a -> str "%a" pp_arg_eigen_suffix a)
        (List.zip_exn argtypetemplates fdargs) )

(** Print the C++ template parameter decleration before a function.
  @param ppf A pretty printer.
 *)
let pp_template_decorator ppf = function
  | [] -> ()
  | templates ->
      pf ppf "@[template <@[<h 8>%a>@]@]@ " (list ~sep:comma string) templates

let mk_extra_args templates args =
  List.map ~f:(fun (t, v) -> t ^ "& " ^ v) (List.zip_exn templates args)

(** Print the C++ function definition.
  @param ppf A pretty printer
  Refactor this please - one idea might be to have different functions for
   printing user defined distributions vs rngs vs regular functions.
*)
let pp_fun_def ppf
    ( Program.{fdrt; fdname; fdsuffix; fdargs; fdbody; _}
    , functors
    , funs_used_in_reduce_sum
    , funs_used_in_variadic_ode
    , funs_used_in_variadic_dae ) =
  let extra, extra_templates =
    match fdsuffix with
    | Fun_kind.FnTarget -> (["lp__"; "lp_accum__"], ["T_lp__"; "T_lp_accum__"])
    | FnRng -> (["base_rng__"], ["RNG"])
    | FnLpdf _ | FnPlain -> ([], []) in
  let pp_body ppf (Stmt.Fixed.{pattern; _} as fdbody) =
    pf ppf "@[<hv 8>using local_scalar_t__ =@ %a;@]@," pp_promoted_scalar fdargs ;
    pf ppf "int current_statement__ = 0; @ " ;
    if List.exists ~f:(fun (_, _, t) -> UnsizedType.is_eigen_type t) fdargs then
      pp_eigen_arg_to_ref ppf fdargs ;
    ( match fdsuffix with
    | FnLpdf _ | FnTarget -> ()
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
    pf ppf "@ " in
  let get_templates exprs variadic =
    let argtypetemplates, require_templates, args =
      get_templates_and_args exprs fdargs in
    let templates =
      List.(map ~f:typename (argtypetemplates @ extra_templates))
      @ require_templates in
    match (fdsuffix, variadic) with
    | (FnLpdf _ | FnTarget), `None -> (Bool "propto__" :: templates, args)
    | _ -> (templates, args) in
  let pp_sig ppf (name, args, variadic) =
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
    Format.close_box () in
  let templates, templated_args = get_templates true `None in
  pp_templates ~defaults:(Option.is_some fdbody) ppf templates ;
  pp_sig ppf (fdname, templated_args, `None) ;
  match fdbody with
  | None -> pf ppf ";@ "
  | Some fdbody ->
      pp_block ppf (pp_body, fdbody) ;
      let register_functor (str_args, args, variadic) =
        let suffix =
          match variadic with
          | `None -> functor_suffix
          | `ReduceSum -> reduce_sum_functor_suffix
          | `VariadicODE -> variadic_ode_functor_suffix
          | `VariadicDAE -> variadic_dae_functor_suffix in
        let functor_name = fdname ^ suffix in
        let struct_template =
          match (fdsuffix, variadic) with
          | FnLpdf _, `ReduceSum -> Some (Bool "propto__")
          | _ -> None in
        let arg_templates, templated_args = get_templates false variadic in
        let signature =
          str "%a" pp_sig ("operator()", templated_args, variadic) in
        let defn =
          str "%a@ const@,{@.  return %a;@.}@." pp_sig
            ( functor_name
              ^ (if struct_template <> None then "<propto__>" else "")
              ^ "::operator()"
            , templated_args
            , variadic )
            pp_call_str
            ( ( match fdsuffix with
              | FnLpdf _ | FnTarget -> fdname ^ "<propto__>"
              | _ -> fdname )
            , str_args
              @ List.map ~f:(fun (_, name, _) -> name) args
              @ extra @ ["pstream__"] ) in
        Hashtbl.add_multi functors ~key:functor_name
          ~data:{struct_template; arg_templates; signature; defn} in
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

(** Creates functions outside the model namespaces which only call the ones
   inside the namespaces *)
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

let version = "// Code generated by %%NAME%% %%VERSION%%"
let includes = "#include <stan/model/model_header.hpp>"

(** Validate the dimensions of the C++ object are correct at runtime
 @param ppf A pretty printer
 @param name The name of the object.
 @param st The SizedType of the object.
 *)
let pp_validate_data ppf (name, st) =
  if String.is_suffix ~suffix:"__" name then ()
  else
    let pp_stdvector ppf args =
      let pp_cast ppf x = pf ppf "static_cast<size_t>(%a)" pp_expr x in
      pf ppf "@[<hov 2> std::vector<size_t>{@,%a}@]" (list ~sep:comma pp_cast)
        args in
    pf ppf "@[<hov 4>context__.validate_dims(@,%S,@,%S,@,%S,@,%a);@]@ "
      "data initialization"
      (Mangle.remove_prefix name)
      (stantype_prim_str (SizedType.to_unsized st))
      pp_stdvector (SizedType.get_dims_io st)

let pp_mul ppf () = pf ppf " * "

let get_unconstrained_param_st lst =
  match lst with
  | _, {Program.out_block= Parameters; out_unconstrained_st= st; _} -> (
    match SizedType.get_dims_io st with
    | [] -> Some [Expr.Helpers.loop_bottom]
    | ls -> Some ls )
  | _ -> None

let get_constrained_param_st lst =
  match lst with
  | _, {Program.out_block= Parameters; out_constrained_st= st; _} -> (
    match SizedType.get_dims_io st with
    | [] -> Some [Expr.Helpers.loop_bottom]
    | ls -> Some ls )
  | _ -> None

let pp_num_param ppf (dims : Expr.Typed.t list) =
  match dims with
  | [a] -> pf ppf "@[%a@]@," (list ~sep:pp_mul pp_expr) [a]
  | _ -> pf ppf "@[(%a)@]@," (list ~sep:pp_mul pp_expr) dims

(** Print the constructor of the model class.
 Read in data steps:
   1. context__.validate_dims() to verify the dimensions are correct at runtime.
   1. find vals_%s__ from context__.vals_%s(vident)
   1. keep track of pos__
   1. run checks on resulting vident
*)
let pp_ctor ppf p =
  let params =
    [ "stan::io::var_context& context__"; "unsigned int random_seed__ = 0"
    ; "std::ostream* pstream__ = nullptr" ] in
  pf ppf "%s(@[<hov 0>%a) : model_base_crtp(0) @]" p.Program.prog_name
    (list ~sep:comma string) params ;
  let data_idents = List.map ~f:fst p.input_vars |> String.Set.of_list in
  let pp_stmt_topdecl_size_only ppf (Stmt.Fixed.{pattern; meta} as s) =
    match pattern with
    | Decl {decl_id; decl_type; _} when decl_id <> "pos__" -> (
      match decl_type with
      | Sized st -> (
          Locations.pp_smeta ppf meta ;
          let is_input_data = Set.mem data_idents decl_id in
          match is_input_data with
          | true ->
              pp_validate_data ppf (decl_id, st) ;
              pp_assign_data ppf (decl_id, st, false)
          | false -> pp_assign_data ppf (decl_id, st, true) )
      | Unsized _ -> () )
    | _ -> pp_statement ppf s in
  pp_block ppf
    ( (fun ppf {Program.prog_name; prepare_data; output_vars; _} ->
        pf ppf "int current_statement__ = 0;@ " ;
        pf ppf "using local_scalar_t__ = double ;@ " ;
        pf ppf "boost::ecuyer1988 base_rng__ = @ " ;
        pf ppf "    stan::services::util::create_rng(random_seed__, 0);@ " ;
        pf ppf "%a@ " pp_unused "base_rng__" ;
        pf ppf "%a@ " pp_function__ (prog_name, prog_name) ;
        pf ppf
          "local_scalar_t__ \
           DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());@ " ;
        pf ppf "%a" pp_unused "DUMMY_VAR__" ;
        pp_located_error ppf
          (pp_block, (list ~sep:cut pp_stmt_topdecl_size_only, prepare_data)) ;
        cut ppf () ;
        let output_params =
          List.filter_map ~f:get_unconstrained_param_st output_vars in
        let pp_plus ppf () = pf ppf " + " in
        let pp_set_params ppf pars = (list ~sep:pp_plus pp_num_param) ppf pars in
        match output_params with
        | [] -> pf ppf "num_params_r__ = 0U;@,"
        | _ ->
            pf ppf "@[<hov 2>num_params_r__ = %a;@]@," pp_set_params
              output_params )
    , p )

let rec top_level_decls Stmt.Fixed.{pattern; _} =
  match pattern with
  | Decl d when d.decl_id <> "pos__" ->
      [(d.decl_id, Type.to_unsized d.decl_type)]
  | SList stmts -> List.concat_map ~f:top_level_decls stmts
  | _ -> []

(** Print the private data members of the model class *)
let pp_model_private ppf {Program.prepare_data; _} =
  let data_decls = List.concat_map ~f:top_level_decls prepare_data in
  (*Filter out Any data that is not an Eigen matrix*)
  let get_eigen_map (name, ut) =
    if UnsizedType.is_eigen_type ut && not (Transform_Mir.is_opencl_var name)
    then true
    else false in
  let eigen_map_decls = (List.filter ~f:get_eigen_map) data_decls in
  pf ppf "%a @ %a"
    (list ~sep:cut pp_data_decl)
    data_decls
    (list ~sep:cut pp_map_decl)
    eigen_map_decls

(** Print the signature and blocks of the model class methods.
  @param ppf A pretty printer
  @param rt The return type.
  @param name The method name.
  @param intro Anything that needs printed before the method body.
  @param outro Anything that needs printed after the method body.
  @param cv_attr Optional parameter to add method attributes.
  @param ppbody (?A pretty printer of the method's body)
 *)
let pp_method ppf rt name params intro ?(outro = nop)
    ?(cv_attr : string list = ["const"]) ppbody =
  pf ppf "@[<v 2>inline %s %s(@[<hov>@,%a@]) %a " rt name
    (list ~sep:comma string) params (list ~sep:cut string) cv_attr ;
  pf ppf "{@,%a@ " intro () ;
  ppbody ppf ;
  outro ppf () ;
  pf ppf "@,} // %s() @,@]" name

(** Print the [get_param_names] method of the model class
  @param ppf A pretty printer.
 *)
let pp_get_param_names ppf {Program.output_vars; _} =
  let add_param = fmt "%S" in
  let extract_name var = Mangle.remove_prefix (fst var) in
  pp_method ppf "void" "get_param_names"
    ["std::vector<std::string>& names__"]
    nop
    (fun ppf ->
      pf ppf "@[<hov 2>names__ = std::vector<std::string>{%a};@]@,"
        (list ~sep:comma add_param)
        (List.map ~f:extract_name output_vars) )
    ~cv_attr:["const"]

(** Print the [get_dims] method of the model class. *)
let pp_get_dims ppf {Program.output_vars; _} =
  let pp_cast ppf cast_dims =
    pf ppf "@[<hov 2>static_cast<size_t>(%a)@]@," pp_expr cast_dims in
  let pp_pack ppf inner_dims =
    pf ppf "std::vector<size_t>{@[<hov>@,%a@]}" (list ~sep:comma pp_cast)
      inner_dims in
  let pp_add_pack ppf dims = pf ppf "%a" pp_pack dims in
  let dim_list =
    List.(
      map ~f:(fun (_, {Program.out_constrained_st= st; _}) -> st) output_vars)
  in
  let pp_output_var ppf dims =
    (list ~sep:comma pp_add_pack) ppf List.(map ~f:SizedType.get_dims_io dims)
  in
  pp_method ppf "void" "get_dims"
    ["std::vector<std::vector<size_t>>& dimss__"]
    nop
    (fun ppf ->
      pf ppf "@[<hov 2>dimss__ = std::vector<std::vector<size_t>>{%a};@]@,"
        pp_output_var dim_list )
    ~cv_attr:["const"]

let pp_method_b ppf rt name params intro ?(outro = nop) ?(cv_attr = ["const"])
    body =
  pp_method ppf rt name params intro
    (fun ppf -> pp_located_error_b ppf body)
    ~outro ~cv_attr

(** Print the write_array method of the model class *)
let pp_write_array ppf {Program.prog_name; generate_quantities; _} =
  pf ppf
    "template <typename RNG, typename VecR, typename VecI, typename VecVar, @ \
     stan::require_vector_like_vt<std::is_floating_point, VecR>* = nullptr, @ \
     stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr, @ \
     stan::require_std_vector_vt<std::is_floating_point, VecVar>* = nullptr> \
     @ " ;
  let params =
    [ "RNG& base_rng__"; "VecR& params_r__"; "VecI& params_i__"; "VecVar& vars__"
    ; "const bool emit_transformed_parameters__ = true"
    ; "const bool emit_generated_quantities__ = true"
    ; "std::ostream* pstream__ = nullptr" ] in
  let intro ppf () =
    pf ppf "%a@ %a@ %a" (list ~sep:cut string)
      [ "using local_scalar_t__ = double;"
      ; "stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);"
      ; "stan::io::serializer<local_scalar_t__> out__(vars__);"
      ; "static constexpr bool propto__ = true;"; "(void) propto__;"
      ; "double lp__ = 0.0;"
      ; "(void) lp__;  // dummy to suppress unused var warning"
      ; "int current_statement__ = 0; "
      ; "stan::math::accumulator<double> lp_accum__;"
      ; "local_scalar_t__ \
         DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());"
      ; "constexpr bool jacobian__ = false;" ]
      pp_unused "DUMMY_VAR__" pp_function__ (prog_name, "write_array") in
  pp_method_b ppf "void" "write_array_impl" params intro generate_quantities

(** Prints the for loop for [constrained_param_names]
    and [unconstrained_param_names]
  @param index_ids Optional named parameter of a SizedType's dimensions
  @param ppf A pretty printer
  @param dims A list of the dimensions of a SizedType
  @param pp_body Pretty printer for the body of the loop.
 *)
let rec pp_for_loop_iteratee ?(index_ids = []) ppf (iteratee, dims, pp_body) =
  let iter d pp_body =
    let loopvar, gensym_exit = Common.Gensym.enter () in
    pp_for_loop ppf
      ( loopvar
      , Expr.Helpers.loop_bottom
      , d
      , pp_block
      , (pp_body, (iteratee, loopvar :: index_ids)) ) ;
    gensym_exit () in
  match dims with
  | [] -> pp_body ppf (iteratee, index_ids)
  | dim :: dims ->
      iter dim (fun ppf (i, idcs) ->
          pf ppf "@[%a @]" pp_block
            (pp_for_loop_iteratee ~index_ids:idcs, (i, dims, pp_body)) )

let emit_name ppf (name, idcs) =
  let name = Mangle.remove_prefix name in
  let to_string = fmt "std::to_string(%s)" in
  pf ppf "param_names__.emplace_back(std::string() + %a);"
    (list ~sep:(fun ppf () -> pf ppf " + '.' + ") string)
    (str "%S" name :: List.map ~f:(str "%a" to_string) idcs)

let emit_complex_name ppf (name, idcs) =
  let name = Mangle.remove_prefix name in
  let to_string = fmt "std::to_string(%s)" in
  pf ppf "@[param_names__.emplace_back(std::string() + %a);@]@,"
    (list ~sep:(fun ppf () -> pf ppf " + '.' + ") string)
    ((str "%S" name :: List.map ~f:(str "%a" to_string) idcs) @ ["\"real\""]) ;
  pf ppf "param_names__.emplace_back(std::string() + %a);"
    (list ~sep:(fun ppf () -> pf ppf " + '.' + ") string)
    ((str "%S" name :: List.map ~f:(str "%a" to_string) idcs) @ ["\"imag\""])

(** Print the [constrained_param_names] method of the model class. *)
let pp_constrained_param_names ppf {Program.output_vars; _} =
  let params =
    [ "std::vector<std::string>& param_names__"
    ; "bool emit_transformed_parameters__ = true"
    ; "bool emit_generated_quantities__ = true" ] in
  let paramvars, tparamvars, gqvars =
    List.partition3_map
      ~f:(function
        | id, {Program.out_block= Parameters; out_constrained_st= st; _} ->
            `Fst (id, st)
        | id, {out_block= TransformedParameters; out_constrained_st= st; _} ->
            `Snd (id, st)
        | id, {out_block= GeneratedQuantities; out_constrained_st= st; _} ->
            `Trd (id, st) )
      output_vars in
  let pp_param_names ppf (decl_id, st) =
    let gen_name =
      if SizedType.contains_complex st then emit_complex_name else emit_name
    in
    let dims = List.rev (SizedType.get_dims st) in
    pp_for_loop_iteratee ppf (decl_id, dims, gen_name) in
  pp_method ppf "void" "constrained_param_names" params nop
    (fun ppf ->
      (list ~sep:cut pp_param_names) ppf paramvars ;
      pf ppf "@,if (emit_transformed_parameters__) %a@," pp_block
        (list ~sep:cut pp_param_names, tparamvars) ;
      pf ppf "@,if (emit_generated_quantities__) %a@," pp_block
        (list ~sep:cut pp_param_names, gqvars) )
    ~cv_attr:["const"; "final"]

(** Print the [unconstrained_param_names] method of the model class.
  This is just a copy of constrained, I need to figure out which one is wrong
   and fix it eventually. From Bob,

   Off the top of my head, I think the four that change sizes
   from constrained to unconstrained are:

   simplex:  K -> (K - 1)
   covar_matrix:  K^2 -> (K choose 2) + K
   corr_matrix:  K^2 -> (K choose 2)
   cholesky_corr: (K choose 2) + K -> (K choose 2)

   cholesky_cov does not change size (it's (K choose 2) + K).
   Now that our unit vector uses the normal thing, that also doesn't
   change size.  The ordered types and constrained types don't
   change sizes either.
*)
let pp_unconstrained_param_names ppf {Program.output_vars; _} =
  let params =
    [ "std::vector<std::string>& param_names__"
    ; "bool emit_transformed_parameters__ = true"
    ; "bool emit_generated_quantities__ = true" ] in
  let paramvars, tparamvars, gqvars =
    List.partition3_map
      ~f:(function
        | id, {Program.out_block= Parameters; out_unconstrained_st= st; _} ->
            `Fst (id, st)
        | id, {out_block= TransformedParameters; out_unconstrained_st= st; _} ->
            `Snd (id, st)
        | id, {out_block= GeneratedQuantities; out_unconstrained_st= st; _} ->
            `Trd (id, st) )
      output_vars in
  let pp_param_names ppf (decl_id, st) =
    let pp_names =
      if SizedType.contains_complex st then emit_complex_name else emit_name
    in
    pp_for_loop_iteratee ppf
      (decl_id, List.rev (SizedType.get_dims st), pp_names) in
  let cv_attr = ["const"; "final"] in
  pp_method ppf "void" "unconstrained_param_names" params nop
    (fun ppf ->
      (list ~sep:cut pp_param_names) ppf paramvars ;
      pf ppf "@,if (emit_transformed_parameters__) %a@," pp_block
        (list ~sep:cut pp_param_names, tparamvars) ;
      pf ppf "@,if (emit_generated_quantities__) %a@," pp_block
        (list ~sep:cut pp_param_names, gqvars) )
    ~cv_attr

(** Print the [transform_inits] method of the model class *)
let pp_transform_inits_impl ppf {Program.transform_inits; _} =
  pf ppf
    "template <typename VecVar, typename VecI, @ \
     stan::require_std_vector_t<VecVar>* = nullptr, @ \
     stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr> @ " ;
  let params =
    [ "VecVar& params_r__"; "VecI& params_i__"; "VecVar& vars__"
    ; "std::ostream* pstream__ = nullptr" ] in
  let intro ppf () =
    pf ppf "%a" (list ~sep:cut string)
      [ "using local_scalar_t__ = double;"
      ; "stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);"
      ; "stan::io::serializer<local_scalar_t__> out__(vars__);"
      ; "int current_statement__ = 0;"
      ; "local_scalar_t__ \
         DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());" ] in
  let cv_attr = ["const"] in
  pp_method_b ppf "void" "transform_inits_impl" params intro transform_inits
    ~cv_attr

(** Print the [log_prob] method of the model class *)
let pp_log_prob ppf Program.{prog_name; log_prob; _} =
  pf ppf
    "@ template <bool propto__, bool jacobian__ , typename VecR, typename \
     VecI, @ stan::require_vector_like_t<VecR>* = nullptr, @ \
     stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr> @ " ;
  let params =
    ["VecR& params_r__"; "VecI& params_i__"; "std::ostream* pstream__ = nullptr"]
  in
  let intro ppf () =
    pf ppf "%a@ %a@ %a" (list ~sep:cut string)
      [ "using T__ = stan::scalar_type_t<VecR>;"; "using local_scalar_t__ = T__;"
      ; "T__ lp__(0.0);"; "stan::math::accumulator<T__> lp_accum__;"
      ; "stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);"
      ; "int current_statement__ = 0;"
      ; "local_scalar_t__ \
         DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());" ]
      pp_unused "DUMMY_VAR__" pp_function__ (prog_name, "log_prob") in
  let outro ppf () =
    pf ppf "@ lp_accum__.add(lp__);@ return lp_accum__.sum();" in
  let cv_attr = ["const"] in
  pp_method_b ppf "stan::scalar_type_t<VecR>" "log_prob_impl" params intro
    log_prob ~outro ~cv_attr

(** Print the body of the constrained and unconstrained sizedtype methods
 in the model class
 @param ppf A pretty printer
 @param method_name The name of the method to wrap the body in.
 @param outvars The parameters to gather the sizes for.
 *)
let pp_outvar_metadata ppf (method_name, outvars) =
  let json_str = Cpp_Json.out_var_interpolated_json_str outvars in
  let ppbody ppf = pf ppf "@[<hov 2>return std::string(%s);@]@," json_str in
  pp_method ppf "std::string" method_name [] nop ppbody ~cv_attr:["const"]

(** Print the [get_unconstrained_sizedtypes] method of the model class *)
let pp_unconstrained_types ppf {Program.output_vars; _} =
  let grab_unconstrained (name, {Program.out_unconstrained_st; out_block; _}) =
    (name, out_unconstrained_st, out_block) in
  let outvars = List.map ~f:grab_unconstrained output_vars in
  pp_outvar_metadata ppf ("get_unconstrained_sizedtypes", outvars)

(** Print the [get_constrained_sizedtypes] method of the model class *)
let pp_constrained_types ppf {Program.output_vars; _} =
  let grab_constrained (name, {Program.out_constrained_st; out_block; _}) =
    (name, out_constrained_st, out_block) in
  let outvars = List.map ~f:grab_constrained output_vars in
  pp_outvar_metadata ppf ("get_constrained_sizedtypes", outvars)

(** Print the generic method overloads needed in the model class. *)
let pp_overloads ppf {Program.output_vars; _} =
  (* An expression for the number of individual parameters in a list of output variables *)
  let num_outvars (outvars : Expr.Typed.t Program.outvar list) =
    Expr.Helpers.binop_list
      (List.map
         ~f:(fun outvar ->
           SizedType.num_elems_expr outvar.Program.out_constrained_st )
         outvars )
      Operator.Plus ~default:(Expr.Helpers.int 0) in
  (* The list of output variables that came from a particular block *)
  let block_outvars (block : Program.io_block) =
    List.filter_map output_vars
      ~f:(fun ((_ : string), (outvar : Expr.Typed.t Program.outvar)) ->
        if outvar.out_block = block then Some outvar else None ) in
  let num_gen_quantities = num_outvars (block_outvars GeneratedQuantities) in
  let num_params = num_outvars (block_outvars Parameters) in
  let num_transformed = num_outvars (block_outvars TransformedParameters) in
  pf ppf
    {|
    // Begin method overload boilerplate
    template <typename RNG>
    inline void write_array(RNG& base_rng,
                            Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                            Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                            const bool emit_transformed_parameters = true,
                            const bool emit_generated_quantities = true,
                            std::ostream* pstream = nullptr) const {
      const size_t num_params__ = %a;
      const size_t num_transformed = %a;
      const size_t num_gen_quantities = %a;
      std::vector<double> vars_vec(num_params__
       + (emit_transformed_parameters * num_transformed)
       + (emit_generated_quantities * num_gen_quantities));
      std::vector<int> params_i;
      write_array_impl(base_rng, params_r, params_i, vars_vec,
          emit_transformed_parameters, emit_generated_quantities, pstream);
      vars = Eigen::Map<Eigen::Matrix<double,Eigen::Dynamic,1>>(
        vars_vec.data(), vars_vec.size());
    }

    template <typename RNG>
    inline void write_array(RNG& base_rng, std::vector<double>& params_r,
                            std::vector<int>& params_i,
                            std::vector<double>& vars,
                            bool emit_transformed_parameters = true,
                            bool emit_generated_quantities = true,
                            std::ostream* pstream = nullptr) const {
      const size_t num_params__ = %a;
      const size_t num_transformed = %a;
      const size_t num_gen_quantities = %a;
      vars.resize(num_params__
        + (emit_transformed_parameters * num_transformed)
        + (emit_generated_quantities * num_gen_quantities));
      write_array_impl(base_rng, params_r, params_i, vars, emit_transformed_parameters, emit_generated_quantities, pstream);
    }

    template <bool propto__, bool jacobian__, typename T_>
    inline T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
                       std::ostream* pstream = nullptr) const {
      Eigen::Matrix<int, -1, 1> params_i;
      return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
    }

    template <bool propto__, bool jacobian__, typename T__>
    inline T__ log_prob(std::vector<T__>& params_r,
                        std::vector<int>& params_i,
                        std::ostream* pstream = nullptr) const {
      return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
    }


    inline void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream = nullptr) const final {
      std::vector<double> params_r_vec(params_r.size());
      std::vector<int> params_i;
      transform_inits(context, params_i, params_r_vec, pstream);
      params_r = Eigen::Map<Eigen::Matrix<double,Eigen::Dynamic,1>>(
        params_r_vec.data(), params_r_vec.size());
    }
|}
    pp_expr num_params pp_expr num_transformed pp_expr num_gen_quantities
    pp_expr num_params pp_expr num_transformed pp_expr num_gen_quantities

(** Print the [get_constrained_sizedtypes] method of the model class *)
let pp_transform_inits ppf {Program.output_vars; _} =
  let params =
    [ "const stan::io::var_context& context"; "std::vector<int>& params_i"
    ; "std::vector<double>& vars"; "std::ostream* pstream__ = nullptr" ] in
  let list_names ((stri : string), (Program.{out_block; _} : 'a Program.outvar))
      =
    match out_block with Parameters -> Some stri | _ -> None in
  let param_names = List.filter_map ~f:list_names output_vars in
  let list_len = List.length param_names in
  let constrained_params =
    List.filter_map ~f:get_constrained_param_st output_vars in
  let get_names ppf () =
    let add_param = fmt "%S" in
    pf ppf "@[<hov 2> constexpr std::array<const char*, %i> names__{%a};@]@,"
      list_len
      (list ~sep:comma add_param)
      (List.map ~f:Mangle.remove_prefix param_names) in
  let get_constrain_param_size_arr ppf () =
    match constrained_params with
    | [] ->
        pf ppf
          "@[<hov 2> const std::array<Eigen::Index, 0> \
           constrain_param_sizes__{};@]@,"
    | _ ->
        let pp_set_params ppf pars = (list ~sep:comma pp_num_param) ppf pars in
        pf ppf
          "@[<hov 2> const std::array<Eigen::Index, %i> \
           constrain_param_sizes__{%a};@]@,"
          list_len pp_set_params constrained_params in
  let get_constrained_param_size ppf () =
    pf ppf
      "@[<hov 2> const auto num_constrained_params__ = std::accumulate(@,\
      \ constrain_param_sizes__.begin(),@,\
       @ constrain_param_sizes__.end(), 0);@]@," in
  let pp_body ppf =
    pf ppf "%a" (list ~sep:cut string)
      [ " std::vector<double> params_r_flat__(num_constrained_params__);"
      ; " Eigen::Index size_iter__ = 0;"; " Eigen::Index flat_iter__ = 0;"
      ; " for (auto&& param_name__ : names__) {"
      ; "   const auto param_vec__ = context.vals_r(param_name__);"
      ; "   for (Eigen::Index i = 0; i < constrain_param_sizes__[size_iter__]; \
         ++i) {"; "     params_r_flat__[flat_iter__] = param_vec__[i];"
      ; "     ++flat_iter__;"; "   }"; "   ++size_iter__;"; " }"
      ; " vars.resize(num_params_r__);"
      ; " transform_inits_impl(params_r_flat__, params_i, vars, pstream__);" ]
  in
  let cv_attr = ["const"] in
  let intro ppf =
    pf ppf "%a %a %a" get_names () get_constrain_param_size_arr ()
      get_constrained_param_size in
  pp_method ppf "void" "transform_inits" params intro
    (fun ppf -> pp_body ppf)
    ~cv_attr

(** Print the public parts of the model class *)
let pp_model_public ppf p =
  pf ppf "@ %a" pp_ctor p ;
  pf ppf "@ %a" pp_log_prob p ;
  pf ppf "@ %a" pp_write_array p ;
  pf ppf "@ %a" pp_transform_inits_impl p ;
  (* Begin metadata methods *)
  pf ppf "@ %a" pp_get_param_names p ;
  (* Post-data metadata methods *)
  pf ppf "@ %a" pp_get_dims p ;
  pf ppf "@ %a" pp_constrained_param_names p ;
  pf ppf "@ %a" pp_unconstrained_param_names p ;
  pf ppf "@ %a" pp_constrained_types p ;
  pf ppf "@ %a" pp_unconstrained_types p ;
  (* Boilerplate *)
  pf ppf "@ %a" pp_overloads p ;
  pf ppf "@ %a" pp_transform_inits p

let model_prefix = "model_"

(** Print the full model class. *)
let pp_model ppf ({Program.prog_name; _} as p) =
  pf ppf "class %s final : public model_base_crtp<%s> {" prog_name prog_name ;
  pf ppf "@ @[<v 1>@ private:@ @[<v 1> %a@]@ " pp_model_private p ;
  pf ppf "@ public:@ @[<v 1> ~%s() { }" prog_name ;
  pf ppf "@ @ inline std::string model_name() const final { return \"%s\"; }"
    prog_name ;
  pf ppf
    {|

  inline std::vector<std::string> model_compile_info() const noexcept {
    return std::vector<std::string>{"stanc_version = %s", "stancflags = %s"};
  }
  |}
    "%%NAME%%3 %%VERSION%%" stanc_args_to_print ;
  pf ppf "@ %a@]@]@ };" pp_model_public p

let usings =
  {|
using stan::model::model_base_crtp;
using namespace stan::math;
|}

(** Create the model's namespace. *)
let namespace Program.{prog_name; _} = prog_name ^ "_namespace"

(** Find and register functiors used for map_rect. *)
let pp_register_map_rect_functors ppf p =
  let pp_register_functor ppf (i, f) =
    pf ppf "STAN_REGISTER_MAP_RECT(%d, %s::%s)" i (namespace p) f in
  pf ppf "@ %a"
    (list ~sep:cut pp_register_functor)
    (List.sort ~compare (Hashtbl.to_alist map_rect_calls))

let is_fun_used_with_variadic_fn variadic_fn_test p =
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

let collect_functors_functions p =
  let (functors : (string, found_functor list) Hashtbl.t) =
    String.Table.create () in
  let reduce_sum_fns =
    is_fun_used_with_variadic_fn Stan_math_signatures.is_reduce_sum_fn p in
  let variadic_ode_fns =
    is_fun_used_with_variadic_fn Stan_math_signatures.is_variadic_ode_fn p in
  let variadic_dae_fns =
    is_fun_used_with_variadic_fn Stan_math_signatures.is_variadic_dae_fn p in
  let pp_fun_def_with_variadic_fn_list ppf fblock =
    (hovbox ~indent:2 pp_fun_def)
      ppf
      (fblock, functors, reduce_sum_fns, variadic_ode_fns, variadic_dae_fns)
  in
  ( str "@[<v>%a@]"
      (list ~sep:cut pp_fun_def_with_variadic_fn_list)
      p.functions_block
  , functors )

(** Print the full C++ for the stan program. *)
let pp_prog ppf (p : Program.Typed.t) =
  (* First, do some transformations on the MIR itself before we begin printing it.*)
  let p, s = Locations.prepare_prog p in
  let fns_str, functors = collect_functors_functions p in
  let pp_functor_decls ppf tbl =
    Hashtbl.iteri tbl ~f:(fun ~key ~data ->
        pf ppf "@[<v 2>%astruct %s {@,%aconst;@]@,};@."
          (option
             (option (fun ppf t ->
                  pf ppf "template <%a>@ " pp_template_defaults t ) ) )
          (Option.map ~f:(fun x -> x.struct_template) (List.hd data))
          key
          (list ~sep:(any "const;@,") (fun ppf (ts, sign) ->
               pf ppf "%a@[<h>%a@]" (pp_templates ~defaults:true) ts text sign )
          )
          (List.map
             ~f:(fun {arg_templates; signature; _} -> (arg_templates, signature))
             data ) ) in
  let pp_functors ppf tbl =
    Hashtbl.iter tbl ~f:(fun data ->
        List.iter data ~f:(fun {struct_template; defn; arg_templates; _} ->
            pf ppf "%a%a%s@."
              (option (fun ppf t -> pf ppf "template <%a>@ " pp_template t))
              struct_template
              (pp_templates ~defaults:false)
              arg_templates defn ) ) in
  pf ppf "@[<v>@ %s@ %s@ namespace %s {@ %s@ %a@ %a@ %s@ %a@ %a@ }@ @]" version
    includes (namespace p) usings Locations.pp_globals s pp_functor_decls
    functors fns_str pp_functors functors
    (if !standalone_functions then fun _ _ -> () else pp_model)
    p ;
  if !standalone_functions then
    pf ppf "@[<v>%a@ @]"
      (list ~sep:cut (pp_standalone_fun_def (namespace p)))
      p.functions_block
  else (
    pf ppf "@,using stan_model = %s_namespace::%s;@," p.prog_name p.prog_name ;
    pf ppf
      {|
#ifndef USING_R

// Boilerplate
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}

stan::math::profile_map& get_stan_profile_data() {
  return %s_namespace::profiles__;
}

#endif
|}
      p.prog_name ;
    pf ppf "@[<v>%a@]" pp_register_map_rect_functors p )
