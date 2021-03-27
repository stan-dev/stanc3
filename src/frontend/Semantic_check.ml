(** Semantic validation of AST*)

(* Idea: check many of things related to identifiers that are hard to check
   during parsing and are in fact irrelevant for building up the parse tree *)

open Core_kernel
open Symbol_table
open Middle
open Ast
open Errors
module Validate = Common.Validation.Make (Semantic_error)

(* There is a semantic checking function for each AST node that calls
   the checking functions for its children left to right. *)

(* Top level function semantic_check_program declares the AST while operating
   on (1) a global symbol table vm, and (2) structure of type context_flags_record
   to communicate information down the AST. *)

let check_of_compatible_return_type (rt1 : UnsizedType.returntype)
    (srt2 : Ast.statement_returntype) : sexp_bool =
  UnsizedType.(
    match (rt1, srt2) with
    | Void, NoReturnType
     |Void, Incomplete Void
     |Void, Complete Void
     |Void, AnyReturnType ->
        true
    | ReturnType UReal, Complete (ReturnType UInt) -> true
    | ReturnType rt1, Complete (ReturnType rt2) -> rt1 = rt2
    | ReturnType _, AnyReturnType -> true
    | _ -> false)

(** Origin blocks, to keep track of where variables are declared *)
type originblock =
  | MathLibrary
  | Functions
  | Data
  | TData
  | Param
  | TParam
  | Model
  | GQuant

(** Print all the signatures of a stan math operator, for the purposes of error messages. *)
let check_that_all_functions_have_definition = ref true

let model_name = ref ""
let vm = Symbol_table.initialize ()

(* Record structure holding flags and other markers about context to be
   used for error reporting. *)
type context_flags_record =
  { current_block: originblock
  ; in_toplevel_decl: bool
  ; in_fun_def: bool
  ; in_returning_fun_def: bool
  ; in_rng_fun_def: bool
  ; in_lp_fun_def: bool
  ; in_udf_dist_def: bool
  ; loop_depth: int }

(* Some helper functions *)
let dup_exists (l : string sexp_list) : sexp_bool =
  match List.find_a_dup ~compare:String.compare l with
  | Some _ -> true
  | None -> false

let type_of_expr_typed (ue : typed_expression) = ue.emeta.type_

let calculate_autodifftype (cf : context_flags_record) (at : originblock)
    (ut : UnsizedType.t) : UnsizedType.autodifftype =
  match at with
  | (Param | TParam | Model | Functions)
    when not (UnsizedType.contains_int ut || cf.current_block = GQuant) ->
      UnsizedType.AutoDiffable
  | _ -> DataOnly

let has_int_type (ue : typed_expression) = ue.emeta.type_ = UInt
let has_int_array_type (ue : typed_expression) = ue.emeta.type_ = UArray UInt

let has_int_or_real_type (ue : typed_expression) =
  match ue.emeta.type_ with UnsizedType.UInt | UReal -> true | _ -> false

let probability_distribution_name_variants (id : Ast.identifier) :
    identifier sexp_list =
  let name = id.name in
  let open String in
  List.map
    ~f:(fun n -> {name= n; id_loc= id.id_loc})
    ( if name = "multiply_log" || name = "binomial_coefficient_log" then [name]
    else if is_suffix ~suffix:"_lpmf" name then
      [name; drop_suffix name 5 ^ "_lpdf"; drop_suffix name 5 ^ "_log"]
    else if is_suffix ~suffix:"_lpdf" name then
      [name; drop_suffix name 5 ^ "_lpmf"; drop_suffix name 5 ^ "_log"]
    else if is_suffix ~suffix:"_lcdf" name then
      [name; drop_suffix name 5 ^ "_cdf_log"]
    else if is_suffix ~suffix:"_lccdf" name then
      [name; drop_suffix name 6 ^ "_ccdf_log"]
    else if is_suffix ~suffix:"_cdf_log" name then
      [name; drop_suffix name 8 ^ "_lcdf"]
    else if is_suffix ~suffix:"_ccdf_log" name then
      [name; drop_suffix name 9 ^ "_lccdf"]
    else if is_suffix ~suffix:"_log" name then
      [name; drop_suffix name 4 ^ "_lpmf"; drop_suffix name 4 ^ "_lpdf"]
    else [name] )

let lub_rt (loc : Location_span.t) (rt1 : UnsizedType.returntype)
    (rt2 : UnsizedType.returntype) : UnsizedType.returntype Validate.t =
  match (rt1, rt2) with
  | UnsizedType.ReturnType UReal, UnsizedType.ReturnType UInt
   |ReturnType UInt, ReturnType UReal ->
      Validate.ok (UnsizedType.ReturnType UReal)
  | _, _ when rt1 = rt2 -> Validate.ok rt2
  | _ -> Semantic_error.mismatched_return_types loc rt1 rt2 |> Validate.error

(*
Checks that a variable/function name:
 - if UDF that it does not match a Stan Math function
 - a function/identifier does not have the _lupdf/_lupmf suffix
 - is not already in use
*)
let check_fresh_variable_basic (id : Ast.identifier) (is_udf : sexp_bool) :
    unit Validate.t =
  Validate.(
    if
      is_udf
      && ( Stan_math_signatures.is_stan_math_function_name id.name
         (* variadic functions are currently not in math sigs *)
         || Stan_math_signatures.is_reduce_sum_fn id.name
         || Stan_math_signatures.is_variadic_ode_fn id.name )
    then Semantic_error.ident_is_stanmath_name id.id_loc id.name |> error
    else if Utils.is_unnormalized_distribution id.name then
      if is_udf then
        Semantic_error.udf_is_unnormalized_fn id.id_loc id.name |> error
      else
        Semantic_error.ident_has_unnormalized_suffix id.id_loc id.name |> error
    else
      match Symbol_table.look vm id.name with
      | Some _ -> Semantic_error.ident_in_use id.id_loc id.name |> error
      | None -> ok ())

let check_fresh_variable (id : Ast.identifier) (is_udf : sexp_bool) :
    unit Validate.t =
  List.fold ~init:(Validate.ok ())
    ~f:(fun (v0 : unit Validate.t) (name : identifier) ->
      ( check_fresh_variable_basic name is_udf |> Validate.apply_const v0
        : unit Validate.t ) )
    (probability_distribution_name_variants id)

(* == SEMANTIC CHECK OF PROGRAM ELEMENTS ==================================== *)

(* Probably nothing to do here *)
let semantic_check_assignmentoperator (op : assignmentoperator) =
  Validate.ok op

(* Probably nothing to do here *)
let semantic_check_autodifftype at = Validate.ok at

(* Probably nothing to do here *)
let rec semantic_check_unsizedtype : UnsizedType.t -> unit Validate.t =
  function
  | UFun
      ( (l : (UnsizedType.autodifftype * UnsizedType.t) sexp_list)
      , (rt : UnsizedType.returntype) ) ->
      (* fold over argument types accumulating errors with initial state
       given by validating the return type *)
      List.fold
        ~f:
          (fun (v0 : unit Validate.t)
               ((at : UnsizedType.autodifftype), (ut : UnsizedType.t)) ->
          ( Validate.(
              apply_const
                (apply_const v0 (semantic_check_autodifftype at))
                (semantic_check_unsizedtype ut))
            : unit Validate.t ) )
        ~init:(semantic_check_returntype rt)
        l
  | UArray (ut : UnsizedType.t) -> semantic_check_unsizedtype ut
  | _ -> Validate.ok ()

and semantic_check_returntype : UnsizedType.returntype -> unit Validate.t =
  function
  | Void -> Validate.ok ()
  | ReturnType ut -> semantic_check_unsizedtype ut

(* -- Indentifiers ---------------------------------------------------------- *)
let reserved_keywords =
  [ "true"; "false"; "repeat"; "until"; "then"; "var"; "fvar"; "STAN_MAJOR"
  ; "STAN_MINOR"; "STAN_PATCH"; "STAN_MATH_MAJOR"; "STAN_MATH_MINOR"
  ; "STAN_MATH_PATCH"; "alignas"; "alignof"; "and"; "and_eq"; "asm"; "auto"
  ; "bitand"; "bitor"; "bool"; "break"; "case"; "catch"; "char"; "char16_t"
  ; "char32_t"; "class"; "compl"; "const"; "constexpr"; "const_cast"
  ; "continue"; "decltype"; "default"; "delete"; "do"; "double"; "dynamic_cast"
  ; "else"; "enum"; "explicit"; "export"; "extern"; "false"; "float"; "for"
  ; "friend"; "goto"; "if"; "inline"; "int"; "long"; "mutable"; "namespace"
  ; "new"; "noexcept"; "not"; "not_eq"; "nullptr"; "operator"; "or"; "or_eq"
  ; "private"; "protected"; "public"; "register"; "reinterpret_cast"; "return"
  ; "short"; "signed"; "sizeof"; "static"; "static_assert"; "static_cast"
  ; "struct"; "switch"; "template"; "this"; "thread_local"; "throw"; "true"
  ; "try"; "typedef"; "typeid"; "typename"; "union"; "unsigned"; "using"
  ; "virtual"; "void"; "volatile"; "wchar_t"; "while"; "xor"; "xor_eq"
  ; "functions"; "data"; "parameters"; "model"; "return"; "if"; "else"; "while"
  ; "for"; "in"; "break"; "continue"; "void"; "int"; "real"; "vector"
  ; "row_vector"; "matrix"; "ordered"; "positive_ordered"; "simplex"
  ; "unit_vector"; "cholesky_factor_corr"; "cholesky_factor_cov"; "corr_matrix"
  ; "cov_matrix"; "print"; "reject"; "target"; "get_lp"; "profile" ]

let semantic_check_identifier (id : Ast.identifier) =
  Validate.(
    if id.name = !model_name then
      Semantic_error.ident_is_model_name id.id_loc id.name |> error
    else if
      String.is_suffix id.name ~suffix:"__"
      || List.exists ~f:(fun str -> str = id.name) reserved_keywords
    then Semantic_error.ident_is_keyword id.id_loc id.name |> error
    else ok ())

(* -- Operators ------------------------------------------------------------- *)
let semantic_check_operator _ = Validate.ok ()

(* == Expressions =========================================================== *)

let arg_type (x : 'a abstract_typed_expression) :
    UnsizedType.autodifftype * UnsizedType.t =
  (x.emeta.ad_level, x.emeta.type_)

let get_arg_types :
       'a abstract_typed_expression list
    -> (UnsizedType.autodifftype * UnsizedType.t) list =
  List.map ~f:arg_type

(* -- Function application -------------------------------------------------- *)

let semantic_check_fn_map_rect ~(loc : Location_span.t) (id : Ast.identifier)
    es =
  Validate.(
    match (id.name, es) with
    | "map_rect", {expr= Variable arg1; _} :: _
      when String.(
             is_suffix arg1.name ~suffix:"_lp"
             || is_suffix arg1.name ~suffix:"_rng") ->
        Semantic_error.invalid_map_rect_fn loc arg1.name |> error
    | _ -> ok ())

let semantic_check_fn_conditioning ~(loc : Location_span.t)
    (id : Ast.identifier) =
  Validate.(
    if
      List.exists
        ~f:(fun suffix -> String.is_suffix id.name ~suffix)
        Utils.conditioning_suffices
    then Semantic_error.conditioning_required loc |> error
    else ok ())

(** `Target+=` can only be used in model and functions
    with right suffix (same for tilde etc)
*)
let semantic_check_fn_target_plus_equals (cf : context_flags_record)
    ~(loc : Location_span.t) (id : Ast.identifier) =
  Validate.(
    if
      String.is_suffix id.name ~suffix:"_lp"
      && not
           ( cf.in_lp_fun_def || cf.current_block = Model
           || cf.current_block = TParam )
    then Semantic_error.target_plusequals_outisde_model_or_logprob loc |> error
    else ok ())

(** Rng functions cannot be used in Tp or Model and only
    in function defs with the right suffix
*)
let semantic_check_fn_rng (cf : context_flags_record) ~(loc : Location_span.t)
    (id : Ast.identifier) =
  Validate.(
    if String.is_suffix id.name ~suffix:"_rng" && cf.in_toplevel_decl then
      Semantic_error.invalid_decl_rng_fn loc |> error
    else if
      String.is_suffix id.name ~suffix:"_rng"
      && ( (cf.in_fun_def && not cf.in_rng_fun_def)
         || cf.current_block = TParam || cf.current_block = Model )
    then Semantic_error.invalid_rng_fn loc |> error
    else ok ())

(** unnormalized _lpdf/_lpmf functions can only be used in _lpdf/_lpmf/_lp udfs
    or the model block
*)
let semantic_check_unnormalized (cf : context_flags_record)
    ~(loc : Location_span.t) (id : Ast.identifier) =
  Validate.(
    if
      Utils.is_unnormalized_distribution id.name
      && not ((cf.in_fun_def && cf.in_udf_dist_def) || cf.current_block = Model)
    then Semantic_error.invalid_unnormalized_fn loc |> error
    else ok ())

let mk_fun_app ~(is_cond_dist : sexp_bool) (x, y, z) =
  if is_cond_dist then CondDistApp (x, y, z) else FunApp (x, y, z)

(* Regular function application *)
let semantic_check_fn_normal ~(is_cond_dist : sexp_bool)
    ~(loc : Location_span.t) (id : Ast.identifier) es =
  Validate.(
    match Symbol_table.look vm (Utils.normalized_name id.name) with
    | Some (_, UnsizedType.UFun (_, Void)) ->
        Semantic_error.returning_fn_expected_nonreturning_found loc id.name
        |> error
    | Some (_, UFun (listedtypes, rt))
      when not
             (UnsizedType.check_compatible_arguments_mod_conv id.name
                listedtypes (get_arg_types es)) ->
        es
        |> List.map ~f:type_of_expr_typed
        |> Semantic_error.illtyped_userdefined_fn_app loc id.name listedtypes
             rt
        |> error
    | Some (_, UFun (_, ReturnType ut)) ->
        mk_typed_expression
          ~expr:(mk_fun_app ~is_cond_dist (UserDefined, id, es))
          ~ad_level:(expr_ad_lub es) ~type_:ut ~loc
        |> ok
    | Some _ ->
        (* Check that Funaps are actually functions *)
        Semantic_error.returning_fn_expected_nonfn_found loc id.name |> error
    | None ->
        Semantic_error.returning_fn_expected_undeclaredident_found loc id.name
        |> error)

(* Stan-Math function application *)
let semantic_check_fn_stan_math ~(is_cond_dist : sexp_bool)
    ~(loc : Location_span.t) (id : Ast.identifier) es =
  match
    Stan_math_signatures.stan_math_returntype id.name (get_arg_types es)
  with
  | Some UnsizedType.Void ->
      Semantic_error.returning_fn_expected_nonreturning_found loc id.name
      |> Validate.error
  | Some (UnsizedType.ReturnType ut) ->
      mk_typed_expression
        ~expr:(mk_fun_app ~is_cond_dist (StanLib, id, es))
        ~ad_level:(expr_ad_lub es) ~type_:ut ~loc
      |> Validate.ok
  | _ ->
      es
      |> List.map ~f:(fun e -> e.emeta.type_)
      |> Semantic_error.illtyped_stanlib_fn_app loc id.name
      |> Validate.error

let arg_match (x_ad, x_t) y =
  UnsizedType.check_of_same_type_mod_conv "" x_t y.emeta.type_
  && UnsizedType.autodifftype_can_convert x_ad y.emeta.ad_level

let args_match a b =
  List.length a = List.length b && List.for_all2_exn ~f:arg_match a b

let semantic_check_reduce_sum ~(is_cond_dist : sexp_bool)
    ~(loc : Location_span.t) (id : Ast.identifier) es =
  match es with
  | { emeta=
        { type_=
            UnsizedType.UFun
              ( ((_, sliced_arg_fun_type) as sliced_arg_fun)
                :: (_, UInt) :: (_, UInt) :: fun_args
              , ReturnType UReal ); _ }; _ }
    :: sliced :: {emeta= {type_= UInt; _}; _} :: args
    when arg_match sliced_arg_fun sliced
         && List.mem Stan_math_signatures.reduce_sum_slice_types
              sliced.emeta.type_ ~equal:( = )
         && List.mem Stan_math_signatures.reduce_sum_slice_types
              sliced_arg_fun_type ~equal:( = ) ->
      if args_match fun_args args then
        mk_typed_expression
          ~expr:(mk_fun_app ~is_cond_dist (StanLib, id, es))
          ~ad_level:(expr_ad_lub es) ~type_:UnsizedType.UReal ~loc
        |> Validate.ok
      else
        Semantic_error.illtyped_reduce_sum loc id.name
          (List.map ~f:type_of_expr_typed es)
          (sliced_arg_fun :: fun_args)
        |> Validate.error
  | _ ->
      es
      |> List.map ~f:type_of_expr_typed
      |> Semantic_error.illtyped_reduce_sum_generic loc id.name
      |> Validate.error

let semantic_check_variadic_ode ~(is_cond_dist : sexp_bool)
    ~(loc : Location_span.t) (id : Ast.identifier) es =
  let optional_tol_mandatory_args =
    if Stan_math_signatures.is_variadic_ode_tol_fn id.name then
      Stan_math_signatures.variadic_ode_tol_arg_types
    else []
  in
  let mandatory_arg_types =
    Stan_math_signatures.variadic_ode_mandatory_arg_types
    @ optional_tol_mandatory_args
  in
  let generic_variadic_ode_semantic_error =
    Semantic_error.illtyped_variadic_ode loc id.name
      (List.map ~f:type_of_expr_typed es)
      []
    |> Validate.error
  in
  let fun_arg_match (x_ad, x_t) (y_ad, y_t) =
    UnsizedType.check_of_same_type_mod_conv "" x_t y_t
    && UnsizedType.autodifftype_can_convert x_ad y_ad
  in
  let fun_args_match a b =
    List.length a = List.length b && List.for_all2_exn ~f:fun_arg_match a b
  in
  match es with
  | {emeta= {type_= UnsizedType.UFun (fun_args, ReturnType return_type); _}; _}
    :: args ->
      let num_of_mandatory_args =
        if Stan_math_signatures.is_variadic_ode_tol_fn id.name then 6 else 3
      in
      let mandatory_args, variadic_args =
        List.split_n args num_of_mandatory_args
      in
      let mandatory_fun_args, variadic_fun_args = List.split_n fun_args 2 in
      if
        fun_args_match mandatory_fun_args
          Stan_math_signatures.variadic_ode_mandatory_fun_args
        && UnsizedType.check_of_same_type_mod_conv "" return_type
             Stan_math_signatures.variadic_ode_fun_return_type
        && args_match mandatory_arg_types mandatory_args
      then
        if args_match variadic_fun_args variadic_args then
          mk_typed_expression
            ~expr:(mk_fun_app ~is_cond_dist (StanLib, id, es))
            ~ad_level:(expr_ad_lub es)
            ~type_:Stan_math_signatures.variadic_ode_return_type ~loc
          |> Validate.ok
        else
          Semantic_error.illtyped_variadic_ode loc id.name
            (List.map ~f:type_of_expr_typed es)
            fun_args
          |> Validate.error
      else generic_variadic_ode_semantic_error
  | _ -> generic_variadic_ode_semantic_error

let fn_kind_from_application (id : Ast.identifier) es =
  (* We need to check an application here, rather than a mere name of the
     function because, technically, user defined functions can shadow
     constants in StanLib. *)
  if
    Stan_math_signatures.stan_math_returntype id.name
      (List.map ~f:(fun x -> (x.emeta.ad_level, x.emeta.type_)) es)
    <> None
    || Symbol_table.look vm id.name = None
       && Stan_math_signatures.is_stan_math_function_name id.name
  then StanLib
  else UserDefined

(** Determines the function kind based on the identifier and performs the
    corresponding semantic check
*)
let semantic_check_fn ~(is_cond_dist : sexp_bool) ~(loc : Location_span.t)
    (id : Ast.identifier) es =
  match fn_kind_from_application id es with
  | StanLib when Stan_math_signatures.is_reduce_sum_fn id.name ->
      semantic_check_reduce_sum ~is_cond_dist ~loc id es
  | StanLib when Stan_math_signatures.is_variadic_ode_fn id.name ->
      semantic_check_variadic_ode ~is_cond_dist ~loc id es
  | StanLib -> semantic_check_fn_stan_math ~is_cond_dist ~loc id es
  | UserDefined -> semantic_check_fn_normal ~is_cond_dist ~loc id es

(* -- Ternary If ------------------------------------------------------------ *)

let semantic_check_ternary_if (loc : Location_span.t) (pe, te, fe) =
  Validate.(
    let err =
      Semantic_error.illtyped_ternary_if loc pe.emeta.type_ te.emeta.type_
        fe.emeta.type_
    in
    if pe.emeta.type_ = UInt then
      match UnsizedType.common_type (te.emeta.type_, fe.emeta.type_) with
      | Some type_ ->
          mk_typed_expression
            ~expr:(TernaryIf (pe, te, fe))
            ~ad_level:(expr_ad_lub [pe; te; fe])
            ~type_ ~loc
          |> ok
      | None -> error err
    else error err)

(* -- Binary (Infix) Operators ---------------------------------------------- *)

let semantic_check_binop (loc : Location_span.t) (op : Operator.t) (le, re) =
  Validate.(
    let err =
      Semantic_error.illtyped_binary_op loc op le.emeta.type_ re.emeta.type_
    in
    [le; re] |> List.map ~f:arg_type
    |> Stan_math_signatures.operator_stan_math_return_type op
    |> Option.value_map ~default:(error err) ~f:(function
         | ReturnType type_ ->
             mk_typed_expression
               ~expr:(BinOp (le, op, re))
               ~ad_level:(expr_ad_lub [le; re])
               ~type_ ~loc
             |> ok
         | Void -> error err ))

let to_exn v =
  v |> Validate.to_result
  |> Result.map_error ~f:Fmt.(to_to_string @@ list ~sep:cut Semantic_error.pp)
  |> Result.ok_or_failwith

let semantic_check_binop_exn (loc : Location_span.t) (op : Middle.Operator.t)
    (le, re) =
  semantic_check_binop loc op (le, re) |> to_exn

(* -- Prefix Operators ------------------------------------------------------ *)

let semantic_check_prefixop (loc : Location_span.t) (op : Middle.Operator.t) e
    =
  Validate.(
    let err = Semantic_error.illtyped_prefix_op loc op e.emeta.type_ in
    Stan_math_signatures.operator_stan_math_return_type op [arg_type e]
    |> Option.value_map ~default:(error err) ~f:(function
         | ReturnType type_ ->
             mk_typed_expression
               ~expr:(PrefixOp (op, e))
               ~ad_level:(expr_ad_lub [e])
               ~type_ ~loc
             |> ok
         | Void -> error err ))

(* -- Postfix operators ----------------------------------------------------- *)

let semantic_check_postfixop (loc : Location_span.t) (op : Middle.Operator.t) e
    =
  Validate.(
    let err = Semantic_error.illtyped_postfix_op loc op e.emeta.type_ in
    Stan_math_signatures.operator_stan_math_return_type op [arg_type e]
    |> Option.value_map ~default:(error err) ~f:(function
         | ReturnType type_ ->
             mk_typed_expression
               ~expr:(PostfixOp (e, op))
               ~ad_level:(expr_ad_lub [e])
               ~type_ ~loc
             |> ok
         | Void -> error err ))

(* -- Variables ------------------------------------------------------------- *)
let semantic_check_variable (cf : context_flags_record) (loc : Location_span.t)
    (id : Ast.identifier) =
  Validate.(
    match Symbol_table.look vm (Utils.stdlib_distribution_name id.name) with
    | None when not (Stan_math_signatures.is_stan_math_function_name id.name)
      ->
        Semantic_error.ident_not_in_scope loc id.name |> error
    | None ->
        mk_typed_expression ~expr:(Variable id)
          ~ad_level:
            (calculate_autodifftype cf MathLibrary UMathLibraryFunction)
          ~type_:UMathLibraryFunction ~loc
        |> ok
    | Some ((Param | TParam | GQuant), _) when cf.in_toplevel_decl ->
        Semantic_error.non_data_variable_size_decl loc |> error
    | Some _
      when Utils.is_unnormalized_distribution id.name
           && not
                ( (cf.in_fun_def && (cf.in_udf_dist_def || cf.in_lp_fun_def))
                || cf.current_block = Model ) ->
        Semantic_error.invalid_unnormalized_fn loc |> error
    | Some (originblock, type_) ->
        mk_typed_expression ~expr:(Variable id)
          ~ad_level:(calculate_autodifftype cf originblock type_)
          ~type_ ~loc
        |> ok)

(* -- Conditioned Distribution Application ---------------------------------- *)

let semantic_check_conddist_name ~(loc : Location_span.t) (id : Ast.identifier)
    =
  Validate.(
    if
      List.exists
        ~f:(fun x -> String.is_suffix id.name ~suffix:x)
        Utils.conditioning_suffices
    then ok ()
    else Semantic_error.conditional_notation_not_allowed loc |> error)

(* -- Array Expressions ----------------------------------------------------- *)

let check_consistent_types ad_level type_ es =
  let f state e =
    match state with
    | Error e -> Error e
    | Ok (ad, ty) -> (
        let ad =
          if UnsizedType.autodifftype_can_convert e.emeta.ad_level ad then
            e.emeta.ad_level
          else ad
        in
        match UnsizedType.common_type (ty, e.emeta.type_) with
        | Some ty -> Ok (ad, ty)
        | None -> Error (ty, e.emeta) )
  in
  List.fold ~init:(Ok (ad_level, type_)) ~f es

let semantic_check_array_expr ~(loc : Location_span.t) es =
  Validate.(
    match es with
    | [] -> Semantic_error.empty_array loc |> error
    | {emeta= {ad_level; type_; _}; _} :: elements -> (
      match check_consistent_types ad_level type_ elements with
      | Error (ty, meta) ->
          Semantic_error.mismatched_array_types meta.loc ty meta.type_ |> error
      | Ok (ad_level, type_) ->
          let type_ = UnsizedType.UArray type_ in
          mk_typed_expression ~expr:(ArrayExpr es) ~ad_level ~type_ ~loc |> ok
      ))

(* -- Row Vector Expresssion ------------------------------------------------ *)

let semantic_check_rowvector ~(loc : Location_span.t) es =
  Validate.(
    match es with
    | {emeta= {ad_level; type_= UnsizedType.URowVector; _}; _} :: elements -> (
      match check_consistent_types ad_level URowVector elements with
      | Ok (ad_level, _) ->
          mk_typed_expression ~expr:(RowVectorExpr es) ~ad_level ~type_:UMatrix
            ~loc
          |> ok
      | Error (_, meta) ->
          Semantic_error.invalid_matrix_types meta.loc meta.type_ |> error )
    | _ -> (
      match check_consistent_types DataOnly UReal es with
      | Ok (ad_level, _) ->
          mk_typed_expression ~expr:(RowVectorExpr es) ~ad_level
            ~type_:URowVector ~loc
          |> ok
      | Error (_, meta) ->
          Semantic_error.invalid_row_vector_types meta.loc meta.type_ |> error
      ))

(* -- Indexed Expressions --------------------------------------------------- *)
let tuple2 a b = (a, b)
let tuple3 a b c = (a, b, c)

let indexing_type idx =
  match idx with
  | Single {emeta= {type_= UnsizedType.UInt; _}; _} -> `Single
  | _ -> `Multi

let inferred_unsizedtype_of_indexed ~(loc : Location_span.t) ut indices =
  let rec aux type_ idcs =
    match (type_, idcs) with
    | _, [] -> Validate.ok type_
    | UnsizedType.UArray type_, `Single :: tl -> aux type_ tl
    | UArray type_, `Multi :: tl ->
        aux type_ tl |> Validate.map ~f:(fun t -> UnsizedType.UArray t)
    | (UVector | URowVector), [`Single] | UMatrix, [`Single; `Single] ->
        Validate.ok UnsizedType.UReal
    | (UVector | URowVector | UMatrix), [`Multi] | UMatrix, [`Multi; `Multi] ->
        Validate.ok type_
    | UMatrix, ([`Single] | [`Single; `Multi]) ->
        Validate.ok UnsizedType.URowVector
    | UMatrix, [`Multi; `Single] -> Validate.ok UnsizedType.UVector
    | UMatrix, _ :: _ :: _ :: _
     |(UVector | URowVector), _ :: _ :: _
     |(UInt | UReal | UFun _ | UMathLibraryFunction), _ :: _ ->
        Semantic_error.not_indexable loc ut (List.length indices)
        |> Validate.error
  in
  aux ut (List.map ~f:indexing_type indices)

let inferred_unsizedtype_of_indexed_exn ~(loc : Location_span.t) ut indices =
  inferred_unsizedtype_of_indexed ~loc ut indices |> to_exn

let inferred_ad_type_of_indexed at uindices =
  UnsizedType.lub_ad_type
    ( at
    :: List.map
         ~f:(function
           | All -> UnsizedType.DataOnly
           | Single ue1 | Upfrom ue1 | Downfrom ue1 ->
               UnsizedType.lub_ad_type [at; ue1.emeta.ad_level]
           | Between (ue1, ue2) ->
               UnsizedType.lub_ad_type
                 [at; ue1.emeta.ad_level; ue2.emeta.ad_level])
         uindices )

let rec semantic_check_indexed ~(loc : Location_span.t)
    ~(cf : context_flags_record) e indices =
  Validate.(
    indices
    |> List.map ~f:(semantic_check_index cf)
    |> sequence
    |> liftA2 tuple2 (semantic_check_expression cf e)
    >>= fun (ue, uindices) ->
    let at = inferred_ad_type_of_indexed ue.emeta.ad_level uindices in
    uindices
    |> inferred_unsizedtype_of_indexed ~loc ue.emeta.type_
    |> map ~f:(fun ut ->
           mk_typed_expression
             ~expr:(Indexed (ue, uindices))
             ~ad_level:at ~type_:ut ~loc ))

and semantic_check_index (cf : context_flags_record) = function
  | All -> Validate.ok All
  (* Check that indexes have int (container) type *)
  | Single e ->
      Validate.(
        semantic_check_expression cf e
        >>= fun ue ->
        if has_int_type ue || has_int_array_type ue then ok @@ Single ue
        else
          Semantic_error.int_intarray_or_range_expected ue.emeta.loc
            ue.emeta.type_
          |> error)
  | Upfrom e ->
      semantic_check_expression_of_int_type cf e "Range bound"
      |> Validate.map ~f:(fun e -> Upfrom e)
  | Downfrom e ->
      semantic_check_expression_of_int_type cf e "Range bound"
      |> Validate.map ~f:(fun e -> Downfrom e)
  | Between (e1, e2) ->
      let le = semantic_check_expression_of_int_type cf e1 "Range bound"
      and ue = semantic_check_expression_of_int_type cf e2 "Range bound" in
      Validate.liftA2 (fun l u -> Between (l, u)) le ue

(* -- Top-level expressions ------------------------------------------------- *)
and semantic_check_expression (cf : context_flags_record)
    ({emeta; expr} : Ast.untyped_expression) : Ast.typed_expression Validate.t
    =
  match expr with
  | TernaryIf (e1, e2, e3) ->
      let pe = semantic_check_expression cf e1
      and te = semantic_check_expression cf e2
      and fe = semantic_check_expression cf e3 in
      Validate.(liftA3 tuple3 pe te fe >>= semantic_check_ternary_if emeta.loc)
  | BinOp (e1, (op : Middle.Operator.t), e2) ->
      let le = semantic_check_expression cf e1
      and re = semantic_check_expression cf e2
      and warn_int_division (x, y) =
        match (x.emeta.type_, y.emeta.type_, op) with
        | UInt, UInt, Divide ->
            let hint ppf () =
              match (x.expr, y.expr) with
              | IntNumeral x, _ ->
                  Fmt.pf ppf "%s.0 / %a" x Pretty_printing.pp_expression y
              | _, Ast.IntNumeral y ->
                  Fmt.pf ppf "%a / %s.0" Pretty_printing.pp_expression x y
              | _ ->
                  Fmt.pf ppf "%a * 1.0 / %a" Pretty_printing.pp_expression x
                    Pretty_printing.pp_expression y
            in
            Fmt.pr
              "@[<v>@[<hov 0>Info: Found int division at %s:@]@   @[<hov \
               2>%a@]@,@[<hov>%a@]@   @[<hov 2>%a@]@,@[<hov>%a@]@]"
              (Location_span.to_string x.emeta.loc)
              Pretty_printing.pp_expression {expr; emeta} Fmt.text
              "Values will be rounded towards zero. If rounding is not \
               desired you can write the division as"
              hint () Fmt.text
              "If rounding is intended please use the integer division \
               operator %/%." ;
            (x, y)
        | _ -> (x, y)
      in
      Validate.(
        liftA2 tuple2 le re |> map ~f:warn_int_division
        |> apply_const (semantic_check_operator op)
        >>= semantic_check_binop emeta.loc op)
  | PrefixOp ((op : Middle.Operator.t), e) ->
      Validate.(
        semantic_check_expression cf e
        |> apply_const (semantic_check_operator op)
        >>= semantic_check_prefixop emeta.loc op)
  | PostfixOp (e, (op : Middle.Operator.t)) ->
      Validate.(
        semantic_check_expression cf e
        |> apply_const (semantic_check_operator op)
        >>= semantic_check_postfixop emeta.loc op)
  | Variable (id : Ast.identifier) ->
      semantic_check_variable cf emeta.loc id
      |> Validate.apply_const (semantic_check_identifier id)
  | IntNumeral s -> (
    match float_of_string_opt s with
    | Some i when i < 2_147_483_648.0 ->
        mk_typed_expression ~expr:(IntNumeral s) ~ad_level:DataOnly ~type_:UInt
          ~loc:emeta.loc
        |> Validate.ok
    | _ -> Semantic_error.bad_int_literal emeta.loc |> Validate.error )
  | RealNumeral s ->
      mk_typed_expression ~expr:(RealNumeral s) ~ad_level:DataOnly ~type_:UReal
        ~loc:emeta.loc
      |> Validate.ok
  | FunApp (_, id, es) ->
      semantic_check_funapp ~is_cond_dist:false id es cf emeta
  | CondDistApp (_, id, es) ->
      semantic_check_funapp ~is_cond_dist:true id es cf emeta
  | GetLP ->
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      if
        not
          ( cf.in_lp_fun_def || cf.current_block = Model
          || cf.current_block = TParam )
      then
        Semantic_error.target_plusequals_outisde_model_or_logprob emeta.loc
        |> Validate.error
      else
        mk_typed_expression ~expr:GetLP
          ~ad_level:(calculate_autodifftype cf cf.current_block UReal)
          ~type_:UReal ~loc:emeta.loc
        |> Validate.ok
  | GetTarget ->
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      if
        not
          ( cf.in_lp_fun_def || cf.current_block = Model
          || cf.current_block = TParam )
      then
        Semantic_error.target_plusequals_outisde_model_or_logprob emeta.loc
        |> Validate.error
      else
        mk_typed_expression ~expr:GetTarget
          ~ad_level:(calculate_autodifftype cf cf.current_block UReal)
          ~type_:UReal ~loc:emeta.loc
        |> Validate.ok
  | ArrayExpr es ->
      Validate.(
        es
        |> List.map ~f:(semantic_check_expression cf)
        |> sequence
        >>= fun ues -> semantic_check_array_expr ~loc:emeta.loc ues)
  | RowVectorExpr es ->
      Validate.(
        es
        |> List.map ~f:(semantic_check_expression cf)
        |> sequence
        >>= semantic_check_rowvector ~loc:emeta.loc)
  | Paren e ->
      semantic_check_expression cf e
      |> Validate.map ~f:(fun ue ->
             mk_typed_expression ~expr:(Paren ue) ~ad_level:ue.emeta.ad_level
               ~type_:ue.emeta.type_ ~loc:emeta.loc )
  | Indexed (e, indices) -> semantic_check_indexed ~loc:emeta.loc ~cf e indices

and semantic_check_funapp ~(is_cond_dist : sexp_bool) (id : Ast.identifier) es
    (cf : context_flags_record) emeta =
  let name_check =
    if is_cond_dist then semantic_check_conddist_name
    else semantic_check_fn_conditioning
  in
  Validate.(
    es
    |> List.map ~f:(semantic_check_expression cf)
    |> sequence
    >>= fun ues ->
    semantic_check_fn ~is_cond_dist ~loc:emeta.loc id ues
    |> apply_const (semantic_check_identifier id)
    |> apply_const (semantic_check_fn_map_rect ~loc:emeta.loc id ues)
    |> apply_const (name_check ~loc:emeta.loc id)
    |> apply_const (semantic_check_fn_target_plus_equals cf ~loc:emeta.loc id)
    |> apply_const (semantic_check_fn_rng cf ~loc:emeta.loc id)
    |> apply_const (semantic_check_unnormalized cf ~loc:emeta.loc id))

and semantic_check_expression_of_int_type (cf : context_flags_record) e name =
  Validate.(
    semantic_check_expression cf e
    >>= fun ue ->
    if has_int_type ue then ok ue
    else Semantic_error.int_expected ue.emeta.loc name ue.emeta.type_ |> error)

and semantic_check_expression_of_int_or_real_type (cf : context_flags_record) e
    name =
  Validate.(
    semantic_check_expression cf e
    >>= fun ue ->
    if has_int_or_real_type ue then ok ue
    else
      Semantic_error.int_or_real_expected ue.emeta.loc name ue.emeta.type_
      |> error)

let semantic_check_expression_of_scalar_or_type (cf : context_flags_record) t e
    name =
  Validate.(
    semantic_check_expression cf e
    >>= fun ue ->
    if UnsizedType.is_scalar_type ue.emeta.type_ || ue.emeta.type_ = t then
      ok ue
    else
      Semantic_error.scalar_or_type_expected ue.emeta.loc name t ue.emeta.type_
      |> error)

(* -- Sized Types ----------------------------------------------------------- *)
let rec semantic_check_sizedtype (cf : context_flags_record) = function
  | SizedType.SInt -> Validate.ok SizedType.SInt
  | SReal -> Validate.ok SizedType.SReal
  | SVector e ->
      semantic_check_expression_of_int_type cf e "Vector sizes"
      |> Validate.map ~f:(fun ue -> SizedType.SVector ue)
  | SRowVector e ->
      semantic_check_expression_of_int_type cf e "Row vector sizes"
      |> Validate.map ~f:(fun ue -> SizedType.SRowVector ue)
  | SMatrix (e1, e2) ->
      let ue1 = semantic_check_expression_of_int_type cf e1 "Matrix sizes"
      and ue2 = semantic_check_expression_of_int_type cf e2 "Matrix sizes" in
      Validate.liftA2 (fun ue1 ue2 -> SizedType.SMatrix (ue1, ue2)) ue1 ue2
  | SArray (st, e) ->
      let ust = semantic_check_sizedtype cf st
      and ue = semantic_check_expression_of_int_type cf e "Array sizes" in
      Validate.liftA2 (fun ust ue -> SizedType.SArray (ust, ue)) ust ue

(* -- Transformations ------------------------------------------------------- *)
let semantic_check_transformation (cf : context_flags_record) ut = function
  | Program.Identity -> Validate.ok Program.Identity
  | Lower e ->
      semantic_check_expression_of_scalar_or_type cf ut e "Lower bound"
      |> Validate.map ~f:(fun ue -> Program.Lower ue)
  | Upper e ->
      semantic_check_expression_of_scalar_or_type cf ut e "Upper bound"
      |> Validate.map ~f:(fun ue -> Program.Upper ue)
  | LowerUpper (e1, e2) ->
      let ue1 =
        semantic_check_expression_of_scalar_or_type cf ut e1 "Lower bound"
      and ue2 =
        semantic_check_expression_of_scalar_or_type cf ut e2 "Upper bound"
      in
      Validate.liftA2 (fun ue1 ue2 -> Program.LowerUpper (ue1, ue2)) ue1 ue2
  | Offset e ->
      semantic_check_expression_of_scalar_or_type cf ut e "Offset"
      |> Validate.map ~f:(fun ue -> Program.Offset ue)
  | Multiplier e ->
      semantic_check_expression_of_scalar_or_type cf ut e "Multiplier"
      |> Validate.map ~f:(fun ue -> Program.Multiplier ue)
  | OffsetMultiplier (e1, e2) ->
      let ue1 = semantic_check_expression_of_scalar_or_type cf ut e1 "Offset"
      and ue2 =
        semantic_check_expression_of_scalar_or_type cf ut e2 "Multiplier"
      in
      Validate.liftA2
        (fun ue1 ue2 -> Program.OffsetMultiplier (ue1, ue2))
        ue1 ue2
  | Ordered -> Validate.ok Program.Ordered
  | PositiveOrdered -> Validate.ok Program.PositiveOrdered
  | Simplex -> Validate.ok Program.Simplex
  | UnitVector -> Validate.ok Program.UnitVector
  | CholeskyCorr -> Validate.ok Program.CholeskyCorr
  | CholeskyCov -> Validate.ok Program.CholeskyCov
  | Correlation -> Validate.ok Program.Correlation
  | Covariance -> Validate.ok Program.Covariance

(* -- Printables ------------------------------------------------------------ *)

let semantic_check_printable (cf : context_flags_record) = function
  | PString s -> Validate.ok @@ PString s
  (* Print/reject expressions cannot be of function type. *)
  | PExpr e -> (
      Validate.(
        semantic_check_expression cf e
        >>= fun ue ->
        match ue.emeta.type_ with
        | UFun _ | UMathLibraryFunction ->
            Semantic_error.not_printable ue.emeta.loc |> error
        | _ -> ok @@ PExpr ue) )

(* -- Truncations ----------------------------------------------------------- *)

let semantic_check_truncation (cf : context_flags_record) = function
  | NoTruncate -> Validate.ok NoTruncate
  | TruncateUpFrom e ->
      semantic_check_expression_of_int_or_real_type cf e "Truncation bound"
      |> Validate.map ~f:(fun ue -> TruncateUpFrom ue)
  | TruncateDownFrom e ->
      semantic_check_expression_of_int_or_real_type cf e "Truncation bound"
      |> Validate.map ~f:(fun ue -> TruncateDownFrom ue)
  | TruncateBetween (e1, e2) ->
      let ue1 =
        semantic_check_expression_of_int_or_real_type cf e1 "Truncation bound"
      and ue2 =
        semantic_check_expression_of_int_or_real_type cf e2 "Truncation bound"
      in
      Validate.liftA2 (fun ue1 ue2 -> TruncateBetween (ue1, ue2)) ue1 ue2

(* == Statements ============================================================ *)

(* -- Non-returning function application ------------------------------------ *)

let semantic_check_nrfn_target ~(loc : Location_span.t)
    ~(cf : context_flags_record) (id : Ast.identifier) =
  Validate.(
    if
      String.is_suffix id.name ~suffix:"_lp"
      && not
           ( cf.in_lp_fun_def || cf.current_block = Model
           || cf.current_block = TParam )
    then Semantic_error.target_plusequals_outisde_model_or_logprob loc |> error
    else ok ())

let semantic_check_nrfn_normal ~(loc : Location_span.t) (id : Ast.identifier)
    es =
  Validate.(
    match Symbol_table.look vm id.name with
    | Some (_, UFun (listedtypes, Void))
      when UnsizedType.check_compatible_arguments_mod_conv id.name listedtypes
             (get_arg_types es) ->
        mk_typed_statement
          ~stmt:(NRFunApp (UserDefined, id, es))
          ~return_type:NoReturnType ~loc
        |> ok
    | Some (_, UFun (listedtypes, Void)) ->
        es
        |> List.map ~f:type_of_expr_typed
        |> Semantic_error.illtyped_userdefined_fn_app loc id.name listedtypes
             Void
        |> error
    | Some (_, UFun (_, ReturnType _)) ->
        Semantic_error.nonreturning_fn_expected_returning_found loc id.name
        |> error
    | Some _ ->
        Semantic_error.nonreturning_fn_expected_nonfn_found loc id.name
        |> error
    | None ->
        Semantic_error.nonreturning_fn_expected_undeclaredident_found loc
          id.name
        |> error)

let semantic_check_nrfn_stan_math ~(loc : Location_span.t)
    (id : Ast.identifier) (es : typed_expression sexp_list) =
  Validate.(
    match
      Stan_math_signatures.stan_math_returntype id.name (get_arg_types es)
    with
    | Some UnsizedType.Void ->
        mk_typed_statement
          ~stmt:(NRFunApp (StanLib, id, es))
          ~return_type:NoReturnType ~loc
        |> ok
    | Some (UnsizedType.ReturnType _) ->
        Semantic_error.nonreturning_fn_expected_returning_found loc id.name
        |> error
    | None ->
        es
        |> List.map ~f:type_of_expr_typed
        |> Semantic_error.illtyped_stanlib_fn_app loc id.name
        |> error)

let semantic_check_nr_fnkind ~(loc : Location_span.t) (id : Ast.identifier)
    (es : typed_expression sexp_list) =
  match fn_kind_from_application id es with
  | StanLib -> semantic_check_nrfn_stan_math ~loc id es
  | UserDefined -> semantic_check_nrfn_normal ~loc id es

let semantic_check_nr_fn_app ~(loc : Location_span.t)
    ~(cf : context_flags_record) (id : Ast.identifier)
    (es : untyped_expression sexp_list) =
  Validate.(
    es
    |> List.map ~f:(semantic_check_expression cf)
    |> sequence
    |> apply_const (semantic_check_identifier id)
    |> apply_const (semantic_check_nrfn_target ~loc ~cf id)
    >>= semantic_check_nr_fnkind ~loc id)

(* -- Assignment ------------------------------------------------------------ *)

let semantic_check_assignment_read_only ~(loc : Location_span.t)
    (id : Ast.identifier) =
  Validate.(
    if Symbol_table.get_read_only vm id.name then
      Semantic_error.cannot_assign_to_read_only loc id.name |> error
    else ok ())

(* Variables from previous blocks are read-only.
   In particular, data and parameters never assigned to
*)
let semantic_check_assignment_global ~(loc : Location_span.t)
    ~(cf : context_flags_record) ~block (id : Ast.identifier) =
  Validate.(
    if (not (Symbol_table.is_global vm id.name)) || block = cf.current_block
    then ok ()
    else Semantic_error.cannot_assign_to_global loc id.name |> error)

let mk_assignment_from_indexed_expr (assop : Ast.assignmentoperator) lhs rhs =
  Assignment
    {assign_lhs= Ast.lvalue_of_expr lhs; assign_op= assop; assign_rhs= rhs}

let semantic_check_assignment_operator ~(loc : Location_span.t) assop lhs rhs =
  let op =
    match assop with
    | Assign | ArrowAssign -> Operator.Equals
    | OperatorAssign (op : Middle.Operator.t) -> op
  in
  Validate.(
    let err =
      Semantic_error.illtyped_assignment loc op lhs.emeta.type_ rhs.emeta.type_
    in
    match assop with
    | Assign | ArrowAssign ->
        if
          UnsizedType.check_of_same_type_mod_array_conv "" lhs.emeta.type_
            rhs.emeta.type_
        then
          mk_typed_statement ~return_type:NoReturnType ~loc
            ~stmt:(mk_assignment_from_indexed_expr assop lhs rhs)
          |> ok
        else error err
    | OperatorAssign (op : Middle.Operator.t) ->
        List.map ~f:arg_type [lhs; rhs]
        |> Stan_math_signatures.assignmentoperator_stan_math_return_type op
        |> Option.value_map ~default:(error err) ~f:(function
             | ReturnType _ -> error err
             | Void ->
                 mk_typed_statement ~return_type:NoReturnType ~loc
                   ~stmt:(mk_assignment_from_indexed_expr assop lhs rhs)
                 |> ok ))

let semantic_check_assignment ~(loc : Location_span.t)
    ~(cf : context_flags_record) (assign_lhs : Ast.untyped_lval)
    (assign_op : Ast.assignmentoperator) (assign_rhs : untyped_expression) =
  let assign_id = Ast.id_of_lvalue assign_lhs in
  let lhs = expr_of_lvalue assign_lhs |> semantic_check_expression cf
  and assop = semantic_check_assignmentoperator assign_op
  and rhs = semantic_check_expression cf assign_rhs
  and block =
    Symbol_table.look vm assign_id.name
    |> Option.map ~f:(fun ((block : originblock), _) -> Validate.ok block)
    |> Option.value
         ~default:
           ( if Stan_math_signatures.is_stan_math_function_name assign_id.name
           then Validate.ok MathLibrary
           else
             Validate.error
             @@ Semantic_error.ident_not_in_scope loc assign_id.name )
  in
  Validate.(
    liftA2 tuple2 (liftA3 tuple3 lhs assop rhs) block
    >>= fun ((lhs, assop, rhs), block) ->
    semantic_check_assignment_operator ~loc assop lhs rhs
    |> apply_const (semantic_check_assignment_global ~loc ~cf ~block assign_id)
    |> apply_const (semantic_check_assignment_read_only ~loc assign_id))

(* -- Target plus-equals / Increment log-prob ------------------------------- *)

let semantic_check_target_pe_expr_type ~(loc : Location_span.t) e =
  match e.emeta.type_ with
  | UFun _ | UMathLibraryFunction ->
      Semantic_error.int_or_real_container_expected loc e.emeta.type_
      |> Validate.error
  | _ -> Validate.ok ()

let semantic_check_target_pe_usage ~(loc : Location_span.t)
    ~(cf : context_flags_record) =
  if cf.in_lp_fun_def || cf.current_block = Model then Validate.ok ()
  else
    Semantic_error.target_plusequals_outisde_model_or_logprob loc
    |> Validate.error

let semantic_check_target_pe ~(loc : Location_span.t)
    ~(cf : context_flags_record) (e : untyped_expression) =
  Validate.(
    semantic_check_expression cf e
    |> apply_const (semantic_check_target_pe_usage ~loc ~cf)
    >>= fun ue ->
    semantic_check_target_pe_expr_type ~loc ue
    |> map ~f:(fun _ ->
           mk_typed_statement ~stmt:(TargetPE ue) ~return_type:NoReturnType
             ~loc ))

let semantic_check_incr_logprob ~(loc : Location_span.t)
    ~(cf : context_flags_record) e =
  Validate.(
    semantic_check_expression cf e
    |> apply_const (semantic_check_target_pe_usage ~loc ~cf)
    >>= fun ue ->
    semantic_check_target_pe_expr_type ~loc ue
    |> map ~f:(fun _ ->
           mk_typed_statement ~stmt:(IncrementLogProb ue)
             ~return_type:NoReturnType ~loc ))

(* -- Tilde (Sampling notation) --------------------------------------------- *)

let semantic_check_sampling_pdf_pmf (id : Ast.identifier) =
  Validate.(
    if
      String.(
        is_suffix id.name ~suffix:"_lpdf"
        || is_suffix id.name ~suffix:"_lpmf"
        || is_suffix id.name ~suffix:"_lupdf"
        || is_suffix id.name ~suffix:"_lupmf")
    then error @@ Semantic_error.invalid_sampling_pdf_or_pmf id.id_loc
    else ok ())

let semantic_check_sampling_cdf_ccdf ~(loc : Location_span.t)
    (id : Ast.identifier) =
  Validate.(
    if
      String.(
        is_suffix id.name ~suffix:"_cdf" || is_suffix id.name ~suffix:"_ccdf")
    then error @@ Semantic_error.invalid_sampling_cdf_or_ccdf loc id.name
    else ok ())

(* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
let semantic_check_valid_sampling_pos ~(loc : Location_span.t)
    ~(cf : context_flags_record) =
  Validate.(
    if not (cf.in_lp_fun_def || cf.current_block = Model) then
      error @@ Semantic_error.target_plusequals_outisde_model_or_logprob loc
    else ok ())

let semantic_check_sampling_distribution ~(loc : Location_span.t)
    (id : Ast.identifier) (arguments : 'a abstract_typed_expression sexp_list)
    =
  let name = id.name
  and argumenttypes = List.map ~f:arg_type arguments
  and is_real_rt = function
    | UnsizedType.ReturnType UReal -> true
    | _ -> false
  in
  let is_name_w_suffix_sampling_dist (suffix : string) =
    Stan_math_signatures.stan_math_returntype (name ^ suffix) argumenttypes
    |> Option.value_map ~default:false ~f:is_real_rt
  in
  let is_sampling_dist_in_math =
    List.exists ~f:is_name_w_suffix_sampling_dist
      (Utils.distribution_suffices @ Utils.unnormalized_suffices)
    && name <> "binomial_coefficient"
    && name <> "multiply"
  in
  let is_name_w_suffix_udf_sampling_dist (suffix : string) =
    match Symbol_table.look vm (name ^ suffix) with
    | Some (Functions, UFun (listedtypes, ReturnType UReal)) ->
        UnsizedType.check_compatible_arguments_mod_conv name listedtypes
          argumenttypes
    | _ -> false
  in
  let is_udf_sampling_dist =
    List.exists ~f:is_name_w_suffix_udf_sampling_dist
      (Utils.distribution_suffices @ Utils.unnormalized_suffices)
  in
  Validate.(
    if is_sampling_dist_in_math || is_udf_sampling_dist then ok ()
    else error @@ Semantic_error.invalid_sampling_no_such_dist loc name)

let cumulative_density_is_defined (id : Ast.identifier)
    (arguments : 'a abstract_typed_expression sexp_list) =
  let name = id.name
  and argumenttypes = List.map ~f:arg_type arguments
  and is_real_rt = function
    | UnsizedType.ReturnType UReal -> true
    | _ -> false
  in
  let is_real_rt_for_suffix suffix =
    Stan_math_signatures.stan_math_returntype (name ^ suffix) argumenttypes
    |> Option.value_map ~default:false ~f:is_real_rt
  and valid_arg_types_for_suffix suffix =
    match Symbol_table.look vm (name ^ suffix) with
    | Some (Functions, UFun (listedtypes, ReturnType UReal)) ->
        UnsizedType.check_compatible_arguments_mod_conv name listedtypes
          argumenttypes
    | _ -> false
  in
  ( is_real_rt_for_suffix "_lcdf"
  || valid_arg_types_for_suffix "_lcdf"
  || is_real_rt_for_suffix "_cdf_log"
  || valid_arg_types_for_suffix "_cdf_log" )
  && ( is_real_rt_for_suffix "_lccdf"
     || valid_arg_types_for_suffix "_lccdf"
     || is_real_rt_for_suffix "_ccdf_log"
     || valid_arg_types_for_suffix "_ccdf_log" )

let can_truncate_distribution ~(loc : Location_span.t) (arg : typed_expression)
    = function
  | NoTruncate -> Validate.ok ()
  | _ ->
      if UnsizedType.is_scalar_type arg.emeta.type_ then Validate.ok ()
      else Validate.error @@ Semantic_error.multivariate_truncation loc

let semantic_check_sampling_cdf_defined ~(loc : Location_span.t)
    (id : Ast.identifier)
    (truncation : 'a abstract_typed_expression truncation)
    (args : 'a abstract_typed_expression sexp_list) =
  Validate.(
    match truncation with
    | NoTruncate -> ok ()
    | TruncateUpFrom e when cumulative_density_is_defined id (e :: args) ->
        ok ()
    | TruncateDownFrom e when cumulative_density_is_defined id (e :: args) ->
        ok ()
    | TruncateBetween (e1, e2)
      when cumulative_density_is_defined id (e1 :: args)
           && cumulative_density_is_defined id (e2 :: args) ->
        ok ()
    | _ -> error @@ Semantic_error.invalid_truncation_cdf_or_ccdf loc)

let semantic_check_tilde ~(loc : Location_span.t) ~(cf : context_flags_record)
    (distribution : Ast.identifier)
    (truncation : untyped_expression truncation) (arg : untyped_expression)
    (args : untyped_expression sexp_list) =
  Validate.(
    let ue = semantic_check_expression cf arg
    and ues = List.map ~f:(semantic_check_expression cf) args |> sequence
    and ut = semantic_check_truncation cf truncation in
    liftA3 tuple3 ut ue ues
    |> apply_const (semantic_check_identifier distribution)
    |> apply_const (semantic_check_sampling_pdf_pmf distribution)
    |> apply_const (semantic_check_valid_sampling_pos ~loc ~cf)
    |> apply_const (semantic_check_sampling_cdf_ccdf ~loc distribution)
    >>= fun ( (truncation : typed_expression truncation)
            , (arg : typed_expression)
            , (args : typed_expression sexp_list) ) ->
    semantic_check_sampling_distribution ~loc distribution (arg :: args)
    |> apply_const
         (semantic_check_sampling_cdf_defined ~loc distribution truncation args)
    |> apply_const (can_truncate_distribution ~loc arg truncation)
    |> map ~f:(fun _ ->
           let stmt = Tilde {arg; distribution; args; truncation} in
           mk_typed_statement ~stmt ~loc ~return_type:NoReturnType ))

(* -- Break ----------------------------------------------------------------- *)
(* Break and continue only occur in loops. *)
let semantic_check_break ~(loc : Location_span.t) ~(cf : context_flags_record)
    =
  Validate.(
    if cf.loop_depth = 0 then Semantic_error.break_outside_loop loc |> error
    else mk_typed_statement ~stmt:Break ~return_type:NoReturnType ~loc |> ok)

(* -- Continue -------------------------------------------------------------- *)

let semantic_check_continue ~(loc : Location_span.t)
    ~(cf : context_flags_record) =
  Validate.(
    (* Break and continue only occur in loops. *)
    if cf.loop_depth = 0 then Semantic_error.continue_outside_loop loc |> error
    else mk_typed_statement ~stmt:Continue ~return_type:NoReturnType ~loc |> ok)

(* -- Return ---------------------------------------------------------------- *)

(** No returns outside of function definitions
    In case of void function, no return statements anywhere
*)
let semantic_check_return ~(loc : Location_span.t) ~(cf : context_flags_record)
    e =
  Validate.(
    if not cf.in_returning_fun_def then
      Semantic_error.expression_return_outside_returning_fn loc |> error
    else
      semantic_check_expression cf e
      |> map ~f:(fun ue ->
             mk_typed_statement ~stmt:(Return ue)
               ~return_type:(Complete (ReturnType ue.emeta.type_)) ~loc ))

(* -- Return `void` --------------------------------------------------------- *)

let semantic_check_returnvoid ~(loc : Location_span.t)
    ~(cf : context_flags_record) =
  Validate.(
    if (not cf.in_fun_def) || cf.in_returning_fun_def then
      Semantic_error.void_ouside_nonreturning_fn loc |> error
    else
      mk_typed_statement ~stmt:ReturnVoid ~return_type:(Complete Void) ~loc
      |> ok)

(* -- Print ----------------------------------------------------------------- *)

let semantic_check_print ~(loc : Location_span.t) ~(cf : context_flags_record)
    ps =
  Validate.(
    ps
    |> List.map ~f:(semantic_check_printable cf)
    |> sequence
    |> map ~f:(fun ups ->
           mk_typed_statement ~stmt:(Print ups) ~return_type:NoReturnType ~loc
       ))

(* -- Reject ---------------------------------------------------------------- *)

let semantic_check_reject ~(loc : Location_span.t) ~(cf : context_flags_record)
    ps =
  Validate.(
    ps
    |> List.map ~f:(semantic_check_printable cf)
    |> sequence
    |> map ~f:(fun ups ->
           mk_typed_statement ~stmt:(Reject ups) ~return_type:AnyReturnType
             ~loc ))

(* -- Skip ------------------------------------------------------------------ *)

let semantic_check_skip ~(loc : Location_span.t) =
  mk_typed_statement ~stmt:Skip ~return_type:NoReturnType ~loc |> Validate.ok

(* -- If-Then-Else ---------------------------------------------------------- *)

let try_compute_ifthenelse_statement_returntype (loc : Location_span.t) srt1
    srt2 =
  match (srt1, srt2) with
  | Complete rt1, Complete rt2 ->
      lub_rt loc rt1 rt2
      |> Validate.map ~f:(fun (t : UnsizedType.returntype) -> Complete t)
  | Incomplete rt1, Incomplete rt2
   |Complete rt1, Incomplete rt2
   |Incomplete rt1, Complete rt2 ->
      lub_rt loc rt1 rt2
      |> Validate.map ~f:(fun (t : UnsizedType.returntype) -> Incomplete t)
  | AnyReturnType, NoReturnType
   |NoReturnType, AnyReturnType
   |NoReturnType, NoReturnType ->
      Validate.ok NoReturnType
  | AnyReturnType, Incomplete rt
   |Incomplete rt, AnyReturnType
   |Complete rt, NoReturnType
   |NoReturnType, Complete rt
   |NoReturnType, Incomplete rt
   |Incomplete rt, NoReturnType ->
      Validate.ok @@ Incomplete rt
  | Complete rt, AnyReturnType | AnyReturnType, Complete rt ->
      Validate.ok @@ Complete rt
  | AnyReturnType, AnyReturnType -> Validate.ok AnyReturnType

let rec semantic_check_if_then_else ~(loc : Location_span.t)
    ~(cf : context_flags_record) (pred_e : untyped_expression)
    (s_true : untyped_statement) (s_false_opt : untyped_statement sexp_option)
    =
  let us1 = semantic_check_statement cf s_true
  and uos2 =
    s_false_opt
    |> Option.map ~f:(fun s ->
           semantic_check_statement cf s |> Validate.map ~f:Option.some )
    |> Option.value ~default:(Validate.ok None)
  and ue =
    semantic_check_expression_of_int_or_real_type cf pred_e
      "Condition in conditional"
  in
  Validate.(
    liftA3 tuple3 ue us1 uos2
    >>= fun (ue, us1, uos2) ->
    let stmt = IfThenElse (ue, us1, uos2)
    and srt1 = us1.smeta.return_type
    and srt2 =
      uos2
      |> Option.map ~f:(fun s -> s.smeta.return_type)
      |> Option.value ~default:NoReturnType
    in
    try_compute_ifthenelse_statement_returntype loc srt1 srt2
    |> map ~f:(fun return_type -> mk_typed_statement ~stmt ~return_type ~loc))

(* -- While Statements ------------------------------------------------------ *)
and semantic_check_while ~(loc : Location_span.t) ~(cf : context_flags_record)
    e s =
  let us = semantic_check_statement {cf with loop_depth= cf.loop_depth + 1} s
  and ue =
    semantic_check_expression_of_int_or_real_type cf e
      "Condition in while-loop"
  in
  Validate.liftA2
    (fun ue us ->
      mk_typed_statement
        ~stmt:(While (ue, us))
        ~return_type:us.smeta.return_type ~loc )
    ue us

(* -- For Statements -------------------------------------------------------- *)
and semantic_check_loop_body ~(cf : context_flags_record)
    (loop_var : Ast.identifier) (loop_var_ty : UnsizedType.t)
    (loop_body : Ast.untyped_statement) =
  Symbol_table.begin_scope vm ;
  let is_fresh_var = check_fresh_variable loop_var false in
  Symbol_table.enter vm loop_var.name (cf.current_block, loop_var_ty) ;
  (* Check that function args and loop identifiers are not modified in
      function. (passed by const ref) *)
  Symbol_table.set_read_only vm loop_var.name ;
  let us =
    semantic_check_statement {cf with loop_depth= cf.loop_depth + 1} loop_body
    |> Validate.apply_const is_fresh_var
  in
  Symbol_table.end_scope vm ; us

and semantic_check_for ~(loc : Location_span.t) ~(cf : context_flags_record)
    (loop_var : Ast.identifier) (lower_bound_e : untyped_expression)
    (upper_bound_e : untyped_expression) (loop_body : untyped_statement) =
  let ue1 =
    semantic_check_expression_of_int_type cf lower_bound_e
      "Lower bound of for-loop"
  and ue2 =
    semantic_check_expression_of_int_type cf upper_bound_e
      "Upper bound of for-loop"
  in
  Validate.(
    liftA2 tuple2 ue1 ue2
    |> apply_const (semantic_check_identifier loop_var)
    >>= fun ((ue1 : typed_expression), (ue2 : typed_expression)) ->
    semantic_check_loop_body ~cf loop_var UInt loop_body
    |> map ~f:(fun us ->
           mk_typed_statement
             ~stmt:
               (For
                  { loop_variable= loop_var
                  ; lower_bound= ue1
                  ; upper_bound= ue2
                  ; loop_body= us })
             ~return_type:us.smeta.return_type ~loc ))

(* -- Foreach Statements ---------------------------------------------------- *)
and semantic_check_foreach_loop_identifier_type ~(loc : Location_span.t)
    (ty : UnsizedType.t) =
  Validate.(
    match ty with
    | UnsizedType.UArray ut -> ok ut
    | UVector | URowVector | UMatrix -> ok UnsizedType.UReal
    | _ ->
        Semantic_error.array_vector_rowvector_matrix_expected loc ty |> error)

and semantic_check_foreach ~(loc : Location_span.t)
    ~(cf : context_flags_record) (loop_var : Ast.identifier)
    (foreach_expr : untyped_expression) (loop_body : untyped_statement) =
  Validate.(
    semantic_check_expression cf foreach_expr
    |> apply_const (semantic_check_identifier loop_var)
    >>= fun ue ->
    semantic_check_foreach_loop_identifier_type ~loc:ue.emeta.loc
      ue.emeta.type_
    >>= fun loop_var_ty ->
    semantic_check_loop_body ~cf loop_var loop_var_ty loop_body
    |> map ~f:(fun us ->
           mk_typed_statement
             ~stmt:(ForEach (loop_var, ue, us))
             ~return_type:us.smeta.return_type ~loc ))

(* -- Blocks ---------------------------------------------------------------- *)
and stmt_is_escape {stmt; _} =
  match stmt with
  | Break | Continue | Reject _ | Return _ | ReturnVoid -> true
  | _ -> false

and list_until_escape xs =
  let rec aux accu = function
    | next :: next' :: _ when stmt_is_escape next' ->
        List.rev (next' :: next :: accu)
    | next :: rest -> aux (next :: accu) rest
    | [] -> List.rev accu
  in
  aux [] xs

and try_compute_block_statement_returntype (loc : Location_span.t)
    (srt1 : statement_returntype) (srt2 : statement_returntype) =
  match (srt1, srt2) with
  | Complete rt1, Complete rt2 | Incomplete rt1, Complete rt2 ->
      lub_rt loc rt1 rt2 |> Validate.map ~f:(fun t -> Complete t)
  | Incomplete rt1, Incomplete rt2 | Complete rt1, Incomplete rt2 ->
      lub_rt loc rt1 rt2 |> Validate.map ~f:(fun t -> Incomplete t)
  | NoReturnType, NoReturnType -> Validate.ok NoReturnType
  | AnyReturnType, Incomplete rt
   |Complete rt, NoReturnType
   |NoReturnType, Incomplete rt
   |Incomplete rt, NoReturnType ->
      Validate.ok @@ Incomplete rt
  | NoReturnType, Complete rt
   |Complete rt, AnyReturnType
   |Incomplete rt, AnyReturnType
   |AnyReturnType, Complete rt ->
      Validate.ok @@ Complete rt
  | AnyReturnType, NoReturnType
   |NoReturnType, AnyReturnType
   |AnyReturnType, AnyReturnType ->
      Validate.ok AnyReturnType

and semantic_check_block ~(loc : Location_span.t) ~(cf : context_flags_record)
    stmts =
  Symbol_table.begin_scope vm ;
  (* Any statements after a break or continue or return or reject
     do not count for the return type.
  *)
  let validated_stmts =
    List.map ~f:(semantic_check_statement cf) stmts |> Validate.sequence
  in
  Symbol_table.end_scope vm ;
  Validate.(
    validated_stmts
    >>= fun xs ->
    let return_ty =
      xs |> list_until_escape
      |> List.map ~f:(fun s -> s.smeta.return_type)
      |> List.fold ~init:(ok NoReturnType) ~f:(fun accu x ->
             accu >>= fun y -> try_compute_block_statement_returntype loc y x
         )
    in
    map return_ty ~f:(fun return_type ->
        mk_typed_statement ~stmt:(Block xs) ~return_type ~loc ))

and semantic_check_profile ~(loc : Location_span.t)
    ~(cf : context_flags_record) (name : string)
    (stmts : untyped_statement sexp_list) =
  Symbol_table.begin_scope vm ;
  (* Any statements after a break or continue or return or reject
     do not count for the return type.
  *)
  let validated_stmts =
    List.map ~f:(semantic_check_statement cf) stmts |> Validate.sequence
  in
  Symbol_table.end_scope vm ;
  Validate.(
    validated_stmts
    >>= fun xs ->
    let return_ty =
      xs |> list_until_escape
      |> List.map ~f:(fun s -> s.smeta.return_type)
      |> List.fold ~init:(ok NoReturnType) ~f:(fun accu x ->
             accu >>= fun y -> try_compute_block_statement_returntype loc y x
         )
    in
    map return_ty ~f:(fun return_type ->
        mk_typed_statement ~stmt:(Profile (name, xs)) ~return_type ~loc ))

(* -- Variable Declarations ------------------------------------------------- *)
and semantic_check_var_decl_bounds ~(loc : Location_span.t)
    (is_global : sexp_bool) (sized_ty : typed_expression SizedType.t)
    (trans : typed_expression Program.transformation) =
  let is_real {emeta; _} = emeta.type_ = UReal in
  let is_valid_transformation =
    match trans with
    | Program.Lower e -> is_real e
    | Upper e -> is_real e
    | LowerUpper (e1, e2) -> is_real e1 || is_real e2
    | _ -> false
  in
  Validate.(
    if is_global && sized_ty = SizedType.SInt && is_valid_transformation then
      Semantic_error.non_int_bounds loc |> error
    else ok ())

and semantic_check_transformed_param_ty ~(loc : Location_span.t)
    ~(cf : context_flags_record) (is_global : sexp_bool)
    (unsized_ty : UnsizedType.t) =
  Validate.(
    if
      is_global
      && (cf.current_block = Param || cf.current_block = TParam)
      && UnsizedType.contains_int unsized_ty
    then Semantic_error.transformed_params_int loc |> error
    else ok ())

and semantic_check_var_decl_initial_value ~(loc : Location_span.t)
    ~(cf : context_flags_record) (id : Ast.identifier)
    (init_val_opt : Ast.untyped_expression sexp_option) =
  init_val_opt
  |> Option.value_map ~default:(Validate.ok None)
       ~f:(fun (e : untyped_expression) ->
         let stmt =
           Assignment
             { assign_lhs= {lval= LVariable id; lmeta= {loc}}
             ; assign_op= Assign
             ; assign_rhs= e }
         in
         mk_untyped_statement ~loc ~stmt
         |> semantic_check_statement cf
         |> Validate.map ~f:(fun (ts : Ast.typed_statement) ->
                match (ts.stmt, ts.smeta.return_type) with
                | Assignment {assign_rhs= ue; _}, NoReturnType -> Some ue
                | _ ->
                    let msg =
                      "semantic_check_var_decl: `Assignment` expected."
                    in
                    fatal_error ~msg () ) )

and semantic_check_var_decl ~(loc : Location_span.t)
    ~(cf : context_flags_record) (sized_ty : untyped_expression SizedType.t)
    (trans : untyped_expression Program.transformation) (id : Ast.identifier)
    (init : untyped_expression sexp_option) (is_global : sexp_bool) =
  let checked_stmt =
    semantic_check_sizedtype {cf with in_toplevel_decl= is_global} sized_ty
  in
  Validate.(
    let checked_trans =
      checked_stmt
      >>= fun ust ->
      semantic_check_transformation cf (SizedType.to_unsized ust) trans
    in
    liftA2 tuple2 checked_stmt checked_trans
    |> apply_const (semantic_check_identifier id)
    |> apply_const (check_fresh_variable id false)
    >>= fun (ust, utrans) ->
    let ut = SizedType.to_unsized ust in
    Symbol_table.enter vm id.name (cf.current_block, ut) ;
    semantic_check_var_decl_initial_value ~loc ~cf id init
    |> apply_const (semantic_check_var_decl_bounds ~loc is_global ust utrans)
    |> apply_const (semantic_check_transformed_param_ty ~loc ~cf is_global ut)
    |> map ~f:(fun uinit ->
           let stmt =
             VarDecl
               { decl_type= Sized ust
               ; transformation= utrans
               ; identifier= id
               ; initial_value= uinit
               ; is_global }
           in
           mk_typed_statement ~stmt ~loc ~return_type:NoReturnType ))

(* -- Function definitions -------------------------------------------------- *)
and semantic_check_fundef_overloaded ~(loc : Location_span.t)
    (id : Ast.identifier)
    (arg_tys : (UnsizedType.autodifftype * UnsizedType.t) sexp_list)
    (rt : UnsizedType.returntype) =
  Validate.(
    (* User defined functions cannot be overloaded *)
    if Symbol_table.check_is_unassigned vm id.name then
      match Symbol_table.look vm id.name with
      | Some (Functions, UFun (arg_tys', rt'))
        when arg_tys' = arg_tys && rt' = rt ->
          ok ()
      | _ ->
          Symbol_table.look vm id.name
          |> Option.map ~f:snd
          |> Semantic_error.mismatched_fn_def_decl loc id.name
          |> error
    else check_fresh_variable id true)

(** WARNING: side effecting *)
and semantic_check_fundef_decl ~(loc : Location_span.t) (id : Ast.identifier)
    (body :
      (untyped_expression, located_meta, untyped_lval, unit) statement_with) =
  Validate.(
    match body with
    | {stmt= Skip; _} ->
        if Symbol_table.check_is_unassigned vm id.name then
          error @@ Semantic_error.fn_decl_without_def loc
        else
          let () = Symbol_table.set_is_unassigned vm id.name in
          ok ()
    | _ ->
        Symbol_table.set_is_assigned vm id.name ;
        ok ())

and semantic_check_fundef_dist_rt ~(loc : Location_span.t)
    (id : Ast.identifier) (return_ty : UnsizedType.returntype) =
  Validate.(
    let is_dist =
      List.exists
        ~f:(fun x -> String.is_suffix id.name ~suffix:x)
        Utils.conditioning_suffices_w_log
    in
    if is_dist then
      match return_ty with
      | UnsizedType.ReturnType UReal -> ok ()
      | _ -> error @@ Semantic_error.non_real_prob_fn_def loc
    else ok ())

and semantic_check_pdf_fundef_first_arg_ty ~(loc : Location_span.t)
    (id : Ast.identifier)
    (arg_tys : (UnsizedType.autodifftype * UnsizedType.t) sexp_list) =
  Validate.(
    (* TODO: I think these kind of functions belong with the type definition *)
    let is_real_type = function
      | UnsizedType.UReal | UVector | URowVector | UMatrix
       |UArray UReal
       |UArray UVector
       |UArray URowVector
       |UArray UMatrix ->
          true
      | _ -> false
    in
    if String.is_suffix id.name ~suffix:"_lpdf" then
      List.hd arg_tys
      |> Option.value_map
           ~default:
             (error @@ Semantic_error.prob_density_non_real_variate loc None)
           ~f:(fun (_, rt) ->
             if is_real_type rt then ok ()
             else
               error
               @@ Semantic_error.prob_density_non_real_variate loc (Some rt) )
    else ok ())

and semantic_check_pmf_fundef_first_arg_ty ~(loc : Location_span.t)
    (id : Ast.identifier)
    (arg_tys : (UnsizedType.autodifftype * UnsizedType.t) sexp_list) =
  Validate.(
    (* TODO: I think these kind of functions belong with the type definition *)
    let is_int_type = function
      | UnsizedType.UInt | UArray UInt -> true
      | _ -> false
    in
    if String.is_suffix id.name ~suffix:"_lpmf" then
      List.hd arg_tys
      |> Option.value_map
           ~default:(error @@ Semantic_error.prob_mass_non_int_variate loc None)
           ~f:(fun (_, (rt : UnsizedType.t)) ->
             if is_int_type rt then ok ()
             else
               error @@ Semantic_error.prob_mass_non_int_variate loc (Some rt)
             )
    else ok ())

(* All function arguments are distinct *)
and semantic_check_fundef_distinct_arg_ids ~(loc : Location_span.t) arg_names =
  Validate.(
    if dup_exists arg_names then
      error @@ Semantic_error.duplicate_arg_names loc
    else ok ())

(* Check that every trace through function body contains return statement of right type *)
and semantic_check_fundef_return_tys ~(loc : Location_span.t)
    (id : Ast.identifier) (return_type : UnsizedType.returntype)
    (body :
      ( typed_expression
      , stmt_typed_located_meta
      , typed_lval
      , fun_kind )
      statement_with) =
  Validate.(
    if
      Symbol_table.check_is_unassigned vm id.name
      || check_of_compatible_return_type return_type body.smeta.return_type
    then ok ()
    else error @@ Semantic_error.incompatible_return_types loc)

and semantic_check_fundef ~(loc : Location_span.t) ~(cf : context_flags_record)
    (return_ty : UnsizedType.returntype) (id : Ast.identifier)
    (args : (UnsizedType.autodifftype * UnsizedType.t * identifier) sexp_list)
    (body :
      (untyped_expression, located_meta, untyped_lval, unit) statement_with) =
  let uargs =
    List.map args
      ~f:(fun ( (at : UnsizedType.autodifftype)
              , (ut : UnsizedType.t)
              , (id : Ast.identifier) )
         ->
        Validate.(
          semantic_check_autodifftype at
          |> apply_const (semantic_check_unsizedtype ut)
          |> apply_const (semantic_check_identifier id)
          |> map ~f:(fun at -> (at, ut, id))) )
    |> Validate.sequence
  in
  Validate.(
    uargs
    |> apply_const (semantic_check_identifier id)
    |> apply_const (semantic_check_returntype return_ty)
    >>= fun (uargs :
              (UnsizedType.autodifftype * UnsizedType.t * identifier) sexp_list)
            ->
    let urt = return_ty in
    let uarg_types =
      List.map
        ~f:(fun ((w : UnsizedType.autodifftype), (y : UnsizedType.t), _) ->
          (w, y) )
        uargs
    in
    let uarg_identifiers =
      List.map ~f:(fun (_, _, (z : Ast.identifier)) -> z) uargs
    in
    let uarg_names = List.map ~f:(fun x -> x.name) uarg_identifiers in
    semantic_check_fundef_overloaded ~loc id uarg_types urt
    |> apply_const (semantic_check_fundef_decl ~loc id body)
    >>= fun _ ->
    (* WARNING: SIDE EFFECTING *)
    Symbol_table.enter vm id.name (Functions, UFun (uarg_types, urt)) ;
    (* Check that function args and loop identifiers are not modified in
       function. (passed by const ref)*)
    List.iter ~f:(Symbol_table.set_read_only vm) uarg_names ;
    semantic_check_fundef_dist_rt ~loc id urt
    |> apply_const (semantic_check_pdf_fundef_first_arg_ty ~loc id uarg_types)
    |> apply_const (semantic_check_pmf_fundef_first_arg_ty ~loc id uarg_types)
    >>= fun _ ->
    (* WARNING: SIDE EFFECTING *)
    Symbol_table.begin_scope vm ;
    List.map ~f:(fun x -> check_fresh_variable x false) uarg_identifiers
    |> sequence
    |> apply_const (semantic_check_fundef_distinct_arg_ids ~loc uarg_names)
    >>= fun _ ->
    (* TODO: Bob was suggesting that function arguments must be allowed to
        shadow user defined functions but not library functions.
        Should we allow for that?
    *)
    (* We treat DataOnly arguments as if they are data and AutoDiffable arguments
        as if they are parameters, for the purposes of type checking.
    *)
    (* WARNING: SIDE EFFECTING *)
    let _ : unit Base.List.Or_unequal_lengths.t =
      List.iter2 ~f:(Symbol_table.enter vm) uarg_names
        (List.map
           ~f:(function
             | UnsizedType.DataOnly, (ut : UnsizedType.t) -> (Data, ut)
             | AutoDiffable, ut -> (Param, ut))
           uarg_types)
    and context =
      let is_udf_dist (name : string) =
        List.exists
          ~f:(fun suffix -> String.is_suffix name ~suffix)
          Utils.distribution_suffices
      in
      { cf with
        in_fun_def= true
      ; in_rng_fun_def= String.is_suffix id.name ~suffix:"_rng"
      ; in_lp_fun_def= String.is_suffix id.name ~suffix:"_lp"
      ; in_udf_dist_def= is_udf_dist id.name
      ; in_returning_fun_def= urt <> Void }
    in
    let body' = semantic_check_statement context body in
    body'
    >>= fun (ub : typed_statement) ->
    semantic_check_fundef_return_tys ~loc id urt ub
    |> map ~f:(fun _ ->
           (* WARNING: SIDE EFFECTING *)
           Symbol_table.end_scope vm ;
           let stmt =
             FunDef {returntype= urt; funname= id; arguments= uargs; body= ub}
           in
           mk_typed_statement ~return_type:NoReturnType ~loc ~stmt ))

(* -- Top-level Statements -------------------------------------------------- *)
and semantic_check_statement (cf : context_flags_record)
    (s : Ast.untyped_statement) : Ast.typed_statement Validate.t =
  let loc = s.smeta.loc in
  match s.stmt with
  | NRFunApp (_, (id : Ast.identifier), (es : untyped_expression sexp_list)) ->
      semantic_check_nr_fn_app ~loc ~cf id es
  | Assignment {assign_lhs; assign_op; assign_rhs} ->
      semantic_check_assignment ~loc ~cf assign_lhs assign_op assign_rhs
  | TargetPE (e : untyped_expression) -> semantic_check_target_pe ~loc ~cf e
  | IncrementLogProb (e : untyped_expression) ->
      semantic_check_incr_logprob ~loc ~cf e
  | Tilde {arg; distribution; args; truncation} ->
      semantic_check_tilde ~loc ~cf distribution truncation arg args
  | Break -> semantic_check_break ~loc ~cf
  | Continue -> semantic_check_continue ~loc ~cf
  | Return e -> semantic_check_return ~loc ~cf e
  | ReturnVoid -> semantic_check_returnvoid ~loc ~cf
  | Print (ps : untyped_expression printable sexp_list) ->
      semantic_check_print ~loc ~cf ps
  | Reject (ps : untyped_expression printable sexp_list) ->
      semantic_check_reject ~loc ~cf ps
  | Skip -> semantic_check_skip ~loc
  | IfThenElse ((e : untyped_expression), s1, os2) ->
      semantic_check_if_then_else ~loc ~cf e s1 os2
  | While ((e : untyped_expression), s) -> semantic_check_while ~loc ~cf e s
  | For {loop_variable; lower_bound; upper_bound; loop_body} ->
      semantic_check_for ~loc ~cf loop_variable lower_bound upper_bound
        loop_body
  | ForEach
      ( (id : Ast.identifier)
      , (e : untyped_expression)
      , (s :
          (untyped_expression, located_meta, untyped_lval, unit) statement_with)
      ) ->
      semantic_check_foreach ~loc ~cf id e s
  | Block vdsl -> semantic_check_block ~loc ~cf vdsl
  | Profile ((name : string), vdsl) ->
      semantic_check_profile ~loc ~cf name vdsl
  | VarDecl {decl_type= Unsized _; _} ->
      raise_s [%message "Don't support unsized declarations yet."]
  | VarDecl
      { decl_type= Sized st
      ; transformation
      ; identifier
      ; initial_value
      ; is_global } ->
      semantic_check_var_decl ~loc ~cf st transformation identifier
        initial_value is_global
  | FunDef {returntype; funname; arguments; body} ->
      semantic_check_fundef ~loc ~cf returntype funname arguments body

(* == Untyped programs ====================================================== *)

let semantic_check_ostatements_in_block ~(cf : context_flags_record)
    (block : originblock) (stmts_opt : untyped_statement sexp_list sexp_option)
    =
  let cf' = {cf with current_block= block} in
  Option.value_map stmts_opt ~default:(Validate.ok None) ~f:(fun stmts ->
      (* I'm folding since I'm not sure if map is guaranteed to
         respect the ordering of the list *)
      List.fold ~init:[] stmts ~f:(fun accu stmt ->
          let s = semantic_check_statement cf' stmt in
          s :: accu )
      |> List.rev |> Validate.sequence
      |> Validate.map ~f:Option.some )

let check_fun_def_body_in_block = function
  | {stmt= FunDef {body= {stmt= Block _; _}; _}; _}
   |{stmt= FunDef {body= {stmt= Skip; _}; _}; _} ->
      Validate.ok ()
  | {stmt= FunDef {body= {stmt= _; smeta}; _}; _} ->
      Validate.error @@ Semantic_error.fn_decl_needs_block smeta.loc
  | _ -> Validate.ok ()

let semantic_check_functions_have_defn function_block_stmts_opt =
  Validate.(
    if
      Symbol_table.check_some_id_is_unassigned vm
      && !check_that_all_functions_have_definition
    then
      match function_block_stmts_opt with
      | Some ({smeta; _} :: _) ->
          (* TODO: insert better location in the error *)
          error @@ Semantic_error.fn_decl_without_def smeta.loc
      | _ -> fatal_error ~msg:"semantic_check_functions_have_defn" ()
    else
      match function_block_stmts_opt with
      | Some [] | None -> ok ()
      | Some ls ->
          List.map ~f:check_fun_def_body_in_block ls
          |> sequence
          |> map ~f:(fun _ -> ()))

(* The actual semantic checks for all AST nodes! *)
let semantic_check_program
    { functionblock= fb
    ; datablock= db
    ; transformeddatablock= tdb
    ; parametersblock= pb
    ; transformedparametersblock= tpb
    ; modelblock= mb
    ; generatedquantitiesblock= gb } =
  (* NB: We always want to make sure we start with an empty symbol table, in
     case we are processing multiple files in one run. *)
  unsafe_clear_symbol_table vm ;
  let cf =
    { current_block= Functions
    ; in_toplevel_decl= false
    ; in_fun_def= false
    ; in_returning_fun_def= false
    ; in_rng_fun_def= false
    ; in_lp_fun_def= false
    ; in_udf_dist_def= false
    ; loop_depth= 0 }
  in
  let ufb =
    Validate.(
      semantic_check_ostatements_in_block ~cf Functions fb
      >>= fun (xs : typed_statement sexp_list sexp_option) ->
      semantic_check_functions_have_defn xs |> map ~f:(fun _ -> xs))
  in
  let udb = semantic_check_ostatements_in_block ~cf Data db in
  let utdb = semantic_check_ostatements_in_block ~cf TData tdb in
  let upb = semantic_check_ostatements_in_block ~cf Param pb in
  let utpb = semantic_check_ostatements_in_block ~cf TParam tpb in
  (* Model top level variables only assigned and read in model  *)
  Symbol_table.begin_scope vm ;
  let umb = semantic_check_ostatements_in_block ~cf Model mb in
  Symbol_table.end_scope vm ;
  let ugb = semantic_check_ostatements_in_block ~cf GQuant gb in
  let mk_typed_prog (ufb : typed_statement sexp_list sexp_option) udb utdb upb
      utpb umb ugb : Ast.typed_program =
    { functionblock= ufb
    ; datablock= udb
    ; transformeddatablock= utdb
    ; parametersblock= upb
    ; transformedparametersblock= utpb
    ; modelblock= umb
    ; generatedquantitiesblock= ugb }
  in
  let apply_to x f = Validate.apply ~f x in
  let check_correctness_invariant (decorated_ast : typed_program) :
      typed_program =
    if
      compare_untyped_program
        { functionblock= fb
        ; datablock= db
        ; transformeddatablock= tdb
        ; parametersblock= pb
        ; transformedparametersblock= tpb
        ; modelblock= mb
        ; generatedquantitiesblock= gb }
        (untyped_program_of_typed_program decorated_ast)
      = 0
    then decorated_ast
    else
      raise_s
        [%message
          "Type checked AST does not match original AST. Please file a bug!"
            (decorated_ast : typed_program)]
  in
  let check_correctness_invariant_validate =
    Validate.map ~f:check_correctness_invariant
  in
  Validate.(
    ok mk_typed_prog |> apply_to ufb |> apply_to udb |> apply_to utdb
    |> apply_to upb |> apply_to utpb |> apply_to umb |> apply_to ugb
    |> check_correctness_invariant_validate
    |> get_with
         ~with_ok:(fun ok -> Result.Ok ok)
         ~with_errors:(fun errs -> Result.Error errs))
