(** a type/semantic checker for Stan ASTs

  Functions which begin with "check_" return a typed version of their input
  Functions which begin with "verify_" return unit if a check succeeds, or else
    throw an Errors.SemanticError exception.
  Other functions which begin with "infer"/"calculate" vary. Usually they return
    a value, but a few do have error conditions.

  All Error.SemanticError excpetions are caught by check_program
  which turns the ast or exception into a Result.t for external usage

  A type environment (Env.t) is used to hold variables and functions, including
  stan math functions. This is a functional map, meaning it is handled immutably.
*)

open Core_kernel
open Core_kernel.Poly
open Middle
open Ast
module Env = Environment

(* we only allow errors raised by this function *)
let error e = raise (Errors.SemanticError e)

(* warnings are built up in a list *)
let warnings : Warnings.t list ref = ref []

let add_warning (span : Location_span.t) (message : string) =
  warnings := (span, message) :: !warnings

let attach_warnings x = (x, List.rev !warnings)

(* model name - don't love this here *)
let model_name = ref ""
let check_that_all_functions_have_definition = ref true

(* Record structure holding flags and other markers about context to be
   used for error reporting. *)
type context_flags_record =
  { current_block: Env.originblock
  ; in_toplevel_decl: bool
  ; in_fun_def: bool
  ; in_returning_fun_def: bool
  ; in_rng_fun_def: bool
  ; in_lp_fun_def: bool
  ; in_udf_dist_def: bool
  ; loop_depth: int }

let context block =
  { current_block= block
  ; in_toplevel_decl= false
  ; in_fun_def= false
  ; in_returning_fun_def= false
  ; in_rng_fun_def= false
  ; in_lp_fun_def= false
  ; in_udf_dist_def= false
  ; loop_depth= 0 }

let calculate_autodifftype cf origin ut =
  match origin with
  | Env.(Param | TParam | Model | Functions)
    when not (UnsizedType.is_int_type ut || cf.current_block = GQuant) ->
      UnsizedType.AutoDiffable
  | _ -> DataOnly

let arg_type x = (x.emeta.ad_level, x.emeta.type_)
let get_arg_types = List.map ~f:arg_type
let type_of_expr_typed ue = ue.emeta.type_
let has_int_type ue = ue.emeta.type_ = UInt
let has_int_array_type ue = ue.emeta.type_ = UArray UInt

let has_int_or_real_type ue =
  match ue.emeta.type_ with UInt | UReal -> true | _ -> false

(* -- General checks ---------------------------------------------- *)
let reserved_keywords =
  [ "for"; "in"; "while"; "repeat"; "until"; "if"; "then"; "else"; "true"
  ; "false"; "target"; "int"; "real"; "complex"; "void"; "vector"; "simplex"
  ; "unit_vector"; "ordered"; "positive_ordered"; "row_vector"; "matrix"
  ; "cholesky_factor_corr"; "cholesky_factor_cov"; "corr_matrix"; "cov_matrix"
  ; "functions"; "model"; "data"; "parameters"; "quantities"; "transformed"
  ; "generated"; "profile"; "return"; "break"; "continue"; "increment_log_prob"
  ; "get_lp"; "print"; "reject"; "typedef"; "struct"; "var"; "export"; "extern"
  ; "static"; "auto" ]

let verify_identifier id : unit =
  if id.name = !model_name then
    Semantic_error.ident_is_model_name id.id_loc id.name |> error
  else if
    String.is_suffix id.name ~suffix:"__"
    || List.mem reserved_keywords id.name ~equal:String.equal
  then Semantic_error.ident_is_keyword id.id_loc id.name |> error

let distribution_name_variants name =
  if name = "multiply_log" || name = "binomial_coefficient_log" then [name]
  else
    (* this will have some duplicates, but preserves order better *)
    match Utils.split_distribution_suffix name with
    | Some (stem, "lpmf") | Some (stem, "lpdf") | Some (stem, "log") ->
        [name; stem ^ "_lpmf"; stem ^ "_lpdf"; stem ^ "_log"]
    | Some (stem, "lcdf") | Some (stem, "cdf_log") ->
        [name; stem ^ "_lcdf"; stem ^ "_cdf_log"]
    | Some (stem, "lccdf") | Some (stem, "ccdf_log") ->
        [name; stem ^ "_lccdf"; stem ^ "_ccdf_log"]
    | _ -> [name]

(** verify that the variable being declared is previous unused.
   allowed to shadow StanLib *)
let verify_name_fresh_var loc tenv name =
  if Utils.is_unnormalized_distribution name then
    Semantic_error.ident_has_unnormalized_suffix loc name |> error
  else if
    List.exists (Env.find tenv name) ~f:(function
      | {kind= `StanMath; _} ->
          false (* user variables can shadow library names *)
      | _ -> true )
  then Semantic_error.ident_in_use loc name |> error

(** verify that the variable being declared is previous unused. *)
let verify_name_fresh_udf loc tenv name =
  if
    (* variadic functions are currently not in math sigs and aren't
       overloadable due to their separate typechecking *)
    Stan_math_signatures.is_reduce_sum_fn name
    || Stan_math_signatures.is_variadic_ode_fn name
    || Stan_math_signatures.is_variadic_dae_fn name
  then Semantic_error.ident_is_stanmath_name loc name |> error
  else if Utils.is_unnormalized_distribution name then
    Semantic_error.udf_is_unnormalized_fn loc name |> error
  else if
    (* if a variable is already defined with this name
       - not really possible as all functions are defined before data,
         but future-proofing is good *)
    List.exists
      ~f:(function {kind= `Variable _; _} -> true | _ -> false)
      (Env.find tenv name)
  then Semantic_error.ident_in_use loc name |> error

(** Checks that a variable/function name:
  - a function/identifier does not have the _lupdf/_lupmf suffix
  - is not already in use (for now)
*)
let verify_name_fresh tenv id ~is_udf =
  let f =
    if is_udf then verify_name_fresh_udf id.id_loc tenv
    else verify_name_fresh_var id.id_loc tenv in
  List.iter ~f (distribution_name_variants id.name)

let is_of_compatible_return_type rt1 srt2 =
  UnsizedType.(
    match (rt1, srt2) with
    | Void, NoReturnType
     |Void, Incomplete Void
     |Void, Complete Void
     |Void, AnyReturnType ->
        true
    | ReturnType UReal, Complete (ReturnType UInt) -> true
    | ReturnType UComplex, Complete (ReturnType UReal) -> true
    | ReturnType UComplex, Complete (ReturnType UInt) -> true
    | ReturnType rt1, Complete (ReturnType rt2) -> rt1 = rt2
    | ReturnType _, AnyReturnType -> true
    | _ -> false)

(* -- Expressions ------------------------------------------------- *)
let check_ternary_if loc pe te fe =
  let promote expr type_ ad_level =
    if
      (not (UnsizedType.equal expr.emeta.type_ type_))
      || UnsizedType.compare_autodifftype expr.emeta.ad_level ad_level <> 0
    then
      { expr= Promotion (expr, UnsizedType.internal_scalar type_, ad_level)
      ; emeta= {expr.emeta with type_; ad_level} }
    else expr in
  match
    ( pe.emeta.type_
    , UnsizedType.common_type (te.emeta.type_, fe.emeta.type_)
    , expr_ad_lub [pe; te; fe] )
  with
  | UInt, Some type_, ad_level when not (UnsizedType.is_fun_type type_) ->
      mk_typed_expression
        ~expr:
          (TernaryIf (pe, promote te type_ ad_level, promote fe type_ ad_level))
        ~ad_level ~type_ ~loc
  | _, _, _ ->
      Semantic_error.illtyped_ternary_if loc pe.emeta.type_ te.emeta.type_
        fe.emeta.type_
      |> error

let match_to_rt_option = function
  | SignatureMismatch.UniqueMatch (rt, _, _) -> Some rt
  | _ -> None

let stan_math_return_type name arg_tys =
  match name with
  | x when Stan_math_signatures.is_reduce_sum_fn x ->
      Some (UnsizedType.ReturnType UReal)
  | x when Stan_math_signatures.is_variadic_ode_fn x ->
      Some (UnsizedType.ReturnType (UArray UVector))
  | x when Stan_math_signatures.is_variadic_dae_fn x ->
      Some (UnsizedType.ReturnType (UArray UVector))
  | _ ->
      SignatureMismatch.matching_stanlib_function name arg_tys
      |> match_to_rt_option

let operator_stan_math_return_type op arg_tys =
  match (op, arg_tys) with
  | Operator.IntDivide, [(_, UnsizedType.UInt); (_, UInt)] ->
      Some (UnsizedType.(ReturnType UInt), [Promotion.NoPromotion; NoPromotion])
  | IntDivide, _ -> None
  | _ ->
      Stan_math_signatures.operator_to_stan_math_fns op
      |> List.filter_map ~f:(fun name ->
             SignatureMismatch.matching_stanlib_function name arg_tys
             |> function
             | SignatureMismatch.UniqueMatch (rt, _, p) -> Some (rt, p)
             | _ -> None )
      |> List.hd

let assignmentoperator_stan_math_return_type assop arg_tys =
  ( match assop with
  | Operator.Divide ->
      SignatureMismatch.matching_stanlib_function "divide" arg_tys
      |> match_to_rt_option
  | Plus | Minus | Times | EltTimes | EltDivide ->
      operator_stan_math_return_type assop arg_tys |> Option.map ~f:fst
  | _ -> None )
  |> Option.bind ~f:(function
       | ReturnType rtype
         when rtype = snd (List.hd_exn arg_tys)
              && not
                   ( (assop = Operator.EltTimes || assop = Operator.EltDivide)
                   && UnsizedType.is_scalar_type rtype ) ->
           Some UnsizedType.Void
       | _ -> None )

let check_binop loc op le re =
  let rt = [le; re] |> get_arg_types |> operator_stan_math_return_type op in
  match rt with
  | Some (ReturnType type_, [p1; p2]) ->
      mk_typed_expression
        ~expr:(BinOp (Promotion.promote le p1, op, Promotion.promote re p2))
        ~ad_level:(expr_ad_lub [le; re])
        ~type_ ~loc
  | _ ->
      Semantic_error.illtyped_binary_op loc op le.emeta.type_ re.emeta.type_
      |> error

let check_prefixop loc op te =
  let rt = operator_stan_math_return_type op [arg_type te] in
  match rt with
  | Some (ReturnType type_, _) ->
      mk_typed_expression
        ~expr:(PrefixOp (op, te))
        ~ad_level:(expr_ad_lub [te])
        ~type_ ~loc
  | _ -> Semantic_error.illtyped_prefix_op loc op te.emeta.type_ |> error

let check_postfixop loc op te =
  let rt = operator_stan_math_return_type op [arg_type te] in
  match rt with
  | Some (ReturnType type_, _) ->
      mk_typed_expression
        ~expr:(PostfixOp (te, op))
        ~ad_level:(expr_ad_lub [te])
        ~type_ ~loc
  | _ -> Semantic_error.illtyped_postfix_op loc op te.emeta.type_ |> error

let check_id cf loc tenv id =
  match Env.find tenv (Utils.stdlib_distribution_name id.name) with
  | [] ->
      Semantic_error.ident_not_in_scope loc id.name
        (Env.nearest_ident tenv id.name)
      |> error
  | {kind= `StanMath; _} :: _ ->
      ( calculate_autodifftype cf MathLibrary UMathLibraryFunction
      , UnsizedType.UMathLibraryFunction )
  | {kind= `Variable {origin= Param | TParam | GQuant; _}; _} :: _
    when cf.in_toplevel_decl ->
      Semantic_error.non_data_variable_size_decl loc |> error
  | _ :: _
    when Utils.is_unnormalized_distribution id.name
         && not
              ( (cf.in_fun_def && (cf.in_udf_dist_def || cf.in_lp_fun_def))
              || cf.current_block = Model ) ->
      Semantic_error.invalid_unnormalized_fn loc |> error
  | {kind= `Variable {origin; _}; type_} :: _ ->
      (calculate_autodifftype cf origin type_, type_)
  | { kind= `UserDefined | `UserDeclared _
    ; type_= UFun (args, rt, FnLpdf _, mem_pattern) }
    :: _ ->
      let type_ =
        UnsizedType.UFun
          (args, rt, Fun_kind.suffix_from_name id.name, mem_pattern) in
      (calculate_autodifftype cf Functions type_, type_)
  | {kind= `UserDefined | `UserDeclared _; type_} :: _ ->
      (calculate_autodifftype cf Functions type_, type_)

let check_variable cf loc tenv id =
  let ad_level, type_ = check_id cf loc tenv id in
  mk_typed_expression ~expr:(Variable id) ~ad_level ~type_ ~loc

let get_consistent_types ad_level type_ es =
  let ad =
    UnsizedType.lub_ad_type
      (ad_level :: List.map ~f:(fun e -> e.emeta.ad_level) es) in
  let f state e =
    match state with
    | Error e -> Error e
    | Ok ty -> (
      match UnsizedType.common_type (ty, e.emeta.type_) with
      | Some ty -> Ok ty
      | None -> Error (ty, e.emeta) ) in
  List.fold ~init:(Ok type_) ~f es
  |> Result.map ~f:(fun ty ->
         let promotions =
           List.map (get_arg_types es)
             ~f:(Promotion.get_type_promotion_exn (ad, ty)) in
         (ad, ty, promotions) )

let check_array_expr loc es =
  match es with
  | [] -> Semantic_error.empty_array loc |> error
  | {emeta= {ad_level; type_; _}; _} :: _ -> (
    match get_consistent_types ad_level type_ es with
    | Error (ty, meta) ->
        Semantic_error.mismatched_array_types meta.loc ty meta.type_ |> error
    | Ok (ad_level, type_, promotions) ->
        let type_ = UnsizedType.UArray type_ in
        mk_typed_expression
          ~expr:(ArrayExpr (Promotion.promote_list es promotions))
          ~ad_level ~type_ ~loc )

let check_rowvector loc es =
  match es with
  | {emeta= {ad_level; type_= UnsizedType.URowVector; _}; _} :: _ -> (
    match get_consistent_types ad_level URowVector es with
    | Ok (ad_level, typ, promotions) ->
        mk_typed_expression
          ~expr:(RowVectorExpr (Promotion.promote_list es promotions))
          ~ad_level
          ~type_:(if typ = UComplex then UComplexMatrix else UMatrix)
          ~loc
    | Error (_, meta) ->
        Semantic_error.invalid_matrix_types meta.loc meta.type_ |> error )
  | {emeta= {ad_level; type_= UnsizedType.UComplexRowVector; _}; _} :: _ -> (
    match get_consistent_types ad_level UComplexRowVector es with
    | Ok (ad_level, _, promotions) ->
        mk_typed_expression
          ~expr:(RowVectorExpr (Promotion.promote_list es promotions))
          ~ad_level ~type_:UComplexMatrix ~loc
    | Error (_, meta) ->
        Semantic_error.invalid_matrix_types meta.loc meta.type_ |> error )
  | _ -> (
    match get_consistent_types DataOnly UReal es with
    | Ok (ad_level, typ, promotions) ->
        mk_typed_expression
          ~expr:(RowVectorExpr (Promotion.promote_list es promotions))
          ~ad_level
          ~type_:(if typ = UComplex then UComplexRowVector else URowVector)
          ~loc
    | Error (_, meta) ->
        Semantic_error.invalid_row_vector_types meta.loc meta.type_ |> error )

(* index checking *)

let indexing_type idx =
  match idx with
  | Single {emeta= {type_= UnsizedType.UInt; _}; _} -> `Single
  | _ -> `Multi

let is_multiindex i =
  match indexing_type i with `Single -> false | `Multi -> true

let inferred_unsizedtype_of_indexed ~loc ut indices =
  let rec aux type_ idcs =
    let vec, rowvec, scalar =
      if UnsizedType.is_complex_type type_ then
        UnsizedType.(UComplexVector, UComplexRowVector, UComplex)
      else (UVector, URowVector, UReal) in
    match (type_, idcs) with
    | _, [] -> type_
    | UnsizedType.UArray type_, `Single :: tl -> aux type_ tl
    | UArray type_, `Multi :: tl -> aux type_ tl |> UnsizedType.UArray
    | (UVector | URowVector | UComplexRowVector | UComplexVector), [`Single]
     |(UMatrix | UComplexMatrix), [`Single; `Single] ->
        scalar
    | ( ( UVector | URowVector | UMatrix | UComplexVector | UComplexMatrix
        | UComplexRowVector )
      , [`Multi] )
     |(UMatrix | UComplexMatrix), [`Multi; `Multi] ->
        type_
    | (UMatrix | UComplexMatrix), ([`Single] | [`Single; `Multi]) -> rowvec
    | (UMatrix | UComplexMatrix), [`Multi; `Single] -> vec
    | (UMatrix | UComplexMatrix), _ :: _ :: _ :: _
     |(UVector | URowVector | UComplexRowVector | UComplexVector), _ :: _ :: _
     |(UInt | UReal | UComplex | UFun _ | UMathLibraryFunction), _ :: _ ->
        Semantic_error.not_indexable loc ut (List.length indices) |> error in
  aux ut (List.map ~f:indexing_type indices)

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
                 [at; ue1.emeta.ad_level; ue2.emeta.ad_level] )
         uindices )

(* function checking *)

let verify_conddist_name loc id =
  if
    List.exists
      ~f:(fun x -> String.is_suffix id.name ~suffix:x)
      Utils.conditioning_suffices
  then ()
  else Semantic_error.conditional_notation_not_allowed loc |> error

let verify_fn_conditioning loc id =
  if
    List.exists
      ~f:(fun suffix -> String.is_suffix id.name ~suffix)
      Utils.conditioning_suffices
    && not (String.is_suffix id.name ~suffix:"_cdf")
  then Semantic_error.conditioning_required loc |> error

(** `Target+=` can only be used in model and functions
    with right suffix (same for tilde etc)
*)
let verify_fn_target_plus_equals cf loc id =
  if
    String.is_suffix id.name ~suffix:"_lp"
    && not
         ( cf.in_lp_fun_def || cf.current_block = Model
         || cf.current_block = TParam )
  then Semantic_error.target_plusequals_outisde_model_or_logprob loc |> error

(** Rng functions cannot be used in Tp or Model and only
    in function defs with the right suffix
*)
let verify_fn_rng cf loc id =
  if String.is_suffix id.name ~suffix:"_rng" && cf.in_toplevel_decl then
    Semantic_error.invalid_decl_rng_fn loc |> error
  else if
    String.is_suffix id.name ~suffix:"_rng"
    && ( (cf.in_fun_def && not cf.in_rng_fun_def)
       || cf.current_block = TParam || cf.current_block = Model )
  then Semantic_error.invalid_rng_fn loc |> error

(** unnormalized _lpdf/_lpmf functions can only be used in _lpdf/_lpmf/_lp udfs
    or the model block
*)
let verify_unnormalized cf loc id =
  if
    Utils.is_unnormalized_distribution id.name
    && not ((cf.in_fun_def && cf.in_udf_dist_def) || cf.current_block = Model)
  then Semantic_error.invalid_unnormalized_fn loc |> error

let mk_fun_app ~is_cond_dist (x, y, z) =
  if is_cond_dist then CondDistApp (x, y, z) else FunApp (x, y, z)

let check_normal_fn ~is_cond_dist loc tenv id es =
  match Env.find tenv (Utils.normalized_name id.name) with
  | {kind= `Variable _; _} :: _
  (* variables can sometimes shadow stanlib functions, so we have to check this *)
    when not
           (Stan_math_signatures.is_stan_math_function_name
              (Utils.normalized_name id.name) ) ->
      Semantic_error.returning_fn_expected_nonfn_found loc id.name |> error
  | [] ->
      ( match Utils.split_distribution_suffix id.name with
      | Some (prefix, suffix) -> (
          let known_families =
            List.map
              ~f:(fun (_, y, _, _) -> y)
              Stan_math_signatures.distributions in
          let is_known_family s =
            List.mem known_families s ~equal:String.equal in
          match suffix with
          | ("lpmf" | "lumpf") when Env.mem tenv (prefix ^ "_lpdf") ->
              Semantic_error.returning_fn_expected_wrong_dist_suffix_found loc
                (prefix, suffix)
          | ("lpdf" | "lumdf") when Env.mem tenv (prefix ^ "_lpmf") ->
              Semantic_error.returning_fn_expected_wrong_dist_suffix_found loc
                (prefix, suffix)
          | _ ->
              if
                is_known_family prefix
                && List.mem ~equal:String.equal
                     Utils.cumulative_distribution_suffices_w_rng suffix
              then
                Semantic_error
                .returning_fn_expected_undeclared_dist_suffix_found loc
                  (prefix, suffix)
              else
                Semantic_error.returning_fn_expected_undeclaredident_found loc
                  id.name
                  (Env.nearest_ident tenv id.name) )
      | None ->
          Semantic_error.returning_fn_expected_undeclaredident_found loc id.name
            (Env.nearest_ident tenv id.name) )
      |> error
  | _ (* a function *) -> (
    (* NB: At present, [SignatureMismatch.matching_function] cannot handle overloaded function types.
       This is not needed until UDFs can be higher-order, as it is special cased for
       variadic functions
    *)
    match
      SignatureMismatch.matching_function tenv id.name (get_arg_types es)
    with
    | UniqueMatch (Void, _, _) ->
        Semantic_error.returning_fn_expected_nonreturning_found loc id.name
        |> error
    | UniqueMatch (ReturnType ut, fnk, promotions) ->
        mk_typed_expression
          ~expr:
            (mk_fun_app ~is_cond_dist
               ( fnk (Fun_kind.suffix_from_name id.name)
               , id
               , Promotion.promote_list es promotions ) )
          ~ad_level:(expr_ad_lub es) ~type_:ut ~loc
    | AmbiguousMatch sigs ->
        Semantic_error.ambiguous_function_promotion loc id.name
          (Some (List.map ~f:type_of_expr_typed es))
          sigs
        |> error
    | SignatureErrors (l, b) ->
        es
        |> List.map ~f:(fun e -> e.emeta.type_)
        |> Semantic_error.illtyped_fn_app loc id.name (l, b)
        |> error )

(** Given a constraint function [matches], find any signature which exists
    Returns the first [Ok] if any exist, or else [Error]
*)
let find_matching_first_order_fn tenv matches fname =
  let candidates =
    Utils.stdlib_distribution_name fname.name
    |> Env.find tenv |> List.map ~f:matches in
  let ok, errs = List.partition_map candidates ~f:Result.to_either in
  match SignatureMismatch.unique_minimum_promotion ok with
  | Ok a -> SignatureMismatch.UniqueMatch a
  | Error (Some promotions) ->
      List.filter_map promotions ~f:(function
        | UnsizedType.UFun (args, rt, _, _) -> Some (rt, args)
        | _ -> None )
      |> AmbiguousMatch
  | Error None -> SignatureMismatch.SignatureErrors (List.hd_exn errs)

let make_function_variable cf loc id = function
  | UnsizedType.UFun (args, rt, FnLpdf _, mem_pattern) ->
      let type_ =
        UnsizedType.UFun
          (args, rt, Fun_kind.suffix_from_name id.name, mem_pattern) in
      mk_typed_expression ~expr:(Variable id)
        ~ad_level:(calculate_autodifftype cf Functions type_)
        ~type_ ~loc
  | UnsizedType.UFun _ as type_ ->
      mk_typed_expression ~expr:(Variable id)
        ~ad_level:(calculate_autodifftype cf Functions type_)
        ~type_ ~loc
  | type_ ->
      Common.FatalError.fatal_error_msg
        [%message
          "Attempting to create function variable out of "
            (type_ : UnsizedType.t)]

let rec check_fn ~is_cond_dist loc cf tenv id (tes : Ast.typed_expression list)
    =
  if Stan_math_signatures.is_reduce_sum_fn id.name then
    check_reduce_sum ~is_cond_dist loc cf tenv id tes
  else if Stan_math_signatures.is_variadic_ode_fn id.name then
    check_variadic_ode ~is_cond_dist loc cf tenv id tes
  else if Stan_math_signatures.is_variadic_dae_fn id.name then
    check_variadic_dae ~is_cond_dist loc cf tenv id tes
  else check_normal_fn ~is_cond_dist loc tenv id tes

and check_reduce_sum ~is_cond_dist loc cf tenv id tes =
  let basic_mismatch () =
    let mandatory_args =
      UnsizedType.[(AutoDiffable, UArray UReal); (AutoDiffable, UInt)] in
    let mandatory_fun_args =
      UnsizedType.
        [(AutoDiffable, UArray UReal); (DataOnly, UInt); (DataOnly, UInt)] in
    SignatureMismatch.check_variadic_args true mandatory_args mandatory_fun_args
      UReal (get_arg_types tes) in
  let fail () =
    let expected_args, err =
      basic_mismatch () |> Result.error |> Option.value_exn in
    Semantic_error.illtyped_reduce_sum_generic loc id.name
      (List.map ~f:type_of_expr_typed tes)
      expected_args err
    |> error in
  let matching remaining_es fn =
    match fn with
    | Env.
        { type_=
            UnsizedType.UFun
              (((_, sliced_arg_fun_type) as sliced_arg_fun) :: _, _, _, _) as
            ftype
        ; _ }
      when List.mem Stan_math_signatures.reduce_sum_slice_types
             sliced_arg_fun_type ~equal:( = ) ->
        let mandatory_args = [sliced_arg_fun; (AutoDiffable, UInt)] in
        let mandatory_fun_args =
          [sliced_arg_fun; (DataOnly, UInt); (DataOnly, UInt)] in
        let arg_types =
          (calculate_autodifftype cf Functions ftype, ftype)
          :: get_arg_types remaining_es in
        SignatureMismatch.check_variadic_args true mandatory_args
          mandatory_fun_args UReal arg_types
    | _ -> basic_mismatch () in
  match tes with
  | {expr= Variable fname; _} :: remaining_es -> (
    match find_matching_first_order_fn tenv (matching remaining_es) fname with
    | SignatureMismatch.UniqueMatch (ftype, promotions) ->
        (* a valid signature exists *)
        let tes = make_function_variable cf loc fname ftype :: remaining_es in
        mk_typed_expression
          ~expr:
            (mk_fun_app ~is_cond_dist
               (StanLib FnPlain, id, Promotion.promote_list tes promotions) )
          ~ad_level:(expr_ad_lub tes) ~type_:UnsizedType.UReal ~loc
    | AmbiguousMatch ps ->
        Semantic_error.ambiguous_function_promotion loc fname.name None ps
        |> error
    | SignatureErrors (expected_args, err) ->
        Semantic_error.illtyped_reduce_sum loc id.name
          (List.map ~f:type_of_expr_typed tes)
          expected_args err
        |> error )
  | _ -> fail ()

and check_variadic_ode ~is_cond_dist loc cf tenv id tes =
  let optional_tol_mandatory_args =
    if Stan_math_signatures.variadic_ode_adjoint_fn = id.name then
      Stan_math_signatures.variadic_ode_adjoint_ctl_tol_arg_types
    else if Stan_math_signatures.is_variadic_ode_nonadjoint_tol_fn id.name then
      Stan_math_signatures.variadic_ode_tol_arg_types
    else [] in
  let mandatory_arg_types =
    Stan_math_signatures.variadic_ode_mandatory_arg_types
    @ optional_tol_mandatory_args in
  let fail () =
    let expected_args, err =
      SignatureMismatch.check_variadic_args false mandatory_arg_types
        Stan_math_signatures.variadic_ode_mandatory_fun_args
        Stan_math_signatures.variadic_ode_fun_return_type (get_arg_types tes)
      |> Result.error |> Option.value_exn in
    Semantic_error.illtyped_variadic_ode loc id.name
      (List.map ~f:type_of_expr_typed tes)
      expected_args err
    |> error in
  let matching remaining_es Env.{type_= ftype; _} =
    let arg_types =
      (calculate_autodifftype cf Functions ftype, ftype)
      :: get_arg_types remaining_es in
    SignatureMismatch.check_variadic_args false mandatory_arg_types
      Stan_math_signatures.variadic_ode_mandatory_fun_args
      Stan_math_signatures.variadic_ode_fun_return_type arg_types in
  match tes with
  | {expr= Variable fname; _} :: remaining_es -> (
    match find_matching_first_order_fn tenv (matching remaining_es) fname with
    | SignatureMismatch.UniqueMatch (ftype, promotions) ->
        let tes = make_function_variable cf loc fname ftype :: remaining_es in
        mk_typed_expression
          ~expr:
            (mk_fun_app ~is_cond_dist
               (StanLib FnPlain, id, Promotion.promote_list tes promotions) )
          ~ad_level:(expr_ad_lub tes)
          ~type_:Stan_math_signatures.variadic_ode_return_type ~loc
    | AmbiguousMatch ps ->
        Semantic_error.ambiguous_function_promotion loc fname.name None ps
        |> error
    | SignatureErrors (expected_args, err) ->
        Semantic_error.illtyped_variadic_ode loc id.name
          (List.map ~f:type_of_expr_typed tes)
          expected_args err
        |> error )
  | _ -> fail ()

and check_variadic_dae ~is_cond_dist loc cf tenv id tes =
  let optional_tol_mandatory_args =
    if Stan_math_signatures.is_variadic_dae_tol_fn id.name then
      Stan_math_signatures.variadic_dae_tol_arg_types
    else [] in
  let mandatory_arg_types =
    Stan_math_signatures.variadic_dae_mandatory_arg_types
    @ optional_tol_mandatory_args in
  let fail () =
    let expected_args, err =
      SignatureMismatch.check_variadic_args false mandatory_arg_types
        Stan_math_signatures.variadic_dae_mandatory_fun_args
        Stan_math_signatures.variadic_dae_fun_return_type (get_arg_types tes)
      |> Result.error |> Option.value_exn in
    Semantic_error.illtyped_variadic_dae loc id.name
      (List.map ~f:type_of_expr_typed tes)
      expected_args err
    |> error in
  let matching remaining_es Env.{type_= ftype; _} =
    let arg_types =
      (calculate_autodifftype cf Functions ftype, ftype)
      :: get_arg_types remaining_es in
    SignatureMismatch.check_variadic_args false mandatory_arg_types
      Stan_math_signatures.variadic_dae_mandatory_fun_args
      Stan_math_signatures.variadic_dae_fun_return_type arg_types in
  match tes with
  | {expr= Variable fname; _} :: remaining_es -> (
    match find_matching_first_order_fn tenv (matching remaining_es) fname with
    | SignatureMismatch.UniqueMatch (ftype, promotions) ->
        let tes = make_function_variable cf loc fname ftype :: remaining_es in
        mk_typed_expression
          ~expr:
            (mk_fun_app ~is_cond_dist
               (StanLib FnPlain, id, Promotion.promote_list tes promotions) )
          ~ad_level:(expr_ad_lub tes)
          ~type_:Stan_math_signatures.variadic_dae_return_type ~loc
    | AmbiguousMatch ps ->
        Semantic_error.ambiguous_function_promotion loc fname.name None ps
        |> error
    | SignatureErrors (expected_args, err) ->
        Semantic_error.illtyped_variadic_dae loc id.name
          (List.map ~f:type_of_expr_typed tes)
          expected_args err
        |> error )
  | _ -> fail ()

and check_funapp loc cf tenv ~is_cond_dist id (es : Ast.typed_expression list) =
  let name_check =
    if is_cond_dist then verify_conddist_name else verify_fn_conditioning in
  let res = check_fn ~is_cond_dist loc cf tenv id es in
  verify_identifier id ;
  name_check loc id ;
  verify_fn_target_plus_equals cf loc id ;
  verify_fn_rng cf loc id ;
  verify_unnormalized cf loc id ;
  res

and check_indexed loc cf tenv e indices =
  let tindices = List.map ~f:(check_index cf tenv) indices in
  let te = check_expression cf tenv e in
  let ad_level = inferred_ad_type_of_indexed te.emeta.ad_level tindices in
  let type_ = inferred_unsizedtype_of_indexed ~loc te.emeta.type_ tindices in
  mk_typed_expression ~expr:(Indexed (te, tindices)) ~ad_level ~type_ ~loc

and check_index cf tenv = function
  | All -> All
  (* Check that indexes have int (container) type *)
  | Single e ->
      let te = check_expression cf tenv e in
      if has_int_type te || has_int_array_type te then Single te
      else
        Semantic_error.int_intarray_or_range_expected te.emeta.loc
          te.emeta.type_
        |> error
  | Upfrom e -> check_expression_of_int_type cf tenv e "Range bound" |> Upfrom
  | Downfrom e ->
      check_expression_of_int_type cf tenv e "Range bound" |> Downfrom
  | Between (e1, e2) ->
      let le = check_expression_of_int_type cf tenv e1 "Range bound" in
      let ue = check_expression_of_int_type cf tenv e2 "Range bound" in
      Between (le, ue)

and check_expression cf tenv ({emeta; expr} : Ast.untyped_expression) :
    Ast.typed_expression =
  let loc = emeta.loc in
  let ce = check_expression cf tenv in
  match expr with
  | TernaryIf (e1, e2, e3) ->
      let pe = ce e1 in
      let te = ce e2 in
      let fe = ce e3 in
      check_ternary_if loc pe te fe
  | BinOp (e1, op, e2) ->
      let le = ce e1 in
      let re = ce e2 in
      let binop_type_warnings x y =
        match (x.emeta.type_, y.emeta.type_, op) with
        | UInt, UInt, Divide ->
            let hint ppf () =
              match (x.expr, y.expr) with
              | IntNumeral x, _ ->
                  Fmt.pf ppf "%s.0 / %a" x Pretty_printing.pp_typed_expression y
              | _, Ast.IntNumeral y ->
                  Fmt.pf ppf "%a / %s.0" Pretty_printing.pp_typed_expression x y
              | _ ->
                  Fmt.pf ppf "%a * 1.0 / %a" Pretty_printing.pp_typed_expression
                    x Pretty_printing.pp_typed_expression y in
            let s =
              Fmt.str
                "@[<v>@[<hov 0>Found int division:@]@   @[<hov 2>%a@]@,\
                 @[<hov>%a@]@   @[<hov 2>%a@]@,\
                 @[<hov>%a@]@]"
                Pretty_printing.pp_expression {expr; emeta} Fmt.text
                "Values will be rounded towards zero. If rounding is not \
                 desired you can write the division as"
                hint () Fmt.text
                "If rounding is intended please use the integer division \
                 operator %/%." in
            add_warning x.emeta.loc s
        | (UArray UMatrix | UMatrix), (UInt | UReal), Pow ->
            let s =
              Fmt.str
                "@[<v>@[<hov 0>Found matrix^scalar:@]@   @[<hov 2>%a@]@,\
                 @[<hov>%a@]@ @[<hov>%a@]@]" Pretty_printing.pp_expression
                {expr; emeta} Fmt.text
                "matrix ^ number is interpreted as element-wise \
                 exponentiation. If this is intended, you can silence this \
                 warning by using elementwise operator .^"
                Fmt.text
                "If you intended matrix exponentiation, use the function \
                 matrix_power(matrix,int) instead." in
            add_warning x.emeta.loc s
        | _ -> () in
      binop_type_warnings le re ; check_binop loc op le re
  | PrefixOp (op, e) -> ce e |> check_prefixop loc op
  | PostfixOp (e, op) -> ce e |> check_postfixop loc op
  | Variable id ->
      verify_identifier id ;
      check_variable cf loc tenv id
  | IntNumeral s -> (
    match float_of_string_opt s with
    | Some i when i < 2_147_483_648.0 ->
        mk_typed_expression ~expr:(IntNumeral s) ~ad_level:DataOnly ~type_:UInt
          ~loc
    | _ -> Semantic_error.bad_int_literal loc |> error )
  | RealNumeral s ->
      mk_typed_expression ~expr:(RealNumeral s) ~ad_level:DataOnly ~type_:UReal
        ~loc
  | ImagNumeral s ->
      mk_typed_expression ~expr:(ImagNumeral s) ~ad_level:DataOnly
        ~type_:UComplex ~loc
  | GetLP ->
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      if
        not
          ( cf.in_lp_fun_def || cf.current_block = Model
          || cf.current_block = TParam )
      then
        Semantic_error.target_plusequals_outisde_model_or_logprob loc |> error
      else
        mk_typed_expression ~expr:GetLP
          ~ad_level:(calculate_autodifftype cf cf.current_block UReal)
          ~type_:UReal ~loc
  | GetTarget ->
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      if
        not
          ( cf.in_lp_fun_def || cf.current_block = Model
          || cf.current_block = TParam )
      then
        Semantic_error.target_plusequals_outisde_model_or_logprob loc |> error
      else
        mk_typed_expression ~expr:GetTarget
          ~ad_level:(calculate_autodifftype cf cf.current_block UReal)
          ~type_:UReal ~loc
  | ArrayExpr es -> es |> List.map ~f:ce |> check_array_expr loc
  | RowVectorExpr es -> es |> List.map ~f:ce |> check_rowvector loc
  | Paren e ->
      let te = ce e in
      mk_typed_expression ~expr:(Paren te) ~ad_level:te.emeta.ad_level
        ~type_:te.emeta.type_ ~loc
  | Indexed (e, indices) -> check_indexed loc cf tenv e indices
  | FunApp ((), id, es) ->
      es |> List.map ~f:ce |> check_funapp loc cf tenv ~is_cond_dist:false id
  | CondDistApp ((), id, es) ->
      es |> List.map ~f:ce |> check_funapp loc cf tenv ~is_cond_dist:true id
  | Promotion (e, _, _) ->
      (* Should never happen: promotions are produced during typechecking *)
      Common.FatalError.fatal_error_msg
        [%message "Promotion in untyped AST" (e : Ast.untyped_expression)]

and check_expression_of_int_type cf tenv e name =
  let te = check_expression cf tenv e in
  if has_int_type te then te
  else Semantic_error.int_expected te.emeta.loc name te.emeta.type_ |> error

let check_expression_of_int_or_real_type cf tenv e name =
  let te = check_expression cf tenv e in
  if has_int_or_real_type te then te
  else
    Semantic_error.int_or_real_expected te.emeta.loc name te.emeta.type_
    |> error

let check_expression_of_scalar_or_type cf tenv t e name =
  let te = check_expression cf tenv e in
  if UnsizedType.is_scalar_type te.emeta.type_ || te.emeta.type_ = t then te
  else
    Semantic_error.scalar_or_type_expected te.emeta.loc name t te.emeta.type_
    |> error

(* -- Statements ------------------------------------------------- *)
(* non returning functions *)
let verify_nrfn_target loc cf id =
  if
    String.is_suffix id.name ~suffix:"_lp"
    && not
         ( cf.in_lp_fun_def || cf.current_block = Model
         || cf.current_block = TParam )
  then Semantic_error.target_plusequals_outisde_model_or_logprob loc |> error

let check_nrfn loc tenv id es =
  match Env.find tenv id.name with
  | {kind= `Variable _; _} :: _
  (* variables can shadow stanlib functions, so we have to check this *)
    when not (Stan_math_signatures.is_stan_math_function_name id.name) ->
      Semantic_error.nonreturning_fn_expected_nonfn_found loc id.name |> error
  | [] ->
      Semantic_error.nonreturning_fn_expected_undeclaredident_found loc id.name
        (Env.nearest_ident tenv id.name)
      |> error
  | _ (* a function *) -> (
    match
      SignatureMismatch.matching_function tenv id.name (get_arg_types es)
    with
    | UniqueMatch (Void, fnk, promotions) ->
        mk_typed_statement
          ~stmt:
            (NRFunApp
               ( fnk (Fun_kind.suffix_from_name id.name)
               , id
               , Promotion.promote_list es promotions ) )
          ~return_type:NoReturnType ~loc
    | UniqueMatch (ReturnType _, _, _) ->
        Semantic_error.nonreturning_fn_expected_returning_found loc id.name
        |> error
    | AmbiguousMatch sigs ->
        Semantic_error.ambiguous_function_promotion loc id.name
          (Some (List.map ~f:type_of_expr_typed es))
          sigs
        |> error
    | SignatureErrors (l, b) ->
        es
        |> List.map ~f:type_of_expr_typed
        |> Semantic_error.illtyped_fn_app loc id.name (l, b)
        |> error )

let check_nr_fn_app loc cf tenv id es =
  let tes = List.map ~f:(check_expression cf tenv) es in
  verify_identifier id ;
  verify_nrfn_target loc cf id ;
  check_nrfn loc tenv id tes

(* assignments *)
let verify_assignment_read_only loc is_readonly id =
  if is_readonly then
    Semantic_error.cannot_assign_to_read_only loc id.name |> error

(* Variables from previous blocks are read-only.
     In particular, data and parameters never assigned to
*)
let verify_assignment_global loc cf block is_global id =
  if (not is_global) || block = cf.current_block then ()
  else Semantic_error.cannot_assign_to_global loc id.name |> error

(* Until function types are added to the user language, we
   disallow assignments to function values
*)
let verify_assignment_non_function loc ut id =
  match ut with
  | UnsizedType.UFun _ | UMathLibraryFunction ->
      Semantic_error.cannot_assign_function loc ut id.name |> error
  | _ -> ()

let check_assignment_operator loc assop lhs rhs =
  let err op =
    Semantic_error.illtyped_assignment loc op lhs.lmeta.type_ rhs.emeta.type_
  in
  match assop with
  | Assign | ArrowAssign -> (
    match
      SignatureMismatch.check_of_same_type_mod_conv lhs.lmeta.type_
        rhs.emeta.type_
    with
    | Ok p -> Promotion.promote rhs p
    | Error _ -> err Operator.Equals |> error )
  | OperatorAssign op -> (
      let args = List.map ~f:arg_type [Ast.expr_of_lvalue lhs; rhs] in
      let return_type = assignmentoperator_stan_math_return_type op args in
      match return_type with Some Void -> rhs | _ -> err op |> error )

let check_lvalue cf tenv = function
  | {lval= LVariable id; lmeta= ({loc} : located_meta)} ->
      verify_identifier id ;
      let ad_level, type_ = check_id cf loc tenv id in
      {lval= LVariable id; lmeta= {ad_level; type_; loc}}
  | {lval= LIndexed (lval, idcs); lmeta= {loc}} ->
      let rec check_inner = function
        | {lval= LVariable id; lmeta= ({loc} : located_meta)} ->
            verify_identifier id ;
            let ad_level, type_ = check_id cf loc tenv id in
            let var = {lval= LVariable id; lmeta= {ad_level; type_; loc}} in
            (var, var, [])
        | {lval= LIndexed (lval, idcs); lmeta= {loc}} ->
            let lval, var, flat = check_inner lval in
            let idcs = List.map ~f:(check_index cf tenv) idcs in
            let ad_level =
              inferred_ad_type_of_indexed lval.lmeta.ad_level idcs in
            let type_ =
              inferred_unsizedtype_of_indexed ~loc lval.lmeta.type_ idcs in
            ( {lval= LIndexed (lval, idcs); lmeta= {ad_level; type_; loc}}
            , var
            , flat @ idcs ) in
      let lval, var, flat = check_inner lval in
      let idcs = List.map ~f:(check_index cf tenv) idcs in
      let ad_level = inferred_ad_type_of_indexed lval.lmeta.ad_level idcs in
      let type_ = inferred_unsizedtype_of_indexed ~loc lval.lmeta.type_ idcs in
      if List.exists ~f:is_multiindex flat then (
        add_warning loc
          "Nested multi-indexing on the left hand side of assignment does not \
           behave the same as nested indexing in expressions. This is \
           considered a bug and will be disallowed in Stan 2.32.0. The \
           indexing can be automatically fixed using the canonicalize flag for \
           stanc." ;
        let lvalue_rvalue_types_differ =
          try
            let flat_type =
              inferred_unsizedtype_of_indexed ~loc var.lmeta.type_ (flat @ idcs)
            in
            let rec can_assign = function
              | UnsizedType.(UArray t1, UArray t2) -> can_assign (t1, t2)
              | UVector, URowVector | URowVector, UVector -> false
              | t1, t2 -> UnsizedType.compare t1 t2 <> 0 in
            can_assign (flat_type, type_)
          with Errors.SemanticError _ -> true in
        if lvalue_rvalue_types_differ then
          Semantic_error.cannot_assign_to_multiindex loc |> error ) ;
      {lval= LIndexed (lval, idcs); lmeta= {ad_level; type_; loc}}

let check_assignment loc cf tenv assign_lhs assign_op assign_rhs =
  let assign_id = Ast.id_of_lvalue assign_lhs in
  let lhs = check_lvalue cf tenv assign_lhs in
  let rhs = check_expression cf tenv assign_rhs in
  let block, global, readonly =
    let var = Env.find tenv assign_id.name in
    match var with
    | {kind= `Variable {origin; global; readonly}; _} :: _ ->
        (origin, global, readonly)
    | {kind= `StanMath; _} :: _ -> (MathLibrary, true, false)
    | {kind= `UserDefined | `UserDeclared _; _} :: _ -> (Functions, true, false)
    | _ ->
        Semantic_error.ident_not_in_scope loc assign_id.name
          (Env.nearest_ident tenv assign_id.name)
        |> error in
  verify_assignment_global loc cf block global assign_id ;
  verify_assignment_read_only loc readonly assign_id ;
  verify_assignment_non_function loc rhs.emeta.type_ assign_id ;
  let rhs' = check_assignment_operator loc assign_op lhs rhs in
  mk_typed_statement ~return_type:NoReturnType ~loc
    ~stmt:(Assignment {assign_lhs= lhs; assign_op; assign_rhs= rhs'})

(* target plus-equals / increment log-prob *)

let verify_target_pe_expr_type loc e =
  if UnsizedType.is_fun_type e.emeta.type_ then
    Semantic_error.int_or_real_container_expected loc e.emeta.type_ |> error

let verify_target_pe_usage loc cf =
  if cf.in_lp_fun_def || cf.current_block = Model then ()
  else Semantic_error.target_plusequals_outisde_model_or_logprob loc |> error

let check_target_pe loc cf tenv e =
  let te = check_expression cf tenv e in
  verify_target_pe_usage loc cf ;
  verify_target_pe_expr_type loc te ;
  mk_typed_statement ~stmt:(TargetPE te) ~return_type:NoReturnType ~loc

let check_incr_logprob loc cf tenv e =
  let te = check_expression cf tenv e in
  verify_target_pe_usage loc cf ;
  verify_target_pe_expr_type loc te ;
  mk_typed_statement ~stmt:(IncrementLogProb te) ~return_type:NoReturnType ~loc

(* tilde/sampling notation*)
let verify_sampling_pdf_pmf id =
  if
    String.(
      is_suffix id.name ~suffix:"_lpdf"
      || is_suffix id.name ~suffix:"_lpmf"
      || is_suffix id.name ~suffix:"_lupdf"
      || is_suffix id.name ~suffix:"_lupmf")
  then Semantic_error.invalid_sampling_pdf_or_pmf id.id_loc |> error

let verify_sampling_cdf_ccdf loc id =
  if
    String.(
      is_suffix id.name ~suffix:"_cdf" || is_suffix id.name ~suffix:"_ccdf")
  then Semantic_error.invalid_sampling_cdf_or_ccdf loc id.name |> error

(* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
let verify_valid_sampling_pos loc cf =
  if cf.in_lp_fun_def || cf.current_block = Model then ()
  else Semantic_error.target_plusequals_outisde_model_or_logprob loc |> error

let verify_sampling_distribution loc tenv id arguments =
  let name = id.name in
  let argumenttypes = List.map ~f:arg_type arguments in
  let name_w_suffix_sampling_dist suffix =
    SignatureMismatch.matching_function tenv (name ^ suffix) argumenttypes in
  let sampling_dists =
    List.map ~f:name_w_suffix_sampling_dist Utils.distribution_suffices in
  let is_sampling_dist_defined =
    List.exists
      ~f:(function UniqueMatch (ReturnType UReal, _, _) -> true | _ -> false)
      sampling_dists
    && name <> "binomial_coefficient"
    && name <> "multiply" in
  if is_sampling_dist_defined then ()
  else
    match
      List.max_elt sampling_dists
        ~compare:SignatureMismatch.compare_match_results
    with
    | None | Some (UniqueMatch _) | Some (SignatureErrors ([], _)) ->
        (* Either non-existant or a very odd case,
           output the old non-informative error *)
        Semantic_error.invalid_sampling_no_such_dist loc name |> error
    | Some (AmbiguousMatch sigs) ->
        Semantic_error.ambiguous_function_promotion loc id.name
          (Some (List.map ~f:type_of_expr_typed arguments))
          sigs
        |> error
    | Some (SignatureErrors (l, b)) ->
        arguments
        |> List.map ~f:(fun e -> e.emeta.type_)
        |> Semantic_error.illtyped_fn_app loc id.name (l, b)
        |> error

let is_cumulative_density_defined tenv id arguments =
  let name = id.name in
  let argumenttypes = List.map ~f:arg_type arguments in
  let valid_arg_types_for_suffix suffix =
    match
      SignatureMismatch.matching_function tenv (name ^ suffix) argumenttypes
    with
    | UniqueMatch (ReturnType UReal, _, _) -> true
    | _ -> false in
  (valid_arg_types_for_suffix "_lcdf" || valid_arg_types_for_suffix "_cdf_log")
  && ( valid_arg_types_for_suffix "_lccdf"
     || valid_arg_types_for_suffix "_ccdf_log" )

let verify_can_truncate_distribution loc (arg : typed_expression) = function
  | NoTruncate -> ()
  | _ ->
      if UnsizedType.is_scalar_type arg.emeta.type_ then ()
      else Semantic_error.multivariate_truncation loc |> error

let verify_sampling_cdf_defined loc tenv id truncation args =
  let check e = is_cumulative_density_defined tenv id (e :: args) in
  match truncation with
  | NoTruncate -> ()
  | (TruncateUpFrom e | TruncateDownFrom e) when check e -> ()
  | TruncateBetween (e1, e2) when check e1 && check e2 -> ()
  | _ -> Semantic_error.invalid_truncation_cdf_or_ccdf loc |> error

let check_truncation cf tenv truncation =
  let check e =
    check_expression_of_int_or_real_type cf tenv e "Truncation bound" in
  match truncation with
  | NoTruncate -> NoTruncate
  | TruncateUpFrom e -> check e |> TruncateUpFrom
  | TruncateDownFrom e -> check e |> TruncateDownFrom
  | TruncateBetween (e1, e2) -> (check e1, check e2) |> TruncateBetween

let check_tilde loc cf tenv distribution truncation arg args =
  let te = check_expression cf tenv arg in
  let tes = List.map ~f:(check_expression cf tenv) args in
  let ttrunc = check_truncation cf tenv truncation in
  verify_identifier distribution ;
  verify_sampling_pdf_pmf distribution ;
  verify_valid_sampling_pos loc cf ;
  verify_sampling_cdf_ccdf loc distribution ;
  verify_sampling_distribution loc tenv distribution (te :: tes) ;
  verify_sampling_cdf_defined loc tenv distribution ttrunc tes ;
  verify_can_truncate_distribution loc te ttrunc ;
  let stmt = Tilde {arg= te; distribution; args= tes; truncation= ttrunc} in
  mk_typed_statement ~stmt ~loc ~return_type:NoReturnType

(* Break and continue only occur in loops. *)
let check_break loc cf =
  if cf.loop_depth = 0 then Semantic_error.break_outside_loop loc |> error
  else mk_typed_statement ~stmt:Break ~return_type:NoReturnType ~loc

let check_continue loc cf =
  if cf.loop_depth = 0 then Semantic_error.continue_outside_loop loc |> error
  else mk_typed_statement ~stmt:Continue ~return_type:NoReturnType ~loc

let check_return loc cf tenv e =
  if not cf.in_returning_fun_def then
    Semantic_error.expression_return_outside_returning_fn loc |> error
  else
    let te = check_expression cf tenv e in
    mk_typed_statement ~stmt:(Return te)
      ~return_type:(Complete (ReturnType te.emeta.type_)) ~loc

let check_returnvoid loc cf =
  if (not cf.in_fun_def) || cf.in_returning_fun_def then
    Semantic_error.void_ouside_nonreturning_fn loc |> error
  else mk_typed_statement ~stmt:ReturnVoid ~return_type:(Complete Void) ~loc

let check_printable cf tenv = function
  | PString s -> PString s
  (* Print/reject expressions cannot be of function type. *)
  | PExpr e -> (
      let te = check_expression cf tenv e in
      match te.emeta.type_ with
      | UFun _ | UMathLibraryFunction ->
          Semantic_error.not_printable te.emeta.loc |> error
      | _ -> PExpr te )

let check_print loc cf tenv ps =
  let tps = List.map ~f:(check_printable cf tenv) ps in
  mk_typed_statement ~stmt:(Print tps) ~return_type:NoReturnType ~loc

let check_reject loc cf tenv ps =
  let tps = List.map ~f:(check_printable cf tenv) ps in
  mk_typed_statement ~stmt:(Reject tps) ~return_type:AnyReturnType ~loc

let check_skip loc =
  mk_typed_statement ~stmt:Skip ~return_type:NoReturnType ~loc

let rec stmt_is_escape {stmt; _} =
  match stmt with
  | Break | Continue | Reject _ | Return _ | ReturnVoid -> true
  | _ -> false

and list_until_escape xs =
  let rec aux accu = function
    | [next; next'] when stmt_is_escape next' -> List.rev (next' :: next :: accu)
    | next :: next' :: unreachable :: _ when stmt_is_escape next' ->
        add_warning unreachable.smeta.loc
          "Unreachable statement (following a reject, break, continue, or \
           return) found, is this intended?" ;
        List.rev (next' :: next :: accu)
    | next :: rest -> aux (next :: accu) rest
    | [] -> List.rev accu in
  aux [] xs

let returntype_leastupperbound loc rt1 rt2 =
  match (rt1, rt2) with
  | UnsizedType.ReturnType UReal, UnsizedType.ReturnType UInt
   |ReturnType UInt, ReturnType UReal ->
      UnsizedType.ReturnType UReal
  | _, _ when rt1 = rt2 -> rt2
  | _ -> Semantic_error.mismatched_return_types loc rt1 rt2 |> error

let try_compute_block_statement_returntype loc srt1 srt2 =
  match (srt1, srt2) with
  | Complete rt1, Complete rt2 | Incomplete rt1, Complete rt2 ->
      Complete (returntype_leastupperbound loc rt1 rt2)
  | Incomplete rt1, Incomplete rt2 | Complete rt1, Incomplete rt2 ->
      Incomplete (returntype_leastupperbound loc rt1 rt2)
  | NoReturnType, NoReturnType -> NoReturnType
  | AnyReturnType, Incomplete rt
   |Complete rt, NoReturnType
   |NoReturnType, Incomplete rt
   |Incomplete rt, NoReturnType ->
      Incomplete rt
  | NoReturnType, Complete rt
   |Complete rt, AnyReturnType
   |Incomplete rt, AnyReturnType
   |AnyReturnType, Complete rt ->
      Complete rt
  | AnyReturnType, NoReturnType
   |NoReturnType, AnyReturnType
   |AnyReturnType, AnyReturnType ->
      AnyReturnType

let try_compute_ifthenelse_statement_returntype loc srt1 srt2 =
  match (srt1, srt2) with
  | Complete rt1, Complete rt2 ->
      returntype_leastupperbound loc rt1 rt2 |> Complete
  | Incomplete rt1, Incomplete rt2
   |Complete rt1, Incomplete rt2
   |Incomplete rt1, Complete rt2 ->
      returntype_leastupperbound loc rt1 rt2 |> Incomplete
  | AnyReturnType, NoReturnType
   |NoReturnType, AnyReturnType
   |NoReturnType, NoReturnType ->
      NoReturnType
  | AnyReturnType, Incomplete rt
   |Incomplete rt, AnyReturnType
   |Complete rt, NoReturnType
   |NoReturnType, Complete rt
   |NoReturnType, Incomplete rt
   |Incomplete rt, NoReturnType ->
      Incomplete rt
  | Complete rt, AnyReturnType | AnyReturnType, Complete rt -> Complete rt
  | AnyReturnType, AnyReturnType -> AnyReturnType

(* statements which contain statements, and therefore need to be mutually recursive
   with check_statement
*)
let rec check_if_then_else loc cf tenv pred_e s_true s_false_opt =
  (* we don't need these nested type environments *)
  let _, ts_true = check_statement cf tenv s_true in
  let ts_false_opt =
    s_false_opt |> Option.map ~f:(check_statement cf tenv) |> Option.map ~f:snd
  in
  let te =
    check_expression_of_int_or_real_type cf tenv pred_e
      "Condition in conditional" in
  let stmt = IfThenElse (te, ts_true, ts_false_opt) in
  let srt1 = ts_true.smeta.return_type in
  let srt2 =
    ts_false_opt
    |> Option.map ~f:(fun s -> s.smeta.return_type)
    |> Option.value ~default:NoReturnType in
  let return_type = try_compute_ifthenelse_statement_returntype loc srt1 srt2 in
  mk_typed_statement ~stmt ~return_type ~loc

and check_while loc cf tenv cond_e loop_body =
  let _, ts =
    check_statement {cf with loop_depth= cf.loop_depth + 1} tenv loop_body
  and te =
    check_expression_of_int_or_real_type cf tenv cond_e
      "Condition in while-loop" in
  mk_typed_statement
    ~stmt:(While (te, ts))
    ~return_type:ts.smeta.return_type ~loc

and check_for loc cf tenv loop_var lower_bound_e upper_bound_e loop_body =
  let te1 =
    check_expression_of_int_type cf tenv lower_bound_e "Lower bound of for-loop"
  and te2 =
    check_expression_of_int_type cf tenv upper_bound_e "Upper bound of for-loop"
  in
  verify_identifier loop_var ;
  let ts = check_loop_body cf tenv loop_var UnsizedType.UInt loop_body in
  mk_typed_statement
    ~stmt:
      (For
         { loop_variable= loop_var
         ; lower_bound= te1
         ; upper_bound= te2
         ; loop_body= ts } )
    ~return_type:ts.smeta.return_type ~loc

and check_foreach_loop_identifier_type loc ty =
  match ty with
  | UnsizedType.UArray ut -> ut
  | UVector | URowVector | UMatrix -> UnsizedType.UReal
  | _ -> Semantic_error.array_vector_rowvector_matrix_expected loc ty |> error

and check_foreach loc cf tenv loop_var foreach_e loop_body =
  let te = check_expression cf tenv foreach_e in
  verify_identifier loop_var ;
  let loop_var_ty =
    check_foreach_loop_identifier_type te.emeta.loc te.emeta.type_ in
  let ts = check_loop_body cf tenv loop_var loop_var_ty loop_body in
  mk_typed_statement
    ~stmt:(ForEach (loop_var, te, ts))
    ~return_type:ts.smeta.return_type ~loc

and check_loop_body cf tenv loop_var loop_var_ty loop_body =
  verify_name_fresh tenv loop_var ~is_udf:false ;
  (* Add to type environment as readonly.
     Check that function args and loop identifiers are not modified in
     function. (passed by const ref)
  *)
  let tenv =
    Env.add tenv loop_var.name loop_var_ty
      (`Variable {origin= cf.current_block; global= false; readonly= true})
  in
  snd (check_statement {cf with loop_depth= cf.loop_depth + 1} tenv loop_body)

and check_block loc cf tenv stmts =
  let _, checked_stmts =
    List.fold_map stmts ~init:tenv ~f:(check_statement cf) in
  let return_type =
    checked_stmts |> list_until_escape
    |> List.map ~f:(fun s -> s.smeta.return_type)
    |> List.fold ~init:NoReturnType
         ~f:(try_compute_block_statement_returntype loc) in
  mk_typed_statement ~stmt:(Block checked_stmts) ~return_type ~loc

and check_profile loc cf tenv name stmts =
  let _, checked_stmts =
    List.fold_map stmts ~init:tenv ~f:(check_statement cf) in
  let return_type =
    checked_stmts |> list_until_escape
    |> List.map ~f:(fun s -> s.smeta.return_type)
    |> List.fold ~init:NoReturnType
         ~f:(try_compute_block_statement_returntype loc) in
  mk_typed_statement ~stmt:(Profile (name, checked_stmts)) ~return_type ~loc

(* variable declarations *)
and verify_valid_transformation_for_type loc is_global sized_ty trans =
  let is_real {emeta; _} = emeta.type_ = UReal in
  let is_real_transformation =
    match trans with
    | Transformation.Lower e -> is_real e
    | Upper e -> is_real e
    | LowerUpper (e1, e2) -> is_real e1 || is_real e2
    | _ -> false in
  if is_global && sized_ty = SizedType.SInt && is_real_transformation then
    Semantic_error.non_int_bounds loc |> error ;
  let is_transformation =
    match trans with Transformation.Identity -> false | _ -> true in
  if is_global && SizedType.(contains_complex sized_ty) && is_transformation
  then Semantic_error.complex_transform loc |> error

and verify_transformed_param_ty loc cf is_global unsized_ty =
  if
    is_global
    && (cf.current_block = Param || cf.current_block = TParam)
    && UnsizedType.is_int_type unsized_ty
  then Semantic_error.transformed_params_int loc |> error

and check_sizedtype cf tenv sizedty =
  let check e msg = check_expression_of_int_type cf tenv e msg in
  match sizedty with
  | SizedType.SInt -> SizedType.SInt
  | SReal -> SReal
  | SComplex -> SComplex
  | SVector (mem_pattern, e) ->
      let te = check e "Vector sizes" in
      SVector (mem_pattern, te)
  | SRowVector (mem_pattern, e) ->
      let te = check e "Row vector sizes" in
      SRowVector (mem_pattern, te)
  | SMatrix (mem_pattern, e1, e2) ->
      let te1 = check e1 "Matrix row size" in
      let te2 = check e2 "Matrix column size" in
      SMatrix (mem_pattern, te1, te2)
  | SComplexVector e ->
      let te = check e "complex vector sizes" in
      SComplexVector te
  | SComplexRowVector e ->
      let te = check e "complex row vector sizes" in
      SComplexRowVector te
  | SComplexMatrix (e1, e2) ->
      let te1 = check e1 "Complex matrix row size" in
      let te2 = check e2 "Complex matrix column size" in
      SComplexMatrix (te1, te2)
  | SArray (st, e) ->
      let tst = check_sizedtype cf tenv st in
      let te = check e "Array sizes" in
      SArray (tst, te)

and check_var_decl_initial_value loc cf tenv id init_val_opt =
  match init_val_opt with
  | Some e -> (
      let lhs = check_lvalue cf tenv {lval= LVariable id; lmeta= {loc}} in
      let rhs = check_expression cf tenv e in
      match
        SignatureMismatch.check_of_same_type_mod_conv lhs.lmeta.type_
          rhs.emeta.type_
      with
      | Ok p -> Some (Promotion.promote rhs p)
      | Error _ ->
          Semantic_error.illtyped_assignment loc Equals lhs.lmeta.type_
            rhs.emeta.type_
          |> error )
  | None -> None

and check_transformation cf tenv ut trans =
  let check e msg = check_expression_of_scalar_or_type cf tenv ut e msg in
  match trans with
  | Transformation.Identity -> Transformation.Identity
  | Lower e -> check e "Lower bound" |> Lower
  | Upper e -> check e "Upper bound" |> Upper
  | LowerUpper (e1, e2) ->
      (check e1 "Lower bound", check e2 "Upper bound") |> LowerUpper
  | Offset e -> check e "Offset" |> Offset
  | Multiplier e -> check e "Multiplier" |> Multiplier
  | OffsetMultiplier (e1, e2) ->
      (check e1 "Offset", check e2 "Multiplier") |> OffsetMultiplier
  | Ordered -> Ordered
  | PositiveOrdered -> PositiveOrdered
  | Simplex -> Simplex
  | UnitVector -> UnitVector
  | CholeskyCorr -> CholeskyCorr
  | CholeskyCov -> CholeskyCov
  | Correlation -> Correlation
  | Covariance -> Covariance

and check_var_decl loc cf tenv sized_ty trans id init is_global =
  let checked_type =
    check_sizedtype {cf with in_toplevel_decl= is_global} tenv sized_ty in
  let unsized_type = SizedType.to_unsized checked_type in
  let checked_trans = check_transformation cf tenv unsized_type trans in
  verify_identifier id ;
  verify_name_fresh tenv id ~is_udf:false ;
  let tenv =
    Env.add tenv id.name unsized_type
      (`Variable {origin= cf.current_block; global= is_global; readonly= false})
  in
  let tinit = check_var_decl_initial_value loc cf tenv id init in
  verify_valid_transformation_for_type loc is_global checked_type checked_trans ;
  verify_transformed_param_ty loc cf is_global unsized_type ;
  let stmt =
    VarDecl
      { decl_type= Sized checked_type
      ; transformation= checked_trans
      ; identifier= id
      ; initial_value= tinit
      ; is_global } in
  (tenv, mk_typed_statement ~stmt ~loc ~return_type:NoReturnType)

(* function definitions *)
and exists_matching_fn_declared tenv id arg_tys rt =
  let options =
    List.concat_map ~f:(Env.find tenv) (distribution_name_variants id.name)
  in
  let f = function
    | Env.{kind= `UserDeclared _; type_= UFun (listedtypes, rt', _, _)}
      when arg_tys = listedtypes && rt = rt' ->
        true
    | _ -> false in
  List.exists ~f options

and verify_unique_signature tenv loc id arg_tys rt =
  let existing =
    List.concat_map ~f:(Env.find tenv) (distribution_name_variants id.name)
  in
  let same_args = function
    | Env.{type_= UFun (listedtypes, _, _, _); _}
      when List.map ~f:snd arg_tys = List.map ~f:snd listedtypes ->
        true
    | _ -> false in
  match List.filter existing ~f:same_args with
  | [] -> ()
  | {type_= UFun (_, rt', _, _); _} :: _ when rt <> rt' ->
      Semantic_error.fn_overload_rt_only loc id.name rt rt' |> error
  | {kind; _} :: _ ->
      Semantic_error.fn_decl_redefined loc id.name
        ~stan_math:(kind = `StanMath)
        (UnsizedType.UFun (arg_tys, rt, Fun_kind.suffix_from_name id.name, AoS))
      |> error

and verify_fundef_overloaded loc tenv id arg_tys rt =
  if exists_matching_fn_declared tenv id arg_tys rt then
    (* this is the definition to an existing forward declaration *)
    ()
  else
    (* this should be an overload with a unique signature *)
    verify_unique_signature tenv loc id arg_tys rt ;
  verify_name_fresh tenv id ~is_udf:true

and get_fn_decl_or_defn loc tenv id arg_tys rt body =
  match body with
  | {stmt= Skip; _} ->
      if exists_matching_fn_declared tenv id arg_tys rt then
        Semantic_error.fn_decl_without_def loc |> error
      else `UserDeclared id.id_loc
  | _ -> `UserDefined

and verify_fundef_dist_rt loc id return_ty =
  let is_dist =
    List.exists
      ~f:(fun x -> String.is_suffix id.name ~suffix:x)
      Utils.conditioning_suffices_w_log in
  if is_dist then
    match return_ty with
    | UnsizedType.ReturnType UReal -> ()
    | _ -> Semantic_error.non_real_prob_fn_def loc |> error

and verify_pdf_fundef_first_arg_ty loc id arg_tys =
  if String.is_suffix id.name ~suffix:"_lpdf" then
    let rt = List.hd arg_tys |> Option.map ~f:snd in
    match rt with
    | Some rt when not (UnsizedType.is_int_type rt) -> ()
    | _ -> Semantic_error.prob_density_non_real_variate loc rt |> error

and verify_pmf_fundef_first_arg_ty loc id arg_tys =
  if String.is_suffix id.name ~suffix:"_lpmf" then
    let rt = List.hd arg_tys |> Option.map ~f:snd in
    match rt with
    | Some rt when UnsizedType.is_int_type rt -> ()
    | _ -> Semantic_error.prob_mass_non_int_variate loc rt |> error

and verify_fundef_distinct_arg_ids loc arg_names =
  let dup_exists l =
    List.find_a_dup ~compare:String.compare l |> Option.is_some in
  if dup_exists arg_names then Semantic_error.duplicate_arg_names loc |> error

and verify_fundef_return_tys loc return_type body =
  if
    body.stmt = Skip
    || is_of_compatible_return_type return_type body.smeta.return_type
  then ()
  else Semantic_error.incompatible_return_types loc |> error

and add_function tenv name type_ defined =
  (* if we're providing a definition, we remove prior declarations
     to simplify the environment *)
  if defined = `UserDefined then
    let existing_defns = Env.find tenv name in
    let defns =
      List.filter
        ~f:(function
          | Env.{kind= `UserDeclared _; type_= type'} when type' = type_ ->
              false
          | _ -> true )
        existing_defns in
    let new_fn = Env.{kind= `UserDefined; type_} in
    Env.set_raw tenv name (new_fn :: defns)
  else Env.add tenv name type_ defined

and check_fundef loc cf tenv return_ty id args body =
  List.iter args ~f:(fun (_, _, id) -> verify_identifier id) ;
  verify_identifier id ;
  let arg_types = List.map ~f:(fun (w, y, _) -> (w, y)) args in
  let arg_identifiers = List.map ~f:(fun (_, _, z) -> z) args in
  let arg_names = List.map ~f:(fun x -> x.name) arg_identifiers in
  verify_fundef_overloaded loc tenv id arg_types return_ty ;
  let defined = get_fn_decl_or_defn loc tenv id arg_types return_ty body in
  verify_fundef_dist_rt loc id return_ty ;
  verify_pdf_fundef_first_arg_ty loc id arg_types ;
  verify_pmf_fundef_first_arg_ty loc id arg_types ;
  let tenv =
    add_function tenv id.name
      (UFun (arg_types, return_ty, Fun_kind.suffix_from_name id.name, AoS))
      defined in
  List.iter
    ~f:(fun id -> verify_name_fresh tenv id ~is_udf:false)
    arg_identifiers ;
  verify_fundef_distinct_arg_ids loc arg_names ;
  (* We treat DataOnly arguments as if they are data and AutoDiffable arguments
      as if they are parameters, for the purposes of type checking.
  *)
  let arg_types_internal =
    List.map
      ~f:(function
        | UnsizedType.DataOnly, ut -> (Env.Data, ut)
        | AutoDiffable, ut -> (Param, ut) )
      arg_types in
  let tenv_body =
    List.fold2_exn arg_names arg_types_internal ~init:tenv
      ~f:(fun env name (origin, typ) ->
        Env.add env name typ
          (* readonly so that function args and loop identifiers
             are not modified in function. (passed by const ref) *)
          (`Variable {origin; readonly= true; global= false}) ) in
  let context =
    let is_udf_dist name =
      List.exists
        ~f:(fun suffix -> String.is_suffix name ~suffix)
        Utils.distribution_suffices in
    { cf with
      in_fun_def= true
    ; in_rng_fun_def= String.is_suffix id.name ~suffix:"_rng"
    ; in_lp_fun_def= String.is_suffix id.name ~suffix:"_lp"
    ; in_udf_dist_def= is_udf_dist id.name
    ; in_returning_fun_def= return_ty <> Void } in
  let _, checked_body = check_statement context tenv_body body in
  verify_fundef_return_tys loc return_ty checked_body ;
  let stmt =
    FunDef
      {returntype= return_ty; funname= id; arguments= args; body= checked_body}
  in
  (* NB: **not** tenv_body, so args don't leak out *)
  (tenv, mk_typed_statement ~return_type:NoReturnType ~loc ~stmt)

and check_statement (cf : context_flags_record) (tenv : Env.t)
    (s : Ast.untyped_statement) : Env.t * typed_statement =
  let loc = s.smeta.loc in
  match s.stmt with
  | NRFunApp (_, id, es) -> (tenv, check_nr_fn_app loc cf tenv id es)
  | Assignment {assign_lhs; assign_op; assign_rhs} ->
      (tenv, check_assignment loc cf tenv assign_lhs assign_op assign_rhs)
  | TargetPE e -> (tenv, check_target_pe loc cf tenv e)
  | IncrementLogProb e -> (tenv, check_incr_logprob loc cf tenv e)
  | Tilde {arg; distribution; args; truncation} ->
      (tenv, check_tilde loc cf tenv distribution truncation arg args)
  | Break -> (tenv, check_break loc cf)
  | Continue -> (tenv, check_continue loc cf)
  | Return e -> (tenv, check_return loc cf tenv e)
  | ReturnVoid -> (tenv, check_returnvoid loc cf)
  | Print ps -> (tenv, check_print loc cf tenv ps)
  | Reject ps -> (tenv, check_reject loc cf tenv ps)
  | Skip -> (tenv, check_skip loc)
  (* the following can contain further statements *)
  | IfThenElse (e, s1, os2) -> (tenv, check_if_then_else loc cf tenv e s1 os2)
  | While (e, s) -> (tenv, check_while loc cf tenv e s)
  | For {loop_variable; lower_bound; upper_bound; loop_body} ->
      ( tenv
      , check_for loc cf tenv loop_variable lower_bound upper_bound loop_body )
  | ForEach (id, e, s) -> (tenv, check_foreach loc cf tenv id e s)
  | Block stmts -> (tenv, check_block loc cf tenv stmts)
  | Profile (name, vdsl) -> (tenv, check_profile loc cf tenv name vdsl)
  | VarDecl {decl_type= Unsized _; _} ->
      (* currently unallowed by parser *)
      Common.FatalError.fatal_error_msg
        [%message "Don't support unsized declarations yet."]
  (* these two are special in that they're allowed to change the type environment *)
  | VarDecl
      {decl_type= Sized st; transformation; identifier; initial_value; is_global}
    ->
      check_var_decl loc cf tenv st transformation identifier initial_value
        is_global
  | FunDef {returntype; funname; arguments; body} ->
      check_fundef loc cf tenv returntype funname arguments body

let verify_fun_def_body_in_block = function
  | {stmt= FunDef {body= {stmt= Block _; _}; _}; _}
   |{stmt= FunDef {body= {stmt= Skip; _}; _}; _} ->
      ()
  | {stmt= FunDef {body= {stmt= _; smeta}; _}; _} ->
      Semantic_error.fn_decl_needs_block smeta.loc |> error
  | _ -> ()

let verify_functions_have_defn tenv function_block_stmts_opt =
  let error_on_undefined funs =
    List.iter funs ~f:(fun f ->
        match f with
        | Env.{kind= `UserDeclared loc; _} ->
            Semantic_error.fn_decl_without_def loc |> error
        | _ -> () ) in
  if !check_that_all_functions_have_definition then
    Env.iter tenv error_on_undefined ;
  match function_block_stmts_opt with
  | Some {stmts= []; _} | None -> ()
  | Some {stmts= ls; _} -> List.iter ~f:verify_fun_def_body_in_block ls

let check_toplevel_block block tenv stmts_opt =
  let cf = context block in
  match stmts_opt with
  | Some {stmts; xloc} ->
      let tenv', stmts =
        List.fold_map stmts ~init:tenv ~f:(check_statement cf) in
      (tenv', Some {stmts; xloc})
  | None -> (tenv, None)

let verify_correctness_invariant (ast : untyped_program)
    (decorated_ast : typed_program) =
  let detyped = untyped_program_of_typed_program decorated_ast in
  if compare_untyped_program ast detyped = 0 then ()
  else
    Common.FatalError.fatal_error_msg
      [%message
        "Type checked AST does not match original AST. "
          (detyped : untyped_program)
          (ast : untyped_program)]

let check_program_exn
    ( { functionblock= fb
      ; datablock= db
      ; transformeddatablock= tdb
      ; parametersblock= pb
      ; transformedparametersblock= tpb
      ; modelblock= mb
      ; generatedquantitiesblock= gqb
      ; comments } as ast ) =
  warnings := [] ;
  (* create a new type environment which has only stan-math functions *)
  let tenv = Env.stan_math_environment in
  let tenv, typed_fb = check_toplevel_block Functions tenv fb in
  verify_functions_have_defn tenv typed_fb ;
  let tenv, typed_db = check_toplevel_block Data tenv db in
  let tenv, typed_tdb = check_toplevel_block TData tenv tdb in
  let tenv, typed_pb = check_toplevel_block Param tenv pb in
  let tenv, typed_tpb = check_toplevel_block TParam tenv tpb in
  let _, typed_mb = check_toplevel_block Model tenv mb in
  let _, typed_gqb = check_toplevel_block GQuant tenv gqb in
  let prog =
    { functionblock= typed_fb
    ; datablock= typed_db
    ; transformeddatablock= typed_tdb
    ; parametersblock= typed_pb
    ; transformedparametersblock= typed_tpb
    ; modelblock= typed_mb
    ; generatedquantitiesblock= typed_gqb
    ; comments } in
  verify_correctness_invariant ast prog ;
  attach_warnings prog

let check_program ast =
  try Result.Ok (check_program_exn ast)
  with Errors.SemanticError err -> Result.Error err
