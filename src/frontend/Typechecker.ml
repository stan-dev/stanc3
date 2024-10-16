(** a type/semantic checker for Stan ASTs

  Functions which begin with "check_" return a typed version of their input
  Functions which begin with "verify_" return unit if a check succeeds, or else
    throw an Errors.SemanticError exception.
  Other functions which begin with "infer"/"calculate" vary. Usually they return
    a value, but a few do have error conditions.

  All Error.SemanticError exceptions are caught by check_program
  which turns the ast or exception into a Result.t for external usage

  A type environment (Env.t) is used to hold variables and functions, including
  stan math functions. This is a functional map, meaning it is handled immutably.
*)

open Core
open Core.Poly
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

type function_indicator =
  | NotInFunction
  | NonReturning of unit Fun_kind.suffix
  | Returning of unit Fun_kind.suffix * UnsizedType.t

(* Record structure holding flags and other markers about context to be
   used for error reporting. *)
type context_flags_record =
  { current_block: Env.originblock
  ; in_toplevel_decl: bool
  ; containing_function: function_indicator
  ; loop_depth: int }

let in_function cf = cf.containing_function <> NotInFunction

let in_rng_function cf =
  match cf.containing_function with
  | NonReturning FnRng | Returning (FnRng, _) -> true
  | _ -> false

let in_lp_function cf =
  match cf.containing_function with
  | NonReturning FnTarget | Returning (FnTarget, _) -> true
  | _ -> false

let in_jacobian_function cf =
  match cf.containing_function with
  | NonReturning FnJacobian | Returning (FnJacobian, _) -> true
  | _ -> false

let in_udf_distribution cf =
  match cf.containing_function with
  | NonReturning (FnLpdf ()) | Returning (FnLpdf (), _) -> true
  | _ -> false

let context block =
  { current_block= block
  ; in_toplevel_decl= false
  ; containing_function= NotInFunction
  ; loop_depth= 0 }

let rec calculate_autodifftype cf origin ut =
  let ut, _ = UnsizedType.unwind_array_type ut in
  match (origin, ut) with
  | _, UTuple ts ->
      UnsizedType.TupleAD (List.map ~f:(calculate_autodifftype cf origin) ts)
  | Env.(Param | TParam | Model | Functions), _
    when not (UnsizedType.is_discrete_type ut || cf.current_block = GQuant) ->
      UnsizedType.AutoDiffable
  | _, _ -> DataOnly

let arg_type x = (x.emeta.ad_level, x.emeta.type_)
let get_arg_types = List.map ~f:arg_type
let type_of_expr_typed ue = ue.emeta.type_
let has_int_type ue = ue.emeta.type_ = UInt
let has_int_array_type ue = ue.emeta.type_ = UArray UInt

let rec name_of_lval lv =
  match lv.lval with
  | LVariable id -> id.name
  | LTupleProjection (lv, _) -> name_of_lval lv
  | LIndexed (lv, _) -> name_of_lval lv

let has_int_or_real_type ue =
  match ue.emeta.type_ with UInt | UReal -> true | _ -> false

(* -- General checks ---------------------------------------------- *)
let reserved_keywords =
  (* parser stops most keywords currently in use, but we still have some extra
     reserved for the future *)
  [ "generated"; "quantities"; "transformed"; "repeat"; "until"; "then"; "true"
  ; "false"; "typedef"; "struct"; "var"; "export"; "extern"; "static"; "auto" ]

let verify_identifier id : unit =
  if id.name = "jacobian" then
    add_warning id.id_loc
      "Variable name 'jacobian' will be a reserved word starting in Stan 2.38. \
       Please rename it!";
  if id.name = !model_name then
    Semantic_error.ident_is_model_name id.id_loc id.name |> error;
  if
    String.is_suffix id.name ~suffix:"__"
    || List.mem reserved_keywords id.name ~equal:String.equal
  then Semantic_error.ident_is_keyword id.id_loc id.name |> error

(** verify that the variable being declared is previous unused.
   allowed to shadow StanLib *)
let verify_name_fresh_var loc tenv name =
  if Utils.is_unnormalized_distribution name then
    Semantic_error.ident_has_unnormalized_suffix loc name |> error
  else if
    List.exists (Env.find tenv name) ~f:(function
      | {kind= `Variable _; _} -> true
      | _ -> false (* user variables can shadow function names *))
  then Semantic_error.ident_in_use loc name |> error

(** verify that the variable being declared is previous unused. *)
let verify_name_fresh_udf loc tenv name =
  if
    (* variadic functions are currently not in math sigs and aren't
       overloadable due to their separate typechecking *)
    Stan_math_signatures.is_reduce_sum_fn name
    || Stan_math_signatures.is_stan_math_variadic_function_name name
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
  if is_udf then verify_name_fresh_udf id.id_loc tenv id.name
  else verify_name_fresh_var id.id_loc tenv id.name

let is_of_compatible_return_type rt1 srt2 =
  UnsizedType.(
    match (rt1, srt2) with
    | Void, _ -> true
    | ReturnType _, Complete -> true
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
  | UInt, Some type_, Some ad_level when not (UnsizedType.is_fun_type type_) ->
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
  match
    Hashtbl.find Stan_math_signatures.stan_math_variadic_signatures name
  with
  | Some {return_type; _} -> Some (UnsizedType.ReturnType return_type)
  | None when Stan_math_signatures.is_reduce_sum_fn name ->
      Some (UnsizedType.ReturnType UReal)
  | None ->
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
             | _ -> None)
      |> List.hd

let assignmentoperator_stan_math_return_type assop arg_tys =
  (match assop with
  | Operator.Divide ->
      SignatureMismatch.matching_stanlib_function "divide" arg_tys
      |> match_to_rt_option
  | Plus | Minus | Times | EltTimes | EltDivide ->
      operator_stan_math_return_type assop arg_tys |> Option.map ~f:fst
  | _ -> None)
  |> Option.bind ~f:(function
       | ReturnType rtype
         when rtype = snd (List.hd_exn arg_tys)
              && not
                   ((assop = Operator.EltTimes || assop = Operator.EltDivide)
                   && UnsizedType.is_scalar_type rtype) ->
           Some UnsizedType.Void
       | _ -> None)

let check_binop loc op le re =
  let rt = [le; re] |> get_arg_types |> operator_stan_math_return_type op in
  match (rt, expr_ad_lub [le; re]) with
  | Some (ReturnType type_, [p1; p2]), Some ad_level ->
      mk_typed_expression
        ~expr:(BinOp (Promotion.promote le p1, op, Promotion.promote re p2))
        ~ad_level ~type_ ~loc
  | _ ->
      Semantic_error.illtyped_binary_op loc op le.emeta.type_ re.emeta.type_
      |> error

let check_prefixop loc op te =
  let rt = operator_stan_math_return_type op [arg_type te] in
  match rt with
  | Some (ReturnType type_, _) ->
      mk_typed_expression
        ~expr:(PrefixOp (op, te))
        ~ad_level:te.emeta.ad_level ~type_ ~loc
  | _ -> Semantic_error.illtyped_prefix_op loc op te.emeta.type_ |> error

let check_postfixop loc op te =
  let rt = operator_stan_math_return_type op [arg_type te] in
  match rt with
  | Some (ReturnType type_, _) ->
      mk_typed_expression
        ~expr:(PostfixOp (te, op))
        ~ad_level:te.emeta.ad_level ~type_ ~loc
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
              ((in_udf_distribution cf || in_lp_function cf)
              || cf.current_block = Model) ->
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

let get_consistent_types type_ es =
  let f state e =
    Result.bind state ~f:(fun ty ->
        match UnsizedType.common_type (ty, e.emeta.type_) with
        | Some ty -> Ok ty
        | None -> Error (ty, e.emeta)) in
  List.fold ~init:(Ok type_) ~f es
  |> Result.map ~f:(fun ty ->
         let ad =
           expr_ad_lub es |> Option.value_exn
           (* correctness: Result.Ok case only contains tuples of same lengths, expr_ad_lub cannot fail *)
         in
         let promotions =
           List.map (get_arg_types es)
             ~f:(Promotion.get_type_promotion_exn (ad, ty)) in
         (ad, ty, promotions))

let check_array_expr loc es =
  match es with
  | [] ->
      (* NB: This is actually disallowed by parser *)
      Semantic_error.empty_array loc |> error
  | {emeta= {type_; _}; _} :: _ -> (
      match get_consistent_types type_ es with
      | Error (ty, meta) ->
          Semantic_error.mismatched_array_types meta.loc ty meta.type_ |> error
      | Ok (ad_level, type_, promotions) ->
          let type_ = UnsizedType.UArray type_ in
          mk_typed_expression
            ~expr:(ArrayExpr (Promotion.promote_list es promotions))
            ~ad_level ~type_ ~loc)

let check_rowvector loc es =
  match es with
  | {emeta= {type_= UnsizedType.URowVector; _}; _} :: _ -> (
      match get_consistent_types URowVector es with
      | Ok (ad_level, typ, promotions) ->
          mk_typed_expression
            ~expr:(RowVectorExpr (Promotion.promote_list es promotions))
            ~ad_level
            ~type_:(if typ = UComplexRowVector then UComplexMatrix else UMatrix)
            ~loc
      | Error (_, meta) ->
          Semantic_error.invalid_matrix_types meta.loc meta.type_ |> error)
  | {emeta= {type_= UnsizedType.UComplexRowVector; _}; _} :: _ -> (
      match get_consistent_types UComplexRowVector es with
      | Ok (ad_level, _, promotions) ->
          mk_typed_expression
            ~expr:(RowVectorExpr (Promotion.promote_list es promotions))
            ~ad_level ~type_:UComplexMatrix ~loc
      | Error (_, meta) ->
          Semantic_error.invalid_matrix_types meta.loc meta.type_ |> error)
  | _ -> (
      match get_consistent_types UReal es with
      | Ok (ad_level, typ, promotions) ->
          mk_typed_expression
            ~expr:(RowVectorExpr (Promotion.promote_list es promotions))
            ~ad_level
            ~type_:(if typ = UComplex then UComplexRowVector else URowVector)
            ~loc
      | Error (_, meta) ->
          Semantic_error.invalid_row_vector_types meta.loc meta.type_ |> error)

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
     |( (UInt | UReal | UComplex | UFun _ | UMathLibraryFunction | UTuple _)
      , _ :: _ ) ->
        Semantic_error.not_indexable loc ut (List.length indices) |> error in
  aux ut (List.map ~f:indexing_type indices)

let inferred_ad_type_of_indexed at ut uindices =
  UnsizedType.fill_adtype_for_type
    (* correctness: index expressions only contain int types,
       so lub_ad_tupe should never be [None]. *)
    (UnsizedType.lub_ad_type
       (at
       :: List.map
            ~f:(function
              | All -> UnsizedType.DataOnly
              | Single ue1 | Upfrom ue1 | Downfrom ue1 -> ue1.emeta.ad_level
              | Between (ue1, ue2) ->
                  UnsizedType.lub_ad_type
                    [ue1.emeta.ad_level; ue2.emeta.ad_level]
                  |> Option.value_exn)
            uindices)
    |> Option.value_exn)
    ut

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
  then Semantic_error.conditioning_required loc |> error

(** `Target+=` can only be used in model and functions
    with right suffix (same for tilde etc)
*)
let verify_fn_target_plus_equals cf loc id =
  if String.is_suffix id.name ~suffix:"_lp" then
    if cf.current_block = TParam then
      add_warning loc
        "Using _lp functions in transformed parameters is deprecated and will \
         be disallowed in Stan 2.39. Use an _jacobian function instead, as \
         this allows change of variable adjustments which are conditionally \
         enabled by the algorithms."
    else if in_lp_function cf || cf.current_block = Model then ()
    else Semantic_error.target_plusequals_outside_model_or_logprob loc |> error

let verify_fn_jacobian_plus_equals cf loc id =
  if
    String.is_suffix id.name ~suffix:"_jacobian"
    && not (in_jacobian_function cf || cf.current_block = TParam)
  then Semantic_error.jacobian_plusequals_not_allowed loc |> error

(** Rng functions cannot be used in Tp or Model and only
    in function defs with the right suffix
*)
let verify_fn_rng cf loc id =
  if String.is_suffix id.name ~suffix:"_rng" && cf.in_toplevel_decl then
    Semantic_error.invalid_decl_rng_fn loc |> error
  else if
    String.is_suffix id.name ~suffix:"_rng"
    && ((in_function cf && not (in_rng_function cf))
       || cf.current_block = TParam || cf.current_block = Model)
  then Semantic_error.invalid_rng_fn loc |> error

(** unnormalized _lpdf/_lpmf functions can only be used in _lpdf/_lpmf/_lp udfs
    or the model block
*)
let verify_unnormalized cf loc id =
  if
    Utils.is_unnormalized_distribution id.name
    && not (in_udf_distribution cf || cf.current_block = Model)
  then Semantic_error.invalid_unnormalized_fn loc |> error

let mk_fun_app ~is_cond_dist ~loc kind name args ~type_ : Ast.typed_expression =
  let fn =
    if is_cond_dist then CondDistApp (kind, name, args)
    else FunApp (kind, name, args) in
  let ad_type =
    if UnsizedType.is_discrete_type type_ then UnsizedType.DataOnly
    else if
      UnsizedType.any_autodiff (List.map ~f:(fun x -> x.emeta.ad_level) args)
    then AutoDiffable
    else DataOnly in
  mk_typed_expression ~expr:fn ~loc ~type_
    ~ad_level:(UnsizedType.fill_adtype_for_type ad_type type_)

let check_normal_fn ~is_cond_dist loc tenv id es =
  match Env.find tenv (Utils.normalized_name id.name) with
  | {kind= `Variable _; _} :: _
  (* variables can sometimes shadow stanlib functions, so we have to check this *)
    when not
           (Stan_math_signatures.is_stan_math_function_name
              (Utils.normalized_name id.name)) ->
      Semantic_error.returning_fn_expected_nonfn_found loc id.name |> error
  | [] ->
      (match Utils.split_distribution_suffix id.name with
      | Some (prefix, suffix) -> (
          let known_families =
            List.map
              ~f:(fun (_, y, _, _) -> y)
              Stan_math_signatures.distributions in
          let is_known_family s =
            List.mem known_families s ~equal:String.equal in
          match suffix with
          | ("lpmf" | "lupmf") when Env.mem tenv (prefix ^ "_lpdf") ->
              Semantic_error.returning_fn_expected_wrong_dist_suffix_found loc
                (prefix, suffix)
          | ("lpdf" | "lupdf") when Env.mem tenv (prefix ^ "_lpmf") ->
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
                  (Env.nearest_ident tenv id.name))
      | None ->
          Semantic_error.returning_fn_expected_undeclaredident_found loc id.name
            (Env.nearest_ident tenv id.name))
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
          mk_fun_app ~is_cond_dist ~loc
            (fnk (Fun_kind.suffix_from_name id.name))
            id
            (Promotion.promote_list es promotions)
            ~type_:ut
      | AmbiguousMatch sigs ->
          Semantic_error.ambiguous_function_promotion loc id.name
            (Some (List.map ~f:type_of_expr_typed es))
            sigs
          |> error
      | SignatureErrors (l, b) ->
          es
          |> List.map ~f:(fun e -> e.emeta.type_)
          |> Semantic_error.illtyped_fn_app loc id.name (l, b)
          |> error)

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
        | _ -> None)
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
      Common.ICE.internal_compiler_error
        [%message
          "Attempting to create function variable out of "
            (type_ : UnsizedType.t)]

let rec check_fn ~is_cond_dist loc cf tenv id (tes : Ast.typed_expression list)
    =
  if Stan_math_signatures.is_stan_math_variadic_function_name id.name then
    check_variadic ~is_cond_dist loc cf tenv id tes
  else if Stan_math_signatures.is_reduce_sum_fn id.name then
    check_reduce_sum ~is_cond_dist loc cf tenv id tes
  else check_normal_fn ~is_cond_dist loc tenv id tes

(** Reduce sum is a special case, even compared to the other
    variadic functions, because it is polymorphic in the type of the
    first argument. The first, fourth, and fifth arguments must agree,
    which is too complicated to be captured declaratively. *)
and check_reduce_sum ~is_cond_dist loc cf tenv id tes =
  let basic_mismatch () =
    let mandatory_args =
      UnsizedType.[(AutoDiffable, UArray UReal); (AutoDiffable, UInt)] in
    let mandatory_fun_args =
      UnsizedType.
        [(AutoDiffable, UArray UReal); (DataOnly, UInt); (DataOnly, UInt)] in
    SignatureMismatch.check_variadic_args ~allow_lpdf:true mandatory_args
      mandatory_fun_args UReal (get_arg_types tes) in
  let matching remaining_es fn =
    match fn with
    | Env.{type_= UnsizedType.UFun (sliced_arg_fun :: _, _, _, _) as ftype; _}
      ->
        let mandatory_args = [sliced_arg_fun; (AutoDiffable, UInt)] in
        let mandatory_fun_args =
          [sliced_arg_fun; (DataOnly, UInt); (DataOnly, UInt)] in
        let arg_types =
          (calculate_autodifftype cf Functions ftype, ftype)
          :: get_arg_types remaining_es in
        SignatureMismatch.check_variadic_args ~allow_lpdf:true mandatory_args
          mandatory_fun_args UReal arg_types
    | _ -> basic_mismatch () in
  match tes with
  | {expr= Variable fname; _}
    :: ({emeta= {type_= slice_type; _}; _} :: _ as remaining_es) -> (
      let slice_type, n = UnsizedType.unwind_array_type slice_type in
      if n = 0 then
        Semantic_error.illtyped_reduce_sum_not_array loc slice_type |> error
      else if
        not
        @@ List.mem Stan_math_signatures.reduce_sum_slice_types slice_type
             ~equal:( = )
      then Semantic_error.illtyped_reduce_sum_slice loc slice_type |> error;
      match find_matching_first_order_fn tenv (matching remaining_es) fname with
      | SignatureMismatch.UniqueMatch (ftype, promotions) ->
          (* a valid signature exists *)
          let tes = make_function_variable cf loc fname ftype :: remaining_es in
          mk_fun_app ~is_cond_dist ~loc (StanLib FnPlain) id
            (Promotion.promote_list tes promotions)
            ~type_:UnsizedType.UReal
      | AmbiguousMatch ps ->
          Semantic_error.ambiguous_function_promotion loc fname.name None ps
          |> error
      | SignatureErrors (expected_args, err) ->
          Semantic_error.illtyped_reduce_sum loc id.name
            (List.map ~f:type_of_expr_typed tes)
            expected_args err
          |> error)
  | _ ->
      let expected_args, err =
        basic_mismatch () |> Result.error |> Option.value_exn in
      Semantic_error.illtyped_reduce_sum loc id.name
        (List.map ~f:type_of_expr_typed tes)
        expected_args err
      |> error

and check_variadic ~is_cond_dist loc cf tenv id tes =
  let Stan_math_signatures.
        {control_args; required_fn_args; required_fn_rt; return_type} =
    Hashtbl.find_exn Stan_math_signatures.stan_math_variadic_signatures id.name
  in
  let matching remaining_es Env.{type_= ftype; _} =
    let arg_types =
      (calculate_autodifftype cf Functions ftype, ftype)
      :: get_arg_types remaining_es in
    SignatureMismatch.check_variadic_args ~allow_lpdf:false control_args
      required_fn_args required_fn_rt arg_types in
  match tes with
  | {expr= Variable fname; _} :: remaining_es -> (
      match find_matching_first_order_fn tenv (matching remaining_es) fname with
      | SignatureMismatch.UniqueMatch (ftype, promotions) ->
          let tes = make_function_variable cf loc fname ftype :: remaining_es in
          mk_fun_app ~is_cond_dist ~loc (StanLib FnPlain) id
            (Promotion.promote_list tes promotions)
            ~type_:return_type
      | AmbiguousMatch ps ->
          Semantic_error.ambiguous_function_promotion loc fname.name None ps
          |> error
      | SignatureErrors (expected_args, err) ->
          Semantic_error.illtyped_variadic loc id.name
            (List.map ~f:type_of_expr_typed tes)
            expected_args required_fn_rt err
          |> error)
  | _ ->
      let expected_args, err =
        SignatureMismatch.check_variadic_args ~allow_lpdf:false control_args
          required_fn_args required_fn_rt (get_arg_types tes)
        |> Result.error |> Option.value_exn in
      Semantic_error.illtyped_variadic loc id.name
        (List.map ~f:type_of_expr_typed tes)
        expected_args required_fn_rt err
      |> error

and check_funapp loc cf tenv ~is_cond_dist id (es : Ast.typed_expression list) =
  let name_check =
    if is_cond_dist then verify_conddist_name else verify_fn_conditioning in
  let res = check_fn ~is_cond_dist loc cf tenv id es in
  verify_identifier id;
  name_check loc id;
  verify_fn_target_plus_equals cf loc id;
  verify_fn_jacobian_plus_equals cf loc id;
  verify_fn_rng cf loc id;
  verify_unnormalized cf loc id;
  res

and check_indexed loc cf tenv e indices =
  let tindices = List.map ~f:(check_index cf tenv) indices in
  let te = check_expression cf tenv e in
  let type_ = inferred_unsizedtype_of_indexed ~loc te.emeta.type_ tindices in
  let ad_level = inferred_ad_type_of_indexed te.emeta.ad_level type_ tindices in
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
        | _ when Operator.is_cmp op -> (
            match le.expr with
            | BinOp (e1, op2, e2) when Operator.is_cmp op2 ->
                let pp_e = Pretty_printing.pp_typed_expression in
                let pp = Operator.pp in
                add_warning loc
                  (Fmt.str
                     "Found %a. This is interpreted as %a. Consider if the \
                      intended meaning was %a instead.@ You can silence this \
                      warning by adding explicit parenthesis. This can be \
                      automatically changed using the canonicalize flag for \
                      stanc"
                     (fun ppf () ->
                       Fmt.pf ppf "@[<hov>%a %a %a@]" pp_e le pp op2 pp_e re)
                     ()
                     (fun ppf () ->
                       Fmt.pf ppf "@[<hov>(%a) %a %a@]" pp_e le pp op2 pp_e re)
                     ()
                     (fun ppf () ->
                       Fmt.pf ppf "@[<hov>%a %a %a && %a %a %a@]" pp_e e1 pp op
                         pp_e e2 pp_e e2 pp op2 pp_e re)
                     ())
            | _ -> ())
        | _ -> () in
      binop_type_warnings le re;
      check_binop loc op le re
  | PrefixOp (op, e) -> ce e |> check_prefixop loc op
  | PostfixOp (e, op) -> ce e |> check_postfixop loc op
  | Variable id ->
      verify_identifier id;
      check_variable cf loc tenv id
  | IntNumeral s -> (
      match float_of_string_opt s with
      | Some i when i < 2_147_483_648.0 ->
          mk_typed_expression ~expr:(IntNumeral s) ~ad_level:DataOnly
            ~type_:UInt ~loc
      | _ -> Semantic_error.bad_int_literal loc |> error)
  | RealNumeral s ->
      mk_typed_expression ~expr:(RealNumeral s) ~ad_level:DataOnly ~type_:UReal
        ~loc
  | ImagNumeral s ->
      mk_typed_expression ~expr:(ImagNumeral s) ~ad_level:DataOnly
        ~type_:UComplex ~loc
  | GetTarget ->
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      if
        not
          (in_lp_function cf || cf.current_block = Model
         || cf.current_block = TParam)
      then
        Semantic_error.target_plusequals_outside_model_or_logprob loc |> error
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
  | TupleProjection (e, i) -> (
      let te = ce e in
      match (te.emeta.type_, te.emeta.ad_level) with
      | UTuple ts, TupleAD ads -> (
          match (List.nth ts (i - 1), List.nth ads (i - 1)) with
          | Some t, Some ad ->
              mk_typed_expression
                ~expr:(TupleProjection (te, i))
                ~ad_level:ad ~type_:t ~loc:emeta.loc
          | None, None ->
              Semantic_error.tuple_index_invalid_index emeta.loc
                (List.length ts) i
              |> error
          | _ ->
              Common.ICE.internal_compiler_error
                [%message
                  "Error in internal representation: tuple types don't match AD"]
          )
      | UTuple _, ad ->
          Common.ICE.internal_compiler_error
            [%message
              "Error in internal representation: tuple doesn't have tupleAD"
                (ad : UnsizedType.autodifftype)]
      | _, _ ->
          Semantic_error.tuple_index_not_tuple emeta.loc te.emeta.type_ |> error
      )
  | TupleExpr es ->
      let tes = List.map ~f:ce es in
      if List.is_empty tes then Semantic_error.empty_tuple emeta.loc |> error
      else
        mk_typed_expression ~expr:(TupleExpr tes)
          ~ad_level:(TupleAD (List.map ~f:(fun e -> e.emeta.ad_level) tes))
          ~type_:(UTuple (List.map ~f:(fun e -> e.emeta.type_) tes))
          ~loc:emeta.loc
  | FunApp ((), id, es) ->
      es |> List.map ~f:ce |> check_funapp loc cf tenv ~is_cond_dist:false id
  | CondDistApp ((), id, es) ->
      es |> List.map ~f:ce |> check_funapp loc cf tenv ~is_cond_dist:true id
  | Promotion (e, _, _) ->
      (* Should never happen: promotions are produced during typechecking *)
      Common.ICE.internal_compiler_error
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
         (in_lp_function cf || cf.current_block = Model
        || cf.current_block = TParam)
  then Semantic_error.target_plusequals_outside_model_or_logprob loc |> error

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
                 , Promotion.promote_list es promotions ))
            ~return_type:Incomplete ~loc
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
          |> error)

let check_nr_fn_app loc cf tenv id es =
  let tes = List.map ~f:(check_expression cf tenv) es in
  verify_identifier id;
  verify_nrfn_target loc cf id;
  check_nrfn loc tenv id tes

(* target plus-equals / jacobian plus-equals *)

let verify_target_pe_expr_type loc e =
  if UnsizedType.is_fun_type e.emeta.type_ then
    Semantic_error.int_or_real_container_expected loc e.emeta.type_ |> error

let verify_target_pe_usage loc cf =
  if in_lp_function cf || cf.current_block = Model then ()
  else Semantic_error.target_plusequals_outside_model_or_logprob loc |> error

let check_target_pe loc cf tenv e =
  let te = check_expression cf tenv e in
  verify_target_pe_usage loc cf;
  verify_target_pe_expr_type loc te;
  mk_typed_statement ~stmt:(TargetPE te) ~return_type:Incomplete ~loc

let verify_jacobian_pe_usage loc cf =
  if in_jacobian_function cf || cf.current_block = TParam then ()
  else Semantic_error.jacobian_plusequals_not_allowed loc |> error

let check_jacobian_pe loc cf tenv e =
  let te = check_expression cf tenv e in
  verify_jacobian_pe_usage loc cf;
  verify_target_pe_expr_type loc te;
  mk_typed_statement ~stmt:(JacobianPE te) ~return_type:Incomplete ~loc

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
let rec verify_assignment_non_function loc id ut =
  match ut with
  | UnsizedType.UFun _ | UMathLibraryFunction ->
      Semantic_error.cannot_assign_function loc id ut |> error
  | UTuple ts -> List.iter ~f:(verify_assignment_non_function loc id) ts
  | _ -> ()

(** We issue a warning if the initial value for a declaration contains
    any reference to the variable being declared *)
let warn_self_declare loc variable_name rhs_opt =
  Option.iter rhs_opt ~f:(fun rhs ->
      let rhs_ids = Ast.extract_ids rhs in
      if List.exists rhs_ids ~f:(fun id -> String.equal id.name variable_name)
      then
        add_warning loc
          "Assignment of variable to itself during declaration. This is almost \
           certainly a bug.")

(** For general assignments, we only warn if we believe the lhs and rhs are exactly the same value *)
let warn_self_assignment loc lhs rhs =
  let rec strip_parens e =
    match e.expr with Paren e -> strip_parens e | _ -> e in
  let rhs_opt =
    rhs |> Ast.untyped_expression_of_typed_expression |> strip_parens
    |> Ast.lvalue_of_expr_opt in
  Option.iter rhs_opt ~f:(fun rhs ->
      let lhs = lhs |> Ast.untyped_lvalue_of_typed_lvalue_pack in
      if Ast.compare_untyped_lval_pack lhs rhs = 0 then
        add_warning loc "Assignment of variable to itself.")

let check_assignment_operator loc assop lhs rhs =
  let rec type_of_lvalue = function
    | LValue {lmeta; _} -> lmeta.type_
    | LTuplePack {lvals; _} ->
        UnsizedType.UTuple (List.map ~f:type_of_lvalue lvals) in
  let err lhs op rhs =
    let loc = Ast.get_loc_lvalue_pack lhs in
    Semantic_error.illtyped_assignment loc op (type_of_lvalue lhs) rhs |> error
  in
  match assop with
  | Assign ->
      warn_self_assignment loc lhs rhs;
      let rec typechk lhs rhs =
        match (lhs, rhs) with
        | LValue ({lmeta= {type_; ad_level; _}; _} as lval), rhs -> (
            verify_assignment_non_function loc (name_of_lval lval) rhs;
            match SignatureMismatch.check_of_same_type_mod_conv type_ rhs with
            | Ok p -> (p, ad_level)
            | Error _ -> err lhs Equals rhs)
        | LTuplePack {lvals; _}, UnsizedType.UTuple tps ->
            let proms, ad_levels =
              match List.map2 ~f:typechk lvals tps with
              | Unequal_lengths -> err lhs Equals rhs
              | Ok l -> List.unzip l in
            if
              List.exists ~f:(function NoPromotion -> false | _ -> true) proms
            then (TuplePromotion proms, TupleAD ad_levels)
            else (NoPromotion, TupleAD ad_levels)
        | LTuplePack _, rhs -> err lhs Equals rhs in
      let prom, ad_level = typechk lhs rhs.emeta.type_ in
      let rhs =
        (* Hack: need RHS to properly get promoted to var if needed *)
        {rhs with emeta= {rhs.emeta with ad_level}} in
      Promotion.promote rhs prom
  | OperatorAssign op -> (
      let args =
        [(UnsizedType.AutoDiffable, type_of_lvalue lhs); arg_type rhs] in
      let return_type = assignmentoperator_stan_math_return_type op args in
      match return_type with
      | Some Void -> rhs
      | _ -> err lhs op rhs.emeta.type_)

let overlapping_lvalues lvals =
  (* Prevent assigning to multiple indices in the same object at once.
     In principle it is safe to assign to disjoint indices, but statically
     checking that is impossible. *)
  let rec compare_no_indexing lv1 lv2 =
    match (lv1.lval, lv2.lval) with
    | LIndexed (l, _), _ -> compare_no_indexing l lv2
    | _, LIndexed (l, _) -> compare_no_indexing lv1 l
    | LVariable id1, LVariable id2 -> String.compare id1.name id2.name
    | LTupleProjection (lv1, idx1), LTupleProjection (lv2, idx2)
      when idx1 = idx2 ->
        compare_no_indexing lv1 lv2
    | _, _ ->
        (* remaining cases are not equal, we don't care *)
        Ast.compare_untyped_lval lv1 lv2 in
  List.find_all_dups lvals ~compare:compare_no_indexing

let lvalues_written_to lv =
  let rec add_tuple_idxs lv : typed_lval list =
    (* If we're assigning an entire tuple, we also need to prevent
       assigning to any slot in this statement. *)
    let type_, _ = UnsizedType.unwind_array_type lv.lmeta.type_ in
    match (lv.lval, type_) with
    | _, UTuple ts ->
        List.concat_mapi ts ~f:(fun i ty ->
            add_tuple_idxs
              { lval= LTupleProjection (lv, i + 1)
              ; lmeta= {lv.lmeta with type_= ty} })
    | _ -> [lv] in
  let rec flatten_lvalue_pack lv =
    match lv with
    | LTuplePack {lvals; _} -> List.concat_map ~f:flatten_lvalue_pack lvals
    | LValue lv -> [lv] in
  flatten_lvalue_pack lv
  |> List.concat_map ~f:add_tuple_idxs
  |> List.map ~f:Ast.untyped_lvalue_of_typed_lvalue

let variables_accessed_in lv =
  let exprs_in_index = function
    | All -> []
    | Single e -> [e]
    | Upfrom e -> [e]
    | Downfrom e -> [e]
    | Between (e1, e2) -> [e1; e2] in
  (* We only care about values being read inside the lvalue here,
     which means only the expressions inside of LIndexed *)
  let rec extract_indices lv =
    match lv.lval with
    | LVariable _ -> String.Set.empty
    | LTupleProjection (lv, _) -> extract_indices lv
    | LIndexed (lv, es) ->
        List.concat_map ~f:exprs_in_index es
        |> List.concat_map ~f:extract_ids
        |> List.map ~f:(fun {name; _} -> name)
        |> String.Set.of_list
        |> Set.union (extract_indices lv) in
  let rec extract_indices_pack lv =
    match lv with
    | LTuplePack {lvals; _} ->
        String.Set.union_list (List.map ~f:extract_indices_pack lvals)
    | LValue lv -> extract_indices lv in
  extract_indices_pack lv

(** When an lvalue is assigning to multiple places at once (e.g., a tuple
    is being unpacked), we want to ensure that the same memory is not being
    written to multiple times. This lets us preserve the illusion of
    simultaneous assignment and generally avoid confusion.
    For the same reasons, we also want to avoid reading the value of
    a variable which is being updated in this same lvalue. *)
let verify_lvalue_unique (lv : Ast.typed_lval_pack) =
  let loc = Ast.get_loc_lvalue_pack lv in
  let all_lvals = lvalues_written_to lv in
  let () =
    (* check that things being assigned to are all unique *)
    match overlapping_lvalues all_lvals with
    | [] -> ()
    | dupes ->
        Semantic_error.cannot_assign_duplicate_unpacking loc dupes |> error
  in
  (* check that things being assigned to are not also being read
     Note: this is much less refined than the above and forbids some
     cases that would be harmless, but this is also in general a very
     weird thing to try to do, so I think that is acceptable
  *)
  let all_variables = List.map ~f:name_of_lval all_lvals |> String.Set.of_list in
  let accessed_lvals = variables_accessed_in lv in
  match Set.inter accessed_lvals all_variables |> Set.to_list with
  | [] -> ()
  | dupes -> Semantic_error.cannot_access_assigning_var loc dupes |> error

let verify_assignable_id loc cf tenv assign_id =
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
  verify_assignment_global loc cf block global assign_id;
  verify_assignment_read_only loc readonly assign_id

let rec check_lvalue cf tenv {lval; lmeta= ({loc} : located_meta)} =
  match lval with
  | LVariable id ->
      verify_identifier id;
      verify_assignable_id id.id_loc cf tenv id;
      let ad_level, type_ = check_id cf loc tenv id in
      {lval= LVariable id; lmeta= {ad_level; type_; loc}}
  | LTupleProjection (lval, idx) -> (
      let tlval = (check_lvalue cf tenv) lval in
      match (tlval.lmeta.type_, tlval.lmeta.ad_level) with
      | UTuple types_, TupleAD ads -> (
          match (List.nth types_ (idx - 1), List.nth ads (idx - 1)) with
          | Some type_, Some ad_level ->
              { lval= LTupleProjection (tlval, idx)
              ; lmeta= {ad_level; type_; loc} }
          | None, None ->
              Semantic_error.tuple_index_invalid_index loc (List.length types_)
                idx
              |> error
          | _ ->
              Common.ICE.internal_compiler_error
                [%message
                  "Error in internal representation: tuple types don't match AD"]
          )
      | _, _ ->
          Semantic_error.tuple_index_not_tuple loc tlval.lmeta.type_ |> error)
  | LIndexed (lval, idcs) ->
      let rec check_inner = function
        | {lval= LIndexed (lval, idcs); lmeta= ({loc} : located_meta)} ->
            let lval, flat = check_inner lval in
            let idcs = List.map ~f:(check_index cf tenv) idcs in
            let type_ =
              inferred_unsizedtype_of_indexed ~loc lval.lmeta.type_ idcs in
            let ad_level =
              inferred_ad_type_of_indexed lval.lmeta.ad_level type_ idcs in
            ( {lval= LIndexed (lval, idcs); lmeta= {ad_level; type_; loc}}
            , flat @ idcs )
        | {lval= LVariable _ | LTupleProjection _; _} as lval ->
            (*  I think the right thing to do here is treat tuples like variables *)
            let tval = check_lvalue cf tenv lval in
            (tval, []) in
      let lval, flat = check_inner lval in
      let idcs = List.map ~f:(check_index cf tenv) idcs in
      let type_ = inferred_unsizedtype_of_indexed ~loc lval.lmeta.type_ idcs in
      let ad_level =
        inferred_ad_type_of_indexed lval.lmeta.ad_level type_ idcs in
      if List.exists ~f:is_multiindex flat then
        Semantic_error.cannot_assign_to_multiindex loc |> error;
      {lval= LIndexed (lval, idcs); lmeta= {ad_level; type_; loc}}

let rec check_lvalues cf tenv = function
  | LValue l -> LValue (check_lvalue cf tenv l)
  | LTuplePack {lvals; loc} ->
      let lvals = List.map lvals ~f:(check_lvalues cf tenv) in
      LTuplePack {lvals; loc}

let check_assignment loc cf tenv assign_lhs assign_op assign_rhs =
  (* TODO(2.38): Remove this workaround *)
  match (assign_lhs, assign_op, Env.find tenv "jacobian") with
  | LValue {lval= LVariable {name= "jacobian"; _}; _}, OperatorAssign Plus, []
    ->
      (* if jacobian is not a user-defined variable, and we find a statement like "jacobian +=",
         we can assume it is the new statement
      *)
      check_jacobian_pe loc cf tenv assign_rhs
  | _ ->
      let lhs = check_lvalues cf tenv assign_lhs in
      verify_lvalue_unique lhs;
      let rhs = check_expression cf tenv assign_rhs in
      let rhs' = check_assignment_operator loc assign_op lhs rhs in
      mk_typed_statement ~return_type:Incomplete ~loc
        ~stmt:(Assignment {assign_lhs= lhs; assign_op; assign_rhs= rhs'})

(* tilde/distribution notation*)
let verify_distribution_pdf_pmf id =
  if
    String.(
      is_suffix id.name ~suffix:"_lpdf"
      || is_suffix id.name ~suffix:"_lpmf"
      || is_suffix id.name ~suffix:"_lupdf"
      || is_suffix id.name ~suffix:"_lupmf")
  then Semantic_error.invalid_tilde_pdf_or_pmf id.id_loc |> error

let verify_distribution_cdf_ccdf loc id =
  if
    String.(
      is_suffix id.name ~suffix:"_cdf" || is_suffix id.name ~suffix:"_ccdf")
  then Semantic_error.invalid_tilde_cdf_or_ccdf loc id.name |> error

(* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
let verify_valid_distribution_pos loc cf =
  if in_lp_function cf || cf.current_block = Model then ()
  else Semantic_error.target_plusequals_outside_model_or_logprob loc |> error

let check_tilde_distribution loc tenv id arguments =
  let name = id.name in
  let argumenttypes = List.map ~f:arg_type arguments in
  let name_w_suffix_dist suffix =
    SignatureMismatch.matching_function tenv (name ^ suffix) argumenttypes in
  let distributions =
    List.map ~f:name_w_suffix_dist Utils.distribution_suffices in
  match
    List.min_elt distributions ~compare:SignatureMismatch.compare_match_results
  with
  | Some (UniqueMatch (_, _, p)) ->
      Promotion.promote_list arguments p
      (* real return type is enforced by [verify_fundef_dist_rt] *)
  | None | Some (SignatureErrors ([], _)) ->
      (* Function is non existent *)
      Semantic_error.invalid_tilde_no_such_dist loc name
        (List.hd_exn argumenttypes |> snd |> UnsizedType.is_int_type)
      |> error
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
  valid_arg_types_for_suffix "_lcdf" && valid_arg_types_for_suffix "_lccdf"

let verify_distribution_cdf_defined loc tenv id truncation args =
  let check e =
    if not (is_cumulative_density_defined tenv id (e :: args)) then
      Semantic_error.invalid_truncation_cdf_or_ccdf loc
        (get_arg_types (e :: args))
      |> error in
  match truncation with
  | NoTruncate -> ()
  | TruncateUpFrom e | TruncateDownFrom e -> check e
  | TruncateBetween (e1, e2) ->
      check e1;
      check e2

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
  verify_identifier distribution;
  verify_distribution_pdf_pmf distribution;
  verify_valid_distribution_pos loc cf;
  verify_distribution_cdf_ccdf loc distribution;
  let promoted_args =
    check_tilde_distribution loc tenv distribution (te :: tes) in
  let te, tes = (List.hd_exn promoted_args, List.tl_exn promoted_args) in
  verify_distribution_cdf_defined loc tenv distribution ttrunc tes;
  let stmt = Tilde {arg= te; distribution; args= tes; truncation= ttrunc} in
  mk_typed_statement ~stmt ~loc ~return_type:Incomplete

(* Break and continue only occur in loops. *)
let check_break loc cf =
  if cf.loop_depth = 0 then Semantic_error.break_outside_loop loc |> error
  else mk_typed_statement ~stmt:Break ~return_type:NonlocalControlFlow ~loc

let check_continue loc cf =
  if cf.loop_depth = 0 then Semantic_error.continue_outside_loop loc |> error
  else mk_typed_statement ~stmt:Continue ~return_type:Incomplete ~loc

let check_return loc cf tenv e =
  match cf.containing_function with
  | NotInFunction | NonReturning _ ->
      Semantic_error.expression_return_outside_returning_fn loc |> error
  | Returning (_, expected) -> (
      let te = check_expression cf tenv e in
      let actual = te.emeta.type_ in
      match SignatureMismatch.check_of_same_type_mod_conv expected actual with
      | Ok _ ->
          (* we ignore the promotion from SignatureMismatch so we can get one that _also_
             ensures our returns use local_scalar_t *)
          let promotions =
            Promotion.get_type_promotion_exn
              ( UnsizedType.fill_adtype_for_type UnsizedType.AutoDiffable
                  expected
              , expected )
              (te.emeta.ad_level, actual) in
          let promoted = Promotion.promote te promotions in
          mk_typed_statement ~stmt:(Return promoted) ~return_type:Complete ~loc
      | _ -> Semantic_error.invalid_return loc expected actual |> error)

let check_returnvoid loc cf =
  match cf.containing_function with
  | NonReturning _ ->
      mk_typed_statement ~stmt:ReturnVoid ~return_type:Complete ~loc
  | _ -> Semantic_error.void_outside_nonreturning_fn loc |> error

let check_printable cf tenv = function
  | PString s -> PString s
  (* Print/reject expressions cannot be of function type. *)
  | PExpr e -> (
      let te = check_expression cf tenv e in
      match te.emeta.type_ with
      | UFun _ | UMathLibraryFunction ->
          Semantic_error.not_printable te.emeta.loc |> error
      | _ -> PExpr te)

let check_print loc cf tenv ps =
  let tps = List.map ~f:(check_printable cf tenv) ps in
  mk_typed_statement ~stmt:(Print tps) ~return_type:Incomplete ~loc

let check_reject loc cf tenv ps =
  let tps = List.map ~f:(check_printable cf tenv) ps in
  mk_typed_statement ~stmt:(Reject tps) ~return_type:Complete ~loc

let check_fatal_error loc cf tenv ps =
  let tps = List.map ~f:(check_printable cf tenv) ps in
  mk_typed_statement ~stmt:(FatalError tps) ~return_type:Complete ~loc

let check_skip loc = mk_typed_statement ~stmt:Skip ~return_type:Incomplete ~loc

let rec stmt_is_escape {stmt; _} =
  match stmt with
  | Break | Continue | Reject _ | FatalError _ | Return _ | ReturnVoid -> true
  | _ -> false

and list_until_escape xs =
  let rec aux accu = function
    | next' :: unreachable :: _ when stmt_is_escape next' ->
        add_warning unreachable.smeta.loc
          "Unreachable statement (following a reject, fatal_error, break, \
           continue, or return) found, is this intended?";
        List.rev (next' :: accu)
    | next :: rest -> aux (next :: accu) rest
    | [] -> List.rev accu in
  aux [] xs

let compute_block_statement_returntype srt1 srt2 =
  match (srt1, srt2) with
  | Complete, Complete | Incomplete, Complete -> Complete
  | NonlocalControlFlow, _ | _, NonlocalControlFlow -> NonlocalControlFlow
  | _ -> Incomplete

let compute_ifthenelse_statement_returntype srt1 srt2 =
  match (srt1, srt2) with
  | Complete, Complete -> Complete
  | NonlocalControlFlow, _ | _, NonlocalControlFlow -> NonlocalControlFlow
  | _ -> Incomplete

(* when we exit a loop, the loop's entire return type is either complete or not *)
let compute_loop_statement_returntype = function
  | Complete -> Complete
  | Incomplete | NonlocalControlFlow -> Incomplete

(* statements which contain statements, and therefore need to be mutually recursive
   with check_statement
*)
let rec check_if_then_else loc cf tenv pred_e s_true s_false_opt =
  let te =
    check_expression_of_int_type cf tenv pred_e "Condition in conditional" in
  (* we don't need these nested type environments *)
  let _, ts_true = check_statement cf tenv s_true in
  let ts_false_opt =
    s_false_opt |> Option.map ~f:(check_statement cf tenv) |> Option.map ~f:snd
  in
  let stmt = IfThenElse (te, ts_true, ts_false_opt) in
  let srt1 = ts_true.smeta.return_type in
  let srt2 =
    ts_false_opt
    |> Option.map ~f:(fun s -> s.smeta.return_type)
    |> Option.value ~default:Incomplete in
  let return_type = compute_ifthenelse_statement_returntype srt1 srt2 in
  mk_typed_statement ~stmt ~return_type ~loc

and check_while loc cf tenv cond_e loop_body =
  let hardcoded_true e =
    (* heuristic for "will this loop forever" *)
    match e.expr with
    | Ast.IntNumeral s -> String.exists s ~f:(fun c -> c > '0' && c <= '9')
    | _ -> false in
  let te =
    check_expression_of_int_type cf tenv cond_e "Condition in while-loop" in
  let _, ts =
    check_statement {cf with loop_depth= cf.loop_depth + 1} tenv loop_body in
  let return_type =
    match ts.smeta.return_type with
    | Complete -> Complete
    | Incomplete when hardcoded_true te ->
        (* if the only way out of the loop is a return or reject,
            we can consider that like a return statement *)
        Complete
    | Incomplete | NonlocalControlFlow -> Incomplete in
  mk_typed_statement ~stmt:(While (te, ts)) ~return_type ~loc

and check_for loc cf tenv loop_var lower_bound_e upper_bound_e loop_body =
  let te1 =
    check_expression_of_int_type cf tenv lower_bound_e "Lower bound of for-loop"
  and te2 =
    check_expression_of_int_type cf tenv upper_bound_e "Upper bound of for-loop"
  in
  verify_identifier loop_var;
  let ts = check_loop_body cf tenv loop_var UnsizedType.UInt loop_body in
  mk_typed_statement
    ~stmt:
      (For
         { loop_variable= loop_var
         ; lower_bound= te1
         ; upper_bound= te2
         ; loop_body= ts })
    ~return_type:(compute_loop_statement_returntype ts.smeta.return_type)
    ~loc

and check_foreach_loop_identifier_type loc ty =
  match ty with
  | UnsizedType.UArray ut -> ut
  | UVector | URowVector | UMatrix -> UnsizedType.UReal
  | _ -> Semantic_error.array_vector_rowvector_matrix_expected loc ty |> error

and check_foreach loc cf tenv loop_var foreach_e loop_body =
  let te = check_expression cf tenv foreach_e in
  verify_identifier loop_var;
  let loop_var_ty =
    check_foreach_loop_identifier_type te.emeta.loc te.emeta.type_ in
  let ts = check_loop_body cf tenv loop_var loop_var_ty loop_body in
  mk_typed_statement
    ~stmt:(ForEach (loop_var, te, ts))
    ~return_type:(compute_loop_statement_returntype ts.smeta.return_type)
    ~loc

and check_loop_body cf tenv loop_var loop_var_ty loop_body =
  verify_name_fresh tenv loop_var ~is_udf:false;
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
    |> List.fold ~init:Incomplete ~f:compute_block_statement_returntype in
  mk_typed_statement ~stmt:(Block checked_stmts) ~return_type ~loc

and check_profile loc cf tenv name stmts =
  let _, checked_stmts =
    List.fold_map stmts ~init:tenv ~f:(check_statement cf) in
  let return_type =
    checked_stmts |> list_until_escape
    |> List.map ~f:(fun s -> s.smeta.return_type)
    |> List.fold ~init:Incomplete ~f:compute_block_statement_returntype in
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
    Semantic_error.non_int_bounds loc |> error;
  let is_transformation =
    match trans with Transformation.Identity -> false | _ -> true in
  if is_global && SizedType.(is_complex_type sized_ty) && is_transformation then
    Semantic_error.complex_transform loc |> error

and verify_transformed_param_ty loc cf is_global unsized_ty =
  if
    is_global
    && (cf.current_block = Param || cf.current_block = TParam)
    && UnsizedType.contains_int unsized_ty
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
  | STuple subtypes ->
      let typed_subtypes = List.map ~f:(check_sizedtype cf tenv) subtypes in
      STuple typed_subtypes

and check_var_decl_initial_value loc cf tenv {identifier; initial_value} =
  match initial_value with
  | Some e -> (
      let lhs =
        check_lvalue cf tenv {lval= LVariable identifier; lmeta= {loc}} in
      let rhs = check_expression cf tenv e in
      let rhs =
        (* Hack: need the RHS to be promoted correctly to vars if needed *)
        {rhs with emeta= {rhs.emeta with ad_level= lhs.lmeta.ad_level}} in
      match
        SignatureMismatch.check_of_same_type_mod_conv lhs.lmeta.type_
          rhs.emeta.type_
      with
      | Ok p -> Ast.{identifier; initial_value= Some (Promotion.promote rhs p)}
      | Error _ ->
          Semantic_error.illtyped_assignment loc Equals lhs.lmeta.type_
            rhs.emeta.type_
          |> error)
  | None -> Ast.{identifier; initial_value= None}

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
  | SumToZero -> SumToZero
  | CholeskyCorr -> CholeskyCorr
  | CholeskyCov -> CholeskyCov
  | Correlation -> Correlation
  | Covariance -> Covariance
  | StochasticColumn -> StochasticColumn
  | StochasticRow -> StochasticRow
  | TupleTransformation tms ->
      let typesTrans = Utils.zip_utuple_trans_exn ut tms in
      let tes =
        List.map typesTrans ~f:(fun (ut, tm) ->
            check_transformation cf tenv ut tm) in
      TupleTransformation tes

and check_var_decl loc cf tenv sized_ty trans
    (variables : untyped_expression Ast.variable list) is_global =
  let checked_type =
    check_sizedtype {cf with in_toplevel_decl= is_global} tenv sized_ty in
  let unsized_type = SizedType.to_unsized checked_type in
  let checked_trans = check_transformation cf tenv unsized_type trans in
  let tenv, tvariables =
    List.fold_map ~init:tenv
      ~f:(fun tenv' ({identifier; initial_value} as var) ->
        verify_identifier identifier;
        verify_name_fresh tenv' identifier ~is_udf:false;
        let tenv'' =
          Env.add tenv' identifier.name unsized_type
            (`Variable
              {origin= cf.current_block; global= is_global; readonly= false})
        in
        warn_self_declare loc identifier.name initial_value;
        (tenv'', check_var_decl_initial_value loc cf tenv'' var))
      variables in
  verify_valid_transformation_for_type loc is_global checked_type checked_trans;
  verify_transformed_param_ty loc cf is_global unsized_type;
  let stmt =
    VarDecl
      { decl_type= checked_type
      ; transformation= checked_trans
      ; variables= tvariables
      ; is_global } in
  (tenv, mk_typed_statement ~stmt ~loc ~return_type:Incomplete)

(* function definitions *)
and exists_matching_fn_declared tenv id arg_tys rt =
  let options = Env.find tenv id.name in
  let f = function
    | Env.{kind= `UserDeclared _; type_= UFun (listedtypes, rt', _, _)}
      when arg_tys = listedtypes && rt = rt' ->
        true
    | _ -> false in
  List.exists ~f options

and verify_unique_signature tenv loc id arg_tys rt =
  let existing = Env.find tenv id.name in
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
      Semantic_error.fn_decl_redefined loc id.name ~stan_math:(kind = `StanMath)
        (UnsizedType.UFun (arg_tys, rt, Fun_kind.suffix_from_name id.name, AoS))
      |> error

and verify_fundef_overloaded loc tenv id arg_tys rt =
  if exists_matching_fn_declared tenv id arg_tys rt then
    (* this is the definition to an existing forward declaration *)
    ()
  else
    (* this should be an overload with a unique signature *)
    verify_unique_signature tenv loc id arg_tys rt;
  verify_name_fresh tenv id ~is_udf:true

and get_fn_decl_or_defn loc tenv id arg_tys rt body =
  match body with
  | {stmt= Skip; _} ->
      if exists_matching_fn_declared tenv id arg_tys rt then
        Semantic_error.fn_decl_exists loc id.name |> error
      else `UserDeclared id.id_loc
  | _ -> `UserDefined

and verify_fundef_dist_rt loc id return_ty =
  let is_dist =
    List.exists
      ~f:(fun x -> String.is_suffix id.name ~suffix:x)
      Utils.conditioning_suffices in
  if is_dist then
    match return_ty with
    | UnsizedType.ReturnType UReal -> ()
    | _ -> Semantic_error.non_real_prob_fn_def loc |> error

and verify_pdf_fundef_first_arg_ty loc id arg_tys =
  if String.is_suffix id.name ~suffix:"_lpdf" then
    let rt = List.hd arg_tys |> Option.map ~f:snd in
    match rt with
    | Some rt when not (UnsizedType.is_discrete_type rt) -> ()
    | _ -> Semantic_error.prob_density_non_real_variate loc rt |> error

and verify_pmf_fundef_first_arg_ty loc id arg_tys =
  if String.is_suffix id.name ~suffix:"_lpmf" then
    let rt = List.hd arg_tys |> Option.map ~f:snd in
    match rt with
    | Some rt when UnsizedType.is_discrete_type rt -> ()
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
          | _ -> true)
        existing_defns in
    let new_fn = Env.{kind= `UserDefined; type_} in
    Env.set_raw tenv name (new_fn :: defns)
  else Env.add tenv name type_ defined

and check_fundef loc cf tenv return_ty id args body =
  List.iter args ~f:(fun (_, _, id) -> verify_identifier id);
  verify_identifier id;
  let arg_types = List.map ~f:(fun (w, y, _) -> (w, y)) args in
  let arg_identifiers = List.map ~f:(fun (_, _, z) -> z) args in
  let arg_names = List.map ~f:(fun x -> x.name) arg_identifiers in
  verify_fundef_dist_rt loc id return_ty;
  verify_pdf_fundef_first_arg_ty loc id arg_types;
  verify_pmf_fundef_first_arg_ty loc id arg_types;
  List.iter
    ~f:(fun id -> verify_name_fresh tenv id ~is_udf:false)
    arg_identifiers;
  verify_fundef_distinct_arg_ids loc arg_names;
  (* We treat DataOnly arguments as if they are data and AutoDiffable arguments
      as if they are parameters, for the purposes of type checking.
  *)
  let arg_types_internal =
    List.map
      ~f:(function
        | UnsizedType.DataOnly, ut -> (Env.Data, ut)
        | AutoDiffable, ut -> (Param, ut)
        | TupleAD _, _ ->
            Common.ICE.internal_compiler_error
              [%message "TupleAD in function definition, this is unexpected!"])
      arg_types in
  let tenv_body =
    List.fold2_exn arg_names arg_types_internal ~init:tenv
      ~f:(fun env name (origin, typ) ->
        Env.add env name typ
          (* readonly so that function args and loop identifiers
             are not modified in function. (passed by const ref) *)
          (`Variable {origin; readonly= true; global= false})) in
  let context =
    let is_udf_dist name =
      List.exists
        ~f:(fun suffix -> String.is_suffix name ~suffix)
        Utils.distribution_suffices in
    let kind =
      if is_udf_dist id.name then Fun_kind.FnLpdf ()
      else if String.is_suffix id.name ~suffix:"_rng" then FnRng
      else if String.is_suffix id.name ~suffix:"_lp" then FnTarget
      else if String.is_suffix id.name ~suffix:"_jacobian" then FnJacobian
      else FnPlain in
    { cf with
      containing_function=
        UnsizedType.returntype_to_type_opt return_ty
        |> Option.value_map ~default:(NonReturning kind) ~f:(fun r ->
               Returning (kind, r)) } in
  let _, checked_body = check_statement context tenv_body body in
  verify_fundef_return_tys loc return_ty checked_body;
  let stmt =
    FunDef
      {returntype= return_ty; funname= id; arguments= args; body= checked_body}
  in
  (* NB: **not** tenv_body, so args don't leak out *)
  (tenv, mk_typed_statement ~return_type:Incomplete ~loc ~stmt)

and check_statement (cf : context_flags_record) (tenv : Env.t)
    (s : Ast.untyped_statement) : Env.t * typed_statement =
  let loc = s.smeta.loc in
  match s.stmt with
  | NRFunApp (_, id, es) -> (tenv, check_nr_fn_app loc cf tenv id es)
  | Assignment {assign_lhs; assign_op; assign_rhs} ->
      (tenv, check_assignment loc cf tenv assign_lhs assign_op assign_rhs)
  | TargetPE e -> (tenv, check_target_pe loc cf tenv e)
  | JacobianPE e -> (tenv, check_jacobian_pe loc cf tenv e)
  | Tilde {arg; distribution; args; truncation} ->
      (tenv, check_tilde loc cf tenv distribution truncation arg args)
  | Break -> (tenv, check_break loc cf)
  | Continue -> (tenv, check_continue loc cf)
  | Return e -> (tenv, check_return loc cf tenv e)
  | ReturnVoid -> (tenv, check_returnvoid loc cf)
  | Print ps -> (tenv, check_print loc cf tenv ps)
  | Reject ps -> (tenv, check_reject loc cf tenv ps)
  | FatalError ps -> (tenv, check_fatal_error loc cf tenv ps)
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
  (* these two are special in that they're allowed to change the type environment *)
  | VarDecl {decl_type; transformation; variables; is_global} ->
      check_var_decl loc cf tenv decl_type transformation variables is_global
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
  let error_on_undefined name funs =
    List.iter (List.rev funs) ~f:(fun f ->
        match f with
        | Env.{kind= `UserDeclared loc; _} ->
            Semantic_error.fn_decl_without_def loc name |> error
        | _ -> ()) in
  if !check_that_all_functions_have_definition then
    Env.iteri tenv error_on_undefined;
  match function_block_stmts_opt with
  | Some {stmts= []; _} | None -> ()
  | Some {stmts= ls; _} -> List.iter ~f:verify_fun_def_body_in_block ls

let add_userdefined_functions tenv stmts_opt =
  match stmts_opt with
  | None -> tenv
  | Some {stmts; _} ->
      let f tenv (s : Ast.untyped_statement) =
        match s with
        | {stmt= FunDef {returntype; funname; arguments; body}; smeta= {loc}} ->
            let arg_types = Ast.type_of_arguments arguments in
            verify_fundef_overloaded loc tenv funname arg_types returntype;
            let defined =
              get_fn_decl_or_defn loc tenv funname arg_types returntype body
            in
            add_function tenv funname.name
              (UFun
                 ( arg_types
                 , returntype
                 , Fun_kind.suffix_from_name funname.name
                 , AoS ))
              defined
        | _ -> tenv in
      List.fold ~init:tenv ~f stmts

let check_toplevel_block block tenv stmts_opt =
  let cf = context block in
  match stmts_opt with
  | Some {stmts; xloc} ->
      let tenv', stmts =
        List.fold_map stmts ~init:tenv ~f:(check_statement cf) in
      let _ = list_until_escape stmts in
      (tenv', Some {stmts; xloc})
  | None -> (tenv, None)

let verify_correctness_invariant (ast : untyped_program)
    (decorated_ast : typed_program) =
  let detyped = untyped_program_of_typed_program decorated_ast in
  if compare_untyped_program ast detyped = 0 then ()
  else
    Common.ICE.internal_compiler_error
      [%message
        "Type checked AST does not match original AST. "
          (detyped : untyped_program)
          (ast : untyped_program)]

let check_program_exn
    ({ functionblock= fb
     ; datablock= db
     ; transformeddatablock= tdb
     ; parametersblock= pb
     ; transformedparametersblock= tpb
     ; modelblock= mb
     ; generatedquantitiesblock= gqb
     ; comments } as ast) =
  warnings := [];
  (* create a new type environment which has only stan-math functions *)
  let tenv = Env.stan_math_environment in
  let tenv = add_userdefined_functions tenv fb in
  let tenv, typed_fb = check_toplevel_block Functions tenv fb in
  verify_functions_have_defn tenv typed_fb;
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
  verify_correctness_invariant ast prog;
  attach_warnings prog

let check_program ast =
  try Result.Ok (check_program_exn ast)
  with Errors.SemanticError err -> Result.Error err
