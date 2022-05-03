(** This module serves as the backend-specific portion
      of the typechecker. *)

open Core_kernel
open Core_kernel.Poly
open Ast
open Typechecker_utils
open Middle
open Stan_math_backend.Stan_math_signatures

let error e = raise (Errors.SemanticError e)
let reduce_sum_allowed_dimensionalities = [1; 2; 3; 4; 5; 6; 7]

let rec bare_array_type (t, i) =
  match i with 0 -> t | j -> UnsizedType.UArray (bare_array_type (t, j - 1))

let reduce_sum_slice_types =
  let base_slice_type i =
    [ bare_array_type (UnsizedType.UReal, i)
    ; bare_array_type (UnsizedType.UInt, i)
    ; bare_array_type (UnsizedType.UMatrix, i)
    ; bare_array_type (UnsizedType.UVector, i)
    ; bare_array_type (UnsizedType.URowVector, i) ] in
  List.concat (List.map ~f:base_slice_type reduce_sum_allowed_dimensionalities)

(* Variadic ODE *)
let variadic_ode_adjoint_ctl_tol_arg_types =
  [ (UnsizedType.DataOnly, UnsizedType.UReal)
    (* real relative_tolerance_forward *)
  ; (DataOnly, UVector) (* vector absolute_tolerance_forward *)
  ; (DataOnly, UReal) (* real relative_tolerance_backward *)
  ; (DataOnly, UVector) (* real absolute_tolerance_backward *)
  ; (DataOnly, UReal) (* real relative_tolerance_quadrature *)
  ; (DataOnly, UReal) (* real absolute_tolerance_quadrature *)
  ; (DataOnly, UInt) (* int max_num_steps *)
  ; (DataOnly, UInt) (* int num_steps_between_checkpoints *)
  ; (DataOnly, UInt) (* int interpolation_polynomial *)
  ; (DataOnly, UInt) (* int solver_forward *); (DataOnly, UInt)
    (* int solver_backward *) ]

let variadic_ode_tol_arg_types =
  [ (UnsizedType.DataOnly, UnsizedType.UReal); (DataOnly, UReal)
  ; (DataOnly, UInt) ]

let variadic_ode_mandatory_arg_types =
  [ (UnsizedType.AutoDiffable, UnsizedType.UVector); (AutoDiffable, UReal)
  ; (AutoDiffable, UArray UReal) ]

let variadic_ode_mandatory_fun_args =
  [ (UnsizedType.AutoDiffable, UnsizedType.UReal)
  ; (UnsizedType.AutoDiffable, UnsizedType.UVector) ]

let variadic_ode_fun_return_type = UnsizedType.UVector
let variadic_ode_return_type = UnsizedType.UArray UnsizedType.UVector

let variadic_dae_tol_arg_types =
  [ (UnsizedType.DataOnly, UnsizedType.UReal); (DataOnly, UReal)
  ; (DataOnly, UInt) ]

let variadic_dae_mandatory_arg_types =
  [ (UnsizedType.AutoDiffable, UnsizedType.UVector); (* yy *)
    (UnsizedType.AutoDiffable, UnsizedType.UVector); (* yp *)
    (AutoDiffable, UReal); (AutoDiffable, UArray UReal) ]

let variadic_dae_mandatory_fun_args =
  [ (UnsizedType.AutoDiffable, UnsizedType.UReal)
  ; (UnsizedType.AutoDiffable, UnsizedType.UVector)
  ; (UnsizedType.AutoDiffable, UnsizedType.UVector) ]

let variadic_dae_fun_return_type = UnsizedType.UVector
let variadic_dae_return_type = UnsizedType.UArray UnsizedType.UVector

let variadic_function_returntype name =
  if is_reduce_sum_fn name then Some (UnsizedType.ReturnType UReal)
  else if is_variadic_ode_fn name then
    Some (UnsizedType.ReturnType variadic_ode_return_type)
  else if is_variadic_dae_fn name then
    Some (UnsizedType.ReturnType variadic_dae_return_type)
  else None

let make_function_variable current_block loc id = function
  | UnsizedType.UFun (args, rt, FnLpdf _, mem_pattern) ->
      let type_ =
        UnsizedType.UFun
          (args, rt, Fun_kind.suffix_from_name id.name, mem_pattern) in
      mk_typed_expression ~expr:(Variable id)
        ~ad_level:(calculate_autodifftype current_block Functions type_)
        ~type_ ~loc
  | UnsizedType.UFun _ as type_ ->
      mk_typed_expression ~expr:(Variable id)
        ~ad_level:(calculate_autodifftype current_block Functions type_)
        ~type_ ~loc
  | type_ ->
      Common.FatalError.fatal_error_msg
        [%message
          "Attempting to create function variable out of "
            (type_ : UnsizedType.t)]

let check_reduce_sum ~is_cond_dist loc current_block tenv id tes =
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
    Semantic_error.illtyped_variadic_fn loc id.name
      (List.map ~f:type_of_expr_typed tes)
      expected_args err UReal
    |> error in
  let matching remaining_es fn =
    match fn with
    | Environment.
        { type_=
            UnsizedType.UFun
              (((_, sliced_arg_fun_type) as sliced_arg_fun) :: _, _, _, _) as
            ftype
        ; _ }
      when List.mem reduce_sum_slice_types sliced_arg_fun_type ~equal:( = ) ->
        let mandatory_args = [sliced_arg_fun; (AutoDiffable, UInt)] in
        let mandatory_fun_args =
          [sliced_arg_fun; (DataOnly, UInt); (DataOnly, UInt)] in
        let arg_types =
          (calculate_autodifftype current_block Functions ftype, ftype)
          :: get_arg_types remaining_es in
        SignatureMismatch.check_variadic_args true mandatory_args
          mandatory_fun_args UReal arg_types
    | _ -> basic_mismatch () in
  match tes with
  | {expr= Variable fname; _} :: remaining_es -> (
    match
      SignatureMismatch.find_matching_first_order_fn tenv
        (matching remaining_es) fname
    with
    | SignatureMismatch.UniqueMatch (ftype, promotions) ->
        (* a valid signature exists *)
        let tes =
          make_function_variable current_block loc fname ftype :: remaining_es
        in
        mk_typed_expression
          ~expr:
            (mk_fun_app ~is_cond_dist
               (StanLib FnPlain, id, Promotion.promote_list tes promotions) )
          ~ad_level:(expr_ad_lub tes) ~type_:UnsizedType.UReal ~loc
    | AmbiguousMatch ps ->
        Semantic_error.ambiguous_function_promotion loc fname.name None ps
        |> error
    | SignatureErrors (expected_args, err) ->
        Semantic_error.illtyped_variadic_fn loc id.name
          (List.map ~f:type_of_expr_typed tes)
          expected_args err UReal
        |> error )
  | _ -> fail ()

let check_variadic_ode ~is_cond_dist loc current_block tenv id tes =
  let optional_tol_mandatory_args =
    if variadic_ode_adjoint_fn = id.name then
      variadic_ode_adjoint_ctl_tol_arg_types
    else if is_variadic_ode_nonadjoint_tol_fn id.name then
      variadic_ode_tol_arg_types
    else [] in
  let mandatory_arg_types =
    variadic_ode_mandatory_arg_types @ optional_tol_mandatory_args in
  let fail () =
    let expected_args, err =
      SignatureMismatch.check_variadic_args false mandatory_arg_types
        variadic_ode_mandatory_fun_args variadic_ode_fun_return_type
        (get_arg_types tes)
      |> Result.error |> Option.value_exn in
    Semantic_error.illtyped_variadic_fn loc id.name
      (List.map ~f:type_of_expr_typed tes)
      expected_args err variadic_ode_fun_return_type
    |> error in
  let matching remaining_es Environment.{type_= ftype; _} =
    let arg_types =
      (calculate_autodifftype current_block Functions ftype, ftype)
      :: get_arg_types remaining_es in
    SignatureMismatch.check_variadic_args false mandatory_arg_types
      variadic_ode_mandatory_fun_args variadic_ode_fun_return_type arg_types
  in
  match tes with
  | {expr= Variable fname; _} :: remaining_es -> (
    match
      SignatureMismatch.find_matching_first_order_fn tenv
        (matching remaining_es) fname
    with
    | SignatureMismatch.UniqueMatch (ftype, promotions) ->
        let tes =
          make_function_variable current_block loc fname ftype :: remaining_es
        in
        mk_typed_expression
          ~expr:
            (mk_fun_app ~is_cond_dist
               (StanLib FnPlain, id, Promotion.promote_list tes promotions) )
          ~ad_level:(expr_ad_lub tes) ~type_:variadic_ode_return_type ~loc
    | AmbiguousMatch ps ->
        Semantic_error.ambiguous_function_promotion loc fname.name None ps
        |> error
    | SignatureErrors (expected_args, err) ->
        Semantic_error.illtyped_variadic_fn loc id.name
          (List.map ~f:type_of_expr_typed tes)
          expected_args err variadic_ode_fun_return_type
        |> error )
  | _ -> fail ()

let check_variadic_dae ~is_cond_dist loc current_block tenv id tes =
  let optional_tol_mandatory_args =
    if is_variadic_dae_tol_fn id.name then variadic_dae_tol_arg_types else []
  in
  let mandatory_arg_types =
    variadic_dae_mandatory_arg_types @ optional_tol_mandatory_args in
  let fail () =
    let expected_args, err =
      SignatureMismatch.check_variadic_args false mandatory_arg_types
        variadic_dae_mandatory_fun_args variadic_dae_fun_return_type
        (get_arg_types tes)
      |> Result.error |> Option.value_exn in
    Semantic_error.illtyped_variadic_fn loc id.name
      (List.map ~f:type_of_expr_typed tes)
      expected_args err variadic_dae_fun_return_type
    |> error in
  let matching remaining_es Environment.{type_= ftype; _} =
    let arg_types =
      (calculate_autodifftype current_block Functions ftype, ftype)
      :: get_arg_types remaining_es in
    SignatureMismatch.check_variadic_args false mandatory_arg_types
      variadic_dae_mandatory_fun_args variadic_dae_fun_return_type arg_types
  in
  match tes with
  | {expr= Variable fname; _} :: remaining_es -> (
    match
      SignatureMismatch.find_matching_first_order_fn tenv
        (matching remaining_es) fname
    with
    | SignatureMismatch.UniqueMatch (ftype, promotions) ->
        let tes =
          make_function_variable current_block loc fname ftype :: remaining_es
        in
        mk_typed_expression
          ~expr:
            (mk_fun_app ~is_cond_dist
               (StanLib FnPlain, id, Promotion.promote_list tes promotions) )
          ~ad_level:(expr_ad_lub tes) ~type_:variadic_dae_return_type ~loc
    | AmbiguousMatch ps ->
        Semantic_error.ambiguous_function_promotion loc fname.name None ps
        |> error
    | SignatureErrors (expected_args, err) ->
        Semantic_error.illtyped_variadic_fn loc id.name
          (List.map ~f:type_of_expr_typed tes)
          expected_args err variadic_dae_fun_return_type
        |> error )
  | _ -> fail ()
