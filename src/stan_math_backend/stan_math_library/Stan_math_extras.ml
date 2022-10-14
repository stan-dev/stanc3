(** This module serves as the backend-specific portion
      of the typechecker. *)

open Core_kernel
open Core_kernel.Poly
open Ast
open Typechecker_utils
open Middle
open Std_library_utils
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

let check_reduce_sum ~is_cond_dist loc current_block tenv id tes =
  let basic_mismatch () =
    let mandatory_args =
      UnsizedType.[(AutoDiffable, UArray UReal); (AutoDiffable, UInt)] in
    let mandatory_fun_args =
      UnsizedType.
        [(AutoDiffable, UArray UReal); (DataOnly, UInt); (DataOnly, UInt)] in
    SignatureMismatch.check_variadic_args ~allow_lpdf:true mandatory_args
      mandatory_fun_args UReal (get_arg_types tes) in
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
        SignatureMismatch.check_variadic_args ~allow_lpdf:true mandatory_args
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

(** The variadic signatures hash table
    These functions cannot be overloaded.
*)
let (variadic_signatures : (string, variadic_signature) Hashtbl.t) =
  let t = String.Table.create () in
  let add_variadic_fn name ~return_type ?control_args ~required_fn_rt
      ?required_fn_args () =
    Hashtbl.add_exn t ~key:name
      ~data:
        (create_variadic_signature ~return_type ?control_args ?required_fn_args
           ~required_fn_rt () ) in
  let () =
    (* DAEs *)
    add_variadic_fn "dae" ~return_type:variadic_dae_return_type
      ~control_args:variadic_dae_mandatory_arg_types
      ~required_fn_args:variadic_dae_mandatory_fun_args
      ~required_fn_rt:variadic_dae_fun_return_type () ;
    add_variadic_fn "dae_tol" ~return_type:variadic_dae_return_type
      ~control_args:
        (variadic_dae_mandatory_arg_types @ variadic_dae_tol_arg_types)
      ~required_fn_args:variadic_dae_mandatory_fun_args
      ~required_fn_rt:variadic_dae_fun_return_type () ;
    (* non-adjoint ODES - same for all *)
    let add_ode name =
      add_variadic_fn name ~return_type:variadic_ode_return_type
        ~control_args:
          ( if String.is_suffix name ~suffix:ode_tolerances_suffix then
            variadic_ode_mandatory_arg_types @ variadic_ode_tol_arg_types
          else variadic_ode_mandatory_arg_types )
        ~required_fn_rt:variadic_ode_fun_return_type
        ~required_fn_args:variadic_ode_mandatory_fun_args () in
    Set.iter ~f:add_ode variadic_ode_nonadjoint_fns ;
    (* Adjoint ODE function *)
    add_variadic_fn variadic_ode_adjoint_fn
      ~return_type:variadic_ode_return_type
      ~control_args:
        ( variadic_ode_mandatory_arg_types
        @ variadic_ode_adjoint_ctl_tol_arg_types )
      ~required_fn_rt:variadic_ode_fun_return_type
      ~required_fn_args:variadic_ode_mandatory_fun_args () in
  t

let is_variadic_function_name name = Hashtbl.mem variadic_signatures name

let is_not_overloadable name =
  is_variadic_function_name name || is_special_function_name name

let variadic_dae_fun_return_type = UnsizedType.UVector
let variadic_dae_return_type = UnsizedType.UArray UnsizedType.UVector
