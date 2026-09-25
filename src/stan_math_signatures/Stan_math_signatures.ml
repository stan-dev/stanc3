(** The signatures of the Stan Math library, which are used for type checking *)

open Std
open Middle

(** The [Generated_signatures] module is produced by the [Generate.ml]
    executable in this folder.

    {b This is an optimization}, one can proceed as-if the hashtables here were
    built at runtime by the code in that module (i.e., by [include Generate]) *)

include Generated_signatures

let is_stan_math_function_name name =
  let name = Utils.stdlib_distribution_name name in
  Hashtbl.mem (Lazy.force stan_math_signatures) name

let lookup_stan_math_function name =
  Hashtbl.find_multi (Lazy.force stan_math_signatures) name

let signatures_alist =
  Lazy.map stan_math_signatures ~f:(Fun.compose List.of_seq Hashtbl.to_seq)

let is_stan_math_variadic_function_name =
  Hashtbl.mem stan_math_variadic_signatures

let lookup_stan_math_variadic_function =
  Hashtbl.find_opt stan_math_variadic_signatures

let operator_to_stan_math_fns op =
  match op with
  | Operator.Plus -> ["add"]
  | PPlus -> ["plus"]
  | Minus -> ["subtract"]
  | PMinus -> ["minus"]
  | Times -> ["multiply"]
  | Divide -> ["divide"; "mdivide_right"]
  | Modulo -> ["modulus"]
  | IntDivide -> ["divide"]
  | LDivide -> ["mdivide_left"]
  | EltTimes -> ["elt_multiply"]
  | EltDivide -> ["elt_divide"]
  | Pow -> ["pow"]
  | EltPow -> ["pow"]
  | Or -> ["logical_or"]
  | And -> ["logical_and"]
  | Equals -> ["logical_eq"]
  | NEquals -> ["logical_neq"]
  | Less -> ["logical_lt"]
  | Leq -> ["logical_lte"]
  | Greater -> ["logical_gt"]
  | Geq -> ["logical_gte"]
  | PNot -> ["logical_negation"]
  | Transpose -> ["transpose"]

let get_sigs name =
  let name = Utils.stdlib_distribution_name name in
  Hashtbl.find_multi (Lazy.force stan_math_signatures) name
  |> List.sort ~cmp:UnsizedType.compare_signature

let operator_to_stan_math_signatures op =
  let int_only_operator_type =
    UnsizedType.
      ( [(AutoDiffable, UInt); (AutoDiffable, UInt)]
      , ReturnType UInt
      , Fun_kind.FnPlain
      , Mem_pattern.AoS ) in
  match op with
  | Operator.IntDivide | And | Or -> [int_only_operator_type]
  | _ -> operator_to_stan_math_fns op |> List.concat_map ~f:get_sigs

let pretty_print_all_math_distributions ppf () =
  let open Fmt in
  let pp_dist ppf (name, kinds) =
    pf ppf "@[%s: %a@]" name (list ~sep:comma Fmt.string) kinds in
  pf ppf "@[<v>%a@]" (list ~sep:cut pp_dist) distributions

(* variadics *)

let reduce_sum_slice_types =
  UnsizedType.[UReal; UInt; UMatrix; UVector; URowVector]

let reduce_sum_fns = String.Set.of_list ["reduce_sum"; "reduce_sum_static"]
let is_reduce_sum_fn name = String.Set.mem name reduce_sum_fns

let reduce_sum_signature slice : UnsizedType.variadic_signature =
  { return_type= UReal
  ; control_args= [slice; (AutoDiffable, UInt)]
  ; required_fn_rt= UReal
  ; required_fn_args= [slice; (DataOnly, UInt); (DataOnly, UInt)] }

let embedded_laplace_functions =
  [ (* general fns *) "laplace_marginal"; "laplace_marginal_tol"
  ; "laplace_latent_rng"; "laplace_latent_tol_rng"; "laplace_latent_solve"
  ; "laplace_latent_solve_tol"; (* "helpers" *)
    "laplace_marginal_bernoulli_logit_lpmf"
  ; "laplace_marginal_tol_bernoulli_logit_lpmf"
  ; "laplace_marginal_neg_binomial_2_log_lpmf"
  ; "laplace_marginal_tol_neg_binomial_2_log_lpmf"
  ; "laplace_marginal_poisson_log_lpmf"; "laplace_marginal_tol_poisson_log_lpmf"
  ; (* rngs *) "laplace_latent_bernoulli_logit_rng"
  ; "laplace_latent_tol_bernoulli_logit_rng"
  ; "laplace_latent_neg_binomial_2_log_rng"
  ; "laplace_latent_tol_neg_binomial_2_log_rng"
  ; "laplace_latent_poisson_log_rng"; "laplace_latent_tol_poisson_log_rng" ]
  |> String.Set.of_list

let is_embedded_laplace_fn name =
  String.Set.mem
    (Utils.stdlib_distribution_name name)
    embedded_laplace_functions

let laplace_helper_lik_args =
  [ ( "bernoulli_logit"
    , [ UnsizedType.(AutoDiffable, UArray UInt); (AutoDiffable, UArray UInt)
      ; (AutoDiffable, UVector) ] )
  ; ( "neg_binomial_2_log"
    , [ (AutoDiffable, UArray UInt); (AutoDiffable, UArray UInt)
      ; (AutoDiffable, UVector); (AutoDiffable, UVector) ] )
  ; ( "poisson_log"
    , [ (AutoDiffable, UArray UInt); (AutoDiffable, UArray UInt)
      ; (AutoDiffable, UVector) ] ) ]
  |> String.Map.of_list

let laplace_helper_param_types name =
  let without_prefix =
    String.chop_prefix_exn name ~prefix:"laplace_"
    |> String.chop_prefix_if_exists ~prefix:"marginal_"
    |> String.chop_prefix_if_exists ~prefix:"latent_"
    |> String.chop_prefix_if_exists ~prefix:"tol_" in
  let variant =
    without_prefix |> Utils.split_distribution_suffix
    |> Option.value_map ~f:fst ~default:without_prefix in
  String.Map.find_opt variant laplace_helper_lik_args
  |> Option.value ~default:[]

let laplace_tolerance_argument_types =
  UnsizedType.
    [ (AutoDiffable, UVector) (* theta_0 *); (DataOnly, UReal) (* tolerance *)
    ; (DataOnly, UInt) (* max_num_steps *); (DataOnly, UInt) (* solver *)
    ; (DataOnly, UInt) (* max_steps_line_search *)
    ; (DataOnly, UInt) (* allow_fallthrough *) ]

let is_special_function_name name =
  is_stan_math_variadic_function_name name
  || is_reduce_sum_fn name
  || is_embedded_laplace_fn name

let disallowed_second_order =
  [ "algebra_solver"; "algebra_solver_newton"; "integrate_1d"; "integrate_ode"
  ; "integrate_ode_adams"; "integrate_ode_bdf"; "integrate_ode_rk45"; "map_rect"
  ; "hmm_marginal"; "hmm_hidden_state_prob" ]
  |> String.Set.of_list

let lacks_higher_order_autodiff name =
  String.Set.mem name disallowed_second_order || is_special_function_name name

let pretty_print_all_math_sigs ppf () =
  let open Fmt in
  Format.pp_set_margin ppf 180;
  let pp_args = list ~sep:comma UnsizedType.pp in
  let pp_sig name ppf (args, rt, _, _) =
    pf ppf "%s(@[<h>%a@]) => %a" name pp_args (List.map ~f:snd args)
      UnsizedType.pp_returntype rt in
  let pp_variadic_sig name ppf
      UnsizedType.{return_type; control_args; required_fn_rt; required_fn_args}
      =
    pf ppf "%s(@[<h>function(%a, ...) => %a, %a, ...@]) => %a" name
      (list ~sep:comma UnsizedType.pp_fun_arg)
      required_fn_args UnsizedType.pp required_fn_rt pp_args
      (List.map ~f:snd control_args)
      UnsizedType.pp return_type in
  let pp_laplace_sig name ppf =
    let pp_likelihood ppf =
      match laplace_helper_param_types name with
      | [] -> string ppf "function(vector, ...) => real, tuple(...)"
      | args -> pp_args ppf (List.map ~f:snd args) in
    let pp_tolerances ppf =
      if String.includes ~affix:"_tol" name then
        pf ppf ", %a" UnsizedType.pp
          (UTuple (List.map ~f:snd laplace_tolerance_argument_types)) in
    let return_type =
      if String.ends_with ~suffix:"_rng" name then "vector"
      else if String.includes ~affix:"_solve" name then "tuple(vector, matrix)"
      else "real" in
    pf ppf "%s(@[<h>%t, int, function(...) => matrix, tuple(...)%t@]) => %s"
      name pp_likelihood pp_tolerances return_type in
  let pp_sigs_for_name ppf name =
    match lookup_stan_math_variadic_function name with
    | Some sig_ -> pp_variadic_sig name ppf sig_
    | None when is_reduce_sum_fn name ->
        let sigs =
          List.concat_map (List.range 1 8) ~f:(fun depth ->
              List.map reduce_sum_slice_types ~f:(fun t ->
                  reduce_sum_signature
                    (AutoDiffable, UnsizedType.wind_array_type (t, depth))))
        in
        (list ~sep:cut (pp_variadic_sig name)) ppf sigs
    | None when is_embedded_laplace_fn name -> pp_laplace_sig name ppf
    | None -> (list ~sep:cut (pp_sig name)) ppf (get_sigs name) in
  let names =
    List.of_seq (Hashtbl.to_seq_keys (Lazy.force stan_math_signatures))
    @ List.of_seq (Hashtbl.to_seq_keys stan_math_variadic_signatures)
    @ String.Set.to_list reduce_sum_fns
    @ String.Set.to_list embedded_laplace_functions
    |> List.sort ~cmp:String.compare in
  pf ppf "@[<v>%a@]" (list ~sep:cut pp_sigs_for_name) names
