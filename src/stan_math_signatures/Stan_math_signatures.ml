(** The signatures of the Stan Math library, which are used for type checking *)
open Core

open Core.Poly
open Middle

(** The [Generated_signatures] module is produced by the [Generate.ml]
    executable in this folder.

    {b This is an optimization}, one can proceed as-if the hashtables here were
    built at runtime by the code in that module (i.e., by [include Generate]) *)

let stan_math_signatures = Generated_signatures.stan_math_signatures

let stan_math_variadic_signatures =
  Generated_signatures.stan_math_variadic_signatures

let distributions = Generated_signatures.distributions

let is_stan_math_function_name name =
  let name = Utils.stdlib_distribution_name name in
  Hashtbl.mem stan_math_signatures name

let lookup_stan_math_function = Hashtbl.find_multi stan_math_signatures
let get_stan_math_signatures_alist () = Hashtbl.to_alist stan_math_signatures

let is_stan_math_variadic_function_name =
  Hashtbl.mem stan_math_variadic_signatures

let lookup_stan_math_variadic_function =
  Hashtbl.find stan_math_variadic_signatures

let operator_to_stan_math_fns op =
  match op with
  | Operator.Plus -> ["add"]
  | PPlus -> ["plus"]
  | Minus -> ["subtract"]
  | PMinus -> ["minus"]
  | Times -> ["multiply"]
  | Divide -> ["mdivide_right"; "divide"]
  | Modulo -> ["modulus"]
  | IntDivide -> []
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

let int_divide_type =
  UnsizedType.
    ( [(AutoDiffable, UInt); (AutoDiffable, UInt)]
    , ReturnType UInt
    , Fun_kind.FnPlain
    , Mem_pattern.AoS )

let get_sigs name =
  let name = Utils.stdlib_distribution_name name in
  Hashtbl.find_multi stan_math_signatures name
  |> List.sort ~compare:UnsizedType.compare_signature

let make_assignmentoperator_stan_math_signatures assop =
  (match assop with
    | Operator.Divide -> ["divide"]
    | assop -> operator_to_stan_math_fns assop)
  |> List.concat_map ~f:get_sigs
  |> List.concat_map ~f:(function
    | [(ad1, lhs); (ad2, rhs)], ReturnType rtype, _, _
      when rtype = lhs
           && not
                ((assop = Operator.EltTimes || assop = Operator.EltDivide)
                && UnsizedType.is_scalar_type rtype) ->
        if rhs = UReal then
          [ ( [(ad1, lhs); (ad2, UInt)]
            , UnsizedType.Void
            , Fun_kind.FnPlain
            , Mem_pattern.SoA )
          ; ([(ad1, lhs); (ad2, UReal)], Void, FnPlain, SoA) ]
        else [([(ad1, lhs); (ad2, rhs)], Void, FnPlain, SoA)]
    | _ -> [])

let pp_math_sigs ppf name =
  (Fmt.list ~sep:Fmt.cut UnsizedType.pp_math_sig) ppf (get_sigs name)

let pretty_print_math_sigs = Fmt.str "@[<v>@,%a@]" pp_math_sigs

let string_operator_to_stan_math_fns str =
  match str with
  | "Plus__" -> "add"
  | "PPlus__" -> "plus"
  | "Minus__" -> "subtract"
  | "PMinus__" -> "minus"
  | "Times__" -> "multiply"
  | "Divide__" -> "divide"
  | "Modulo__" -> "modulus"
  | "IntDivide__" -> "divide"
  | "LDivide__" -> "mdivide_left"
  | "EltTimes__" -> "elt_multiply"
  | "EltDivide__" -> "elt_divide"
  | "Pow__" -> "pow"
  | "EltPow__" -> "pow"
  | "Or__" -> "logical_or"
  | "And__" -> "logical_and"
  | "Equals__" -> "logical_eq"
  | "NEquals__" -> "logical_neq"
  | "Less__" -> "logical_lt"
  | "Leq__" -> "logical_lte"
  | "Greater__" -> "logical_gt"
  | "Geq__" -> "logical_gte"
  | "PNot__" -> "logical_negation"
  | "Transpose__" -> "transpose"
  | _ -> str

let pretty_print_all_math_sigs ppf () =
  let open Fmt in
  Format.pp_set_margin ppf 180;
  let pp_sig ppf (name, (args, rt, _, _)) =
    pf ppf "%s(@[<h>%a@]) => %a" name
      (list ~sep:comma UnsizedType.pp)
      (List.map ~f:snd args) UnsizedType.pp_returntype rt in
  let pp_sigs_for_name ppf name =
    (list ~sep:cut pp_sig) ppf
      (List.map ~f:(fun t -> (name, t)) (get_sigs name)) in
  pf ppf "@[<v>%a@]"
    (list ~sep:cut pp_sigs_for_name)
    (List.sort ~compare:String.compare (Hashtbl.keys stan_math_signatures))

let pretty_print_all_math_distributions ppf () =
  let open Fmt in
  let pp_dist ppf (name, kinds) =
    pf ppf "@[%s: %a@]" name (list ~sep:comma Fmt.string) kinds in
  pf ppf "@[<v>%a@]" (list ~sep:cut pp_dist) distributions

let pretty_print_math_lib_operator_sigs op =
  if op = Operator.IntDivide then
    [Fmt.str "@[<v>@,%a@]" UnsizedType.pp_math_sig int_divide_type]
  else operator_to_stan_math_fns op |> List.map ~f:pretty_print_math_sigs

(* variadics *)

let reduce_sum_slice_types =
  UnsizedType.[UReal; UInt; UMatrix; UVector; URowVector]

let is_reduce_sum_fn f =
  String.equal f "reduce_sum" || String.equal f "reduce_sum_static"

let embedded_laplace_functions =
  [ (* general fns *) "laplace_marginal"; "laplace_marginal_tol"
  ; "laplace_latent_rng"; "laplace_latent_tol_rng"; (* "helpers" *)
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
  Set.mem embedded_laplace_functions (Utils.stdlib_distribution_name name)

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
  |> String.Map.of_alist_exn

let laplace_helper_param_types name =
  let without_prefix =
    String.chop_prefix_exn name ~prefix:"laplace_"
    |> String.chop_prefix_if_exists ~prefix:"marginal_"
    |> String.chop_prefix_if_exists ~prefix:"latent_"
    |> String.chop_prefix_if_exists ~prefix:"tol_" in
  let variant =
    without_prefix |> Utils.split_distribution_suffix
    |> Option.value_map ~f:fst ~default:without_prefix in
  Map.find laplace_helper_lik_args variant |> Option.value ~default:[]

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
  Set.mem disallowed_second_order name || is_special_function_name name
