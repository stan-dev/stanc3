open Core_kernel
open Middle
open Fmt

let ends_with suffix s = String.is_suffix ~suffix s
let starts_with prefix s = String.is_prefix ~prefix s

let functions_requiring_namespace =
  String.Set.of_list
    [ "e"; "pi"; "log2"; "log10"; "sqrt2"; "not_a_number"; "positive_infinity"
    ; "negative_infinity"; "machine_precision"; "abs"; "acos"; "acosh"; "asin"
    ; "asinh"; "atan"; "atan2"; "atanh"; "cbrt"; "ceil"; "cos"; "cosh"; "erf"
    ; "erfc"; "exp"; "exp2"; "expm1"; "fabs"; "floor"; "lgamma"; "log"; "log1p"
    ; "log2"; "log10"; "round"; "sin"; "sinh"; "sqrt"; "tan"; "tanh"; "tgamma"
    ; "trunc"; "fdim"; "fmax"; "fmin"; "hypot"; "fma" ]

let stan_namespace_qualify f =
  if Set.mem functions_requiring_namespace f then "stan::math::" ^ f else f

(* return true if the types of the two expression are the same *)
let types_match e1 e2 = Expr.Typed.type_of e1 = Expr.Typed.type_of e2
let is_stan_math f = ends_with "__" f || starts_with "stan::math::" f

(* retun true if the tpe of the expression is integer or real *)
let is_scalar e =
  match Expr.Typed.type_of e with UInt | UReal -> true | _ -> false

let is_matrix e =
  match Expr.Typed.type_of e with UMatrix -> true | _ -> false

let is_row_vector e =
  match Expr.Typed.type_of e with URowVector -> true | _ -> false

(* stub *)
let pretty_print _e = "pretty printed e"

let rec stantype_prim_str = function
  | UnsizedType.UInt -> "int"
  | UArray t -> stantype_prim_str t
  | _ -> "double"

let rec local_scalar ut ad =
  match (ut, ad) with
  | UnsizedType.UArray t, _ -> local_scalar t ad
  | _, UnsizedType.DataOnly | UInt, AutoDiffable -> stantype_prim_str ut
  | _, AutoDiffable -> "local_scalar_t__"

(* stub *)
let rec pp_return_type ppf = function
  | UnsizedType.Void -> pf ppf "void"
  | ReturnType rt -> pp_unsizedtype ppf rt

and pp_unsizedtype_custom_scalar ppf (scalar, ut) =
  match ut with
  | UnsizedType.UInt | UReal -> string ppf scalar
  | UArray t ->
      pf ppf "std::vector<%a>" pp_unsizedtype_custom_scalar (scalar, t)
  | UMatrix -> pf ppf "Eigen::Matrix<%s, -1, -1>" scalar
  | URowVector -> pf ppf "Eigen::Matrix<%s, 1, -1>" scalar
  | UVector -> pf ppf "Eigen::Matrix<%s, -1, 1>" scalar
  | x -> raise_s [%message (x : UnsizedType.t) "not implemented yet"]

and pp_unsizedtype_local ppf (adtype, ut) =
  let s = local_scalar ut adtype in
  pp_unsizedtype_custom_scalar ppf (s, ut)

and pp_unsizedtype ppf ut =
  match ut with
  | UnsizedType.UInt -> pf ppf "int"
  | UReal -> pf ppf "local_scalar_t__"
  | UVector -> pf ppf "Eigen::Matrix<local_scalar_t, -1, 1>"
  | URowVector -> pf ppf "Eigen::Matrix<local_scalar_t, 1, -1>"
  | UMatrix -> pf ppf "Eigen::Matrix<local_scalar_t, -1, 1>"
  | UArray t -> pf ppf "std::vector<%a>" pp_unsizedtype t
  | UFun (args_t, return_t) ->
      let arg_types = List.map ~f:snd args_t in
      pf ppf "std::function<%a(%a)>" pp_return_type return_t
        (list ~sep:(const string ", ") pp_unsizedtype)
        arg_types
  | UMathLibraryFunction -> pf ppf "std::function<void()>"

let pp_expr_type ppf e =
  pp_unsizedtype_local ppf Expr.Typed.(adlevel_of e, type_of e)

let suffix_args f =
  if ends_with "_rng" f then ["base_rng__"]
  else if ends_with "_lp" f then ["lp__"; "lp_accum__"]
  else []

let gen_extra_fun_args f = suffix_args f @ ["pstream__"]

let rec pp_index ppf = function
  | Expr.All -> pf ppf "index_omni()"
  | Single e -> pf ppf "index_uni(%a)" pp_expr e
  | Upfrom e -> pf ppf "index_min(%a)" pp_expr e
  | Between (e_low, e_high) ->
      pf ppf "index_min_max(%a, %a)" pp_expr e_low pp_expr e_high
  | MultiIndex e -> pf ppf "index_multi(%a)" pp_expr e

and pp_indexes ppf = function
  | [] -> pf ppf "nil_index_list()"
  | idx :: idxs -> pf ppf "cons_list(%a, %a)" pp_index idx pp_indexes idxs

and pp_logical_op ppf op lhs rhs =
  pf ppf "(primitive_value(%a) %s primitive_value(%a))" pp_expr lhs op pp_expr
    rhs

and pp_unary ppf fm es = pf ppf fm pp_expr (List.hd_exn es)
and pp_binary ppf fm es = pf ppf fm pp_expr (first es) pp_expr (second es)

and pp_binary_f ppf f es =
  pf ppf "%s(%a, %a)" f pp_expr (first es) pp_expr (second es)

and first es = List.nth_exn es 0
and second es = List.nth_exn es 1

and pp_scalar_binary ppf scalar_fmt generic_fmt es =
  pp_binary ppf
    ( if is_scalar (first es) && is_scalar (second es) then scalar_fmt
    else generic_fmt )
    es

and gen_operator_app = function
  | Operator.Plus ->
      fun ppf es -> pp_scalar_binary ppf "(%a + %a)" "add(%a, %a)" es
  | PMinus ->
      fun ppf es ->
        pp_unary ppf
          (if is_scalar (List.hd_exn es) then "-%a" else "minus(%a)")
          es
  | PPlus -> fun ppf es -> pp_unary ppf "%a" es
  | Transpose ->
      fun ppf es ->
        pp_unary ppf
          (if is_scalar (List.hd_exn es) then "%a" else "transpose(%a)")
          es
  | PNot -> fun ppf es -> pp_unary ppf "logical_negation(%a)" es
  | Minus ->
      fun ppf es -> pp_scalar_binary ppf "(%a - %a)" "subtract(%a, %a)" es
  | Times ->
      fun ppf es -> pp_scalar_binary ppf "(%a * %a)" "multiply(%a, %a)" es
  | Divide ->
      fun ppf es ->
        if
          is_matrix (second es)
          && (is_matrix (first es) || is_row_vector (first es))
        then pp_binary_f ppf "mdivide_right" es
        else pp_scalar_binary ppf "(%a / %a)" "divide(%a, %a)" es
  | Modulo -> fun ppf es -> pp_binary_f ppf "modulus" es
  | LDivide -> fun ppf es -> pp_binary_f ppf "mdivide_left" es
  | And | Or ->
      raise_s [%message "And/Or should have been converted to an expression"]
  | EltTimes ->
      fun ppf es -> pp_scalar_binary ppf "(%a * %a)" "elt_multiply(%a, %a)" es
  | EltDivide ->
      fun ppf es -> pp_scalar_binary ppf "(%a / %a)" "elt_divide(%a, %a)" es
  | Pow -> fun ppf es -> pp_binary_f ppf "pow" es
  | Equals -> fun ppf es -> pp_binary_f ppf "logical_eq" es
  | NEquals -> fun ppf es -> pp_binary_f ppf "logical_neq" es
  | Less -> fun ppf es -> pp_binary_f ppf "logical_lt" es
  | Leq -> fun ppf es -> pp_binary_f ppf "logical_lte" es
  | Greater -> fun ppf es -> pp_binary_f ppf "logical_gt" es
  | Geq -> fun ppf es -> pp_binary_f ppf "logical_gte" es

and gen_misc_special_math_app f =
  match f with
  | "lmultiply" -> Some (fun ppf es -> pp_binary ppf "multiply_log(%a, %a)" es)
  | "lchoose" ->
      Some (fun ppf es -> pp_binary ppf "binomial_coefficient_log(%a, %a)" es)
  | "target" -> Some (fun ppf _ -> pf ppf "get_lp(lp__, lp_accum__)")
  | "get_lp" -> Some (fun ppf _ -> pf ppf "get_lp(lp__, lp_accum__)")
  | "max" | "min" ->
      Some
        (fun ppf es ->
          if List.length es = 2 then pp_binary_f ppf f es
          else pf ppf "%s(@[<hov>%a@])" f (list ~sep:comma pp_expr) es )
  | "ceil" ->
      Some
        (fun ppf es ->
          if is_scalar (first es) then pp_unary ppf "std::ceil(%a)" es
          else pf ppf "%s(@[<hov>%a@])" f (list ~sep:comma pp_expr) es )
  | f when f = Internal_fun.to_string FnLength ->
      Some (fun ppf -> gen_fun_app ppf "stan::length")
  | f when f = Internal_fun.to_string FnNegInf ->
      Some (fun ppf -> gen_fun_app ppf "stan::math::negative_infinity")
  | _ -> None

and read_data ut ppf es =
  let i_or_r =
    match ut with
    | UnsizedType.UInt -> "i"
    | UReal -> "r"
    | UVector | URowVector | UMatrix | UArray _
     |UFun (_, _)
     |UMathLibraryFunction ->
        raise_s [%message "Can't ReadData of " (ut : UnsizedType.t)]
  in
  pf ppf "context__.vals_%s(%a)" i_or_r pp_expr (List.hd_exn es)

and gen_distribution_app f =
  if Utils.is_propto_distribution f then
    Some
      (fun ppf ->
        pf ppf "%s<propto__>(@[<hov>%a@])"
          (Utils.stdlib_distribution_name f)
          (list ~sep:comma pp_expr) )
  else if Utils.is_distribution_name f then
    Some
      (fun ppf -> pf ppf "%s<false>(@[<hov>%a@])" f (list ~sep:comma pp_expr))
  else None

(* assumes everything well formed from parser checks *)
and gen_fun_app ppf f es =
  let default ppf es =
    let convert_hof_vars = function
      | Expr.({Fixed.pattern= Var name; meta= {Typed.Meta.type_= UFun _; _}})
        as e ->
          {e with pattern= FunApp (StanLib, name ^ "_functor__", [])}
      | e -> e
    in
    let converted_es = List.map ~f:convert_hof_vars es in
    let extra =
      (suffix_args f @ if es = converted_es then [] else ["pstream__"])
      |> List.map ~f:(fun s -> Expr.(var Typed.Meta.empty s))
    in
    pf ppf "%s(@[<hov>%a@])" (stan_namespace_qualify f)
      (list ~sep:comma pp_expr) (converted_es @ extra)
  in
  let pp =
    [ Option.map ~f:gen_operator_app (Operator.of_string_opt f)
    ; gen_misc_special_math_app f
    ; gen_distribution_app f ]
    |> List.filter_opt |> List.hd |> Option.value ~default
  in
  pp ppf es

and pp_constrain_funapp constrain_or_un_str ppf = function
  | var :: {Expr.Fixed.pattern= Lit (Str, constraint_flavor); _} :: args ->
      pf ppf "%s_%s(@[<hov>%a@])" constraint_flavor constrain_or_un_str
        (list ~sep:comma pp_expr) (var :: args)
  | es -> raise_s [%message "Bad constraint " (es : Expr.Typed.t list)]

and pp_ordinary_fn ppf f es =
  let extra_args = gen_extra_fun_args f in
  let sep = if List.is_empty extra_args then "" else ", " in
  pf ppf "%s(@[<hov>%a%s@])" f (list ~sep:comma pp_expr) es
    (sep ^ String.concat ~sep:", " extra_args)

and pp_compiler_internal_fn ut f ppf es =
  let array_literal ppf es =
    pf ppf "{@[<hov>%a@]}" (list ~sep:comma pp_expr) es
  in
  match Internal_fun.of_string_opt f with
  | Some FnMakeArray -> array_literal ppf es
  | Some FnMakeRowVec ->
      pf ppf "stan::math::to_row_vector(%a)" array_literal es
  | Some FnConstrain -> pp_constrain_funapp "constrain" ppf es
  | Some FnUnconstrain -> pp_constrain_funapp "free" ppf es
  | Some FnReadData -> read_data ut ppf es
  | Some FnReadParam -> (
    match es with
    | _ :: {Expr.Fixed.pattern= Lit (Str, base_type); _} :: dims ->
        pf ppf "in__.%s(@[<hov>%a@])" base_type (list ~sep:comma pp_expr) dims
    | _ -> raise_s [%message "emit ReadParam with " (es : Expr.Typed.t list)] )
  | _ -> gen_fun_app ppf f es

and pp_indexed ppf (vident, indices, pretty) =
  pf ppf "rvalue(%s, %a, %S)" vident pp_indexes indices pretty

and pp_indexed_simple ppf (vident, idcs) =
  let minus_one e =
    Expr.(binop (Fixed.meta e) Operator.Minus e (lit_int Typed.Meta.empty 1))
  in
  let idx_minus_one = Expr.map_index minus_one in
  (Expr.pp_indexed pp_expr) ppf (vident, List.map ~f:idx_minus_one idcs)

and pp_expr ppf e =
  match Expr.Fixed.pattern e with
  | Var s -> pf ppf "%s" s
  | Lit (Str, s) -> pf ppf "%S" s
  | Lit (_, s) -> pf ppf "%s" s
  | FunApp (StanLib, f, es) -> gen_fun_app ppf f es
  | FunApp (CompilerInternal, f, es) ->
      pp_compiler_internal_fn (Expr.Typed.type_of e) (stan_namespace_qualify f)
        ppf es
  | FunApp (UserDefined, f, es) -> pp_ordinary_fn ppf f es
  | EAnd (e1, e2) -> pp_logical_op ppf "&&" e1 e2
  | EOr (e1, e2) -> pp_logical_op ppf "||" e1 e2
  | TernaryIf (ec, et, ef) ->
      let promoted ppf (t, e) =
        pf ppf "stan::math::promote_scalar<%a>(%a)" pp_expr_type t pp_expr e
      in
      let tform ppf = pf ppf "(@[<hov>%a@ ?@ %a@ :@ %a@])" in
      if types_match et ef then tform ppf pp_expr ec pp_expr et pp_expr ef
      else tform ppf pp_expr ec promoted (e, et) promoted (e, ef)
  | Indexed (e, idx) -> (
    match Expr.Fixed.pattern e with
    | FunApp (CompilerInternal, f, _)
      when Internal_fun.of_string_opt f = Some FnReadParam ->
        pp_expr ppf e
    | FunApp (CompilerInternal, f, _)
      when Internal_fun.of_string_opt f = Some FnReadData ->
        pp_indexed_simple ppf (strf "%a" pp_expr e, idx)
    | _ -> pp_indexed ppf (strf "%a" pp_expr e, idx, pretty_print e) )

(* these functions are just for testing *)
let dummy_locate e = Expr.(Fixed.fix Typed.Meta.empty e)
let pp_unlocated e = strf "%a" pp_expr (dummy_locate e)

let%expect_test "pp_expr1" =
  printf "%s" (pp_unlocated (Var "a")) ;
  [%expect {| a |}]

let%expect_test "pp_expr2" =
  printf "%s" (pp_unlocated (Lit (Str, "b"))) ;
  [%expect {| "b" |}]

let%expect_test "pp_expr3" =
  printf "%s" (pp_unlocated (Lit (Int, "112"))) ;
  [%expect {| 112 |}]

let%expect_test "pp_expr4" =
  printf "%s" (pp_unlocated (Lit (Int, "112"))) ;
  [%expect {| 112 |}]

let%expect_test "pp_expr5" =
  printf "%s" (pp_unlocated (FunApp (StanLib, "pi", []))) ;
  [%expect {| stan::math::pi() |}]

let%expect_test "pp_expr6" =
  printf "%s"
    (pp_unlocated (FunApp (StanLib, "sqrt", [dummy_locate (Lit (Int, "123"))]))) ;
  [%expect {| stan::math::sqrt(123) |}]

let%expect_test "pp_expr7" =
  printf "%s"
    (pp_unlocated
       (FunApp
          ( StanLib
          , "atan2"
          , [dummy_locate (Lit (Int, "123")); dummy_locate (Lit (Real, "1.2"))]
          ))) ;
  [%expect {| stan::math::atan2(123, 1.2) |}]

let%expect_test "pp_expr9" =
  printf "%s"
    (pp_unlocated
       (TernaryIf
          ( dummy_locate (Lit (Int, "1"))
          , dummy_locate (Lit (Real, "1.2"))
          , dummy_locate (Lit (Real, "2.3")) ))) ;
  [%expect {| (1 ? 1.2 : 2.3) |}]

let%expect_test "pp_expr10" =
  printf "%s" (pp_unlocated (Indexed (dummy_locate (Var "a"), [All]))) ;
  [%expect
    {| rvalue(a, cons_list(index_omni(), nil_index_list()), "pretty printed e") |}]

let%expect_test "pp_expr11" =
  printf "%s"
    (pp_unlocated
       (FunApp (UserDefined, "poisson_rng", [dummy_locate (Lit (Int, "123"))]))) ;
  [%expect {| poisson_rng(123, base_rng__, pstream__) |}]
