open Core_kernel
open Mir
open Fmt

let ends_with suffix s = String.is_suffix ~suffix s
let starts_with prefix s = String.is_prefix ~prefix s

let functions_requiring_namespace =
  Set.Poly.of_list
    [ "e"; "pi"; "log2"; "log10"; "sqrt2"; "not_a_number"; "positive_infinity"
    ; "negative_infinity"; "machine_precision"; "abs"; "acos"; "acosh"; "asin"
    ; "asinh"; "atan"; "atan2"; "atanh"; "cbrt"; "ceil"; "cos"; "cosh"; "erf"
    ; "erfc"; "exp"; "exp2"; "expm1"; "fabs"; "floor"; "lgamma"; "log"; "log1p"
    ; "log2"; "log10"; "round"; "sin"; "sinh"; "sqrt"; "tan"; "tanh"; "tgamma"
    ; "trunc"; "fdim"; "fmax"; "fmin"; "hypot"; "fma" ]

let contains_elt lst elt = List.mem lst elt ~equal:( = )

let stan_namespace_qualify f =
  if Set.mem functions_requiring_namespace f then "stan::math::" ^ f else f

(* return true if the types of the two expression are the same *)
let types_match e1 e2 = e1.texpr_type = e2.texpr_type

(* "__" is an illegal suffix for user functions, used for built-in operators not in signatures *)
let is_user_defined f =
  (not (Stan_math_signatures.is_stan_math_function_name f))
  && (not (ends_with "__" f))
  && not (starts_with "stan::math::" f)

(* retun true if the tpe of the expression is integer or real *)
let is_scalar e = e.texpr_type = Ast.UInt || e.texpr_type = Ast.UReal
let is_matrix e = e.texpr_type = Ast.UMatrix
let is_row_vector e = e.texpr_type = Ast.URowVector

(* stub *)
let pretty_print _e = "pretty printed e"

(* stub *)
let rec pp_return_type ppf = function
  | Ast.Void -> pf ppf "void"
  | Ast.ReturnType rt -> pp_unsizedtype ppf rt

and pp_unsizedtype ppf ut =
  match ut with
  | Ast.UInt -> pf ppf "int"
  | Ast.UReal -> pf ppf "local_scalar_t__"
  | Ast.UVector -> pf ppf "Eigen::Matrix<local_scalar_t, -1, 1>"
  | Ast.URowVector -> pf ppf "Eigen::Matrix<local_scalar_t, 1, -1>"
  | Ast.UMatrix -> pf ppf "Eigen::Matrix<local_scalar_t, -1, 1>"
  | Ast.UArray t -> pf ppf "std::vector<%a>" pp_unsizedtype t
  | Ast.UFun (args_t, return_t) ->
      let arg_types = List.map ~f:snd args_t in
      pf ppf "std::function<%a(%a)>" pp_return_type return_t
        (list ~sep:(const string ", ") pp_unsizedtype)
        arg_types
  | Ast.UMathLibraryFunction -> pf ppf "std::function<void()>"

let pp_expr_type ppf e = pp_unsizedtype ppf e.texpr_type

let suffix_args f =
  if ends_with "_rng" f then ["base_rng__"]
  else if ends_with "_lp" f then ["lp__"; "lp_accum__"]
  else []

let user_defined_args f = if is_user_defined f then ["pstream__"] else []
let gen_extra_fun_args f = suffix_args f @ user_defined_args f

let rec pp_index ppf = function
  | All -> pf ppf "stan::model::index_omni()"
  | Single e -> pf ppf "stan::model::index_uni(%a)" pp_expr e
  | Upfrom e -> pf ppf "stan::model::index_min(%a)" pp_expr e
  | Downfrom e -> pf ppf "stan::model::index_max(%a)" pp_expr e
  | Between (e_low, e_high) ->
      pf ppf "stan::model::index_min_max(%a, %a)" pp_expr e_low pp_expr e_high
  | MultiIndex e -> pf ppf "stan::model::index_multi(%a)" pp_expr e

and pp_indexes ppf = function
  | [] -> pf ppf "stan::model::nil_index_list()"
  | idx :: idxs ->
      pf ppf "stan::model::cons_list(%a, %a)" pp_index idx pp_indexes idxs

and pp_logical_op ppf op es =
  pf ppf "(stan::math::value(%a) %s stan::math::value(%a))" pp_expr
    (List.nth_exn es 0) op pp_expr (List.nth_exn es 1)

and pp_unary ppf fm es = pf ppf fm pp_expr (List.hd_exn es)
and pp_binary ppf fm es = pf ppf fm pp_expr (first es) pp_expr (second es)
and first es = List.nth_exn es 0
and second es = List.nth_exn es 1

and pp_scalar_binary ppf scalar_fmt generic_fmt es =
  pp_binary ppf
    ( if is_scalar (first es) && is_scalar (second es) then scalar_fmt
    else generic_fmt )
    es

(* assumes everything well formed from parser checks *)
and gen_fun_app ppf f es =
  match f with
  | "And" -> pp_logical_op ppf "&&" es
  | "Or" -> pp_logical_op ppf "||" es
  | "PMinus" ->
      pp_unary ppf
        (if is_scalar (List.hd_exn es) then "-%a" else "minus(%a)")
        es
  | "PPlus" -> pp_unary ppf "%a" es
  | "Transpose" ->
      pp_unary ppf
        (if is_scalar (List.hd_exn es) then "transpose(%a)" else "%a")
        es
  | "PNot" -> pp_unary ppf "logial_negation(%a)" es
  | "Minus" -> pp_scalar_binary ppf "(%a - %a)" "subtract(%a, %a)" es
  | "Plus" -> pp_scalar_binary ppf "(%a + %a)" "add(%a, %a)" es
  | "Times" -> pp_scalar_binary ppf "(%a * %a)" "multiply(%a, %a)" es
  | "Divide" ->
      if
        is_matrix (second es)
        && (is_matrix (first es) || is_row_vector (first es))
      then pp_binary ppf "mdivide_right(%a, %a)" es
      else pp_scalar_binary ppf "(%a / %a)" "divide(%a, %a)" es
  | "Modulo" -> pp_binary ppf "modulus(%a, %a)" es
  | "LDivide" -> pp_binary ppf "mdivide_left(%a, %a)" es
  | "EltTimes" -> pp_scalar_binary ppf "(%a * %a)" "elt_multiply(%a, %a)" es
  | "EltDivide" -> pp_scalar_binary ppf "(%a / %a)" "elt_divide(%a, %a)" es
  | "Pow" -> pp_binary ppf "pow(%a, %a)" es
  | "Equals" -> pp_binary ppf "logical_eq(%a, %a)" es
  | "NEquals" -> pp_binary ppf "logical_neq(%a, %a)" es
  | "Less" -> pp_binary ppf "logical_lt(%a, %a)" es
  | "Leq" -> pp_binary ppf "logical_lte(%a, %a)" es
  | "Greater" -> pp_binary ppf "logical_gt(%a, %a)" es
  | "Geq" -> pp_binary ppf "logical_gte(%a, %a)" es
  | "lmultiply" -> pp_binary ppf "multiply_log(%a, %a)" es
  | "lchoose" -> pp_binary ppf "binomial_coefficient_log(%a, %a)" es
  | "target" -> pf ppf "get_lp(lp__, lp_accum__)"
  | "get_lp" -> pf ppf "get_lp(lp__, lp_accum__)"
  | "max" ->
      if List.length es = 2 then pp_binary ppf "std::max(%a, %a)" es
      else pp_ordinary_fn ppf f es
  | "min" ->
      if List.length es = 2 then pp_binary ppf "std::min(%a, %a)" es
      else pp_ordinary_fn ppf f es
  | "ceil" ->
      if is_scalar (first es) then pp_unary ppf "std::ceil(%a)" es
      else pp_ordinary_fn ppf f es
  | _ -> pp_ordinary_fn ppf (stan_namespace_qualify f) es

and pp_ordinary_fn ppf f es =
  let extra_args = gen_extra_fun_args f in
  let sep = if List.is_empty extra_args then "" else ", " in
  pf ppf "%s(@[<hov>%a%s@])" f (list ~sep:comma pp_expr) es
    (sep ^ String.concat ~sep:", " extra_args)

and pp_expr ppf (e : expr_typed_located) =
  match e.texpr with
  | Var s -> pf ppf "%s" s
  | Lit (Str, s) -> pf ppf "%S" s
  | Lit (_, s) -> pf ppf "%s" s
  | FunApp (f, es) -> gen_fun_app ppf f es
  | TernaryIf (ec, et, ef) ->
      if types_match et ef then
        pf ppf "(%a ? %a : %a)" pp_expr ec pp_expr et pp_expr ef
      else
        pf ppf
          "(%a ? stan::math::promote_scalar<%a>(%a) : \
           stan::math::promote_scalar<%a>(%a)"
          pp_expr ec pp_expr_type e pp_expr et pp_expr_type e pp_expr ef
  | Indexed (e, idx) ->
      pf ppf "stan::model::rvalue(%a, %a, %S)" pp_expr e pp_indexes idx
        (pretty_print e)

(* these functions are just for testing *)
let dummy_locate e =
  {texpr= e; texpr_type= UInt; texpr_adlevel= DataOnly; texpr_loc= no_span}

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
  printf "%s" (pp_unlocated (FunApp ("pi", []))) ;
  [%expect {| stan::math::pi() |}]

let%expect_test "pp_expr6" =
  printf "%s"
    (pp_unlocated (FunApp ("sqrt", [dummy_locate (Lit (Int, "123"))]))) ;
  [%expect {| stan::math::sqrt(123) |}]

let%expect_test "pp_expr7" =
  printf "%s"
    (pp_unlocated
       (FunApp
          ( "atan2"
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
    {| stan::model::rvalue(a, stan::model::cons_list(stan::model::index_omni(), stan::model::nil_index_list()), "pretty printed e") |}]

let%expect_test "pp_expr11" =
  printf "%s"
    (pp_unlocated (FunApp ("poisson_rng", [dummy_locate (Lit (Int, "123"))]))) ;
  [%expect {| poisson_rng(123, base_rng__) |}]
