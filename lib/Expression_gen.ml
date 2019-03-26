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
let rec gen_return_type = function
  | Ast.Void -> "void"
  | Ast.ReturnType rt -> gen_type_ut rt

(* terrible quadratic implementation *)
and gen_arg_types = function
  | [] -> ""
  | [(_adt, ut)] -> gen_type_ut ut
  | (_adt, ut) :: ts -> sprintf "%s, %s" (gen_type_ut ut) (gen_arg_types ts)

and gen_type_ut = function
  | Ast.UInt -> "int"
  | Ast.UReal -> "local_scalar_t__"
  | Ast.UVector -> "Eigen::Matrix<local_scalar_t, -1, 1>"
  | Ast.URowVector -> "Eigen::Matrix<local_scalar_t, 1, -1>"
  | Ast.UMatrix -> "Eigen::Matrix<local_scalar_t, -1, 1>"
  | Ast.UArray t -> sprintf "std::vector<%s>" (gen_type_ut t)
  | Ast.UFun (args_t, return_t) ->
      sprintf "std::function<%s(%s)>" (gen_return_type return_t)
        (gen_arg_types args_t)
  | Ast.UMathLibraryFunction -> "std::function<void()>"

let gen_type ppf e = pf ppf "%s" (gen_type_ut e.texpr_type)

let suffix_args f =
  if ends_with "_rng" f then ["base_rng__"]
  else if ends_with "_lp" f then ["lp__"; "lp_accum__"]
  else []

let user_defined_args f = if is_user_defined f then ["pstream__"] else []
let gen_extra_fun_args f = suffix_args f @ user_defined_args f

let rec gen_index ppf = function
  | All -> pf ppf "stan::model::index_omni()"
  | Single e -> pf ppf "stan::model::index_uni(%a)" gen_expr e
  | Upfrom e -> pf ppf "stan::model::index_min(%a)" gen_expr e
  | Downfrom e -> pf ppf "stan::model::index_max(%a)" gen_expr e
  | Between (e_low, e_high) ->
      pf ppf "stan::model::index_min_max(%a, %a)" gen_expr e_low gen_expr
        e_high
  | MultiIndex e -> pf ppf "stan::model::index_multi(%a)" gen_expr e

and gen_indexes ppf = function
  | [] -> pf ppf "stan::model::nil_index_list()"
  | idx :: idxs ->
      pf ppf "stan::model::cons_list(%a, %a)" gen_index idx gen_indexes idxs

and gen_short_circuit_logical_op ppf op es =
  pf ppf "(stan::math::value(%a) %s stan::math::value(%a))" gen_expr
    (List.nth_exn es 0) op gen_expr (List.nth_exn es 1)

and gen_unary ppf fm es = pf ppf fm gen_expr (List.hd_exn es)
and gen_binary ppf fm es = pf ppf fm gen_expr (first es) gen_expr (second es)
and first es = List.nth_exn es 0
and second es = List.nth_exn es 1

and gen_scalar_binary ppf scalar_fmt generic_fmt es =
  gen_binary ppf
    ( if is_scalar (first es) && is_scalar (second es) then scalar_fmt
    else generic_fmt )
    es

(* assumes everything well formed from parser checks *)
and gen_fun_app ppf f es =
  match f with
  | "And" -> gen_short_circuit_logical_op ppf "&&" es
  | "Or" -> gen_short_circuit_logical_op ppf "||" es
  | "PMinus" ->
      gen_unary ppf
        (if is_scalar (List.hd_exn es) then "-%a" else "minus(%a)")
        es
  | "PPlus" -> gen_unary ppf "%a" es
  | "Transpose" ->
      gen_unary ppf
        (if is_scalar (List.hd_exn es) then "transpose(%a)" else "%a")
        es
  | "PNot" -> gen_unary ppf "logial_negation(%a)" es
  | "Minus" -> gen_scalar_binary ppf "(%a - %a)" "subtract(%a, %a)" es
  | "Plus" -> gen_scalar_binary ppf "(%a + %a)" "add(%a, %a)" es
  | "Times" -> gen_scalar_binary ppf "(%a * %a)" "multiply(%a, %a)" es
  | "Divide" ->
      if
        is_matrix (second es)
        && (is_matrix (first es) || is_row_vector (first es))
      then gen_binary ppf "mdivide_right(%a, %a)" es
      else gen_scalar_binary ppf "(%a / %a)" "divide(%a, %a)" es
  | "Modulo" -> gen_binary ppf "modulus(%a, %a)" es
  | "LDivide" -> gen_binary ppf "mdivide_left(%a, %a)" es
  | "EltTimes" -> gen_scalar_binary ppf "(%a * %a)" "elt_multiply(%a, %a)" es
  | "EltDivide" -> gen_scalar_binary ppf "(%a / %a)" "elt_divide(%a, %a)" es
  | "Pow" -> gen_binary ppf "pow(%a, %a)" es
  | "Equals" -> gen_binary ppf "logical_eq(%a, %a)" es
  | "NEquals" -> gen_binary ppf "logical_neq(%a, %a)" es
  | "Less" -> gen_binary ppf "logical_lt(%a, %a)" es
  | "Leq" -> gen_binary ppf "logical_lte(%a, %a)" es
  | "Greater" -> gen_binary ppf "logical_gt(%a, %a)" es
  | "Geq" -> gen_binary ppf "logical_gte(%a, %a)" es
  | "lmultiply" -> gen_binary ppf "multiply_log(%a, %a)" es
  | "lchoose" -> gen_binary ppf "binomial_coefficient_log(%a, %a)" es
  | "target" -> pf ppf "get_lp(lp__, lp_accum__)"
  | "get_lp" -> pf ppf "get_lp(lp__, lp_accum__)"
  | "max" ->
      if List.length es = 2 then gen_binary ppf "std::max(%a, %a)" es
      else gen_ordinary_function ppf f es
  | "min" ->
      if List.length es = 2 then gen_binary ppf "std::min(%a, %a)" es
      else gen_ordinary_function ppf f es
  | "ceil" ->
      if is_scalar (first es) then gen_unary ppf "std::ceil(%a)" es
      else gen_ordinary_function ppf f es
  | _ -> gen_ordinary_function ppf (stan_namespace_qualify f) es

and gen_ordinary_function ppf f es =
  let extra_args = gen_extra_fun_args f in
  let sep = if List.is_empty extra_args then "" else ", " in
  pf ppf "%s(@[<hov>%a%s@])" f (list ~sep:comma gen_expr) es
    (sep ^ String.concat ~sep:", " extra_args)

and gen_expr ppf (e : expr_typed_located) =
  match e.texpr with
  | Var s -> pf ppf "%s" s
  | Lit (Str, s) -> pf ppf "%S" s
  | Lit (_, s) -> pf ppf "%s" s
  | FunApp (f, es) -> gen_fun_app ppf f es
  | TernaryIf (ec, et, ef) ->
      if types_match et ef then
        pf ppf "(%a ? %a : %a)" gen_expr ec gen_expr et gen_expr ef
      else
        pf ppf
          "(%a ? stan::math::promote_scalar<%a>(%a) : \
           stan::math::promote_scalar<%a>(%a)"
          gen_expr ec gen_type e gen_expr et gen_type e gen_expr ef
  | Indexed (e, idx) ->
      pf ppf "stan::model::rvalue(%a, %a, %S)" gen_expr e gen_indexes idx
        (pretty_print e)

(* these functions are just for testing *)
let dummy_locate e =
  {texpr= e; texpr_type= UInt; texpr_adlevel= DataOnly; texpr_loc= no_span}

let gen_unlocated e = strf "%a" gen_expr (dummy_locate e)

let%expect_test "gen_expr1" =
  printf "%s" (gen_unlocated (Var "a")) ;
  [%expect {| a |}]

let%expect_test "gen_expr2" =
  printf "%s" (gen_unlocated (Lit (Str, "b"))) ;
  [%expect {| "b" |}]

let%expect_test "gen_expr3" =
  printf "%s" (gen_unlocated (Lit (Int, "112"))) ;
  [%expect {| 112 |}]

let%expect_test "gen_expr4" =
  printf "%s" (gen_unlocated (Lit (Int, "112"))) ;
  [%expect {| 112 |}]

let%expect_test "gen_expr5" =
  printf "%s" (gen_unlocated (FunApp ("pi", []))) ;
  [%expect {| stan::math::pi() |}]

let%expect_test "gen_expr6" =
  printf "%s"
    (gen_unlocated (FunApp ("sqrt", [dummy_locate (Lit (Int, "123"))]))) ;
  [%expect {| stan::math::sqrt(123) |}]

let%expect_test "gen_expr7" =
  printf "%s"
    (gen_unlocated
       (FunApp
          ( "atan2"
          , [dummy_locate (Lit (Int, "123")); dummy_locate (Lit (Real, "1.2"))]
          ))) ;
  [%expect {| stan::math::atan2(123, 1.2) |}]

let%expect_test "gen_expr9" =
  printf "%s"
    (gen_unlocated
       (TernaryIf
          ( dummy_locate (Lit (Int, "1"))
          , dummy_locate (Lit (Real, "1.2"))
          , dummy_locate (Lit (Real, "2.3")) ))) ;
  [%expect {| (1 ? 1.2 : 2.3) |}]

let%expect_test "gen_expr10" =
  printf "%s" (gen_unlocated (Indexed (dummy_locate (Var "a"), [All]))) ;
  [%expect
    {| stan::model::rvalue(a, stan::model::cons_list(stan::model::index_omni(), stan::model::nil_index_list()), "pretty printed e") |}]

let%expect_test "gen_expr11" =
  printf "%s"
    (gen_unlocated (FunApp ("poisson_rng", [dummy_locate (Lit (Int, "123"))]))) ;
  [%expect {| poisson_rng(123, base_rng__) |}]
