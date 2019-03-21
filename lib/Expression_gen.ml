open Core_kernel
open Mir

let ends_with suffix s =
  String.is_suffix ~suffix:suffix s

let starts_with prefix s =
  String.is_prefix ~prefix:prefix s

let functions_requiring_namespace = 
  [ "e"; "pi"; "log2"; "log10"; "sqrt2"; "not_a_number"; 
    "positive_infinity"; "negative_infinity"; "machine_precision"; 
    "abs"; "acos"; "acosh"; "asin"; "asinh"; "atan"; "atan2"; "atanh"; 
    "cbrt"; "ceil"; "cos"; "cosh"; "erf"; "erfc"; "exp"; "exp2"; "expm1"; 
    "fabs"; "floor"; "lgamma"; "log"; "log1p"; "log2"; "log10"; "round"; 
    "sin"; "sinh"; "sqrt"; "tan"; "tanh"; "tgamma"; "trunc"; 
    "fdim"; "fmax"; "fmin"; "hypot"; "fma" ]

let equality a b = (a = b)

let contains_elt lst elt  = (List.mem lst elt ~equal:equality)

let stan_namespace_qualify f = 
  if (contains_elt functions_requiring_namespace f) then "stan::math::"^f else f

(* return true if the types of the two expression are the same *)
let types_match e1 e2 = 
  e1.texpr_type = e2.texpr_type

(* "__" is an illegal suffix for user functions, used for built-in operators not in signatures *)
let is_user_defined f = 
  not (Stan_math_signatures.is_stan_math_function_name f)
  && (not (ends_with "__" f))
  && (not (starts_with "stan::math::" f))

(* retun true if the tpe of the expression is integer or real *)
let is_scalar e = e.texpr_type = 
  Ast.UInt || e.texpr_type = Ast.UReal

let is_matrix e = 
  e.texpr_type = Ast.UMatrix

let is_row_vector e =
  e.texpr_type = Ast.URowVector

(* stub *)
let pretty_print _e = "pretty printed e"

(* stub *)
let rec gen_return_type = function
  | Ast.Void -> "void"
  | Ast.ReturnType(rt) -> gen_type_ut rt

(* terrible quadratic implementation *)
and gen_arg_types = function
  | [] -> ""
  | [(_adt, ut)] -> (gen_type_ut ut)
  | (_adt, ut)::ts -> sprintf "%s, %s" (gen_type_ut ut) (gen_arg_types ts)

and gen_type_ut = function
  | Ast.UInt -> "int"
  | Ast.UReal -> "local_scalar_t__"
  | Ast.UVector -> "Eigen::Matrix<local_scalar_t, -1, 1>"
  | Ast.URowVector -> "Eigen::Matrix<local_scalar_t, 1, -1>"
  | Ast.UMatrix -> "Eigen::Matrix<local_scalar_t, -1, 1>"
  | Ast.UArray(t) -> sprintf "std::vector<%s>" (gen_type_ut t)
  | Ast.UFun(args_t, return_t) -> 
    sprintf "std::function<%s(%s)>" 
      (gen_return_type return_t)
      (gen_arg_types args_t)
  | Ast.UMathLibraryFunction -> "std::function<void()>"

let gen_type e = gen_type_ut e.texpr_type


let suffix_args f =
  if ends_with "_rng" f then ["base_rng__"]
  else if ends_with "_lp" f then ["lp__"; "lp_accum__"]
  else []

let user_defined_args f = if is_user_defined f then ["pstream__"] else []

let gen_extra_fun_args f =
  String.concat ~sep:", " (List.append (suffix_args f) (user_defined_args f))

let include_sep s1 s2 = String.length s1 > 0 && String.length s2 > 0

let gen_sep s1 s2 =
  sprintf "%s%s%s" s1 (if include_sep s1 s2 then ", " else "") s2

let rec gen_exprs = function
  | [] -> ""
  | [hd] -> gen_expr hd
  | hd :: tl -> sprintf "%s, %s" (gen_expr hd) (gen_exprs tl)

and gen_index = function
  | All -> "stan::model::index_omni()"
  | Single e -> sprintf "stan::model::index_uni(%s)" (gen_expr e)
  | Upfrom e -> sprintf "stan::model::index_min(%s)" (gen_expr e)
  | Downfrom e -> sprintf "stan::model::index_max(%s)" (gen_expr e)
  | Between (e_low, e_high) ->
    sprintf "stan::model::index_min_max(%s, %s)" (gen_expr e_low)
      (gen_expr e_high)
  | MultiIndex e -> sprintf "stan::model::index_multi(%s)" (gen_expr e)

and gen_indexes = function
  | [] -> "stan::model::nil_index_list()"
  | idx :: idxs ->
    sprintf "stan::model::cons_list(%s, %s)" (gen_index idx)
      (gen_indexes idxs)

and gen_short_circuit_logical_op op es = 
  sprintf "(stan::math::value(%s) %s stan::math::value(%s))" 
    (gen_expr (List.nth_exn es 0)) op (gen_expr (List.nth_exn es 1))

and gen_unary fm es = 
  sprintf fm (gen_expr (List.hd_exn es))

and gen_binary fm es = 
  sprintf fm (gen_expr (first es)) (gen_expr (second es))

and first es = (List.nth_exn es 0)

and second es = (List.nth_exn es 1)

and gen_scalar_binary scalar_fmt generic_fmt es =
  gen_binary (if (is_scalar (first es) && is_scalar(second es)) then scalar_fmt else generic_fmt) es

(* assumes everything well formed from parser checks *)
and gen_fun_app f es = match f with
  | "And" -> gen_short_circuit_logical_op "&&" es
  | "Or" -> gen_short_circuit_logical_op "||" es
  | "PMinus" -> gen_unary (if is_scalar (List.hd_exn es) then "-%s" else "minus(%s)") es
  | "PPlus" -> gen_unary "%s" es
  | "Transpose" -> gen_unary (if (is_scalar (List.hd_exn es)) then "transpose(%s)"  else "%s") es
  | "PNot" -> gen_unary "logial_negation(%s)" es
  | "Minus" -> gen_scalar_binary "(%s - %s)" "subtract(%s, %s)" es
  | "Plus" -> gen_scalar_binary "(%s + %s)" "add(%s, %s)" es
  | "Times" -> gen_scalar_binary "(%s * %s)" "multiply(%s, %s)" es
  | "Divide" -> if (is_matrix (second es) && (is_matrix (first es) || is_row_vector (first es)))
    then gen_binary "mdivide_right(%s, %s)" es
    else gen_scalar_binary "(%s / %s)" "divide(%s, %s)" es
  | "Modulo" -> gen_binary "modulus(%s, %s)" es
  | "LDivide" -> gen_binary "mdivide_left(%s, %s)" es
  | "EltTimes" -> gen_scalar_binary "(%s * %s)" "elt_multiply(%s, %s)" es
  | "EltDivide" -> gen_scalar_binary "(%s / %s)" "elt_divide(%s, %s)" es
  | "Pow" -> gen_binary "pow(%s, %s)" es
  | "Equals" -> gen_binary "logical_eq(%s, %s)" es
  | "NEquals" -> gen_binary "logical_neq(%s, %s)" es
  | "Less" -> gen_binary "logical_lt(%s, %s)" es
  | "Leq" -> gen_binary "logical_lte(%s, %s)" es
  | "Greater" -> gen_binary "logical_gt(%s, %s)" es
  | "Geq" -> gen_binary "logical_gte(%s, %s)" es
  | "lmultiply" -> gen_binary "multiply_log(%s, %s)" es
  | "lchoose" -> gen_binary "binomial_coefficient_log(%s, %s)" es 
  | "target" -> "get_lp(lp__, lp_accum__)"
  | "get_lp" -> "get_lp(lp__, lp_accum__)"
  | "max" -> if (List.length es = 2) then gen_binary "std::max(%s, %s)" es
    else gen_ordinary_function f es
  | "min" -> if (List.length es = 2) then gen_binary "std::min(%s, %s)" es
    else gen_ordinary_function f es 
  | "ceil" -> if (is_scalar (first es)) then gen_unary "std::ceil(%s)" es
    else gen_ordinary_function f es
  | _ -> gen_ordinary_function (stan_namespace_qualify f) es 

and gen_ordinary_function f es =
  sprintf "%s(%s)" f (gen_sep (gen_exprs es) (gen_extra_fun_args f))

and gen_expr (e : expr_typed_located) =
  match e.texpr with
  | Var s -> s
  | Lit (Str, s) -> sprintf "%S" s
  | Lit (_, s) -> s
  | FunApp (f, es) -> gen_fun_app f es
  | TernaryIf (ec, et, ef) ->
    if types_match et ef then
      sprintf "(%s ? %s : %s)" (gen_expr ec) (gen_expr et) (gen_expr ef)
    else
      sprintf
        "(%s ? stan::math::promote_scalar<%s>(%s) : \
         stan::math::promote_scalar<%s>(%s)"
        (gen_expr ec)
        (gen_type e)
        (gen_expr et)
        (gen_type e)
        (gen_expr ef)
  | Indexed (e, idx) ->
    sprintf "stan::model::rvalue(%s, %s, %S)" (gen_expr e) (gen_indexes idx)
      (pretty_print e)

let%expect_test "endswith1" =
  printf "%B" (ends_with "" "") ;
  [%expect {| true |}]

let%expect_test "endswith2" =
  printf "%B" (ends_with "" "a") ;
  [%expect {| true |}]

let%expect_test "endswith3" =
  printf "%B" (ends_with "b" "") ;
  [%expect {| false |}]

let%expect_test "endswith4" =
  printf "%B" (ends_with "c" "c") ;
  [%expect {| true |}]

let%expect_test "endswith5" =
  printf "%B" (ends_with "_rng" "foo_rng") ;
  [%expect {| true |}]

let%expect_test "endswith5" =
  printf "%B" (ends_with "_rng" "_r") ;
  [%expect {| false |}]

(* these functions are just for testing *)
let dummy_locate e =
  { texpr = e; texpr_type = UInt; texpr_adlevel = DataOnly; texpr_loc = no_span }

let gen_unlocated e = gen_expr (dummy_locate e)

let%expect_test "gen_expr1" =
  printf "%s" (gen_unlocated (Var "a"));
  [%expect {| a |}]

let%expect_test "gen_expr2" =
  printf "%s" (gen_unlocated (Lit (Str, "b")));
  [%expect {| "b" |}]

let%expect_test "gen_expr3" =
  printf "%s" (gen_unlocated (Lit (Int, "112")));
  [%expect {| 112 |}]

let%expect_test "gen_expr4" =
  printf "%s" (gen_unlocated (Lit (Int, "112")));
  [%expect {| 112 |}]

let%expect_test "gen_expr5" =
  printf "%s" (gen_unlocated (FunApp ("pi", [])));
  [%expect {| stan::math::pi() |}]

let%expect_test "gen_expr6" =
  printf "%s" (gen_unlocated (FunApp ("sqrt", [(dummy_locate (Lit (Int, "123")))])));
  [%expect {| stan::math::sqrt(123) |}]

let%expect_test "gen_expr7" =
  printf "%s" 
    (gen_unlocated 
       (FunApp ("atan2", [(dummy_locate (Lit (Int, "123")));
                          (dummy_locate (Lit(Real, "1.2")))])));
  [%expect {| stan::math::atan2(123, 1.2) |}]

let%expect_test "gen_expr9" =
  printf "%s" (gen_unlocated(TernaryIf  (dummy_locate (Lit (Int, "1")), 
                                         (dummy_locate (Lit (Real, "1.2"))),
                                         (dummy_locate (Lit (Real, "2.3"))))));
  [%expect {| (1 ? 1.2 : 2.3) |}]

let%expect_test "gen_expr10" =
  printf "%s" (gen_unlocated (Indexed (dummy_locate (Var "a"), [All])));
  [%expect {| stan::model::rvalue(a, stan::model::cons_list(stan::model::index_omni(), stan::model::nil_index_list()), "pretty printed e") |}]

let%expect_test "gen_expr11" =
  printf "%s" (gen_unlocated (FunApp ("poisson_rng", [(dummy_locate (Lit (Int, "123")))])));
  [%expect {| poisson_rng(123, base_rng__) |}]
