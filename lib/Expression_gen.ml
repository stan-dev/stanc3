open Core_kernel
open Mir
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

let rec stantype_prim_str = function
  | Ast.UInt -> "int"
  | UArray t -> stantype_prim_str t
  | _ -> "double"

let local_scalar ut = function
  | Ast.DataOnly -> stantype_prim_str ut
  | AutoDiffable -> "local_scalar_t__"

(* stub *)
let rec pp_return_type ppf = function
  | Ast.Void -> pf ppf "void"
  | Ast.ReturnType rt -> pp_unsizedtype ppf rt

and pp_unsizedtype_custom_scalar ppf (scalar, ut) =
  match ut with
  | Ast.UInt | UReal -> string ppf scalar
  | UArray t ->
      pf ppf "std::vector<%a>" pp_unsizedtype_custom_scalar (scalar, t)
  | UMatrix -> pf ppf "Eigen::Matrix<%s, -1, -1>" scalar
  | URowVector -> pf ppf "Eigen::Matrix<%s, 1, -1>" scalar
  | UVector -> pf ppf "Eigen::Matrix<%s, -1, 1>" scalar
  | x -> raise_s [%message (x : unsizedtype) "not implemented yet"]

and pp_unsizedtype_local ppf (adtype, ut) =
  let s = local_scalar ut adtype in
  pp_unsizedtype_custom_scalar ppf (s, ut)

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

let pp_expr_type ppf e =
  pp_unsizedtype_local ppf (e.texpr_adlevel, e.texpr_type)

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

and pp_logical_op ppf op lhs rhs =
  pf ppf "(stan::math::value(%a) %s stan::math::value(%a))" pp_expr lhs op
    pp_expr rhs

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
and gen_fun_app ppf ut f es =
  let operator_functions =
    Map.Poly.of_alist_exn
    @@ List.map ~f:(fun (k, v) -> (Operators.operator_name k, v))
    @@ [ ( Ast.PMinus
         , fun ppf es ->
             pp_unary ppf
               (if is_scalar (List.hd_exn es) then "-%a" else "minus(%a)")
               es )
       ; (PPlus, fun ppf es -> pp_unary ppf "%a" es)
       ; ( Transpose
         , fun ppf es ->
             pp_unary ppf
               (if is_scalar (List.hd_exn es) then "transpose(%a)" else "%a")
               es )
       ; (PNot, fun ppf es -> pp_unary ppf "logial_negation(%a)" es)
       ; ( Minus
         , fun ppf es -> pp_scalar_binary ppf "(%a - %a)" "subtract(%a, %a)" es
         )
       ; (Plus, fun ppf es -> pp_scalar_binary ppf "(%a + %a)" "add(%a, %a)" es)
       ; ( Times
         , fun ppf es -> pp_scalar_binary ppf "(%a * %a)" "multiply(%a, %a)" es
         )
       ; ( Divide
         , fun ppf es ->
             if
               is_matrix (second es)
               && (is_matrix (first es) || is_row_vector (first es))
             then pp_binary ppf "mdivide_right(%a, %a)" es
             else pp_scalar_binary ppf "(%a / %a)" "divide(%a, %a)" es )
       ; (Modulo, fun ppf es -> pp_binary ppf "modulus(%a, %a)" es)
       ; (LDivide, fun ppf es -> pp_binary ppf "mdivide_left(%a, %a)" es)
       ; ( EltTimes
         , fun ppf es ->
             pp_scalar_binary ppf "(%a * %a)" "elt_multiply(%a, %a)" es )
       ; ( EltDivide
         , fun ppf es ->
             pp_scalar_binary ppf "(%a / %a)" "elt_divide(%a, %a)" es )
       ; (Pow, fun ppf es -> pp_binary ppf "pow(%a, %a)" es)
       ; (Equals, fun ppf es -> pp_binary ppf "logical_eq(%a, %a)" es)
       ; (NEquals, fun ppf es -> pp_binary ppf "logical_neq(%a, %a)" es)
       ; (Less, fun ppf es -> pp_binary ppf "logical_lt(%a, %a)" es)
       ; (Leq, fun ppf es -> pp_binary ppf "logical_lte(%a, %a)" es)
       ; (Greater, fun ppf es -> pp_binary ppf "logical_gt(%a, %a)" es)
       ; (Geq, fun ppf es -> pp_binary ppf "logical_gte(%a, %a)" es) ]
  in
  let misc_special_math_fns =
    Map.Poly.of_alist_exn
      [ ("lmultiply", fun ppf es -> pp_binary ppf "multiply_log(%a, %a)" es)
      ; ( "lchoose"
        , fun ppf es -> pp_binary ppf "binomial_coefficient_log(%a, %a)" es )
      ; ("target", fun ppf _ -> pf ppf "get_lp(lp__, lp_accum__)")
      ; ("get_lp", fun ppf _ -> pf ppf "get_lp(lp__, lp_accum__)")
      ; ( "max"
        , fun ppf es ->
            if List.length es = 2 then pp_binary ppf "std::max(%a, %a)" es
            else pp_ordinary_fn ppf f es )
      ; ( "min"
        , fun ppf es ->
            if List.length es = 2 then pp_binary ppf "std::min(%a, %a)" es
            else pp_ordinary_fn ppf f es )
      ; ( "ceil"
        , fun ppf es ->
            if is_scalar (first es) then pp_unary ppf "std::ceil(%a)" es
            else pp_ordinary_fn ppf f es ) ]
  in
  let read_data_or_param ppf es =
    let i_or_r =
      match ut with
      | Ast.UInt -> "i"
      | UReal -> "r"
      | UVector | URowVector | UMatrix | UArray _
       |UFun (_, _)
       |UMathLibraryFunction ->
          raise_s [%message "Can't ReadData of " (ut : unsizedtype)]
    in
    pf ppf "context__.vals_%s(%a)" i_or_r pp_expr (List.hd_exn es)
  in
  let mir_special_functions =
    Map.Poly.of_alist_exn
      [ (fn_length, fun ppf es -> pp_unary ppf "length(%a)" es)
      ; ( fn_make_array
        , fun ppf es -> pf ppf "{%a}" (list ~sep:comma pp_expr) es )
      ; (fn_read_data, read_data_or_param)
      ; (fn_read_param, read_data_or_param)
      ; (fn_constrain, pp_constrain_funapp "constrain")
      ; (fn_unconstrain, pp_constrain_funapp "unconstrain")
      (* XXX Fill in the rest of the functions at the bottom of Mir.ml *)
       ]
  in
  let default ppf es = pp_ordinary_fn ppf (stan_namespace_qualify f) es in
  let pp_fn =
    [operator_functions; misc_special_math_fns; mir_special_functions]
    |> List.find_map ~f:(fun m -> Map.Poly.find m f)
    |> Option.value ~default
  in
  pp_fn ppf es

(* XXX actually, for params we have to combine read and constrain into one funapp *)
and pp_constrain_funapp constrain_or_un_str ppf = function
  | var
    :: {texpr= Lit (Str, constraint_flavor); _}
       :: {texpr= Lit (Str, base_type); _} :: dims ->
      pf ppf "%s_%s_%s(@[<hov>%a@])" base_type constraint_flavor
        constrain_or_un_str (list ~sep:comma pp_expr) (var :: dims)
  | es -> raise_s [%message "Bad constraint " (es : expr_typed_located list)]

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
  | FunApp (f, es) -> gen_fun_app ppf e.texpr_type f es
  | And (e1, e2) -> pp_logical_op ppf "&&" e1 e2
  | Or (e1, e2) -> pp_logical_op ppf "||" e1 e2
  | TernaryIf (ec, et, ef) ->
      let promoted ppf (t, e) =
        pf ppf "stan::math::promote_scalar<%a>(%a)" pp_expr_type t pp_expr e
      in
      let tform ppf = pf ppf "(@[<hov>%a@ ?@ %a@ :@ %a@])" in
      if types_match et ef then tform ppf pp_expr ec pp_expr et pp_expr ef
      else tform ppf pp_expr ec promoted (e, et) promoted (e, ef)
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
