open Core_kernel
open Mir

let rec ends_with suffix s = 
  let len_suffix = String.length suffix in
  let len_s = String.length s in
  len_suffix <= len_s &&
  String.equal suffix (String.sub s ~pos:(len_s - len_suffix) ~len:len_suffix)

and gen_exprs = function
  | [] -> ""
  | hd::[] -> gen_expr hd
  | hd::tl -> sprintf "%s, %s" (gen_expr hd) (gen_exprs tl)

and gen_op = function
  | Plus
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Modulo -> "%"
  | Or -> "||"
  | And -> "&&"
  | Equals -> "=="
  | NEquals -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

and gen_index = function
  | All -> "stan::model::index_omni()"
  | Single e -> sprintf "stan::model::index_uni(%s)" (gen_expr e)
  | Upfrom e -> sprintf "stan::model::index_min(%s)" (gen_expr e)
  | Downfrom e -> sprintf "stan::model::index_max(%s)" (gen_expr e)
  | Between (e_low, e_high) -> sprintf "stan::model::index_min_max(%s, %s)" (gen_expr e_low) (gen_expr e_high)
  | MultiIndex e -> sprintf "stan::model::index_multi(%s)" (gen_expr e)

and gen_indexes = function
   | [] -> "stan::model::nil_index_list()"
   | idx::idxs -> sprintf "stan::model::cons_list(%s, %s)" (gen_index idx) (gen_indexes idxs)

and is_user_defined _ = false

and suffix_args f =
  if (ends_with "_rng" f) 
  then [ "base_rng__" ]
  else (if (ends_with "_lp" f) 
        then [ "lp__"; "lp_accum__"]
        else [])

and user_defined_args f =
  if is_user_defined f then [ "pstream__" ] else []

and gen_extra_fun_args f = 
  String.concat ~sep:", " (List.append (suffix_args f) (user_defined_args f))

and include_sep s1 s2 = ((String.length s1) > 0) && ((String.length s2) > 0)

and gen_sep s1 s2 =
  sprintf "%s%s%s" s1 (if (include_sep s1 s2) then ", " else "") s2

and gen_expr = function
| Var s -> s
| Lit (Str, s) -> sprintf "%S" s
| Lit (_, s) -> s
| FunApp (f, es) -> sprintf "%s(%s)" f (gen_sep (gen_exprs es) (gen_extra_fun_args f))
| BinOp (e1, op, e2) -> sprintf "(%s %s %s)" (gen_expr e1) (gen_op op) (gen_expr e2)
| TernaryIf (ec, et, ef) -> sprintf "(%s ? %s : %s)" (gen_expr ec) (gen_expr et) (gen_expr ef)
| Indexed (e, idx) -> sprintf "stan::model::rvalue(%s, %s, %S)" (gen_expr e) (gen_indexes idx) (gen_expr e)

let%expect_test "endswith1" =
printf "%B" (ends_with "" "");
[%expect {| true |}]

let%expect_test "endswith2" =
printf "%B" (ends_with "" "a");
[%expect {| true |}]

let%expect_test "endswith3" =
printf "%B" (ends_with "b" "");
[%expect {| false |}]

let%expect_test "endswith4" =
printf "%B" (ends_with "c" "c");
[%expect {| true |}]

let%expect_test "endswith5" =
printf "%B" (ends_with "_rng" "foo_rng");
[%expect {| true |}]

let%expect_test "endswith5" =
printf "%B" (ends_with "_rng" "_r");
[%expect {| false |}]

let%expect_test "gen_expr1" =
printf "%s" (gen_expr (Var "a"));
[%expect {| a |}]

let%expect_test "gen_expr2" =
printf "%s" (gen_expr (Lit (Str, "b")));
[%expect {| "b" |}]

let%expect_test "gen_expr3" =
printf "%s" (gen_expr (Lit (Int, "112")));
[%expect {| 112 |}]

let%expect_test "gen_expr4" =
printf "%s" (gen_expr (Lit (Int, "112")));
[%expect {| 112 |}]

let%expect_test "gen_expr5" =
printf "%s" (gen_expr (FunApp ("foo", [])));
[%expect {| foo() |}]

let%expect_test "gen_expr6" =
printf "%s" (gen_expr (FunApp ("foo", [(Lit (Int, "123"))])));
[%expect {| foo(123) |}]

let%expect_test "gen_expr7" =
printf "%s" (gen_expr (FunApp ("bar", [(Lit (Int, "123")); Lit(Real, "1.2")])));
[%expect {| bar(123, 1.2) |}]

let%expect_test "gen_expr8" =
printf "%s" (gen_expr (BinOp ((Lit (Int, "123")), Plus,  (Lit (Int, "4")))));
[%expect {| (123 - 4) |}]

let%expect_test "gen_expr9" =
printf "%s" (gen_expr (TernaryIf ((Lit (Int, "1")), (Lit (Real, "1.2")), (Lit (Real, "2.3")))));
[%expect {| (1 ? 1.2 : 2.3) |}]


let%expect_test "gen_expr10" =
printf "%s" (gen_expr (Indexed ((Var "a"), [All])));
[%expect {| stan::model::rvalue(a, stan::model::cons_list(stan::model::index_omni(), stan::model::nil_index_list()), "a") |}]

let%expect_test "gen_expr11" =
printf "%s" (gen_expr (FunApp ("foo_rng", [(Lit (Int, "123"))])));
[%expect {| foo_rng(123, base_rng__) |}]