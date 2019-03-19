open Core_kernel
open Mir

(* stub *)
let types_match _e1 _e2 = true

(* stub *)
let is_user_defined _e = false

(* stub *)
let is_scalar _e = true

(* stub *)
let pretty_print _e = "pretty printed e"

(* stub *)
let gen_type _e = "double"

let ends_with suffix s =
  let len_suffix = String.length suffix in
  let len_s = String.length s in
  len_suffix <= len_s
  && String.equal suffix
    (String.sub s ~pos:(len_s - len_suffix) ~len:len_suffix)

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

and gen_expr (e : expr_typed_located) =
  match e.texpr with
  | Var s -> s
  | Lit (Str, s) -> sprintf "%S" s
  | Lit (_, s) -> s
  | FunApp (f, es) ->
    if f = "And__" then
      sprintf "(%s && %s)"
        (gen_expr (List.nth_exn es 0))
        (gen_expr (List.nth_exn es 1))
    else if f = "Or__" then
      sprintf "(%s || %s)"
        (gen_expr (List.nth_exn es 0))
        (gen_expr (List.nth_exn es 1))
    else sprintf "%s(%s)" f (gen_sep (gen_exprs es) (gen_extra_fun_args f))
  | TernaryIf (ec, et, ef) ->
    if types_match et ef then
      sprintf "(%s ? %s : %s)" (gen_expr ec) (gen_expr et) (gen_expr ef)
    else
      sprintf
        "(%s ? stan::math::promote_scalar<%s>(%s) : \
         stan::math::promote_scalar<%s>(%s)"
        (gen_expr ec)
        (gen_type (TernaryIf (ec, et, ef)))
        (gen_expr et)
        (gen_type (TernaryIf (ec, et, ef)))
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
  printf "%s" (gen_unlocated (FunApp ("foo", [])));
  [%expect {| foo() |}]


let%expect_test "gen_expr6" =
  printf "%s" (gen_unlocated (FunApp ("foo", [(dummy_locate (Lit (Int, "123")))])));
  [%expect {| foo(123) |}]


let%expect_test "gen_expr7" =
  printf "%s" 
    (gen_unlocated 
       (FunApp ("bar", [(dummy_locate (Lit (Int, "123")));
                        (dummy_locate (Lit(Real, "1.2")))])));
  [%expect {| bar(123, 1.2) |}]

let%expect_test "gen_expr9" =
  printf "%s" (gen_unlocated(TernaryIf (dummy_locate (Lit (Int, "1")), 
                                        (dummy_locate (Lit (Real, "1.2"))), (dummy_locate (Lit (Real, "2.3"))))));
  [%expect {| (1 ? 1.2 : 2.3) |}]


let%expect_test "gen_expr10" =
  printf "%s" (gen_unlocated (Indexed (dummy_locate (Var "a"), [All])));
  [%expect {| stan::model::rvalue(a, stan::model::cons_list(stan::model::index_omni(), stan::model::nil_index_list()), "pretty printed e") |}]

let%expect_test "gen_expr11" =
  printf "%s" (gen_unlocated (FunApp ("foo_rng", [(dummy_locate (Lit (Int, "123")))])));
  [%expect {| foo_rng(123, base_rng__) |}]
