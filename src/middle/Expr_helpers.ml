open Core_kernel
open Expr
open Common
open Helpers

let var meta name = Fixed.fix meta @@ Var name

(* == Literals ============================================================== *)

let lit meta lit_type str_value = Fixed.fix meta @@ Lit (lit_type, str_value)
let lit_int meta value = lit meta Int @@ string_of_int value
let lit_real meta value = lit meta Real @@ string_of_float value
let lit_string meta value = lit meta Str value

let is_lit ?type_ expr =
  match Fixed.pattern expr with
  | Lit (lit_ty, _) ->
      Option.value_map ~default:true ~f:(fun ty -> ty = lit_ty) type_
  | _ -> false

let int_of_lit expr =
  match Fixed.pattern expr with
  | Lit (Int, str) -> int_of_string_opt str
  | _ -> None

let real_of_lit expr =
  match Fixed.pattern expr with
  | Lit (Real, str) -> float_of_string_opt str
  | _ -> None

let string_of_lit expr =
  match Fixed.pattern expr with Lit (Str, str) -> Some str | _ -> None

(* == Logical =============================================================== *)

let simplify_and_opt meta e1 e2 =
  Option.map2
    ~f:(fun i1 i2 ->
      if i1 <> 0 && i2 <> 0 then lit_int meta 1 else lit_int meta 0 )
    (int_of_lit e1) (int_of_lit e2)

let and_ meta e1 e2 =
  simplify_and_opt meta e1 e2
  |> Option.value ~default:(Fixed.fix meta @@ EAnd (e1, e2))

let simplify_or_opt meta e1 e2 =
  Option.map2
    ~f:(fun i1 i2 ->
      if i1 <> 0 || i2 <> 0 then lit_int meta 1 else lit_int meta 0 )
    (int_of_lit e1) (int_of_lit e2)

let or_ meta e1 e2 =
  simplify_or_opt meta e1 e2
  |> Option.value ~default:(Fixed.fix meta @@ EOr (e1, e2))

(* == Indexed expressions =================================================== *)
let indexed meta e idxs = Fixed.fix meta @@ Indexed (e, idxs)
let index_all meta e = indexed meta e [All]
let index_single meta e ~idx = indexed meta e [Single idx]
let index_multi meta e ~idx = indexed meta e [MultiIndex idx]
let index_upfrom meta e ~idx = indexed meta e [Upfrom idx]
let index_between meta e ~lower ~upper = indexed meta e [Between (lower, upper)]

let index_bounds = function
  | Expr.All -> []
  | Single e | MultiIndex e | Upfrom e -> [e]
  | Between (e1, e2) -> [e1; e2]

let indices_of expr =
  match Fixed.pattern expr with Indexed (_, indices) -> indices | _ -> []

(* == Ternary If ============================================================ *)
let simplify_ternary_if_opt pred e_true e_false =
  int_of_lit pred |> Option.map ~f:(fun x -> if x = 1 then e_true else e_false)

let ternary_if meta pred e_true e_false =
  simplify_ternary_if_opt pred e_true e_false
  |> Option.value ~default:(Fixed.fix meta @@ TernaryIf (pred, e_true, e_false))

(* == Function application ================================================== *)

let fun_app meta fun_kind name args =
  Fixed.fix meta @@ FunApp (fun_kind, name, args)

let internal_fun meta fn args =
  fun_app meta CompilerInternal (Internal_fun.to_string fn) args

let stanlib_fun meta name args = Fixed.fix meta @@ FunApp (StanLib, name, args)
let user_fun meta name args = Fixed.fix meta @@ FunApp (UserDefined, name, args)

let is_fun ?kind ?name expr =
  match Fixed.pattern expr with
  | FunApp (fun_kind, fun_name, _) ->
      let same_name =
        Option.value_map ~default:true ~f:(fun name -> name = fun_name) name
      and same_kind =
        Option.value_map ~default:true ~f:(fun kind -> kind = fun_kind) kind
      in
      same_name && same_kind
  | _ -> false

let is_internal_fun ?fn expr =
  is_fun expr ~kind:CompilerInternal
    ?name:(Option.map ~f:Internal_fun.to_string fn)

let is_operator ?op expr =
  is_fun expr ~kind:StanLib ?name:(Option.map ~f:Operator.to_string op)

let is_trivial expr =
  match Fixed.pattern expr with Var _ | Lit _ -> true | _ -> false

let free_vars_algebra : ('a, String.Set.t) Fixed.algebra = function
  | _, Var n -> String.Set.singleton n
  | _, Lit _ -> String.Set.empty
  | _, TernaryIf (p, t, f) -> String.Set.union_list [p; t; f]
  | _, EAnd (a, b) | _, EOr (a, b) -> String.Set.union a b
  | _, Indexed (e, idxs) ->
      List.map idxs ~f:(fun idx -> String.Set.union_list @@ index_bounds idx)
      |> String.Set.union_list |> String.Set.union e
  | _, FunApp (_, fn_name, args) -> String.Set.(add (union_list args) fn_name)

(** Calculate the free (non-bound) variables in an expression *)
let free_vars expr = Fixed.cata free_vars_algebra expr

let contains_fun_algebra ?kind ?name = function
  | _, Expr.Fixed.Pattern.FunApp (fun_kind, fun_name, args) ->
      Option.(
        value_map ~default:true ~f:(fun name -> name = fun_name) name
        && value_map ~default:true ~f:(fun kind -> kind = fun_kind) kind)
      || List.exists ~f:Fn.id args
  | _, Var _ | _, Lit _ -> false
  | _, TernaryIf (e1, e2, e3) -> e1 || e2 || e3
  | _, EAnd (e1, e2) | _, EOr (e1, e2) -> e1 || e2
  | _, Indexed (e, idxs) ->
      e
      || List.exists idxs ~f:(fun idx ->
             List.exists ~f:Fn.id @@ index_bounds idx )

let contains_fun ?kind ?name expr =
  Fixed.cata (contains_fun_algebra ?kind ?name) expr

let contains_operator ?op expr =
  contains_fun ~kind:StanLib ?name:(Option.map ~f:Operator.to_string op) expr

let contains_internal_fun ?fn expr =
  contains_fun ~kind:StanLib
    ?name:(Option.map ~f:Internal_fun.to_string fn)
    expr


let monoid_algebra (type a) (module M : Monoid.S with type t = a) ~f ~bottom x = 
  M.combine 
    (f x)
    (match x with 
    | _ , Expr.Fixed.Pattern.Var _ | _ , Lit _ -> bottom
    | _ , FunApp (_,_,args) -> List.fold args ~init:M.empty ~f:M.combine
    | _ , TernaryIf (p,t,f) -> M.(combine p @@ combine t f)
    | _ , EAnd(a,b) | _ , EOr(a,b) -> M.combine a b 
    | _ , Indexed(a,idxs) -> 
      M.combine a @@ 
        List.fold ~init:M.empty ~f:M.combine 
          (List.concat_map ~f:index_bounds idxs)        
    )
    
let exists ~pred expr = 
  Expr.Fixed.cata 
    (monoid_algebra (module Monoid.Bool_or) ~f:pred ~bottom:false) expr

let for_all ~pred expr = 
  Expr.Fixed.cata 
    (monoid_algebra (module Monoid.Bool_and) ~f:pred ~bottom:false) expr

let contains_fun ?kind ?name expr = 
  let pred = function 
      | _ , Expr.Fixed.Pattern.FunApp (fun_kind, fun_name, _) ->    
        Option.(
            value_map ~default:true ~f:((=) fun_name) name && 
            value_map ~default:true ~f:((=) fun_kind) kind
        )
      | _ -> false
  in
  exists ~pred expr
(* == Binary operations ===================================================== *)

let lift_int_binop = function
  | Operator.Plus -> Some ( + )
  | Minus -> Some ( - )
  | Times -> Some ( * )
  | Divide -> Some ( / )
  | Modulo -> Some ( % )
  | Equals -> Some (fun x y -> if x = y then 1 else 0)
  | NEquals -> Some (fun x y -> if x = y then 0 else 1)
  | Less -> Some (fun x y -> if x < y then 1 else 0)
  | Leq -> Some (fun x y -> if x <= y then 1 else 0)
  | Greater -> Some (fun x y -> if x > y then 1 else 0)
  | Geq -> Some (fun x y -> if x >= y then 1 else 0)
  | And -> Some (fun x y -> if x = 1 && y = 1 then 1 else 0)
  | Or -> Some (fun x y -> if x = 1 || y = 1 then 1 else 0)
  | _ -> None

let lift_real_arith_binop = function
  | Operator.Plus -> Some ( +. )
  | Minus -> Some ( -. )
  | Times -> Some ( *. )
  | Divide -> Some ( /. )
  | _ -> None

let lift_real_logical_binop = function
  | Operator.Equals -> Some (fun x y -> if Float.equal x y then 1 else 0)
  | NEquals -> Some (fun x y -> if x <> y then 1 else 0)
  | Less -> Some (fun x y -> if x < y then 1 else 0)
  | Leq -> Some (fun x y -> if x <= y then 1 else 0)
  | Greater -> Some (fun x y -> if x > y then 1 else 0)
  | Geq -> Some (fun x y -> if x >= y then 1 else 0)
  | And -> Some (fun x y -> if x = 1.0 && y = 1.0 then 1 else 0)
  | Or -> Some (fun x y -> if x = 1.0 || y = 1.0 then 1 else 0)
  | _ -> None

let apply_int_binop meta op a b =
  lift_int_binop op
  |> Option.map2 ~f:(fun a f -> f a) (int_of_lit a)
  |> Option.map2 ~f:(fun b f -> f b) (int_of_lit b)
  |> Option.map ~f:(lit_int meta)

let apply_real_arith_binop meta op a b =
  lift_real_arith_binop op
  |> Option.map2 ~f:(fun a f -> f a) (real_of_lit a)
  |> Option.map2 ~f:(fun b f -> f b) (real_of_lit b)
  |> Option.map ~f:(lit_real meta)

let apply_real_logical_binop meta op a b =
  lift_real_logical_binop op
  |> Option.map2 ~f:(fun a f -> f a) (real_of_lit a)
  |> Option.map2 ~f:(fun b f -> f b) (real_of_lit b)
  |> Option.map ~f:(lit_int meta)

let apply_binop meta op a b =
  apply_int_binop meta op a b
  |> option_or_else ~if_none:(apply_real_arith_binop meta op a b)
  |> option_or_else ~if_none:(apply_real_logical_binop meta op a b)

let binop meta op a b =
  apply_binop meta op a b
  |> Option.value ~default:(stanlib_fun meta (Operator.to_string op) [a; b])

(* -- Plus ------------------------------------------------------------------ *)

let plus_fma meta a b =
  match Fixed.(pattern a, pattern b) with
  | FunApp (StanLib, "Times__", [c; d]), _ ->
      Some (stanlib_fun meta "fma" [c; d; b])
  | _, FunApp (StanLib, "Times__", [c; d]) ->
      Some (stanlib_fun meta "fma" [c; d; a])
  | _ -> None

let simplify_plus_opt meta a b = plus_fma meta a b

let plus meta a b =
  simplify_plus_opt meta a b
  |> Option.value ~default:(binop meta Operator.Plus a b)

(* -- Minus ----------------------------------------------------------------- *)

let minus_erfc meta a b =
  match Fixed.(pattern a, pattern b) with
  | Lit (Int, "1"), FunApp (StanLib, "erf", x) ->
      Some (stanlib_fun meta "erfc" x)
  | _ -> None

let minus_erf meta a b =
  match Fixed.(pattern a, pattern b) with
  | Lit (Int, "1"), FunApp (StanLib, "erfc", x) ->
      Some (stanlib_fun meta "erf" x)
  | _ -> None

let minus_expm1 meta a b =
  match Fixed.(pattern a, pattern b) with
  | FunApp (StanLib, "exp", x), Lit (Int, "1") ->
      Some (stanlib_fun meta "expm1" x)
  | _ -> None

let minus_gamma_q meta a b =
  match Fixed.(pattern a, pattern b) with
  | Lit (Int, "1"), FunApp (StanLib, "gamma_p", x) ->
      Some (stanlib_fun meta "gamma_q" x)
  | _ -> None

let minus_gamma_p meta a b =
  match Fixed.(pattern a, pattern b) with
  | Lit (Int, "1"), FunApp (StanLib, "gamma_q", x) ->
      Some (stanlib_fun meta "gamma_p" x)
  | _ -> None

let simplify_minus_opt meta a b =
  minus_erfc meta a b
  |> option_or_else ~if_none:(minus_erf meta a b)
  |> option_or_else ~if_none:(minus_expm1 meta a b)
  |> option_or_else ~if_none:(minus_gamma_q meta a b)
  |> option_or_else ~if_none:(minus_gamma_p meta a b)

let minus meta a b =
  simplify_minus_opt meta a b
  |> Option.value ~default:(binop meta Operator.Minus a b)

(* -- Times ----------------------------------------------------------------- *)

let times_scale_matrix_exp_multiply ~type_of meta a b =
  match (Fixed.pattern2 a, Fixed.pattern2 b) with
  | FunApp (StanLib, "matrix_exp", [FunApp (StanLib, "Times__", [x; y])]), _
    when type_of x = UnsizedType.UInt || type_of x = UReal ->
      Some (stanlib_fun meta "scale_matrix_exp_multiply" [x; y; b])
  | _, FunApp (StanLib, "matrix_exp", [FunApp (StanLib, "Times__", [x; y])])
    when type_of x = UInt || type_of x = UReal ->
      Some (stanlib_fun meta "scale_matrix_exp_multiply" [x; y; a])
  | _ -> None

let times_matrix_exp_multiply meta a b =
  match Fixed.pattern a with
  | FunApp (StanLib, "matrix_exp", [x]) ->
      Some (stanlib_fun meta "matrix_exp_multiply" [x; b])
  | _ -> None

let times_multiply_log meta a b =
  match Fixed.pattern a with
  | FunApp (StanLib, "log", [x]) ->
      Some (stanlib_fun meta "multiply_log" [x; b])
  | _ -> None

let times_quad_form_diag ~equal meta a b =
  match Fixed.(proj3 a, proj2 b) with
  | ( ( _
      , FunApp
          (StanLib, "transpose", [(_, FunApp (StanLib, "diag_matrix", [v]))]) )
    , ( _
      , FunApp
          (StanLib, "Times__", [c; (_, FunApp (StanLib, "diag_matrix", [w]))])
      ) )
    when equal (Fixed.inj v) w ->
      Some (stanlib_fun meta "quad_form_diag" @@ List.map ~f:Fixed.inj [c; v])
  | ( ( _
      , FunApp
          ( StanLib
          , "Times__"
          , [ ( _
              , FunApp
                  ( StanLib
                  , "transpose"
                  , [(_, FunApp (StanLib, "diag_matrix", [v]))] ) )
            ; c ] ) )
    , (_, FunApp (StanLib, "diag_matrix", [w])) )
    when equal v (Fixed.inj w) ->
      Some (stanlib_fun meta "quad_form_diag" [Fixed.inj2 c; v])
  | _ -> None

let times_quad_form ~equal meta lhs rhs =
  match Fixed.(proj2 lhs, pattern rhs) with
  | (_, FunApp (StanLib, "transpose", [b])), FunApp (StanLib, "Times__", [a; c])
    when equal (Fixed.inj b) c ->
      Some (stanlib_fun meta "quad_form" [a; Fixed.inj b])
  | ( ( _
      , FunApp
          (StanLib, "Times__", [(_, FunApp (StanLib, "transpose", [b])); a]) )
    , _ )
    when equal b rhs ->
      Some (stanlib_fun meta "quad_form" [Fixed.inj a; b])
  | _ -> None

let times_diag_post_multiply meta lhs rhs =
  match Fixed.pattern rhs with
  | FunApp (StanLib, "diag_matrix", [v]) ->
      Some (stanlib_fun meta "diag_post_multiply" [lhs; v])
  | _ -> None

let times_diag_pre_multiply meta lhs rhs =
  match Fixed.pattern lhs with
  | FunApp (StanLib, "diag_matrix", [v]) ->
      Some (stanlib_fun meta "diag_post_multiply" [v; rhs])
  | _ -> None

let simplify_times_opt ~type_of ~equal meta a b =
  times_scale_matrix_exp_multiply ~type_of meta a b
  |> option_or_else ~if_none:(times_quad_form_diag ~equal meta a b)
  |> option_or_else ~if_none:(times_quad_form ~equal meta a b)
  |> option_or_else ~if_none:(times_matrix_exp_multiply meta a b)
  |> option_or_else ~if_none:(times_multiply_log meta a b)
  |> option_or_else ~if_none:(times_diag_post_multiply meta a b)
  |> option_or_else ~if_none:(times_diag_pre_multiply meta a b)

let times meta a b =
  simplify_times_opt ~type_of:Typed.type_of ~equal:Typed.equal meta a b
  |> Option.value ~default:(binop meta Operator.Times a b)

(* -- Divide ---------------------------------------------------------------- *)

let divide meta a b = binop meta Operator.Divide a b

(* -- Pow ------------------------------------------------------------------- *)

let pow_exp2 meta a b =
  match Fixed.pattern a with
  | Lit (Int, "2") -> Some (stanlib_fun meta "exp2" [b])
  | _ -> None

let pow_square meta a b =
  match Fixed.pattern b with
  | Lit (Int, "2") -> Some (stanlib_fun meta "square" [a])
  | _ -> None

let pow_sqrt meta a b =
  match Fixed.pattern2 b with
  | Lit (Int, "0.5")
   |FunApp (StanLib, "Divide__", [Lit (Int, "1"); Lit (Int, "2")]) ->
      Some (stanlib_fun meta "sqrt" [a])
  | _ -> None

let simplify_pow_opt meta a b =
  pow_exp2 meta a b
  |> option_or_else ~if_none:(pow_square meta a b)
  |> option_or_else ~if_none:(pow_sqrt meta a b)

(** TODO: should the operator names be the same as the stan math function
    names?
*)
let pow meta a b =
  simplify_pow_opt meta a b
  |> Option.value ~default:(binop meta Operator.Pow a b)

(* -- Modulo ---------------------------------------------------------------- *)
let modulo meta a b = binop meta Operator.Modulo a b

(* -- Comparison ------------------------------------------------------------ *)
let eq meta a b = binop meta Operator.Equals a b
let neq meta a b = binop meta Operator.NEquals a b
let gt meta a b = binop meta Operator.Greater a b
let gteq meta a b = binop meta Operator.Geq a b
let lt meta a b = binop meta Operator.Less a b
let lteq meta a b = binop meta Operator.Leq a b

(* -- Logical --------------------------------------------------------------- *)

let logical_and meta a b = binop meta Operator.And a b
let logical_or meta a b = binop meta Operator.Or a b

(* == Unary operations ====================================================== *)

let lift_int_prefix_op = function
  | Operator.PPlus -> Some (fun x -> +x)
  | PMinus -> Some (fun x -> -x)
  | PNot -> Some (fun x -> if x = 0 then 1 else 0)
  | _ -> None

let lift_real_prefix_op = function
  | Operator.PPlus -> Some (fun x -> +.x)
  | PMinus -> Some (fun x -> -.x)
  | _ -> None

let unop_constant_int meta op e =
  let kf = lift_int_prefix_op op and kv = int_of_lit e in
  Option.map2 ~f:(fun f v -> lit_int meta @@ f v) kf kv

let unop_constant_real meta op e =
  let kf = lift_real_prefix_op op and kv = real_of_lit e in
  Option.map2 ~f:(fun f v -> lit_real meta @@ f v) kf kv

let apply_unop meta op e =
  unop_constant_int meta op e
  |> option_or_else ~if_none:(unop_constant_real meta op e)

let unop meta op e =
  apply_unop meta op e
  |> Option.value ~default:(stanlib_fun meta (Operator.to_string op) [e])

let transpose meta e = unop meta Operator.Transpose e
let logical_not meta e = unop meta Operator.PNot e
let negate meta e = unop meta Operator.PMinus e
let positive meta e = unop meta Operator.PPlus e

(* == General derived helpers =============================================== *)

let incr expr =
  let meta = Fixed.meta expr in
  binop meta Operator.Plus expr @@ lit_int meta 1

let decr expr =
  let meta = Fixed.meta expr in
  binop meta Operator.Minus expr @@ lit_int meta 1

(* == Constants ============================================================= *)

let zero meta = lit_int meta 0
let loop_bottom meta = lit_int meta 1
let sqrt2 meta = stanlib_fun meta "sqrt2" []

(* == StanLib smart constructors ============================================ *)

(* -- Log ------------------------------------------------------------------- *)

let log_log1m_exp meta a =
  match Fixed.pattern2 a with
  | FunApp (StanLib, "Minus__", [Lit (Int, "1"); FunApp (StanLib, "exp", [x])])
    ->
      Some (stanlib_fun meta "log1m_exp" [x])
  | _ -> None

let log_log1m_inv_logit meta a =
  match Fixed.pattern2 a with
  | FunApp
      (StanLib, "Minus__", [Lit (Int, "1"); FunApp (StanLib, "inv_logit", [x])])
    ->
      Some (stanlib_fun meta "log1m_inv_logit" [x])
  | _ -> None

let log_log1m meta a =
  match Fixed.proj2 a with
  | _, FunApp (StanLib, "Minus__", [(_, Lit (Int, "1")); x]) ->
      Some (stanlib_fun meta "log1m" [Fixed.inj x])
  | _ -> None

let log_log1p_exp meta a =
  match Fixed.pattern2 a with
  | FunApp (StanLib, "Plus__", [Lit (Int, "1"); FunApp (StanLib, "exp", [x])])
    ->
      Some (stanlib_fun meta "log1p_exp" [x])
  | _ -> None

let log_log1p meta a =
  match Fixed.proj2 a with
  | _, FunApp (StanLib, "Plus", [(_, Lit (Int, "1")); x])
   |_, FunApp (StanLib, "Plus", [x; (_, Lit (Int, "1"))]) ->
      Some (stanlib_fun meta "log1p" [Fixed.inj x])
  | _ -> None

let log_log_determinant meta a =
  match Fixed.pattern2 a with
  | FunApp (StanLib, "fabs", [FunApp (StanLib, "determinant", [x])]) ->
      Some (stanlib_fun meta "log_determinant" [x])
  | _ -> None

let log_log_diff_exp meta a =
  match Fixed.pattern2 a with
  | FunApp
      ( StanLib
      , "Minus__"
      , [FunApp (StanLib, "exp", [x]); FunApp (StanLib, "exp", [y])] ) ->
      Some (stanlib_fun meta "log_diff_exp" [x; y])
  | _ -> None

let log_log_sum_exp meta a =
  match Fixed.pattern2 a with
  | FunApp
      ( StanLib
      , "Plus__"
      , [FunApp (StanLib, "exp", [x]); FunApp (StanLib, "exp", [y])] ) ->
      Some (stanlib_fun meta "log_sum_exp" [x; y])
  | FunApp (StanLib, "sum", [FunApp (StanLib, "exp", xs)]) ->
      Some (stanlib_fun meta "log_sum_exp" xs)
  | _ -> None

let log_log_mix ~equal meta a =
  match Fixed.proj4 a with
  | ( _
    , FunApp
        ( StanLib
        , "Plus__"
        , [ ( _
            , FunApp
                ( StanLib
                , "Times__"
                , [theta; (_, FunApp (StanLib, "exp", [lambda1]))] ) )
          ; ( _
            , FunApp
                ( StanLib
                , "Times__"
                , [ ( _
                    , FunApp (StanLib, "Minus__", [(_, Lit (Int, "1")); theta'])
                    )
                  ; (_, FunApp (StanLib, "exp", [lambda2])) ] ) ) ] ) )
    when equal (Fixed.inj2 theta) (Fixed.inj theta') ->
      Some
        ( stanlib_fun meta "log_mix"
        @@ List.map ~f:Fixed.inj [theta'; lambda1; lambda2] )
  | _ -> None

let log_log_falling_factorial meta a =
  match Fixed.pattern a with
  | FunApp (StanLib, "falling_factorial", x) ->
      Some (stanlib_fun meta "log_falling_factorial" x)
  | _ -> None

let log_log_rising_factorial meta a =
  match Fixed.pattern a with
  | FunApp (StanLib, "rising_factorial", x) ->
      Some (stanlib_fun meta "log_rising_factorial" x)
  | _ -> None

let log_log_inv_logit meta a =
  match Fixed.pattern a with
  | FunApp (StanLib, "inv_logit", x) ->
      Some (stanlib_fun meta "log_inv_logit" x)
  | _ -> None

let log_log_softmax meta a =
  match Fixed.pattern a with
  | FunApp (StanLib, "softmax", x) -> Some (stanlib_fun meta "log_softmax" x)
  | _ -> None

let simplify_log_opt ~equal meta a =
  log_log1m_exp meta a
  |> option_or_else ~if_none:(log_log1m_inv_logit meta a)
  |> option_or_else ~if_none:(log_log1m meta a)
  |> option_or_else ~if_none:(log_log1p_exp meta a)
  |> option_or_else ~if_none:(log_log1p meta a)
  |> option_or_else ~if_none:(log_log_determinant meta a)
  |> option_or_else ~if_none:(log_log_diff_exp meta a)
  |> option_or_else ~if_none:(log_log_sum_exp meta a)
  |> option_or_else ~if_none:(log_log_mix ~equal meta a)
  |> option_or_else ~if_none:(log_log_falling_factorial meta a)
  |> option_or_else ~if_none:(log_log_rising_factorial meta a)
  |> option_or_else ~if_none:(log_log_inv_logit meta a)
  |> option_or_else ~if_none:(log_log_softmax meta a)

let log meta a =
  simplify_log_opt ~equal:Typed.equal meta a
  |> Option.value ~default:(stanlib_fun meta "log" [a])

(* -- Sum ------------------------------------------------------------------- *)

let sum_squared_distance meta a =
  match Fixed.pattern2 a with
  | FunApp (StanLib, "square", [FunApp (StanLib, "Minus__", [x; y])]) ->
      Some (stanlib_fun meta "squared_distance" [x; y])
  | _ -> None

let sum_trace meta a =
  match Fixed.pattern a with
  | FunApp (StanLib, "diagonal", x) -> Some (stanlib_fun meta "trace" x)
  | _ -> None

let simplify_sum_opt meta a =
  sum_squared_distance meta a |> option_or_else ~if_none:(sum_trace meta a)

let sum meta a =
  simplify_sum_opt meta a |> Option.value ~default:(stanlib_fun meta "sum" [a])

(* -- Square ---------------------------------------------------------------- *)

let square_variance meta a =
  match Fixed.pattern a with
  | FunApp (StanLib, "sd", [x]) -> Some (stanlib_fun meta "variance" [x])
  | _ -> None

let simplify_square_opt meta a = square_variance meta a

let square meta a =
  simplify_square_opt meta a
  |> Option.value ~default:(stanlib_fun meta "square" [a])

(* -- Sqrt ------------------------------------------------------------------ *)

let sqrt_sqrt2 meta a =
  match Fixed.pattern a with
  | Lit (Int, "2") -> Some (stanlib_fun meta "sqrt2" [])
  | _ -> None

let simplify_sqrt_opt meta a = sqrt_sqrt2 meta a

let sqrt meta a =
  simplify_sqrt_opt meta a
  |> Option.value ~default:(stanlib_fun meta "sqrt" [a])

(* -- Inv ------------------------------------------------------------------- *)

let inv_inv_sqrt meta a =
  match Fixed.pattern a with
  | FunApp (StanLib, "sqrt", [x]) -> Some (stanlib_fun meta "inv_sqrt" [x])
  | _ -> None

let inv_inv_square meta a =
  match Fixed.pattern a with
  | FunApp (StanLib, "square", [x]) -> Some (stanlib_fun meta "inv_square" [x])
  | _ -> None

let simplify_inv_opt meta a =
  inv_inv_sqrt meta a |> option_or_else ~if_none:(inv_inv_square meta a)

let inv meta a =
  simplify_inv_opt meta a |> Option.value ~default:(stanlib_fun meta "inv" [a])

(* == Matrix functions ====================================================== *)

(* -- Trace ----------------------------------------------------------------- *)

let trace_trace_gen_quad_form ~equal meta a =
  match Fixed.proj4 a with
  | ( _
    , FunApp
        ( StanLib
        , "Times__"
        , [ ( _
            , FunApp
                ( StanLib
                , "Times__"
                , [ ( _
                    , FunApp
                        ( StanLib
                        , "Times__"
                        , [d; (_, FunApp (StanLib, "transpose", [b]))] ) )
                  ; a ] ) )
          ; c ] ) )
    when equal b (Fixed.inj3 c) ->
      Some
        (stanlib_fun meta "trace_gen_quad_form" [Fixed.inj d; Fixed.inj2 a; b])
  | _ -> None

let trace_trace_quad_form meta a =
  match Fixed.pattern a with
  | FunApp (StanLib, "quad_form", [x; y]) ->
      Some (stanlib_fun meta "trace_quad_form" [x; y])
  | _ -> None

let simplify_trace_opt ~equal meta a =
  trace_trace_gen_quad_form ~equal meta a
  |> option_or_else ~if_none:(trace_trace_quad_form meta a)

let trace meta a =
  simplify_trace_opt ~equal:Typed.equal meta a
  |> Option.value ~default:(stanlib_fun meta "trace" [a])

(* -- Dot product ----------------------------------------------------------- *)

let dot_product_dot_self ~equal meta x y =
  if equal x y then Some (stanlib_fun meta "dot_self" [x]) else None

let simplify_dot_product_opt ~equal meta a b =
  dot_product_dot_self ~equal meta a b

let dot_product meta a b =
  simplify_dot_product_opt ~equal:Typed.equal meta a b
  |> Option.value ~default:(stanlib_fun meta "dot_product" [a; b])

(* -- Rows dot product ------------------------------------------------------ *)

let rows_dot_product_rows_dot_self ~equal meta x y =
  if equal x y then Some (stanlib_fun meta "rows_dot_self" [x]) else None

let simplify_rows_dot_product_opt ~equal meta a b =
  rows_dot_product_rows_dot_self ~equal meta a b

let rows_dot_product meta a b =
  simplify_rows_dot_product_opt ~equal:Typed.equal meta a b
  |> Option.value ~default:(stanlib_fun meta "rows_dot_product" [a; b])

(* -- Columns dot product --------------------------------------------------- *)

let columns_dot_product_columns_dot_self ~equal meta x y =
  if equal x y then Some (stanlib_fun meta "columns_dot_self" [x]) else None

let simplify_columns_dot_product_opt ~equal meta a b =
  columns_dot_product_columns_dot_self ~equal meta a b

let columns_dot_product meta a b =
  simplify_columns_dot_product_opt ~equal:Typed.equal meta a b
  |> Option.value ~default:(stanlib_fun meta "columns_dot_product" [a; b])

(* == Transformations for distributions ===================================== *)

(** Rewrite a distribution which is implicitly a linear model as an linear model
*)
let lpdf_glm_lpdf ~type_of to_glm param =
  match Fixed.proj2 param with
  | ( _
    , FunApp
        ( StanLib
        , "Plus__"
        , [alpha; (_, FunApp (StanLib, "Times__", [x; beta]))] ) )
    when type_of x = UnsizedType.UMatrix ->
      Some (to_glm x (Fixed.inj alpha) beta)
  | ( _
    , FunApp
        ( StanLib
        , "Plus__"
        , [(_, FunApp (StanLib, "Times__", [x; beta])); alpha] ) )
    when type_of x = UMatrix ->
      Some (to_glm x (Fixed.inj alpha) beta)
  | _ -> None

(** Rewrite a distribution which is implicitly a GLM as a GLM *)
let lpdf_trans_glm_lpdf ~type_of ~link to_glm param =
  match Fixed.proj3 param with
  | ( _
    , FunApp
        ( StanLib
        , link'
        , [ ( _
            , FunApp
                ( StanLib
                , "Plus__"
                , [alpha; (_, FunApp (StanLib, "Times__", [x; beta]))] ) ) ] )
    )
    when link' = link && type_of x = UnsizedType.UMatrix ->
      Some (to_glm x (Fixed.inj alpha) beta)
  | ( _
    , FunApp
        ( StanLib
        , link'
        , [ ( _
            , FunApp
                ( StanLib
                , "Plus__"
                , [(_, FunApp (StanLib, "Times__", [x; beta])); alpha] ) ) ] )
    )
    when link' = link && type_of x = UMatrix ->
      Some (to_glm x (Fixed.inj alpha) beta)
  | _, FunApp (StanLib, link', [(_, FunApp (StanLib, "Times__", [x; beta]))])
    when link' = link && type_of (Fixed.inj x) = UMatrix ->
      Some (to_glm (Fixed.inj x) (zero @@ fst x) (Fixed.inj beta))
  | _ -> None

(** Rewrite distribution to it's altenative parameterization *)
let lpdf_trans_lpdf ~link to_trans param =
  match Fixed.pattern param with
  | FunApp (StanLib, link', [alpha]) when link' = link -> Some (to_trans alpha)
  | _ -> None

let rng_trans_rng ~link to_trans param =
  match Fixed.pattern param with
  | FunApp (StanLib, link', [alpha]) when link' = link -> Some (to_trans alpha)
  | _ -> None

(* == Partial evaluation  =================================================== *)

let eval_binop ~type_of ~equal meta op e1 e2 =
  match op with
  | Operator.Pow -> pow meta e1 e2
  | Times ->
      simplify_times_opt ~type_of ~equal meta e1 e2
      |> Option.value ~default:(binop meta Operator.Times e1 e2)
  | Plus -> plus meta e1 e2
  | Minus -> minus meta e1 e2
  | _ -> binop meta op e1 e2

let eval_stanlib_fun_app ~type_of ~equal meta fn_name args =
  match (fn_name, args) with
  | "log", [x] ->
      simplify_log_opt ~equal meta x
      |> Option.value ~default:(stanlib_fun meta "log" [x])
  | "pow", [x; y] -> pow meta x y
  | "sum", [x] -> sum meta x
  | "square", [x] -> square meta x
  | "sqrt", [x] -> sqrt meta x
  | "inv", [x] -> inv meta x
  | "trace", [x] ->
      simplify_trace_opt ~equal meta x
      |> Option.value ~default:(stanlib_fun meta "trace" [x])
  | "dot_product", [x; y] ->
      simplify_dot_product_opt ~equal meta x y
      |> Option.value ~default:(stanlib_fun meta "dot_product" [x; y])
  | "rows_dot_product", [x; y] ->
      simplify_rows_dot_product_opt ~equal meta x y
      |> Option.value ~default:(stanlib_fun meta "rows_dot_product" [x; y])
  | "columns_dot_product", [x; y] ->
      simplify_columns_dot_product_opt ~equal meta x y
      |> Option.value ~default:(stanlib_fun meta "columns_dot_product" [x; y])
  | "bernoulli_lpmf", [y; theta] ->
      let to_logit_glm x alpha beta =
        stanlib_fun meta "bernoulli_logit_glm_lpmf" [y; x; alpha; beta]
      and to_logit alpha = stanlib_fun meta "bernoulli_logit_lpmf" [y; alpha]
      and default = stanlib_fun meta "bernoulli_lpmf" [y; theta] in
      lpdf_trans_glm_lpdf ~type_of ~link:"inv_logit" to_logit_glm theta
      |> option_or_else
           ~if_none:(lpdf_trans_lpdf ~link:"inv_logit" to_logit theta)
      |> Option.value ~default
  | "bernoulli_rng", [theta] ->
      let to_logit alpha = stanlib_fun meta "bernoulli_logit_rng" [alpha]
      and default = stanlib_fun meta "bernoulli_rng" [theta] in
      rng_trans_rng ~link:"inv_logit" to_logit theta |> Option.value ~default
  | "bernoulli_logit_lpmf", [y; alpha] ->
      let to_logit_glm x alpha beta =
        stanlib_fun meta "bernoulli_logit_glm_lpmf" [y; x; alpha; beta]
      and default = stanlib_fun meta "bernoulli_logit_lpmf" [y; alpha] in
      lpdf_glm_lpdf to_logit_glm ~type_of alpha |> Option.value ~default
  | "binomial_lpmf", [successes; trials; theta] ->
      let to_logit alpha =
        stanlib_fun meta "binomial_logit_lpmf" [successes; trials; alpha]
      and default =
        stanlib_fun meta "binomial_lpmf" [successes; trials; theta]
      in
      lpdf_trans_lpdf ~link:"inv_logit" to_logit theta |> Option.value ~default
  | "categorical_lpmf", [y; theta] ->
      let to_logit beta = stanlib_fun meta "categorical_logit_lpmf" [y; beta]
      and default = stanlib_fun meta "categorical_lpmf" [y; theta] in
      lpdf_trans_lpdf ~link:"inv_logit" to_logit theta |> Option.value ~default
  | "categorical_rng", [theta] ->
      let to_logit alpha = stanlib_fun meta "categorical_logit_rng" [alpha]
      and default = stanlib_fun meta "categorical_rng" [theta] in
      rng_trans_rng ~link:"inv_logit" to_logit theta |> Option.value ~default
  | "neg_binomial_2_lpmf", [n; location; precision] ->
      let to_logit_glm x alpha beta =
        stanlib_fun meta "neg_binomial_2_log_glm_lpmf"
          [n; x; alpha; beta; precision]
      and to_logit eta =
        stanlib_fun meta "neg_binomial_2_log_lpmf" [n; eta; precision]
      and default =
        stanlib_fun meta "neg_binomial_2_lpmf" [n; location; precision]
      in
      lpdf_trans_glm_lpdf ~type_of ~link:"exp" to_logit_glm location
      |> option_or_else
           ~if_none:(lpdf_trans_lpdf ~link:"exp" to_logit location)
      |> Option.value ~default
  | "neg_binomial_2_log_lpmf", [n; log_location; precision] ->
      let to_logit_glm x alpha beta =
        stanlib_fun meta "neg_binomial_2_log_glm_lpmf"
          [n; x; alpha; beta; precision]
      and default =
        stanlib_fun meta "neg_binomial_2_log_lpmf" [n; log_location; precision]
      in
      lpdf_glm_lpdf to_logit_glm ~type_of log_location |> Option.value ~default
  | "neg_binomial_2_rng", [location; precision] ->
      let to_logit eta =
        stanlib_fun meta "neg_binomial_2_log_rng" [eta; precision]
      and default =
        stanlib_fun meta "neg_binomial_2_rng" [location; precision]
      in
      rng_trans_rng ~link:"exp" to_logit location |> Option.value ~default
  | "poisson_lpmf", [y; lambda] ->
      let to_log_glm x alpha beta =
        stanlib_fun meta "poisson_log_glm_lpmf" [y; x; alpha; beta]
      and to_log eta = stanlib_fun meta "poisson_log_lpmf" [y; eta]
      and default = stanlib_fun meta "poisson_lpmf" [y; lambda] in
      lpdf_trans_glm_lpdf ~type_of ~link:"exp" to_log_glm lambda
      |> option_or_else ~if_none:(lpdf_trans_lpdf ~link:"exp" to_log lambda)
      |> Option.value ~default
  | "poisson_rng", [lambda] ->
      let to_log eta = stanlib_fun meta "poisson_log_rng" [eta]
      and default = stanlib_fun meta "poisson_rng" [lambda] in
      rng_trans_rng ~link:"exp" to_log lambda |> Option.value ~default
  | "poisson_log_lpmf", [y; alpha] ->
      let to_log_glm x alpha beta =
        stanlib_fun meta "poisson_log_glm_lpmf" [y; x; alpha; beta]
      and default = stanlib_fun meta "poisson_log_lpmf" [y; alpha] in
      lpdf_glm_lpdf ~type_of to_log_glm alpha |> Option.value ~default
  | "normal_lpdf", [y; mu; sigma] ->
      let to_glm x alpha beta =
        stanlib_fun meta "normal_id_glm_lpdf" [y; x; alpha; beta; sigma]
      and default = stanlib_fun meta "normal_lpdf" [y; mu; sigma] in
      lpdf_glm_lpdf ~type_of to_glm mu |> Option.value ~default
  | "multi_normal_lpdf", [y; mu; sigma] ->
      let to_trans tau = stanlib_fun meta "multi_normal_prec_lpdf" [y; mu; tau]
      and default = stanlib_fun meta "multi_normal_lpdf" [y; mu; sigma] in
      lpdf_trans_lpdf ~link:"inverse" to_trans sigma |> Option.value ~default
  | _ -> stanlib_fun meta fn_name args

let arg_types ~adlevel_of ~type_of xs =
  List.map ~f:(fun expr -> (adlevel_of expr, type_of expr)) xs

let return_type ~adlevel_of ~type_of expr =
  match Fixed.pattern expr with
  | FunApp (StanLib, name, args)
    when Option.is_some (Operator.of_string_opt name) ->
      let op = Option.value_exn (Operator.of_string_opt name)
      and arg_tys = arg_types ~adlevel_of ~type_of args in
      Stan_math.op_return_type op arg_tys
  | FunApp (StanLib, name, args) ->
      Stan_math.return_type name @@ arg_types ~adlevel_of ~type_of args
  | _ -> None

let check_return_type ~adlevel_of ~type_of e_orig e_simple =
  match return_type ~adlevel_of ~type_of e_simple with
  | None -> e_orig
  | _ -> e_simple

module type Evaluable = sig
  module Meta : Common.Meta.S

  val type_of : Meta.t Fixed.t -> UnsizedType.t
  val adlevel_of : Meta.t Fixed.t -> UnsizedType.autodifftype
  val equal : Meta.t Fixed.t -> Meta.t Fixed.t -> bool
end

let eval ~type_of ~adlevel_of ~equal ?(env = String.Map.empty) expr =
  let f expr =
    match Fixed.proj expr with
    | _, Var n -> String.Map.find env n |> Option.value ~default:expr
    | _, Lit _ -> expr
    | meta, TernaryIf (pred, e1, e2) -> ternary_if meta pred e1 e2
    | meta, EAnd (e1, e2) -> and_ meta e1 e2
    | meta, EOr (e1, e2) -> or_ meta e1 e2
    | _, Indexed _ -> expr
    | meta, FunApp (StanLib, fn_name, [x])
      when Option.is_some (Operator.of_string_opt fn_name) ->
        unop meta (Option.value_exn (Operator.of_string_opt fn_name)) x
        |> check_return_type ~adlevel_of ~type_of expr
    | meta, FunApp (StanLib, fn_name, [x; y])
      when Option.is_some (Operator.of_string_opt fn_name) ->
        eval_binop meta ~type_of ~equal
          (Option.value_exn (Operator.of_string_opt fn_name))
          x y
        |> check_return_type ~adlevel_of ~type_of expr
    | meta, FunApp (StanLib, fn_name, args) ->
        eval_stanlib_fun_app ~type_of ~equal meta fn_name args
        |> check_return_type ~adlevel_of ~type_of expr
    | _, FunApp _ -> expr
  in
  Fixed.transform_bottom_up f expr
