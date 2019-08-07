open Core_kernel
open Common
open Helpers
open State

type litType = Mir_pattern.litType = Int | Real | Str
[@@deriving sexp, hash, compare]

type 'a index = 'a Mir_pattern.index =
  | All
  | Single of 'a
  | Upfrom of 'a
  | Between of 'a * 'a
  | MultiIndex of 'a
[@@deriving sexp, hash, map, compare, fold]

let pp_index pp_e ppf x = Mir_pretty_printer.pp_index pp_e ppf x

let pp_indexed pp_e ppf (ident, indices) =
  Fmt.pf ppf {|@[%s%a@]|} ident
    ( if List.is_empty indices then fun _ _ -> ()
    else Fmt.(list (pp_index pp_e) ~sep:comma |> brackets) )
    indices

module Fixed = struct
  module Pattern = struct
    type 'a t = 'a Mir_pattern.expr =
      | Var of string
      | Lit of litType * string
      | FunApp of Fun_kind.t * string * 'a list
      | TernaryIf of 'a * 'a * 'a
      | EAnd of 'a * 'a
      | EOr of 'a * 'a
      | Indexed of 'a * 'a index list
    [@@deriving sexp, hash, map, compare, fold]

    let pp pp_e ppf = Mir_pretty_printer.pp_expr pp_e ppf

    include Foldable.Make (struct type nonrec 'a t = 'a t

                                  let fold = fold
    end)

    module Make_traversable = Mir_pattern.Make_traversable_expr
    module Make_traversable2 = Mir_pattern.Make_traversable_expr2
  end

  (** Fixed-point of `expr` *)
  include Fix.Make (Pattern)

  module Traversable_state = Make_traversable2 (Cps.State)
  module Traversable_state_r = Make_traversable2 (Right.Cps.State)

  let map_accum_left ~f ~init x =
    Cps.State.(
      Traversable_state.traverse ~f:(fun a -> state @@ Fn.flip f a) x
      |> run_state ~init)

  let map_accum_right ~f ~init x =
    Right.Cps.State.(
      Traversable_state_r.traverse ~f:(fun a -> state @@ Fn.flip f a) x
      |> run_state ~init)
end

(** Expressions without meta data *)
module NoMeta = struct
  module Meta = struct
    type t = unit [@@deriving compare, sexp, hash]

    let pp _ _ = ()
  end

  include Specialized.Make (Fixed) (Meta)

  let remove_meta x = Fixed.map (Fn.const ()) x
end

(** Expressions with associated location and type *)
module Typed = struct
  module Meta = struct
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t sexp_opaque [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype }
    [@@deriving compare, create, sexp, hash]

    let empty =
      create ~type_:UnsizedType.uint ~adlevel:UnsizedType.DataOnly
        ~loc:Location_span.empty ()

    let adlevel {adlevel; _} = adlevel
    let type_ {type_; _} = type_
    let loc {loc; _} = loc
    let pp _ _ = ()
    let with_type ty meta = {meta with type_= ty}
  end

  include Specialized.Make (Fixed) (Meta)

  let type_of x = Meta.type_ @@ Fixed.meta x
  let loc_of x = Meta.loc @@ Fixed.meta x
  let adlevel_of x = Meta.adlevel @@ Fixed.meta x
end

(** Expressions with associated location, type and label *)
module Labelled = struct
  module Meta = struct
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t sexp_opaque [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype
      ; label: Label.t [@compare.ignore] }
    [@@deriving compare, create, sexp, hash]

    let label {label; _} = label
    let adlevel {adlevel; _} = adlevel
    let type_ {type_; _} = type_
    let loc {loc; _} = loc
    let pp _ _ = ()
  end

  include Specialized.Make (Fixed) (Meta)

  let label_of x = Meta.label @@ Fixed.meta x
  let type_of x = Meta.type_ @@ Fixed.meta x
  let loc_of x = Meta.loc @@ Fixed.meta x
  let adlevel_of x = Meta.adlevel @@ Fixed.meta x

  module Traversable_state = Fixed.Make_traversable2 (State)

  (** Statefully traverse a typed expression adding unique labels *)
  let label ?(init = Label.init) (expr : Typed.t) : t =
    let f {Typed.Meta.adlevel; type_; loc} =
      State.(
        get
        >>= fun label ->
        put (Label.next label)
        >>= fun _ -> return @@ Meta.create ~label ~adlevel ~type_ ~loc ())
    in
    Traversable_state.traverse ~f expr |> State.run_state ~init |> fst

  (** Build a map from expression labels to expressions *)
  let rec associate ?init:(assocs = Label.Map.empty) (expr : t) =
    let assocs_result : t Label.Map.t Map_intf.Or_duplicate.t =
      Label.Map.add ~key:(label_of expr) ~data:expr
        (associate_pattern assocs @@ Fixed.pattern expr)
    in
    match assocs_result with `Ok x -> x | _ -> assocs

  and associate_pattern assocs = function
    | Mir_pattern.Lit _ | Var _ -> assocs
    | FunApp (_, _, args) ->
        List.fold args ~init:assocs ~f:(fun accu x -> associate ~init:accu x)
    | EAnd (e1, e2) | EOr (e1, e2) ->
        associate ~init:(associate ~init:assocs e2) e1
    | TernaryIf (e1, e2, e3) ->
        associate ~init:(associate ~init:(associate ~init:assocs e3) e2) e1
    | Indexed (e, idxs) ->
        List.fold idxs ~init:(associate ~init:assocs e) ~f:associate_index

  and associate_index assocs = function
    | Mir_pattern.All -> assocs
    | Single e | Upfrom e | MultiIndex e -> associate ~init:assocs e
    | Between (e1, e2) -> associate ~init:(associate ~init:assocs e2) e1
end

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
  | All -> []
  | Single e | MultiIndex e | Upfrom e -> [e]
  | Between (e1, e2) -> [e1; e2]

let indices_of expr =
  match Fixed.pattern expr with Indexed (_, indices) -> indices | _ -> []

(* == Ternary If ============================================================ *)
let simplify_ternary_if_opt pred e_true e_false =
  int_of_lit pred |> Option.map ~f:(fun x -> if x = 1 then e_true else e_false)

let if_ meta pred e_true e_false =
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

let contains_fun_algebra ?kind ?name = function
  | _, Fixed.Pattern.FunApp (fun_kind, fun_name, args) ->
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

let times_scale_matrix_exp_multiply meta a b =
  match (Fixed.pattern2 a, Fixed.pattern2 b) with
  | FunApp (StanLib, "matrix_exp", [FunApp (StanLib, "Times__", [x; y])]), _
    when Typed.type_of x = UInt || Typed.type_of x = UReal ->
      Some (stanlib_fun meta "scale_matrix_exp_multiply" [x; y; b])
  | _, FunApp (StanLib, "matrix_exp", [FunApp (StanLib, "Times__", [x; y])])
    when Typed.type_of x = UInt || Typed.type_of x = UReal ->
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

let times_quad_form_diag meta a b =
  match Fixed.(proj3 a, proj2 b) with
  | ( ( _
      , FunApp
          (StanLib, "transpose", [(_, FunApp (StanLib, "diag_matrix", [v]))]) )
    , ( _
      , FunApp
          (StanLib, "Times__", [c; (_, FunApp (StanLib, "diag_matrix", [w]))])
      ) )
    when Typed.equal (Fixed.inj v) w ->
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
    when Typed.equal v (Fixed.inj w) ->
      Some (stanlib_fun meta "quad_form_diag" [Fixed.inj2 c; v])
  | _ -> None

let times_quad_form meta lhs rhs =
  match Fixed.(proj2 lhs, pattern rhs) with
  | (_, FunApp (StanLib, "transpose", [b])), FunApp (StanLib, "Times__", [a; c])
    when Typed.equal (Fixed.inj b) c ->
      Some (stanlib_fun meta "quad_form" [a; Fixed.inj b])
  | ( ( _
      , FunApp
          (StanLib, "Times__", [(_, FunApp (StanLib, "transpose", [b])); a]) )
    , _ )
    when Typed.equal b rhs ->
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

let simplify_times_opt meta a b =
  times_scale_matrix_exp_multiply meta a b
  |> option_or_else ~if_none:(times_matrix_exp_multiply meta a b)
  |> option_or_else ~if_none:(times_multiply_log meta a b)
  |> option_or_else ~if_none:(times_quad_form_diag meta a b)
  |> option_or_else ~if_none:(times_quad_form meta a b)
  |> option_or_else ~if_none:(times_diag_post_multiply meta a b)
  |> option_or_else ~if_none:(times_diag_pre_multiply meta a b)

let times meta a b =
  simplify_times_opt meta a b
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

let log_log_mix meta a =
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
    when Typed.equal (Fixed.inj2 theta) (Fixed.inj theta') ->
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

let simplify_log_opt meta a =
  log_log1m_exp meta a
  |> option_or_else ~if_none:(log_log1m_inv_logit meta a)
  |> option_or_else ~if_none:(log_log1m meta a)
  |> option_or_else ~if_none:(log_log1p_exp meta a)
  |> option_or_else ~if_none:(log_log1p meta a)
  |> option_or_else ~if_none:(log_log_determinant meta a)
  |> option_or_else ~if_none:(log_log_diff_exp meta a)
  |> option_or_else ~if_none:(log_log_sum_exp meta a)
  |> option_or_else ~if_none:(log_log_mix meta a)
  |> option_or_else ~if_none:(log_log_falling_factorial meta a)
  |> option_or_else ~if_none:(log_log_rising_factorial meta a)
  |> option_or_else ~if_none:(log_log_inv_logit meta a)
  |> option_or_else ~if_none:(log_log_softmax meta a)

let log meta a =
  simplify_log_opt meta a |> Option.value ~default:(stanlib_fun meta "log" [a])

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

let trace_trace_gen_quad_form meta a =
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
    when Typed.equal b (Fixed.inj3 c) ->
      Some
        (stanlib_fun meta "trace_gen_quad_form" [Fixed.inj d; Fixed.inj2 a; b])
  | _ -> None

let trace_trace_quad_form meta a =
  match Fixed.pattern a with
  | FunApp (StanLib, "quad_form", [x; y]) ->
      Some (stanlib_fun meta "trace_quad_form" [x; y])
  | _ -> None

let simplify_trace_opt meta a =
  trace_trace_gen_quad_form meta a
  |> option_or_else ~if_none:(trace_trace_quad_form meta a)

let trace meta a =
  simplify_trace_opt meta a
  |> Option.value ~default:(stanlib_fun meta "trace" [a])

(* -- Dot product ----------------------------------------------------------- *)

let dot_product_dot_self meta x y =
  if Typed.equal x y then Some (stanlib_fun meta "dot_self" [x]) else None

let simplify_dot_product_opt meta a b = dot_product_dot_self meta a b

let dot_product meta a b =
  simplify_dot_product_opt meta a b
  |> Option.value ~default:(stanlib_fun meta "dot_product" [a; b])

(* -- Rows dot product ------------------------------------------------------ *)

let rows_dot_product_rows_dot_self meta x y =
  if Typed.equal x y then Some (stanlib_fun meta "rows_dot_self" [x]) else None

let simplify_rows_dot_product_opt meta a b =
  rows_dot_product_rows_dot_self meta a b

let rows_dot_product meta a b =
  simplify_rows_dot_product_opt meta a b
  |> Option.value ~default:(stanlib_fun meta "rows_dot_product" [a; b])

(* -- Columns dot product --------------------------------------------------- *)

let columns_dot_product_columns_dot_self meta x y =
  if Typed.equal x y then Some (stanlib_fun meta "columns_dot_self" [x])
  else None

let simplify_columns_dot_product_opt meta a b =
  columns_dot_product_columns_dot_self meta a b

let columns_dot_product meta a b =
  simplify_columns_dot_product_opt meta a b
  |> Option.value ~default:(stanlib_fun meta "columns_dot_product" [a; b])

(* == Transformations for distributions ===================================== *)

(** Rewrite a distribution which is implicitly a linear model as an linear model
*)
let lpdf_glm_lpdf to_glm param =
  match Fixed.proj2 param with
  | ( _
    , FunApp
        ( StanLib
        , "Plus__"
        , [alpha; (_, FunApp (StanLib, "Times__", [x; beta]))] ) )
    when Typed.type_of x = UMatrix ->
      Some (to_glm x (Fixed.inj alpha) beta)
  | ( _
    , FunApp
        ( StanLib
        , "Plus__"
        , [(_, FunApp (StanLib, "Times__", [x; beta])); alpha] ) )
    when Typed.type_of x = UMatrix ->
      Some (to_glm x (Fixed.inj alpha) beta)
  | _ -> None

(** Rewrite a distribution which is implicitly a GLM as a GLM *)
let lpdf_trans_glm_lpdf ~link to_glm param =
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
    when link' = link && Typed.type_of x = UMatrix ->
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
    when link' = link && Typed.type_of x = UMatrix ->
      Some (to_glm x (Fixed.inj alpha) beta)
  | _, FunApp (StanLib, link', [(_, FunApp (StanLib, "Times__", [x; beta]))])
    when link' = link && Typed.Meta.type_ (fst x) = UMatrix ->
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

(* == Binary distributions ================================================== *)

(** Bernoulli-Logit Generalised Linear Model (Logistic Regression) *)
module Bernoulli_logit_glm = struct
  let lpmf meta y x alpha beta =
    stanlib_fun meta "bernoulli_logit_glm_lpmf" [y; x; alpha; beta]
end

(** Bernoulli Distribution, Logit Parameterization *)
module Bernoulli_logit = struct
  let rng meta theta = stanlib_fun meta "bernoulli_logit_rng" [theta]

  let lpmf meta y alpha =
    let to_logit_glm = Bernoulli_logit_glm.lpmf meta y
    and default = stanlib_fun meta "bernoulli_logit_lpmf" [y; alpha] in
    lpdf_glm_lpdf to_logit_glm alpha |> Option.value ~default
end

(** Bernoulli Distribution  *)
module Bernoulli = struct
  let distribution_prefix suffix = "bernoulli_" ^ suffix

  let lpmf meta y theta =
    let to_logit_glm = Bernoulli_logit_glm.lpmf meta y
    and to_logit = Bernoulli_logit.lpmf meta y
    and default = stanlib_fun meta (distribution_prefix "lpmf") [y; theta] in
    lpdf_trans_glm_lpdf ~link:"inv_logit" to_logit_glm theta
    |> option_or_else
         ~if_none:(lpdf_trans_lpdf ~link:"inv_logit" to_logit theta)
    |> Option.value ~default

  let cdf meta y theta = stanlib_fun meta (distribution_prefix "cdf") [y; theta]

  let lcdf meta y theta =
    stanlib_fun meta (distribution_prefix "lcdf") [y; theta]

  let lccdf meta y theta =
    stanlib_fun meta (distribution_prefix "lccmf") [y; theta]

  let rng meta theta =
    let to_logit = Bernoulli_logit.rng meta
    and default = stanlib_fun meta (distribution_prefix "rng") [theta] in
    rng_trans_rng ~link:"inv_logit" to_logit theta |> Option.value ~default
end

(* == Bounded discrete distributions ======================================== *)

(** Binomial Distribution, Logit Parameterization *)
module Binomial_logit = struct
  let distribution_prefix suffix = "bernoulli_logit_" ^ suffix

  let lpmf meta successes trials alpha =
    stanlib_fun meta (distribution_prefix "lpmf") [successes; trials; alpha]
end

(** Binomial Distribution *)
module Binomial = struct
  let distribution_prefix suffix = "binomial_" ^ suffix

  let lpmf meta successes trials theta =
    let to_logit = Binomial_logit.lpmf meta successes trials
    and default =
      stanlib_fun meta (distribution_prefix "lpmf") [successes; trials; theta]
    in
    lpdf_trans_lpdf ~link:"inv_logit" to_logit theta |> Option.value ~default

  let cdf meta successes trials theta =
    stanlib_fun meta (distribution_prefix "cdf") [successes; trials; theta]

  let lcdf meta successes trials theta =
    stanlib_fun meta (distribution_prefix "lcdf") [successes; trials; theta]

  let lccdf meta successes trials theta =
    stanlib_fun meta (distribution_prefix "lccdf") [successes; trials; theta]

  (** Log parameterization has no rng *)
  let rng meta trials theta =
    stanlib_fun meta (distribution_prefix "rng") [trials; theta]
end

(** Beta-Binomial Distribution *)
module Beta_binomial = struct
  let distribution_prefix suffix = "beta_binomial_" ^ suffix

  let lpmf meta successes trials alpha beta =
    stanlib_fun meta
      (distribution_prefix "lpmf")
      [successes; trials; alpha; beta]

  let cdf meta successes trials alpha beta =
    stanlib_fun meta
      (distribution_prefix "cdf")
      [successes; trials; alpha; beta]

  let lcdf meta successes trials alpha beta =
    stanlib_fun meta
      (distribution_prefix "lcdf")
      [successes; trials; alpha; beta]

  let lccdf meta successes trials alpha beta =
    stanlib_fun meta
      (distribution_prefix "lccdf")
      [successes; trials; alpha; beta]

  let rng meta trials alpha beta =
    stanlib_fun meta (distribution_prefix "rng") [trials; alpha; beta]
end

(** Hypergeometric Distribution *)
module Hypergeometric = struct
  let distribution_prefix suffix = "hypergeometric_" ^ suffix

  let lpmf meta successes trials a b =
    stanlib_fun meta (distribution_prefix "lpmf") [successes; trials; a; b]

  let rng meta trials a b =
    stanlib_fun meta (distribution_prefix "rng") [trials; a; b]
end

(** Categorical Distribution, Logit Parameterization *)
module Categorical_logit = struct
  let distribution_prefix suffix = "categorical_logit_" ^ suffix
  let lpmf meta y beta = stanlib_fun meta (distribution_prefix "lpmf") [y; beta]
  let rng meta beta = stanlib_fun meta (distribution_prefix "rng") [beta]
end

(** Categorical Distribution *)
module Categorical = struct
  let distribution_prefix suffix = "categorical_" ^ suffix

  let lpmf meta y theta =
    let to_logit = Categorical_logit.lpmf meta y
    and default = stanlib_fun meta (distribution_prefix "lpmf") [y; theta] in
    lpdf_trans_lpdf ~link:"inv_logit" to_logit theta |> Option.value ~default

  let rng meta theta =
    let to_logit = Categorical_logit.rng meta
    and default = stanlib_fun meta (distribution_prefix "rng") [theta] in
    rng_trans_rng ~link:"inv_logit" to_logit theta |> Option.value ~default
end

(** Ordered Logistic Distribution *)
module Ordered_logistic = struct
  let distribution_prefix suffix = "ordered_logistic_" ^ suffix

  let lpmf meta k eta c =
    stanlib_fun meta (distribution_prefix "lpmf") [k; eta; c]

  let rng meta eta c = stanlib_fun meta (distribution_prefix "rng") [eta; c]
end

(** Ordered Probit Distribution *)
module Ordered_probit = struct
  let distribution_prefix suffix = "ordered_probit_" ^ suffix

  let lpmf meta k eta c =
    stanlib_fun meta (distribution_prefix "lpmf") [k; eta; c]

  let rng meta eta c = stanlib_fun meta (distribution_prefix "rng") [eta; c]
end

(* == Unbounded discrete distributions ====================================== *)

(** Negative Binomial Distribution (log alternative parameterization) *)
module Neg_binomial_2_log_glm = struct
  let distribution_prefix suffix = "neg_binomial_2_log_glm_" ^ suffix

  let lpmf meta n x alpha beta precision =
    stanlib_fun meta (distribution_prefix "lpmf") [n; x; alpha; beta; precision]

  let rng meta x alpha beta precision =
    stanlib_fun meta (distribution_prefix "rng") [x; alpha; beta; precision]
end

(** Negative Binomial Distribution (log alternative parameterization) *)
module Neg_binomial_2_log = struct
  let distribution_prefix suffix = "neg_binomial_2_log_" ^ suffix

  let lpmf meta n log_location precision =
    let to_logit_glm x alpha beta =
      Neg_binomial_2_log_glm.lpmf meta n x alpha beta precision
    and default =
      stanlib_fun meta (distribution_prefix "lpmf") [n; log_location; precision]
    in
    lpdf_glm_lpdf to_logit_glm log_location |> Option.value ~default

  let rng meta log_location inv_overdispersion =
    stanlib_fun meta
      (distribution_prefix "rng")
      [log_location; inv_overdispersion]
end

(** Negative Binomial Distribution (alternative parameterization) *)
module Neg_binomial_2 = struct
  let distribution_prefix suffix = "neg_binomial_2_" ^ suffix

  let lpmf meta n location precision =
    let to_logit_glm x alpha beta =
      Neg_binomial_2_log_glm.lpmf meta n x alpha beta precision
    and to_logit eta = Neg_binomial_2_log.lpmf meta n eta precision
    and default =
      stanlib_fun meta (distribution_prefix "lpmf") [n; location; precision]
    in
    lpdf_trans_glm_lpdf ~link:"exp" to_logit_glm location
    |> option_or_else ~if_none:(lpdf_trans_lpdf ~link:"exp" to_logit location)
    |> Option.value ~default

  let cdf meta n location precision =
    stanlib_fun meta (distribution_prefix "cdf") [n; location; precision]

  let lcdf meta n location precision =
    stanlib_fun meta (distribution_prefix "lcdf") [n; location; precision]

  let lccdf meta n location precision =
    stanlib_fun meta (distribution_prefix "lccdf") [n; location; precision]

  let rng meta location precision =
    let to_logit eta = Neg_binomial_2_log.rng meta eta precision
    and default =
      stanlib_fun meta (distribution_prefix "rng") [location; precision]
    in
    rng_trans_rng ~link:"exp" to_logit location |> Option.value ~default
end

(** Negative Binomial Distribution *)
module Neg_binomial = struct
  let distribution_prefix suffix = "neg_binomial_" ^ suffix

  let lpmf meta n shape inverse_scale =
    stanlib_fun meta (distribution_prefix "lpmf") [n; shape; inverse_scale]

  let cdf meta n shape inverse_scale =
    stanlib_fun meta (distribution_prefix "cdf") [n; shape; inverse_scale]

  let lcdf meta n shape inverse_scale =
    stanlib_fun meta (distribution_prefix "lcdf") [n; shape; inverse_scale]

  let lccdf meta n shape inverse_scale =
    stanlib_fun meta (distribution_prefix "lccdf") [n; shape; inverse_scale]

  let rng meta shape inverse_scale =
    stanlib_fun meta (distribution_prefix "rng") [shape; inverse_scale]
end

module Poisson_log_glm = struct
  let distribution_prefix suffix = "poisson_log_glm_" ^ suffix

  let lpmf meta y x alpha beta =
    stanlib_fun meta (distribution_prefix "lpmf") [y; x; alpha; beta]

  let rng meta x alpha beta =
    stanlib_fun meta (distribution_prefix "rng") [x; alpha; beta]
end

module Poisson_log = struct
  let distribution_prefix suffix = "poisson_log_" ^ suffix

  let lpmf meta n alpha =
    let to_log_glm x alpha beta = Poisson_log_glm.lpmf meta n x alpha beta
    and default = stanlib_fun meta (distribution_prefix "lpmf") [n; alpha] in
    lpdf_glm_lpdf to_log_glm alpha |> Option.value ~default

  let rng meta alpha = stanlib_fun meta (distribution_prefix "rng") [alpha]
end

(** Poisson Distribution *)
module Poisson = struct
  let distribution_prefix suffix = "poisson_" ^ suffix

  let lpmf meta n lambda =
    let to_log_glm x alpha beta = Poisson_log_glm.lpmf meta n x alpha beta
    and to_log eta = Poisson_log.lpmf meta n eta
    and default = stanlib_fun meta (distribution_prefix "lpmf") [n; lambda] in
    lpdf_trans_glm_lpdf ~link:"exp" to_log_glm lambda
    |> option_or_else ~if_none:(lpdf_trans_lpdf ~link:"exp" to_log lambda)
    |> Option.value ~default

  let cdf meta n lambda =
    stanlib_fun meta (distribution_prefix "cdf") [n; lambda]

  let lcdf meta n lambda =
    stanlib_fun meta (distribution_prefix "lcdf") [n; lambda]

  let lccdf meta n lambda =
    stanlib_fun meta (distribution_prefix "lccdf") [n; lambda]

  let rng meta lambda =
    let to_log eta = Poisson_log.rng meta eta
    and default = stanlib_fun meta (distribution_prefix "rng") [lambda] in
    rng_trans_rng ~link:"exp" to_log lambda |> Option.value ~default
end

module Multinomial = struct
  let distribution_prefix suffix = "multinomial_" ^ suffix

  let lpmf meta y theta =
    stanlib_fun meta (distribution_prefix "lpmf") [y; theta]

  let rng meta theta total_count =
    stanlib_fun meta (distribution_prefix "rng") [theta; total_count]
end

(* == Unbounded continuous ================================================== *)

(** Normal-Id Generalised Linear Model (Linear Regression) *)
module Normal_id_glm = struct
  let distribution_prefix suffix = "normal_id_glm_" ^ suffix

  let lpdf meta y x alpha beta sigma =
    stanlib_fun meta (distribution_prefix "lpdf") [y; x; alpha; beta; sigma]
end

(** Standard Normal Distribution *)
module Std_normal = struct
  let distribution_prefix suffix = "std_normal_" ^ suffix
  let lpdf meta y = stanlib_fun meta (distribution_prefix "ldmf") [y]
end

(** Normal Distribution *)
module Normal = struct
  let distribution_prefix suffix = "normal_" ^ suffix

  let lpdf meta y mu sigma =
    let to_glm x alpha beta = Normal_id_glm.lpdf meta y x alpha beta sigma
    and default =
      stanlib_fun meta (distribution_prefix "lpdf") [y; mu; sigma]
    in
    lpdf_glm_lpdf to_glm mu |> Option.value ~default

  let cdf meta y mu sigma =
    stanlib_fun meta (distribution_prefix "cdf") [y; mu; sigma]

  let lcdf meta y mu sigma =
    stanlib_fun meta (distribution_prefix "lcdf") [y; mu; sigma]

  let lccdf meta y mu sigma =
    stanlib_fun meta (distribution_prefix "lccdf") [y; mu; sigma]

  let rng meta mu sigma =
    stanlib_fun meta (distribution_prefix "rng") [mu; sigma]
end

(** Exponentially Modified Normal Distribution *)
module Exp_mod_normal = struct
  let distribution_prefix suffix = "exp_mod_normal_" ^ suffix

  let lpdf meta y mu sigma lambda =
    stanlib_fun meta (distribution_prefix "lpdf") [y; mu; sigma; lambda]

  let cdf meta y mu sigma lambda =
    stanlib_fun meta (distribution_prefix "cdf") [y; mu; sigma; lambda]

  let lcdf meta y mu sigma lambda =
    stanlib_fun meta (distribution_prefix "lcdf") [y; mu; sigma; lambda]

  let lccdf meta y mu sigma lambda =
    stanlib_fun meta (distribution_prefix "lccdf") [y; mu; sigma; lambda]

  let rng meta mu sigma lambda =
    stanlib_fun meta (distribution_prefix "rng") [mu; sigma; lambda]
end

(** Skew Normal Distribution *)
module Skew_normal = struct
  let distribution_prefix suffix = "skew_normal_" ^ suffix

  let lpdf meta y location scale shape =
    stanlib_fun meta (distribution_prefix "lpdf") [y; location; scale; shape]

  let cdf meta y location scale shape =
    stanlib_fun meta (distribution_prefix "cdf") [y; location; scale; shape]

  let lcdf meta y location scale shape =
    stanlib_fun meta (distribution_prefix "lcdf") [y; location; scale; shape]

  let lccdf meta y location scale shape =
    stanlib_fun meta (distribution_prefix "lccdf") [y; location; scale; shape]

  let rng meta location scale shape =
    stanlib_fun meta (distribution_prefix "rng") [location; scale; shape]
end

(** Student-T Distribution *)
module Student_t = struct
  let distribution_prefix suffix = "student_t_" ^ suffix

  let lpdf meta y dof location scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; dof; location; scale]

  let cdf meta y dof location scale =
    stanlib_fun meta (distribution_prefix "cdf") [y; dof; location; scale]

  let lcdf meta y dof location scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; dof; location; scale]

  let lccdf meta y dof location scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; dof; location; scale]

  let rng meta dof location scale =
    stanlib_fun meta (distribution_prefix "rng") [dof; location; scale]
end

(** Cauchy Distribution *)
module Cauchy = struct
  let distribution_prefix suffix = "cauchy_" ^ suffix

  let lpdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; location; scale]

  let cdf meta y location scale =
    stanlib_fun meta (distribution_prefix "cdf") [y; location; scale]

  let lcdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; location; scale]

  let lccdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; location; scale]

  let rng meta location scale =
    stanlib_fun meta (distribution_prefix "rng") [location; scale]
end

(** Double Exponential (Laplace) Distribution *)
module Double_exponential = struct
  let distribution_prefix suffix = "double_exponential_" ^ suffix

  let lpdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; location; scale]

  let cdf meta y location scale =
    stanlib_fun meta (distribution_prefix "cdf") [y; location; scale]

  let lcdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; location; scale]

  let lccdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; location; scale]

  let rng meta location scale =
    stanlib_fun meta (distribution_prefix "rng") [location; scale]
end

(** Logistic Distribution *)
module Logistic = struct
  let distribution_prefix suffix = "logistic_" ^ suffix

  let lpdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; location; scale]

  let cdf meta y location scale =
    stanlib_fun meta (distribution_prefix "cdf") [y; location; scale]

  let lcdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; location; scale]

  let lccdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; location; scale]

  let rng meta location scale =
    stanlib_fun meta (distribution_prefix "rng") [location; scale]
end

(** Gumbel Distribution *)
module Gumbel = struct
  let distribution_prefix suffix = "gumbel_" ^ suffix

  let lpdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; location; scale]

  let cdf meta y location scale =
    stanlib_fun meta (distribution_prefix "cdf") [y; location; scale]

  let lcdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; location; scale]

  let lccdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; location; scale]

  let rng meta location scale =
    stanlib_fun meta (distribution_prefix "rng") [location; scale]
end

(* == Positive Continuous Distributions ===================================== *)

(** Lognormal Distribution *)
module Lognormal = struct
  let distribution_prefix suffix = "log_normal_" ^ suffix

  let lpdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; location; scale]

  let cdf meta y location scale =
    stanlib_fun meta (distribution_prefix "cdf") [y; location; scale]

  let lcdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; location; scale]

  let lccdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; location; scale]

  let rng meta location scale =
    stanlib_fun meta (distribution_prefix "rng") [location; scale]
end

(** Chi-Square Distribution *)
module Chi_square = struct
  let distribution_prefix suffix = "chi_square_" ^ suffix
  let lpdf meta y dof = stanlib_fun meta (distribution_prefix "lpdf") [y; dof]
  let cdf meta y dof = stanlib_fun meta (distribution_prefix "cdf") [y; dof]
  let lcdf meta y dof = stanlib_fun meta (distribution_prefix "lcdf") [y; dof]
  let lccdf meta y dof = stanlib_fun meta (distribution_prefix "lccdf") [y; dof]
  let rng meta dof = stanlib_fun meta (distribution_prefix "rng") [dof]
end

(** Inverse Chi-Square Distribution *)
module Inv_chi_square = struct
  let distribution_prefix suffix = "inv_chi_square_" ^ suffix
  let lpdf meta y dof = stanlib_fun meta (distribution_prefix "lpdf") [y; dof]
  let cdf meta y dof = stanlib_fun meta (distribution_prefix "cdf") [y; dof]
  let lcdf meta y dof = stanlib_fun meta (distribution_prefix "lcdf") [y; dof]
  let lccdf meta y dof = stanlib_fun meta (distribution_prefix "lccdf") [y; dof]
  let rng meta dof = stanlib_fun meta (distribution_prefix "rng") [dof]
end

(** Inverse Chi-Square Distribution *)
module Scaled_inv_chi_square = struct
  let distribution_prefix suffix = "scaled_inv_chi_square_" ^ suffix

  let lpdf meta y dof scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; dof; scale]

  let cdf meta y dof scale =
    stanlib_fun meta (distribution_prefix "cdf") [y; dof; scale]

  let lcdf meta y dof scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; dof; scale]

  let lccdf meta y dof scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; dof; scale]

  let rng meta dof scale =
    stanlib_fun meta (distribution_prefix "rng") [dof; scale]
end

(** Exponential Distribution *)
module Exponential = struct
  let distribution_prefix suffix = "exponential_" ^ suffix

  let lpdf meta y inv_scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; inv_scale]

  let cdf meta y inv_scale =
    stanlib_fun meta (distribution_prefix "cdf") [y; inv_scale]

  let lcdf meta y inv_scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; inv_scale]

  let lccdf meta y inv_scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; inv_scale]

  let rng meta inv_scale =
    stanlib_fun meta (distribution_prefix "rng") [inv_scale]
end

(** Gamma Distribution *)
module Gamma = struct
  let distribution_prefix suffix = "gamma_" ^ suffix

  let lpdf meta y shape inv_scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; shape; inv_scale]

  let cdf meta y shape inv_scale =
    stanlib_fun meta (distribution_prefix "cdf") [y; shape; inv_scale]

  let lcdf meta y shape inv_scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; shape; inv_scale]

  let lccdf meta y shape inv_scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; shape; inv_scale]

  let rng meta shape inv_scale =
    stanlib_fun meta (distribution_prefix "rng") [shape; inv_scale]
end

(** Inverse Gamma Distribution *)
module Inv_gamma = struct
  let distribution_prefix suffix = "inv_gamma_" ^ suffix

  let lpdf meta y shape scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; shape; scale]

  let cdf meta y shape scale =
    stanlib_fun meta (distribution_prefix "cdf") [y; shape; scale]

  let lcdf meta y shape scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; shape; scale]

  let lccdf meta y shape scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; shape; scale]

  let rng meta shape scale =
    stanlib_fun meta (distribution_prefix "rng") [shape; scale]
end

(** Weibull Distribution *)
module Weibull = struct
  let distribution_prefix suffix = "weibull_" ^ suffix

  let lpdf meta y shape scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; shape; scale]

  let cdf meta y shape scale =
    stanlib_fun meta (distribution_prefix "cdf") [y; shape; scale]

  let lcdf meta y shape scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; shape; scale]

  let lccdf meta y shape scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; shape; scale]

  let rng meta shape scale =
    stanlib_fun meta (distribution_prefix "rng") [shape; scale]
end

(** Frechet Distribution *)
module Frechet = struct
  let distribution_prefix suffix = "frechet_" ^ suffix

  let lpdf meta y shape scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; shape; scale]

  let cdf meta y shape scale =
    stanlib_fun meta (distribution_prefix "cdf") [y; shape; scale]

  let lcdf meta y shape scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; shape; scale]

  let lccdf meta y shape scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; shape; scale]

  let rng meta shape scale =
    stanlib_fun meta (distribution_prefix "rng") [shape; scale]
end

(* == Non-negative continuous distributions ================================= *)

(** Rayleigh Distribution *)
module Rayleigh = struct
  let distribution_prefix suffix = "rayleigh_" ^ suffix

  let lpdf meta y scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; scale]

  let cdf meta y scale = stanlib_fun meta (distribution_prefix "cdf") [y; scale]

  let lcdf meta y scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; scale]

  let lccdf meta y scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; scale]

  let rng meta scale = stanlib_fun meta (distribution_prefix "rng") [scale]
end

(** Wiener First Passage Time Distribution *)
module Wiener = struct
  let distribution_prefix suffix = "wiener_" ^ suffix

  let lpdf meta y alpha tau beta delta =
    stanlib_fun meta (distribution_prefix "lpdf") [y; alpha; tau; beta; delta]
end

(* == Positive lower-bounded ================================================ *)

(** Pareto Distribution *)
module Pareto = struct
  let distribution_prefix suffix = "pareto_" ^ suffix

  let lpdf meta y y_min shape =
    stanlib_fun meta (distribution_prefix "lpdf") [y; y_min; shape]

  let cdf meta y y_min shape =
    stanlib_fun meta (distribution_prefix "cdf") [y; y_min; shape]

  let lcdf meta y y_min shape =
    stanlib_fun meta (distribution_prefix "lcdf") [y; y_min; shape]

  let lccdf meta y y_min shape =
    stanlib_fun meta (distribution_prefix "lccdf") [y; y_min; shape]

  let rng meta y_min shape =
    stanlib_fun meta (distribution_prefix "rng") [y_min; shape]
end

(** Pareto Type 2 Distribution *)
module Pareto_type_2 = struct
  let distribution_prefix suffix = "pareto_type_2_" ^ suffix

  let lpdf meta y location scale shape =
    stanlib_fun meta (distribution_prefix "lpdf") [y; location; scale; shape]

  let cdf meta y location scale shape =
    stanlib_fun meta (distribution_prefix "cdf") [y; location; scale; shape]

  let lcdf meta y location scale shape =
    stanlib_fun meta (distribution_prefix "lcdf") [y; location; scale; shape]

  let lccdf meta y location scale shape =
    stanlib_fun meta (distribution_prefix "lccdf") [y; location; scale; shape]

  let rng meta location scale shape =
    stanlib_fun meta (distribution_prefix "rng") [location; scale; shape]
end

(* == Continuous on [0,1] =================================================== *)

(** Beta Distribution *)
module Beta = struct
  let distribution_prefix suffix = "beta_" ^ suffix

  let lpdf meta theta alpha beta =
    stanlib_fun meta (distribution_prefix "lpdf") [theta; alpha; beta]

  let cdf meta theta alpha beta =
    stanlib_fun meta (distribution_prefix "cdf") [theta; alpha; beta]

  let lcdf meta theta alpha beta =
    stanlib_fun meta (distribution_prefix "lcdf") [theta; alpha; beta]

  let lccdf meta theta alpha beta =
    stanlib_fun meta (distribution_prefix "lccdf") [theta; alpha; beta]

  let rng meta alpha beta =
    stanlib_fun meta (distribution_prefix "rng") [alpha; beta]
end

(** Beta Proportion Distribution *)
module Beta_proportion = struct
  let distribution_prefix suffix = "beta_proportion_" ^ suffix

  let lpdf meta theta mean precision =
    stanlib_fun meta (distribution_prefix "lpdf") [theta; mean; precision]

  let cdf meta theta mean precision =
    stanlib_fun meta (distribution_prefix "cdf") [theta; mean; precision]

  let lcdf meta theta mean precision =
    stanlib_fun meta (distribution_prefix "lcdf") [theta; mean; precision]

  let lccdf meta theta mean precision =
    stanlib_fun meta (distribution_prefix "lccdf") [theta; mean; precision]

  let rng meta mean precision =
    stanlib_fun meta (distribution_prefix "rng") [mean; precision]
end

(* == Circular ============================================================== *)

(** Von Mises Distribution *)
module Von_mises = struct
  let distribution_prefix suffix = "von_mises_" ^ suffix

  let lpdf meta y location scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; location; scale]
end

(* == Bounded continuous ==================================================== *)

(** Uniform Distribution *)
module Uniform = struct
  let distribution_prefix suffix = "uniform_" ^ suffix

  let lpdf meta y lower upper =
    stanlib_fun meta (distribution_prefix "lpdf") [y; lower; upper]

  let cdf meta y lower upper =
    stanlib_fun meta (distribution_prefix "cdf") [y; lower; upper]

  let lcdf meta y lower upper =
    stanlib_fun meta (distribution_prefix "lcdf") [y; lower; upper]

  let lccdf meta y lower upper =
    stanlib_fun meta (distribution_prefix "lccdf") [y; lower; upper]

  let rng meta lower upper =
    stanlib_fun meta (distribution_prefix "rng") [lower; upper]
end

(* == Distributions over unbounded vectors ================================== *)

(** Multivariate Normal Distribution, Precision Parameterization *)
module Multi_normal_prec = struct
  let distribution_prefix suffix = "multi_normal_prec_" ^ suffix

  let lpdf meta y mu omega =
    stanlib_fun meta (distribution_prefix "lpdf") [y; mu; omega]
end

(** Multivariate Normal Distribution, Cholesky Parameterization *)
module Multi_normal_cholesky = struct
  let distribution_prefix suffix = "multi_normal_cholesky_" ^ suffix

  let lpdf meta y mu omega =
    stanlib_fun meta (distribution_prefix "lpdf") [y; mu; omega]

  let rng meta mu omega =
    stanlib_fun meta (distribution_prefix "rng") [mu; omega]
end

(** Multivariate Normal Distribution *)
module Multi_normal = struct
  let distribution_prefix suffix = "multi_normal_" ^ suffix

  let lpdf meta y mu sigma =
    let to_trans tau = Multi_normal_prec.lpdf meta y mu tau
    and default =
      stanlib_fun meta (distribution_prefix "lpdf") [y; mu; sigma]
    in
    lpdf_trans_lpdf ~link:"inverse" to_trans sigma |> Option.value ~default

  let cdf meta y mu sigma =
    stanlib_fun meta (distribution_prefix "cdf") [y; mu; sigma]

  let lcdf meta y mu sigma =
    stanlib_fun meta (distribution_prefix "lcdf") [y; mu; sigma]

  let lccdf meta y mu sigma =
    stanlib_fun meta (distribution_prefix "lccdf") [y; mu; sigma]

  let rng meta mu sigma =
    stanlib_fun meta (distribution_prefix "rng") [mu; sigma]
end

(** Multivariate Student-T Distribution *)
module Multi_student_t = struct
  let distribution_prefix suffix = "multi_student_t_" ^ suffix

  let lpdf meta y dof location scale =
    stanlib_fun meta (distribution_prefix "lpdf") [y; dof; location; scale]

  let cdf meta y dof location scale =
    stanlib_fun meta (distribution_prefix "cdf") [y; dof; location; scale]

  let lcdf meta y dof location scale =
    stanlib_fun meta (distribution_prefix "lcdf") [y; dof; location; scale]

  let lccdf meta y dof location scale =
    stanlib_fun meta (distribution_prefix "lccdf") [y; dof; location; scale]

  let rng meta dof location scale =
    stanlib_fun meta (distribution_prefix "rng") [dof; location; scale]
end

(* == Simplex distributions ================================================= *)

(** Dirichlet Distribution *)
module Dirichlet = struct
  let distribution_prefix suffix = "dirichlet_" ^ suffix

  let lpdf meta theta alpha =
    stanlib_fun meta (distribution_prefix "lpdf") [theta; alpha]

  let rng meta alpha = stanlib_fun meta (distribution_prefix "rng") [alpha]
end

(* == Correlation matrix distributions ====================================== *)

(** LKJ Correlation Matrix Distribution, Cholesky PArameterization *)
module LKJ_corr_cholesky = struct
  let distribution_prefix suffix = "lkj_corr_cholesky_" ^ suffix
  let lpdf meta l eta = stanlib_fun meta (distribution_prefix "lpdf") [l; eta]
  let rng meta k eta = stanlib_fun meta (distribution_prefix "rng") [k; eta]
end

(** LKJ Correlation Matrix Distribution *)
module LKJ_corr = struct
  let distribution_prefix suffix = "lkj_corr_" ^ suffix
  let lpdf meta y eta = stanlib_fun meta (distribution_prefix "lpdf") [y; eta]
  let rng meta k eta = stanlib_fun meta (distribution_prefix "rng") [k; eta]
end

(* == Covariance Matrix Distributions ======================================= *)

module Wishart = struct
  let distribution_prefix suffix = "wishart_" ^ suffix

  let lpdf meta w dof sigma =
    stanlib_fun meta (distribution_prefix "lpdf") [w; dof; sigma]

  let rng meta dof sigma =
    stanlib_fun meta (distribution_prefix "rng") [dof; sigma]
end

module Inv_wishart = struct
  let distribution_prefix suffix = "inv_wishart_" ^ suffix

  let lpdf meta w dof sigma =
    stanlib_fun meta (distribution_prefix "lpdf") [w; dof; sigma]

  let rng meta dof sigma =
    stanlib_fun meta (distribution_prefix "rng") [dof; sigma]
end

(* == Partial evaluation  =================================================== *)

let eval_binop meta op e1 e2 =
  match op with
  | Operator.Pow -> pow meta e1 e2
  | Times -> times meta e1 e2
  | Plus -> plus meta e1 e2
  | Minus -> minus meta e1 e2
  | _ -> binop meta op e1 e2

let eval_stanlib_fun_app meta fn_name args =
  match (fn_name, args) with
  | "log", [x] -> sum meta x
  | "pow", [x; y] -> pow meta x y
  | "sum", [x] -> sum meta x
  | "square", [x] -> square meta x
  | "sqrt", [x] -> sqrt meta x
  | "inv", [x] -> inv meta x
  | "trace", [x] -> trace meta x
  | "dot_product", [x; y] -> dot_product meta x y
  | "rows_dot_product", [x; y] -> rows_dot_product meta x y
  | "columns_dot_product", [x; y] -> columns_dot_product meta x y
  | "bernoulli_lpmf", [y; theta] -> Bernoulli.lpmf meta y theta
  | "bernoulli_rng", [theta] -> Bernoulli.rng meta theta
  | "bernoulli_logit_lpmf", [y; theta] -> Bernoulli_logit.lpmf meta y theta
  | "categorical_lpmf", [y; theta] -> Categorical.lpmf meta y theta
  | "categorical_rng", [theta] -> Categorical.rng meta theta
  | "neg_binomial_2_lpmf", [a; b; c] -> Neg_binomial_2.lpmf meta a b c
  | "neg_binomial_2_log_lpmf", [a; b; c] -> Neg_binomial_2_log.lpmf meta a b c
  | "poisson_lpmf", [y; theta] -> Poisson.lpmf meta y theta
  | "poisson_rng", [theta] -> Poisson.rng meta theta
  | "poisson_log_lpmf", [y; theta] -> Poisson_log.lpmf meta y theta
  | "normal_lpdf", [y; mu; sigma] -> Normal.lpdf meta y mu sigma
  | "multi_normal_lpdf", [y; mu; sigma] -> Multi_normal.lpdf meta y mu sigma
  | _ -> stanlib_fun meta fn_name args

let arg_types xs =
  List.map ~f:(fun expr -> Typed.(adlevel_of expr, type_of expr)) xs

let return_type expr =
  match Fixed.pattern expr with
  | FunApp (StanLib, name, args)
    when Option.is_some (Operator.of_string_opt name) ->
      let op = Option.value_exn (Operator.of_string_opt name)
      and arg_tys = arg_types args in
      Stan_math.op_return_type op arg_tys
  | FunApp (StanLib, name, args) ->
      Stan_math.return_type name @@ arg_types args
  | _ -> None

let check_return_type e_orig e_simple =
  match return_type e_simple with None -> e_orig | _ -> e_simple

let eval ?(env = String.Map.empty) expr =
  let f expr =
    match Fixed.proj expr with
    | _, Var n -> String.Map.find env n |> Option.value ~default:expr
    | _, Lit _ -> expr
    | meta, TernaryIf (pred, e1, e2) -> if_ meta pred e1 e2
    | meta, EAnd (e1, e2) -> and_ meta e1 e2
    | meta, EOr (e1, e2) -> or_ meta e1 e2
    | _, Indexed _ -> expr
    | meta, FunApp (StanLib, fn_name, [x])
      when Option.is_some (Operator.of_string_opt fn_name) ->
        unop meta (Option.value_exn (Operator.of_string_opt fn_name)) x
        |> check_return_type expr
    | meta, FunApp (StanLib, fn_name, [x; y])
      when Option.is_some (Operator.of_string_opt fn_name) ->
        eval_binop meta (Option.value_exn (Operator.of_string_opt fn_name)) x y
        |> check_return_type expr
    | meta, FunApp (StanLib, fn_name, args) ->
        eval_stanlib_fun_app meta fn_name args |> check_return_type expr
    | _, FunApp _ -> expr
  in
  Fixed.transform_bottom_up f expr
