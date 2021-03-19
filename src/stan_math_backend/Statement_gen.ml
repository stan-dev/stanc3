open Core_kernel
open Middle
open Fmt
open Expression_gen

let pp_call_str ppf (name, args) = pp_call ppf (name, string, args)
let pp_block ppf (pp_body, body) = pf ppf "{@;<1 2>@[<v>%a@]@,}" pp_body body

let pp_profile ppf (pp_body, name, body) =
  let profile =
    Fmt.strf
      "profile<local_scalar_t__> profile__(%s, \
       const_cast<profile_map&>(profiles__));"
      name
  in
  pf ppf "{@;<1 2>@[<v>%s@;@;%a@]@,}" profile pp_body body

let rec contains_eigen = function
  | UnsizedType.UArray t -> contains_eigen t
  | UMatrix | URowVector | UVector -> true
  | UInt | UReal | UMathLibraryFunction | UFun _ -> false

let pp_set_size ppf (decl_id, st, adtype) =
  (* TODO: generate optimal adtypes for expressions and declarations *)
  let real_nan =
    match adtype with
    | UnsizedType.AutoDiffable -> "DUMMY_VAR__"
    | DataOnly -> "std::numeric_limits<double>::quiet_NaN()"
  in
  let rec pp_size_ctor ppf st =
    let pp_st ppf st =
      pf ppf "%a" pp_unsizedtype_local (adtype, SizedType.to_unsized st)
    in
    match st with
    | SizedType.SInt -> pf ppf "std::numeric_limits<int>::min()"
    | SReal -> pf ppf "%s" real_nan
    | SVector d | SRowVector d -> pf ppf "%a(%a)" pp_st st pp_expr d
    | SMatrix (d1, d2) -> pf ppf "%a(%a, %a)" pp_st st pp_expr d1 pp_expr d2
    | SArray (t, d) -> pf ppf "%a(%a, %a)" pp_st st pp_expr d pp_size_ctor t
  in
  pf ppf "@[<hov 2>%s = %a;@]@," decl_id pp_size_ctor st ;
  if contains_eigen (SizedType.to_unsized st) then
    pf ppf "@[<hov 2>stan::math::fill(%s, %s);@]@," decl_id real_nan

let%expect_test "set size mat array" =
  let int = Expr.Helpers.int in
  strf "@[<v>%a@]" pp_set_size
    ("d", SArray (SArray (SMatrix (int 2, int 3), int 4), int 5), DataOnly)
  |> print_endline ;
  [%expect
    {|
      d = std::vector<std::vector<Eigen::Matrix<double, -1, -1>>>(5, std::vector<Eigen::Matrix<double, -1, -1>>(4, Eigen::Matrix<double, -1, -1>(2, 3)));
      stan::math::fill(d, std::numeric_limits<double>::quiet_NaN()); |}]

(** [pp_for_loop ppf (loopvar, lower, upper, pp_body, body)] tries to
    pretty print a for-loop from lower to upper given some loopvar.*)
let pp_for_loop ppf (loopvar, lower, upper, pp_body, body) =
  pf ppf "@[<hov>for (@[<hov>int %s = %a;@ %s <= %a;@ ++%s@])" loopvar pp_expr
    lower loopvar pp_expr upper loopvar ;
  pf ppf " %a@]" pp_body body

let rec integer_el_type = function
  | SizedType.SReal | SVector _ | SMatrix _ | SRowVector _ -> false
  | SInt -> true
  | SArray (st, _) -> integer_el_type st

let pp_decl ppf (vident, ut, adtype) =
  let pp_type =
    match (Transform_Mir.is_opencl_var vident, ut) with
    | _, UnsizedType.(UInt | UReal) | false, _ -> pp_unsizedtype_local
    | true, UArray UInt -> fun ppf _ -> pf ppf "matrix_cl<int>"
    | true, _ -> fun ppf _ -> pf ppf "matrix_cl<double>"
  in
  pf ppf "%a %s;" pp_type (adtype, ut) vident

let pp_sized_decl ppf (vident, st, adtype) =
  pf ppf "%a@,%a" pp_decl
    (vident, SizedType.to_unsized st, adtype)
    pp_set_size (vident, st, adtype)

let pp_possibly_sized_decl ppf (vident, pst, adtype) =
  match pst with
  | Type.Sized st -> pp_sized_decl ppf (vident, st, adtype)
  | Unsized ut -> pp_decl ppf (vident, ut, adtype)

let math_fn_translations = function
  | Internal_fun.FnLength -> Some ("length", [])
  | FnValidateSize -> Some ("validate_non_negative_index", [])
  | FnValidateSizeSimplex -> Some ("validate_positive_index", [])
  | FnValidateSizeUnitVector -> Some ("validate_unit_vector_index", [])
  | _ -> None

let trans_math_fn fname =
  Option.(
    value ~default:(fname, [])
      (bind (Internal_fun.of_string_opt fname) ~f:math_fn_translations))

let pp_bool_expr ppf expr =
  match Expr.Typed.type_of expr with
  | UReal -> pp_call ppf ("as_bool", pp_expr, [expr])
  | _ -> pp_expr ppf expr

let rec pp_statement (ppf : Format.formatter)
    (Stmt.Fixed.({pattern; meta}) as stmt) =
  (* ({stmt; smeta} : (mtype_loc_ad, 'a) stmt_with) = *)
  let pp_stmt_list = list ~sep:cut pp_statement in
  ( match pattern with
  | Block _ | SList _ | Decl _ | Skip | Break | Continue -> ()
  | _ -> Locations.pp_smeta ppf meta ) ;
  match pattern with
  | Assignment
      ((vident, _, []), ({pattern= FunApp (CompilerInternal, f, _); _} as rhs))
    when f = Internal_fun.to_string FnReadData
         || f = Internal_fun.to_string FnReadParam ->
      pf ppf "@[<hov 4>%s = %a;@]" vident pp_expr rhs
  | Assignment
      ((vident, _, []), ({meta= Expr.Typed.Meta.({type_= UInt; _}); _} as rhs))
   |Assignment ((vident, _, []), ({meta= {type_= UReal; _}; _} as rhs)) ->
      pf ppf "@[<hov 4>%s = %a;@]" vident pp_expr rhs
  | Assignment ((assignee, UInt, idcs), rhs)
   |Assignment ((assignee, UReal, idcs), rhs)
    when List.for_all ~f:is_single_index idcs ->
      pf ppf "@[<hov 4>%a = %a;@]" pp_indexed_simple (assignee, idcs) pp_expr
        rhs
  | Assignment ((assignee, _, idcs), rhs) ->
      (* XXX I think in general we don't need to do a deepcopy if e is nested
       inside some function call - the function should get its own copy
       (in all cases???) *)
      let rec maybe_deep_copy e =
        let recurse (e : 'a Expr.Fixed.t) =
          { e with
            Expr.Fixed.pattern=
              Expr.Fixed.Pattern.map maybe_deep_copy e.pattern }
        in
        match e.pattern with
        | _ when UnsizedType.is_scalar_type (Expr.Typed.type_of e) -> e
        | FunApp (CompilerInternal, _, _) -> e
        | (Indexed ({Expr.Fixed.pattern= Var v; _}, _) | Var v)
          when v = assignee ->
            { e with
              Expr.Fixed.pattern=
                FunApp (CompilerInternal, "stan::model::deep_copy", [e]) }
        | _ -> recurse e
      in
      let rhs =
        match rhs.pattern with
        | FunApp (CompilerInternal, f, _)
          when f = Internal_fun.to_string FnConstrain
               || f = Internal_fun.to_string FnUnconstrain ->
            rhs
        | _ -> maybe_deep_copy rhs
      in
      pf ppf "@[<hov 2>assign(@,%s,@ %a,@ %S%s%a@]);" assignee pp_expr rhs
        (strf "assigning variable %s" assignee)
        (if List.length idcs = 0 then "" else ", ")
        pp_indexes idcs
  | TargetPE e -> pf ppf "@[<hov 2>lp_accum__.add(@,%a@]);" pp_expr e
  | NRFunApp (CompilerInternal, fname, args)
    when fname = Internal_fun.to_string FnPrint ->
      let pp_arg ppf a = pf ppf "stan_print(pstream__, %a);" pp_expr a in
      let args = args @ [Expr.Helpers.str "\n"] in
      pf ppf "if (pstream__) %a" pp_block (list ~sep:cut pp_arg, args)
  | NRFunApp (CompilerInternal, fname, args)
    when fname = Internal_fun.to_string FnReject ->
      let err_strm = "errmsg_stream__" in
      let add_to_string ppf e = pf ppf "%s << %a;" err_strm pp_expr e in
      pf ppf "std::stringstream %s;@," err_strm ;
      pf ppf "%a@," (list ~sep:cut add_to_string) args ;
      pf ppf "throw std::domain_error(%s.str());" err_strm
  | NRFunApp
      (CompilerInternal, fname, {pattern= Lit (Str, check_name); _} :: args)
    when fname = Internal_fun.to_string FnCheck ->
      let args =
        {Expr.Fixed.pattern= Var "function__"; meta= Expr.Typed.Meta.empty}
        :: args
      in
      pp_statement ppf
        { pattern= NRFunApp (CompilerInternal, "check_" ^ check_name, args)
        ; meta= stmt.meta }
  | NRFunApp (CompilerInternal, fname, [var])
    when fname = Internal_fun.to_string FnWriteParam ->
      pf ppf "@[<hov 2>vars__.emplace_back(@,%a);@]" pp_expr var
  | NRFunApp (CompilerInternal, fname, args) ->
      let fname, extra_args = trans_math_fn fname in
      pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr)
        (extra_args @ args)
  | NRFunApp (StanLib, fname, args) ->
      pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr) args
  | NRFunApp (UserDefined, fname, args) ->
      pf ppf "%a;" pp_user_defined_fun (fname, args)
  | Break -> string ppf "break;"
  | Continue -> string ppf "continue;"
  | Return e -> pf ppf "@[<hov 4>return %a;@]" (option pp_expr) e
  | Skip -> string ppf ";"
  | IfElse (cond, ifbranch, elsebranch) ->
      let pp_else ppf x = pf ppf "else %a" pp_statement x in
      pf ppf "if (@[<hov>%a@]) %a %a" pp_bool_expr cond pp_block_s ifbranch
        (option pp_else) elsebranch
  | While (cond, body) ->
      pf ppf "while (@[<hov>%a@]) %a" pp_bool_expr cond pp_block_s body
  | For
      { body=
          { pattern=
              Assignment (_, {pattern= FunApp (CompilerInternal, f, _); _}); _
          } as body; _ }
    when Internal_fun.of_string_opt f = Some FnReadParam ->
      pp_statement ppf body
      (* Skip For loop part, just emit body due to the way FnReadParam emits *)
  | For {loopvar; lower; upper; body} ->
      pp_for_loop ppf (loopvar, lower, upper, pp_statement, body)
  | Profile (name, ls) -> pp_profile ppf (pp_stmt_list, name, ls)
  | Block ls -> pp_block ppf (pp_stmt_list, ls)
  | SList ls -> pp_stmt_list ppf ls
  | Decl {decl_adtype; decl_id; decl_type} ->
      pp_possibly_sized_decl ppf (decl_id, decl_type, decl_adtype)

and pp_block_s ppf body =
  match body.pattern with
  | Block ls -> pp_block ppf (list ~sep:cut pp_statement, ls)
  | _ -> pp_block ppf (pp_statement, body)
