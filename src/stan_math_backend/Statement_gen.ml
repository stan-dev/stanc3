open Core_kernel
open Middle
open Fmt
open Expression_gen

let pp_call ppf (name, pp_arg, args) =
  pf ppf "%s(@[<hov>%a@])" name (list ~sep:comma pp_arg) args

let pp_call_str ppf (name, args) = pp_call ppf (name, string, args)
let pp_block ppf (pp_body, body) = pf ppf "{@;<1 2>@[<v>%a@]@,}" pp_body body

let pp_set_size ppf (decl_id, st, adtype) =
  (* TODO: generate optimal adtypes for expressions and declarations *)
  let rec pp_size_ctor ppf st =
    let pp_st ppf st =
      pf ppf "%a" pp_unsizedtype_local (adtype, remove_size st)
    in
    match st with
    | SInt | SReal -> pf ppf "0"
    | SVector d | SRowVector d -> pf ppf "%a(%a)" pp_st st pp_expr d
    | SMatrix (d1, d2) -> pf ppf "%a(%a, %a)" pp_st st pp_expr d1 pp_expr d2
    | SArray (t, d) -> pf ppf "%a(%a, %a)" pp_st st pp_expr d pp_size_ctor t
  in
  match st with
  | SInt | SReal -> ()
  | st -> pf ppf "%s = %a;@," decl_id pp_size_ctor st

let%expect_test "set size mat array" =
  let int i = {expr= Lit (Int, string_of_int i); emeta= internal_meta} in
  strf "@[<v>%a@]" pp_set_size
    ("d", SArray (SArray (SMatrix (int 2, int 3), int 4), int 5), DataOnly)
  |> print_endline ;
  [%expect
    {| d = std::vector<std::vector<Eigen::Matrix<double, -1, -1>>>(5, std::vector<Eigen::Matrix<double, -1, -1>>(4, Eigen::Matrix<double, -1, -1>(2, 3))); |}]

(** [pp_for_loop ppf (loopvar, lower, upper, pp_body, body)] tries to
    pretty print a for-loop from lower to upper given some loopvar.*)
let pp_for_loop ppf (loopvar, lower, upper, pp_body, body) =
  pf ppf "@[<hov>for (@[<hov>size_t %s = %a;@ %s <= %a;@ ++%s@])" loopvar
    pp_expr lower loopvar pp_expr upper loopvar ;
  pf ppf " %a@]" pp_body body

let rec pp_for_loop_iteratee ?(index_ids = []) ppf (iteratee, st, pp_body) =
  let iter d pp_body =
    let loopvar, gensym_exit = gensym_enter () in
    pp_for_loop ppf
      ( loopvar
      , loop_bottom
      , d
      , pp_block
      , (pp_body, (iteratee, loopvar :: index_ids)) ) ;
    gensym_exit ()
  in
  match st with
  | SReal | SInt -> pp_body ppf (iteratee, index_ids)
  | SRowVector d | SVector d -> iter d pp_body
  | SMatrix (d1, d2) ->
      iter
        { expr= FunApp (StanLib, string_of_operator Times, [d1; d2])
        ; emeta= internal_meta }
        pp_body
  | SArray (t, d) ->
      iter d (fun ppf (i, idcs) ->
          pf ppf "%a" pp_block
            (pp_for_loop_iteratee ~index_ids:idcs, (i, t, pp_body)) )

let rec integer_el_type = function
  | SReal | SVector _ | SMatrix _ | SRowVector _ -> false
  | SInt -> true
  | SArray (st, _) -> integer_el_type st

let pp_decl ppf (vident, ut, adtype) =
  pf ppf "%a %s;" pp_unsizedtype_local (adtype, ut) vident

let pp_sized_decl ppf (vident, st, adtype) =
  pf ppf "%a@,%a" pp_decl
    (vident, remove_size st, adtype)
    pp_set_size (vident, st, adtype)

let pp_possibly_sized_decl ppf (vident, pst, adtype) =
  match pst with
  | Sized st -> pp_sized_decl ppf (vident, st, adtype)
  | Unsized ut -> pp_decl ppf (vident, ut, adtype)

(*
  pf ppf "@ try %a" pp_body_block body ;
  string ppf " catch (const std::exception& e) " ;
  pp_block ppf (pp_located_msg, err_msg)
 *)

let math_fn_translations = function
  | FnPrint ->
      Some ("stan_print", [{expr= Var "pstream__"; emeta= internal_meta}])
  | FnLength -> Some ("length", [])
  | _ -> None

let trans_math_fn fname =
  Option.(
    value ~default:(fname, [])
      (bind (internal_fn_of_string fname) ~f:math_fn_translations))

let rec pp_statement (ppf : Format.formatter)
    ({stmt; smeta} : (mtype_loc_ad, 'a) stmt_with) =
  let pp_stmt_list = list ~sep:cut pp_statement in
  match stmt with
  | Assignment (lhs, {expr= FunApp (CompilerInternal, f, _) as expr; emeta})
    when internal_fn_of_string f = Some FnReadData ->
      let with_vestigial_idx =
        {expr= Indexed ({expr; emeta}, [Single loop_bottom]); emeta}
      in
      pp_statement ppf {stmt= Assignment (lhs, with_vestigial_idx); smeta}
      (* XXX In stan2 this often generates:
                stan::model::assign(theta,
                            stan::model::cons_list(stan::model::index_uni(j), stan::model::nil_index_list()),
                            (mu + (tau * get_base1(theta_tilde,j,"theta_tilde",1))),
                            "assigning variable theta");
            }
        *)
  | Assignment (lhs, {expr= Lit (Str, s); _}) ->
      pf ppf "%a = %S;" pp_indexed_simple lhs s
  | Assignment (lhs, {expr= Lit (_, s); _}) ->
      pf ppf "%a = %s;" pp_indexed_simple lhs s
  | Assignment (lhs, ({expr= FunApp (CompilerInternal, f, _); _} as rhs))
    when internal_fn_of_string f = Some FnMakeArray ->
      pf ppf "%a = @[<hov>%a;@]" pp_indexed_simple lhs pp_expr rhs
  | Assignment ((assignee, idcs), rhs) ->
      let rec maybe_deep_copy vident = function
        | {expr= Var v; _} as e when v = vident ->
            { e with
              expr= FunApp (CompilerInternal, "stan::model::deep_copy", [e]) }
        | {expr; emeta} -> {expr= map_expr (maybe_deep_copy vident) expr; emeta}
      in
      let rhs =
        match rhs.expr with
        | FunApp (CompilerInternal, f, _)
          when f = string_of_internal_fn FnConstrain
               || f = string_of_internal_fn FnUnconstrain ->
            rhs
        | _ -> maybe_deep_copy assignee rhs
      in
      pf ppf "assign(@[<hov>%s, %a, %a, %S@]);" assignee pp_indexes idcs
        pp_expr rhs
        (strf "assigning variable %a" pp_indexed_simple (assignee, idcs))
  | TargetPE e -> pf ppf "lp_accum__.add(%a);" pp_expr e
  | NRFunApp (CompilerInternal, fname, {expr= Lit (Str, check_name); _} :: args)
    when fname = string_of_internal_fn FnCheck ->
      let args = {expr= Var "function__"; emeta= internal_meta} :: args in
      pp_statement ppf
        {stmt= NRFunApp (CompilerInternal, "check_" ^ check_name, args); smeta}
  | NRFunApp (CompilerInternal, fname, [var])
    when fname = string_of_internal_fn FnWriteParam ->
      pf ppf "vars__.push_back(@[<hov>%a@]);" pp_expr var
  | NRFunApp (CompilerInternal, fname, args) ->
      let fname, extra_args = trans_math_fn fname in
      pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr)
        (extra_args @ args)
  | NRFunApp (StanLib, fname, args) ->
      pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr) args
  | NRFunApp (UserDefined, fname, args) ->
      pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr) args
  | Break -> string ppf "break;"
  | Continue -> string ppf "continue;"
  | Return e -> pf ppf "return %a;" (option pp_expr) e
  | Skip -> ()
  | IfElse (cond, ifbranch, elsebranch) ->
      let pp_else ppf x = pf ppf "else %a" pp_statement x in
      pf ppf "if (@[<hov>%a@]) %a %a" pp_expr cond pp_block_s ifbranch
        (option pp_else) elsebranch
  | While (cond, body) ->
      pf ppf "while (@[<hov>%a@]) %a" pp_expr cond pp_block_s body
  | For
      { body=
          {stmt= Assignment (_, {expr= FunApp (CompilerInternal, f, _); _}); _}
          as body; _ }
    when internal_fn_of_string f = Some FnReadParam ->
      pp_statement ppf body
      (* Skip For loop part, just emit body due to the way FnReadParam emits *)
  | For {loopvar; lower; upper; body} ->
      pp_for_loop ppf (loopvar, lower, upper, pp_statement, body)
  | Block ls -> pp_block ppf (pp_stmt_list, ls)
  | SList ls -> pp_stmt_list ppf ls
  | Decl {decl_adtype; decl_id; decl_type} ->
      pp_possibly_sized_decl ppf (decl_id, decl_type, decl_adtype)

and pp_block_s ppf body =
  match body.stmt with
  | Block ls -> pp_block ppf (list ~sep:cut pp_statement, ls)
  | _ -> pp_block ppf (pp_statement, body)
