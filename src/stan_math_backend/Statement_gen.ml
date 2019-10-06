open Core_kernel
open Middle
open Fmt
open Expression_gen

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

let rec integer_el_type = function
  | SReal | SVector _ | SMatrix _ | SRowVector _ -> false
  | SInt -> true
  | SArray (st, _) -> integer_el_type st

let pp_decl ppf (vident, ut, adtype) =
  let pp_type =
    if Transform_Mir.is_opencl_var vident then fun ppf _ ->
      pf ppf "matrix_cl<double>"
    else pp_unsizedtype_local
  in
  pf ppf "%a %s;" pp_type (adtype, ut) vident

let pp_sized_decl ppf (vident, st, adtype) =
  pf ppf "%a@,%a" pp_decl
    (vident, remove_size st, adtype)
    pp_set_size (vident, st, adtype)

let pp_possibly_sized_decl ppf (vident, pst, adtype) =
  match pst with
  | Sized st -> pp_sized_decl ppf (vident, st, adtype)
  | Unsized ut -> pp_decl ppf (vident, ut, adtype)

let math_fn_translations = function
  | FnLength -> Some ("length", [])
  | _ -> None

let trans_math_fn fname =
  Option.(
    value ~default:(fname, [])
      (bind (internal_fn_of_string fname) ~f:math_fn_translations))

let rec pp_statement (ppf : Format.formatter)
    ({stmt; smeta} : (mtype_loc_ad, 'a) stmt_with) =
  let pp_stmt_list = list ~sep:cut pp_statement in
  ( match stmt with
  | Block _ | SList _ | Decl _ | Skip | Break | Continue -> ()
  | _ -> Locations.pp_smeta ppf smeta ) ;
  match stmt with
  | Assignment ((vident, _, []), ({emeta= {mtype= UInt; _}; _} as rhs))
   |Assignment ((vident, _, []), ({emeta= {mtype= UReal; _}; _} as rhs)) ->
      pf ppf "%s = %a;" vident pp_expr rhs
  | Assignment
      ((id, _, idcs), ({expr= FunApp (CompilerInternal, f, _); _} as rhs))
    when internal_fn_of_string f = Some FnMakeArray ->
      pf ppf "%a = @[<hov>%a;@]" pp_indexed_simple (id, idcs) pp_expr rhs
  | Assignment ((assignee, UInt, idcs), rhs)
   |Assignment ((assignee, UReal, idcs), rhs)
    when List.for_all ~f:is_single_index idcs ->
      pf ppf "%a = %a;" pp_indexed_simple (assignee, idcs) pp_expr rhs
  (* | Assignment ((assignee, ut, idcs), rhs)
   *   when List.for_all ~f:is_single_index idcs
   *        && not (is_indexing_matrix (ut, idcs)) ->
   *     pf ppf "%a = %a;" pp_indexed_simple (assignee, idcs) pp_expr rhs *)
  | Assignment ((assignee, _, idcs), rhs) ->
      (* XXX I think in general we don't need to do a deepcopy if e is nested
       inside some function call - the function should get its own copy
       (in all cases???) *)
      let rec maybe_deep_copy e =
        let recurse e = {e with expr= map_expr maybe_deep_copy e.expr} in
        match e with
        | {emeta= {mtype= UInt; _}; _} | {emeta= {mtype= UReal; _}; _} -> e
        | {expr= FunApp (CompilerInternal, _, _); _} -> e
        | ({expr= Indexed ({expr= Var v; _}, _); _} | {expr= Var v; _})
          when v = assignee ->
            { e with
              expr= FunApp (CompilerInternal, "stan::model::deep_copy", [e]) }
        | e -> recurse e
      in
      let rhs =
        match rhs.expr with
        | FunApp (CompilerInternal, f, _)
          when f = string_of_internal_fn FnConstrain
               || f = string_of_internal_fn FnUnconstrain ->
            rhs
        | _ -> maybe_deep_copy rhs
      in
      pf ppf "assign(@[<hov>%s, %a, %a, %S@]);" assignee pp_indexes idcs
        pp_expr rhs
        (strf "assigning variable %s"
           assignee
           (* (list ~sep:comma (Pretty.pp_index Pretty.pp_expr_typed_located)) idcs *))
  | TargetPE e -> pf ppf "lp_accum__.add(%a);" pp_expr e
  | NRFunApp (CompilerInternal, fname, args)
    when fname = string_of_internal_fn FnPrint ->
      let pp_arg ppf a = pf ppf "stan_print(pstream__, %a);" pp_expr a in
      let args = args @ [{expr= Lit (Str, "\n"); emeta= internal_meta}] in
      pf ppf "if (pstream__) %a" pp_block (list ~sep:cut pp_arg, args)
  | NRFunApp (CompilerInternal, fname, args)
    when fname = string_of_internal_fn FnReject ->
      let err_strm = "errmsg_stream__" in
      let add_to_string ppf e = pf ppf "%s << %a;" err_strm pp_expr e in
      pf ppf "std::stringstream %s;@," err_strm ;
      pf ppf "%a@," (list ~sep:cut add_to_string) args ;
      pf ppf "throw std::domain_error(%s.str());" err_strm
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
      pf ppf "%a;" pp_user_defined_fun (fname, args)
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
