open Core_kernel
open Middle
open Fmt
open Expression_gen
open Expr_helpers


let pp_call ppf (name, pp_arg, args) =
  pf ppf "%s(@[<hov>%a@])" name (list ~sep:comma pp_arg) args

let pp_call_str ppf (name, args) = pp_call ppf (name, string, args)
let pp_block ppf (pp_body, body) = pf ppf "{@;<1 2>@[<v>%a@]@,}" pp_body body

let pp_set_size ppf (decl_id, st, adtype) =
  (* TODO: generate optimal adtypes for expressions and declarations *)
  let rec pp_size_ctor ppf st =
    let pp_st ppf st =
      pf ppf "%a" pp_unsizedtype_local (adtype, SizedType.to_unsizedtype st)
    in
    match st with
    | SizedType.SInt | SReal -> pf ppf "0"
    | SVector d | SRowVector d -> pf ppf "%a(%a)" pp_st st pp_expr d
    | SMatrix (d1, d2) -> pf ppf "%a(%a, %a)" pp_st st pp_expr d1 pp_expr d2
    | SArray (t, d) -> pf ppf "%a(%a, %a)" pp_st st pp_expr d pp_size_ctor t
  in
  match st with
  | SizedType.SInt | SReal -> ()
  | st -> pf ppf "%s = %a;@," decl_id pp_size_ctor st

let%expect_test "set size mat array" =
  let int i = lit_int Expr.Typed.Meta.empty i in
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
  | SizedType.SReal | SVector _ | SMatrix _ | SRowVector _ -> false
  | SInt -> true
  | SArray (st, _) -> integer_el_type st

let pp_decl ppf (vident, ut, adtype) =
  pf ppf "%a %s;" pp_unsizedtype_local (adtype, ut) vident

let pp_sized_decl ppf (vident, st, adtype) =
  pf ppf "%a@,%a" pp_decl
    (vident, SizedType.to_unsizedtype st, adtype)
    pp_set_size (vident, st, adtype)

let pp_possibly_sized_decl ppf (vident, pst, adtype) =
  match pst with
  | Type.Sized st -> pp_sized_decl ppf (vident, st, adtype)
  | Unsized ut -> pp_decl ppf (vident, ut, adtype)

let math_fn_translations = function
  | Internal_fun.FnLength -> Some ("length", [])
  | _ -> None

let trans_math_fn fname =
  Option.(
    value ~default:(fname, [])
      (bind (Internal_fun.of_string_opt fname) ~f:math_fn_translations))

let body_is_read_param stmt =
  match Stmt.Fixed.pattern stmt with
  | Assignment (_, rhs)
    when is_fun ~name:(Internal_fun.to_string FnReadParam) rhs ->
      true
  | _ -> false

let rec maybe_deep_copy assignee e =
  let recurse e =
    let meta = Expr.Fixed.meta e and pattern = Expr.Fixed.pattern e in
    Expr.Fixed.Pattern.map (maybe_deep_copy assignee) pattern
    |> Expr.Fixed.fix meta
  in
  match (Expr.Fixed.pattern e, Expr.Typed.type_of e) with
  | _, UInt | _, UReal -> recurse e
  | Var v, _ when v = assignee ->
      let meta = Expr.Fixed.meta e in
      Expr_helpers.fun_app meta CompilerInternal "stan::model::deep_copy" [e]
  | _ -> recurse e

let rec pp_statement (ppf : Format.formatter) stmt =
  let pattern = Stmt.Fixed.pattern stmt and meta = Stmt.Fixed.meta stmt in
  ( match pattern with
  | Block _ | SList _ | Decl _ | Skip | Break | Continue -> ()
  | _ -> Locations.pp_smeta ppf meta ) ;
  match pattern with
  | Assignment ((assignee, idxs), rhs) -> pp_assignment ppf assignee idxs rhs
  | TargetPE e -> pf ppf "lp_accum__.add(%a);" pp_expr e
  | NRFunApp (CompilerInternal, fname, args) ->
      pp_internal_fun ppf meta fname args
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
  | For {body; _} when body_is_read_param body ->
      pp_statement ppf body
      (* Skip For loop part, just emit body due to the way FnReadParam emits *)
  | For {loopvar; lower; upper; body} ->
      pp_for_loop ppf (loopvar, lower, upper, pp_statement, body)
  | Block ls -> pp_block ppf (pp_stmt_list, ls)
  | SList ls -> pp_stmt_list ppf ls
  | Decl {decl_adtype; decl_id; decl_type} ->
      pp_possibly_sized_decl ppf (decl_id, decl_type, decl_adtype)

and pp_stmt_list ppf xs = Fmt.(list ~sep:cut pp_statement) ppf xs

and pp_block_s ppf body =
  match Stmt.Fixed.pattern body with
  | Block ls -> pp_block ppf (list ~sep:cut pp_statement, ls)
  | _ -> pp_block ppf (pp_statement, body)

and pp_assignment ppf assignee idxs rhs =
  match idxs with
  | [] when Expr.Typed.type_of rhs = UInt || Expr.Typed.type_of rhs = UReal ->
      pf ppf "%s = %a;" assignee pp_expr rhs
  | _ -> (
    match Expr.Fixed.pattern rhs with
    | FunApp (CompilerInternal, name, _)
      when name = Internal_fun.to_string FnMakeArray ->
        pf ppf "%a = @[<hov>%a;@]" pp_indexed_simple (assignee, idxs) pp_expr
          rhs
    | FunApp (CompilerInternal, name, _)
      when name = Internal_fun.to_string FnConstrain
           || name = Internal_fun.to_string FnUnconstrain ->
        pf ppf "assign(@[<hov>%s, %a, %a, %S@]);" assignee pp_indexes idxs
          pp_expr rhs
          (strf "assigning variable %a" pp_indexed_simple (assignee, idxs))
    | _ ->
        pf ppf "assign(@[<hov>%s, %a, %a, %S@]);" assignee pp_indexes idxs
          pp_expr
          (maybe_deep_copy assignee rhs)
          (strf "assigning variable %a" pp_indexed_simple (assignee, idxs)) )

and pp_internal_fun ppf meta fname args =
  match Internal_fun.of_string_opt fname with
  | Some FnPrint ->
      let pp_arg ppf a = pf ppf "stan_print(pstream__, %a);" pp_expr a in
      let new_arg = lit_string Expr.Typed.Meta.empty "\n" in
      let args = args @ [new_arg] in
      pf ppf "if (pstream__) %a" pp_block (list ~sep:cut pp_arg, args)
  | Some FnReject ->
      let err_strm = "errmsg_stream__" in
      let add_to_string ppf e = pf ppf "%s << %a;" err_strm pp_expr e in
      pf ppf "std::stringstream %s;@," err_strm ;
      pf ppf "%a@," (list ~sep:cut add_to_string) args ;
      pf ppf "throw std::domain_error(%s.str());" err_strm
  | Some FnCheck
    when Option.value_map ~default:false
           ~f:(is_lit ~type_:Str)
           (List.hd args) ->
      (* Both of these are safe since we have checked that the arguments list is 
        non-empty and that the first element is a string literal *)
      let first_arg = List.hd_exn args in
      let check_name =
        match Expr.Fixed.pattern first_arg with
        | Lit (_, str) -> str
        | _ -> failwith "Can't happen"
      in
      let rest_args = List.tl args |> Option.value ~default:[] in
      let new_arg = var Expr.Typed.Meta.empty "function__" in
      let new_args = new_arg :: rest_args in
      let stmt =
        Stmt_helpers.nrfun_app meta CompilerInternal ("check_" ^ check_name) new_args
      in
      pp_statement ppf stmt
  | Some FnWriteParam when List.length args = 1 ->
      let var = List.hd_exn args in
      pf ppf "vars__.push_back(@[<hov>%a@]);" pp_expr var
  | _ ->
      let fname, extra_args = trans_math_fn fname in
      pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr)
        (extra_args @ args)
