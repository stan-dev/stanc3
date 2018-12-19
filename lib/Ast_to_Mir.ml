open Mir
open Core_kernel

let rec trans_expr {Ast.expr_typed; expr_typed_type= t; _} =
  match expr_typed with
  | Ast.TernaryIf (cond, ifb, elseb) ->
      TernaryIf (trans_expr cond, trans_expr ifb, trans_expr elseb)
  | Ast.BinOp (lhs, op, rhs) -> BinOp (trans_expr lhs, op, trans_expr rhs)
  | Ast.PrefixOp (op, e) | Ast.PostfixOp (e, op) ->
      FnApp (Operators.operator_name op, [trans_expr e])
  | Ast.Variable {name; _} -> Var name
  | Ast.IntNumeral x -> Lit (Int, x)
  | Ast.RealNumeral x -> Lit (Real, x)
  | Ast.FunApp ({name; _}, args) | Ast.CondDistApp ({name; _}, args) ->
      FnApp (name, List.map ~f:trans_expr args)
  | Ast.GetLP | Ast.GetTarget -> Var "target"
  | Ast.ArrayExpr eles -> FnApp ("make_array", List.map ~f:trans_expr eles)
  | Ast.RowVectorExpr eles -> FnApp ("make_rowvec", List.map ~f:trans_expr eles)
  | Ast.Paren x -> trans_expr x
  | Ast.Indexed (lhs, indices) ->
      Indexed (trans_expr lhs, List.map ~f:trans_idx indices)

and trans_idx = function
  | Ast.All -> All
  | Ast.Upfrom e -> Upfrom (trans_expr e)
  | Ast.Downfrom e -> Downfrom (trans_expr e)
  | Ast.Between (lb, ub) -> Between (trans_expr lb, trans_expr ub)
  | Ast.Single e -> (
    match e.expr_typed_type with
    | Ast.Int -> Single (trans_expr e)
    | Ast.Array _ -> MultiIndex (trans_expr e)
    | _ ->
        raise_s
          [%message
            "Expecting int or array" (e.expr_typed_type : Ast.unsizedtype)] )

and trans_unsizedtype = function
  | Ast.Int -> SInt
  | Ast.Real -> SReal
  | Ast.Vector -> SVector None
  | Ast.RowVector -> SRowVector None
  | Ast.Matrix -> SMatrix None
  | Ast.Array st -> SArray (None, trans_unsizedtype st)
  | Ast.Fun (_, _) ->
      raise_s [%message "Shouldn't need to convert function type"]
  | Ast.MathLibraryFunction ->
      raise_s [%message "Shouldn't need to convert Math library function type"]

and trans_sizedtype = function
  | Ast.SInt -> SInt
  | Ast.SReal -> SReal
  | Ast.SVector s -> SVector (Some (trans_expr s))
  | Ast.SRowVector s -> SRowVector (Some (trans_expr s))
  | Ast.SMatrix (rows, cols) ->
      SMatrix (Some (trans_expr rows, trans_expr cols))
  | Ast.SArray (st, s) -> SArray (Some (trans_expr s), trans_sizedtype st)

and trans_stmt {Ast.stmt_typed; stmt_typed_loc; _} =
  let targetpe e =
    let t = Var "target" in
    Assignment (t, BinOp (t, Plus, e))
  and pos_inf = FnApp("positive_infinity", [])
  and neg_inf = FnApp("negative_infinity", [])
  in
  let s =
    match stmt_typed with
    | Ast.Assignment {assign_indices; assign_rhs; assign_identifier; assign_op}
      ->
        let assignee = Var assign_identifier.name in
        let assignee =
          match assign_indices with
          | [] -> assignee
          | lst -> Indexed (assignee, List.map ~f:trans_idx lst)
        and rhs = trans_expr assign_rhs in
        let rhs =
          match assign_op with
          | Ast.Assign | Ast.ArrowAssign -> rhs
          | Ast.OperatorAssign op ->
              FnApp (Operators.operator_name op, [assignee; rhs])
        in
        Assignment (assignee, rhs)
    | Ast.NRFunApp ({name; _}, args) ->
        NRFnApp (name, List.map ~f:trans_expr args)
    | Ast.IncrementLogProb e | Ast.TargetPE e -> targetpe (trans_expr e)
    | Ast.Tilde {arg; distribution; args; truncation} ->
      let full_dist = FnApp (distribution.name, List.map ~f:trans_expr (arg :: args))
      in
      let full_dist = (match truncation with
          | Ast.NoTruncate -> full_dist
          | Ast.TruncateUpFrom _ -> (??)
          | Ast.TruncateDownFrom _ -> (??)
          | Ast.TruncateBetween (_, _) -> (??))


        (* XXX truncation: add
            if (0.5 < 0.1) lp_accum__.add(-std::numeric_limits<double>::infinity());
            else if (0.5 > 1.1) lp_accum__.add(-std::numeric_limits<double>::infinity());
            else lp_accum__.add(-log_diff_exp(normal_cdf_log(1.1, 0, 1), normal_cdf_log(0.1, 0, 1)));
        *)
        (* XXX distribution name suffix? *)
        targetpe

    | Ast.Print ps -> NRFnApp ("print", List.map ~f:trans_printable ps)
    | Ast.Reject ps -> NRFnApp ("reject", List.map ~f:trans_printable ps)
    | Ast.IfThenElse (cond, ifb, elseb) ->
        IfElse (trans_expr cond, trans_stmt ifb, Option.map ~f:trans_stmt elseb)
    | Ast.While (_, _) -> ( ?? )
    | Ast.For _ -> ( ?? )
    | Ast.ForEach (_, _, _) -> ( ?? )
    | Ast.Block _ -> ( ?? )
    | Ast.VarDecl _ -> ( ?? )
    | Ast.FunDef _ -> ( ?? )
    | Ast.Return e -> Return (Some (trans_expr e))
    | Ast.ReturnVoid -> Return None
    | Ast.Break -> Break
    | Ast.Continue -> Continue
    | Ast.Skip -> Skip
  in
  {Mir.stmt= s; sloc= trans_loc stmt_typed_loc}

and trans_printable (p : Ast.typed_expression Ast.printable) =
  match p with Ast.PString s -> Lit (Str, s) | Ast.PExpr e -> trans_expr e

and trans_loc = function
  | Ast.Nowhere -> ""
  | Ast.Location (start, end_) ->
      (* XXX hack *)
      let open Lexing in
      sprintf "\"%s\", line %d-%d" start.pos_fname start.pos_lnum end_.pos_lnum
