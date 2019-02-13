(** Translate from the AST to the MIR *)

open Core_kernel
open Mir

let rec trans_expr {Ast.expr_typed; _} =
  match expr_typed with
  | Ast.TernaryIf (cond, ifb, elseb) ->
      TernaryIf (trans_expr cond, trans_expr ifb, trans_expr elseb)
  | Ast.BinOp (lhs, op, rhs) -> BinOp (trans_expr lhs, op, trans_expr rhs)
  | Ast.PrefixOp (op, e) | Ast.PostfixOp (e, op) ->
      FunApp (Operators.operator_name op, [trans_expr e])
  | Ast.Variable {name; _} -> Var name
  | Ast.IntNumeral x -> Lit (Int, x)
  | Ast.RealNumeral x -> Lit (Real, x)
  | Ast.FunApp ({name; _}, args) | Ast.CondDistApp ({name; _}, args) ->
      FunApp (name, List.map ~f:trans_expr args)
  | Ast.GetLP | Ast.GetTarget -> Var "target"
  | Ast.ArrayExpr eles -> FunApp ("make_array", List.map ~f:trans_expr eles)
  | Ast.RowVectorExpr eles ->
      FunApp ("make_rowvec", List.map ~f:trans_expr eles)
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
    | Ast.UInt -> Single (trans_expr e)
    | Ast.UArray _ -> MultiIndex (trans_expr e)
    | _ ->
        raise_s
          [%message
            "Expecting int or array" (e.expr_typed_type : Ast.unsizedtype)] )

let trans_sizedtype = Ast.map_sizedtype trans_expr
let neg_inf = FunApp ("negative_infinity", [])

let targetpe e =
  let t = Var "target" in
  Assignment (t, BinOp (t, Plus, e))

let trans_loc = function
  | {Ast.begin_loc; end_loc} ->
      (* TODO: this is a stub; insert actual definition here. *)
      sprintf "\"%s\", line %d-%d" begin_loc.filename begin_loc.line_num
        end_loc.line_num

let bind_loc loc s = {stmt= s; sloc= trans_loc loc}
let no_loc = ""
let with_no_loc s = {stmt= s; sloc= no_loc}
let trans_trans = Ast.map_transformation trans_expr
let trans_arg (adtype, ut, ident) = (adtype, ident.Ast.name, ut)

let truncate_dist ast_obs t =
  let add_inf = targetpe neg_inf and obs = trans_expr ast_obs in
  let trunc cond x y =
    bind_loc x.Ast.expr_typed_loc
      (IfElse
         (BinOp (obs, cond, trans_expr x), bind_loc x.expr_typed_loc add_inf, y))
  in
  match t with
  | Ast.NoTruncate -> None
  | Ast.TruncateUpFrom lb -> Some (trunc Less lb None)
  | Ast.TruncateDownFrom ub -> Some (trunc Greater ub None)
  | Ast.TruncateBetween (lb, ub) ->
      Some (trunc Less lb (Some (trunc Greater ub None)))

let unquote s =
  if s.[0] = '"' && s.[String.length s - 1] = '"' then
    String.drop_suffix (String.drop_prefix s 1) 1
  else s

let trans_printable (p : Ast.typed_expression Ast.printable) =
  match p with
  | Ast.PString s -> Lit (Str, unquote s)
  | Ast.PExpr e -> trans_expr e

let rec trans_stmt {Ast.stmt_typed; stmt_typed_loc; _} =
  let or_skip = Option.value ~default:Skip in
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
              FunApp (Operators.operator_name op, [assignee; rhs])
        in
        Assignment (assignee, rhs)
    | Ast.NRFunApp ({name; _}, args) ->
        NRFunApp (name, List.map ~f:trans_expr args)
    | Ast.IncrementLogProb e | Ast.TargetPE e -> targetpe (trans_expr e)
    | Ast.Tilde {arg; distribution; args; truncation} ->
        let add_dist =
          (* XXX distribution name suffix? *)
          (* XXX Reminder to differentiate between tilde, which drops constants, and
             vanilla target +=, which doesn't. Can use _unnormalized or something.*)
          targetpe
            (FunApp (distribution.name, List.map ~f:trans_expr (arg :: args)))
        in
        Block
          [ Option.value
              ~default:(bind_loc stmt_typed_loc Skip)
              (truncate_dist arg truncation)
          ; bind_loc stmt_typed_loc add_dist ]
    | Ast.Print ps -> NRFunApp ("print", List.map ~f:trans_printable ps)
    | Ast.Reject ps -> NRFunApp ("reject", List.map ~f:trans_printable ps)
    | Ast.IfThenElse (cond, ifb, elseb) ->
        IfElse (trans_expr cond, trans_stmt ifb, Option.map ~f:trans_stmt elseb)
    | Ast.While (cond, body) -> While (trans_expr cond, trans_stmt body)
    | Ast.For {loop_variable; lower_bound; upper_bound; loop_body} ->
        For
          { loopvar= Var loop_variable.Ast.name
          ; lower= trans_expr lower_bound
          ; upper= trans_expr upper_bound
          ; body= trans_stmt loop_body }
    | Ast.ForEach (loopvar, iteratee, body) ->
        For
          { loopvar= Var loopvar.Ast.name
          ; lower= Lit (Int, "0")
          ; upper= FunApp ("length", [trans_expr iteratee])
          ; body= trans_stmt body }
    | Ast.FunDef {returntype; funname; arguments; body} ->
        FunDef
          { fdrt=
              ( match returntype with
              | Ast.Void -> None
              | ReturnType ut -> Some ut )
          ; fdname= funname.name
          ; fdargs= List.map ~f:trans_arg arguments
          ; fdbody= trans_stmt body }
    | Ast.VarDecl
        {sizedtype; transformation; identifier; initial_value; is_global} ->
        (* Should have already taken care of global and trans stuff. *)
        ignore (transformation, is_global) ;
        let name = identifier.name in
        (* XXX Deal with global vs unglobal *)
        (* XXX Should also generate the statements that will read the data in
         and validate it... Then a CSE pass will automatically fulfill one of our
         Stanc3 promises to do data checking only once and at the appropriate level
      *)
        SList
          (List.map ~f:(bind_loc stmt_typed_loc)
             [ Decl
                 { decl_adtype= AutoDiffable
                 ; decl_id= name
                 ; decl_type= trans_sizedtype sizedtype }
             ; Option.map
                 ~f:(fun x -> Assignment (Var name, trans_expr x))
                 initial_value
               |> or_skip ])
    | Ast.Block stmts -> Block (List.map ~f:trans_stmt stmts)
    | Ast.Return e -> Return (Some (trans_expr e))
    | Ast.ReturnVoid -> Return None
    | Ast.Break -> Break
    | Ast.Continue -> Continue
    | Ast.Skip -> Skip
  in
  bind_loc stmt_typed_loc s

(* XXX Write a function that generates MIR to execute once on each thing in some nested
   arrays (but not elements within a matrix or vector) *)

(*
let mir_for_each_in_array (st : sizedtype) (s : expr -> stmt_loc) =
  match st with
  | SInt -> s
  | SReal -> ( ?? )
  | SArray (_, _) -> ( ?? )
  | SVector _ -> ( ?? )
  | SRowVector _ -> ( ?? )
  | SMatrix _ -> ( ?? )
*)

let rec trans_checks ccvid cctype t =
  let check = {ccvid; cctype; ccargs= []; ccfunname= ""} in
  match t with
  | Ast.Identity -> []
  | Ast.Lower lb ->
      [Check {check with ccargs= [lb]; ccfunname= "greater_or_equal"}]
  | Ast.Upper ub ->
      [Check {check with ccargs= [ub]; ccfunname= "less_or_equal"}]
  | Ast.LowerUpper (lb, ub) ->
      [Ast.Lower lb; Upper ub]
      |> List.map ~f:(trans_checks ccvid cctype)
      |> List.concat
  | Ast.Ordered -> [Check {check with ccfunname= "ordered"}]
  | Ast.PositiveOrdered -> [Check {check with ccfunname= "positive_ordered"}]
  | Ast.Simplex -> [Check {check with ccfunname= "simplex"}]
  | Ast.UnitVector -> [Check {check with ccfunname= "unit_vector"}]
  | Ast.CholeskyCorr -> [Check {check with ccfunname= "cholesky_factor_corr"}]
  | Ast.CholeskyCov -> [Check {check with ccfunname= "cholesky_factor"}]
  | Ast.Correlation -> [Check {check with ccfunname= "corr_matrix"}]
  | Ast.Covariance -> [Check {check with ccfunname= "cov_matrix"}]
  | Ast.Offset _ | Ast.Multiplier _ | Ast.OffsetMultiplier (_, _) -> []

let trans_tvdecl {Ast.stmt_typed; stmt_typed_loc; _} =
  match stmt_typed with
  | Ast.VarDecl
      {sizedtype; transformation; identifier; initial_value; is_global} ->
      ignore (initial_value, is_global) ;
      Some
        { tvident= identifier.name
        ; tvtype= trans_sizedtype sizedtype
        ; tvtrans= trans_trans transformation
        ; tvloc= trans_loc stmt_typed_loc }
  | _ -> None

let mktvtable lst =
  (* Someday we may support "topvars" below the "top" level, but for now
     we only deal with ones that are in a list at the top of the
  *)
  List.(filter_opt (map ~f:trans_tvdecl lst))
  |> List.map ~f:(fun t -> (t.tvident, t))
  |> Map.Poly.of_alist_exn

(** Adds Mir statements that validate the variable once it has been read.
    The code to read it in is emitted in the backend.contents
    Here we intentionally only check declarations at the top level, i.e.
    we only recurse into blocks and lists as we don't care about other declarations.
*)
let tvdecl_checks {tvident; tvtrans; tvtype; tvloc} =
  let with_sloc stmt = {sloc= tvloc; stmt} in
  let check_stmts =
    List.map ~f:with_sloc (trans_checks tvident tvtype tvtrans)
  in
  with_sloc (SList check_stmts)

let pull_tvdecls = function
  | None -> (Map.Poly.empty, [])
  | Some lst ->
      let tvdecls, other_statements =
        List.partition_tf ~f:(Fn.compose Option.is_some trans_tvdecl) lst
      in
      let tvtable = mktvtable tvdecls in
      let checks = Map.Poly.map ~f:tvdecl_checks tvtable |> Map.Poly.data in
      (tvtable, checks @ List.map ~f:trans_stmt other_statements)

(* We represent the data and parameter blocks as maps from a variable's identifier
   to a tvdecl containing a little more information about it that we need to
   generate data checks, read in data fields, or transform parameters.

   vardecls mapping:
   * data block -> private fields; checks in ctor
   * tdata -> private fields; checks in ctor
   * param -> log_prob fn params; transformations in write_array, transform_inits, log_prob
   * tparam -> no initialization by default, checks that it has been initialized,
               checked in log_prob, write_array,
   * model -> no constraints allowed (not even corr_matrix et al); not visible
   * gq -> declared and checked in write_array, shows up as param in some methods

*)

(*
   There are at least three places where we currently generate redundant code:
   - checks and validation of data and bounds
   - assignment of newly declared vars, which may immediately be filled
   - setting the current line number

   I think in general we'd like to try reifying these things in the MIR. The hope here
   is that the optimization pass can easily and correctly determine dependencies
   and purity for reordering, hoisting, and elimination.
*)

let trans_prog filename
    { Ast.functionblock
    ; datablock
    ; transformeddatablock
    ; parametersblock
    ; transformedparametersblock
    ; modelblock
    ; generatedquantitiesblock } =
  let trans_or_skip lst_option =
    with_no_loc
      ( match lst_option with
      | None | Some [] -> Skip
      | Some lst -> SList (List.map ~f:trans_stmt lst) )
  in
  let lbind s =
    match s.stmt with SList ls | Block ls -> ls | Skip -> [] | _ -> [s]
  in
  let coalesce stmts =
    let flattened = List.(concat (map ~f:lbind stmts)) in
    with_no_loc (match flattened with [] -> Skip | _ :: _ -> Block flattened)
  in
  let pull_tvdecls_multi blocks =
    let merge_maps maps =
      List.map ~f:Map.Poly.to_alist maps
      |> List.concat |> Map.Poly.of_alist_exn
    and tvtables, stmts = blocks |> List.map ~f:pull_tvdecls |> List.unzip in
    (merge_maps tvtables, coalesce (List.concat stmts))
  in
  let datavars, datachecks = pull_tvdecls datablock in
  (* XXX probably a weird place to keep the name*)
  { prog_name= !Semantic_check.model_name
  ; prog_path= filename
  ; functionsb= trans_or_skip functionblock
  ; datavars
  ; tdatab=
      (let tvtables, stmt = pull_tvdecls_multi [transformeddatablock] in
       (tvtables, coalesce (datachecks @ [stmt])))
  ; modelb=
      (let tvtables, stmt =
         pull_tvdecls_multi [parametersblock; transformedparametersblock]
       in
       (tvtables, coalesce [stmt; trans_or_skip modelblock]))
  ; gqb= pull_tvdecls_multi [generatedquantitiesblock] }
