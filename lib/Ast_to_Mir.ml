(** Translate from the AST to the MIR *)

open Core_kernel
open Mir

let op_to_funapp op args = FunApp (Operators.operator_name op, args)

let rec trans_expr {Ast.expr_typed; _} =
  match expr_typed with
  | Ast.TernaryIf (cond, ifb, elseb) ->
      TernaryIf (trans_expr cond, trans_expr ifb, trans_expr elseb)
  | Ast.BinOp (lhs, op, rhs) -> op_to_funapp op [trans_expr lhs; trans_expr rhs]
  | Ast.PrefixOp (op, e) | Ast.PostfixOp (e, op) ->
      op_to_funapp op [trans_expr e]
  | Ast.Variable {name; _} -> Var name
  | Ast.IntNumeral x -> Lit (Int, x)
  | Ast.RealNumeral x -> Lit (Real, x)
  | Ast.FunApp ({name; _}, args) | Ast.CondDistApp ({name; _}, args) ->
      FunApp (name, trans_exprs args)
  | Ast.GetLP | Ast.GetTarget -> Var "target"
  | Ast.ArrayExpr eles -> FunApp ("make_array", trans_exprs eles)
  | Ast.RowVectorExpr eles -> FunApp ("make_rowvec", trans_exprs eles)
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

and trans_exprs = List.map ~f:trans_expr

let trans_sizedtype = Ast.map_sizedtype trans_expr
let neg_inf = FunApp ("negative_infinity", [])

let lbind s =
  match s.stmt with SList ls | Block ls -> ls | Skip -> [] | _ -> [s]

let add_to_or_create_block source target =
  {target with stmt= Block ({target with stmt= source} :: lbind target)}

let trans_loc = Errors.string_of_location_span
let bind_loc loc s = {stmt= s; sloc= trans_loc loc}
let trans_trans = Ast.map_transformation trans_expr
let trans_arg (adtype, ut, ident) = (adtype, ident.Ast.name, ut)

let truncate_dist ast_obs t =
  let add_inf = TargetPE neg_inf in
  let trunc cond_op x y =
    bind_loc x.Ast.expr_typed_loc
      (IfElse
         ( op_to_funapp cond_op (trans_exprs [ast_obs; x])
         , bind_loc x.expr_typed_loc add_inf
         , y ))
  in
  match t with
  | Ast.NoTruncate -> []
  | TruncateUpFrom lb -> [trunc Ast.Less lb None]
  | TruncateDownFrom ub -> [trunc Ast.Greater ub None]
  | TruncateBetween (lb, ub) ->
      [trunc Ast.Less lb (Some (trunc Ast.Greater ub None))]

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
        in
        let rhs = trans_expr assign_rhs in
        let rhs =
          match assign_op with
          | Ast.Assign | Ast.ArrowAssign -> trans_expr assign_rhs
          | Ast.OperatorAssign op -> op_to_funapp op [assignee; rhs]
        in
        Assignment (assignee, rhs)
    | Ast.NRFunApp ({name; _}, args) -> NRFunApp (name, trans_exprs args)
    | Ast.IncrementLogProb e | Ast.TargetPE e -> TargetPE (trans_expr e)
    | Ast.Tilde {arg; distribution; args; truncation} ->
        let add_dist =
          (* XXX distribution name suffix? *)
          (* XXX Reminder to differentiate between tilde, which drops constants, and
             vanilla target +=, which doesn't. Can use _unnormalized or something.*)
          TargetPE (FunApp (distribution.name, trans_exprs (arg :: args)))
        in
        SList
          (truncate_dist arg truncation @ [bind_loc stmt_typed_loc add_dist])
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
        let iteratee = trans_expr iteratee
        and indexing_var = Var (Util.gensym ())
        and body = trans_stmt body in
        let assign_loopvar =
          Assignment
            (Var loopvar.name, Indexed (iteratee, [Single indexing_var]))
        in
        For
          { loopvar= indexing_var
          ; lower= Lit (Int, "0")
          ; upper= FunApp ("length", [iteratee])
          ; body= add_to_or_create_block assign_loopvar body }
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
        (* Should have already taken care of global and transformation-related stuff
          in other passes over this AST.*)
        ignore (transformation, is_global) ;
        let name = identifier.name in
        SList
          (List.map ~f:(bind_loc stmt_typed_loc)
             [ Decl
                 { decl_adtype=
                     AutoDiffable
                     (* XXX Shouldn't be autodiffable in tdata or gen quant *)
                 ; decl_id= name
                 ; decl_type= Ast.remove_size (trans_sizedtype sizedtype) }
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

(** [add_index expression index] returns an expression that (additionally)
    indexes into the input [expression] by [index].*)
let add_index e i =
  match e with
  | Var _ -> Indexed (e, [i])
  | Indexed (e, indices) -> Indexed (e, indices @ [i])
  | _ -> raise_s [%message "These should go away with Ryan's LHS"]

(** [mkfor] returns a MIR For statement that iterates over the given expression
    [iteratee]. *)
let mkfor ut bodyfn iteratee sloc =
  let idx s =
    match ut with
    (*  | Ast.UMatrix -> MatrixSingle (Var s)
*)
    | Ast.UVector | URowVector | UMatrix | UArray _ -> Single (Var s)
    | _ ->
        raise_s
          [%message "Why are we making for loops around" (ut : unsizedtype)]
  in
  let sym, reset = Util.gensym_enter () in
  let stmt =
    For
      { loopvar= Var sym
      ; lower= Lit (Int, "0")
      ; upper= FunApp ("length", [iteratee])
      ; body= {stmt= Block [bodyfn (add_index iteratee (idx sym))]; sloc} }
  in
  reset () ; {stmt; sloc}

(** [for_scalar unsizedtype...] generates a For statement that loops
    over the scalars in the underlying [unsizedtype] *)
let rec for_scalar (ut : unsizedtype) bodyfn var sloc =
  match ut with
  | Ast.UInt | UReal -> bodyfn var
  | UVector | URowVector | UMatrix -> mkfor ut bodyfn var sloc
  | UArray t -> mkfor ut (fun e -> for_scalar t bodyfn e sloc) var sloc
  | UFun _ | UMathLibraryFunction ->
      raise_s [%message "Can't for over " (ut : unsizedtype)]

(** [for_eigen unsizedtype...] generates a For statement that loops
    over the eigen types in the underlying [unsizedtype]; i.e. just iterating
    overarrays and running bodyfn on any eign types found within.*)
let rec for_eigen ut bodyfn var sloc =
  match ut with
  | Ast.UInt | UReal | UVector | URowVector | UMatrix -> bodyfn var
  | UArray t -> mkfor ut (fun e -> for_eigen t bodyfn e sloc) var sloc
  | UFun _ | UMathLibraryFunction ->
      raise_s [%message "Can't for over " (ut : unsizedtype)]

let rec trans_checks tvd =
  let chk forl fn args =
    forl
      (Ast.remove_size tvd.tvtype)
      (fun id -> {stmt= Check (fn, id :: args); sloc= tvd.tvloc})
      (Var tvd.tvident) tvd.tvloc
  in
  match tvd.tvtrans with
  | Ast.Identity -> []
  | Ast.Lower lb -> [chk for_scalar "greater_or_equal" [lb]]
  | Ast.Upper ub -> [chk for_scalar "less_or_equal" [ub]]
  | Ast.LowerUpper (lb, ub) ->
      trans_checks {tvd with tvtrans= Ast.Lower lb}
      @ trans_checks {tvd with tvtrans= Ast.Upper ub}
  | Ast.Ordered -> [chk for_eigen "ordered" []]
  | Ast.PositiveOrdered -> [chk for_eigen "positive_ordered" []]
  | Ast.Simplex -> [chk for_eigen "simplex" []]
  | Ast.UnitVector -> [chk for_eigen "unit_vector" []]
  | Ast.CholeskyCorr -> [chk for_eigen "cholesky_factor_corr" []]
  | Ast.CholeskyCov -> [chk for_eigen "cholesky_factor" []]
  | Ast.Correlation -> [chk for_eigen "corr_matrix" []]
  | Ast.Covariance -> [chk for_eigen "cov_matrix" []]
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
let pull_tvdecls list_op =
  let is_tvdecl = function
    | {Ast.stmt_typed= VarDecl _; _} -> true
    | _ -> false
  in
  match list_op with
  | None -> (Map.Poly.empty, [])
  | Some lst ->
      let tvdecls, statements = List.partition_tf ~f:is_tvdecl lst in
      (mktvtable tvdecls, List.map ~f:trans_stmt statements)

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

(* There are at least three places where we currently generate redundant code:
   - checks and validation of data and bounds
   - assignment of newly declared vars, which may immediately be filled
   - setting the current line number

   I think in general we'd like to try reifying these things in the MIR. The hope here
   is that the optimization pass can easily and correctly determine dependencies
   and purity for reordering, hoisting, and elimination.*)
let trans_prog filename
    { Ast.functionblock
    ; datablock
    ; transformeddatablock
    ; parametersblock
    ; transformedparametersblock
    ; modelblock
    ; generatedquantitiesblock } =
  let map f list_op = Option.to_list list_op |> List.concat |> List.map ~f in
  let or_empty f list_op =
    Option.(value ~default:Map.Poly.empty (map ~f list_op))
  in
  let map_tvds f tvtable = Map.Poly.data tvtable |> List.map ~f in
  let data_vars = or_empty mktvtable datablock in
  let params = or_empty mktvtable parametersblock in
  let tdata_vars, tdata = pull_tvdecls transformeddatablock in
  let tparams, prepare_params = pull_tvdecls transformedparametersblock in
  let prepare_data =
    List.(
      concat
        (concat
           [ map_tvds trans_checks data_vars
           ; [tdata]
           ; map_tvds trans_checks tdata_vars ]))
  in
  let gen_quant_vars, generate_quantities =
    pull_tvdecls generatedquantitiesblock
  in
  { functions_block= map trans_stmt functionblock
  ; data_vars
  ; tdata_vars
  ; prepare_data
  ; params
  ; tparams
  ; prepare_params
  ; log_prob= map trans_stmt modelblock
  ; gen_quant_vars
  ; generate_quantities
  ; prog_name= !Semantic_check.model_name
  ; prog_path= filename }
