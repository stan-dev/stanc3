(** Translate from the AST to the MIR *)

open Core_kernel
open Stanc_mir.Mir


(* Conversion between ast and mir 'shared' types 
   XXX I suspect it would make more sense to have a single type but 
   I have duplicated in order to minimize impact on the rest of the
   frontend code
*)
let ast_to_mir_autodifftype = function
  | Ast.DataOnly -> DataOnly
  | Ast.AutoDiffable -> AutoDiffable

let mir_to_ast_autodifftype = function
  | DataOnly -> Ast.DataOnly
  | AutoDiffable -> Ast.AutoDiffable

(* XXX make tail recursive *)
let rec ast_to_mir_unsizedtype = function
  | Ast.UInt -> UInt
  | UReal -> UReal
  | UVector -> UVector
  | URowVector -> URowVector
  | UMatrix -> UMatrix
  | UArray inner -> ast_to_mir_unsizedtype inner |> UArray
  | UFun(arg_tys,return_ty) -> 
      UFun
        ( List.map arg_tys 
          ~f:(fun (ad,ut) -> 
                (ast_to_mir_autodifftype ad , ast_to_mir_unsizedtype ut)
            )
        , ast_to_mir_returntype return_ty
        )
  | UMathLibraryFunction -> UMathLibraryFunction
and ast_to_mir_returntype = function
  | Ast.Void -> Void 
  | ReturnType ut -> ast_to_mir_unsizedtype ut |> ReturnType


let rec mir_to_ast_unsizedtype = function
  | UInt -> Ast.UInt
  | UReal -> UReal
  | UVector -> UVector
  | URowVector -> URowVector
  | UMatrix -> UMatrix
  | UArray inner -> mir_to_ast_unsizedtype inner |> UArray
  | UFun(arg_tys,return_ty) -> 
      UFun
        ( List.map arg_tys 
          ~f:(fun (ad,ut) -> 
                (mir_to_ast_autodifftype ad , mir_to_ast_unsizedtype ut)
            )
        , mir_to_ast_returntype return_ty
        )
  | UMathLibraryFunction -> UMathLibraryFunction
and mir_to_ast_returntype = function
  | Void -> Ast.Void 
  | ReturnType ut -> mir_to_ast_unsizedtype ut |> ReturnType

let rec ast_to_mir_location ({ filename; line_num; col_num; included_from} : Ast.location) = 
  { filename = filename
  ; line_num = line_num
  ; col_num = col_num
  ; included_from = Option.map ~f:ast_to_mir_location included_from
  }

let ast_to_mir_location_span ({begin_loc;end_loc} : Ast.location_span) : location_span = 
  { begin_loc = ast_to_mir_location begin_loc
  ; end_loc = ast_to_mir_location  end_loc
  }

let rec mir_to_ast_location  {filename; line_num; col_num; included_from} : Ast.location = 
  { filename = filename
  ; line_num = line_num
  ; col_num = col_num
  ; included_from = Option.map ~f:mir_to_ast_location included_from
  }

let mir_to_ast_location_span {begin_loc;end_loc} : Ast.location_span = 
  { begin_loc = mir_to_ast_location begin_loc
  ; end_loc = mir_to_ast_location  end_loc
  }

let rec ast_to_mir_sizedtype = function 
  | Ast.SInt -> SInt
  | SReal -> SReal
  | SVector e -> SVector e 
  | SRowVector e -> SRowVector e
  | SMatrix(e,f) -> SMatrix(e,f)
  | SArray(st,e) -> SArray(ast_to_mir_sizedtype st,e)

let ast_to_mir_transformation = function 
  | Ast.Identity -> Identity
  | Lower e -> Lower e 
  | Upper e -> Upper e
  | LowerUpper(e,f) -> LowerUpper(e,f)
  | Offset e -> Offset e 
  | Multiplier e -> Multiplier e 
  | OffsetMultiplier(e,f) -> OffsetMultiplier(e,f)
  | Ordered -> Ordered
  | PositiveOrdered -> PositiveOrdered
  | Simplex -> Simplex
  | UnitVector -> UnitVector
  | CholeskyCorr -> CholeskyCorr
  | CholeskyCov -> CholeskyCov
  | Correlation -> Correlation
  | Covariance -> Covariance

(* XXX fix exn *)
let unwrap_return_exn = function
  | Some (Ast.ReturnType ut) -> ast_to_mir_unsizedtype ut
  | x ->
      raise_s [%message "Unexpected return type " (x : Ast.returntype option)]

let rec op_to_funapp op args =
  { texpr = FunApp (Operators.operator_name op, trans_exprs args)
  ; texpr_type = Operators.operator_return_type op args |> unwrap_return_exn
  ; texpr_loc = Ast.expr_loc_lub args |> ast_to_mir_location_span
  ; texpr_adlevel= Ast.expr_ad_lub args |> ast_to_mir_autodifftype
  }

and trans_expr
    { Ast.expr_typed
    ; expr_typed_type= texpr_type
    ; expr_typed_loc= texpr_loc
    ; expr_typed_ad_level= texpr_adlevel } =
  match expr_typed with
  | Ast.Paren x -> trans_expr x
  | BinOp (lhs, op, rhs) -> op_to_funapp op [lhs; rhs]
  | PrefixOp (op, e) | Ast.PostfixOp (e, op) -> op_to_funapp op [e]
  | _ ->
      let texpr =
        match expr_typed with
        | Ast.TernaryIf (cond, ifb, elseb) ->
            TernaryIf (trans_expr cond, trans_expr ifb, trans_expr elseb)
        | Variable {name; _} -> Var name
        | IntNumeral x -> Lit (Int, x)
        | RealNumeral x -> Lit (Real, x)
        | FunApp ({name; _}, args) | Ast.CondDistApp ({name; _}, args) ->
            FunApp (name, trans_exprs args)
        | GetLP | GetTarget -> Var "target"
        | ArrayExpr eles -> FunApp ("make_array", trans_exprs eles)
        | RowVectorExpr eles -> FunApp ("make_rowvec", trans_exprs eles)
        | Indexed (lhs, indices) ->
            Indexed (trans_expr lhs, List.map ~f:trans_idx indices)
        | Paren _ | BinOp _ | PrefixOp _ | PostfixOp _ ->
            raise_s [%message "Impossible!"]
      in
      { texpr = texpr
      ; texpr_type = ast_to_mir_unsizedtype texpr_type
      ; texpr_loc =  ast_to_mir_location_span texpr_loc
      ; texpr_adlevel = ast_to_mir_autodifftype texpr_adlevel
      }

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

and trans_exprs xs = List.map ~f:trans_expr xs

let trans_sizedtype = map_sizedtype trans_expr

let neg_inf =
  { texpr_type = UReal
  ; texpr_loc = no_span
  ; texpr = FunApp ("negative_infinity", [])
  ; texpr_adlevel = DataOnly
  }

let lbind s =
  match s.stmt with 
  | SList ls 
  | Block ls -> ls 
  | Skip -> [] 
  | _ -> [s]

let add_to_or_create_block source target =
  {target with 
    stmt= Block ({target with stmt= source} :: lbind target)
  }

let trans_trans = map_transformation trans_expr

let trans_arg (adtype, ut, ident) = 
  ( ast_to_mir_autodifftype adtype
  , ident.Ast.name
  , ast_to_mir_unsizedtype ut
  )

let truncate_dist ast_obs t =
  let trunc cond_op x y =
    let sloc = 
      x.Ast.expr_typed_loc 
      |> ast_to_mir_location_span 
    in
    { sloc
    ; stmt=
        IfElse
          (op_to_funapp cond_op [ast_obs; x]
          , {sloc; stmt= TargetPE neg_inf}, y
          )
    }
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

let trans_printables texpr_loc (ps : Ast.typed_expression Ast.printable list) =
  List.map
    ~f:(function
      | Ast.PString s ->
          { texpr_adlevel= DataOnly
          ; texpr= Lit (Str, unquote s)
          ; texpr_loc
          ; texpr_type= UReal }
          (*XXX hack strings aren't real*)
      | Ast.PExpr e -> trans_expr e)
    ps

let rec trans_stmt {Ast.stmt_typed; stmt_typed_loc  = sloc; _} =
  let texpr_loc = sloc in
  let or_skip = Option.value ~default:Skip in
  let stmt =
    match stmt_typed with
    | Ast.Assignment 
      {assign_indices; assign_rhs; assign_identifier; assign_op} ->
        let wrap_expr expr_typed =
          { Ast.expr_typed_loc= sloc
          ; expr_typed_ad_level= assign_rhs.expr_typed_ad_level
          ; expr_typed_type= assign_rhs.expr_typed_type
          ; expr_typed }
        in
        let assignee = wrap_expr @@ Ast.Variable assign_identifier in
        let assignee =
          match assign_indices with
          | [] -> assignee
          | lst -> wrap_expr @@ Ast.Indexed (assignee, lst)
        in
        let rhs =
          match assign_op with
          | Ast.Assign | Ast.ArrowAssign -> trans_expr assign_rhs
          | Ast.OperatorAssign op -> op_to_funapp op [assignee; assign_rhs]
        in
        Assignment (trans_expr assignee, rhs)

    | Ast.NRFunApp ({name; _}, args) ->
        NRFunApp (name, trans_exprs args)

    | Ast.IncrementLogProb e 
    | Ast.TargetPE e -> 
        TargetPE (trans_expr e)

    | Ast.Tilde {arg; distribution; args; truncation} ->
        let add_dist =
          (* XXX distribution name suffix? *)
          (* XXX Reminder to differentiate between tilde, which drops constants, and
             vanilla target +=, which doesn't. Can use _unnormalized or something.*)
          TargetPE
            { texpr = FunApp (distribution.name, trans_exprs (arg :: args))
            ; texpr_loc = texpr_loc |> ast_to_mir_location_span
            ; texpr_adlevel = Ast.expr_ad_lub (arg :: args) |> ast_to_mir_autodifftype
            ; texpr_type = UReal 
            }
        in
        SList 
          (truncate_dist arg truncation 
          @ [ { sloc = sloc |> ast_to_mir_location_span  
              ; stmt = add_dist
              }
            ]
          )

    | Ast.Print ps ->
        NRFunApp
          ( "print"
          , trans_printables (ast_to_mir_location_span sloc) ps
          )

    | Ast.Reject ps -> 
        NRFunApp
          ( "reject"
          , trans_printables (ast_to_mir_location_span sloc) ps
          )

    | Ast.IfThenElse (cond, ifb, elseb) ->
        IfElse 
          ( trans_expr cond
          , trans_stmt ifb
          , Option.map ~f:trans_stmt elseb
          )

    | Ast.While (cond, body) ->
        While (trans_expr cond, trans_stmt body)

    | Ast.For {loop_variable; lower_bound; upper_bound; loop_body} ->
        For
          { loopvar= loop_variable.Ast.name
          ; lower= trans_expr lower_bound
          ; upper= trans_expr upper_bound
          ; body= trans_stmt loop_body
          }

    | Ast.ForEach (loopvar, iteratee, body) ->
        let newsym = Util.gensym () in
        let wrap texpr =
          { texpr = texpr
          ; texpr_loc = ast_to_mir_location_span texpr_loc
          ; texpr_type= UInt
          ; texpr_adlevel= DataOnly
          }
        in
        let iteratee = trans_expr iteratee
        and indexing_var = wrap (Var newsym)
        and body = trans_stmt body in
        let assign_loopvar =
          Assignment
            ( Var loopvar.name |> wrap
            , Indexed (iteratee, [Single indexing_var]) |> wrap
            )
        in
        For
          { loopvar= newsym
          ; lower= wrap @@ Lit (Int, "0")
          ; upper= wrap @@ FunApp ("length", [iteratee])
          ; body= add_to_or_create_block assign_loopvar body 
          }

    | Ast.FunDef {returntype; funname; arguments; body} ->
        FunDef
          { fdrt=
              ( match returntype with
              | Ast.Void -> None
              | ReturnType ut -> Some (ast_to_mir_unsizedtype ut)
              )
          ; fdname= funname.name
          ; fdargs= List.map ~f:trans_arg arguments
          ; fdbody= trans_stmt body 
          }

    | Ast.VarDecl
        {sizedtype; transformation; identifier; initial_value; is_global} ->
        (* Should have already taken care of global and transformation-related stuff
          in other passes over this AST.*)
        ignore (transformation, is_global) ;
        let name = identifier.name in
        let assign rhs =
          let rhs = trans_expr rhs in
          Assignment ({rhs with texpr = Var name}, rhs)
        in
        SList
          (List.map
             ~f:(fun stmt -> {sloc = ast_to_mir_location_span sloc ; stmt})
             [ Decl
                 { decl_adtype=
                     AutoDiffable
                     (* XXX Shouldn't be autodiffable in tdata or gen quant *)
                 ; decl_id= name
                 ; decl_type= 
                      remove_size (trans_sizedtype (ast_to_mir_sizedtype sizedtype) )
                      
                 }
             ; Option.map ~f:assign initial_value |> or_skip 
             ]
          )

    | Ast.Block stmts ->
        Block (List.map ~f:trans_stmt stmts)

    | Ast.Return e ->
        Return (Some (trans_expr e))

    | Ast.ReturnVoid ->
        Return None

    | Ast.Break ->
        Break

    | Ast.Continue ->
        Continue

    | Ast.Skip ->
        Skip

  in
  { sloc = ast_to_mir_location_span sloc
  ; stmt
  }

(** [add_index expression index] returns an expression that (additionally)
    indexes into the input [expression] by [index].*)
let add_int_index e i =
  let mir_i = trans_idx i in
  let texpr =
    match e.texpr with
    | Var _ -> Indexed (e, [mir_i])
    | Indexed (e, indices) -> Indexed (e, indices @ [mir_i])
    | _ -> raise_s [%message "These should go away with Ryan's LHS"]
  in
  let texpr_type =
    Semantic_check.inferred_unsizedtype_of_indexed 
      (mir_to_ast_location_span e.texpr_loc)
      (mir_to_ast_unsizedtype e.texpr_type)
      [(i, UInt)]
  in
  { e with 
        texpr = texpr
      ; texpr_type = ast_to_mir_unsizedtype texpr_type
  }

(** [mkfor] returns a MIR For statement that iterates over the given expression
    [iteratee]. *)
let mkfor ut bodyfn iteratee (sloc: location_span) =
  let idx s =
    match ut with
    (*  | Ast.UMatrix -> MatrixSingle (Var s)
*)
    | UVector | URowVector | UMatrix | UArray _ ->
        let expr_typed = Ast.Variable {name= s; id_loc= mir_to_ast_location_span sloc} in
        Ast.Single
          { Ast.expr_typed_loc= mir_to_ast_location_span sloc
          ; expr_typed
          ; expr_typed_ad_level= DataOnly
          ; expr_typed_type= UInt }
    | _ ->
        raise_s
          [%message "Why are we making for loops around" (ut : unsizedtype)]
  in
  let loopvar, reset = Util.gensym_enter () in
  let stmt =
    For
      { loopvar
      ; lower= {internal_expr with texpr= Lit (Int, "0")}
      ; upper= {internal_expr with texpr= FunApp ("length", [iteratee])}
      ; body=
          { stmt = Block [bodyfn (add_int_index iteratee (idx loopvar))]
          ; sloc = sloc
          }
      }
  in
  reset ();
  { stmt
  ; sloc = sloc
  }

(** [for_scalar unsizedtype...] generates a For statement that loops
    over the scalars in the underlying [unsizedtype] *)
let rec for_scalar (ut : unsizedtype) bodyfn var (sloc: location_span) =
  match ut with
  | UInt 
  | UReal -> bodyfn var

  | UVector 
  | URowVector
  | UMatrix -> 
    mkfor ut bodyfn var sloc

  | UArray t -> 
    mkfor ut (fun e -> for_scalar t bodyfn e sloc) var sloc

  | UFun _ 
  | UMathLibraryFunction ->
      raise_s [%message "Can't for over " (ut : unsizedtype)]

(** [for_eigen unsizedtype...] generates a For statement that loops
    over the eigen types in the underlying [unsizedtype]; i.e. just iterating
    overarrays and running bodyfn on any eign types found within.*)
let rec for_eigen ut bodyfn var sloc =
  match ut with
  | UInt 
  | UReal 
  | UVector 
  | URowVector 
  | UMatrix -> 
      bodyfn var

  | UArray t -> 
      mkfor ut (fun e -> for_eigen t bodyfn e sloc) var sloc

  | UFun _
  | UMathLibraryFunction ->
      raise_s [%message "Can't for over " (ut : unsizedtype)]

let rec trans_checks tvd =
  let chk forl fn args =
    forl
      (remove_size tvd.tvtype)
      (fun id -> {stmt= Check (fn, id :: args); sloc= tvd.tvloc})
      { texpr= Var tvd.tvident
      ; texpr_type= remove_size tvd.tvtype
      ; texpr_loc= tvd.tvloc
      ; texpr_adlevel= DataOnly }
      tvd.tvloc
  in
  match tvd.tvtrans with
  | Identity -> []
  | Lower lb -> [chk for_scalar "greater_or_equal" [lb]]
  | Upper ub -> [chk for_scalar "less_or_equal" [ub]]
  | LowerUpper (lb, ub) ->
      trans_checks {tvd with tvtrans= Lower lb}
      @ trans_checks {tvd with tvtrans= Upper ub}

  | Ordered -> [chk for_eigen "ordered" []]
  | PositiveOrdered -> [chk for_eigen "positive_ordered" []]
  | Simplex -> [chk for_eigen "simplex" []]
  | UnitVector -> [chk for_eigen "unit_vector" []]
  | CholeskyCorr -> [chk for_eigen "cholesky_factor_corr" []]
  | CholeskyCov -> [chk for_eigen "cholesky_factor" []]
  | Correlation -> [chk for_eigen "corr_matrix" []]
  | Covariance -> [chk for_eigen "cov_matrix" []]
  | Offset _ 
  | Multiplier _ 
  | OffsetMultiplier (_, _) -> []

let trans_tvdecl {Ast.stmt_typed; stmt_typed_loc= tvloc; _} =
  match stmt_typed with
  | Ast.VarDecl
      {sizedtype; transformation; identifier; _} ->
      Some
        { tvident = identifier.name
        ; tvtype = trans_sizedtype (ast_to_mir_sizedtype sizedtype)
        ; tvtrans = trans_trans (ast_to_mir_transformation transformation)
        ; tvloc = ast_to_mir_location_span tvloc 
        }

  | _ -> None

let mktvtable lst =
  (* Someday we may support "topvars" below the "top" level, but for now
     we only deal with ones that are in a list at the top of the
  *)
  List.filter_map lst
    ~f:(fun x -> 
        trans_tvdecl x 
        |> Option.map ~f:(fun t -> (t.tvident,t))
    )    
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
  { functions_block = map trans_stmt functionblock
  ; data_vars
  ; tdata_vars
  ; prepare_data
  ; params
  ; tparams
  ; prepare_params
  ; log_prob = map trans_stmt modelblock
  ; gen_quant_vars
  ; generate_quantities
  ; prog_name = !Semantic_check.model_name
  ; prog_path = filename
  }

let%expect_test "Prefix-Op-Example" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        model {
          int i;
          if (i < -1) {
            print("Badger");
          }
        }
      |}
  in
  let mir = trans_prog "" (Semantic_check.semantic_check_program ast) in
  print_s [%sexp (mir : typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (data_vars ()) (tdata_vars ()) (prepare_data ())
       (params ()) (tparams ()) (prepare_params ())
       (log_prob
        (((sloc <opaque>)
          (stmt
           (SList
            (((sloc <opaque>)
              (stmt (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type UInt))))
             ((sloc <opaque>) (stmt Skip))))))
         ((sloc <opaque>)
          (stmt
           (IfElse
            ((texpr_type UInt) (texpr_loc <opaque>)
             (texpr
              (FunApp Less__
               (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Var i))
                 (texpr_adlevel DataOnly))
                ((texpr_type UInt) (texpr_loc <opaque>)
                 (texpr
                  (FunApp PMinus__
                   (((texpr_type UInt) (texpr_loc <opaque>) (texpr (Lit Int 1))
                     (texpr_adlevel DataOnly)))))
                 (texpr_adlevel DataOnly)))))
             (texpr_adlevel DataOnly))
            ((sloc <opaque>)
             (stmt
              (Block
               (((sloc <opaque>)
                 (stmt
                  (NRFunApp print
                   (((texpr_type UReal) (texpr_loc <opaque>)
                     (texpr (Lit Str Badger)) (texpr_adlevel DataOnly))))))))))
            ())))))
       (gen_quant_vars ()) (generate_quantities ()) (prog_name "") (prog_path "")) |}]
