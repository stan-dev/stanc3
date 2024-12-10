open Core
open Ast
open Middle
open Yojson.Basic

let rec unsized_basetype_json t =
  let to_json (type_, dim) =
    [("type", `String type_); ("dimensions", `Int dim)] in
  let internal, dims = UnsizedType.unwind_array_type t in
  match internal with
  | UnsizedType.UInt -> to_json ("int", dims)
  | UReal -> to_json ("real", dims)
  | UComplex -> to_json ("complex", dims)
  | UVector | URowVector -> to_json ("real", dims + 1)
  | UComplexVector | UComplexRowVector -> to_json ("complex", dims + 1)
  | UMatrix -> to_json ("real", dims + 2)
  | UComplexMatrix -> to_json ("complex", dims + 2)
  | UTuple internals ->
      [ ( "type"
        , `List
            (List.map ~f:(fun t -> `Assoc (unsized_basetype_json t)) internals)
        ); ("dimensions", `Int dims) ]
  | UMathLibraryFunction | UFun _ | UArray _ -> assert false

let basetype_dims t = SizedType.to_unsized t |> unsized_basetype_json

let rec transformation t =
  let expr_string = Fmt.to_to_string Pretty_printing.pp_expression in
  let expr_string e =
    `String (expr_string (Ast.untyped_expression_of_typed_expression e)) in
  let transform details = [("constraint", details)] in
  match t with
  | Transformation.Identity -> transform (`String "none")
  | Lower e -> transform @@ `Assoc [("lower", expr_string e)]
  | Upper e -> transform @@ `Assoc [("upper", expr_string e)]
  | LowerUpper (e1, e2) ->
      transform @@ `Assoc [("lower", expr_string e1); ("upper", expr_string e2)]
  | Offset e -> transform @@ `Assoc [("offset", expr_string e)]
  | Multiplier e -> transform @@ `Assoc [("multiplier", expr_string e)]
  | OffsetMultiplier (e1, e2) ->
      transform
      @@ `Assoc [("offset", expr_string e1); ("multiplier", expr_string e2)]
  | Ordered -> transform (`String "ordered")
  | PositiveOrdered -> transform (`String "positive_ordered")
  | Simplex -> transform (`String "simplex")
  | UnitVector -> transform (`String "unit_vector")
  | SumToZero -> transform (`String "sum_to_zero")
  | CholeskyCorr -> transform (`String "cholesky_corr")
  | CholeskyCov -> transform (`String "cholesky_cov")
  | Correlation -> transform (`String "correlation")
  | Covariance -> transform (`String "covariance")
  | StochasticRow -> transform (`String "stochastic_row")
  | StochasticColumn -> transform (`String "stochastic_column")
  | TupleTransformation ts ->
      transform (`List (List.map ~f:(fun t -> `Assoc (transformation t)) ts))

let get_var_decl {stmts; _} : t =
  `Assoc
    (List.fold_right ~init:[]
       ~f:(fun stmt acc ->
         match stmt.Ast.stmt with
         | Ast.VarDecl decl ->
             let type_info = basetype_dims decl.decl_type in
             let transform_info = transformation decl.transformation in
             let decl_info =
               List.map
                 ~f:(fun {identifier; _} ->
                   (identifier.name, `Assoc (type_info @ transform_info)))
                 decl.variables in
             decl_info @ acc
         | _ -> acc)
       stmts)

let block_info_json name block : t =
  `Assoc [(name, Option.value_map block ~default:(`Assoc []) ~f:get_var_decl)]

let rec get_function_calls_expr (funs, distrs) expr =
  let acc =
    match expr.expr with
    | FunApp (StanLib _, f, _) -> (Set.add funs f.name, distrs)
    | CondDistApp (StanLib _, f, _) -> (funs, Set.add distrs f.name)
    | _ -> (funs, distrs) in
  fold_expression get_function_calls_expr (fun acc _ -> acc) acc expr.expr

let rec get_function_calls_stmt ud_dists (funs, distrs) stmt =
  let acc =
    match stmt.stmt with
    | NRFunApp (StanLib _, f, _) -> (Set.add funs f.name, distrs)
    | Print _ -> (Set.add funs "print", distrs)
    | Reject _ -> (Set.add funs "reject", distrs)
    | FatalError _ -> (Set.add funs "fatal_error", distrs)
    | Tilde {distribution; kind= StanLib (FnLpdf _); _} ->
        (funs, Set.add distrs (distribution.name ^ "_lupdf"))
    | Tilde {distribution; kind= StanLib (FnLpmf _); _} ->
        (funs, Set.add distrs (distribution.name ^ "_lupmf"))
    | _ -> (funs, distrs) in
  fold_statement get_function_calls_expr
    (get_function_calls_stmt ud_dists)
    (fun acc _ -> acc)
    (fun acc _ -> acc)
    acc stmt.stmt

let function_calls_json p =
  let map f list_op =
    Option.value_map ~default:[]
      ~f:(fun {stmts; _} -> List.concat_map ~f stmts)
      list_op in
  let grab_fundef_names_and_types = function
    | {Ast.stmt= Ast.FunDef {funname; arguments= (_, type_, _) :: _; _}; _} ->
        [(funname.name, type_)]
    | _ -> [] in
  let ud_dists = map grab_fundef_names_and_types p.functionblock in
  let funs, distrs =
    fold_program
      (get_function_calls_stmt ud_dists)
      (String.Set.empty, String.Set.empty)
      p in
  let set_to_List s =
    `List (Set.to_list s |> List.map ~f:(fun str -> `String str)) in
  `Assoc [("functions", set_to_List funs); ("distributions", set_to_List distrs)]

let includes_json () =
  `Assoc
    [ ( "included_files"
      , `List
          (List.rev !Preprocessor.included_files
          |> List.map ~f:(fun str -> `String str)) ) ]

let info_json ast =
  List.fold ~f:Util.combine ~init:(`Assoc [])
    [ block_info_json "inputs" ast.datablock
    ; block_info_json "parameters" ast.parametersblock
    ; block_info_json "transformed parameters" ast.transformedparametersblock
    ; block_info_json "generated quantities" ast.generatedquantitiesblock
    ; function_calls_json ast; includes_json () ]

let info ast = pretty_to_string (info_json ast)
