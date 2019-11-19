open Core_kernel
open Ast

let deprecated_functions = String.Table.create ()
let deprecated_distributions = String.Table.create ()

let () =
  Hashtbl.add_multi deprecated_functions ~key:"multiply_log" ~data:"lmultiply" ;
  Hashtbl.add_multi deprecated_functions ~key:"binomial_coefficient_log"
    ~data:"lchoose" ;
  Hashtbl.add_multi deprecated_functions ~key:"integrate_ode"
    ~data:"integrate_ode_rk45" ;
  List.iter Middle.Stan_math_signatures.distributions
    ~f:(fun (fnkinds, name, _) ->
      List.iter fnkinds ~f:(function
        | Lpdf ->
            Hashtbl.add_multi deprecated_distributions ~key:(name ^ "_log")
              ~data:(name ^ "_lpdf")
        | Lpmf ->
            Hashtbl.add_multi deprecated_distributions ~key:(name ^ "_log")
              ~data:(name ^ "_lpmf")
        | Cdf ->
            Hashtbl.add_multi deprecated_distributions ~key:(name ^ "_cdf_log")
              ~data:(name ^ "_lcdf")
        | Ccdf ->
            Hashtbl.add_multi deprecated_distributions
              ~key:(name ^ "_ccdf_log") ~data:(name ^ "_lccdf")
        | Rng | UnaryVectorized -> () ) )

let is_distribution name =
  Option.is_some (Hashtbl.find deprecated_distributions name)

let rename_distribution name =
  match Hashtbl.find deprecated_distributions name with
  | None | Some [] -> name
  | Some (rename :: _) -> rename

let rename_function name =
  match Hashtbl.find deprecated_functions name with
  | None | Some [] -> name
  | Some (rename :: _) -> rename

let rec replace_deprecated_expr {expr; emeta} =
  let expr =
    match expr with
    | GetLP -> GetTarget
    | FunApp (f, {name; id_loc}, e) ->
        if is_distribution name then
          CondDistApp
            ( f
            , {name= rename_distribution name; id_loc}
            , List.map ~f:replace_deprecated_expr e )
        else
          FunApp
            ( f
            , {name= rename_function name; id_loc}
            , List.map ~f:replace_deprecated_expr e )
    | _ -> map_expression replace_deprecated_expr ident expr
  in
  {expr; emeta}

let replace_deprecated_lval = map_lval_with replace_deprecated_expr ident

let without_suffix name =
  if
    String.is_suffix ~suffix:"_lpdf" name
    || String.is_suffix ~suffix:"_lpmf" name
  then String.drop_suffix name 5
  else name

let rec replace_deprecated_stmt {stmt; smeta} =
  let stmt =
    match stmt with
    | IncrementLogProb e -> TargetPE (replace_deprecated_expr e)
    | Assignment {assign_lhs= l; assign_op= ArrowAssign; assign_rhs= e} ->
        Assignment
          { assign_lhs= replace_deprecated_lval l
          ; assign_op= Assign
          ; assign_rhs= replace_deprecated_expr e }
    | Tilde {arg; distribution= {name; id_loc}; args; truncation} ->
        Tilde
          { arg= replace_deprecated_expr arg
          ; distribution= {name= without_suffix name; id_loc}
          ; args= List.map ~f:replace_deprecated_expr args
          ; truncation= map_truncation replace_deprecated_expr truncation }
    | stmt ->
        map_statement replace_deprecated_expr replace_deprecated_stmt
          replace_deprecated_lval ident stmt
  in
  {stmt; smeta}

let rec no_parens {expr; emeta} =
  match expr with
  | Paren e -> no_parens e
  | Variable _ | IntNumeral _ | RealNumeral _ | GetLP | GetTarget ->
      {expr; emeta}
  | TernaryIf _ | BinOp _ | PrefixOp _ | PostfixOp _ ->
      {expr= map_expression keep_parens ident expr; emeta}
  | Indexed (e, l) ->
      { expr= Indexed (keep_parens e, List.map ~f:(map_index no_parens) l)
      ; emeta }
  | ArrayExpr _ | RowVectorExpr _ | FunApp _ | CondDistApp _ ->
      {expr= map_expression no_parens ident expr; emeta}

and keep_parens {expr; emeta} =
  match expr with
  | Paren {expr= Paren e; _} -> keep_parens e
  | Paren ({expr= BinOp _; _} as e)
   |Paren ({expr= PrefixOp _; _} as e)
   |Paren ({expr= PostfixOp _; _} as e)
   |Paren ({expr= TernaryIf _; _} as e) ->
      {expr= Paren (no_parens e); emeta}
  | _ -> no_parens {expr; emeta}

let parens_lval = map_lval_with no_parens ident
let parens_stmt = map_statement_with no_parens ident parens_lval ident

let canonicalize_program program : untyped_program =
  program |> map_program replace_deprecated_stmt |> map_program parens_stmt
