open Core_kernel
open Ast
open Deprecation_analysis

let rec repair_syntax_stmt user_dists {stmt; smeta} =
  match stmt with
  | Tilde {arg; distribution= {name; id_loc}; args; truncation} ->
      { stmt=
          Tilde
            { arg
            ; distribution= {name= without_suffix user_dists name; id_loc}
            ; args
            ; truncation }
      ; smeta }
  | _ ->
      { stmt=
          map_statement ident (repair_syntax_stmt user_dists) ident ident stmt
      ; smeta }

let rec replace_deprecated_expr
    (deprecated_userdefined : Middle.UnsizedType.t Core_kernel.String.Map.t)
    {expr; emeta} =
  let expr =
    match expr with
    | GetLP -> GetTarget
    | FunApp (StanLib, {name= "abs"; id_loc}, [e])
      when Middle.UnsizedType.is_real_type e.emeta.type_ ->
        FunApp
          ( StanLib
          , {name= "fabs"; id_loc}
          , [replace_deprecated_expr deprecated_userdefined e] )
    | FunApp (StanLib, {name= "if_else"; _}, [c; t; e]) ->
        Paren
          (replace_deprecated_expr deprecated_userdefined
             {expr= TernaryIf ({expr= Paren c; emeta= c.emeta}, t, e); emeta})
    | FunApp (StanLib, {name; id_loc}, e) ->
        if is_deprecated_distribution name then
          CondDistApp
            ( StanLib
            , {name= rename_deprecated deprecated_distributions name; id_loc}
            , List.map ~f:(replace_deprecated_expr deprecated_userdefined) e )
        else
          FunApp
            ( StanLib
            , {name= rename_deprecated deprecated_functions name; id_loc}
            , List.map ~f:(replace_deprecated_expr deprecated_userdefined) e )
    | FunApp (UserDefined, {name; id_loc}, e) -> (
      match String.Map.find deprecated_userdefined name with
      | Some type_ ->
          CondDistApp
            ( UserDefined
            , {name= update_suffix name type_; id_loc}
            , List.map ~f:(replace_deprecated_expr deprecated_userdefined) e )
      | None ->
          FunApp
            ( UserDefined
            , {name; id_loc}
            , List.map ~f:(replace_deprecated_expr deprecated_userdefined) e )
      )
    | _ ->
        map_expression
          (replace_deprecated_expr deprecated_userdefined)
          ident expr
  in
  {expr; emeta}

let replace_deprecated_lval deprecated_userdefined =
  map_lval_with (replace_deprecated_expr deprecated_userdefined) ident

let rec replace_deprecated_stmt
    (deprecated_userdefined : Middle.UnsizedType.t Core_kernel.String.Map.t)
    ({stmt; smeta} : typed_statement) =
  let stmt =
    match stmt with
    | IncrementLogProb e ->
        TargetPE (replace_deprecated_expr deprecated_userdefined e)
    | Assignment {assign_lhs= l; assign_op= ArrowAssign; assign_rhs= e} ->
        Assignment
          { assign_lhs= replace_deprecated_lval deprecated_userdefined l
          ; assign_op= Assign
          ; assign_rhs= (replace_deprecated_expr deprecated_userdefined) e }
    | FunDef {returntype; funname= {name; id_loc}; arguments; body} ->
        let newname =
          match String.Map.find deprecated_userdefined name with
          | Some type_ -> update_suffix name type_
          | None -> name
        in
        FunDef
          { returntype
          ; funname= {name= newname; id_loc}
          ; arguments
          ; body= replace_deprecated_stmt deprecated_userdefined body }
    | _ ->
        map_statement
          (replace_deprecated_expr deprecated_userdefined)
          (replace_deprecated_stmt deprecated_userdefined)
          (replace_deprecated_lval deprecated_userdefined)
          ident stmt
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
      { expr=
          Indexed
            ( keep_parens e
            , List.map
                ~f:(function
                  | Single e -> Single (no_parens e)
                  | i -> map_index keep_parens i)
                l )
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

let rec parens_stmt {stmt; smeta} =
  let stmt =
    match stmt with
    | VarDecl
        { decl_type= d
        ; transformation= t
        ; identifier
        ; initial_value= init
        ; is_global } ->
        VarDecl
          { decl_type= Middle.Type.map no_parens d
          ; transformation= Middle.Program.map_transformation keep_parens t
          ; identifier
          ; initial_value= Option.map ~f:no_parens init
          ; is_global }
    | For {loop_variable; lower_bound; upper_bound; loop_body} ->
        For
          { loop_variable
          ; lower_bound= keep_parens lower_bound
          ; upper_bound= keep_parens upper_bound
          ; loop_body= parens_stmt loop_body }
    | _ -> map_statement no_parens parens_stmt parens_lval ident stmt
  in
  {stmt; smeta}

let repair_syntax program : untyped_program =
  map_program
    (repair_syntax_stmt (userdef_distributions program.functionblock))
    program

let canonicalize_program program : typed_program =
  program
  |> map_program
       (replace_deprecated_stmt (collect_userdef_distributions program))
  |> map_program parens_stmt
