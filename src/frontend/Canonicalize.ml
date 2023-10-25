open Core
open Ast
open Deprecation_analysis

type canonicalizer_settings =
  { deprecations: bool
  ; parentheses: bool
  ; braces: bool
  ; inline_includes: bool
  ; strip_comments: bool }

let legacy =
  { deprecations= true
  ; parentheses= true
  ; inline_includes= true
  ; braces= true
  ; strip_comments= false }

let none =
  { deprecations= false
  ; parentheses= false
  ; inline_includes= false
  ; braces= false
  ; strip_comments= false }

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
          map_statement Fn.id (repair_syntax_stmt user_dists) Fn.id Fn.id stmt
      ; smeta }

let rec replace_deprecated_expr
    (deprecated_userdefined : Middle.UnsizedType.t Core.String.Map.t)
    {expr; emeta} =
  let expr =
    match expr with
    | GetLP -> GetTarget
    | FunApp (StanLib FnPlain, {name= "if_else"; _}, [c; t; e]) ->
        Paren
          (replace_deprecated_expr deprecated_userdefined
             {expr= TernaryIf ({expr= Paren c; emeta= c.emeta}, t, e); emeta})
    | FunApp (StanLib suffix, {name; id_loc}, e) ->
        if is_deprecated_distribution name then
          CondDistApp
            ( StanLib suffix
            , {name= rename_deprecated deprecated_distributions name; id_loc}
            , List.map ~f:(replace_deprecated_expr deprecated_userdefined) e )
        else if String.is_suffix name ~suffix:"_cdf" then
          CondDistApp
            ( StanLib suffix
            , {name; id_loc}
            , List.map ~f:(replace_deprecated_expr deprecated_userdefined) e )
        else
          FunApp
            ( StanLib suffix
            , {name= rename_deprecated deprecated_functions name; id_loc}
            , List.map ~f:(replace_deprecated_expr deprecated_userdefined) e )
    | FunApp (UserDefined suffix, {name; id_loc}, e) -> (
        match Map.find deprecated_userdefined name with
        | Some type_ ->
            CondDistApp
              ( UserDefined suffix
              , {name= update_suffix name type_; id_loc}
              , List.map ~f:(replace_deprecated_expr deprecated_userdefined) e
              )
        | None ->
            if String.is_suffix name ~suffix:"_cdf" then
              CondDistApp
                ( UserDefined suffix
                , {name; id_loc}
                , List.map ~f:(replace_deprecated_expr deprecated_userdefined) e
                )
            else
              FunApp
                ( UserDefined suffix
                , {name; id_loc}
                , List.map ~f:(replace_deprecated_expr deprecated_userdefined) e
                ))
    | PrefixOp (PNot, e) ->
        PrefixOp
          (PNot, replace_boolean_real ~parens:true deprecated_userdefined e)
    | BinOp (e1, ((And | Or) as op), e2) ->
        BinOp
          ( replace_boolean_real ~parens:true deprecated_userdefined e1
          , op
          , replace_boolean_real ~parens:true deprecated_userdefined e2 )
    | _ ->
        map_expression
          (replace_deprecated_expr deprecated_userdefined)
          Fn.id expr in
  {expr; emeta}

and replace_boolean_real ?(parens = false) deprecated_userdefined e =
  match e with
  | {emeta= {type_= UReal; _}; _} when parens ->
      { emeta= {e.emeta with type_= UInt}
      ; expr=
          Paren (replace_boolean_real ~parens:false deprecated_userdefined e) }
  | {emeta= {type_= UReal; _}; _} ->
      { emeta= {e.emeta with type_= UInt}
      ; expr=
          BinOp
            ( replace_deprecated_expr deprecated_userdefined e
            , NEquals
            , { expr= RealNumeral "0.0"
              ; emeta=
                  { type_= UInt
                  ; loc= Middle.Location_span.empty
                  ; ad_level= DataOnly } } ) }
  | _ -> replace_deprecated_expr deprecated_userdefined e

let rec replace_deprecated_lval deprecated_userdefined {lval; lmeta} =
  let is_multiindex = function
    | Single {emeta= {type_= Middle.UnsizedType.UInt; _}; _} -> false
    | _ -> true in
  let rec flatten_multi = function
    | LVariable id -> (LVariable id, None)
    | LTupleProjection (lval, idx) ->
        ( LTupleProjection
            (replace_deprecated_lval deprecated_userdefined lval, idx)
        , None )
    | LIndexed ({lval; lmeta}, idcs) -> (
        let outer =
          List.map idcs
            ~f:(map_index (replace_deprecated_expr deprecated_userdefined))
        in
        let unwrap = Option.value_map ~default:[] ~f:fst in
        match flatten_multi lval with
        | lval, inner when List.exists ~f:is_multiindex outer ->
            (lval, Some (unwrap inner @ outer, lmeta))
        | lval, None -> (LIndexed ({lval; lmeta}, outer), None)
        | lval, Some (inner, _) -> (lval, Some (inner @ outer, lmeta))) in
  let lval =
    match flatten_multi lval with
    | lval, None -> lval
    | lval, Some (idcs, lmeta) -> LIndexed ({lval; lmeta}, idcs) in
  {lval; lmeta}

let rec replace_deprecated_lval_pack deprecated_userdefined = function
  | LValue lv -> LValue (replace_deprecated_lval deprecated_userdefined lv)
  | LTuplePack {lvals; loc} ->
      LTuplePack
        { lvals=
            List.map
              ~f:(replace_deprecated_lval_pack deprecated_userdefined)
              lvals
        ; loc }

let rec replace_deprecated_stmt
    (deprecated_userdefined : Middle.UnsizedType.t Core.String.Map.t)
    ({stmt; smeta} : typed_statement) =
  let stmt =
    match stmt with
    | IncrementLogProb e ->
        TargetPE (replace_deprecated_expr deprecated_userdefined e)
    | Assignment {assign_lhs= l; assign_op= ArrowAssign; assign_rhs= e} ->
        Assignment
          { assign_lhs= replace_deprecated_lval_pack deprecated_userdefined l
          ; assign_op= Assign
          ; assign_rhs= (replace_deprecated_expr deprecated_userdefined) e }
    | FunDef {returntype; funname= {name; id_loc}; arguments; body} ->
        let newname =
          match Map.find deprecated_userdefined name with
          | Some type_ -> update_suffix name type_
          | None -> name in
        FunDef
          { returntype
          ; funname= {name= newname; id_loc}
          ; arguments
          ; body= replace_deprecated_stmt deprecated_userdefined body }
    | IfThenElse (({emeta= {type_= UReal; _}; _} as cond), ifb, elseb) ->
        IfThenElse
          ( replace_boolean_real deprecated_userdefined cond
          , replace_deprecated_stmt deprecated_userdefined ifb
          , Option.map ~f:(replace_deprecated_stmt deprecated_userdefined) elseb
          )
    | While (({emeta= {type_= UReal; _}; _} as cond), body) ->
        While
          ( replace_boolean_real deprecated_userdefined cond
          , replace_deprecated_stmt deprecated_userdefined body )
    | _ ->
        map_statement
          (replace_deprecated_expr deprecated_userdefined)
          (replace_deprecated_stmt deprecated_userdefined)
          (replace_deprecated_lval deprecated_userdefined)
          Fn.id stmt in
  {stmt; smeta}

let rec no_parens {expr; emeta} =
  match expr with
  | Paren e -> no_parens e
  | Variable _ | IntNumeral _ | RealNumeral _ | ImagNumeral _ | GetLP
   |GetTarget ->
      {expr; emeta}
  | BinOp (({expr= BinOp (_, op1, _); _} as e1), op2, e2)
    when Middle.Operator.(is_cmp op1 && is_cmp op2) ->
      { expr= BinOp ({e1 with expr= Paren (no_parens e1)}, op2, keep_parens e2)
      ; emeta }
  | TernaryIf _ | BinOp _ | PrefixOp _ | PostfixOp _ ->
      {expr= map_expression keep_parens Fn.id expr; emeta}
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
  | TupleProjection (e, i) -> {expr= TupleProjection (keep_parens e, i); emeta}
  | ArrayExpr _ | RowVectorExpr _ | FunApp _ | CondDistApp _ | TupleExpr _
   |Promotion _ ->
      {expr= map_expression no_parens Fn.id expr; emeta}

and keep_parens {expr; emeta} =
  match expr with
  | Promotion (e, ut, ad) -> {expr= Promotion (keep_parens e, ut, ad); emeta}
  | Paren ({expr= Paren _; _} as e) -> keep_parens e
  | Paren ({expr= BinOp _; _} as e)
   |Paren ({expr= PrefixOp _; _} as e)
   |Paren ({expr= PostfixOp _; _} as e)
   |Paren ({expr= TernaryIf _; _} as e) ->
      {expr= Paren (no_parens e); emeta}
  | _ -> no_parens {expr; emeta}

let parens_lval = map_lval_with no_parens Fn.id

let rec parens_stmt ({stmt; smeta} : typed_statement) : typed_statement =
  let stmt =
    match stmt with
    | VarDecl {decl_type= d; transformation= t; variables; is_global} ->
        VarDecl
          { decl_type= Middle.SizedType.map no_parens d
          ; transformation= Middle.Transformation.map keep_parens t
          ; variables= List.map ~f:(map_variable no_parens) variables
          ; is_global }
    | For {loop_variable; lower_bound; upper_bound; loop_body} ->
        For
          { loop_variable
          ; lower_bound= keep_parens lower_bound
          ; upper_bound= keep_parens upper_bound
          ; loop_body= parens_stmt loop_body }
    | _ -> map_statement no_parens parens_stmt parens_lval Fn.id stmt in
  {stmt; smeta}

let rec blocks_stmt ({stmt; smeta} : typed_statement) : typed_statement =
  let stmt_to_block ({stmt; smeta} : typed_statement) : typed_statement =
    match stmt with
    | Block _ -> blocks_stmt {stmt; smeta}
    | _ ->
        blocks_stmt
        @@ mk_typed_statement
             ~stmt:(Block [{stmt; smeta}])
             ~return_type:smeta.return_type ~loc:smeta.loc in
  let stmt =
    match stmt with
    | While (e, s) -> While (e, stmt_to_block s)
    | IfThenElse (e, s1, Some ({stmt= IfThenElse _; _} as s2))
     |IfThenElse (e, s1, Some {stmt= Block [({stmt= IfThenElse _; _} as s2)]; _})
      ->
        (* Flatten if ... else if ... constructs *)
        IfThenElse (e, stmt_to_block s1, Some (blocks_stmt s2))
    | IfThenElse (e, s1, s2) ->
        IfThenElse (e, stmt_to_block s1, Option.map ~f:stmt_to_block s2)
    | For ({loop_body; _} as f) ->
        For {f with loop_body= stmt_to_block loop_body}
    | _ -> map_statement Fn.id blocks_stmt Fn.id Fn.id stmt in
  {stmt; smeta}

let repair_syntax program settings =
  if settings.deprecations then
    program
    |> map_program
         (repair_syntax_stmt (userdef_distributions program.functionblock))
  else program

let canonicalize_program program settings : typed_program =
  let program =
    if settings.deprecations then
      remove_unneeded_forward_decls program
      |> map_program
           (replace_deprecated_stmt (collect_userdef_distributions program))
    else program in
  let program =
    if settings.parentheses then program |> map_program parens_stmt else program
  in
  let program =
    if settings.braces then program |> map_program blocks_stmt else program
  in
  program
