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

let rec replace_deprecated_expr {expr; emeta} =
  let expr =
    match expr with
    | FunApp (StanLib suffix, {name; id_loc}, e) ->
        FunApp
          ( StanLib suffix
          , {name= rename_deprecated deprecated_functions name; id_loc}
          , List.map ~f:replace_deprecated_expr e )
    | FunApp (UserDefined suffix, {name; id_loc}, e) ->
        FunApp
          ( UserDefined suffix
          , {name; id_loc}
          , List.map ~f:replace_deprecated_expr e )
    | PrefixOp (PNot, e) -> PrefixOp (PNot, replace_boolean_real ~parens:true e)
    | BinOp (e1, ((And | Or) as op), e2) ->
        BinOp
          ( replace_boolean_real ~parens:true e1
          , op
          , replace_boolean_real ~parens:true e2 )
    | _ -> map_expression replace_deprecated_expr Fn.id expr in
  {expr; emeta}

and replace_boolean_real ?(parens = false) e =
  match e with
  | {emeta= {type_= UReal; _}; _} when parens ->
      { emeta= {e.emeta with type_= UInt}
      ; expr= Paren (replace_boolean_real ~parens:false e) }
  | {emeta= {type_= UReal; _}; _} ->
      { emeta= {e.emeta with type_= UInt}
      ; expr=
          BinOp
            ( replace_deprecated_expr e
            , NEquals
            , { expr= RealNumeral "0.0"
              ; emeta=
                  { type_= UInt
                  ; loc= Middle.Location_span.empty
                  ; ad_level= DataOnly } } ) }
  | _ -> replace_deprecated_expr e

let rec replace_deprecated_lval {lval; lmeta} =
  let is_multiindex = function
    | Single {emeta= {type_= Middle.UnsizedType.UInt; _}; _} -> false
    | _ -> true in
  let rec flatten_multi = function
    | LVariable id -> (LVariable id, None)
    | LTupleProjection (lval, idx) ->
        (LTupleProjection (replace_deprecated_lval lval, idx), None)
    | LIndexed ({lval; lmeta}, idcs) -> (
        let outer = List.map idcs ~f:(map_index replace_deprecated_expr) in
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

let rec replace_deprecated_stmt ({stmt; smeta} : typed_statement) =
  let stmt =
    match stmt with
    | FunDef {returntype; funname; arguments; body} ->
        FunDef
          {returntype; funname; arguments; body= replace_deprecated_stmt body}
    | IfThenElse (({emeta= {type_= UReal; _}; _} as cond), ifb, elseb) ->
        IfThenElse
          ( replace_boolean_real cond
          , replace_deprecated_stmt ifb
          , Option.map ~f:replace_deprecated_stmt elseb )
    | While (({emeta= {type_= UReal; _}; _} as cond), body) ->
        While (replace_boolean_real cond, replace_deprecated_stmt body)
    | _ ->
        map_statement replace_deprecated_expr replace_deprecated_stmt
          replace_deprecated_lval Fn.id stmt in
  {stmt; smeta}

let rec no_parens {expr; emeta} =
  match expr with
  | Paren e -> no_parens e
  | Variable _ | IntNumeral _ | RealNumeral _ | ImagNumeral _ | GetTarget ->
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

let canonicalize_program program settings : typed_program =
  let program =
    if settings.deprecations then
      remove_unneeded_forward_decls program
      |> map_program replace_deprecated_stmt
    else program in
  let program =
    if settings.parentheses then program |> map_program parens_stmt else program
  in
  let program =
    if settings.braces then program |> map_program blocks_stmt else program
  in
  program
