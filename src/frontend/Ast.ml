(** Abstract syntax tree for Stan. Defined with the
  'two-level types' pattern, where the variant types are not
  directly recursive, but rather parametric in some other type.

  This type ends up being substituted for the fixpoint of the recursive
  type itself including metadata. So instead of recursively referencing
  [expression] you would instead reference type parameter ['e], which will
  later be filled in with something like [type expr_with_meta = metadata expression]
*)

open Core_kernel
open Middle

(** Our type for identifiers, on which we record a location *)
type identifier =
  {name: string; id_loc: (Location_span.t[@sexp.opaque] [@compare.ignore])}
[@@deriving sexp, hash, compare]

(** Indices for array access *)
type 'e index =
  | All
  | Single of 'e
  | Upfrom of 'e
  | Downfrom of 'e
  | Between of 'e * 'e
[@@deriving sexp, hash, compare, map, fold]

(** Front-end function kinds *)
type fun_kind =
  | StanLib of bool Fun_kind.suffix
  | UserDefined of bool Fun_kind.suffix
[@@deriving compare, sexp, hash]

(** Expression shapes (used for both typed and untyped expressions, where we
    substitute untyped_expression or typed_expression for 'e *)
type ('e, 'f) expression =
  | TernaryIf of 'e * 'e * 'e
  | BinOp of 'e * Operator.t * 'e
  | PrefixOp of Operator.t * 'e
  | PostfixOp of 'e * Operator.t
  | Variable of identifier
  | IntNumeral of string
  | RealNumeral of string
  | ImagNumeral of string
  | FunApp of 'f * identifier * 'e list
  | CondDistApp of 'f * identifier * 'e list
  | Promotion of 'e * UnsizedType.t * UnsizedType.autodifftype
  (* GetLP is deprecated *)
  | GetLP
  | GetTarget
  | ArrayExpr of 'e list
  | RowVectorExpr of 'e list
  | Paren of 'e
  | Indexed of 'e * 'e index list
[@@deriving sexp, hash, compare, map, fold]

type ('m, 'f) expr_with = {expr: (('m, 'f) expr_with, 'f) expression; emeta: 'm}
[@@deriving sexp, compare, map, hash, fold]

(** Untyped expressions, which have location_spans as meta-data *)
type located_meta = {loc: (Location_span.t[@sexp.opaque] [@compare.ignore])}
[@@deriving sexp, compare, map, hash, fold]

type untyped_expression = (located_meta, unit) expr_with
[@@deriving sexp, compare, map, hash, fold]

(** Typed expressions also have meta-data after type checking: a location_span, as well as a type
    and an origin block (lub of the origin blocks of the identifiers in it) *)
type typed_expr_meta =
  { loc: (Location_span.t[@sexp.opaque] [@compare.ignore])
  ; ad_level: UnsizedType.autodifftype
  ; type_: UnsizedType.t }
[@@deriving sexp, compare, map, hash, fold]

type typed_expression = (typed_expr_meta, fun_kind) expr_with
[@@deriving sexp, compare, map, hash, fold]

let mk_untyped_expression ~expr ~loc = {expr; emeta= {loc}}

let mk_typed_expression ~expr ~loc ~type_ ~ad_level =
  {expr; emeta= {loc; type_; ad_level}}

let expr_loc_lub exprs =
  match List.map ~f:(fun e -> e.emeta.loc) exprs with
  | [] -> Location_span.empty
  | [hd] -> hd
  | x1 :: tl -> List.fold ~init:x1 ~f:Location_span.merge tl

(** Least upper bound of expression autodiff types *)
let expr_ad_lub exprs =
  exprs |> List.map ~f:(fun x -> x.emeta.ad_level) |> UnsizedType.lub_ad_type

(** Assignment operators *)
type assignmentoperator =
  | Assign
  (* ArrowAssign is deprecated *)
  | ArrowAssign
  | OperatorAssign of Operator.t
[@@deriving sexp, hash, compare]

(** Truncations *)
type 'e truncation =
  | NoTruncate
  | TruncateUpFrom of 'e
  | TruncateDownFrom of 'e
  | TruncateBetween of 'e * 'e
[@@deriving sexp, hash, compare, map, fold]

(** Things that can be printed *)
type 'e printable = PString of string | PExpr of 'e
[@@deriving sexp, compare, map, hash, fold]

type ('l, 'e) lvalue =
  | LVariable of identifier
  | LIndexed of 'l * 'e index list
[@@deriving sexp, hash, compare, map, fold]

type ('e, 'm) lval_with = {lval: (('e, 'm) lval_with, 'e) lvalue; lmeta: 'm}
[@@deriving sexp, hash, compare, map, fold]

type untyped_lval = (untyped_expression, located_meta) lval_with
[@@deriving sexp, hash, compare, map, fold]

type typed_lval = (typed_expression, typed_expr_meta) lval_with
[@@deriving sexp, hash, compare, map, fold]

(** Statement shapes, where we substitute untyped_expression and untyped_statement
    for 'e and 's respectively to get untyped_statement and typed_expression and
    typed_statement to get typed_statement    *)
type ('e, 's, 'l, 'f) statement =
  | Assignment of {assign_lhs: 'l; assign_op: assignmentoperator; assign_rhs: 'e}
  | NRFunApp of 'f * identifier * 'e list
  | TargetPE of 'e
  (* IncrementLogProb is deprecated *)
  | IncrementLogProb of 'e
  | Tilde of
      { arg: 'e
      ; distribution: identifier
      ; args: 'e list
      ; truncation: 'e truncation }
  | Break
  | Continue
  | Return of 'e
  | ReturnVoid
  | Print of 'e printable list
  | Reject of 'e printable list
  | Skip
  | IfThenElse of 'e * 's * 's option
  | While of 'e * 's
  | For of
      { loop_variable: identifier
      ; lower_bound: 'e
      ; upper_bound: 'e
      ; loop_body: 's }
  | ForEach of identifier * 'e * 's
  | Profile of string * 's list
  | Block of 's list
  | VarDecl of
      { decl_type: 'e Middle.Type.t
      ; transformation: 'e Transformation.t
      ; identifier: identifier
      ; initial_value: 'e option
      ; is_global: bool }
  | FunDef of
      { returntype: Middle.UnsizedType.returntype
      ; funname: identifier
      ; arguments:
          (Middle.UnsizedType.autodifftype * Middle.UnsizedType.t * identifier)
          list
      ; body: 's }
[@@deriving sexp, hash, compare, map, fold]

(** Statement return types which we will decorate statements with during type
    checking: the purpose is to check that function bodies have the correct
    return type in every possible execution branch.
    NoReturnType corresponds to not having a return statement in it.
    Incomplete rt corresponds to having some return statement(s) of type rt
    in it, but not one in every branch
    Complete rt corresponds to having a return statement of type rt in every branch
    AnyReturnType corresponds to statements which have an error in every branch  *)
type statement_returntype =
  | NoReturnType
  | Incomplete of Middle.UnsizedType.returntype
  | Complete of Middle.UnsizedType.returntype
  | AnyReturnType
[@@deriving sexp, hash, compare]

type ('e, 'm, 'l, 'f) statement_with =
  {stmt: ('e, ('e, 'm, 'l, 'f) statement_with, 'l, 'f) statement; smeta: 'm}
[@@deriving sexp, compare, map, hash, fold]

(** Untyped statements, which have location_spans as meta-data *)
type untyped_statement =
  (untyped_expression, located_meta, untyped_lval, unit) statement_with
[@@deriving sexp, compare, map, hash]

let mk_untyped_statement ~stmt ~loc : untyped_statement = {stmt; smeta= {loc}}

type stmt_typed_located_meta =
  { loc: (Middle.Location_span.t[@sexp.opaque] [@compare.ignore])
  ; return_type: statement_returntype }
[@@deriving sexp, compare, map, hash]

(** Typed statements also have meta-data after type checking: a location_span, as well as a statement returntype
    to check that function bodies have the right return type*)
type typed_statement =
  ( typed_expression
  , stmt_typed_located_meta
  , typed_lval
  , fun_kind )
  statement_with
[@@deriving sexp, compare, map, hash]

let mk_typed_statement ~stmt ~loc ~return_type =
  {stmt; smeta= {loc; return_type}}

(** Program shapes, where we obtain types of programs if we substitute typed or untyped
    statements for 's *)
type 's block = {stmts: 's list; xloc: Middle.Location_span.t [@ignore]}

and comment_type =
  | LineComment of string * Middle.Location_span.t
  | Include of string * Middle.Location_span.t
  | BlockComment of string list * Middle.Location_span.t
  | Separator of Middle.Location.t
      (** Separator records the location of items like commas, operators, and keywords
          which don't have location information stored in the AST
          but are useful for placing comments in pretty printing *)

and 's program =
  { functionblock: 's block option
  ; datablock: 's block option
  ; transformeddatablock: 's block option
  ; parametersblock: 's block option
  ; transformedparametersblock: 's block option
  ; modelblock: 's block option
  ; generatedquantitiesblock: 's block option
  ; comments: (comment_type list[@sexp.opaque] [@ignore]) }
[@@deriving sexp, hash, compare, map, fold]

let get_stmts = Option.value_map ~default:[] ~f:(fun x -> x.stmts)

(** Untyped programs (before type checking) *)
type untyped_program = untyped_statement program [@@deriving sexp, compare, map]

(** Typed programs (after type checking) *)
type typed_program = typed_statement program [@@deriving sexp, compare, map]

(*========================== Helper functions ===============================*)

(** Forgetful function from typed to untyped expressions *)
let rec untyped_expression_of_typed_expression ({expr; emeta} : typed_expression)
    : untyped_expression =
  match expr with
  | Promotion (e, _, _) -> untyped_expression_of_typed_expression e
  | _ ->
      { expr=
          map_expression untyped_expression_of_typed_expression
            (fun _ -> ())
            expr
      ; emeta= {loc= emeta.loc} }

let rec untyped_lvalue_of_typed_lvalue ({lval; lmeta} : typed_lval) :
    untyped_lval =
  { lval=
      map_lvalue untyped_lvalue_of_typed_lvalue
        untyped_expression_of_typed_expression lval
  ; lmeta= {loc= lmeta.loc} }

(** Forgetful function from typed to untyped statements *)
let rec untyped_statement_of_typed_statement {stmt; smeta} =
  { stmt=
      map_statement untyped_expression_of_typed_expression
        untyped_statement_of_typed_statement untyped_lvalue_of_typed_lvalue
        (fun _ -> ())
        stmt
  ; smeta= {loc= smeta.loc} }

(** Forgetful function from typed to untyped programs *)
let untyped_program_of_typed_program : typed_program -> untyped_program =
  map_program untyped_statement_of_typed_statement

let rec expr_of_lvalue {lval; lmeta} =
  { expr=
      ( match lval with
      | LVariable s -> Variable s
      | LIndexed (l, i) -> Indexed (expr_of_lvalue l, i) )
  ; emeta= lmeta }

let rec lvalue_of_expr {expr; emeta} =
  { lval=
      ( match expr with
      | Variable s -> LVariable s
      | Indexed (l, i) -> LIndexed (lvalue_of_expr l, i)
      | _ ->
          Common.FatalError.fatal_error_msg
            [%message "Trying to convert illegal expression to lval."] )
  ; lmeta= emeta }

let rec id_of_lvalue {lval; _} =
  match lval with LVariable s -> s | LIndexed (l, _) -> id_of_lvalue l

(* XXX: the parser produces inaccurate locations: smeta.loc.begin_loc is the last
        token before the current statement and all the whitespace between two statements
        appears as if it were part of the second statement.
        get_first_loc tries to skip the leading whitespace and approximate the location
        of the first token in the statement.
    TODO: See if $sloc works better than $loc for this
*)

let rec get_loc_expr (e : untyped_expression) =
  match e.expr with
  | TernaryIf (e, _, _)
   |BinOp (e, _, _)
   |PostfixOp (e, _)
   |Indexed (e, _)
   |Promotion (e, _, _) ->
      get_loc_expr e
  | PrefixOp (_, e) | ArrayExpr (e :: _) | RowVectorExpr (e :: _) | Paren e ->
      e.emeta.loc.begin_loc
  | Variable _ | IntNumeral _ | RealNumeral _ | ImagNumeral _ | GetLP
   |GetTarget
   |ArrayExpr []
   |RowVectorExpr [] ->
      e.emeta.loc.end_loc
  | FunApp (_, id, _) | CondDistApp (_, id, _) -> id.id_loc.end_loc

let get_loc_dt (t : untyped_expression Type.t) =
  match t with
  | Type.Unsized _ | Sized (SInt | SReal | SComplex) -> None
  | Sized
      (SVector (_, e) | SRowVector (_, e) | SMatrix (_, e, _) | SArray (_, e))
    ->
      Some e.emeta.loc.begin_loc

let get_loc_tf (t : untyped_expression Transformation.t) =
  match t with
  | Lower e
   |Upper e
   |LowerUpper (e, _)
   |Offset e
   |Multiplier e
   |OffsetMultiplier (e, _) ->
      Some e.emeta.loc.begin_loc
  | _ -> None

let get_first_loc (s : untyped_statement) =
  match s.stmt with
  | NRFunApp (_, id, _)
   |For {loop_variable= id; _}
   |ForEach (id, _, _)
   |FunDef {funname= id; _} ->
      id.id_loc.begin_loc
  | TargetPE e
   |IncrementLogProb e
   |Return e
   |IfThenElse (e, _, _)
   |While (e, _) ->
      e.emeta.loc.begin_loc
  | Assignment _ | Profile _ | Block _ | Tilde _ | Break | Continue
   |ReturnVoid | Print _ | Reject _ | Skip ->
      s.smeta.loc.begin_loc
  | VarDecl {decl_type; transformation; identifier; _} -> (
    match get_loc_dt decl_type with
    | Some loc -> loc
    | None -> (
      match get_loc_tf transformation with
      | Some loc -> loc
      | None -> identifier.id_loc.begin_loc ) )
