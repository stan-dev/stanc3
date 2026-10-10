open Std
open Middle
open Mir_utils

type sequential =
  {const: int; symbol: Expr.Typed.t option; loopvar: string option}

type varying_kind = Written | Nonsequential
type index = Sequential of sequential | Varying of varying_kind
type 'index step = Subscript of 'index Index.t | Field of int
type kind = Read | Write | Increment [@@deriving equal]
type 'index access = {var: string; path: 'index step list; kind: kind}
type 'index t = 'index access list

(* Decomposition must fit one symbol and one loop variable, each with
   coefficient one. *)
module Sequential = struct
  let add (left : sequential) (right : sequential) : sequential option =
    match ((left.symbol, right.symbol), (left.loopvar, right.loopvar)) with
    | (Some _, Some _), _ | _, (Some _, Some _) -> None
    | (left_symbol, right_symbol), (left_loopvar, right_loopvar) ->
        Some
          { const= left.const + right.const
          ; symbol= Option.first_some left_symbol right_symbol
          ; loopvar= Option.first_some left_loopvar right_loopvar }

  (* Negative symbolic and loop terms cannot be represented; matching terms
     cancel. *)
  let subtract (left : sequential) (right : sequential) : sequential option =
    match (right.symbol, right.loopvar) with
    | Some _, _
      when not (Option.equal Expr.Typed.equal left.symbol right.symbol) ->
        None
    | _, Some _ when not (Option.equal String.equal left.loopvar right.loopvar)
      ->
        None
    | right_symbol, right_loopvar ->
        Some
          { const= left.const - right.const
          ; symbol= (if Option.is_none right_symbol then left.symbol else None)
          ; loopvar=
              (if Option.is_none right_loopvar then left.loopvar else None) }
end

let classify_index ~(loopvars : string Set.Poly.t)
    ~(written_vars : string Set.Poly.t) (expr : Expr.Typed.t) : index =
  (* A written input prevents treating the expression as invariant. Otherwise,
     an unsupported use of a loop variable prevents storing a sequential
     form. *)
  let classify_whole_expression (subexpr : Expr.Typed.t) : index =
    let read_names = expr_var_names_set subexpr in
    let reads_written_variable =
      not (Set.Poly.disjoint read_names written_vars) in
    let reads_loop_variable = not (Set.Poly.disjoint read_names loopvars) in
    if reads_written_variable then Varying Written
    else if reads_loop_variable then Varying Nonsequential
    else Sequential {const= 0; symbol= Some subexpr; loopvar= None} in
  let rec classify (subexpr : Expr.Typed.t) : index =
    let combine sequential_op lhs rhs =
      match (classify lhs, classify rhs) with
      | Sequential left_term, Sequential right_term -> (
          match sequential_op left_term right_term with
          | Some term -> Sequential term
          | None -> classify_whole_expression subexpr)
      | Varying _, _ | _, Varying _ -> classify_whole_expression subexpr in
    match subexpr.pattern with
    | Var name when Set.Poly.mem name loopvars ->
        Sequential {const= 0; symbol= None; loopvar= Some name}
    | Lit (Int, digits) -> (
        match Int.of_string_opt digits with
        | Some const -> Sequential {const; symbol= None; loopvar= None}
        | None ->
            (* The frontend rejects integer literals outside the OCaml int range
               before index classification. *)
            Common.ICE.internal_error
              ("Integer literal does not fit an int: " ^ digits) [@coverage off]
        )
    | FunApp (Operator Plus, [lhs; rhs]) -> combine Sequential.add lhs rhs
    | FunApp (Operator Minus, [lhs; rhs]) -> combine Sequential.subtract lhs rhs
    | Var _ | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _
     |TupleProjection _ | Promotion _ ->
        classify_whole_expression subexpr in
  classify expr

module Path = struct
  let classify ~loopvars ~written_vars (path : Expr.Typed.t step list) :
      index step list =
    List.map path ~f:(function
      | Subscript index ->
          Subscript (Index.map (classify_index ~loopvars ~written_vars) index)
      | Field field -> Field field)

  let rec of_expr (expr : Expr.Typed.t) :
      (string * Expr.Typed.t step list) option =
    let extend_path base steps =
      (* After a slice or multi-index, further subscripts select the result of
         the selection, so concatenating paths would change the coordinates. *)
      match of_expr base with
      | Some (name, prefix)
        when List.for_all prefix ~f:(function
               | Subscript (Single _) | Field _ -> true
               | Subscript _ -> false) ->
          Some (name, prefix @ steps)
      | Some _ | None -> None in
    match expr.pattern with
    | Var name -> Some (name, [])
    | Indexed (base, indices) ->
        extend_path base (List.map indices ~f:(fun index -> Subscript index))
    | TupleProjection (base, field) -> extend_path base [Field field]
    | FunApp _ | TernaryIf _ | Lit _ | EAnd _ | EOr _ | Promotion _ -> None

  let rec of_lvalue ((lbase, indices) : Expr.Typed.t Stmt.Pattern.lvalue) :
      Expr.Typed.t step list =
    (match lbase with
      | LVariable _ -> []
      | LTupleProjection (inner, field) -> of_lvalue inner @ [Field field])
    @ List.map indices ~f:(fun index -> Subscript index)

  let bounds (path : Expr.Typed.t step list) : Expr.Typed.t list =
    List.concat_map path ~f:(function
      | Subscript index -> Index.bounds index
      | Field _ -> [])
end

let of_var (var : string) (accesses : 'index t) : 'index t =
  List.filter accesses ~f:(fun access -> String.equal access.var var)

let read_vars (accesses : 'index t) : string Set.Poly.t =
  Set.Poly.of_list
    (List.filter_map accesses ~f:(fun {var; kind; _} ->
         match kind with Read | Increment -> Some var | Write -> None))

let written_vars (accesses : 'index t) : string Set.Poly.t =
  Set.Poly.of_list
    (List.filter_map accesses ~f:(fun {var; kind; _} ->
         match kind with Write | Increment -> Some var | Read -> None))

let increment_target : 'index access = {var= "target"; path= []; kind= Increment}

let rec of_expr (expr : Expr.Typed.t) : Expr.Typed.t t =
  let of_children () =
    Expr.Pattern.fold
      (fun running_accesses subexpr -> running_accesses @ of_expr subexpr)
      [] expr.pattern in
  let read var path = [{var; path; kind= Read}] in
  match expr.pattern with
  | Var name -> read name []
  | Indexed _ | TupleProjection _ -> (
      match Path.of_expr expr with
      | Some (name, path) ->
          read name path @ List.concat_map (Path.bounds path) ~f:of_expr
      | None -> of_children ())
  | FunApp (StanLib (_, FnTarget), []) -> read "target" []
  | FunApp (fun_kind, _) when increments_target fun_kind ->
      of_children () @ [increment_target]
  | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ ->
      of_children ()

let of_stmt (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) :
    Expr.Typed.t t =
  (* The statement fold visits expressions but skips nested statements. *)
  let of_children () =
    Stmt.Pattern.fold
      (fun running_accesses subexpr -> running_accesses @ of_expr subexpr)
      Fun.const [] stmt in
  let write var path = [{var; path; kind= Write}] in
  match stmt with
  | Assignment (lhs, _, rhs) ->
      let path = Path.of_lvalue lhs in
      List.concat_map (Path.bounds path @ [rhs]) ~f:of_expr
      @ write (Stmt.Helpers.lhs_variable lhs) path
  | Decl {decl_id; _} -> of_children () @ write decl_id []
  | TargetPE _ | JacobianPE _ -> of_children () @ [increment_target]
  | NRFunApp (fun_kind, _) when increments_target fun_kind ->
      of_children () @ [increment_target]
  | NRFunApp _ | Return _ | IfElse _ | While _ | For _ | Profile _ | Block _
   |SList _ | Break | Continue | Skip ->
      of_children ()

let classify ~loopvars ~written_vars (accesses : Expr.Typed.t t) : index t =
  (* Dependencies on enclosing loops supply their bounds, so loop-variable reads
     need no separate assignment dependency. *)
  let accesses_without_loop_reads =
    List.filter accesses ~f:(fun access ->
        match access.kind with
        | Read -> not (Set.Poly.mem access.var loopvars)
        | Write | Increment -> true) in
  List.map accesses_without_loop_reads ~f:(fun access ->
      {access with path= Path.classify ~loopvars ~written_vars access.path})

(* [leading] indicates that no loop variable has been printed. *)
let pp_sequential ~leading ppf ({const; symbol; _} : sequential) =
  let sign ~first value = if value < 0 then "-" else if first then "" else "+" in
  Option.iter symbol ~f:(fun symbol ->
      Fmt.pf ppf "%s%a" (sign ~first:leading 1) Expr.Typed.pp symbol);
  let bare = Option.is_none symbol in
  if const <> 0 || (leading && bare) then
    Fmt.pf ppf "%s%d" (sign ~first:(leading && bare) const) (abs const)

let pp_varying_kind ppf = function
  | Written -> Fmt.string ppf "written"
  | Nonsequential -> Fmt.string ppf "nonsequential"

let pp_index ppf = function
  | Sequential ({loopvar; _} as term) ->
      Option.iter loopvar ~f:(Fmt.string ppf);
      pp_sequential ~leading:(Option.is_none loopvar) ppf term
  | Varying kind -> Fmt.pf ppf "?%a" pp_varying_kind kind

let pp_subscript pp_index ppf (index : 'index Index.t) =
  match index with
  | MultiIndex indices -> Fmt.pf ppf "{%a}" pp_index indices
  | All | Single _ | Upfrom _ | Between _ -> Index.pp pp_index ppf index

let rec pp_path pp_index ppf = function
  | [] -> ()
  | Field field :: rest -> Fmt.pf ppf ".%d%a" field (pp_path pp_index) rest
  | Subscript index :: rest ->
      Fmt.pf ppf "[%a%a" (pp_subscript pp_index) index
        (pp_subscripts_after pp_index)
        rest

and pp_subscripts_after pp_index ppf = function
  | Subscript index :: rest ->
      Fmt.pf ppf ", %a%a" (pp_subscript pp_index) index
        (pp_subscripts_after pp_index)
        rest
  | path -> Fmt.pf ppf "]%a" (pp_path pp_index) path

let pp_kind ppf kind =
  Fmt.string ppf
    (match kind with Read -> "R" | Increment -> "+=" | Write -> "W")

let pp_access pp_index ppf {var; path; kind} =
  Fmt.pf ppf "%a %s%a" pp_kind kind var (pp_path pp_index) path

let pp pp_index ppf (accesses : 'index t) =
  let of_kind wanted =
    List.filter accesses ~f:(fun access -> equal_kind access.kind wanted) in
  Fmt.(list ~sep:(any ", ") (pp_access pp_index))
    ppf
    (of_kind Read @ of_kind Increment @ of_kind Write)
