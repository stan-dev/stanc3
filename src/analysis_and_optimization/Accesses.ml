open Std
open Std.Compare
open Std.Sexp_conv
open Middle
open Mir_utils

type 'index step = Subscript of 'index Index.t | Field of int
type kind = Read | Write | Increment [@@deriving equal]
type 'index access = {var: string; path: 'index step list; kind: kind}
type 'index t = 'index access list

module Path = struct
  let rec of_expr (expr : Expr.Typed.t) :
      (string * Expr.Typed.t step list) option =
    let extend_path base steps =
      (* After a slice or a multi-index, later subscripts index the selected
         result, not the base variable. The path of [base] and [steps] then
         cannot join into one path of the base variable. *)
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
  (* Each nested statement has its own accesses. The statement fold thus visits
     only the expressions of [stmt] and skips nested statements. *)
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

type direction = Ascending | Descending [@@deriving equal]
type loop_term = {loopvar: String.t; direction: direction} [@@deriving equal]

type sequential =
  {const: int; symbol: Expr.Typed.t option; loop_term: loop_term option}

type varying_kind = Written | Unhandled
type index = Sequential of sequential | Varying of varying_kind

(* A sequential index is the sum of an integer constant, an optional symbol and
   an optional loop term. A symbol is an expression that reads no loop variable
   and no written variable. The symbol has coefficient 1. The loop term is a
   loop variable with coefficient 1 ([Ascending]) or -1 ([Descending]). Each
   operation returns [None] when its result does not have this form. *)
module Sequential = struct
  let add (left : sequential) (right : sequential) : sequential option =
    match ((left.symbol, right.symbol), (left.loop_term, right.loop_term)) with
    | (Some _, Some _), _ | _, (Some _, Some _) -> None
    | (left_symbol, right_symbol), (left_loop_term, right_loop_term) ->
        Some
          { const= left.const + right.const
          ; symbol= Option.first_some left_symbol right_symbol
          ; loop_term= Option.first_some left_loop_term right_loop_term }

  let negate_direction = function
    | Ascending -> Descending
    | Descending -> Ascending

  (* [subtract] calls this function before it negates [right]. When both sides
     have the same symbol, the symbols cancel, as in [(n + k) - k = n]. Equal
     loop terms cancel in the same way. Without this step, [negate] fails on the
     symbol of [right]. *)
  let cancel_matching (left : sequential) (right : sequential) :
      sequential * sequential =
    let symbols_match = Option.equal Expr.Typed.equal left.symbol right.symbol in
    let loop_terms_match =
      Option.equal equal_loop_term left.loop_term right.loop_term in
    let drop_matching (term : sequential) =
      { term with
        symbol= (if symbols_match then None else term.symbol)
      ; loop_term= (if loop_terms_match then None else term.loop_term) } in
    (drop_matching left, drop_matching right)

  (* The symbol has coefficient 1, so a sequential index cannot hold [-symbol].
     The function returns [None] in that case. A negated loop variable has the
     opposite direction, as [-n] has in [N - n]. *)
  let negate (term : sequential) : sequential option =
    match term.symbol with
    | Some _ -> None
    | None ->
        Some
          { const= -term.const
          ; symbol= None
          ; loop_term=
              Option.map term.loop_term ~f:(fun loop_term ->
                  { loop_term with
                    direction= negate_direction loop_term.direction }) }

  let subtract (left : sequential) (right : sequential) : sequential option =
    let left, right = cancel_matching left right in
    match negate right with
    | Some negated_right -> add left negated_right
    | None -> None
end

let classify_index ~(loopvars : string Set.Poly.t)
    ~(written_vars : string Set.Poly.t) (expr : Expr.Typed.t) : index =
  (* This function classifies [subexpr] as one unit, without its sum structure.
     A written variable can change between iterations, so a read of it makes the
     index vary. A loop variable in a form other than a sum, as in [2 * n], also
     makes the index vary. Any other expression is constant in the loops, so it
     becomes the symbol of a sequential index. *)
  let classify_whole_expression (subexpr : Expr.Typed.t) : index =
    let read_names = expr_var_names_set subexpr in
    let reads_written_variable =
      not (Set.Poly.disjoint read_names written_vars) in
    let reads_loop_variable = not (Set.Poly.disjoint read_names loopvars) in
    if reads_written_variable then Varying Written
    else if reads_loop_variable then Varying Unhandled
    else Sequential {const= 0; symbol= Some subexpr; loop_term= None} in
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
        Sequential
          { const= 0
          ; symbol= None
          ; loop_term= Some {loopvar= name; direction= Ascending} }
    | Lit (Int, digits) -> (
        match Int.of_string_opt digits with
        | Some const -> Sequential {const; symbol= None; loop_term= None}
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

let classify_path ~loopvars ~written_vars (path : Expr.Typed.t step list) :
    index step list =
  List.map path ~f:(function
    | Subscript index ->
        Subscript (Index.map (classify_index ~loopvars ~written_vars) index)
    | Field field -> Field field)

let classify ~loopvars ~written_vars (accesses : Expr.Typed.t t) : index t =
  (* A dependency query reaches each enclosing loop, and the bounds of that
     loop, through the control dependency on the loop. No statement writes a
     loop variable, so a search for writes to it finds nothing. Thus the reads
     of loop variables add no information, and this function removes them. *)
  let accesses_without_loop_reads =
    List.filter accesses ~f:(fun access ->
        match access.kind with
        | Read -> not (Set.Poly.mem access.var loopvars)
        | Write | Increment -> true) in
  List.map accesses_without_loop_reads ~f:(fun access ->
      {access with path= classify_path ~loopvars ~written_vars access.path})

type step_comparison =
  | Same_every_iteration
  | Same_in_one_iteration
  | Different
  | May_differ

let compare_steps (left : index step) (right : index step) : step_comparison =
  match (left, right) with
  | Field left_field, Field right_field ->
      if left_field = right_field then Same_every_iteration else Different
  | ( Subscript (Single (Sequential left_term))
    , Subscript (Single (Sequential right_term)) )
    when Option.equal equal_loop_term left_term.loop_term right_term.loop_term
         && Option.equal Expr.Typed.equal left_term.symbol right_term.symbol
    -> (
      match (left_term.loop_term, left_term.const = right_term.const) with
      | None, true -> Same_every_iteration
      | None, false -> Different
      | Some _, true -> Same_in_one_iteration
      (* [n] and [n + 1] differ within an iteration but can select the same
         element in different iterations. Thus the result is [May_differ], not
         [Different]. *)
      | Some _, false -> May_differ)
  | Subscript _, _ | Field _, _ -> May_differ

let rec must_overwrite ~(same_iteration : bool) ~(killer : index step list)
    ~(killed : index step list) : bool =
  match (killer, killed) with
  | [], _ -> true
  | _ :: _, [] -> false
  | killer_step :: killer_rest, killed_step :: killed_rest -> (
      match compare_steps killer_step killed_step with
      | Same_every_iteration ->
          must_overwrite ~same_iteration ~killer:killer_rest ~killed:killed_rest
      | Same_in_one_iteration ->
          same_iteration
          && must_overwrite ~same_iteration ~killer:killer_rest
               ~killed:killed_rest
      | Different | May_differ -> false)

let rec may_overlap (written : index step list) (selected : index step list) :
    bool =
  match (written, selected) with
  | written_step :: written_rest, selected_step :: selected_rest -> (
      match compare_steps written_step selected_step with
      | Different -> false
      | Same_every_iteration | Same_in_one_iteration | May_differ ->
          may_overlap written_rest selected_rest)
  | [], _ | _, [] -> true

type fixed_step = Fixed_index of int | Fixed_field of int
[@@deriving compare, sexp_of]

let fixed_path (path : index step list) : fixed_step list option =
  Option.all
    (List.map path ~f:(function
      | Field field -> Some (Fixed_field field)
      | Subscript (Single (Sequential {const; symbol= None; loop_term= None}))
        ->
          Some (Fixed_index const)
      | Subscript _ -> None))

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

let pp_sequential ppf ({const; symbol; loop_term} : sequential) =
  Option.iter loop_term ~f:(fun {loopvar; direction} ->
      Fmt.pf ppf "%s%s"
        (match direction with Ascending -> "" | Descending -> "-")
        loopvar);
  (* The printer writes the loop term, then the symbol, then the constant. A
     term after a printed term starts with a sign. [leading] is true when no
     loop term is printed, so the symbol prints without a sign. [first] is true
     when no term prints before the constant. Then the constant prints even when
     it is 0, so the index is never empty. *)
  let leading = Option.is_none loop_term in
  Option.iter symbol ~f:(fun symbol ->
      Fmt.pf ppf "%s%a" (if leading then "" else "+") Expr.Typed.pp symbol);
  let first = leading && Option.is_none symbol in
  if const <> 0 || first then
    Fmt.pf ppf "%s%d"
      (if const < 0 then "-" else if first then "" else "+")
      (abs const)

let pp_varying_kind ppf = function
  | Written -> Fmt.string ppf "written"
  | Unhandled -> Fmt.string ppf "unhandled"

let pp_index ppf = function
  | Sequential term -> pp_sequential ppf term
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
