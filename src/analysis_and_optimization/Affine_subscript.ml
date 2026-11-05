open Std
open Std.Compare
open Middle
open Dataflow_types
open Mir_utils

type loop_term = {loop_id: label; is_negated: bool}

type affine =
  {const: int; symbol: Expr.Typed.t option; loop_term: loop_term option}

type t = affine option

(* Arithmetic on affine subscripts. Each operation returns [None] when the
   result is not affine: an affine subscript holds at most one symbol, with
   coefficient 1, and at most one loop term. *)

(** Adds two affine subscripts. The result is [None] when both have a symbol or
    both have a loop term. *)
let add (left : affine) (right : affine) : affine option =
  match ((left.symbol, right.symbol), (left.loop_term, right.loop_term)) with
  | (Some _, Some _), _ | _, (Some _, Some _) -> None
  | (left_symbol, right_symbol), (left_loop_term, right_loop_term) ->
      Some
        { const= left.const + right.const
        ; symbol= Option.first_some left_symbol right_symbol
        ; loop_term= Option.first_some left_loop_term right_loop_term }

(** [subtract] calls this function before negating [right]. When both sides have
    the same symbol, the symbols cancel, as in [(n + k) - k = n]. Equal loop
    terms, with the same loop and the same sign, cancel in the same way. Without
    this step, [negate] fails on the symbol of [right]. *)
let cancel_shared_terms (left : affine) (right : affine) : affine * affine =
  let is_symbol_shared =
    Option.equal Expr.Typed.equal left.symbol right.symbol in
  let is_loop_term_shared =
    Option.equal
      (fun (left_term : loop_term) (right_term : loop_term) ->
        left_term.loop_id = right_term.loop_id
        && Bool.equal left_term.is_negated right_term.is_negated)
      left.loop_term right.loop_term in
  let drop_shared (term : affine) =
    { term with
      symbol= (if is_symbol_shared then None else term.symbol)
    ; loop_term= (if is_loop_term_shared then None else term.loop_term) } in
  (drop_shared left, drop_shared right)

(** The symbol has coefficient 1, so an affine subscript cannot hold [-symbol].
    The function returns [None] in that case. A negated loop variable flips
    [is_negated], as [-n] does in [N - n]. *)
let negate (term : affine) : affine option =
  match term.symbol with
  | Some _ -> None
  | None ->
      Some
        { const= -term.const
        ; symbol= None
        ; loop_term=
            Option.map term.loop_term ~f:(fun loop_term ->
                {loop_term with is_negated= not loop_term.is_negated}) }

(** Subtracts [right] from [left]. The result is [None] when the difference is
    not affine. *)
let subtract (left : affine) (right : affine) : affine option =
  let left, right = cancel_shared_terms left right in
  match negate right with
  | Some negated_right -> add left negated_right
  | None -> None

let of_expr ~(for_loops : (string * label) list)
    ~(defined_vars : string Set.Poly.t) (expr : Expr.Typed.t) : t =
  let loop_id_of_var name =
    List.find_map for_loops ~f:(fun (loopvar, loop_id) ->
        if String.equal loopvar name then Some loop_id else None) in
  (* This function looks at [subexpr] as one unit and does not look at the sums
     inside [subexpr]. A defined variable can change between iterations, so a
     use of a defined variable is not affine. A loop variable in an expression
     other than a sum, as in [2 * n], is also not affine. Any other expression
     is constant in the loops, so the expression becomes the symbol of an affine
     subscript. *)
  let of_symbol (subexpr : Expr.Typed.t) : t =
    let used_names = expr_names subexpr in
    let is_defined_var_used = not (Set.Poly.disjoint used_names defined_vars) in
    let is_loop_var_used =
      Set.Poly.exists used_names ~f:(fun name ->
          Option.is_some (loop_id_of_var name)) in
    if is_defined_var_used || is_loop_var_used then None
    else Some {const= 0; symbol= Some subexpr; loop_term= None} in
  let rec of_sum (subexpr : Expr.Typed.t) : t =
    let combine_operands operation lhs rhs =
      match (of_sum lhs, of_sum rhs) with
      | Some left_affine, Some right_affine -> (
          match operation left_affine right_affine with
          | Some affine -> Some affine
          | None -> of_symbol subexpr)
      | None, _ | _, None -> of_symbol subexpr in
    match subexpr.pattern with
    | Var name when Option.is_some (loop_id_of_var name) ->
        let loop_term =
          Option.map (loop_id_of_var name) ~f:(fun loop_id ->
              {loop_id; is_negated= false}) in
        Some {const= 0; symbol= None; loop_term}
    | Lit (Int, digits) -> (
        match Int.of_string_opt digits with
        | Some const -> Some {const; symbol= None; loop_term= None}
        | None ->
            (* The frontend rejects integer literals outside the OCaml int range
               before index classification. *)
            Common.ICE.internal_error
              ("Integer literal does not fit an int: " ^ digits) [@coverage off]
        )
    | FunApp (Operator Plus, [lhs; rhs]) -> combine_operands add lhs rhs
    | FunApp (Operator Minus, [lhs; rhs]) -> combine_operands subtract lhs rhs
    | Var _ | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _
     |TupleProjection _ | Promotion _ ->
        of_symbol subexpr in
  of_sum expr

type comparison =
  | Always_same
  | Never_same
  | Same_at_distance of {loop_id: label; distance: int}
  | Cannot_tell

let compare_subscripts (first : t Index.t) (second : t Index.t) : comparison =
  match (first, second) with
  | Single (Some first_affine), Single (Some second_affine)
    when Option.equal
           (fun (first_term : loop_term) (second_term : loop_term) ->
             first_term.loop_id = second_term.loop_id
             && Bool.equal first_term.is_negated second_term.is_negated)
           first_affine.loop_term second_affine.loop_term
         && Option.equal Expr.Typed.equal first_affine.symbol
              second_affine.symbol -> (
      match first_affine.loop_term with
      | None ->
          (* The ZIV test: two constant subscripts. *)
          if first_affine.const = second_affine.const then Always_same
          else Never_same
      | Some {loop_id; is_negated} ->
          (* The strong SIV test. In iteration [i], the first subscript selects
             [i + first_affine.const], and in iteration [j], the second selects
             [j + second_affine.const]. These are equal when [j - i] is the
             difference of the constants. A negated loop variable flips the
             sign. *)
          let const_difference = first_affine.const - second_affine.const in
          Same_at_distance
            { loop_id
            ; distance=
                (if is_negated then -const_difference else const_difference) })
  | (Single _ | All | Upfrom _ | Between _ | MultiIndex _), _ -> Cannot_tell

(** Prints the loop term, then the symbol, then the constant, as in [-n+N+1].
    [loop_name] gives the loop variable of a loop label. *)
let pp_affine ~(loop_name : label -> string) ppf
    ({const; symbol; loop_term} : affine) =
  Option.iter loop_term ~f:(fun {loop_id; is_negated} ->
      Fmt.pf ppf "%s%s" (if is_negated then "-" else "") (loop_name loop_id));
  (* The printer writes the loop term, then the symbol, then the constant. A
     term after a printed term starts with a sign. [is_leading] is true when no
     loop term is printed, so the symbol prints without a sign. [is_first] is
     true when no term prints before the constant. Then the constant prints even
     when the constant is 0, so the printed index is never empty. *)
  let is_leading = Option.is_none loop_term in
  Option.iter symbol ~f:(fun symbol ->
      Fmt.pf ppf "%s%a" (if is_leading then "" else "+") Expr.Typed.pp symbol);
  let is_first = is_leading && Option.is_none symbol in
  if const <> 0 || is_first then
    Fmt.pf ppf "%s%d"
      (if const < 0 then "-" else if is_first then "" else "+")
      (abs const)

let pp ~loop_name ppf = function
  | Some affine -> pp_affine ~loop_name ppf affine
  | None -> Fmt.string ppf "?"
