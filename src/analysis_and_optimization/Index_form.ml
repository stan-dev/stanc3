open Std
open Std.Compare
open Middle
open Dataflow_types
open Mir_utils

type loop_term = {loop: label; negated: bool}

type form =
  {const: int; symbol: Expr.Typed.t option; loop_term: loop_term option}

type index = form option

(** The same loop term: the same loop, with the same sign. *)
let same_loop_term (left : loop_term) (right : loop_term) : bool =
  left.loop = right.loop && Bool.equal left.negated right.negated

(** Arithmetic on forms. Each operation returns [None] when the result is not a
    form: a form holds at most one symbol, with coefficient 1, and at most one
    loop term. *)
module Form = struct
  (** Adds two forms. The result is [None] when both have a symbol or both have
      a loop term. *)
  let add (left : form) (right : form) : form option =
    match ((left.symbol, right.symbol), (left.loop_term, right.loop_term)) with
    | (Some _, Some _), _ | _, (Some _, Some _) -> None
    | (left_symbol, right_symbol), (left_loop_term, right_loop_term) ->
        Some
          { const= left.const + right.const
          ; symbol= Option.first_some left_symbol right_symbol
          ; loop_term= Option.first_some left_loop_term right_loop_term }

  (** [subtract] calls this function before negating [right]. When both sides
      have the same symbol, the symbols cancel, as in [(n + k) - k = n]. Equal
      loop terms cancel in the same way. Without this step, [negate] fails on
      the symbol of [right]. *)
  let cancel_matching (left : form) (right : form) : form * form =
    let symbols_match = Option.equal Expr.Typed.equal left.symbol right.symbol in
    let loop_terms_match =
      Option.equal same_loop_term left.loop_term right.loop_term in
    let drop_matching (term : form) =
      { term with
        symbol= (if symbols_match then None else term.symbol)
      ; loop_term= (if loop_terms_match then None else term.loop_term) } in
    (drop_matching left, drop_matching right)

  (** The symbol has coefficient 1, so a form cannot hold [-symbol]. The
      function returns [None] in that case. A negated loop variable flips
      [negated], as [-n] does in [N - n]. *)
  let negate (term : form) : form option =
    match term.symbol with
    | Some _ -> None
    | None ->
        Some
          { const= -term.const
          ; symbol= None
          ; loop_term=
              Option.map term.loop_term ~f:(fun loop_term ->
                  {loop_term with negated= not loop_term.negated}) }

  (** Subtracts [right] from [left]. The result is [None] when the difference is
      not a form. *)
  let subtract (left : form) (right : form) : form option =
    let left, right = cancel_matching left right in
    match negate right with
    | Some negated_right -> add left negated_right
    | None -> None
end

let classify ~(for_loops : (string * label) list)
    ~(written_vars : string Set.Poly.t) (expr : Expr.Typed.t) : index =
  let for_loop_of name =
    List.find_map for_loops ~f:(fun (loopvar, loop) ->
        if String.equal loopvar name then Some loop else None) in
  (* This function looks at [subexpr] as one unit and does not look at the sums
     inside [subexpr]. A written variable can change between iterations, so a
     read of a written variable gives no form. A loop variable in a form other
     than a sum, as in [2 * n], also gives no form. Any other expression is
     constant in the loops, so the expression becomes the symbol of a form. *)
  let classify_whole_expression (subexpr : Expr.Typed.t) : index =
    let read_names = expr_var_names_set subexpr in
    let reads_written_variable =
      not (Set.Poly.disjoint read_names written_vars) in
    let reads_loop_variable =
      Set.Poly.exists read_names ~f:(fun name ->
          Option.is_some (for_loop_of name)) in
    if reads_written_variable || reads_loop_variable then None
    else Some {const= 0; symbol= Some subexpr; loop_term= None} in
  let rec classify_sum (subexpr : Expr.Typed.t) : index =
    let combine form_op lhs rhs =
      match (classify_sum lhs, classify_sum rhs) with
      | Some left_form, Some right_form -> (
          match form_op left_form right_form with
          | Some form -> Some form
          | None -> classify_whole_expression subexpr)
      | None, _ | _, None -> classify_whole_expression subexpr in
    match subexpr.pattern with
    | Var name when Option.is_some (for_loop_of name) ->
        let loop_term =
          Option.map (for_loop_of name) ~f:(fun loop -> {loop; negated= false})
        in
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
    | FunApp (Operator Plus, [lhs; rhs]) -> combine Form.add lhs rhs
    | FunApp (Operator Minus, [lhs; rhs]) -> combine Form.subtract lhs rhs
    | Var _ | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _
     |TupleProjection _ | Promotion _ ->
        classify_whole_expression subexpr in
  classify_sum expr

type comparison =
  | Always_same
  | Never_same
  | Same_at_distance of {loop: label; distance: int}
  | Cannot_tell

let compare_subscripts ~(first : index Index.t) ~(second : index Index.t) :
    comparison =
  match (first, second) with
  | Single (Some first_form), Single (Some second_form)
    when Option.equal same_loop_term first_form.loop_term second_form.loop_term
         && Option.equal Expr.Typed.equal first_form.symbol second_form.symbol
    -> (
      match first_form.loop_term with
      | None ->
          (* The ZIV test: two constant subscripts. *)
          if first_form.const = second_form.const then Always_same
          else Never_same
      | Some {loop; negated} ->
          (* The strong SIV test. In iteration [i], the first subscript selects
             [i + first_form.const], and in iteration [j], the second selects [j
             + second_form.const]. These are equal when [j - i] is the
             difference of the constants. A negated loop variable flips the
             sign. *)
          let const_difference = first_form.const - second_form.const in
          Same_at_distance
            { loop
            ; distance=
                (if negated then -const_difference else const_difference) })
  | (Single _ | All | Upfrom _ | Between _ | MultiIndex _), _ -> Cannot_tell

(** Prints the loop term, then the symbol, then the constant, as in [-n+N+1].
    [loop_name] gives the loop variable of a loop label. *)
let pp_form ~(loop_name : label -> string) ppf
    ({const; symbol; loop_term} : form) =
  Option.iter loop_term ~f:(fun {loop; negated} ->
      Fmt.pf ppf "%s%s" (if negated then "-" else "") (loop_name loop));
  (* The printer writes the loop term, then the symbol, then the constant. A
     term after a printed term starts with a sign. [leading] is true when no
     loop term is printed, so the symbol prints without a sign. [first] is true
     when no term prints before the constant. Then the constant prints even when
     the constant is 0, so the printed index is never empty. *)
  let leading = Option.is_none loop_term in
  Option.iter symbol ~f:(fun symbol ->
      Fmt.pf ppf "%s%a" (if leading then "" else "+") Expr.Typed.pp symbol);
  let first = leading && Option.is_none symbol in
  if const <> 0 || first then
    Fmt.pf ppf "%s%d"
      (if const < 0 then "-" else if first then "" else "+")
      (abs const)

let pp ~loop_name ppf = function
  | Some form -> pp_form ~loop_name ppf form
  | None -> Fmt.string ppf "?"
