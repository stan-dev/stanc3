open Std
open Std.Compare
open Std.Sexp_conv
open Middle
open Dataflow_types
open Mir_utils

module Affine_subscript = struct
  type loop_term = {loop_id: label; is_negated: bool}

  type affine =
    {const: int; symbol: Expr.Typed.t option; loop_term: loop_term option}

  type t = affine option

  (** Whether two loop terms name the same loop with the same sign. *)
  let equal_loop_terms (first : loop_term option) (second : loop_term option) :
      bool =
    Option.equal
      (fun (first_term : loop_term) (second_term : loop_term) ->
        first_term.loop_id = second_term.loop_id
        && Bool.equal first_term.is_negated second_term.is_negated)
      first second

  (** Whether two symbols are the same expression. The comparison ignores the
      metadata, because a symbol that [add] builds carries no type. *)
  let equal_symbols (first : Expr.Typed.t option) (second : Expr.Typed.t option)
      : bool =
    Option.equal
      (fun first_symbol second_symbol ->
        Expr.Helpers.compare_ignore_meta first_symbol second_symbol = 0)
      first second

  (** Whether two affine subscripts have the same loop term and the same symbol,
      so that only the constants can differ. *)
  let has_same_terms (first : affine) (second : affine) : bool =
    equal_loop_terms first.loop_term second.loop_term
    && equal_symbols first.symbol second.symbol

  (* Arithmetic on affine subscripts. An affine subscript holds at most one loop
     term, so [add] returns [None] when both sides have one. Two symbols add up
     to one symbol, the sum of the two expressions. *)

  let add (left : affine) (right : affine) : affine option =
    match (left.loop_term, right.loop_term) with
    | Some _, Some _ -> None
    | left_loop_term, right_loop_term ->
        let symbol =
          match (left.symbol, right.symbol) with
          | Some left_symbol, Some right_symbol ->
              Some (Expr.Helpers.binop left_symbol Operator.Plus right_symbol)
          | left_symbol, right_symbol ->
              Option.first_some left_symbol right_symbol in
        Some
          { const= left.const + right.const
          ; symbol
          ; loop_term= Option.first_some left_loop_term right_loop_term }

  (** Negates each part: the constant, the symbol as [-symbol], and the loop
      term by flipping [is_negated], as [-n] in [N - n]. *)
  let negate (term : affine) : affine =
    { const= -term.const
    ; symbol= Option.map term.symbol ~f:(Expr.Helpers.unary_op Operator.PMinus)
    ; loop_term=
        Option.map term.loop_term ~f:(fun loop_term ->
            {loop_term with is_negated= not loop_term.is_negated}) }

  (** Subtracts [right] from [left]. The result is [None] when both sides keep a
      loop term. *)
  let subtract (left : affine) (right : affine) : affine option =
    (* A term on both sides cancels before the negation, so that [(n + k) - k]
       is [n] with no symbol and [(n + 1) - n] is [1] with no loop term. *)
    let drop_shared_terms (term : affine) : affine =
      { term with
        symbol=
          (if equal_symbols left.symbol right.symbol then None else term.symbol)
      ; loop_term=
          (if equal_loop_terms left.loop_term right.loop_term then None
           else term.loop_term) } in
    add (drop_shared_terms left) (negate (drop_shared_terms right))

  let of_expr ~(for_loops : (string * label) list)
      ~(defined_vars : string Set.Poly.t) (expr : Expr.Typed.t) : t =
    let loop_id_of_var name =
      List.find_map for_loops ~f:(fun (loopvar, loop_id) ->
          if String.equal loopvar name then Some loop_id else None) in
    (* This function looks at [subexpr] as one unit and does not look at the
       sums inside [subexpr]. A defined variable can change between iterations,
       so a use of a defined variable is not affine. A loop variable in an
       expression other than a sum, as in [2 * n], is also not affine. Any other
       expression is constant in the loops, so the expression becomes the symbol
       of an affine subscript. *)
    let of_symbol (subexpr : Expr.Typed.t) : t =
      let used_names = expr_names subexpr in
      let is_defined_var_used =
        not (Set.Poly.disjoint used_names defined_vars) in
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
              (* The frontend rejects integer literals outside the OCaml int
                 range before index classification. *)
              Common.ICE.internal_error
                ("Integer literal does not fit an int: " ^ digits)
              [@coverage off])
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
      when has_same_terms first_affine second_affine -> (
        match first_affine.loop_term with
        | None ->
            (* The ZIV test: two constant subscripts. *)
            if first_affine.const = second_affine.const then Always_same
            else Never_same
        | Some {loop_id; is_negated} ->
            (* The strong SIV test. [v[n + 1]] in iteration 2 and [v[n - 2]] in
               iteration 5 both select [v[3]]. The distance is the iteration of
               the second minus the iteration of the first, 3, which is the
               first constant minus the second, [1 - (-2)]. A negated loop
               variable flips the sign. *)
            let const_difference = first_affine.const - second_affine.const in
            Same_at_distance
              { loop_id
              ; distance=
                  (if is_negated then -const_difference else const_difference)
              })
    | (Single _ | All | Upfrom _ | Between _ | MultiIndex _), _ -> Cannot_tell

  (** Prints the loop term, then the symbol, then the constant, as in [-n+N+1].
      [loop_name] gives the loop variable of a loop label. *)
  let pp_affine ~(loop_name : label -> string) ppf
      ({const; symbol; loop_term} : affine) =
    Option.iter loop_term ~f:(fun {loop_id; is_negated} ->
        Fmt.pf ppf "%s%s" (if is_negated then "-" else "") (loop_name loop_id));
    let is_leading = Option.is_none loop_term in
    Option.iter symbol ~f:(fun (symbol : Expr.Typed.t) ->
        (* A negated symbol prints as [-k], not as [+-(k)]. *)
        match symbol.pattern with
        | FunApp (Operator PMinus, [negated]) ->
            Fmt.pf ppf "-%a" Expr.Typed.pp negated
        | _ ->
            Fmt.pf ppf "%s%a"
              (if is_leading then "" else "+")
              Expr.Typed.pp symbol);
    let is_first = is_leading && Option.is_none symbol in
    if const <> 0 || is_first then
      Fmt.pf ppf "%s%d"
        (if const < 0 then "-" else if is_first then "" else "+")
        (abs const)

  let pp ~loop_name ppf = function
    | Some affine -> pp_affine ~loop_name ppf affine
    | None -> Fmt.string ppf "?"
end

module Accesses = struct
  type 'e step = Subscript of 'e Index.t | Field of int
  type kind = Read | Write | ReadWrite [@@deriving equal]
  type access = {var: string; path: Affine_subscript.t step list; kind: kind}
  type t = access list

  (** Builds paths from expressions and from left-hand sides of assignments. A
      path here still holds the subscript expressions, so that the reads inside
      the subscripts can be collected before {!classify} classifies them. *)
  module Path = struct
    (** The variable and the path that [expr] selects, or [None] if [expr] is
        not a variable with subscripts and fields, or indexes after a slice. *)
    let rec of_expr (expr : Expr.Typed.t) :
        (string * Expr.Typed.t step list) option =
      let extend_path base steps =
        (* After a slice or a multi-index, later subscripts index the result of
           the selection. The path of [base] and [steps] then cannot join into
           one path of the base variable. *)
        match of_expr base with
        | Some (name, prefix)
          when List.for_all prefix ~f:(function
                 | Subscript (Index.Single _) | Field _ -> true
                 | Subscript _ -> false) ->
            Some (name, prefix @ steps)
        | Some _ | None -> None in
      match expr.pattern with
      | Var name -> Some (name, [])
      | Indexed (base, indices) ->
          extend_path base (List.map indices ~f:(fun index -> Subscript index))
      | TupleProjection (base, field) -> extend_path base [Field field]
      | FunApp _ | TernaryIf _ | Lit _ | EAnd _ | EOr _ | Promotion _ -> None

    (** The path of the left-hand side of an assignment. *)
    let rec of_lvalue ((lbase, indices) : Expr.Typed.t Stmt.Pattern.lvalue) :
        Expr.Typed.t step list =
      (match lbase with
        | LVariable _ -> []
        | LTupleProjection (inner, field) -> of_lvalue inner @ [Field field])
      @ List.map indices ~f:(fun index -> Subscript index)

    (** The expressions inside the subscripts of [path]. *)
    let subscript_exprs (path : Expr.Typed.t step list) : Expr.Typed.t list =
      List.concat_map path ~f:(function
        | Subscript index -> Index.bounds index
        | Field _ -> [])

    (** Classifies each subscript of [path] with [classify]. *)
    let classify ~(classify : Expr.Typed.t -> Affine_subscript.t)
        (path : Expr.Typed.t step list) : Affine_subscript.t step list =
      List.map path ~f:(function
        | Subscript index -> Subscript (Index.map classify index)
        | Field field -> Field field)
  end

  (** An access before classification: the variable, the path with the subscript
      expressions still in place, and the kind. *)
  type collected = string * Expr.Typed.t step list * kind

  (** Whether two paths select the element by the same expressions. The metadata
      of the expressions is ignored. *)
  let equal_expr_paths (first : Expr.Typed.t step list)
      (second : Expr.Typed.t step list) : bool =
    let equal_steps first_step second_step =
      match (first_step, second_step) with
      | Field first_field, Field second_field -> first_field = second_field
      | Subscript first_index, Subscript second_index ->
          Index.compare Expr.Helpers.compare_ignore_meta first_index
            second_index
          = 0
      | Field _, Subscript _ | Subscript _, Field _ -> false in
    List.equal ~eq:equal_steps first second

  (** The accesses in [expr]. A subscripted variable gives one read with the
      whole path, then the accesses in the subscripts. *)
  let rec collect_expr (expr : Expr.Typed.t) : collected list =
    let of_children () =
      Expr.Pattern.fold
        (fun running_accesses subexpr ->
          running_accesses @ collect_expr subexpr)
        [] expr.pattern in
    match expr.pattern with
    | Var name -> [(name, [], Read)]
    | Indexed _ | TupleProjection _ -> (
        match Path.of_expr expr with
        | Some (name, path) ->
            (name, path, Read)
            :: List.concat_map (Path.subscript_exprs path) ~f:collect_expr
        | None -> of_children ())
    | FunApp (StanLib (_, FnTarget), []) -> [("target", [], Read)]
    | FunApp (fun_kind, _) when is_target_increment fun_kind ->
        of_children () @ [("target", [], ReadWrite)]
    | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ ->
        of_children ()

  (** The accesses of an assignment to [path] of [var] whose indices and
      right-hand side have the accesses [reads]. A read of the written element,
      as in [x = x + 1], joins the write into one read-write of the element. *)
  let of_assignment (var : string) (path : Expr.Typed.t step list)
      (reads : collected list) : collected list =
    let is_read_of_written (name, read_path, kind) =
      String.equal name var && equal_kind kind Read
      && equal_expr_paths read_path path in
    if List.exists reads ~f:is_read_of_written then
      List.filter reads ~f:(fun read -> not (is_read_of_written read))
      @ [(var, path, ReadWrite)]
    else reads @ [(var, path, Write)]

  let of_statement ~(classify : Expr.Typed.t -> Affine_subscript.t)
      (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) : t =
    (* The accesses of each nested statement are collected separately. The
       statement fold thus visits only the expressions of [stmt] and skips
       nested statements. *)
    let of_children () =
      Stmt.Pattern.fold
        (fun running_accesses subexpr ->
          running_accesses @ collect_expr subexpr)
        Fun.const [] stmt in
    let collected =
      match stmt with
      | Assignment (lhs, _, rhs) ->
          let path = Path.of_lvalue lhs in
          of_assignment
            (Stmt.Helpers.lhs_variable lhs)
            path
            (List.concat_map
               (Path.subscript_exprs path @ [rhs])
               ~f:collect_expr)
      | Decl {decl_id; _} -> of_children () @ [(decl_id, [], Write)]
      | TargetPE _ | JacobianPE _ -> of_children () @ [("target", [], ReadWrite)]
      | NRFunApp (fun_kind, _) when is_target_increment fun_kind ->
          of_children () @ [("target", [], ReadWrite)]
      | NRFunApp _ | Return _ | IfElse _ | While _ | For _ | Profile _
       |Block _ | SList _ | Break | Continue | Skip ->
          of_children () in
    List.map collected ~f:(fun (var, path, kind) ->
        {var; path= Path.classify ~classify path; kind})

  let used_vars (accesses : t) : string Set.Poly.t =
    let used_var {var; kind; _} =
      match kind with Read | ReadWrite -> Some var | Write -> None in
    accesses |> List.filter_map ~f:used_var |> Set.Poly.of_list

  let defined_vars (accesses : t) : string Set.Poly.t =
    let defined_var {var; kind; _} =
      match kind with Write | ReadWrite -> Some var | Read -> None in
    accesses |> List.filter_map ~f:defined_var |> Set.Poly.of_list

  (** The printers of the example notation. *)
  module Printing = struct
    (** Prints one subscript, with braces around a multi-index. *)
    let pp_subscript ~loop_name ppf (index : Affine_subscript.t Index.t) =
      match index with
      | MultiIndex indices ->
          Fmt.pf ppf "{%a}" (Affine_subscript.pp ~loop_name) indices
      | All | Single _ | Upfrom _ | Between _ ->
          Index.pp (Affine_subscript.pp ~loop_name) ppf index

    (** Prints a path. [pp_subscripts_after] continues an open bracket.
        Consecutive subscripts thus share one pair of brackets. *)
    let rec pp_path ~loop_name ppf = function
      | [] -> ()
      | Field field :: rest ->
          Fmt.pf ppf ".%d%a" field (pp_path ~loop_name) rest
      | Subscript index :: rest ->
          Fmt.pf ppf "[%a%a" (pp_subscript ~loop_name) index
            (pp_subscripts_after ~loop_name)
            rest

    and pp_subscripts_after ~loop_name ppf = function
      | Subscript index :: rest ->
          Fmt.pf ppf ", %a%a" (pp_subscript ~loop_name) index
            (pp_subscripts_after ~loop_name)
            rest
      | path -> Fmt.pf ppf "]%a" (pp_path ~loop_name) path

    (** Prints [R], [RW] or [W]. *)
    let pp_kind ppf kind =
      Fmt.string ppf
        (match kind with Read -> "R" | ReadWrite -> "RW" | Write -> "W")

    let pp_access ~loop_name ppf {var; path; kind} =
      Fmt.pf ppf "%a %s%a" pp_kind kind var (pp_path ~loop_name) path

    let pp ~loop_name ppf (accesses : t) =
      let reads, read_writes, writes =
        List.partition3_map accesses ~f:(fun access ->
            match access.kind with
            | Read -> `Fst access
            | ReadWrite -> `Snd access
            | Write -> `Trd access) in
      Fmt.(list ~sep:(any ", ") (pp_access ~loop_name))
        ppf
        (reads @ read_writes @ writes)
  end

  let pp_path = Printing.pp_path
  let pp_access = Printing.pp_access
  let pp = Printing.pp
end

open Accesses

(** Compares the steps at one position of two paths. Two tuple fields compare
    like two constant subscripts. *)
let compare_steps (first : Affine_subscript.t step)
    (second : Affine_subscript.t step) : Affine_subscript.comparison =
  match (first, second) with
  | Field first_field, Field second_field ->
      if first_field = second_field then Always_same else Never_same
  | Subscript first, Subscript second ->
      Affine_subscript.compare_subscripts first second
  | Field _, Subscript _ | Subscript _, Field _ -> Cannot_tell

(** The killer path can be shorter than the killed path, so [m[1]] overwrites
    [m[1, 2]]. At each position of the killer path, the two steps must select
    the same element in every iteration, as [[1]] and [[1]] do. Steps that
    select the same element only within one iteration, as [[n]] and [[n]] do,
    count only when both writes run in the same iteration. *)
let rec is_overwritten ~(is_same_iteration : bool)
    (killer : Affine_subscript.t step list)
    (killed : Affine_subscript.t step list) : bool =
  match (killer, killed) with
  | [], _ -> true
  | _ :: _, [] -> false
  | killer_step :: killer_rest, killed_step :: killed_rest -> (
      let is_rest_overwritten () =
        is_overwritten ~is_same_iteration killer_rest killed_rest in
      match compare_steps killer_step killed_step with
      | Always_same -> is_rest_overwritten ()
      | Same_at_distance {distance= 0; _} ->
          is_same_iteration && is_rest_overwritten ()
      | Never_same | Same_at_distance _ | Cannot_tell -> false)

type t = Independent | Distance_vector of int option list

(** Sets the distance at [loop_id] in [distances], one entry per loop of
    [loop_ids]. The result is [None] when [distances] already holds a different
    distance at [loop_id], because the two positions then never select the same
    element together. *)
let set_distance ~(loop_ids : label list) (distances : int option list)
    ~(loop_id : label) ~(distance : int) : int option list option =
  let updated_distance common_loop_id known_distance =
    match known_distance with
    | _ when common_loop_id <> loop_id -> Some known_distance
    | Some known when known <> distance -> None
    | Some _ | None -> Some (Some distance) in
  Option.all (List.map2 loop_ids distances ~f:updated_distance)

let test_dependence (common_loop_ids : label list)
    (write_path : Affine_subscript.t step list)
    (read_path : Affine_subscript.t step list) : t =
  let all_unknown =
    Distance_vector (List.map common_loop_ids ~f:(fun _ -> None)) in
  if List.compare_lengths write_path read_path <> 0 then all_unknown
  else
    List.fold_left2 write_path read_path ~init:all_unknown
      ~f:(fun dependence write_step read_step ->
        match dependence with
        | Independent -> Independent
        | Distance_vector distances -> (
            match compare_steps write_step read_step with
            | Never_same -> Independent
            | Always_same | Cannot_tell -> dependence
            | Same_at_distance {loop_id; distance} -> (
                match
                  set_distance ~loop_ids:common_loop_ids distances ~loop_id
                    ~distance
                with
                | Some distances -> Distance_vector distances
                | None -> Independent)))

let is_lexicographically_positive (common_loop_ids : label list)
    (is_writer_first : bool) (write_path : Affine_subscript.t step list)
    (read_path : Affine_subscript.t step list) : bool =
  match test_dependence common_loop_ids write_path read_path with
  | Independent -> false
  | Distance_vector distances -> (
      (* The first distance that is not zero decides. An unknown distance allows
         an earlier iteration of that loop. *)
      match
        List.find_map distances ~f:(function
          | Some 0 -> None
          | distance -> Some distance)
      with
      | None -> is_writer_first
      | Some (Some distance) -> distance > 0
      | Some None -> true)

(** Prints the distance at one loop as [[n:1]], with the loop variable that
    [loop_name] gives for the loop label. *)
let pp_distance ~(loop_name : label -> string) ppf
    ((loop_id, distance) : label * int option) =
  Fmt.pf ppf "[%s:%a]" (loop_name loop_id)
    Fmt.(option ~none:(any "*") int)
    distance

let pp ~(loop_name : label -> string) ~(common_loop_ids : label list) ppf =
  function
  | Independent -> Fmt.string ppf "independent"
  | Distance_vector distances when List.for_all distances ~f:Option.is_none ->
      Fmt.string ppf "unknown"
  | Distance_vector distances ->
      Fmt.pf ppf "distances %a"
        Fmt.(list ~sep:nop (pp_distance ~loop_name))
        (List.map2 common_loop_ids distances ~f:(fun loop_id distance ->
             (loop_id, distance)))
