open Std
open Std.Compare
open Std.Sexp_conv
open Middle
open Dataflow_types
open Mir_utils

module Affine = struct
  type 'key term = {key: 'key; coefficient: int}

  type t =
    { constant: int
    ; loop_terms: label term list
    ; invariant_terms: Expr.Typed.t term list }

  module Term_key = struct
    type t = Loop of label | Invariant of Expr.Typed.t

    let compare first second =
      match (first, second) with
      | Loop a, Loop b -> Int.compare a b
      | Invariant a, Invariant b -> Expr.Helpers.compare_ignore_meta a b
      | Loop _, Invariant _ -> -1
      | Invariant _, Loop _ -> 1
  end

  module Term_map = Stdlib.Map.Make (Term_key)

  (* Ordered coefficient maps avoid copying a growing sorted list at every
     addition. Convert to the dependence-test term lists only at the
     boundary. *)
  type sum = {offset: int; coefficients: int Term_map.t}
  type classified = {stable: bool; sum: sum option}

  let constant offset = {offset; coefficients= Term_map.empty}
  let term key = {offset= 0; coefficients= Term_map.singleton key 1}

  let add left right =
    let combine _ a b =
      let coefficient = a + b in
      if coefficient = 0 then None else Some coefficient in
    { offset= left.offset + right.offset
    ; coefficients= Term_map.union combine left.coefficients right.coefficients
    }

  let scale factor sum =
    if factor = 0 then constant 0
    else
      { offset= factor * sum.offset
      ; coefficients=
          Term_map.map
            (fun coefficient -> factor * coefficient)
            sum.coefficients }

  let multiply left right =
    if Term_map.is_empty left.coefficients then Some (scale left.offset right)
    else if Term_map.is_empty right.coefficients then
      Some (scale right.offset left)
    else None

  let combine_operand_sums operation left right =
    match (left.sum, right.sum) with
    | Some left_sum, Some right_sum -> operation left_sum right_sum
    | _ -> None

  let rec classify_expression defined_vars loop_variables (expr : Expr.Typed.t)
      =
    let children =
      Expr.Pattern.map
        (classify_expression defined_vars loop_variables)
        expr.pattern in
    let stable =
      (not (cannot_duplicate_top_expr expr))
      && Expr.Pattern.fold
           (fun children_stable child -> children_stable && child.stable)
           true children
      &&
      match expr.pattern with
      | Var name ->
          not
            (Set.Poly.mem name defined_vars
            || String.Map.mem name loop_variables)
      | _ -> true in
    let arithmetic_sum =
      match children with
      | Var name ->
          Option.map (String.Map.find_opt name loop_variables) ~f:(fun id ->
              term (Term_key.Loop id))
      | Lit (Int, digits) -> Option.map (Int.of_string_opt digits) ~f:constant
      | FunApp (Operator Plus, [left; right]) ->
          combine_operand_sums (fun a b -> Some (add a b)) left right
      | FunApp (Operator Minus, [left; right]) ->
          combine_operand_sums
            (fun a b -> Some (add a (scale (-1) b)))
            left right
      | FunApp (Operator Times, [left; right]) ->
          combine_operand_sums multiply left right
      | FunApp (Operator PMinus, [operand]) ->
          Option.map operand.sum ~f:(scale (-1))
      | FunApp (Operator PPlus, [operand]) -> operand.sum
      | _ -> None in
    let sum =
      match (arithmetic_sum, expr.pattern) with
      | Some _, _ | None, Lit (Int, _) -> arithmetic_sum
      | None, _ when stable -> Some (term (Term_key.Invariant expr))
      | None, _ -> None in
    {stable; sum}

  let to_affine sum =
    let loop_terms, invariant_terms =
      Term_map.fold
        (fun key coefficient (loops, invariants) ->
          match key with
          | Term_key.Loop loop_id ->
              ({key= loop_id; coefficient} :: loops, invariants)
          | Term_key.Invariant expression ->
              (loops, {key= expression; coefficient} :: invariants))
        sum.coefficients ([], []) in
    { constant= sum.offset
    ; loop_terms= List.rev loop_terms
    ; invariant_terms= List.rev invariant_terms }

  let of_expr defined_vars loop_variables expr =
    Option.map (classify_expression defined_vars loop_variables expr).sum
      ~f:to_affine

  let equal_terms (compare : 'key -> 'key -> int) (first : 'key term list)
      (second : 'key term list) : bool =
    List.equal first second ~eq:(fun first_term second_term ->
        compare first_term.key second_term.key = 0
        && first_term.coefficient = second_term.coefficient)

  let has_same_invariant_terms (first : t) (second : t) : bool =
    equal_terms Expr.Helpers.compare_ignore_meta first.invariant_terms
      second.invariant_terms
end

module Loop_context = struct
  type bounds = {lower: int option; upper: int option}

  type t =
    { variables: string LabelMap.t
    ; bounds: bounds LabelMap.t
    ; defined_vars: string Set.Poly.t }

  (* Bounds use the same arithmetic normalization as subscripts, with no
     induction variables in scope. Stable symbolic terms may cancel. *)
  let evaluate_constant_bound defined_vars expr =
    match Affine.of_expr defined_vars String.Map.empty expr with
    | Some {constant; loop_terms= []; invariant_terms= []} -> Some constant
    | Some _ | None -> None

  let create statement_map defined_vars =
    let variables, bounds =
      LabelMap.fold statement_map ~init:(LabelMap.empty, LabelMap.empty)
        ~f:(fun ~key:loop_id ~data:(pattern, _) (variables, bounds) ->
          match pattern with
          | Stmt.Pattern.For {loopvar; lower; upper; _} ->
              ( LabelMap.add variables ~key:loop_id ~data:loopvar
              , LabelMap.add bounds ~key:loop_id
                  ~data:
                    { lower= evaluate_constant_bound defined_vars lower
                    ; upper= evaluate_constant_bound defined_vars upper } )
          | _ -> (variables, bounds)) in
    {variables; bounds; defined_vars}

  (* Enclosing loops are outermost first; inner bindings shadow outer ones. *)
  let variables_in_scope context loop_ids =
    List.fold_left loop_ids ~init:String.Map.empty ~f:(fun variables loop_id ->
        match LabelMap.find_opt loop_id context.variables with
        | None -> variables
        | Some name -> String.Map.add variables ~key:name ~data:loop_id)

  type scope = {context: t; loop_variables: label String.Map.t}

  let create_scope context loop_ids =
    {context; loop_variables= variables_in_scope context loop_ids}

  let is_loop_variable scope name = String.Map.mem name scope.loop_variables
end

module Accesses = struct
  type 'scalar step = Subscript of 'scalar Index.t | Field of int
  type 'scalar location = {variable: string; path: 'scalar step list}
  type 'scalar t = {reads: 'scalar location list; writes: 'scalar location list}

  let empty = {reads= []; writes= []}
  let add_read location effects = {effects with reads= location :: effects.reads}

  let add_write location effects =
    {effects with writes= location :: effects.writes}

  module Path = struct
    let is_single_element (step : Expr.Typed.t step) : bool =
      match step with
      | Subscript (Index.Single _) | Field _ -> true
      | Subscript _ -> false

    (* Descend toward the base with the suffix already built. Each path step is
       copied once, instead of appending to a growing prefix on return. *)
    let rec prepend_expression_path (expr : Expr.Typed.t) suffix =
      match expr.pattern with
      | Var name -> Some (name, suffix)
      | Indexed (base, indices) ->
          let steps = List.map indices ~f:(fun index -> Subscript index) in
          if
            (not (List.is_empty suffix))
            && not (List.for_all steps ~f:is_single_element)
          then None (* Indexing a slice cannot be flattened into a base path. *)
          else prepend_expression_path base (steps @ suffix)
      | TupleProjection (base, field) ->
          prepend_expression_path base (Field field :: suffix)
      | FunApp _ | TernaryIf _ | Lit _ | EAnd _ | EOr _ | Promotion _ -> None

    let of_expr expr = prepend_expression_path expr []

    let rec prepend_lvalue_path
        ((lbase, indices) : Expr.Typed.t Stmt.Pattern.lvalue) suffix =
      let indexed_suffix =
        List.map indices ~f:(fun index -> Subscript index) @ suffix in
      match lbase with
      | LVariable _ -> indexed_suffix
      | LTupleProjection (inner, field) ->
          prepend_lvalue_path inner (Field field :: indexed_suffix)

    let of_lvalue lhs = prepend_lvalue_path lhs []

    let index_expressions_of_step (step : Expr.Typed.t step) : Expr.Typed.t list
        =
      match step with Subscript index -> Index.bounds index | Field _ -> []

    let index_expressions_of_path (path : Expr.Typed.t step list) :
        Expr.Typed.t list =
      List.concat_map path ~f:index_expressions_of_step
  end

  let target = {variable= "target"; path= []}
  let add_target_read_write effects = add_write target (add_read target effects)

  (* Effects are unordered summaries. Prepending avoids repeatedly copying the
     accesses collected from earlier children. *)
  let rec collect_expr effects (expr : Expr.Typed.t) =
    match expr.pattern with
    | Var variable -> add_read {variable; path= []} effects
    | Indexed _ | TupleProjection _ -> (
        match Path.of_expr expr with
        | Some (variable, path) ->
            collect_exprs
              (add_read {variable; path} effects)
              (Path.index_expressions_of_path path)
        | None -> collect_subexprs effects expr)
    | FunApp (StanLib (_, FnTarget), []) -> add_read target effects
    | FunApp (fun_kind, _) when is_target_increment fun_kind ->
        add_target_read_write (collect_subexprs effects expr)
    | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ ->
        collect_subexprs effects expr

  and collect_subexprs effects (expr : Expr.Typed.t) =
    Expr.Pattern.fold collect_expr effects expr.pattern

  and collect_exprs effects exprs =
    List.fold_left exprs ~init:effects ~f:collect_expr

  let collect_statement_exprs stmt =
    Stmt.Pattern.fold collect_expr Fun.const empty stmt

  let of_statement (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) =
    match stmt with
    | Assignment (lhs, _, rhs) ->
        let path = Path.of_lvalue lhs in
        let effects =
          collect_exprs empty (Path.index_expressions_of_path path) in
        add_write
          {variable= Stmt.Helpers.lhs_variable lhs; path}
          (collect_expr effects rhs)
    | Decl {decl_id; _} ->
        add_write {variable= decl_id; path= []} (collect_statement_exprs stmt)
    | TargetPE _ | JacobianPE _ ->
        add_target_read_write (collect_statement_exprs stmt)
    | NRFunApp (fun_kind, _) when is_target_increment fun_kind ->
        add_target_read_write (collect_statement_exprs stmt)
    | NRFunApp _ | Return _ | IfElse _ | While _ | For _ | Profile _ | Block _
     |SList _ | Break | Continue | Skip ->
        collect_statement_exprs stmt

  let map_scalars (f : 'scalar -> 'mapped) (effects : 'scalar t) : 'mapped t =
    let map_step = function
      | Subscript index -> Subscript (Index.map f index)
      | Field field -> Field field in
    let map_location location =
      {location with path= List.map location.path ~f:map_step} in
    { reads= List.map effects.reads ~f:map_location
    ; writes= List.map effects.writes ~f:map_location }

  let variables (locations : 'scalar location list) : string Set.Poly.t =
    List.fold_left locations ~init:Set.Poly.empty ~f:(fun names location ->
        Set.Poly.add location.variable names)

  let read_variables effects = variables effects.reads
  let written_variables effects = variables effects.writes
end

module Access_function = struct
  (* Every scalar shares the region's domain table. Coefficient terms carry only
     loop IDs, never copies of their bounds. *)
  type t =
    { expr: Expr.Typed.t
    ; view: Affine.t option
    ; bounds: Loop_context.bounds LabelMap.t }

  let of_expr (scope : Loop_context.scope) expr =
    { expr
    ; view= Affine.of_expr scope.context.defined_vars scope.loop_variables expr
    ; bounds= scope.context.bounds }

  let compare_syntax first second =
    Expr.Helpers.compare_ignore_meta first.expr second.expr

  let is_provably_equal ~is_same_iteration first second =
    match (first.view, second.view) with
    | Some first_view, Some second_view ->
        first_view.constant = second_view.constant
        && Affine.has_same_invariant_terms first_view second_view
        && Affine.equal_terms Int.compare first_view.loop_terms
             second_view.loop_terms
        && (is_same_iteration || List.is_empty first_view.loop_terms)
    | None, _ | _, None -> false
end

open Accesses

type path = Access_function.t step list

let compare_locations (first : Access_function.t Accesses.location)
    (second : Access_function.t Accesses.location) =
  let compare_step first_step second_step =
    match (first_step, second_step) with
    | Field a, Field b -> Int.compare a b
    | Subscript a, Subscript b ->
        Index.compare Access_function.compare_syntax a b
    | Field _, Subscript _ -> -1
    | Subscript _, Field _ -> 1 in
  let variable_order = String.compare first.variable second.variable in
  if variable_order <> 0 then variable_order
  else List.compare ~cmp:compare_step first.path second.path

let is_definitely_same_element ~(is_same_iteration : bool)
    (first : Access_function.t step) (second : Access_function.t step) : bool =
  match (first, second) with
  | Field first_field, Field second_field -> first_field = second_field
  | Subscript (Single first_function), Subscript (Single second_function) ->
      Access_function.is_provably_equal ~is_same_iteration first_function
        second_function
  | ( Subscript (Single _ | All | Upfrom _ | Between _ | MultiIndex _)
    , Subscript _ )
   |Field _, Subscript _
   |Subscript _, Field _ ->
      false

let rec definitely_overwrites ~(is_same_iteration : bool) (killer : path)
    (killed : path) : bool =
  match (killer, killed) with
  | [], _ -> true
  | _ :: _, [] -> false
  | killer_step :: killer_rest, killed_step :: killed_rest ->
      is_definitely_same_element ~is_same_iteration killer_step killed_step
      && definitely_overwrites ~is_same_iteration killer_rest killed_rest

module Direction_set = struct
  type t =
    {is_writer_before: bool; is_same_iteration: bool; is_writer_after: bool}

  let all : t =
    {is_writer_before= true; is_same_iteration= true; is_writer_after= true}

  let of_distance (distance : int) : t =
    { is_writer_before= distance > 0
    ; is_same_iteration= distance = 0
    ; is_writer_after= distance < 0 }

  let inter (first : t) (second : t) : t =
    { is_writer_before= first.is_writer_before && second.is_writer_before
    ; is_same_iteration= first.is_same_iteration && second.is_same_iteration
    ; is_writer_after= first.is_writer_after && second.is_writer_after }

  let is_empty (directions : t) : bool =
    not
      (directions.is_writer_before || directions.is_same_iteration
     || directions.is_writer_after)
end

type loop_constraint = Distance of int | Directions of Direction_set.t
type dependence_vector = (label * loop_constraint) list

type subscript_result =
  | Disjoint
  | Unconstrained
  | Constrain of label * loop_constraint

let analyze_constant_subscripts (write : Affine.t) (read : Affine.t) :
    subscript_result =
  if write.constant = read.constant then Unconstrained else Disjoint

let distance_exceeds_loop_span (loop : Loop_context.bounds) (distance : int) :
    bool =
  match (loop.lower, loop.upper) with
  | Some lower, Some upper -> abs distance > upper - lower
  | None, _ | _, None -> false

let analyze_equal_stride_subscripts bounds
    ({key= loop; coefficient= stride} : label Affine.term) (write : Affine.t)
    (read : Affine.t) : subscript_result =
  let domain =
    Option.value
      (LabelMap.find_opt loop bounds)
      ~default:Loop_context.{lower= None; upper= None} in
  let difference = write.constant - read.constant in
  let distance = difference / stride in
  if difference mod stride <> 0 || distance_exceeds_loop_span domain distance
  then Disjoint
  else Constrain (loop, Distance distance)

let analyze_weak_zero_subscripts bounds ~(is_writer_varying : bool)
    ({key= loop; coefficient= stride} : label Affine.term) (varying : Affine.t)
    (fixed : Affine.t) : subscript_result =
  let is_before_bound bound iteration =
    match bound with
    | Some lower_bound -> iteration < lower_bound
    | None -> false in
  let is_after_bound bound iteration =
    match bound with
    | Some upper_bound -> iteration > upper_bound
    | None -> false in
  let domain =
    Option.value
      (LabelMap.find_opt loop bounds)
      ~default:Loop_context.{lower= None; upper= None} in
  let difference = fixed.constant - varying.constant in
  let iteration = difference / stride in
  if
    difference mod stride <> 0
    || is_before_bound domain.lower iteration
    || is_after_bound domain.upper iteration
  then Disjoint
  else
    let is_earlier_possible =
      not (is_before_bound domain.lower (iteration - 1)) in
    let is_later_possible = not (is_after_bound domain.upper (iteration + 1)) in
    Constrain
      ( loop
      , Directions
          { is_writer_before=
              (if is_writer_varying then is_later_possible
               else is_earlier_possible)
          ; is_same_iteration= true
          ; is_writer_after=
              (if is_writer_varying then is_earlier_possible
               else is_later_possible) } )

let rec greatest_common_divisor (left : int) (right : int) : int =
  if right = 0 then abs left else greatest_common_divisor right (left mod right)

let analyze_gcd_subscripts (write : Affine.t) (read : Affine.t) :
    subscript_result =
  let divisor =
    List.fold_left (write.loop_terms @ read.loop_terms) ~init:0
      ~f:(fun current_divisor (term : label Affine.term) ->
        greatest_common_divisor current_divisor term.coefficient) in
  if divisor > 0 && (read.constant - write.constant) mod divisor <> 0 then
    Disjoint
  else Unconstrained

let analyze_subscripts (write : Access_function.t Index.t)
    (read : Access_function.t Index.t) : subscript_result =
  match (write, read) with
  | ( Single {view= Some write_view; bounds= write_bounds; _}
    , Single {view= Some read_view; bounds= read_bounds; _} )
    when Affine.has_same_invariant_terms write_view read_view -> (
      match (write_view.loop_terms, read_view.loop_terms) with
      | [], [] -> analyze_constant_subscripts write_view read_view
      | [write_term], [read_term]
        when write_term.key = read_term.key
             && write_term.coefficient = read_term.coefficient ->
          analyze_equal_stride_subscripts write_bounds write_term write_view
            read_view
      | [write_term], [] ->
          analyze_weak_zero_subscripts write_bounds ~is_writer_varying:true
            write_term write_view read_view
      | [], [read_term] ->
          analyze_weak_zero_subscripts read_bounds ~is_writer_varying:false
            read_term read_view write_view
      | _, _ -> analyze_gcd_subscripts write_view read_view)
  | (Single _ | All | Upfrom _ | Between _ | MultiIndex _), _ -> Unconstrained

let compare_steps (write : Access_function.t step)
    (read : Access_function.t step) : subscript_result =
  match (write, read) with
  | Field write_field, Field read_field ->
      if write_field = read_field then Unconstrained else Disjoint
  | Subscript write_index, Subscript read_index ->
      analyze_subscripts write_index read_index
  | Field _, Subscript _ | Subscript _, Field _ -> Unconstrained

let directions_of_constraint (loop_constraint : loop_constraint) :
    Direction_set.t =
  match loop_constraint with
  | Distance distance -> Direction_set.of_distance distance
  | Directions directions -> directions

let intersect_constraints (first : loop_constraint) (second : loop_constraint) :
    loop_constraint option =
  let directions =
    Direction_set.inter
      (directions_of_constraint first)
      (directions_of_constraint second) in
  match (first, second) with
  | _ when Direction_set.is_empty directions -> None
  | Distance first_distance, Distance second_distance
    when first_distance <> second_distance ->
      None
  | Distance _, _ -> Some first
  | Directions _, Distance _ -> Some second
  | Directions _, Directions _ -> Some (Directions directions)

let rec constrain_vector (loop_id : label) (new_constraint : loop_constraint)
    (vector : dependence_vector) : dependence_vector option =
  match vector with
  | [] -> Some [] (* A non-common loop cannot constrain execution order. *)
  | (current_loop_id, existing_constraint) :: remaining_constraints -> (
      if current_loop_id = loop_id then
        match intersect_constraints existing_constraint new_constraint with
        | None -> None
        | Some intersected_constraint ->
            Some
              ((current_loop_id, intersected_constraint)
              :: remaining_constraints)
      else
        match constrain_vector loop_id new_constraint remaining_constraints with
        | None -> None
        | Some constrained_tail ->
            Some ((current_loop_id, existing_constraint) :: constrained_tail))

(* Loop IDs describe lexical ancestry, outermost first, from the same region.
   Once ancestors differ, no subsequent loop can be shared. *)
let rec unconstrained_common_loops (writer : label list) (reader : label list) :
    dependence_vector =
  match (writer, reader) with
  | write_id :: writes, read_id :: reads when write_id = read_id ->
      (write_id, Directions Direction_set.all)
      :: unconstrained_common_loops writes reads
  | _ -> []

let rec constrain_by_paths (vector : dependence_vector) (writes : path)
    (reads : path) : dependence_vector option =
  match (writes, reads) with
  | write :: remaining_writes, read :: remaining_reads -> (
      match compare_steps write read with
      | Disjoint -> None
      | Unconstrained ->
          constrain_by_paths vector remaining_writes remaining_reads
      | Constrain (loop_id, new_constraint) -> (
          match constrain_vector loop_id new_constraint vector with
          | None -> None
          | Some constrained_vector ->
              constrain_by_paths constrained_vector remaining_writes
                remaining_reads))
  | _ -> Some vector

let analyze_dependence (write_loop_ids : label list) (write_path : path)
    (read_loop_ids : label list) (read_path : path) : dependence_vector option =
  let unconstrained = unconstrained_common_loops write_loop_ids read_loop_ids in
  (* Unequal paths may contain slicing; do not align their dimensions. *)
  if List.compare_lengths write_path read_path <> 0 then Some unconstrained
  else constrain_by_paths unconstrained write_path read_path

let rec may_write_precede_read (is_writer_first : bool)
    (vector : dependence_vector) : bool =
  match vector with
  | [] -> is_writer_first
  | (_, loop_constraint) :: remaining_constraints ->
      let directions = directions_of_constraint loop_constraint in
      directions.is_writer_before
      || directions.is_same_iteration
         && may_write_precede_read is_writer_first remaining_constraints
