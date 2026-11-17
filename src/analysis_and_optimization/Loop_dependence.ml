open Std
open Middle
open Dataflow_types

(* Overflow in a proof falls back to unknown; it never wraps a coefficient. *)
module Checked = struct
  exception Overflow

  let of_int64 value =
    if value < Int64.of_int min_int || value > Int64.of_int max_int then
      raise Overflow;
    Int64.to_int value

  let add a b = of_int64 (Int64.add (Int64.of_int a) (Int64.of_int b))
  let sub a b = of_int64 (Int64.sub (Int64.of_int a) (Int64.of_int b))
  let neg a = sub 0 a
  let abs a = if a < 0 then neg a else a

  let mul a b =
    let product = Int64.mul (Int64.of_int a) (Int64.of_int b) in
    if b <> 0 && Int64.div product (Int64.of_int b) <> Int64.of_int a then
      raise Overflow;
    of_int64 product

  let div a b =
    if b = 0 || (a = min_int && b = -1) then raise Overflow;
    a / b
end

module Affine = struct
  type key = Loop of label | Invariant of Expr.Typed.t

  module Terms = Stdlib.Map.Make (struct
    type t = key

    let compare a b =
      match (a, b) with
      | Loop x, Loop y -> Int.compare x y
      | Invariant x, Invariant y -> Expr.Helpers.compare_ignore_meta x y
      | Loop _, Invariant _ -> -1
      | Invariant _, Loop _ -> 1
  end)

  type sum = {offset: int; terms: int Terms.t}

  type t =
    { constant: int
    ; loops: (label * int) list
    ; invariants: (Expr.Typed.t * int) list }

  let constant offset = {offset; terms= Terms.empty}
  let term key = {offset= 0; terms= Terms.singleton key 1}

  let add a b =
    let combine _ left right =
      let coefficient = Checked.add left right in
      if coefficient = 0 then None else Some coefficient in
    { offset= Checked.add a.offset b.offset
    ; terms= Terms.union combine a.terms b.terms }

  let scale factor sum =
    if factor = 0 then constant 0
    else
      { offset= Checked.mul factor sum.offset
      ; terms= Terms.map (Checked.mul factor) sum.terms }

  let is_stable defined_vars expr =
    (not (Mir_utils.cannot_duplicate_expr expr))
    && Set.Poly.disjoint defined_vars (Mir_utils.expr_var_names_set expr)

  let invariant defined_vars expr =
    if is_stable defined_vars expr then Some (term (Invariant expr)) else None

  let rec normalize defined_vars variables (expr : Expr.Typed.t) =
    let binary operation left right =
      match
        ( normalize defined_vars variables left
        , normalize defined_vars variables right )
      with
      | Some a, Some b -> operation a b
      | _ -> None in
    match expr.pattern with
    | Lit (Int, digits) -> Option.map (Int.of_string_opt digits) ~f:constant
    | Var name -> (
        match String.Map.find_opt name variables with
        | Some loop -> Some (term (Loop loop))
        | None -> invariant defined_vars expr)
    | FunApp (Operator Plus, [a; b]) -> binary (fun x y -> Some (add x y)) a b
    | FunApp (Operator Minus, [a; b]) ->
        binary (fun x y -> Some (add x (scale (-1) y))) a b
    | FunApp (Operator Times, [a; b]) ->
        binary
          (fun x y ->
            if Terms.is_empty x.terms then Some (scale x.offset y)
            else if Terms.is_empty y.terms then Some (scale y.offset x)
            else invariant defined_vars expr)
          a b
    | FunApp (Operator PMinus, [a]) ->
        Option.map (normalize defined_vars variables a) ~f:(scale (-1))
    | FunApp (Operator PPlus, [a]) -> normalize defined_vars variables a
    | _ -> invariant defined_vars expr

  let of_expr defined_vars variables expr =
    try
      Option.map (normalize defined_vars variables expr) ~f:(fun sum ->
          let loops, invariants =
            Terms.fold
              (fun key coefficient (loops, invariants) ->
                match key with
                | Loop id -> ((id, coefficient) :: loops, invariants)
                | Invariant invariant_expression ->
                    (loops, (invariant_expression, coefficient) :: invariants))
              sum.terms ([], []) in
          {constant= sum.offset; loops; invariants})
    with Checked.Overflow -> None

  let same_invariants a b =
    List.equal a.invariants b.invariants ~eq:(fun (x, xc) (y, yc) ->
        xc = yc && Expr.Helpers.compare_ignore_meta x y = 0)
end

type bounds = {lower: int option; upper: int option}

type scope =
  {loop_ids: label list; variables: label String.Map.t; controls: controls}

type t =
  { scopes: scope LabelMap.t
  ; bounds: bounds LabelMap.t
  ; defined_vars: string Set.Poly.t
  ; expressions: (label * Expr.Typed.t, Affine.t option) Stdlib.Hashtbl.t }

let create statement_map controls_by_statement defined_vars =
  let constant_bound expr =
    match Affine.of_expr defined_vars String.Map.empty expr with
    | Some {constant; loops= []; invariants= []} -> Some constant
    | _ -> None in
  let loop_variables, bounds =
    LabelMap.fold statement_map ~init:(LabelMap.empty, LabelMap.empty)
      ~f:(fun ~key:id ~data:(pattern, _) (variables, domains) ->
        match pattern with
        | Stmt.Pattern.For {loopvar; lower; upper; _} ->
            ( LabelMap.add variables ~key:id ~data:loopvar
            , LabelMap.add domains ~key:id
                ~data:{lower= constant_bound lower; upper= constant_bound upper}
            )
        | _ -> (variables, domains)) in
  let scopes =
    LabelMap.mapi statement_map ~f:(fun id (pattern, _) ->
        let statement_controls = LabelMap.find id controls_by_statement in
        let variables =
          List.fold_left statement_controls.enclosing ~init:String.Map.empty
            ~f:(fun bindings enclosing ->
              match LabelMap.find_opt enclosing.id loop_variables with
              | None -> bindings
              | Some name ->
                  String.Map.add bindings ~key:name ~data:enclosing.id) in
        { loop_ids=
            Dataflow_utils.enclosing_loop_ids id pattern statement_controls
        ; variables
        ; controls= statement_controls }) in
  {scopes; bounds; defined_vars; expressions= Stdlib.Hashtbl.create 32}

let affine_view context statement_id expr =
  let key = (statement_id, expr) in
  match Stdlib.Hashtbl.find_opt context.expressions key with
  | Some view -> view
  | None ->
      let scope = LabelMap.find statement_id context.scopes in
      let view = Affine.of_expr context.defined_vars scope.variables expr in
      Stdlib.Hashtbl.add context.expressions key view;
      view

type directions = {before: bool; same: bool; after: bool}

(* Distance is the reader iteration minus the writer iteration. *)
type loop_constraint = Distance of int | Directions of directions
type dependence_vector = (label * loop_constraint) list

type subscript_result =
  | Disjoint
  | Unconstrained
  | Constrain of label * loop_constraint

let all_directions = {before= true; same= true; after= true}

let directions = function
  | Distance d -> {before= d > 0; same= d = 0; after= d < 0}
  | Directions choices -> choices

let intersect_constraints first second =
  let a = directions first and b = directions second in
  let common =
    { before= a.before && b.before
    ; same= a.same && b.same
    ; after= a.after && b.after } in
  match (first, second) with
  | _ when not (common.before || common.same || common.after) -> None
  | Distance first_distance, Distance second_distance
    when first_distance <> second_distance ->
      None
  | Distance _, _ -> Some first
  | _, Distance _ -> Some second
  | _ -> Some (Directions common)

let rec constrain_vector loop constraint_ vector =
  match vector with
  | [] -> Some []
  | (id, existing) :: tail ->
      if id = loop then
        Option.map (intersect_constraints existing constraint_)
          ~f:(fun combined -> (id, combined) :: tail)
      else
        Option.map (constrain_vector loop constraint_ tail)
          ~f:(fun constrained_tail -> (id, existing) :: constrained_tail)

let loop_bounds context loop =
  Option.value
    (LabelMap.find_opt loop context.bounds)
    ~default:{lower= None; upper= None}

let compare_equal_strides context (loop, stride) (write : Affine.t)
    (read : Affine.t) =
  let difference = Checked.sub write.constant read.constant in
  let distance = Checked.div difference stride in
  if difference mod stride <> 0 then Disjoint
  else
    let bounds = loop_bounds context loop in
    match (bounds.lower, bounds.upper) with
    | Some lower, Some upper when Checked.abs distance > Checked.sub upper lower
      ->
        Disjoint
    | _ -> Constrain (loop, Distance distance)

let compare_fixed_and_varying context (loop, stride) (varying : Affine.t)
    (fixed : Affine.t) ~is_writer_varying =
  let difference = Checked.sub fixed.constant varying.constant in
  let iteration = Checked.div difference stride in
  let bounds = loop_bounds context loop in
  let outside =
    Option.value_map bounds.lower ~default:false ~f:(fun lower ->
        iteration < lower)
    || Option.value_map bounds.upper ~default:false ~f:(fun upper ->
        iteration > upper) in
  if difference mod stride <> 0 || outside then Disjoint
  else
    let earlier =
      Option.value_map bounds.lower ~default:true ~f:(fun lower ->
          lower < iteration) in
    let later =
      Option.value_map bounds.upper ~default:true ~f:(fun upper ->
          iteration < upper) in
    Constrain
      ( loop
      , Directions
          { before= (if is_writer_varying then later else earlier)
          ; same= true
          ; after= (if is_writer_varying then earlier else later) } )

let rec gcd a b = if b = 0 then Checked.abs a else gcd b (a mod b)

let compare_affine context (write : Affine.t) (read : Affine.t) =
  try
    if not (Affine.same_invariants write read) then Unconstrained
    else
      match (write.loops, read.loops) with
      | [], [] ->
          if write.constant = read.constant then Unconstrained else Disjoint
      | [write_term], [read_term] when write_term = read_term ->
          compare_equal_strides context write_term write read
      | [write_term], [] ->
          compare_fixed_and_varying context write_term write read
            ~is_writer_varying:true
      | [], [read_term] ->
          compare_fixed_and_varying context read_term read write
            ~is_writer_varying:false
      | _ ->
          let divisor =
            List.fold_left (write.loops @ read.loops) ~init:0
              ~f:(fun result (_, coefficient) -> gcd result coefficient) in
          if
            divisor > 0
            && Checked.sub read.constant write.constant mod divisor <> 0
          then Disjoint
          else Unconstrained
  with Checked.Overflow -> Unconstrained

let compare_steps context writer write reader read =
  match (write, read) with
  | Dependence.Field a, Dependence.Field b ->
      if a = b then Unconstrained else Disjoint
  | Subscript (Single a), Subscript (Single b) -> (
      match (affine_view context writer a, affine_view context reader b) with
      | Some write_view, Some read_view ->
          compare_affine context write_view read_view
      | _ -> Unconstrained)
  | _ -> Unconstrained

let rec unconstrained_common_loops writer reader =
  match (writer, reader) with
  | w :: ws, r :: rs when w = r ->
      (w, Directions all_directions) :: unconstrained_common_loops ws rs
  | _ -> []

let rec constrain_paths context writer reader (vector : dependence_vector)
    writes reads =
  match (writes, reads) with
  | ( (Dependence.Field _ as write) :: write_tail
    , (Dependence.Field _ as read) :: read_tail )
   |( (Subscript (Single _) as write) :: write_tail
    , (Subscript (Single _) as read) :: read_tail ) -> (
      match compare_steps context writer write reader read with
      | Disjoint -> None
      | Unconstrained ->
          constrain_paths context writer reader vector write_tail read_tail
      | Constrain (loop, constraint_) ->
          Option.bind (constrain_vector loop constraint_ vector)
            ~f:(fun constrained ->
              constrain_paths context writer reader constrained write_tail
                read_tail))
  | _ -> Some vector

let rec permits_forward_order is_writer_first = function
  | [] -> is_writer_first
  | (_, constraint_) :: rest ->
      let choices = directions constraint_ in
      choices.before
      || (choices.same && permits_forward_order is_writer_first rest)

let may_supply_read context writer (write : Dependence.location) reader
    (read : Dependence.location) =
  if not (String.equal write.variable read.variable) then false
  else
    let writer_scope = LabelMap.find writer context.scopes in
    let reader_scope = LabelMap.find reader context.scopes in
    let vector =
      unconstrained_common_loops writer_scope.loop_ids reader_scope.loop_ids
    in
    let constrained =
      if List.compare_lengths write.path read.path <> 0 then Some vector
      else constrain_paths context writer reader vector write.path read.path
    in
    match constrained with
    | None -> false
    | Some possible -> permits_forward_order (writer < reader) possible

let equal_subscripts context killer a previous b ~is_same_iteration =
  if
    Affine.is_stable context.defined_vars a
    && Expr.Helpers.compare_ignore_meta a b = 0
  then true
  else
    match (affine_view context killer a, affine_view context previous b) with
    | Some first, Some second ->
        first.constant = second.constant
        && first.loops = second.loops
        && Affine.same_invariants first second
        && (is_same_iteration || List.is_empty first.loops)
    | _ -> false

let rec covers_path context killer previous ~is_same_iteration writes old_writes
    =
  match (writes, old_writes) with
  | [], _ -> true
  | Dependence.Field a :: ws, Dependence.Field b :: ps ->
      a = b && covers_path context killer previous ~is_same_iteration ws ps
  | Subscript (Single a) :: ws, Subscript (Single b) :: ps ->
      equal_subscripts context killer a previous b ~is_same_iteration
      && covers_path context killer previous ~is_same_iteration ws ps
  | _ -> false

let includes_controls (covering : controls) (covered : controls) =
  List.for_all covered.enclosing ~f:(fun enclosing ->
      List.mem enclosing ~set:covering.enclosing)
  && Set.Poly.subset covered.jump_ids covering.jump_ids

(* A loop-varying write is killed only when every execution is overwritten
   before that iteration can finish or skip the killer. *)
let definitely_overwrites context killer (write : Dependence.location) previous
    (old_write : Dependence.location) =
  if not (String.equal write.variable old_write.variable) then false
  else if
    covers_path context killer previous ~is_same_iteration:false write.path
      old_write.path
  then true
  else
    let previous_scope = LabelMap.find previous context.scopes in
    let killer_scope = LabelMap.find killer context.scopes in
    match List.rev previous_scope.loop_ids with
    | [] -> false
    | innermost :: _ ->
        previous < killer
        && previous_scope.loop_ids = killer_scope.loop_ids
        && includes_controls previous_scope.controls killer_scope.controls
        && includes_controls previous_scope.controls
             (LabelMap.find innermost context.scopes).controls
        && covers_path context killer previous ~is_same_iteration:true
             write.path old_write.path
