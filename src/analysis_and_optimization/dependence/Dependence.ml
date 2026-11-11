open Std
open Std.Compare
open Std.Sexp_conv
open Middle
open Dataflow_types
open Mir_utils

module Loop_context = struct
  type t =
    { loops: (Expr.Typed.t, label) Stmt.Pattern.t LabelMap.t
    ; defined_vars: string Set.Poly.t }

  let create
      (statement_map :
        ((Expr.Typed.t, label) Stmt.Pattern.t * 'meta) LabelMap.t)
      ~(defined_vars : string Set.Poly.t) : t =
    let loop_statement _ (pattern, _) =
      match (pattern : (Expr.Typed.t, label) Stmt.Pattern.t) with
      | For _ | While _ -> Some pattern
      | Assignment _ | TargetPE _ | JacobianPE _ | NRFunApp _ | Break
       |Continue | Return _ | Skip | IfElse _ | Profile _ | Block _ | SList _
       |Decl _ ->
          None in
    {loops= LabelMap.filter_map statement_map ~f:loop_statement; defined_vars}

  let for_loop_id (context : t) ~(loop_ids : label list) (var : string) :
      label option =
    List.find_opt loop_ids ~f:(fun loop_id ->
        match LabelMap.find_opt loop_id context.loops with
        | Some (For {loopvar; _}) -> String.equal loopvar var
        | Some _ | None -> false)

  let for_loop_bounds (context : t) (loop_id : label) :
      (Expr.Typed.t * Expr.Typed.t) option =
    match LabelMap.find_opt loop_id context.loops with
    | Some (For {lower; upper; _}) -> Some (lower, upper)
    | Some _ | None -> None
end

module Accesses = struct
  type 'scalar step = Subscript of 'scalar Index.t | Field of int
  type kind = Read | Write | ReadWrite [@@deriving equal]
  type 'scalar access = {var: string; path: 'scalar step list; kind: kind}
  type 'scalar t = 'scalar access list

  module Path = struct
    let rec of_expr (expr : Expr.Typed.t) :
        (string * Expr.Typed.t step list) option =
      let extend_path base steps =
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

    let rec of_lvalue ((lbase, indices) : Expr.Typed.t Stmt.Pattern.lvalue) :
        Expr.Typed.t step list =
      (match lbase with
        | LVariable _ -> []
        | LTupleProjection (inner, field) -> of_lvalue inner @ [Field field])
      @ List.map indices ~f:(fun index -> Subscript index)

    let subscript_exprs (path : Expr.Typed.t step list) : Expr.Typed.t list =
      List.concat_map path ~f:(function
        | Subscript index -> Index.bounds index
        | Field _ -> [])
  end

  let is_same_syntax (first : Expr.Typed.t step list)
      (second : Expr.Typed.t step list) : bool =
    let is_same_step first_step second_step =
      match (first_step, second_step) with
      | Field first_field, Field second_field -> first_field = second_field
      | Subscript first_index, Subscript second_index ->
          Index.compare Expr.Helpers.compare_ignore_meta first_index
            second_index
          = 0
      | Field _, Subscript _ | Subscript _, Field _ -> false in
    List.equal ~eq:is_same_step first second

  let rec collect_expr (expr : Expr.Typed.t) : Expr.Typed.t t =
    let of_children () =
      Expr.Pattern.fold
        (fun running_accesses subexpr ->
          running_accesses @ collect_expr subexpr)
        [] expr.pattern in
    let target kind = {var= "target"; path= []; kind} in
    match expr.pattern with
    | Var name -> [{var= name; path= []; kind= Read}]
    | Indexed _ | TupleProjection _ -> (
        match Path.of_expr expr with
        | Some (name, path) ->
            {var= name; path; kind= Read}
            :: List.concat_map (Path.subscript_exprs path) ~f:collect_expr
        | None -> of_children ())
    | FunApp (StanLib (_, FnTarget), []) -> [target Read]
    | FunApp (fun_kind, _) when is_target_increment fun_kind ->
        of_children () @ [target ReadWrite]
    | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ ->
        of_children ()

  let of_assignment (var : string) (path : Expr.Typed.t step list)
      (reads : Expr.Typed.t t) : Expr.Typed.t t =
    let is_read_of_written (read : Expr.Typed.t access) =
      String.equal read.var var && equal_kind read.kind Read
      && is_same_syntax read.path path in
    if List.exists reads ~f:is_read_of_written then
      List.filter reads ~f:(fun read -> not (is_read_of_written read))
      @ [{var; path; kind= ReadWrite}]
    else reads @ [{var; path; kind= Write}]

  let of_statement (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) :
      Expr.Typed.t t =
    let of_children () =
      Stmt.Pattern.fold
        (fun running_accesses subexpr ->
          running_accesses @ collect_expr subexpr)
        Fun.const [] stmt in
    let target_read_write = {var= "target"; path= []; kind= ReadWrite} in
    match stmt with
    | Assignment (lhs, _, rhs) ->
        let path = Path.of_lvalue lhs in
        of_assignment
          (Stmt.Helpers.lhs_variable lhs)
          path
          (List.concat_map (Path.subscript_exprs path @ [rhs]) ~f:collect_expr)
    | Decl {decl_id; _} ->
        of_children () @ [{var= decl_id; path= []; kind= Write}]
    | TargetPE _ | JacobianPE _ -> of_children () @ [target_read_write]
    | NRFunApp (fun_kind, _) when is_target_increment fun_kind ->
        of_children () @ [target_read_write]
    | NRFunApp _ | Return _ | IfElse _ | While _ | For _ | Profile _ | Block _
     |SList _ | Break | Continue | Skip ->
        of_children ()

  let map_scalars ~(f : 'scalar -> 'mapped) (access : 'scalar access) :
      'mapped access =
    let map_step = function
      | Subscript index -> Subscript (Index.map f index)
      | Field field -> Field field in
    {access with path= List.map access.path ~f:map_step}

  let used_vars (accesses : 'scalar t) : string Set.Poly.t =
    let used_var {var; kind; _} =
      match kind with Read | ReadWrite -> Some var | Write -> None in
    accesses |> List.filter_map ~f:used_var |> Set.Poly.of_list

  let defined_vars (accesses : 'scalar t) : string Set.Poly.t =
    let defined_var {var; kind; _} =
      match kind with Write | ReadWrite -> Some var | Read -> None in
    accesses |> List.filter_map ~f:defined_var |> Set.Poly.of_list

  module Printing = struct
    let pp_subscript pp_scalar ppf (index : 'scalar Index.t) =
      match index with
      | MultiIndex indices -> Fmt.pf ppf "{%a}" pp_scalar indices
      | All | Single _ | Upfrom _ | Between _ -> Index.pp pp_scalar ppf index

    let rec pp_path pp_scalar ppf = function
      | [] -> ()
      | Field field :: rest -> Fmt.pf ppf ".%d%a" field (pp_path pp_scalar) rest
      | Subscript index :: rest ->
          Fmt.pf ppf "[%a%a" (pp_subscript pp_scalar) index
            (pp_subscripts_after pp_scalar)
            rest

    and pp_subscripts_after pp_scalar ppf = function
      | Subscript index :: rest ->
          Fmt.pf ppf ", %a%a" (pp_subscript pp_scalar) index
            (pp_subscripts_after pp_scalar)
            rest
      | path -> Fmt.pf ppf "]%a" (pp_path pp_scalar) path

    let pp_kind ppf kind =
      Fmt.string ppf
        (match kind with Read -> "R" | ReadWrite -> "RW" | Write -> "W")

    let pp_access pp_scalar ppf {var; path; kind} =
      Fmt.pf ppf "%a %s%a" pp_kind kind var (pp_path pp_scalar) path

    let pp pp_scalar ppf (accesses : 'scalar t) =
      let reads, read_writes, writes =
        List.partition3_map accesses ~f:(fun access ->
            match access.kind with
            | Read -> `Fst access
            | ReadWrite -> `Snd access
            | Write -> `Trd access) in
      Fmt.(list ~sep:(any ", ") (pp_access pp_scalar))
        ppf
        (reads @ read_writes @ writes)
  end

  let pp_path = Printing.pp_path
  let pp_access = Printing.pp_access
  let pp = Printing.pp
end

module Access_function = struct
  type 'key term = {key: 'key; coefficient: int}
  type loop = {loop_id: label; lower: int option; upper: int option}

  type affine =
    { constant: int
    ; loop_terms: loop term list
    ; invariant_terms: Expr.Typed.t term list }

  let compare_loops (first : loop) (second : loop) : int =
    Int.compare first.loop_id second.loop_id

  type t = {expr: Expr.Typed.t; view: affine option}

  let rec add_terms ~(compare : 'key -> 'key -> int) (left : 'key term list)
      (right : 'key term list) : 'key term list =
    match (left, right) with
    | [], terms | terms, [] -> terms
    | left_term :: left_rest, right_term :: right_rest ->
        let order = compare left_term.key right_term.key in
        let coefficient = left_term.coefficient + right_term.coefficient in
        if order < 0 then left_term :: add_terms ~compare left_rest right
        else if order > 0 then right_term :: add_terms ~compare left right_rest
        else if coefficient = 0 then add_terms ~compare left_rest right_rest
        else
          {left_term with coefficient}
          :: add_terms ~compare left_rest right_rest

  let scale_terms (factor : int) (terms : 'key term list) : 'key term list =
    if factor = 0 then []
    else
      List.map terms ~f:(fun term ->
          {term with coefficient= factor * term.coefficient})

  let add (left : affine) (right : affine) : affine =
    { constant= left.constant + right.constant
    ; loop_terms=
        add_terms ~compare:compare_loops left.loop_terms right.loop_terms
    ; invariant_terms=
        add_terms ~compare:Expr.Helpers.compare_ignore_meta left.invariant_terms
          right.invariant_terms }

  let scale (factor : int) (view : affine) : affine =
    { constant= factor * view.constant
    ; loop_terms= scale_terms factor view.loop_terms
    ; invariant_terms= scale_terms factor view.invariant_terms }

  let subtract (left : affine) (right : affine) : affine =
    add left (scale (-1) right)

  let multiply (left : affine) (right : affine) : affine option =
    let is_constant (view : affine) =
      List.is_empty view.loop_terms && List.is_empty view.invariant_terms in
    if is_constant left then Some (scale left.constant right)
    else if is_constant right then Some (scale right.constant left)
    else None

  let is_stable (context : Loop_context.t) ~(loop_ids : label list)
      (expr : Expr.Typed.t) : bool =
    let used_names = expr_names expr in
    let is_loop_var name =
      Option.is_some (Loop_context.for_loop_id context ~loop_ids name) in
    Set.Poly.disjoint used_names context.defined_vars
    && (not (Set.Poly.exists used_names ~f:is_loop_var))
    && not (cannot_duplicate_expr expr)

  let rec affine_of_expr (context : Loop_context.t) ~(loop_ids : label list)
      (expr : Expr.Typed.t) : affine option =
    let literal_bound (bound : Expr.Typed.t) : int option =
      match affine_of_expr context ~loop_ids:[] bound with
      | Some {constant; loop_terms= []; invariant_terms= []} -> Some constant
      | Some _ | None -> None in
    let loop_of_id (loop_id : label) : loop =
      match Loop_context.for_loop_bounds context loop_id with
      | Some (lower, upper) ->
          {loop_id; lower= literal_bound lower; upper= literal_bound upper}
      | None -> {loop_id; lower= None; upper= None} in
    let of_atom (subexpr : Expr.Typed.t) : affine option =
      if is_stable context ~loop_ids subexpr then
        Some
          { constant= 0
          ; loop_terms= []
          ; invariant_terms= [{key= subexpr; coefficient= 1}] }
      else None in
    let rec of_linear (subexpr : Expr.Typed.t) : affine option =
      let or_atom (view : affine option) : affine option =
        match view with Some _ -> view | None -> of_atom subexpr in
      let combine operation lhs rhs =
        match (of_linear lhs, of_linear rhs) with
        | Some left_view, Some right_view -> operation left_view right_view
        | None, _ | _, None -> None in
      match subexpr.pattern with
      | Var name -> (
          match Loop_context.for_loop_id context ~loop_ids name with
          | Some loop_id ->
              Some
                { constant= 0
                ; loop_terms= [{key= loop_of_id loop_id; coefficient= 1}]
                ; invariant_terms= [] }
          | None -> of_atom subexpr)
      | Lit (Int, digits) ->
          Option.map (Int.of_string_opt digits) ~f:(fun constant ->
              {constant; loop_terms= []; invariant_terms= []})
      | FunApp (Operator Plus, [lhs; rhs]) ->
          or_atom (combine (fun left right -> Some (add left right)) lhs rhs)
      | FunApp (Operator Minus, [lhs; rhs]) ->
          or_atom
            (combine (fun left right -> Some (subtract left right)) lhs rhs)
      | FunApp (Operator Times, [lhs; rhs]) ->
          or_atom (combine multiply lhs rhs)
      | FunApp (Operator PMinus, [operand]) ->
          or_atom (Option.map (of_linear operand) ~f:(scale (-1)))
      | FunApp (Operator PPlus, [operand]) -> or_atom (of_linear operand)
      | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _
       |TupleProjection _ | Promotion _ ->
          of_atom subexpr in
    of_linear expr

  let of_expr (context : Loop_context.t) ~(loop_ids : label list)
      (expr : Expr.Typed.t) : t =
    {expr; view= affine_of_expr context ~loop_ids expr}

  let equal_terms ~(compare : 'key -> 'key -> int) (first : 'key term list)
      (second : 'key term list) : bool =
    List.equal first second ~eq:(fun first_term second_term ->
        compare first_term.key second_term.key = 0
        && first_term.coefficient = second_term.coefficient)

  let has_same_invariant_terms (first : affine) (second : affine) : bool =
    equal_terms ~compare:Expr.Helpers.compare_ignore_meta first.invariant_terms
      second.invariant_terms

  let is_equal_value ~(is_same_iteration : bool) (first : t) (second : t) : bool
      =
    match (first.view, second.view) with
    | Some first_view, Some second_view ->
        first_view.constant = second_view.constant
        && has_same_invariant_terms first_view second_view
        && equal_terms ~compare:compare_loops first_view.loop_terms
             second_view.loop_terms
        && (is_same_iteration || List.is_empty first_view.loop_terms)
    | None, _ | _, None -> false

  let pp_affine ~(loop_name : label -> string) ppf
      ({constant; loop_terms; invariant_terms} : affine) =
    let terms =
      List.map loop_terms ~f:(fun {key; coefficient} ->
          (coefficient, loop_name key.loop_id))
      @ List.map invariant_terms ~f:(fun {key; coefficient} ->
          (coefficient, Fmt.str "%a" Expr.Typed.pp key)) in
    let sign ~is_first value =
      if value < 0 then "-" else if is_first then "" else "+" in
    List.iteri terms ~f:(fun position (coefficient, text) ->
        let sign = sign ~is_first:(position = 0) coefficient in
        if abs coefficient = 1 then Fmt.pf ppf "%s%s" sign text
        else Fmt.pf ppf "%s%d*%s" sign (abs coefficient) text);
    let is_first = List.is_empty terms in
    if constant <> 0 || is_first then
      Fmt.pf ppf "%s%d" (sign ~is_first constant) (abs constant)

  let pp ~loop_name ppf ({expr; view} : t) =
    match view with
    | Some affine -> pp_affine ~loop_name ppf affine
    | None -> Expr.Typed.pp ppf expr
end

open Accesses

type path = Access_function.t step list

let is_same_element ~(is_same_iteration : bool) (first : Access_function.t step)
    (second : Access_function.t step) : bool =
  match (first, second) with
  | Field first_field, Field second_field -> first_field = second_field
  | Subscript (Single first_function), Subscript (Single second_function) ->
      Access_function.is_equal_value ~is_same_iteration first_function
        second_function
  | ( Subscript (Single _ | All | Upfrom _ | Between _ | MultiIndex _)
    , Subscript _ )
   |Field _, Subscript _
   |Subscript _, Field _ ->
      false

let rec is_overwritten ~(is_same_iteration : bool) (killer : path)
    (killed : path) : bool =
  match (killer, killed) with
  | [], _ -> true
  | _ :: _, [] -> false
  | killer_step :: killer_rest, killed_step :: killed_rest ->
      is_same_element ~is_same_iteration killer_step killed_step
      && is_overwritten ~is_same_iteration killer_rest killed_rest

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

  let is_all (directions : t) : bool =
    directions.is_writer_before && directions.is_same_iteration
    && directions.is_writer_after

  let pp ppf (directions : t) =
    if is_all directions then Fmt.string ppf "*"
    else
      Fmt.(list ~sep:(any ",") string)
        ppf
        (List.filter_map ~f:Fun.id
           [ Option.some_if directions.is_writer_before "<"
           ; Option.some_if directions.is_same_iteration "="
           ; Option.some_if directions.is_writer_after ">" ])
end

type subscript_constraint =
  | Disjoint
  | Distance of {loop_id: label; distance: int}
  | Directions of {loop_id: label; directions: Direction_set.t}
  | No_information

let test_constant_subscripts (write : Access_function.affine)
    (read : Access_function.affine) : subscript_constraint =
  if write.constant = read.constant then No_information else Disjoint

let is_longer_than_loop (loop : Access_function.loop) (distance : int) : bool =
  match (loop.lower, loop.upper) with
  | Some lower, Some upper -> abs distance > upper - lower
  | None, _ | _, None -> false

let test_equal_stride_subscripts
    ({key= loop; coefficient= stride} :
      Access_function.loop Access_function.term)
    (write : Access_function.affine) (read : Access_function.affine) :
    subscript_constraint =
  let difference = write.constant - read.constant in
  let distance = difference / stride in
  if difference mod stride <> 0 || is_longer_than_loop loop distance then
    Disjoint
  else Distance {loop_id= loop.loop_id; distance}

let test_weak_zero_subscripts ~(is_writer_varying : bool)
    ({key= loop; coefficient= stride} :
      Access_function.loop Access_function.term)
    (varying : Access_function.affine) (fixed : Access_function.affine) :
    subscript_constraint =
  let is_before_bound bound iteration =
    match bound with Some bound -> iteration < bound | None -> false in
  let is_after_bound bound iteration =
    match bound with Some bound -> iteration > bound | None -> false in
  let difference = fixed.constant - varying.constant in
  let iteration = difference / stride in
  if
    difference mod stride <> 0
    || is_before_bound loop.lower iteration
    || is_after_bound loop.upper iteration
  then Disjoint
  else
    let is_earlier_possible = not (is_before_bound loop.lower (iteration - 1)) in
    let is_later_possible = not (is_after_bound loop.upper (iteration + 1)) in
    Directions
      { loop_id= loop.loop_id
      ; directions=
          { is_writer_before=
              (if is_writer_varying then is_later_possible
               else is_earlier_possible)
          ; is_same_iteration= true
          ; is_writer_after=
              (if is_writer_varying then is_earlier_possible
               else is_later_possible) } }

let rec greatest_common_divisor (left : int) (right : int) : int =
  if right = 0 then abs left else greatest_common_divisor right (left mod right)

let test_gcd_subscripts (write : Access_function.affine)
    (read : Access_function.affine) : subscript_constraint =
  let divisor =
    List.fold_left (write.loop_terms @ read.loop_terms) ~init:0
      ~f:(fun divisor (term : Access_function.loop Access_function.term) ->
        greatest_common_divisor divisor term.coefficient) in
  if divisor > 0 && (read.constant - write.constant) mod divisor <> 0 then
    Disjoint
  else No_information

let test_subscripts (write : Access_function.t Index.t)
    (read : Access_function.t Index.t) : subscript_constraint =
  match (write, read) with
  | Single {view= Some write_view; _}, Single {view= Some read_view; _}
    when Access_function.has_same_invariant_terms write_view read_view -> (
      match (write_view.loop_terms, read_view.loop_terms) with
      | [], [] -> test_constant_subscripts write_view read_view
      | [write_term], [read_term]
        when write_term.key.loop_id = read_term.key.loop_id
             && write_term.coefficient = read_term.coefficient ->
          test_equal_stride_subscripts write_term write_view read_view
      | [write_term], [] ->
          test_weak_zero_subscripts ~is_writer_varying:true write_term
            write_view read_view
      | [], [read_term] ->
          test_weak_zero_subscripts ~is_writer_varying:false read_term read_view
            write_view
      | _, _ -> test_gcd_subscripts write_view read_view)
  | (Single _ | All | Upfrom _ | Between _ | MultiIndex _), _ -> No_information

let test_steps (write : Access_function.t step) (read : Access_function.t step)
    : subscript_constraint =
  match (write, read) with
  | Field write_field, Field read_field ->
      if write_field = read_field then No_information else Disjoint
  | Subscript write_index, Subscript read_index ->
      test_subscripts write_index read_index
  | Field _, Subscript _ | Subscript _, Field _ -> No_information

type loop_relation = {directions: Direction_set.t; distance: int option}
type t = Independent | Possible_overlap of (label * loop_relation) list

let constrain_loop (relations : (label * loop_relation) list) ~(loop_id : label)
    (constraint_ : loop_relation) : (label * loop_relation) list option =
  let constrained (common_loop_id, relation) =
    let directions =
      Direction_set.inter relation.directions constraint_.directions in
    match (relation.distance, constraint_.distance) with
    | _ when common_loop_id <> loop_id -> Some (common_loop_id, relation)
    | Some known, Some distance when known <> distance -> None
    | _ when Direction_set.is_empty directions -> None
    | known, distance ->
        Some
          ( common_loop_id
          , {directions; distance= Option.first_some known distance} ) in
  Option.all (List.map relations ~f:constrained)

let test_dependence ((writer_loop_ids, write_path) : label list * path)
    ((reader_loop_ids, read_path) : label list * path) : t =
  let common_loop_ids =
    List.filter writer_loop_ids ~f:(fun loop_id ->
        List.mem loop_id ~set:reader_loop_ids) in
  let no_information =
    List.map common_loop_ids ~f:(fun loop_id ->
        (loop_id, {directions= Direction_set.all; distance= None})) in
  let constrained relations ~loop_id constraint_ =
    match constrain_loop relations ~loop_id constraint_ with
    | Some relations -> Possible_overlap relations
    | None -> Independent in
  if List.compare_lengths write_path read_path <> 0 then
    Possible_overlap no_information
  else
    List.fold_left2 write_path read_path ~init:(Possible_overlap no_information)
      ~f:(fun dependence write_step read_step ->
        match dependence with
        | Independent -> Independent
        | Possible_overlap relations -> (
            match test_steps write_step read_step with
            | Disjoint -> Independent
            | No_information -> dependence
            | Distance {loop_id; distance} ->
                constrained relations ~loop_id
                  { directions= Direction_set.of_distance distance
                  ; distance= Some distance }
            | Directions {loop_id; directions} ->
                constrained relations ~loop_id {directions; distance= None}))

let is_overlap_possible : t -> bool = function
  | Independent -> false
  | Possible_overlap _ -> true

let is_writer_possibly_first ~(is_writer_first : bool) : t -> bool = function
  | Independent -> false
  | Possible_overlap relations ->
      let rec is_possible = function
        | [] -> is_writer_first
        | (_, {directions; _}) :: inner_relations ->
            directions.Direction_set.is_writer_before
            || (directions.is_same_iteration && is_possible inner_relations)
      in
      is_possible relations

let known_distance ~(loop_id : label) : t -> int option = function
  | Independent -> None
  | Possible_overlap relations ->
      Option.bind (List.assoc_opt loop_id relations) ~f:(fun relation ->
          relation.distance)

let pp_relation ~(loop_name : label -> string) ppf
    ((loop_id, {directions; distance}) : label * loop_relation) =
  match distance with
  | Some distance -> Fmt.pf ppf "[%s:%d]" (loop_name loop_id) distance
  | None -> Fmt.pf ppf "[%s:%a]" (loop_name loop_id) Direction_set.pp directions

let pp ~(loop_name : label -> string) ppf = function
  | Independent -> Fmt.string ppf "independent"
  | Possible_overlap relations
    when List.for_all relations ~f:(fun (_, {directions; distance}) ->
             Option.is_none distance && Direction_set.is_all directions) ->
      Fmt.string ppf "unknown"
  | Possible_overlap relations ->
      Fmt.pf ppf "distances %a"
        Fmt.(list ~sep:nop (pp_relation ~loop_name))
        relations
