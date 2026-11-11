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
  type loop_term = {loop_id: label; is_negated: bool}

  type unit_stride =
    {constant: int; invariant: Expr.Typed.t option; loop_term: loop_term option}

  type t = {expr: Expr.Typed.t; view: unit_stride option}

  let equal_loop_terms (first : loop_term option) (second : loop_term option) :
      bool =
    Option.equal
      (fun (first_term : loop_term) (second_term : loop_term) ->
        first_term.loop_id = second_term.loop_id
        && Bool.equal first_term.is_negated second_term.is_negated)
      first second

  let equal_invariants (first : Expr.Typed.t option)
      (second : Expr.Typed.t option) : bool =
    Option.equal
      (fun first_invariant second_invariant ->
        Expr.Helpers.compare_ignore_meta first_invariant second_invariant = 0)
      first second

  let has_same_terms (first : unit_stride) (second : unit_stride) : bool =
    equal_loop_terms first.loop_term second.loop_term
    && equal_invariants first.invariant second.invariant

  let add (left : unit_stride) (right : unit_stride) : unit_stride option =
    match (left.loop_term, right.loop_term) with
    | Some _, Some _ -> None
    | left_loop_term, right_loop_term ->
        let invariant =
          match (left.invariant, right.invariant) with
          | Some left_invariant, Some right_invariant ->
              Some
                (Expr.Helpers.binop left_invariant Operator.Plus right_invariant)
          | left_invariant, right_invariant ->
              Option.first_some left_invariant right_invariant in
        Some
          { constant= left.constant + right.constant
          ; invariant
          ; loop_term= Option.first_some left_loop_term right_loop_term }

  let negate (term : unit_stride) : unit_stride =
    { constant= -term.constant
    ; invariant=
        Option.map term.invariant ~f:(Expr.Helpers.unary_op Operator.PMinus)
    ; loop_term=
        Option.map term.loop_term ~f:(fun loop_term ->
            {loop_term with is_negated= not loop_term.is_negated}) }

  let subtract (left : unit_stride) (right : unit_stride) : unit_stride option =
    let drop_shared_terms (term : unit_stride) : unit_stride =
      { term with
        invariant=
          (if equal_invariants left.invariant right.invariant then None
           else term.invariant)
      ; loop_term=
          (if equal_loop_terms left.loop_term right.loop_term then None
           else term.loop_term) } in
    add (drop_shared_terms left) (negate (drop_shared_terms right))

  let is_stable (context : Loop_context.t) ~(loop_ids : label list)
      (expr : Expr.Typed.t) : bool =
    let used_names = expr_names expr in
    let is_loop_var name =
      Option.is_some (Loop_context.for_loop_id context ~loop_ids name) in
    Set.Poly.disjoint used_names context.defined_vars
    && (not (Set.Poly.exists used_names ~f:is_loop_var))
    && not (cannot_duplicate_expr expr)

  let unit_stride_of_expr (context : Loop_context.t) ~(loop_ids : label list)
      (expr : Expr.Typed.t) : unit_stride option =
    let of_invariant (subexpr : Expr.Typed.t) : unit_stride option =
      if is_stable context ~loop_ids subexpr then
        Some {constant= 0; invariant= Some subexpr; loop_term= None}
      else None in
    let rec of_sum (subexpr : Expr.Typed.t) : unit_stride option =
      let combine_operands operation lhs rhs =
        match (of_sum lhs, of_sum rhs) with
        | Some left_view, Some right_view -> (
            match operation left_view right_view with
            | Some view -> Some view
            | None -> of_invariant subexpr)
        | None, _ | _, None -> of_invariant subexpr in
      match subexpr.pattern with
      | Var name -> (
          match Loop_context.for_loop_id context ~loop_ids name with
          | Some loop_id ->
              Some
                { constant= 0
                ; invariant= None
                ; loop_term= Some {loop_id; is_negated= false} }
          | None -> of_invariant subexpr)
      | Lit (Int, digits) -> (
          match Int.of_string_opt digits with
          | Some constant -> Some {constant; invariant= None; loop_term= None}
          | None ->
              Common.ICE.internal_error
                ("Integer literal does not fit an int: " ^ digits)
              [@coverage off])
      | FunApp (Operator Plus, [lhs; rhs]) -> combine_operands add lhs rhs
      | FunApp (Operator Minus, [lhs; rhs]) -> combine_operands subtract lhs rhs
      | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _
       |TupleProjection _ | Promotion _ ->
          of_invariant subexpr in
    of_sum expr

  let of_expr (context : Loop_context.t) ~(loop_ids : label list)
      (expr : Expr.Typed.t) : t =
    {expr; view= unit_stride_of_expr context ~loop_ids expr}

  let is_equal_value ~(is_same_iteration : bool) (first : t) (second : t) : bool
      =
    match (first.view, second.view) with
    | Some first_view, Some second_view ->
        has_same_terms first_view second_view
        && first_view.constant = second_view.constant
        && (is_same_iteration || Option.is_none first_view.loop_term)
    | None, _ | _, None -> false

  let pp_unit_stride ~(loop_name : label -> string) ppf
      ({constant; invariant; loop_term} : unit_stride) =
    Option.iter loop_term ~f:(fun {loop_id; is_negated} ->
        Fmt.pf ppf "%s%s" (if is_negated then "-" else "") (loop_name loop_id));
    let is_leading = Option.is_none loop_term in
    Option.iter invariant ~f:(fun (invariant : Expr.Typed.t) ->
        match invariant.pattern with
        | FunApp (Operator PMinus, [negated]) ->
            Fmt.pf ppf "-%a" Expr.Typed.pp negated
        | _ ->
            Fmt.pf ppf "%s%a"
              (if is_leading then "" else "+")
              Expr.Typed.pp invariant);
    let is_first = is_leading && Option.is_none invariant in
    if constant <> 0 || is_first then
      Fmt.pf ppf "%s%d"
        (if constant < 0 then "-" else if is_first then "" else "+")
        (abs constant)

  let pp ~loop_name ppf ({expr; view} : t) =
    match view with
    | Some unit_stride -> pp_unit_stride ~loop_name ppf unit_stride
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

type subscript_constraint =
  | Disjoint
  | Distance of {loop_id: label; distance: int}
  | No_information

let test_constant_subscripts (write : Access_function.unit_stride)
    (read : Access_function.unit_stride) : subscript_constraint =
  if write.constant = read.constant then No_information else Disjoint

let test_equal_stride_subscripts (loop_term : Access_function.loop_term)
    (write : Access_function.unit_stride) (read : Access_function.unit_stride) :
    subscript_constraint =
  let constant_difference = write.constant - read.constant in
  Distance
    { loop_id= loop_term.loop_id
    ; distance=
        (if loop_term.is_negated then -constant_difference
         else constant_difference) }

let test_subscripts (write : Access_function.t Index.t)
    (read : Access_function.t Index.t) : subscript_constraint =
  match (write, read) with
  | Single {view= Some write_view; _}, Single {view= Some read_view; _}
    when Access_function.has_same_terms write_view read_view -> (
      match write_view.loop_term with
      | None -> test_constant_subscripts write_view read_view
      | Some loop_term ->
          test_equal_stride_subscripts loop_term write_view read_view)
  | (Single _ | All | Upfrom _ | Between _ | MultiIndex _), _ -> No_information

let test_steps (write : Access_function.t step) (read : Access_function.t step)
    : subscript_constraint =
  match (write, read) with
  | Field write_field, Field read_field ->
      if write_field = read_field then No_information else Disjoint
  | Subscript write_index, Subscript read_index ->
      test_subscripts write_index read_index
  | Field _, Subscript _ | Subscript _, Field _ -> No_information

type t = Independent | Possible_overlap of (label * int option) list

let constrain_distance (distances : (label * int option) list)
    ~(loop_id : label) ~(distance : int) : (label * int option) list option =
  let constrained (common_loop_id, known_distance) =
    match known_distance with
    | _ when common_loop_id <> loop_id -> Some (common_loop_id, known_distance)
    | Some known when known <> distance -> None
    | Some _ | None -> Some (common_loop_id, Some distance) in
  Option.all (List.map distances ~f:constrained)

let test_dependence ((writer_loop_ids, write_path) : label list * path)
    ((reader_loop_ids, read_path) : label list * path) : t =
  let common_loop_ids =
    List.filter writer_loop_ids ~f:(fun loop_id ->
        List.mem loop_id ~set:reader_loop_ids) in
  let no_information =
    List.map common_loop_ids ~f:(fun loop_id -> (loop_id, None)) in
  if List.compare_lengths write_path read_path <> 0 then
    Possible_overlap no_information
  else
    List.fold_left2 write_path read_path ~init:(Possible_overlap no_information)
      ~f:(fun dependence write_step read_step ->
        match dependence with
        | Independent -> Independent
        | Possible_overlap distances -> (
            match test_steps write_step read_step with
            | Disjoint -> Independent
            | No_information -> dependence
            | Distance {loop_id; distance} -> (
                match constrain_distance distances ~loop_id ~distance with
                | Some distances -> Possible_overlap distances
                | None -> Independent)))

let is_overlap_possible : t -> bool = function
  | Independent -> false
  | Possible_overlap _ -> true

let is_writer_possibly_first ~(is_writer_first : bool) : t -> bool = function
  | Independent -> false
  | Possible_overlap distances -> (
      match
        List.find_map distances ~f:(function
          | _, Some 0 -> None
          | _, distance -> Some distance)
      with
      | None -> is_writer_first
      | Some (Some distance) -> distance > 0
      | Some None -> true)

let known_distance ~(loop_id : label) : t -> int option = function
  | Independent -> None
  | Possible_overlap distances -> Option.join (List.assoc_opt loop_id distances)

let pp_distance ~(loop_name : label -> string) ppf
    ((loop_id, distance) : label * int option) =
  Fmt.pf ppf "[%s:%a]" (loop_name loop_id)
    Fmt.(option ~none:(any "*") int)
    distance

let pp ~(loop_name : label -> string) ppf = function
  | Independent -> Fmt.string ppf "independent"
  | Possible_overlap distances
    when List.for_all distances ~f:(fun (_, distance) ->
             Option.is_none distance) ->
      Fmt.string ppf "unknown"
  | Possible_overlap distances ->
      Fmt.pf ppf "distances %a"
        Fmt.(list ~sep:nop (pp_distance ~loop_name))
        distances
