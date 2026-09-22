(** Loop vectorization by pi-block code generation (Allen and Kennedy 1987 §5.2;
    design doc [design-docs/active/vectorize-loop-fission.md]).

    For every source [For], innermost first: the whole-loop checks, then
    [Dependence_analysis.build_loop_graph] gives the dependence graph of the
    body and [pi_blocks] the strongly connected components in a legal order. A
    component of one statement that widens becomes one vector statement over the
    whole range; every other component stays in a loop with the original header.
    The decisions print behind [--debug-loop-vectorization]. *)

open Std
open Middle
open Mir_utils
open Dataflow_types
open Dependence_analysis

(* ---- Widening one statement over the loop (design §7.6) ---- *)

(** The loop a statement is widened over, and the variables the body writes. *)
type context =
  { loopvar: string
  ; lower: Expr.Typed.t
  ; upper: Expr.Typed.t
  ; written_vars: string Set.Poly.t }

(** Why a statement cannot become one vector statement; the report prints the
    reason. *)
exception Refused of string

let refused fmt = Fmt.kstr (fun reason -> raise (Refused reason)) fmt

let no_signature name =
  refused "no Stan Math signature for %s over the widened arguments" name

(** The loop variable does not occur in [expr]. *)
let is_invariant (ctx : context) (expr : Expr.Typed.t) : bool =
  not (Set.Poly.mem ctx.loopvar (expr_var_names_set expr))

(** [bound + symbol + const] with [bound]'s type and location, or [bound] alone
    for a zero offset; partial evaluation folds the constants later. *)
let shift_bound (bound : Expr.Typed.t) ({const; symbol; _} : linear) :
    Expr.Typed.t =
  let open Expr.Helpers in
  let shifted =
    Option.value_map symbol ~default:bound ~f:(fun symbol ->
        binop bound Plus symbol) in
  let shifted =
    if const = 0 then shifted
    else if const > 0 then binop shifted Plus (int const)
    else binop shifted Minus (int (-const)) in
  {shifted with meta= bound.meta}

(** [*], [/] and [^] have elementwise variants that accept containers. *)
let elementwise_operator (op : Operator.t) : Operator.t option =
  match op with
  | Times -> Some EltTimes
  | Divide -> Some EltDivide
  | Pow -> Some EltPow
  | _ -> None

(** [call] rebuilt as [kind] over the widened [args] when [return_type] gives a
    container, the Stan Math signatures deciding. *)
let as_container (call : Expr.Typed.t) kind (args : Expr.Typed.t list)
    return_type : Expr.Typed.t option =
  match return_type with
  | Some
      (UnsizedType.ReturnType
         ((UVector | URowVector | UArray (UInt | UReal)) as type_)) ->
      Some Expr.{pattern= FunApp (kind, args); meta= {call.meta with type_}}
  | Some _ | None -> None

(** [name] over the widened [args] at a container return type. *)
let container_call (call : Expr.Typed.t) (name : string) mem
    (args : Expr.Typed.t list) : Expr.Typed.t =
  match
    as_container call
      (StanLib (name, FnPlain, mem))
      args
      (Partial_evaluator.stan_math_return_type name args)
  with
  | Some widened -> widened
  | None -> no_signature name

(** [op] over the widened [args] at a container return type, else the
    elementwise operator. *)
let operator_call (call : Expr.Typed.t) (op : Operator.t)
    (args : Expr.Typed.t list) : Expr.Typed.t =
  let at op =
    as_container call (Operator op) args
      (Partial_evaluator.stan_operator_return_type op args) in
  match
    List.find_map (op :: Option.to_list (elementwise_operator op)) ~f:at
  with
  | Some widened -> widened
  | None -> no_signature (Fmt.str "%a" Operator.pp op)

(** [expr] over the whole range: an invariant scalar stays, a varying scalar
    becomes a container, a container is refused (no broadcast). *)
let rec widen (ctx : context) (expr : Expr.Typed.t) : Expr.Typed.t =
  match expr.meta.type_ with
  | (UInt | UReal | UComplex) when is_invariant ctx expr -> expr
  | UInt | UReal -> (
      match expr.pattern with
      | Var name when String.equal name ctx.loopvar ->
          refused "loop variable %s is used as a value" name
      | Indexed (base, indices) when is_invariant ctx base ->
          let indices = widen_indices ctx indices in
          Expr.
            { pattern= Indexed (base, indices)
            ; meta=
                { expr.meta with
                  type_=
                    Expr.Helpers.infer_type_of_indexed base.meta.type_ indices
                } }
      | FunApp (StanLib (name, FnPlain, mem), args) ->
          container_call expr name mem (List.map args ~f:(widen ctx))
      | FunApp (Operator op, args) ->
          operator_call expr op (List.map args ~f:(widen ctx))
      | _ -> refused "%a cannot be widened" Expr.Typed.pp expr)
  | _ -> refused "%a is a container" Expr.Typed.pp expr

(** The index list with the one position that varies with the loop widened to a
    slice; the other positions must be invariant. *)
and widen_indices (ctx : context) (indices : Expr.Typed.t Index.t list) :
    Expr.Typed.t Index.t list =
  let widened = List.map indices ~f:(widen_index ctx) in
  match List.length (List.filter widened ~f:snd) with
  | 1 -> List.map widened ~f:fst
  | 0 -> refused "no index varies with the loop"
  | _ -> refused "more than one index varies with the loop"

(** One index position and whether the position varies: a point affine in the
    loop variable becomes the shifted range, another varying point a gather. *)
and widen_index (ctx : context) (index : Expr.Typed.t Index.t) :
    Expr.Typed.t Index.t * bool =
  match index with
  | All | Upfrom _ | Between _ | MultiIndex _ ->
      refused "index %a is already a slice" (Index.pp Expr.Typed.pp) index
  | Single expr -> (
      match
        classify_point
          ~loopvars:(Set.Poly.singleton ctx.loopvar)
          ~written_vars:ctx.written_vars expr
      with
      | Affine {loopvar= None; _} -> (index, false)
      | Affine ({loopvar= Some _; _} as offset) ->
          ( Between (shift_bound ctx.lower offset, shift_bound ctx.upper offset)
          , true )
      | Varying Written ->
          refused "index %a mentions a variable written in the loop"
            Expr.Typed.pp expr
      | Varying Nonlinear -> (MultiIndex (widen ctx expr), true))

(** [target += f(args)] over the whole range, or the trip count times the call
    when no argument varies; the density's signature must accept the widened
    arguments at a scalar return type. *)
let widen_density (ctx : context) (call : Expr.Typed.t) name suffix mem
    (args : Expr.Typed.t list) : Expr.Typed.t =
  if cannot_duplicate_expr call then
    refused "the density has side effects or draws random numbers";
  let invariant = List.for_all args ~f:(is_invariant ctx) in
  let args = List.map args ~f:(widen ctx) in
  let widened =
    {call with pattern= FunApp (StanLib (name, suffix, mem), args)} in
  if invariant then
    let open Expr.Helpers in
    { (binop (binop ctx.upper Minus (binop ctx.lower Minus one)) Times widened) with
      meta= call.meta }
  else
    match Partial_evaluator.stan_math_return_type name args with
    | Some (ReturnType UReal) -> widened
    | Some _ | None -> no_signature name

(** [var[indices] = rhs] over the whole range: the varying index becomes a slice
    and the right-hand side must widen to that slice's type. *)
let widen_assignment (ctx : context) (var : string)
    (indices : Expr.Typed.t Index.t list) (var_type : UnsizedType.t)
    (rhs : Expr.Typed.t) =
  if
    cannot_duplicate_expr rhs
    || List.exists indices ~f:(idx_any cannot_duplicate_expr)
  then refused "the assignment has side effects or draws random numbers";
  if is_invariant ctx rhs then refused "the right-hand side is loop-invariant";
  let indices = widen_indices ctx indices in
  let rhs = widen ctx rhs in
  let slice = Expr.Helpers.infer_type_of_indexed var_type indices in
  if not (UnsizedType.equal rhs.meta.type_ slice) then
    refused "the right-hand side widens to %a but the assigned slice is %a"
      UnsizedType.pp rhs.meta.type_ UnsizedType.pp slice;
  Stmt.Pattern.Assignment ((LVariable var, indices), var_type, rhs)

(** [stmt] as one vector statement, recursing through a profile or a scope so
    that every statement inside widens or none does; raises [Refused]. *)
let rec widen_stmt (ctx : context) (stmt : Stmt.Located.t) : Stmt.Located.t =
  let pattern =
    match stmt.pattern with
    | TargetPE
        ({ pattern=
             FunApp
               (StanLib (name, ((FnLpdf _ | FnLpmf _) as suffix), mem), args)
         ; _ } as call) ->
        Stmt.Pattern.TargetPE (widen_density ctx call name suffix mem args)
    | TargetPE _ -> refused "the target increment is not a density call"
    | Assignment ((LVariable var, indices), var_type, rhs) ->
        widen_assignment ctx var indices var_type rhs
    | Assignment ((LTupleProjection _, _), _, _) ->
        refused "assignment to a tuple projection"
    | (Profile _ | Block _ | SList _) as compound ->
        Stmt.Pattern.map Fun.id (widen_stmt ctx) compound
    | Decl {decl_id; _} -> refused "declaration of %s in the loop body" decl_id
    | IfElse _ -> refused "if statement"
    | For _ | While _ -> refused "nested loop"
    | NRFunApp _ -> refused "function call statement"
    | JacobianPE _ | Return _ | Break | Continue | Skip ->
        refused "not an assignment or a density increment" in
  {stmt with pattern}

(* ---- Pi-block code generation (Allen and Kennedy 1987 §5.2; design §7.6)
   ---- *)

(** Decide one pi-block: the members and, for a single leaf without a cycle or
    effects that widens, the vector statement; with each leaf's outcome for the
    report. *)
let decide_block (ctx : context) (map : dep_info_map) (graph : loop_graph)
    (block : label list) :
    (label list * Stmt.Located.t option) * (label * string) list =
  let sequential reason =
    ( (block, None)
    , List.map block ~f:(fun leaf -> (leaf, "sequential: " ^ reason)) ) in
  match block with
  | [_] when is_cyclic map graph block -> sequential "recurrence"
  | [leaf] when has_effects map leaf ->
      sequential "has effects (print, reject or a user-defined function call)"
  | [leaf] -> (
      match widen_stmt ctx (statement_at map leaf) with
      | stmt -> ((block, Some stmt), [(leaf, "hoisted")])
      | exception Refused reason -> sequential reason)
  | _ -> sequential "in a dependence cycle"

(** Typed fusion (Kennedy and Allen 2001 §6.2.5): a sequential block moves left
    past the vector blocks that have no edge into it, and fuses with the
    sequential block it then meets. Swapping independent neighbours keeps every
    edge's source ahead of its sink. *)
let fuse_sequential (graph : loop_graph)
    (blocks : (label list * Stmt.Located.t option) list) =
  let rec passed_by leaves behind = function
    | ((from, Some _) as vector) :: placed
      when not (edge_between graph ~from ~into:leaves) ->
        passed_by leaves (vector :: behind) placed
    | placed -> (behind, placed) in
  (* [placed] is the output so far, most recent first *)
  let rec place placed = function
    | [] -> List.rev placed
    | ((_, Some _) as vector) :: rest -> place (vector :: placed) rest
    | ((leaves, None) as block) :: rest ->
        let behind, placed = passed_by leaves [] placed in
        let placed =
          match placed with
          | (earlier, None) :: placed -> (earlier @ leaves, None) :: placed
          | placed -> block :: placed in
        place (List.rev_append behind placed) rest in
  place [] blocks

(** Decide one [For]: the report body and the replacement. The whole-loop checks
    come first and leave the loop alone without any dependence analysis: a bound
    with effects would be evaluated again by every vector statement, a [break]
    or [continue] leaves the loop early, and a bound variable written in the
    body changes the range. Then the pi-blocks are decided in emission order and
    the sequential blocks fused; a sequential block is the loop restricted to
    the block's leaves in lexical order, so every dependence among them holds.
    The loop itself comes back when nothing widened. *)
let decide (mir : Program.Typed.t) (loop : Stmt.Located.t) ~loopvar ~lower
    ~upper ~body : string Lazy.t * Stmt.Located.t =
  let bail reason = (lazy (Fmt.str "  loop left alone: %s@." reason), loop) in
  if cannot_duplicate_expr lower || cannot_duplicate_expr upper then
    bail "a loop bound has side effects or draws random numbers"
  else if contains_top_break_or_continue body then
    bail "break or continue in the loop body"
  else
    (* one map per loop: the written set is then exactly the body's writes *)
    let map = build_dep_info_map mir loop in
    let bounds = read_variables_at map (Set.Poly.singleton root_label) in
    let written_vars = Accesses.written_vars (subtree_accesses map root_label) in
    match Set.Poly.to_list (Set.Poly.inter bounds written_vars) with
    | _ :: _ as written_bounds ->
        bail
          (Fmt.str "loop bound variable %s is written in the body"
             (String.concat ~sep:", " written_bounds))
    | [] -> (
        let ctx = {loopvar; lower; upper; written_vars} in
        let graph = build_loop_graph map ~loop:root_label in
        let blocks, outcomes =
          List.split
            (List.map (pi_blocks graph) ~f:(decide_block ctx map graph)) in
        let report =
          lazy (Fmt.str "%a" (pp_graph map (List.concat outcomes)) graph) in
        let emit (leaves, vector) =
          match vector with
          | Some stmt -> stmt
          | None ->
              let body =
                match
                  List.map (List.sort leaves ~cmp:compare) ~f:(statement_at map)
                with
                | [stmt] -> stmt
                | stmts -> {body with pattern= Block stmts} in
              {loop with pattern= For {loopvar; lower; upper; body}} in
        if List.for_all blocks ~f:(fun (_, vector) -> Option.is_none vector)
        then (report, loop)
        else
          match List.map (fuse_sequential graph blocks) ~f:emit with
          | [stmt] -> (report, stmt)
          | stmts -> (report, {loop with pattern= SList stmts}))

(* ---- The walk and the report (design §7.13) ---- *)

let loop_report_log : string Lazy.t list ref = ref []

(** The reports of the last run, in program order: one block per source loop
    with the header, then the reason the loop was left alone or one line per
    leaf with the leaf's outcome, the edges and the pi-blocks. *)
let loop_reports () : string list = List.rev_map !loop_report_log ~f:Lazy.force

(** Every [For], innermost first, replaced by the emitted statements. The
    decision is recorded when [report] is set and the loop has a source
    location; compiler-generated loops (data reads, parameter unpacking) have
    none. *)
let rewrite_stmt (mir : Program.Typed.t) ~(report : bool) :
    Stmt.Located.t -> Stmt.Located.t =
  Stmt.rewrite_bottom_up ~f:Fun.id ~g:(fun stmt ->
      match stmt.pattern with
      | For {loopvar; lower; upper; body} ->
          let body_report, rewritten =
            decide mir stmt ~loopvar ~lower ~upper ~body in
          if report && Stdlib.compare stmt.meta Location_span.empty <> 0 then
            loop_report_log :=
              lazy
                (Fmt.str "loop at %a  (%s in %a:%a)@.%s"
                   (Location_span.pp ?printed_filename:None)
                   stmt.meta loopvar Expr.Typed.pp lower Expr.Typed.pp upper
                   (Lazy.force body_report))
              :: !loop_report_log;
          rewritten
      | _ -> stmt)

(** Every loop of the program; [reverse_mode_log_prob], a copy of [log_prob], is
    rewritten without a report so every model loop reports once. *)
let vectorize_loops (mir : Program.Typed.t) : Program.Typed.t =
  loop_report_log := [];
  let rewritten =
    Program.map Fun.id
      (rewrite_stmt mir ~report:true)
      Fun.id
      {mir with reverse_mode_log_prob= []} in
  { rewritten with
    reverse_mode_log_prob=
      List.map mir.reverse_mode_log_prob ~f:(rewrite_stmt mir ~report:false) }
