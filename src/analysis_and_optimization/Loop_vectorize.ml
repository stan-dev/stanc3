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

(* ---- The report (design §7.13) ---- *)

(** What became of one leaf: a vector statement, or why the leaf stays in a
    loop. *)
type outcome =
  | Hoisted
  | Recurrence of edge
  | In_cycle of label list
  | Effectful
  | Not_widened of string

(** What happened to one loop: left alone by a whole-loop check before any
    analysis, or analysed with the graph of the body and one outcome per leaf.
*)
type decision =
  | Left_alone of string
  | Analyzed of
      {map: dep_info_map; graph: loop_graph; outcomes: (label * outcome) list}

type loop_report = {loc: Location_span.t; header: string; decision: decision}

let loop_report_log : loop_report list ref = ref []

(** The reports of the last run, in program order. *)
let loop_reports () : loop_report list = List.rev !loop_report_log

let pp_outcome (graph : loop_graph) ppf (outcome : outcome) =
  let pp_leaf ppf leaf =
    Fmt.pf ppf "S%d"
      (Option.value ~default:(-1)
         (List.find_index graph.leaves ~f:(fun other -> other = leaf))) in
  match outcome with
  | Hoisted -> Fmt.string ppf "hoisted"
  | Recurrence edge ->
      Fmt.pf ppf "sequential: recurrence, %a" (pp_edge graph) edge
  | In_cycle others ->
      Fmt.pf ppf "sequential: in a dependence cycle with %a"
        Fmt.(list ~sep:(any " ") pp_leaf)
        others
  | Effectful ->
      Fmt.string ppf
        "sequential: has effects (print, reject or a user-defined function \
         call)"
  | Not_widened reason -> Fmt.pf ppf "sequential: %s" reason

(** One loop of the report: the header, then the reason the loop was left alone,
    or one line per leaf with the leaf's outcome, the edges and the pi-blocks.
*)
let pp_loop_report ppf {loc; header; decision} =
  Fmt.pf ppf "loop at %a  (%s)@."
    (Location_span.pp ?printed_filename:None)
    loc header;
  match decision with
  | Left_alone reason -> Fmt.pf ppf "  loop left alone: %s@." reason
  | Analyzed {map; graph; outcomes} ->
      pp_graph map ppf graph ~outcome:(fun leaf ->
          Option.map
            (List.assoc_opt leaf outcomes)
            ~f:(Fmt.str "%a" (pp_outcome graph)))

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
  | None ->
      refused "no Stan Math signature for %s over the widened arguments" name

(** [op] over the widened [args] at a container return type, else the
    elementwise operator. *)
let operator_call (call : Expr.Typed.t) (op : Operator.t)
    (args : Expr.Typed.t list) : Expr.Typed.t =
  let at op =
    as_container call (Operator op) args
      (Partial_evaluator.stan_operator_return_type op args) in
  match at op with
  | Some widened -> widened
  | None -> (
      match Option.bind (elementwise_operator op) ~f:at with
      | Some widened -> widened
      | None ->
          refused "no Stan Math signature for %a over the widened arguments"
            Operator.pp op)

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
  let widened =
    { call with
      pattern= FunApp (StanLib (name, suffix, mem), List.map args ~f:(widen ctx))
    } in
  if List.for_all args ~f:(is_invariant ctx) then
    let open Expr.Helpers in
    { (binop (binop ctx.upper Minus (binop ctx.lower Minus one)) Times widened) with
      meta= call.meta }
  else
    match widened.pattern with
    | FunApp (_, args) -> (
        match Partial_evaluator.stan_math_return_type name args with
        | Some (ReturnType UReal) -> widened
        | Some _ | None ->
            refused "no Stan Math signature for %s over the widened arguments"
              name)
    | _ -> widened

(** [var[indices] = rhs] over the whole range: the varying index becomes a slice
    and the right-hand side must widen to that slice's type. *)
let widen_assignment (ctx : context) (var : string)
    (indices : Expr.Typed.t Index.t list) (var_type : UnsizedType.t)
    (rhs : Expr.Typed.t) =
  let effectful expr = cannot_duplicate_expr expr in
  if
    effectful rhs
    || List.exists indices
         ~f:(Index.fold (fun found expr -> found || effectful expr) false)
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
    that every statement inside widens or none does. *)
let rec widen_stmt_exn (ctx : context) (stmt : Stmt.Located.t) : Stmt.Located.t
    =
  let pattern =
    match stmt.pattern with
    | TargetPE
        ({ pattern=
             FunApp
               (StanLib (name, ((FnLpdf _ | FnLpmf _) as suffix), mem), args)
         ; _ } as call) ->
        Stmt.Pattern.TargetPE (widen_density ctx call name suffix mem args)
    | TargetPE {pattern= FunApp (UserDefined (name, _), _); _} ->
        refused "user-defined density %s has no container signature" name
    | TargetPE _ -> refused "the target increment is not a density call"
    | Assignment ((LVariable var, indices), var_type, rhs) ->
        widen_assignment ctx var indices var_type rhs
    | Assignment ((LTupleProjection _, _), _, _) ->
        refused "assignment to a tuple projection"
    | (Profile _ | Block _ | SList _) as compound ->
        Stmt.Pattern.map Fun.id (widen_stmt_exn ctx) compound
    | Decl {decl_id; _} -> refused "declaration of %s in the loop body" decl_id
    | IfElse _ -> refused "if statement"
    | For _ | While _ -> refused "nested loop"
    | NRFunApp _ -> refused "function call statement"
    | JacobianPE _ | Return _ | Break | Continue | Skip ->
        refused "not an assignment or a density increment" in
  {stmt with pattern}

let widen_stmt (ctx : context) (stmt : Stmt.Located.t) :
    (Stmt.Located.t, string) result =
  match widen_stmt_exn ctx stmt with
  | widened -> Ok widened
  | exception Refused reason -> Error reason

(* ---- Pi-block code generation (Allen and Kennedy 1987 §5.2; design §7.6)
   ---- *)

(** A pi-block once decided: one vector statement standing for a leaf, or the
    leaves that stay in a sequential loop. *)
type pi_block =
  | Vectorized of {leaf: label; stmt: Stmt.Located.t}
  | Sequential of label list

let members = function
  | Vectorized {leaf; _} -> [leaf]
  | Sequential leaves -> leaves

(** Decide one pi-block: a single leaf without a cycle or effects that widens
    becomes a vector statement; anything else stays sequential, with the reason.
*)
let decide_block (ctx : context) (map : dep_info_map) (graph : loop_graph)
    (block : label list) : pi_block * (label * outcome) list =
  match block with
  | [leaf] when is_cyclic map graph block ->
      let self_edge =
        List.find_opt graph.edges ~f:(fun (edge : edge) ->
            edge.src = leaf && edge.dst = leaf) in
      ( Sequential block
      , [ ( leaf
          , Option.value_map self_edge ~default:(In_cycle []) ~f:(fun edge ->
                Recurrence edge) ) ] )
  | [leaf] when leaf_has_effects map leaf ->
      (Sequential block, [(leaf, Effectful)])
  | [leaf] -> (
      match widen_stmt ctx (statement_at map leaf) with
      | Ok stmt -> (Vectorized {leaf; stmt}, [(leaf, Hoisted)])
      | Error reason -> (Sequential block, [(leaf, Not_widened reason)]))
  | cycle ->
      ( Sequential cycle
      , List.map cycle ~f:(fun leaf ->
            (leaf, In_cycle (List.filter cycle ~f:(fun other -> other <> leaf))))
      )

(** Typed fusion (Kennedy and Allen 2001 §6.2.5): a sequential block moves left
    past the vector blocks that have no edge into it, and fuses with the
    sequential block it then meets. Swapping independent neighbours keeps every
    edge's source ahead of its sink. *)
let fuse_sequential (graph : loop_graph) (blocks : pi_block list) :
    pi_block list =
  let rec passed_by block behind = function
    | (Vectorized _ as vector) :: placed
      when not (edge_between graph ~from:(members vector) ~into:(members block))
      ->
        passed_by block (vector :: behind) placed
    | placed -> (behind, placed) in
  (* [placed] is the output so far, most recent first *)
  let rec place placed = function
    | [] -> List.rev placed
    | (Vectorized _ as vector) :: rest -> place (vector :: placed) rest
    | (Sequential leaves as block) :: rest ->
        let behind, placed = passed_by block [] placed in
        let placed =
          match placed with
          | Sequential earlier :: placed ->
              Sequential (earlier @ leaves) :: placed
          | placed -> block :: placed in
        place (List.rev_append behind placed) rest in
  place [] blocks

(** A vector statement, or the loop restricted to the block's leaves in lexical
    order: the original loop minus some statements, so every dependence among
    them holds. *)
let emit (ctx : context) (map : dep_info_map) (loop : Stmt.Located.t)
    ~(body : Stmt.Located.t) (block : pi_block) : Stmt.Located.t =
  match block with
  | Vectorized {stmt; _} -> stmt
  | Sequential leaves ->
      let body =
        match
          List.map (List.sort leaves ~cmp:compare) ~f:(statement_at map)
        with
        | [stmt] -> stmt
        | stmts -> {body with pattern= Block stmts} in
      { loop with
        pattern=
          For {loopvar= ctx.loopvar; lower= ctx.lower; upper= ctx.upper; body}
      }

(** Code generation for a loop that passed the whole-loop checks: the pi-blocks
    decided in emission order, the sequential blocks fused; the loop itself when
    nothing widened. *)
let rewrite_by_pi_blocks (ctx : context) (map : dep_info_map)
    (loop : Stmt.Located.t) ~(body : Stmt.Located.t) : decision * Stmt.Located.t
    =
  let graph = build_loop_graph map ~loop:root_label in
  let blocks, outcomes =
    List.split (List.map (pi_blocks graph) ~f:(decide_block ctx map graph))
  in
  let decision = Analyzed {map; graph; outcomes= List.concat outcomes} in
  let vectorized = function Vectorized _ -> true | Sequential _ -> false in
  if not (List.exists blocks ~f:vectorized) then (decision, loop)
  else
    match
      List.map (fuse_sequential graph blocks) ~f:(emit ctx map loop ~body)
    with
    | [stmt] -> (decision, stmt)
    | stmts -> (decision, {loop with pattern= SList stmts})

(* ---- The walk ---- *)

(** Decide one [For]. The whole-loop checks come first and leave the loop alone
    without any dependence analysis: a bound with effects would be evaluated
    again by every vector statement, a [break] or [continue] leaves the loop
    early, and a bound variable written in the body changes the range. *)
let decide (mir : Program.Typed.t) (loop : Stmt.Located.t) ~loopvar ~lower
    ~upper ~body : decision * Stmt.Located.t =
  if cannot_duplicate_expr lower || cannot_duplicate_expr upper then
    (Left_alone "a loop bound has side effects or draws random numbers", loop)
  else if contains_top_break_or_continue body then
    (Left_alone "break or continue in the loop body", loop)
  else
    (* one map per loop: the written set is then exactly the body's writes *)
    let map = build_dep_info_map mir loop in
    let bounds = read_variables_at map (Set.Poly.singleton root_label) in
    let written_vars = Accesses.written_vars (subtree_accesses map root_label) in
    match Set.Poly.to_list (Set.Poly.inter bounds written_vars) with
    | _ :: _ as written_bounds ->
        ( Left_alone
            (Fmt.str "loop bound variable %s is written in the body"
               (String.concat ~sep:", " written_bounds))
        , loop )
    | [] ->
        rewrite_by_pi_blocks
          {loopvar; lower; upper; written_vars}
          map loop ~body

(** Every [For], innermost first, replaced by the emitted statements. The
    decision is recorded when [report] is set and the loop has a source
    location; compiler-generated loops (data reads, parameter unpacking) have
    none. *)
let rewrite_stmt (mir : Program.Typed.t) ~(report : bool) :
    Stmt.Located.t -> Stmt.Located.t =
  Stmt.rewrite_bottom_up ~f:Fun.id ~g:(fun stmt ->
      match stmt.pattern with
      | For {loopvar; lower; upper; body} ->
          let decision, rewritten =
            decide mir stmt ~loopvar ~lower ~upper ~body in
          if report && Stdlib.compare stmt.meta Location_span.empty <> 0 then
            loop_report_log :=
              { loc= stmt.meta
              ; header=
                  Fmt.str "%s in %a:%a" loopvar Expr.Typed.pp lower
                    Expr.Typed.pp upper
              ; decision }
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
