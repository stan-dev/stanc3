(** Loop vectorization by pi-block code generation (design doc
    [design-docs/active/vectorize-loop-fission.md]).

    {1 What the pass does}

    A [for] loop whose body applies scalar operations to the [n]-th element of
    containers is rewritten into whole-container statements, one per body
    statement, that Stan Math evaluates in a single call:
    {v
    for (n in 1:N) target += normal_lpdf(y[n] | mu[n], sigma);
      ==>  target += normal_lpdf(y[1:N] | mu[1:N], sigma);
    for (n in 1:N) mu[n] = alpha + beta * x[n];
      ==>  mu[1:N] = alpha + beta * x[1:N];
    v}
    A statement that cannot be rewritten stays in a loop, and the pass may still
    hoist the statements around it. For that to be legal the emitted statements
    must respect every data dependence of the original loop, which is what the
    dependence graph of [Loop_dependence] decides.

    {1 How to read the code}

    The file is laid out in the order the pass runs, from the bottom up: the
    entry point and the whole-loop checks are at the end, the per-statement
    machinery they call is above them.

    {2 1. Walk the program, innermost loops first}

    [vectorize_loops] resets the debug report and maps [vectorize_stmt] over
    every statement. [vectorize_stmt] recurses into a loop's body before
    handling the loop itself, so an inner loop is already vectorized when the
    outer one is examined. Each [For] is packed into a [loop] record and handed
    to [vectorized_for].

    {2 2. Whole-loop checks: [vectorized_for]}

    These are the checks that leave a loop alone before any dependence analysis
    is done, each an early return through [leave_alone]:
    - a bound that has side effects or draws random numbers, because a vector
      statement re-evaluates the bounds inside its slice;
    - a [break] or [continue] that leaves this loop level
      ([has_break_or_continue]);
    - a bound variable assigned in the body, which would change the range.

    {2 3. Per-statement decisions: [rewrite_by_pi_blocks]}

    The body's leaf statements become the nodes of
    [Loop_dependence.loop_dependence_graph], whose edges are the true, anti,
    output and effect dependences between them, and [Loop_dependence.pi_blocks]
    returns the strongly connected components of that graph in a topological
    order. Each component is passed to [decide], which is where a statement
    earns its [hoist_outcome]:
    - several statements in one component form a dependence cycle and stay
      sequential ([In_cycle]);
    - a single statement with an edge to itself is a recurrence and stays
      sequential ([Recurrence]);
    - a single statement with effects, such as [print], stays sequential
      ([Effectful]);
    - otherwise [Widen.stmt] tries to rewrite it; [Ok] gives a [Vectorized]
      block ([Hoisted]) and [Error reason] a [Sequential] one ([Not_widened]).

    {2 4. Rewriting one statement: module [Widen]}

    [Widen.stmt] turns one leaf statement into its whole-container form or
    explains why it cannot. Every helper returns plain syntax and reports
    failure by raising [Widen.Refused]; [Widen.stmt] is the single place that
    catches it. The helpers are named after the syntax they widen:
    - [expr] settles the cases that need no rewriting: an expression that does
      not mention the loop variable ([is_invariant]) is returned as it is, a
      container or complex expression that varies is refused. A varying scalar
      goes to [varying_scalar], which rewrites [v[..n..]] through [index] and a
      Stan Math call through [stanlib_call] after widening its arguments with
      [expr] again;
    - [index] rewrites one index position using
      [Loop_dependence.classify_subscript]: an affine unit-stride index such as
      [n + k] becomes the loop range shifted by [k] ([shift_bound]), a gather
      such as [idx[n]] becomes a multi-index, an invariant index stays, and
      strides, slices and indices mentioning a variable written in the loop are
      refused. [check_at_most_one_varying] allows only one such position per
      reference;
    - [stanlib_call] re-typechecks the call against the Stan Math signatures
      ([container_call]), trying the elementwise operator for [*], [/] and [^]
      ([candidate_names]);
    - [density] handles [target += f(args)]: when every argument is invariant
      the increment is multiplied by [iteration_count], otherwise the density is
      applied once to the widened arguments;
    - [assignment] handles [v[idcs] = rhs] and checks that the widened
      right-hand side has exactly the type of the assigned slice;
    - [stmt_exn] dispatches on the statement kind, recursing into [Profile],
      [Block] and [SList], and refuses every other kind with its reason.

    {2 5. Emitting the result}

    Back in [rewrite_by_pi_blocks], if no block was vectorized the original loop
    is returned. Otherwise [move_sequential_left] applies typed fusion (Kennedy
    and Allen §6.2.5): a sequential block moves ahead of the vectorized blocks
    it does not depend on ([independent]), so that sequential blocks become
    neighbours, and [fuse_adjacent] merges neighbouring sequential blocks into
    one loop. [emit] then produces each block: a vectorized block is its
    statement, a sequential block is the original loop restricted to its
    statements in lexical order, which preserves every dependence between them.
    Several results are wrapped in an [SList].

    {1 The debug report}

    [--debug-loop-vectorization] prints one [loop_report] per source loop,
    recorded by [record_report]: either the whole-loop reason it was
    [Left_alone], or the graph, its pi-blocks and the [hoist_outcome] of every
    statement. [Loop_vectorize.loop_reports] returns them after a run. *)

open Std
open Middle
open Mir_utils
open Dataflow_types

(** The [For] being rewritten, with the location of the loop statement. *)
type loop =
  { meta: Stmt.Located.Meta.t
  ; loopvar: string
  ; lower: Expr.Typed.t
  ; upper: Expr.Typed.t
  ; body: Stmt.Located.t }

let for_of_loop ({meta; loopvar; lower; upper; body} : loop) =
  Stmt.{pattern= For {loopvar; lower; upper; body}; meta}

(** [upper - (lower - 1)]: how many times the body runs. *)
let iteration_count ({lower; upper; _} : loop) =
  Expr.Helpers.(binop upper Minus (binop lower Minus one))

(* ---- The --debug-loop-vectorization report (design §7.13) ---- *)

(** Why one leaf statement was or was not hoisted. *)
type hoist_outcome =
  | Hoisted
  | Recurrence of loop_edge
  | In_cycle of int list
  | Effectful
  | Not_widened of string

(** What happened to one loop: left alone by a whole-loop check before any
    analysis, or analyzed statement by statement. *)
type decision =
  | Left_alone of string
  | Analyzed of
      { graph: loop_dependence_graph
      ; blocks: int list list
      ; outcomes: (int * hoist_outcome) list }

type loop_report =
  { loc: Location_span.t
  ; loopvar: string
  ; lower: Expr.Typed.t
  ; upper: Expr.Typed.t
  ; decision: decision }

let loop_report_log : loop_report list ref = ref []
let loop_reports () = List.rev !loop_report_log

(** Record the decision for one loop. Compiler-generated loops (data reads,
    parameter unpacking) carry no source location and are left out. *)
let record_report ({meta; loopvar; lower; upper; _} : loop) decision =
  if Stdlib.compare meta Location_span.empty <> 0 then
    loop_report_log :=
      {loc= meta; loopvar; lower; upper; decision} :: !loop_report_log

let pp_positions ppf ps =
  Fmt.(list ~sep:(any " ") (fun ppf v -> Fmt.pf ppf "S%d" v)) ppf ps

let pp_hoist_outcome ppf = function
  | Hoisted -> Fmt.string ppf "hoisted"
  | Recurrence e ->
      Fmt.pf ppf "sequential: recurrence, %a" Loop_dependence.pp_loop_edge e
  | In_cycle others ->
      Fmt.pf ppf "sequential: in a dependence cycle with %a" pp_positions others
  | Effectful ->
      Fmt.string ppf
        "sequential: has effects (print, reject or a user-defined function \
         call)"
  | Not_widened reason -> Fmt.pf ppf "sequential: %s" reason

(** One loop of the report: the header, then either the reason the loop was left
    alone or one line per leaf statement with its outcome, the edges and the
    pi-blocks in emission order. *)
let pp_loop_report ppf {loc; loopvar; lower; upper; decision} =
  Fmt.pf ppf "loop at %a  (%s in %a:%a)@."
    (Location_span.pp ?printed_filename:None)
    loc loopvar Expr.Typed.pp lower Expr.Typed.pp upper;
  match decision with
  | Left_alone reason -> Fmt.pf ppf "  loop left alone: %s@." reason
  | Analyzed {graph; blocks; outcomes} ->
      Array.iter graph.nodes ~f:(fun (node : loop_node) ->
          let outcome = List.assoc_opt node.pos outcomes in
          Fmt.pf ppf "  S%d  %a   %a@." node.pos
            Loop_dependence.pp_stmt_one_line node.stmt
            (Fmt.option pp_hoist_outcome)
            outcome);
      Fmt.pf ppf "  %a@.  %a@." Loop_dependence.pp_edges graph
        (Loop_dependence.pp_blocks graph)
        blocks

(* ---- Widening: one iteration's statement as a whole-container statement
   ---- *)

module Widen = struct
  (** What widening needs to know: the loop, and the variables its body assigns
      or declares (an index mentioning one of them is not a slice). *)
  type context = {loop: loop; written_vars: string Set.Poly.t}

  (** Why a statement cannot be widened. Raised by any helper and caught once in
      [stmt], so every helper returns plain syntax instead of threading a
      partial result through each level. *)
  exception Refused of string

  let refused fmt = Fmt.kstr (fun reason -> raise (Refused reason)) fmt

  (** The same value in every iteration: the loop variable does not appear. An
      expression widens exactly when it is not invariant and nothing refuses. *)
  let is_invariant {loop; _} e =
    not (Set.Poly.mem loop.loopvar (expr_var_names_set e))

  let classify_index {loop; written_vars} idx =
    Loop_dependence.classify_subscript ~loopvar:loop.loopvar ~written_vars idx

  (** A single index whose value changes between iterations. Slices are not
      counted: [index] refuses them whatever they mention. *)
  let index_varies ctx = function
    | Index.Single _ as idx -> (
        match classify_index ctx idx with
        | Invariant _ -> false
        | Affine _ | Varying _ -> true)
    | All | Upfrom _ | Between _ | MultiIndex _ -> false

  (** At most one index position may vary with the loop. *)
  let check_at_most_one_varying ctx idcs =
    match List.filter idcs ~f:(index_varies ctx) with
    | _ :: second :: _ ->
        refused "more than one index varies with the loop (%a)"
          (Index.pp Expr.Typed.pp) second
    | [] | [_] -> ()

  (** The loop variable appears where a value is needed rather than as an index,
      e.g. [c[n] * n]. *)
  let rec uses_loopvar_as_value ctx (e : Expr.Typed.t) =
    match e.pattern with
    | Var v -> String.equal v ctx.loop.loopvar
    | Lit _ -> false
    | Indexed (base, _) -> uses_loopvar_as_value ctx base
    | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ | TupleProjection _
      ->
        Expr.Pattern.fold
          (fun acc e -> acc || uses_loopvar_as_value ctx e)
          false e.pattern

  (** [bound + offset] for a loop bound and a symbolic [linear] offset, built
      with [Expr.Helpers.binop] and constant-folded later by partial evaluation;
      [bound] itself when the offset is zero. This is how [a[n + k]] over
      [n in 1:N] becomes [a[(1 + k):(N + k)]]. *)
  let shift_bound (bound : Expr.Typed.t) ({const; terms} : linear) =
    let open Expr.Helpers in
    let add acc (c, (e : Expr.Typed.t)) =
      let scaled = match abs c with 1 -> e | k -> binop (int k) Times e in
      if c > 0 then binop acc Plus scaled else binop acc Minus scaled in
    let shifted = List.fold_left terms ~init:bound ~f:add in
    let shifted =
      if const = 0 then shifted
      else if const > 0 then binop shifted Plus (int const)
      else binop shifted Minus (int (-const)) in
    {shifted with meta= bound.meta}

  (** [*], [/] and [^] have elementwise variants that accept containers. *)
  let elementwise_operator = function
    | Operator.Times -> Some Operator.EltTimes
    | Divide -> Some EltDivide
    | Pow -> Some EltPow
    | Plus | PPlus | Minus | PMinus | IntDivide | Modulo | LDivide | EltTimes
     |EltDivide | EltPow | Or | And | Equals | NEquals | Less | Leq | Greater
     |Geq | PNot | Transpose ->
        None

  (** The function names to try for a call to [name]: itself and, for [*], [/]
      and [^], the elementwise operator. *)
  let candidate_names name =
    let elementwise =
      Option.bind (Operator.of_string_opt name) ~f:elementwise_operator
      |> Option.map ~f:Operator.to_string in
    name :: Option.to_list elementwise

  let pp_arg_types ppf (args : Expr.Typed.t list) =
    Fmt.(list ~sep:(any ", ") UnsizedType.pp)
      ppf
      (List.map args ~f:(fun (a : Expr.Typed.t) -> a.meta.type_))

  (** [name(args)] retyped at its Stan Math container return type, when the
      signatures have one; [original] supplies the location and autodiff level.
  *)
  let container_call (original : Expr.Typed.t) mem_pattern args name :
      Expr.Typed.t option =
    match Partial_evaluator.stan_math_return_type name args with
    | Some
        (ReturnType ((UVector | URowVector | UArray (UInt | UReal)) as type_))
      ->
        Some
          { pattern= FunApp (StanLib (name, FnPlain, mem_pattern), args)
          ; meta= {original.meta with type_} }
    | Some _ | None -> None

  (** A plain Stan Math call on already widened [args]: the first candidate name
      with a container signature. *)
  let stanlib_call original name mem_pattern args =
    match
      List.find_map (candidate_names name)
        ~f:(container_call original mem_pattern args)
    with
    | Some call -> call
    | None -> refused "no Stan Math signature for %s(%a)" name pp_arg_types args

  (** A scalar that varies with the loop becomes a container; an invariant
      scalar is returned as it is; containers never widen. *)
  let rec expr ctx (e : Expr.Typed.t) : Expr.Typed.t =
    match e.meta.type_ with
    | (UInt | UReal | UComplex) when is_invariant ctx e -> e
    | UInt | UReal -> varying_scalar ctx e
    | UComplex ->
        refused "complex expression %a varies with the loop" Expr.Typed.pp e
    | UVector | URowVector | UMatrix | UComplexVector | UComplexRowVector
     |UComplexMatrix | UArray _ | UTuple _ | UFun _ | UMathLibraryFunction ->
        if is_invariant ctx e then
          refused "%a is a loop-invariant container" Expr.Typed.pp e
        else
          refused "%a is a container that varies with the loop" Expr.Typed.pp e

  and varying_scalar ctx (e : Expr.Typed.t) : Expr.Typed.t =
    match e.pattern with
    | Var v when String.equal v ctx.loop.loopvar ->
        refused "loop variable %s is used as a value" v
    | Var _ | Lit _ -> e (* invariant, so handled by [expr] *)
    | Indexed (base, idcs) when is_invariant ctx base ->
        check_at_most_one_varying ctx idcs;
        let idcs = List.map idcs ~f:(index ctx) in
        let type_ = Expr.Helpers.infer_type_of_indexed base.meta.type_ idcs in
        {pattern= Indexed (base, idcs); meta= {e.meta with type_}}
    | Indexed (base, _) ->
        refused "indexed base %a varies with the loop" Expr.Typed.pp base
    | FunApp (StanLib (name, FnPlain, mem_pattern), args) ->
        stanlib_call e name mem_pattern (List.map args ~f:(expr ctx))
    | FunApp
        ( ( StanLib
              (name, (FnRng | FnLpdf _ | FnLpmf _ | FnTarget | FnJacobian), _)
          | UserDefined (name, _) )
        , _ ) ->
        refused "%s is not a plain Stan Math function" name
    | FunApp (CompilerInternal _, _) ->
        refused "%a is a compiler-internal call" Expr.Typed.pp e
    | (TernaryIf _ | EAnd _ | EOr _ | Promotion _ | TupleProjection _)
      when uses_loopvar_as_value ctx e ->
        refused "loop variable %s is used as a value" ctx.loop.loopvar
    | TernaryIf _ | EAnd _ | EOr _ | Promotion _ | TupleProjection _ ->
        refused "%a cannot be widened" Expr.Typed.pp e

  (** One index position: an affine unit-stride index becomes the loop range
      shifted by its offset, a gather becomes a multi-index, an invariant index
      stays. *)
  and index ctx (idx : Expr.Typed.t Index.t) : Expr.Typed.t Index.t =
    match idx with
    | (All | Upfrom _ | Between _ | MultiIndex _) as i ->
        refused "index %a is already a slice" (Index.pp Expr.Typed.pp) i
    | Single e -> (
        match classify_index ctx idx with
        | Invariant _ -> idx
        | Affine {coeff= 1; offset} ->
            let ({lower; upper; _} : loop) = ctx.loop in
            Between (shift_bound lower offset, shift_bound upper offset)
        | Affine {coeff; _} ->
            refused "index %a has stride %d" Expr.Typed.pp e coeff
        | Varying Written ->
            refused "index %a mentions a variable written in the loop"
              Expr.Typed.pp e
        | Varying (Gather | Nonlinear | Slice | Multi_index) ->
            MultiIndex (expr ctx e))

  (** [target += f(args)] for a density [f]: when an argument varies, the
      density is summed once over the widened containers; when every argument is
      invariant, the same increment is added [iteration_count] times. Widening
      an invariant argument only checks that it is a scalar. *)
  let density ctx (e : Expr.Typed.t) name suffix mem_pattern args : Expr.Typed.t
      =
    if cannot_duplicate_expr e then
      refused "the density call has side effects or draws random numbers";
    let all_invariant = List.for_all args ~f:(is_invariant ctx) in
    let args = List.map args ~f:(expr ctx) in
    let call =
      {e with pattern= FunApp (StanLib (name, suffix, mem_pattern), args)} in
    if all_invariant then
      Expr.Helpers.binop (iteration_count ctx.loop) Times call
    else
      match Partial_evaluator.stan_math_return_type name args with
      | Some (ReturnType UReal) -> call
      | Some _ | None ->
          refused "density %s has no signature for (%a)" name pp_arg_types args

  (** [var[idcs] = rhs]: the varying index becomes a slice and [rhs] must widen
      to exactly that slice's type. *)
  let assignment ctx var idcs var_type rhs =
    let has_effects =
      cannot_duplicate_expr rhs
      || List.exists
           (List.concat_map idcs ~f:Index.bounds)
           ~f:cannot_duplicate_expr in
    if has_effects then
      refused "the assignment has side effects or draws random numbers";
    check_at_most_one_varying ctx idcs;
    let widened_idcs = List.map idcs ~f:(index ctx) in
    if not (List.exists idcs ~f:(index_varies ctx)) then
      refused
        "no index of %s varies with the loop (the same element is assigned \
         every iteration)"
        var;
    if is_invariant ctx rhs then
      refused "right-hand side is loop-invariant (would need a broadcast)";
    let rhs = expr ctx rhs in
    let slice_type = Expr.Helpers.infer_type_of_indexed var_type widened_idcs in
    if not (UnsizedType.equal rhs.meta.type_ slice_type) then
      refused "right-hand side widens to %a but the assigned slice is %a"
        UnsizedType.pp rhs.meta.type_ UnsizedType.pp slice_type;
    Stmt.Pattern.Assignment ((LVariable var, widened_idcs), var_type, rhs)

  (** One leaf statement as a vector statement; raises [Refused]. *)
  let rec stmt_exn ctx (s : Stmt.Located.t) : Stmt.Located.t =
    let pattern =
      match s.pattern with
      | TargetPE
          ({ pattern=
               FunApp
                 ( StanLib (name, ((FnLpdf _ | FnLpmf _) as suffix), mem_pattern)
                 , args )
           ; _ } as e) ->
          Stmt.Pattern.TargetPE (density ctx e name suffix mem_pattern args)
      | TargetPE {pattern= FunApp (UserDefined (name, _), _); _} ->
          refused "user-defined density %s has no container signature" name
      | TargetPE _ -> refused "target increment is not a density call"
      | Assignment ((LVariable var, idcs), var_type, rhs) ->
          assignment ctx var idcs var_type rhs
      | Assignment ((LTupleProjection _, _), _, _) ->
          refused "assignment to a tuple projection"
      | (Profile _ | Block _ | SList _) as compound ->
          Stmt.Pattern.map Fun.id (stmt_exn ctx) compound
      | NRFunApp (CompilerInternal (FnPrint | FnReject | FnFatalError), _) ->
          refused "print, reject or fatal_error statement"
      | NRFunApp _ -> refused "function call statement"
      | Decl {decl_id; _} -> refused "declaration of %s inside the loop" decl_id
      | IfElse _ -> refused "if statement (no if-conversion)"
      | While _ -> refused "while loop"
      | For _ -> refused "nested loop"
      | JacobianPE _ -> refused "jacobian increment"
      | Return _ -> refused "return statement"
      | Break | Continue -> refused "break or continue"
      | Skip -> refused "empty statement" in
    Stmt.{pattern; meta= s.meta}

  (** One leaf statement as a vector statement, or the reason it is not. *)
  let stmt ctx (s : Stmt.Located.t) : (Stmt.Located.t, string) result =
    match stmt_exn ctx s with
    | widened -> Ok widened
    | exception Refused reason -> Error reason
end

(* ---- Pi-block code generation (Allen and Kennedy 1987 §5.2; design §7.6)
   ---- *)

(** A pi-block (one strongly connected component of the dependence graph) once
    decided: the vector statement standing for position [pos], or the positions
    that stay in a sequential loop. *)
type pi_block =
  | Vectorized of {pos: int; stmt: Stmt.Located.t}
  | Sequential of int list

let members = function Vectorized {pos; _} -> [pos] | Sequential ps -> ps
let is_vectorized = function Vectorized _ -> true | Sequential _ -> false

(** Decide one strongly connected component [scc] of statement positions. A
    single statement is vectorized when it has no self-dependence, no effects,
    and widens; several statements form a dependence cycle and stay sequential.
*)
let decide ctx (graph : loop_dependence_graph) scc :
    pi_block * (int * hoist_outcome) list =
  match scc with
  | [pos] when Loop_dependence.is_cyclic graph scc ->
      let self_edge =
        List.find_opt graph.edges ~f:(fun (e : loop_edge) ->
            e.src = pos && e.dst = pos) in
      let outcome =
        Option.value_map self_edge ~default:(In_cycle []) ~f:(fun e ->
            Recurrence e) in
      (Sequential [pos], [(pos, outcome)])
  | [pos] when graph.nodes.(pos).effects ->
      (Sequential [pos], [(pos, Effectful)])
  | [pos] -> (
      match Widen.stmt ctx graph.nodes.(pos).stmt with
      | Ok stmt -> (Vectorized {pos; stmt}, [(pos, Hoisted)])
      | Error reason -> (Sequential [pos], [(pos, Not_widened reason)]))
  | cycle ->
      let others pos = List.filter cycle ~f:(fun q -> q <> pos) in
      ( Sequential cycle
      , List.map cycle ~f:(fun pos -> (pos, In_cycle (others pos))) )

(** No dependence edge runs from a statement of [src] into one of [dst]. *)
let independent (graph : loop_dependence_graph) src dst =
  not
    (List.exists graph.edges ~f:(fun (e : loop_edge) ->
         List.mem e.src ~set:(members src) && List.mem e.dst ~set:(members dst)))

(** Typed fusion (Kennedy and Allen §6.2.5): each sequential block moves left
    past the vectorized blocks it does not depend on, so that sequential blocks
    become adjacent. Swapping independent neighbours keeps every edge's source
    ahead of its sink. *)
let move_sequential_left graph pi_blocks =
  (* the vectorized blocks at the head of [placed] that [seq] may move past *)
  let rec vectorized_passed_by seq passed = function
    | (Vectorized _ as vec) :: placed when independent graph vec seq ->
        vectorized_passed_by seq (vec :: passed) placed
    | placed -> (passed, placed) in
  (* [placed] is the output so far, most recent first *)
  let rec place placed = function
    | [] -> List.rev placed
    | (Vectorized _ as vec) :: rest -> place (vec :: placed) rest
    | (Sequential _ as seq) :: rest ->
        let passed, placed = vectorized_passed_by seq [] placed in
        place (List.rev_append passed (seq :: placed)) rest in
  place [] pi_blocks

(** Adjacent sequential blocks become one loop. *)
let rec fuse_adjacent = function
  | Sequential a :: Sequential b :: rest ->
      fuse_adjacent (Sequential (a @ b) :: rest)
  | x :: rest -> x :: fuse_adjacent rest
  | [] -> []

(** A sequential block is the original loop restricted to its statements in
    lexical order. That is always legal: it is the original loop minus some
    statements, and every edge between the kept statements is preserved. *)
let emit (loop : loop) (graph : loop_dependence_graph) = function
  | Vectorized {stmt; _} -> stmt
  | Sequential positions ->
      let stmts =
        List.sort positions ~cmp:Int.compare
        |> List.map ~f:(fun pos -> graph.nodes.(pos).stmt) in
      let body =
        match stmts with
        | [s] -> s
        | stmts -> Stmt.{pattern= Block stmts; meta= loop.body.meta} in
      for_of_loop {loop with body}

(** Pi-block code generation for a loop that passed the whole-loop checks of
    [vectorized_for]. The original loop is returned when nothing is vectorized.
*)
let rewrite_by_pi_blocks (loop : loop) ~written_vars : Stmt.Located.t =
  let graph =
    Loop_dependence.loop_dependence_graph ~loopvar:loop.loopvar loop.body in
  let sccs = Loop_dependence.pi_blocks graph in
  let pi_blocks, outcomes =
    List.map sccs ~f:(decide Widen.{loop; written_vars} graph) |> List.split
  in
  record_report loop
    (Analyzed {graph; blocks= sccs; outcomes= List.concat outcomes});
  if not (List.exists pi_blocks ~f:is_vectorized) then for_of_loop loop
  else
    match
      List.map
        (fuse_adjacent (move_sequential_left graph pi_blocks))
        ~f:(emit loop graph)
    with
    | [s] -> s
    | stmts -> Stmt.{pattern= SList stmts; meta= loop.meta}

(** [break] or [continue] that leaves this loop level (not one nested in an
    inner loop). *)
let rec has_break_or_continue (s : Stmt.Located.t) =
  match s.pattern with
  | Break | Continue -> true
  | For _ | While _ -> false
  | Assignment _ | TargetPE _ | JacobianPE _ | NRFunApp _ | Return _ | Skip
   |IfElse _ | Profile _ | Block _ | SList _ | Decl _ ->
      Stmt.Pattern.fold
        (fun acc _ -> acc)
        (fun acc s -> acc || has_break_or_continue s)
        false s.pattern

(** Rewrite one [For]. The whole-loop checks come first and each leaves the loop
    alone without any dependence analysis: a bound with effects would be
    evaluated again by every vector statement, a [break] or [continue] leaves
    the loop early, and a bound variable written in the body changes the range.
    Every decision is recorded for the [--debug-loop-vectorization] report. *)
let vectorized_for ({lower; upper; body; _} as loop : loop) : Stmt.Located.t =
  let leave_alone reason =
    record_report loop (Left_alone reason);
    for_of_loop loop in
  if cannot_duplicate_expr lower || cannot_duplicate_expr upper then
    leave_alone "a loop bound has side effects or draws random numbers"
  else if has_break_or_continue body then
    leave_alone "break or continue in the loop body"
  else
    let written_vars = Stmt.Helpers.assigned_or_declared_variables body in
    let bound_vars =
      Set.Poly.union (expr_var_names_set lower) (expr_var_names_set upper) in
    let written_bound_vars = Set.Poly.inter written_vars bound_vars in
    if not (Set.Poly.is_empty written_bound_vars) then
      leave_alone
        (Fmt.str "loop bound variable %s is written in the body"
           (String.concat ~sep:", " (Set.Poly.to_list written_bound_vars)))
    else rewrite_by_pi_blocks loop ~written_vars

(** Vectorize every loop, innermost first. *)
let rec vectorize_stmt : Stmt.Located.t -> Stmt.Located.t = function
  | {pattern= For {loopvar; lower; upper; body}; meta} ->
      vectorized_for {meta; loopvar; lower; upper; body= vectorize_stmt body}
  | {pattern; meta} ->
      {pattern= Stmt.Pattern.map Fun.id vectorize_stmt pattern; meta}

let vectorize_loops mir =
  loop_report_log := [];
  Program.map Fun.id vectorize_stmt Fun.id mir
