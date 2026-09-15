(** Data dependence analysis for one loop level: the access model (L1), the
    dependence test (L2) and the loop dependence graph with its pi-blocks (L3)
    of design-docs/active/vectorize-loop-fission.md. [Optimize.vectorize_loops]
    is the client for code generation; [Dependence_analysis] uses the access
    model to refine reaching-definition edges. *)

open Std
open Middle
open Dataflow_types
open Mir_utils

(***********************************)
(* Loop access model (L1) and     *)
(* dependence test (L2)            *)
(***********************************)

(** [Some k] when [e] is the integer literal [k] (including negative literals).
*)
let int_literal (e : Expr.Typed.t) =
  match e.pattern with
  | Lit (Int, s) -> Int.of_string_opt s
  | Lit ((Real | Imaginary | Str), _)
   |Var _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _ | Promotion _
   |TupleProjection _ ->
      None

(** {2 Linear forms} *)

let linear_const const : linear = {const; terms= []}
let linear_zero = linear_const 0

(** Merge equal terms, drop zero coefficients, sort. *)
let linear_normalize ({const; terms} : linear) : linear =
  let terms =
    List.sort terms ~cmp:(fun (_, e1) (_, e2) -> Expr.Typed.compare e1 e2)
    |> List.fold_left ~init:[] ~f:(fun acc (c, e) ->
        match acc with
        | (c', e') :: rest when Expr.Typed.compare e e' = 0 ->
            (c + c', e') :: rest
        | _ -> (c, e) :: acc)
    |> List.filter ~f:(fun (c, _) -> c <> 0)
    |> List.rev in
  {const; terms}

let linear_scale k ({const; terms} : linear) : linear =
  linear_normalize
    {const= k * const; terms= List.map terms ~f:(fun (c, e) -> (k * c, e))}

let linear_add (a : linear) (b : linear) : linear =
  linear_normalize {const= a.const + b.const; terms= a.terms @ b.terms}

let linear_sub a b = linear_add a (linear_scale (-1) b)
let linear_is_const ({terms; _} : linear) = List.is_empty terms

(** [Some (op, args)] when [e] applies a built-in operator. The MIR stores
    operators as [StanLib] calls named by [Operator.to_string]; this view is the
    one place the dependence analysis decodes that name, so the rest of the
    analysis matches on [Operator.t] constructors. *)
let operator_app (e : Expr.Typed.t) : (Operator.t * Expr.Typed.t list) option =
  match e.pattern with
  | FunApp (StanLib (name, FnPlain, _), args) ->
      Option.map (Operator.of_string_opt name) ~f:(fun op -> (op, args))
  | FunApp
      ( ( StanLib (_, (FnRng | FnLpdf _ | FnLpmf _ | FnTarget | FnJacobian), _)
        | CompilerInternal _ | UserDefined _ )
      , _ )
   |Var _ | Lit _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _ | Promotion _
   |TupleProjection _ ->
      None

(** The linear form of an index expression in the loop variable:
    [Some (coeff, offset)] with [e = coeff * loopvar + offset] and [offset]
    loop-invariant, or [None] when [e] is not linear in [loopvar]. Only [+],
    [-], unary [+]/[-] and multiplication by an integer literal are interpreted;
    any other loop-invariant sub-expression becomes a symbolic term. This is a
    small evaluator in the style of LLVM's SCEV builder restricted to one
    induction variable. *)
let rec linear_form ~loopvar ~invariant (e : Expr.Typed.t) :
    (int * linear) option =
  let open Option.Syntax in
  let recur = linear_form ~loopvar ~invariant in
  (* [e] as one opaque symbolic term, if it is loop-invariant at all *)
  let symbolic () =
    if invariant e then Some (0, {const= 0; terms= [(1, e)]}) else None in
  match e.pattern with
  | Var v when String.equal v loopvar -> Some (1, linear_zero)
  | Lit (Int, s) -> (
      match Int.of_string_opt s with
      | Some k -> Some (0, linear_const k)
      | None -> symbolic ())
  | Promotion (e, _, _) -> recur e
  | FunApp _ -> (
      match operator_app e with
      | Some (Plus, [a; b]) ->
          let* ca, oa = recur a in
          let+ cb, ob = recur b in
          (ca + cb, linear_add oa ob)
      | Some (Minus, [a; b]) ->
          let* ca, oa = recur a in
          let+ cb, ob = recur b in
          (ca - cb, linear_sub oa ob)
      | Some (PPlus, [a]) -> recur a
      | Some (PMinus, [a]) ->
          let+ ca, oa = recur a in
          (-ca, linear_scale (-1) oa)
      | Some (Times, [a; b]) -> (
          match (int_literal a, int_literal b) with
          | Some k, _ ->
              let+ c, o = recur b in
              (k * c, linear_scale k o)
          | None, Some k ->
              let+ c, o = recur a in
              (k * c, linear_scale k o)
          | None, None -> symbolic ())
      | Some ((Plus | Minus | PPlus | PMinus | Times), _)
       |Some
          ( ( Divide | IntDivide | Modulo | LDivide | EltTimes | EltDivide | Pow
            | EltPow | Or | And | Equals | NEquals | Less | Leq | Greater | Geq
            | PNot | Transpose )
          , _ )
       |None ->
          symbolic ())
  | Var _
   |Lit ((Real | Imaginary | Str), _)
   |TernaryIf _ | EAnd _ | EOr _ | Indexed _ | TupleProjection _ ->
      symbolic ()

(** Classify one index position of a reference with respect to the loop over
    [loopvar]. [written] is the set of names assigned or declared anywhere in
    the loop body, including inner loop variables
    ([Stmt.Helpers.assigned_or_declared_variables body]).

    A [Single] index is put in linear form [coeff * loopvar + offset] by
    [linear_form]; every loop-invariant sub-expression the evaluator does not
    interpret (a data variable [k], a call [f(k)], ...) becomes a symbolic term
    of [offset]. [coeff <> 0] gives [Affine], [coeff = 0] gives [Invariant].
    Otherwise the result is [Varying] with the reason the debug report prints:
    [Written] if the index mentions a name in [written] (an inner loop variable,
    a body scalar), [Gather] if the loop variable sits under another index
    ([idx[n]]), [Nonlinear] otherwise ([n * k], [n * n]); [All], [Upfrom] and
    [Between] are [Slice] and [MultiIndex] is [Multi_index]. *)
let classify_subscript ~loopvar ~written (idx : Expr.Typed.t Index.t) :
    subscript =
  let invariant (e : Expr.Typed.t) =
    let names = expr_var_names_set e in
    (not (Set.Poly.mem loopvar names)) && Set.Poly.disjoint names written in
  let mentions_written (e : Expr.Typed.t) =
    not (Set.Poly.disjoint (expr_var_names_set e) written) in
  let rec is_gather (e : Expr.Typed.t) =
    match e.pattern with
    | Indexed (_, idcs) ->
        List.exists idcs ~f:(fun idx ->
            List.exists (Index.bounds idx) ~f:(fun b ->
                Set.Poly.mem loopvar (expr_var_names_set b)))
    | Var _ | Lit _ -> false
    | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ | TupleProjection _
      ->
        Expr.Pattern.fold (fun acc e -> acc || is_gather e) false e.pattern
  in
  match idx with
  | Single e -> (
      match linear_form ~loopvar ~invariant e with
      | Some (0, offset) -> Invariant offset
      | Some (coeff, offset) -> Affine {coeff; offset}
      | None ->
          if mentions_written e then Varying Written
          else if is_gather e then Varying Gather
          else Varying Nonlinear)
  | MultiIndex _ -> Varying Multi_index
  | All | Upfrom _ | Between _ -> Varying Slice

(** Every reference in a statement and its substatements, in evaluation order,
    as [access]es of the loop over [loopvar]. An [Assignment] to [v] yields a
    write to [v] (an [LTupleProjection] base is a write with
    [subs = [Varying Nonlinear]]) after the reads of its indices and right-hand
    side; a [Decl] yields a write with [subs = []]; an inner [For] yields a
    write to its own loop variable with [subs = []]. [TargetPE] and [JacobianPE]
    are writes to ["target"] and a [target()] call is a read of it;
    [loop_dependence_graph] ignores the write/write pairs unless some statement
    reads [target()], because the increments form a reduction whose order is
    unobservable until then (design §7.4). Reads of [loopvar] itself are not
    recorded: it is defined by the loop header, so it can never carry a
    dependence. *)
let accesses_of_pattern ~loopvar ~written ~label ~(sub : 's -> access list)
    (stmt : (Expr.Typed.t, 's) Stmt.Pattern.t) : access list =
  let mk var subs is_write = {var; subs; is_write; label} in
  let classify = classify_subscript ~loopvar ~written in
  let rec expr_reads (e : Expr.Typed.t) : access list =
    match e.pattern with
    | Var v when String.equal v loopvar -> []
    | Var v -> [mk v [] false]
    | Lit _ -> []
    | Indexed ({pattern= Var v; _}, idcs) ->
        mk v (List.map idcs ~f:classify) false :: index_reads idcs
    | Indexed (base, idcs) -> expr_reads base @ index_reads idcs
    | FunApp ((StanLib (_, FnTarget, _) | UserDefined (_, FnTarget)), args) ->
        (* [target()] observes the running sum *)
        mk "target" [] false :: List.concat_map args ~f:expr_reads
    | FunApp (kind, args) ->
        List.concat_map (Fun_kind.collect_exprs kind @ args) ~f:expr_reads
    | TernaryIf (a, b, c) -> List.concat_map [a; b; c] ~f:expr_reads
    | EAnd (a, b) | EOr (a, b) -> expr_reads a @ expr_reads b
    | Promotion (e, _, _) | TupleProjection (e, _) -> expr_reads e
  and index_reads idcs =
    List.concat_map idcs ~f:(fun idx ->
        List.concat_map (Index.bounds idx) ~f:expr_reads) in
  let stmt_reads_writes (s : (Expr.Typed.t, 's) Stmt.Pattern.t) : access list =
    match s with
    | Assignment ((LVariable v, idcs), _, rhs) ->
        index_reads idcs @ expr_reads rhs
        @ [mk v (List.map idcs ~f:classify) true]
    | Assignment (((LTupleProjection _, _) as lhs), _, rhs) ->
        index_reads (Stmt.Helpers.lhs_indices lhs)
        @ expr_reads rhs
        @ [mk (Stmt.Helpers.lhs_variable lhs) [Varying Nonlinear] true]
    | Decl {decl_id; initialize= Assign e; _} ->
        expr_reads e @ [mk decl_id [] true]
    | Decl {decl_id; _} -> [mk decl_id [] true]
    | TargetPE e | JacobianPE e -> expr_reads e @ [mk "target" [] true]
    | Return (Some e) -> expr_reads e
    | NRFunApp (kind, args) ->
        List.concat_map (Fun_kind.collect_exprs kind @ args) ~f:expr_reads
    | IfElse (cond, s1, s2) ->
        expr_reads cond @ sub s1 @ Option.value_map s2 ~default:[] ~f:sub
    | While (cond, body) -> expr_reads cond @ sub body
    | For {loopvar= inner; lower; upper; body} ->
        (mk inner [] true :: expr_reads lower) @ expr_reads upper @ sub body
    | Profile (_, stmts) | Block stmts | SList stmts ->
        List.concat_map stmts ~f:sub
    | Break | Continue | Skip | Return None -> [] in
  stmt_reads_writes stmt

let rec stmt_accesses ~loopvar ~written ~label
    (stmt : (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t) : access list =
  accesses_of_pattern ~loopvar ~written ~label stmt
    ~sub:(fun (s : Stmt.Located.t) ->
      stmt_accesses ~loopvar ~written ~label s.pattern)

(** {2 Dependence test} *)

let all_directions = Set.Poly.of_list [Lt; Eq; Gt]
let confused = Dependent {directions= all_directions; distance= None}

(** The dependence between one subscript position of two accesses. *)
let subscript_dependence (a : subscript) (b : subscript) : dependence =
  match (a, b) with
  | Affine {coeff= c1; offset= o1}, Affine {coeff= c2; offset= o2}
    when c1 = c2 && linear_is_const (linear_sub o1 o2) ->
      (* strong SIV (Goff, Kennedy and Tseng 1991 §3; LLVM [strongSIVtest]):
         [c*i1 + o1 = c*i2 + o2] iff [i2 - i1 = (o1 - o2) / c]. Identical
         symbolic terms have cancelled in the subtraction. *)
      let num : int = (linear_sub o1 o2).const in
      if num mod c1 <> 0 then Independent
      else
        let d = num / c1 in
        let directions =
          if d = 0 then Set.Poly.singleton Eq
          else if d > 0 then Set.Poly.singleton Lt
          else Set.Poly.singleton Gt in
        Dependent {directions; distance= Some d}
  | Invariant o1, Invariant o2 when linear_is_const (linear_sub o1 o2) ->
      (* ZIV: same symbols, so the elements differ iff the constants do *)
      if (linear_sub o1 o2).const <> 0 then Independent else confused
  | Affine _, Affine _
   |Affine _, Invariant _
   |Invariant _, Affine _
   |Invariant _, Invariant _
   |Varying _, _
   |_, Varying _ ->
      confused

(** The dependence between two accesses to the same variable, at least one of
    them a write. Each subscript position is tested by [subscript_dependence]
    and the positions are merged as separable subscripts (Kennedy and Allen):
    any [Independent] position, two known distances that differ, or an empty
    intersection of direction sets gives [Independent]; otherwise the direction
    sets are intersected and the common distance is kept. Accesses with
    different numbers of positions ([v[n]] vs [v[n, k]]) and two whole-variable
    accesses ([subs = []]) are confused. Symmetric up to swapping [Lt] and [Gt]
    and negating the distance. *)
let access_dependence (a : access) (b : access) : dependence =
  let merge acc pos =
    match (acc, pos) with
    | Independent, _ | _, Independent -> Independent
    | ( Dependent {directions= d1; distance= dist1}
      , Dependent {directions= d2; distance= dist2} ) -> (
        let directions = Set.Poly.inter d1 d2 in
        match (dist1, dist2) with
        | Some x, Some y when x <> y -> Independent
        | _ when Set.Poly.is_empty directions -> Independent
        | _ -> Dependent {directions; distance= Option.first_some dist1 dist2})
  in
  if List.length a.subs <> List.length b.subs then confused
  else
    List.fold_left2 a.subs b.subs ~init:confused ~f:(fun acc x y ->
        merge acc (subscript_dependence x y))

(** {2 Printers} *)

(** [k+1], [+k-2*m+1] ...; with [leading], the first item has no leading [+] and
    a bare constant is printed even when it is [0]. *)
let pp_linear ~leading ppf ({const; terms} : linear) =
  let first = ref leading in
  let sign c =
    let s = if c < 0 then "-" else if !first then "" else "+" in
    first := false;
    s in
  List.iter terms ~f:(fun (c, e) ->
      let s = sign c in
      match abs c with
      | 1 -> Fmt.pf ppf "%s%a" s Expr.Typed.pp e
      | c -> Fmt.pf ppf "%s%d*%a" s c Expr.Typed.pp e);
  if const <> 0 || (leading && List.is_empty terms) then
    Fmt.pf ppf "%s%d" (sign const) (abs const)

let pp_varying_kind ppf = function
  | Slice -> Fmt.string ppf "slice"
  | Multi_index -> Fmt.string ppf "multi"
  | Written -> Fmt.string ppf "written"
  | Gather -> Fmt.string ppf "gather"
  | Nonlinear -> Fmt.string ppf "nonlinear"

(** [i], [i+1], [-i+2], [2i+k-1] for [Affine]; [3], [k+1] for [Invariant];
    [?gather], [?written], ... for [Varying]. *)
let pp_subscript ppf = function
  | Invariant l -> pp_linear ~leading:true ppf l
  | Affine {coeff; offset} ->
      (match coeff with
      | 1 -> Fmt.string ppf "i"
      | -1 -> Fmt.string ppf "-i"
      | c -> Fmt.pf ppf "%di" c);
      pp_linear ~leading:false ppf offset
  | Varying kind -> Fmt.pf ppf "?%a" pp_varying_kind kind

(** [W v[i+1]], [R v]. *)
let pp_access ppf {var; subs; is_write; _} =
  Fmt.pf ppf "%s %s" (if is_write then "W" else "R") var;
  if not (List.is_empty subs) then
    Fmt.pf ppf "[%a]" Fmt.(list ~sep:(any ", ") pp_subscript) subs

let pp_direction ppf = function
  | Lt -> Fmt.string ppf "<"
  | Eq -> Fmt.string ppf "="
  | Gt -> Fmt.string ppf ">"

(** [independent], or [{<,=,>}] with [d=k] when the distance is known, e.g.
    [{<} d=1], [{=} d=0], [{<,=,>}]. *)
let pp_dependence ppf = function
  | Independent -> Fmt.string ppf "independent"
  | Dependent {directions; distance} ->
      Fmt.pf ppf "{%a}"
        Fmt.(list ~sep:(any ",") pp_direction)
        (Set.Poly.to_list directions);
      Option.iter distance ~f:(Fmt.pf ppf " d=%d")

(***********************************)
(* Loop dependence graph (L3)      *)
(***********************************)

(** The leaf statements of a loop body in lexical order: [Block] and [SList]
    nesting is flattened; a [Profile], an [IfElse], a [While], an inner [For]
    and every simple statement is one leaf. *)
let rec loop_leaves (s : Stmt.Located.t) : Stmt.Located.t list =
  match s.pattern with
  | Block l | SList l -> List.concat_map l ~f:loop_leaves
  | Assignment _ | TargetPE _ | JacobianPE _ | NRFunApp _ | Break | Continue
   |Return _ | Skip | IfElse _ | While _ | For _ | Profile _ | Decl _ ->
      [s]

(** A statement has effects when it, or a substatement, prints, rejects, calls a
    user-defined function as a statement (it may print or reject internally), or
    contains an expression that [Mir_utils.can_side_effect_top_expr] flags. Two
    effectful statements keep their relative order across iterations (§7.5). *)
let rec stmt_has_effects (s : Stmt.Located.t) : bool =
  match s.pattern with
  | NRFunApp (CompilerInternal (FnPrint | FnReject | FnFatalError), _)
   |NRFunApp (UserDefined _, _) ->
      true
  | NRFunApp ((StanLib _ | CompilerInternal _), _)
   |Assignment _ | TargetPE _ | JacobianPE _ | Break | Continue | Return _
   |Skip | IfElse _ | While _ | For _ | Profile _ | Block _ | SList _ | Decl _
    ->
      Stmt.Pattern.fold
        (fun acc e -> acc || cannot_remove_expr e)
        (fun acc s -> acc || stmt_has_effects s)
        false s.pattern

(** The same dependence seen from the other access: [Lt] and [Gt] swap and the
    distance changes sign. *)
let flip_dependence = function
  | Independent -> Independent
  | Dependent {directions; distance} ->
      Dependent
        { directions=
            Set.Poly.map directions ~f:(function
              | Lt -> Gt
              | Gt -> Lt
              | Eq -> Eq)
        ; distance= Option.map distance ~f:(fun d -> -d) }

let dep_kind ~src_is_write ~dst_is_write =
  match (src_is_write, dst_is_write) with
  | true, true -> Output
  | true, false -> True_dep
  | false, _ -> Anti

(** The dependence graph of one loop level (§7.5). Nodes are the [loop_leaves]
    of [body]; for every pair of accesses to the same variable with at least one
    write, [access_dependence] decides the edges:
    - [Eq] between different statements: an edge from the lexically earlier to
      the later one (loop-independent dependences are directed forward, Allen
      and Kennedy 1987 p. 515); [Eq] within one statement adds no edge, because
      a vector statement fetches all inputs before storing (p. 501);
    - [Lt] (the earlier statement's access happens in an earlier iteration): an
      edge from the earlier to the later statement; [Gt]: the reverse;
    - within one statement, [Lt] or [Gt] is a recurrence (self-edge) unless the
      pair is a pure anti-dependence, i.e. the earlier-iteration access is the
      read ([a[n] = a[n+1] + 1]; GCC's "dependence distance negative", LLVM's
      [memdep.ll] [f1_vec]);
    - two effectful statements get edges both ways. Every edge is oriented so
      that its source executes no later than its sink in the original loop,
      which is what makes emitting the pi-blocks in a topological order legal
      (Fundamental Theorem of Dependence). *)
let loop_dependence_graph ~loopvar (body : Stmt.Located.t) :
    loop_dependence_graph =
  let written = Stmt.Helpers.assigned_or_declared_variables body in
  let nodes =
    loop_leaves body
    |> List.mapi ~f:(fun pos (stmt : Stmt.Located.t) ->
        { pos
        ; stmt
        ; accesses= stmt_accesses ~loopvar ~written ~label:pos stmt.pattern
        ; effects= stmt_has_effects stmt })
    |> Array.of_list in
  (* [target +=] increments commute, so they carry no dependence among
     themselves unless a statement observes the running sum with [target()];
     then every increment and every read are kept in order. *)
  let target_observed =
    Array.exists nodes ~f:(fun (node : loop_node) ->
        List.exists node.accesses ~f:(fun (a : access) ->
            String.equal a.var "target" && not a.is_write)) in
  let same_var_with_write (x : access) (y : access) =
    String.equal x.var y.var && (x.is_write || y.is_write)
    && (target_observed || not (String.equal x.var "target")) in
  let has dir (dep : dependence) =
    match dep with
    | Independent -> false
    | Dependent {directions; _} -> Set.Poly.mem dir directions in
  (* [a] is lexically before [b]; [x] in [a], [y] in [b] *)
  let cross_edges (a : loop_node) (b : loop_node) =
    List.concat_map a.accesses ~f:(fun x ->
        List.concat_map b.accesses ~f:(fun y ->
            if not (same_var_with_write x y) then []
            else
              let dep = access_dependence x y in
              let fwd =
                if has Eq dep || has Lt dep then
                  [ { src= a.pos
                    ; dst= b.pos
                    ; var= x.var
                    ; kind=
                        dep_kind ~src_is_write:x.is_write
                          ~dst_is_write:y.is_write
                    ; dep } ]
                else [] in
              let bwd =
                if has Gt dep then
                  [ { src= b.pos
                    ; dst= a.pos
                    ; var= x.var
                    ; kind=
                        dep_kind ~src_is_write:y.is_write
                          ~dst_is_write:x.is_write
                    ; dep } ]
                else [] in
              fwd @ bwd)) in
  let self_edges (a : loop_node) =
    let rec pairs = function
      | [] -> []
      | x :: rest -> List.map rest ~f:(fun y -> (x, y)) @ pairs rest in
    List.concat_map (pairs a.accesses) ~f:(fun (x, y) ->
        if not (same_var_with_write x y) then []
        else
          let dep = access_dependence x y in
          (* [earlier] is the access of the earlier iteration; the edge's
             dependence is expressed from it, so a recurrence always prints as
             [{<} d=k] with [k > 0] *)
          let recurrence ~earlier ~later ~dep =
            let pure_anti = (not earlier.is_write) && later.is_write in
            if pure_anti then []
            else
              [ { src= a.pos
                ; dst= a.pos
                ; var= x.var
                ; kind=
                    dep_kind ~src_is_write:earlier.is_write
                      ~dst_is_write:later.is_write
                ; dep } ] in
          (if has Lt dep then recurrence ~earlier:x ~later:y ~dep else [])
          @
          if has Gt dep then
            recurrence ~earlier:y ~later:x ~dep:(flip_dependence dep)
          else []) in
  let effect_edges (a : loop_node) (b : loop_node) =
    if a.effects && b.effects then
      [ {src= a.pos; dst= b.pos; var= ""; kind= Effects; dep= confused}
      ; {src= b.pos; dst= a.pos; var= ""; kind= Effects; dep= confused} ]
    else [] in
  let n = Array.length nodes in
  let edges = ref [] in
  for i = 0 to n - 1 do
    edges := self_edges nodes.(i) @ !edges;
    for j = i + 1 to n - 1 do
      edges :=
        cross_edges nodes.(i) nodes.(j)
        @ effect_edges nodes.(i) nodes.(j)
        @ !edges
    done
  done;
  (* one edge per (src, dst, var, kind): the first found *)
  let key (e : loop_edge) = (e.src, e.dst, e.var, e.kind) in
  let edges =
    List.fold_left (List.rev !edges) ~init:[] ~f:(fun acc e ->
        if List.exists acc ~f:(fun e' -> key e' = key e) then acc else e :: acc)
    |> List.sort ~cmp:(fun e1 e2 -> compare (key e1) (key e2)) in
  {nodes; edges}

(** The pi-blocks of a graph (Allen and Kennedy 1987 §5.2): its strongly
    connected components (Tarjan), each listing its nodes in lexical order, in a
    topological order of the condensation with ties broken by the smallest
    lexical position (Kahn's algorithm with a min-position priority). A body
    with only forward edges therefore comes back in original order. *)
let pi_blocks (g : loop_dependence_graph) : int list list =
  let n = Array.length g.nodes in
  let succs = Array.make n [] in
  List.iter g.edges ~f:(fun e ->
      if e.src <> e.dst then succs.(e.src) <- e.dst :: succs.(e.src));
  (* Tarjan's strongly connected components *)
  let index = Array.make n (-1) and low = Array.make n 0 in
  let on_stack = Array.make n false in
  let stack = ref [] and counter = ref 0 and comps = ref [] in
  let rec strongconnect v =
    index.(v) <- !counter;
    low.(v) <- !counter;
    incr counter;
    stack := v :: !stack;
    on_stack.(v) <- true;
    List.iter succs.(v) ~f:(fun w ->
        if index.(w) < 0 then (
          strongconnect w;
          low.(v) <- min low.(v) low.(w))
        else if on_stack.(w) then low.(v) <- min low.(v) index.(w));
    if low.(v) = index.(v) then
      let rec pop acc =
        match !stack with
        | w :: rest ->
            stack := rest;
            on_stack.(w) <- false;
            if w = v then w :: acc else pop (w :: acc)
        | [] -> acc in
      comps := List.sort (pop []) ~cmp:Int.compare :: !comps in
  for v = 0 to n - 1 do
    if index.(v) < 0 then strongconnect v
  done;
  (* condensation, then Kahn with a min-position priority *)
  let comps = Array.of_list !comps in
  let m = Array.length comps in
  let comp_of = Array.make n 0 in
  Array.iteri comps ~f:(fun c members ->
      List.iter members ~f:(fun v -> comp_of.(v) <- c));
  let indeg = Array.make m 0 and csucc = Array.make m [] in
  List.iter g.edges ~f:(fun e ->
      let a = comp_of.(e.src) and b = comp_of.(e.dst) in
      if a <> b && not (List.exists csucc.(a) ~f:(fun c -> c = b)) then (
        csucc.(a) <- b :: csucc.(a);
        indeg.(b) <- indeg.(b) + 1));
  let first_pos c = match comps.(c) with v :: _ -> v | [] -> max_int in
  let rec kahn ready acc =
    match ready with
    | [] -> List.rev acc
    | c0 :: rest ->
        let c =
          List.fold_left rest ~init:c0 ~f:(fun best c ->
              if first_pos c < first_pos best then c else best) in
        let ready = List.filter ready ~f:(fun c' -> c' <> c) in
        let ready =
          List.fold_left csucc.(c) ~init:ready ~f:(fun r b ->
              indeg.(b) <- indeg.(b) - 1;
              if indeg.(b) = 0 then b :: r else r) in
        kahn ready (comps.(c) :: acc) in
  kahn (List.filter (List.init ~len:m ~f:Fun.id) ~f:(fun c -> indeg.(c) = 0)) []

(** A pi-block is cyclic iff it has more than one node or a self-edge; a cyclic
    block must stay a sequential loop. *)
let is_cyclic (g : loop_dependence_graph) (block : int list) : bool =
  match block with
  | [] -> false
  | [v] -> List.exists g.edges ~f:(fun e -> e.src = v && e.dst = v)
  | _ :: _ :: _ -> true

(** {2 Printers for the graph} *)

let pp_stmt_one_line ppf (s : Stmt.Located.t) =
  let b = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer b in
  Format.pp_set_margin fmt 100_000;
  Stmt.Located.pp fmt s;
  Format.pp_print_flush fmt ();
  let str =
    Buffer.contents b
    |> String.split_on_char ~sep:'\n'
    |> List.map ~f:String.trim |> String.concat ~sep:" " in
  let limit = 96 in
  if String.length str > limit then
    Fmt.pf ppf "%s..." (String.sub str ~pos:0 ~len:limit)
  else Fmt.string ppf str

let pp_dep_kind ppf = function
  | True_dep -> Fmt.string ppf "true"
  | Anti -> Fmt.string ppf "anti"
  | Output -> Fmt.string ppf "output"
  | Effects -> Fmt.string ppf "effects"

(** [S0 -> S1 muj {=} d=0 (true)], or [S0 -> S2 (effects)]. *)
let pp_loop_edge ppf (e : loop_edge) =
  match e.kind with
  | Effects -> Fmt.pf ppf "S%d -> S%d (effects)" e.src e.dst
  | True_dep | Anti | Output ->
      Fmt.pf ppf "S%d -> S%d %s %a (%a)" e.src e.dst e.var pp_dependence e.dep
        pp_dep_kind e.kind

let pp_edges ppf (g : loop_dependence_graph) =
  match g.edges with
  | [] -> Fmt.string ppf "edges: none"
  | edges ->
      Fmt.pf ppf "edges: %a" Fmt.(list ~sep:(any "; ") pp_loop_edge) edges

(** [blocks: [S0] [S1 S2]cyclic] *)
let pp_blocks (g : loop_dependence_graph) ppf (blocks : int list list) =
  let pp_block ppf block =
    Fmt.pf ppf "[%a]%s"
      Fmt.(list ~sep:(any " ") (fun ppf v -> Fmt.pf ppf "S%d" v))
      block
      (if is_cyclic g block then "cyclic" else "") in
  Fmt.pf ppf "blocks: %a" Fmt.(list ~sep:(any " ") pp_block) blocks

(** Numbered leaf statements, then the edges, then the pi-blocks in emission
    order with a [cyclic] marker. *)
let pp_loop_dependence_graph ppf (g : loop_dependence_graph) =
  Array.iter g.nodes ~f:(fun (node : loop_node) ->
      Fmt.pf ppf "S%d  %a@." node.pos pp_stmt_one_line node.stmt);
  Fmt.pf ppf "%a@.%a" pp_edges g (pp_blocks g) (pi_blocks g)
