(** Data dependence analysis for one loop level: the access model (L1), the
    dependence test (L2) and the loop dependence graph with its pi-blocks (L3)
    of design-docs/active/vectorize-loop-fission.md. [Loop_vectorize] is the
    client for code generation; [Dependence_analysis] uses the access model to
    refine reaching-definition edges. *)

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

(** [e] as one opaque symbolic term. *)
let linear_symbol e : linear = {const= 0; terms= [(1, e)]}

(** Merge equal terms, drop zero coefficients, sort. *)
let linear_normalize ({const; terms} : linear) : linear =
  (* [acc] holds the merged terms seen so far, most recent first *)
  let merge_into acc (c, e) =
    match acc with
    | (c', e') :: rest when Expr.Typed.compare e e' = 0 -> (c + c', e') :: rest
    | [] | (_, _) :: _ -> (c, e) :: acc in
  let terms =
    List.sort terms ~cmp:(fun (_, e1) (_, e2) -> Expr.Typed.compare e1 e2)
    |> List.fold_left ~init:[] ~f:merge_into
    |> List.filter ~f:(fun (c, _) -> c <> 0)
    |> List.rev in
  {const; terms}

let linear_scale k ({const; terms} : linear) : linear =
  linear_normalize
    {const= k * const; terms= List.map terms ~f:(fun (c, e) -> (k * c, e))}

let linear_add (a : linear) (b : linear) : linear =
  linear_normalize {const= a.const + b.const; terms= a.terms @ b.terms}

let linear_sub a b = linear_add a (linear_scale (-1) b)

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
  let symbolic () = if invariant e then Some (0, linear_symbol e) else None in
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

(** [e] mentions one of [vars]. *)
let mentions vars (e : Expr.Typed.t) =
  not (Set.Poly.disjoint (expr_var_names_set e) vars)

(** The loop variable sits under another index somewhere in [e], as in [idx[n]].
*)
let rec is_gather ~loopvar (e : Expr.Typed.t) =
  match e.pattern with
  | Indexed (_, idcs) ->
      List.exists
        (List.concat_map idcs ~f:Index.bounds)
        ~f:(mentions (Set.Poly.singleton loopvar))
  | Var _ | Lit _ -> false
  | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ | TupleProjection _ ->
      Expr.Pattern.fold
        (fun acc e -> acc || is_gather ~loopvar e)
        false e.pattern

(** Classify one index position of a reference with respect to the loop over
    [loopvar]. [written_vars] is the set of names assigned or declared anywhere
    in the loop body, including inner loop variables
    ([Stmt.Helpers.assigned_or_declared_variables body]).

    A [Single] index is put in linear form [coeff * loopvar + offset] by
    [linear_form]; every loop-invariant sub-expression the evaluator does not
    interpret (a data variable [k], a call [f(k)], ...) becomes a symbolic term
    of [offset]. [coeff <> 0] gives [Affine], [coeff = 0] gives [Invariant].
    Otherwise the result is [Varying] with the reason the debug report prints:
    [Written] if the index mentions a name in [written_vars] (an inner loop
    variable, a body scalar), [Gather] if the loop variable sits under another
    index ([idx[n]]), [Nonlinear] otherwise ([n * k], [n * n]); [All], [Upfrom]
    and [Between] are [Slice] and [MultiIndex] is [Multi_index]. *)
let classify_subscript ~loopvar ~written_vars (idx : Expr.Typed.t Index.t) :
    subscript =
  match idx with
  | MultiIndex _ -> Varying Multi_index
  | All | Upfrom _ | Between _ -> Varying Slice
  | Single e -> (
      let invariant e = not (mentions (Set.Poly.add loopvar written_vars) e) in
      match linear_form ~loopvar ~invariant e with
      | Some (0, offset) -> Invariant offset
      | Some (coeff, offset) -> Affine {coeff; offset}
      | None when mentions written_vars e -> Varying Written
      | None when is_gather ~loopvar e -> Varying Gather
      | None -> Varying Nonlinear)

(** {2 Increments (design §7.4)} *)

(** [s = s + e], [s = e + s] or [s = s - e] for a scalar [s] assigned as a
    whole, with [e] free of [s]: the MIR of [s += e] and [s -= e]. [accumulator]
    is the typed [Var s]. *)
type increment =
  {var: string; accumulator: Expr.Typed.t; op: Operator.t; operand: Expr.Typed.t}

let is_var name (e : Expr.Typed.t) =
  match e.pattern with
  | Var v -> String.equal v name
  | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _ | Promotion _
   |TupleProjection _ ->
      false

(** The increment a statement performs, if it has that shape. [u = u + u * a[n]]
    is not one: its operand reads the accumulator, so it is a recurrence. *)
let increment_shape (stmt : (Expr.Typed.t, 's) Stmt.Pattern.t) :
    increment option =
  match stmt with
  | Assignment ((LVariable var, []), (UInt | UReal), rhs) -> (
      let increment accumulator op operand =
        Option.some_if
          (not (mentions (Set.Poly.singleton var) operand))
          {var; accumulator; op; operand} in
      match operator_app rhs with
      | Some (((Plus | Minus) as op), [a; b]) when is_var var a ->
          increment a op b
      | Some (Plus, [a; b]) when is_var var b -> increment b Plus a
      | Some ((Plus | Minus), _)
       |Some
          ( ( PPlus | PMinus | Times | Divide | IntDivide | Modulo | LDivide
            | EltTimes | EltDivide | Pow | EltPow | Or | And | Equals | NEquals
            | Less | Leq | Greater | Geq | PNot | Transpose )
          , _ )
       |None ->
          None)
  | Assignment _ | TargetPE _ | JacobianPE _ | NRFunApp _ | Break | Continue
   |Return _ | Skip | IfElse _ | While _ | For _ | Profile _ | Block _
   |SList _ | Decl _ ->
      None

(** {2 Accesses} *)

let read ~label var subs = {var; subs; kind= Read; label}
let write ~label var subs = {var; subs; kind= Write; label}
let increment ~label var = {var; subs= []; kind= Increment; label}
let index_bounds idcs = List.concat_map idcs ~f:Index.bounds

(** Every reference an expression reads, in evaluation order. A reference
    [v[idcs]] is one access with classified subscripts followed by the reads
    inside its indices; a [target()] call reads ["target"], the running sum.
    Reads of [loopvar] itself are not recorded: it is defined by the loop
    header, so it can never carry a dependence. *)
let rec expr_reads ~loopvar ~written_vars ~label (e : Expr.Typed.t) :
    access list =
  let reads = expr_reads ~loopvar ~written_vars ~label in
  let reads_all es = List.concat_map es ~f:reads in
  match e.pattern with
  | Var v when String.equal v loopvar -> []
  | Var v -> [read ~label v []]
  | Lit _ -> []
  | Indexed ({pattern= Var v; _}, idcs) ->
      let subs = List.map idcs ~f:(classify_subscript ~loopvar ~written_vars) in
      read ~label v subs :: reads_all (index_bounds idcs)
  | Indexed (base, idcs) -> reads base @ reads_all (index_bounds idcs)
  | FunApp ((StanLib (_, FnTarget, _) | UserDefined (_, FnTarget)), args) ->
      read ~label "target" [] :: reads_all args
  | FunApp (kind, args) -> reads_all (Fun_kind.collect_exprs kind @ args)
  | TernaryIf (a, b, c) -> reads_all [a; b; c]
  | EAnd (a, b) | EOr (a, b) -> reads_all [a; b]
  | Promotion (e, _, _) | TupleProjection (e, _) -> reads e

(** Every reference in a statement and its substatements, in evaluation order,
    as [access]es of the loop over [loopvar]. An [Assignment] to [v] yields a
    write to [v] (an [LTupleProjection] base is a write with
    [subs = [Varying Nonlinear]]) after the reads of its indices and right-hand
    side; a [Decl] yields a write with [subs = []]; an inner [For] yields a
    write to its own loop variable with [subs = []]. An assignment with
    [increment_shape], and [TargetPE] and [JacobianPE], yield one [Increment]
    access to the accumulator after the reads of the operand: the accumulator's
    own read and write are the increment (design §7.4). [sub] gives the accesses
    of a substatement. *)
let accesses_of_pattern ~loopvar ~written_vars ~label ~(sub : 's -> access list)
    (stmt : (Expr.Typed.t, 's) Stmt.Pattern.t) : access list =
  let reads = expr_reads ~loopvar ~written_vars ~label in
  let reads_all es = List.concat_map es ~f:reads in
  let write = write ~label in
  let increment = increment ~label in
  match stmt with
  | Assignment ((LVariable v, idcs), _, rhs) -> (
      match increment_shape stmt with
      | Some {operand; _} -> reads operand @ [increment v]
      | None ->
          let subs =
            List.map idcs ~f:(classify_subscript ~loopvar ~written_vars) in
          reads_all (index_bounds idcs) @ reads rhs @ [write v subs])
  | Assignment (((LTupleProjection _, _) as lhs), _, rhs) ->
      reads_all (index_bounds (Stmt.Helpers.lhs_indices lhs))
      @ reads rhs
      @ [write (Stmt.Helpers.lhs_variable lhs) [Varying Nonlinear]]
  | Decl {decl_id; initialize= Assign e; _} -> reads e @ [write decl_id []]
  | Decl {decl_id; _} -> [write decl_id []]
  | TargetPE e | JacobianPE e -> reads e @ [increment "target"]
  | Return (Some e) -> reads e
  | NRFunApp (kind, args) -> reads_all (Fun_kind.collect_exprs kind @ args)
  | IfElse (cond, s1, s2) ->
      reads cond @ sub s1 @ Option.value_map s2 ~default:[] ~f:sub
  | While (cond, body) -> reads cond @ sub body
  | For {loopvar= inner; lower; upper; body} ->
      (write inner [] :: reads_all [lower; upper]) @ sub body
  | Profile (_, stmts) | Block stmts | SList stmts ->
      List.concat_map stmts ~f:sub
  | Break | Continue | Skip | Return None -> []

let rec stmt_accesses ~loopvar ~written_vars ~label
    (stmt : (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t) : access list =
  accesses_of_pattern ~loopvar ~written_vars ~label stmt
    ~sub:(fun (s : Stmt.Located.t) ->
      stmt_accesses ~loopvar ~written_vars ~label s.pattern)

(** {2 Dependence test} *)

let all_directions = Set.Poly.of_list [Lt; Eq; Gt]
let confused = Dependent {directions= all_directions; distance= None}

(** The dependence whose only direction is that of the known distance [d]. *)
let dependence_at_distance d =
  let direction = if d = 0 then Eq else if d > 0 then Lt else Gt in
  Dependent {directions= Set.Poly.singleton direction; distance= Some d}

(** The dependence between one subscript position of two accesses. *)
let subscript_dependence (a : subscript) (b : subscript) : dependence =
  match (a, b) with
  | Affine {coeff= c1; offset= o1}, Affine {coeff= c2; offset= o2} when c1 = c2
    -> (
      (* strong SIV (Goff, Kennedy and Tseng 1991 §3; LLVM [strongSIVtest]):
         [c*i1 + o1 = c*i2 + o2] iff [i2 - i1 = (o1 - o2) / c]. Identical
         symbolic terms have cancelled in the subtraction. *)
      match linear_sub o1 o2 with
      | {const; terms= []} ->
          if const mod c1 <> 0 then Independent
          else dependence_at_distance (const / c1)
      | {terms= _ :: _; _} -> confused)
  | Invariant o1, Invariant o2 -> (
      (* ZIV: same symbols, so the elements differ iff the constants do *)
      match linear_sub o1 o2 with
      | {const; terms= []} -> if const <> 0 then Independent else confused
      | {terms= _ :: _; _} -> confused)
  | Affine _, Affine _
   |Affine _, Invariant _
   |Invariant _, Affine _
   |Varying _, (Affine _ | Invariant _ | Varying _)
   |(Affine _ | Invariant _), Varying _ ->
      confused

(** Merge the dependences of two subscript positions as separable subscripts
    (Kennedy and Allen): any [Independent] position, two known distances that
    differ, or an empty intersection of direction sets gives [Independent];
    otherwise the direction sets are intersected and the common distance kept.
*)
let merge_positions (a : dependence) (b : dependence) : dependence =
  match (a, b) with
  | Independent, (Independent | Dependent _) | Dependent _, Independent ->
      Independent
  | ( Dependent {directions= d1; distance= dist1}
    , Dependent {directions= d2; distance= dist2} ) ->
      let directions = Set.Poly.inter d1 d2 in
      let distances_differ =
        match (dist1, dist2) with
        | Some x, Some y -> x <> y
        | Some _, None | None, Some _ | None, None -> false in
      if distances_differ || Set.Poly.is_empty directions then Independent
      else Dependent {directions; distance= Option.first_some dist1 dist2}

(** The dependence between two accesses to the same variable, at least one of
    them a write: each subscript position is tested by [subscript_dependence]
    and the positions are combined by [merge_positions]. Accesses with different
    numbers of positions ([v[n]] vs [v[n, k]]) and two whole-variable accesses
    ([subs = []]) are confused. Symmetric up to swapping [Lt] and [Gt] and
    negating the distance. *)
let access_dependence (a : access) (b : access) : dependence =
  if List.length a.subs <> List.length b.subs then confused
  else
    List.fold_left2 a.subs b.subs ~init:confused ~f:(fun acc x y ->
        merge_positions acc (subscript_dependence x y))

(** {2 Printers} *)

(** [k+1], [+k-2*m+1] ...; with [leading], the first item has no leading [+] and
    a bare constant is printed even when it is [0]. *)
let pp_linear ~leading ppf ({const; terms} : linear) =
  let sign ~first c = if c < 0 then "-" else if first then "" else "+" in
  List.iteri terms ~f:(fun i (c, e) ->
      let s = sign ~first:(leading && i = 0) c in
      match abs c with
      | 1 -> Fmt.pf ppf "%s%a" s Expr.Typed.pp e
      | c -> Fmt.pf ppf "%s%d*%a" s c Expr.Typed.pp e);
  let no_terms = List.is_empty terms in
  if const <> 0 || (leading && no_terms) then
    Fmt.pf ppf "%s%d" (sign ~first:(leading && no_terms) const) (abs const)

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

(** [W v[i+1]], [R v], [+= s]. *)
let pp_access ppf {var; subs; kind; _} =
  Fmt.pf ppf "%s %s"
    (match kind with Write -> "W" | Read -> "R" | Increment -> "+=")
    var;
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

let has_direction dir = function
  | Independent -> false
  | Dependent {directions; _} -> Set.Poly.mem dir directions

(** Two reads never form an edge; callers pair a write with another access. An
    increment counts as a write. *)
let dep_kind ~src_is_write ~dst_is_write =
  match (src_is_write, dst_is_write) with
  | true, true -> Output
  | true, false -> True_dep
  | false, true | false, false -> Anti

(** The edge from access [src] in node [src_pos] to access [dst] in node
    [dst_pos]. *)
let edge ~src_pos ~dst_pos (src : access) (dst : access) dep : loop_edge =
  { src= src_pos
  ; dst= dst_pos
  ; var= src.var
  ; kind=
      dep_kind ~src_is_write:(access_writes src)
        ~dst_is_write:(access_writes dst)
  ; dep }

(** Two accesses to the same variable that must keep their order: at least one
    writes, and they are not two increments, which commute ([s += a; s += b]
    gives the same [s] in any order, design §7.4). *)
let related (x : access) (y : access) =
  String.equal x.var y.var
  && (access_writes x || access_writes y)
  &&
  match (x.kind, y.kind) with
  | Increment, Increment -> false
  | (Read | Write), (Read | Write | Increment) | Increment, (Read | Write) ->
      true

(** Edges between two different nodes, [a] lexically before [b], for each pair
    of [related] accesses [x] in [a] and [y] in [b]: [Eq] or [Lt] gives an edge
    from [a] to [b], [Gt] one from [b] to [a]. *)
let cross_edges ~related (a : loop_node) (b : loop_node) =
  List.concat_map a.accesses ~f:(fun x ->
      List.concat_map b.accesses ~f:(fun y ->
          if not (related x y) then []
          else
            let dep = access_dependence x y in
            let forward =
              if has_direction Eq dep || has_direction Lt dep then
                [edge ~src_pos:a.pos ~dst_pos:b.pos x y dep]
              else [] in
            let backward =
              if has_direction Gt dep then
                [edge ~src_pos:b.pos ~dst_pos:a.pos y x dep]
              else [] in
            forward @ backward))

(** A self-edge of node [pos] from the access of the earlier iteration to the
    access of the later one, unless the pair is a pure anti-dependence, i.e. the
    earlier access is the read ([a[n] = a[n+1] + 1]; GCC's "dependence distance
    negative", LLVM's [memdep.ll] [f1_vec]). *)
let recurrence_edge pos ~earlier ~later dep =
  let pure_anti = (not (access_writes earlier)) && access_writes later in
  if pure_anti then [] else [edge ~src_pos:pos ~dst_pos:pos earlier later dep]

let rec unordered_pairs = function
  | [] -> []
  | x :: rest -> List.map rest ~f:(fun y -> (x, y)) @ unordered_pairs rest

(** Self-edges of one node from every pair of [related] accesses. The edge's
    dependence is expressed from the earlier iteration's access, so a recurrence
    always prints as [{<} d=k] with [k > 0]. *)
let self_edges ~related (a : loop_node) =
  List.concat_map (unordered_pairs a.accesses) ~f:(fun (x, y) ->
      if not (related x y) then []
      else
        let dep = access_dependence x y in
        let later_y =
          if has_direction Lt dep then
            recurrence_edge a.pos ~earlier:x ~later:y dep
          else [] in
        let later_x =
          if has_direction Gt dep then
            recurrence_edge a.pos ~earlier:y ~later:x (flip_dependence dep)
          else [] in
        later_y @ later_x)

(** Two effectful statements get edges both ways. *)
let effect_edges (a : loop_node) (b : loop_node) =
  if a.effects && b.effects then
    [ {src= a.pos; dst= b.pos; var= ""; kind= Effects; dep= confused}
    ; {src= b.pos; dst= a.pos; var= ""; kind= Effects; dep= confused} ]
  else []

(** One edge per [(src, dst, var, kind)], the first found, sorted by that key.
*)
let dedup_edges (edges : loop_edge list) =
  let key (e : loop_edge) = (e.src, e.dst, e.var, e.kind) in
  let keep (seen, kept) e =
    if Set.Poly.mem (key e) seen then (seen, kept)
    else (Set.Poly.add (key e) seen, e :: kept) in
  List.fold_left edges ~init:(Set.Poly.empty, []) ~f:keep
  |> snd
  |> List.sort ~cmp:(fun e1 e2 -> compare (key e1) (key e2))

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
      pair is a pure anti-dependence;
    - two increments of the same accumulator are not [related]: they commute, so
      a scalar the body only ever increments ([target] included) gets no edge at
      all, while any other access to it orders everything again;
    - two effectful statements get edges both ways. Every edge is oriented so
      that its source executes no later than its sink in the original loop,
      which is what makes emitting the pi-blocks in a topological order legal
      (Fundamental Theorem of Dependence). *)
let loop_dependence_graph ~loopvar (body : Stmt.Located.t) :
    loop_dependence_graph =
  let written_vars = Stmt.Helpers.assigned_or_declared_variables body in
  let node pos (stmt : Stmt.Located.t) : loop_node =
    { pos
    ; stmt
    ; accesses= stmt_accesses ~loopvar ~written_vars ~label:pos stmt.pattern
    ; effects= stmt_has_effects stmt } in
  let nodes = loop_leaves body |> List.mapi ~f:node |> Array.of_list in
  let positions = List.range 0 (Array.length nodes) in
  let edges =
    List.concat_map positions ~f:(fun i ->
        self_edges ~related nodes.(i)
        @ List.concat_map
            (List.filter positions ~f:(fun j -> j > i))
            ~f:(fun j ->
              cross_edges ~related nodes.(i) nodes.(j)
              @ effect_edges nodes.(i) nodes.(j))) in
  {nodes; edges= dedup_edges edges}

(** The nodes reachable from [v] by one or more edges. *)
let reachable ~succs v =
  let rec visit seen w =
    if Set.Poly.mem w seen then seen
    else List.fold_left (succs w) ~init:(Set.Poly.add w seen) ~f:visit in
  List.fold_left (succs v) ~init:Set.Poly.empty ~f:visit

(** The pi-blocks of a graph (Allen and Kennedy 1987 §5.2): its strongly
    connected components, each listing its nodes in lexical order, in a
    topological order of the condensation with ties broken by the smallest
    lexical position. A body with only forward edges therefore comes back in
    original order. Two nodes share a component iff each reaches the other; a
    component is ready to be emitted once no component still waiting has an edge
    into it. *)
let pi_blocks (g : loop_dependence_graph) : int list list =
  let positions = List.range 0 (Array.length g.nodes) in
  let succs v =
    List.filter_map g.edges ~f:(fun e ->
        Option.some_if (e.src = v && e.dst <> v) e.dst) in
  let reach = Array.init (Array.length g.nodes) ~f:(reachable ~succs) in
  let mutually_reachable v w =
    v = w || (Set.Poly.mem w reach.(v) && Set.Poly.mem v reach.(w)) in
  (* one component per smallest member, members in lexical order *)
  let components =
    List.filter_map positions ~f:(fun v ->
        let members = List.filter positions ~f:(mutually_reachable v) in
        Option.some_if (List.hd members = Some v) members) in
  let has_edge_into b a =
    List.exists g.edges ~f:(fun e ->
        List.mem e.src ~set:a && List.mem e.dst ~set:b) in
  let first_pos c = List.hd c |> Option.value ~default:max_int in
  let rec emit waiting =
    let ready =
      List.filter waiting ~f:(fun b ->
          List.for_all waiting ~f:(fun a -> a = b || not (has_edge_into b a)))
    in
    match
      List.min_elt ready ~cmp:(fun a b -> compare (first_pos a) (first_pos b))
    with
    | None -> []
    | Some c -> c :: emit (List.filter waiting ~f:(fun c' -> c' <> c)) in
  emit components

(** A pi-block is cyclic iff it has more than one node or a self-edge; a cyclic
    block must stay a sequential loop. *)
let is_cyclic (g : loop_dependence_graph) (block : int list) : bool =
  match block with
  | [] -> false
  | [v] -> List.exists g.edges ~f:(fun e -> e.src = v && e.dst = v)
  | _ :: _ :: _ -> true

(** {2 Printers for the graph} *)

(** A statement on one line, truncated, for the report. *)
let pp_stmt_one_line ppf (s : Stmt.Located.t) =
  let unbroken =
    Format.asprintf "%t" (fun ppf ->
        Format.pp_set_margin ppf 100_000;
        Stmt.Located.pp ppf s) in
  let one_line =
    String.split_on_char ~sep:'\n' unbroken
    |> List.map ~f:String.trim |> String.concat ~sep:" " in
  let limit = 96 in
  if String.length one_line > limit then
    Fmt.pf ppf "%s..." (String.sub one_line ~pos:0 ~len:limit)
  else Fmt.string ppf one_line

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
