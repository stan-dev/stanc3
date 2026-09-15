open Std
open Middle
open Dataflow_types
open Mir_utils
open Dataflow_utils
open Monotone_framework_sigs
open Monotone_framework

(***********************************)
(* Dependency analysis & interface *)
(***********************************)

type node_dep_info =
  { predecessors: label Set.Poly.t
  ; parents: label Set.Poly.t
  ; reaching_defn_entry: reaching_defn Set.Poly.t
  ; reaching_defn_exit: reaching_defn Set.Poly.t
  ; meta: Location_span.t }

(** Find all of the reaching definitions of a variable in an RD set *)
let reaching_defn_lookup (rds : reaching_defn Set.Poly.t) (var : vexpr) :
    label Set.Poly.t =
  Set.Poly.map (Set.Poly.filter rds ~f:(fun (var', _) -> var' = var)) ~f:snd

let node_immediate_dependencies
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    ?(blockers : vexpr Set.Poly.t = Set.Poly.empty) (label : label) :
    label Set.Poly.t =
  let stmt, info = LabelMap.find label statement_map in
  let rhs_set = Set.Poly.map (stmt_rhs_var_set stmt) ~f:fst in
  let rhs_deps =
    Set.Poly.union_map
      (Set.Poly.diff rhs_set blockers)
      ~f:(reaching_defn_lookup info.reaching_defn_entry) in
  Set.Poly.union info.parents rhs_deps

(* This is doing an explicit graph traversal with edges defined by
   node_immediate_dependencies. *)
let rec node_dependencies_rec
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    ?(blockers : vexpr Set.Poly.t = Set.Poly.empty) (label : label)
    (visited : label Set.Poly.t) : label Set.Poly.t =
  if Set.Poly.mem label visited then visited
  else
    let visited' = Set.Poly.add label visited in
    let deps = node_immediate_dependencies statement_map ~blockers label in
    Set.Poly.fold deps ~init:visited' ~f:(node_dependencies_rec statement_map)

let node_dependencies
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    (label : label) : label Set.Poly.t =
  node_dependencies_rec statement_map label Set.Poly.empty

let node_vars_dependencies
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    ?(blockers : vexpr Set.Poly.t = Set.Poly.empty) (vars : vexpr Set.Poly.t)
    (label : label) : label Set.Poly.t =
  let _, info = LabelMap.find label statement_map in
  let var_deps =
    Set.Poly.union_map
      (Set.Poly.diff vars blockers)
      ~f:(reaching_defn_lookup info.reaching_defn_entry) in
  Set.Poly.fold
    (Set.Poly.union info.parents var_deps)
    ~init:Set.Poly.empty
    ~f:(node_dependencies_rec statement_map ~blockers)

(* The strategy here is to write an update function on the whole dependency
   graph in terms of node_immediate_dependencies, and then to find a
   fixed-point. Since it's updating the dependencies for the whole graph at a
   time, it should be more efficient than doing a graph traversal for each
   node. *)
let all_node_dependencies
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t) :
    label Set.Poly.t LabelMap.t =
  let immediate_map =
    LabelMap.mapi statement_map ~f:(fun label _ ->
        node_immediate_dependencies statement_map label) in
  let step_node label m =
    let immediate = LabelMap.find label immediate_map in
    let updated =
      Set.Poly.union
        (Set.Poly.union_map immediate ~f:(fun label -> LabelMap.find label m))
        immediate in
    Set.Poly.remove label updated in
  let step_map m = LabelMap.mapi m ~f:(fun label _ -> step_node label m) in
  let map_equal = LabelMap.equal ~cmp:Set.Poly.equal in
  let rec step_until_fixed m =
    let m' = step_map m in
    if map_equal m m' then m else step_until_fixed m' in
  step_until_fixed immediate_map

let mir_reaching_definitions (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    reaching_defn Set.Poly.t entry_exit LabelMap.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_stmt stmt in
  let (module Flowgraph) = flowgraph in
  let rd_map =
    reaching_definitions_mfp mir (module Flowgraph) flowgraph_to_mir in
  let to_rd_set set =
    Set.Poly.map set ~f:(fun (s, label_opt) ->
        (VVar s, Option.value label_opt ~default:1)) in
  LabelMap.map rd_map ~f:(fun {entry; exit} ->
      {entry= to_rd_set entry; exit= to_rd_set exit})

let all_labels
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int) : int Set.Poly.t =
  let step set =
    Set.Poly.union set
      (Set.Poly.union_map set ~f:(fun l -> LabelMap.find l Flowgraph.successors))
  in
  let rec step_fix set =
    let next = step set in
    if Set.Poly.equal set next then set else step_fix next in
  step_fix Flowgraph.initials

let prog_rhs_variables
    (flowgraph_to_mir : Stmt.Located.Non_recursive.t LabelMap.t)
    (labels : int Set.Poly.t) : string Set.Poly.t =
  let label_vars label =
    Set.Poly.map
      ~f:(fun (VVar s, _) -> s)
      (stmt_rhs_var_set (LabelMap.find label flowgraph_to_mir).pattern) in
  Set.Poly.union_map labels ~f:label_vars

let stmt_uninitialized_variables (exceptions : string Set.Poly.t)
    (stmt : Stmt.Located.t) : (Location_span.t * string) Set.Poly.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_stmt ~flatten_loops:true stmt in
  let (module Flowgraph) = flowgraph in
  let labels = all_labels (module Flowgraph) in
  let all_variables = prog_rhs_variables flowgraph_to_mir labels in
  let initialized_vars_map =
    initialized_vars_mfp all_variables (module Flowgraph) flowgraph_to_mir in
  let uninitialized =
    LabelMap.fold initialized_vars_map ~init:Set.Poly.empty
      ~f:(fun ~key:label ~data:inits acc ->
        let stmt = LabelMap.find label flowgraph_to_mir in
        let rhs =
          Set.Poly.map
            ~f:(fun (VVar s, Expr.Typed.Meta.{loc; _}) -> (loc, s))
            (stmt_rhs_var_set stmt.pattern) in
        let uninitialized (_, var) = not (Set.Poly.mem var inits.entry) in
        let uninitialized_set = Set.Poly.filter ~f:uninitialized rhs in
        Set.Poly.union acc uninitialized_set) in
  Set.Poly.filter uninitialized ~f:(fun (_, v) ->
      not (Set.Poly.mem v exceptions))

let mir_uninitialized_variables (mir : Program.Typed.t) :
    (Location_span.t * string) Set.Poly.t =
  let flag_variables = List.map ~f:Flag_vars.to_string Flag_vars.enumerate in
  let function_names = function_names mir in
  let data_vars = data_set ~exclude_transformed:true mir in
  let globals =
    Set.Poly.union function_names (Set.Poly.of_list flag_variables) in
  let parameters =
    Set.Poly.of_list
      (List.map ~f:fst3
         (List.filter
            ~f:(fun (_, _, Program.{out_block; _}) -> out_block = Parameters)
            mir.output_vars)) in
  let globals_data = Set.Poly.union globals data_vars in
  let check_against_data b =
    stmt_uninitialized_variables globals_data
      {pattern= SList b; meta= Location_span.empty} in
  let check_against_data_params b =
    let globals_data_prep = Set.Poly.union globals_data parameters in
    stmt_uninitialized_variables globals_data_prep
      (* prepend prepare_data to detect bad transformed data usages *)
      {pattern= SList (mir.prepare_data @ b); meta= Location_span.empty} in
  Set.Poly.union_list
    [ (* prepare_data scope: data *)
      check_against_data mir.prepare_data
      (* log_prob scope: data, prep declarations *)
    ; check_against_data_params mir.log_prob
    ; check_against_data_params mir.reverse_mode_log_prob
      (* gen quant scope: data, prep declarations *)
    ; check_against_data_params mir.generate_quantities
      (* functions scope: arguments *)
    ; Set.Poly.union_list
        (List.map mir.functions_block ~f:(fun Program.{fdbody; fdargs; _} ->
             let arg_vars =
               Set.Poly.of_list
                 (List.map fdargs ~f:(fun (_, arg_name, _) -> arg_name)) in
             Option.value_map fdbody ~default:Set.Poly.empty ~f:(fun fdbody ->
                 stmt_uninitialized_variables
                   (Set.Poly.union arg_vars globals)
                   fdbody))) ]

let build_dep_info_map (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t =
  let statement_map =
    build_statement_map
      (fun Stmt.{pattern; _} -> pattern)
      (fun Stmt.{meta; _} -> meta)
      stmt in
  let _, preds, parents = build_cf_graphs statement_map in
  let rd_map = mir_reaching_definitions mir stmt in
  LabelMap.mapi statement_map ~f:(fun label (stmt, meta) ->
      let rds = LabelMap.find label rd_map in
      ( stmt
      , { predecessors= LabelMap.find label preds
        ; parents= LabelMap.find label parents
        ; reaching_defn_entry= rds.entry
        ; reaching_defn_exit= rds.exit
        ; meta } ))

let log_prob_build_dep_info_map (mir : Program.Typed.t) :
    ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t =
  let log_prob_stmt =
    Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dep_info_map mir log_prob_stmt

let log_prob_dependency_graph (mir : Program.Typed.t) :
    label Set.Poly.t LabelMap.t =
  let dep_info_map = log_prob_build_dep_info_map mir in
  all_node_dependencies dep_info_map

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
    contribute no access to ["target"]: increments are treated as a reduction
    (design §7.4). Reads of [loopvar] itself are not recorded: it is defined by
    the loop header, so it can never carry a dependence. *)
let stmt_accesses ~loopvar ~written ~label
    (stmt : (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t) : access list =
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
    | FunApp (kind, args) ->
        List.concat_map (Fun_kind.collect_exprs kind @ args) ~f:expr_reads
    | TernaryIf (a, b, c) -> List.concat_map [a; b; c] ~f:expr_reads
    | EAnd (a, b) | EOr (a, b) -> expr_reads a @ expr_reads b
    | Promotion (e, _, _) | TupleProjection (e, _) -> expr_reads e
  and index_reads idcs =
    List.concat_map idcs ~f:(fun idx ->
        List.concat_map (Index.bounds idx) ~f:expr_reads) in
  let rec stmt_reads_writes (s : (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t)
      : access list =
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
    | TargetPE e | JacobianPE e | Return (Some e) -> expr_reads e
    | NRFunApp (kind, args) ->
        List.concat_map (Fun_kind.collect_exprs kind @ args) ~f:expr_reads
    | IfElse (cond, s1, s2) ->
        expr_reads cond @ sub s1 @ Option.value_map s2 ~default:[] ~f:sub
    | While (cond, body) -> expr_reads cond @ sub body
    | For {loopvar= inner; lower; upper; body} ->
        (mk inner [] true :: expr_reads lower) @ expr_reads upper @ sub body
    | Profile (_, stmts) | Block stmts | SList stmts ->
        List.concat_map stmts ~f:sub
    | Break | Continue | Skip | Return None -> []
  and sub (s : Stmt.Located.t) = stmt_reads_writes s.pattern in
  stmt_reads_writes stmt

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

(** [independent], or [dependent {<,=,>}] with [distance d] when known. *)
let pp_dependence ppf = function
  | Independent -> Fmt.string ppf "independent"
  | Dependent {directions; distance} ->
      Fmt.pf ppf "dependent {%a}"
        Fmt.(list ~sep:(any ",") pp_direction)
        (Set.Poly.to_list directions);
      Option.iter distance ~f:(Fmt.pf ppf " distance %d")
