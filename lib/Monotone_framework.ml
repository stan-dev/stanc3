(** The common elements of a monotone framework *)

open Core_kernel
open Monotone_framework_sigs
open Dataflow_utils
open Mir_utils

(** Compute the inverse flowgraph of a Stan statement (for reverse analyses) *)
let inverse_flowgraph_of_stmt (stmt : Mir.stmt_loc) :
    (module FLOWGRAPH with type labels = int)
    * (int, Mir.stmt_loc_num) Map.Poly.t =
  let flowgraph_to_mir =
    Dataflow_utils.build_statement_map
      (fun x -> x.Mir.stmt)
      (fun x -> x.Mir.sloc)
      stmt
  in
  let initials, successors =
    Dataflow_utils.build_predecessor_graph flowgraph_to_mir
  in
  ( ( module struct
      type labels = int
      type t = labels

      let compare = Int.compare
      let hash = Int.hash
      let sexp_of_t = Int.sexp_of_t
      let initials = initials
      let successors = successors
    end
    : FLOWGRAPH
      with type labels = int )
  , Map.Poly.map ~f:(fun (stmtn, slocn) -> {Mir.stmtn; slocn}) flowgraph_to_mir
  )

(** Reverse flowgraphs to be used for reverse analyses.
    Observe that this respects the invariants listed for a FLOWGRAPH *)
let reverse (type l) (module F : FLOWGRAPH with type labels = l) =
  ( module struct
    type labels = F.labels
    type t = labels

    let compare = F.compare
    let hash = F.hash
    let sexp_of_t = F.sexp_of_t
    let initials = Set.of_map_keys (Map.filter F.successors ~f:Set.is_empty)

    let successors =
      Map.fold F.successors
        ~init:(Map.map F.successors ~f:(fun _ -> Set.Poly.empty))
        ~f:(fun ~key:old_pred ~data:old_succs accum ->
          Set.fold old_succs ~init:accum ~f:(fun accum old_succ ->
              Map.set accum ~key:old_succ
                ~data:(Set.add (Map.find_exn accum old_succ) old_pred) ) )
  end
  : FLOWGRAPH
    with type labels = l )

(** Compute the forward flowgraph of a Stan statement (for forward analyses) *)
let forward_flowgraph_of_stmt stmt =
  let inv_flowgraph = inverse_flowgraph_of_stmt stmt in
  (reverse (fst inv_flowgraph), snd inv_flowgraph)

(**  The lattice of sets of some values, with the inclusion order, set union
     and the empty set *)
let powerset_lattice (type v) (module S : INITIALTYPE with type vals = v) =
  ( module struct
    type properties = S.vals Set.Poly.t

    let bottom = Set.Poly.empty
    let lub s1 s2 = Set.Poly.union s1 s2
    let leq s1 s2 = Set.Poly.is_subset s1 ~of_:s2
    let initial = S.initial
  end
  : LATTICE
    with type properties = v Set.Poly.t )

(**  The lattice of subsets of some set, with the inverse inclusion order,
     set intersection and the total set *)
let dual_powerset_lattice (type v)
    (module S : INITIALTOTALTYPE with type vals = v) =
  ( module struct
    type properties = S.vals Set.Poly.t

    let bottom = S.total
    let lub s1 s2 = Set.Poly.inter s1 s2
    let leq s1 s2 = Set.Poly.is_subset s2 ~of_:s1
    let initial = S.initial
  end
  : LATTICE
    with type properties = v Set.Poly.t )

let powerset_lattice_expressions (initial : Mir.ExprSet.t) =
  ( module struct
    type properties = Mir.ExprSet.t

    let bottom = Mir.ExprSet.empty
    let lub s1 s2 = Mir.ExprSet.union s1 s2
    let leq s1 s2 = Mir.ExprSet.is_subset s1 ~of_:s2
    let initial = initial
  end
  : LATTICE
    with type properties = Mir.ExprSet.t )

let dual_powerset_lattice_expressions (initial : Mir.ExprSet.t)
    (total : Mir.ExprSet.t) =
  ( module struct
    type properties = Mir.ExprSet.t

    let bottom = total
    let lub s1 s2 = Mir.ExprSet.inter s1 s2
    let leq s1 s2 = Mir.ExprSet.is_subset s2 ~of_:s1
    let initial = initial
  end
  : LATTICE
    with type properties = Mir.ExprSet.t )

(**  Add a fresh bottom element to a lattice (possibly without bottom) *)
let new_bot (type p) (module L : LATTICE_NO_BOT with type properties = p) =
  ( module struct
    type properties = L.properties option

    let bottom = None

    let lub = function
      | Some s1 -> (
          function Some s2 -> Some (L.lub s1 s2) | None -> Some s1 )
      | None -> fun x -> x

    let leq = function
      | Some s1 -> ( function Some s2 -> L.leq s1 s2 | None -> false )
      | None -> fun _ -> true

    let initial = Some L.initial
  end
  : LATTICE
    with type properties = p option )

(** The lattice (without bottom) of partial functions, ordered under
    inverse graph inclusion, with intersection *)
let dual_partial_function_lattice (type dv cv)
    (module Dom : TOTALTYPE with type vals = dv)
    (module Codom : TYPE with type vals = cv) =
  ( module struct
    type properties = (Dom.vals, Codom.vals) Map.Poly.t

    let lub s1 s2 =
      let f ~key ~data = Map.find s2 key = Some data in
      Map.filteri ~f s1

    let leq s1 s2 =
      Set.for_all Dom.total ~f:(fun k ->
          match (Map.find s1 k, Map.find s2 k) with
          | Some x, Some y -> x = y
          | Some _, None | None, None -> true
          | None, Some _ -> false )

    let initial = Map.Poly.empty
  end
  : LATTICE_NO_BOT
    with type properties = (dv, cv) Map.Poly.t )

(* The lattice of partial functions, where we add a fresh bottom element,
   to represent an inconsistent combination of functions *)
let dual_partial_function_lattice_with_bot (type dv cv)
    (module Dom : TOTALTYPE with type vals = dv)
    (module Codom : TYPE with type vals = cv) =
  new_bot (dual_partial_function_lattice (module Dom) (module Codom))

(* A dual powerset lattice, where we set the initial set to be empty *)
let dual_powerset_lattice_empty_initial (type v)
    (module T : TOTALTYPE with type vals = v) =
  dual_powerset_lattice
    ( module struct
      type vals = T.vals

      let initial = Set.Poly.empty
      let total = T.total
    end )

(* A powerset lattice, where we set the initial set to be empty *)
let powerset_lattice_empty_initial (type v)
    (module T : TYPE with type vals = v) =
  powerset_lattice
    (module struct type vals = T.vals

                   let initial = Set.Poly.empty end)

(* The specific powerset lattice we use for reaching definitions analysis *)
let reaching_definitions_lattice (type v l)
    (module Variables : INITIALTYPE with type vals = v)
    (module Labels : TYPE with type vals = l) =
  powerset_lattice
    ( module struct
      type vals = Variables.vals * Labels.vals option

      let initial = Set.Poly.map ~f:(fun x -> (x, None)) Variables.initial
    end )

(* The transfer function for a constant propagation analysis *)
let constant_propagation_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc_num) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string, Mir.expr_typed_located) Map.Poly.t option

    let transfer_function l p =
      match p with
      | None -> None
      | Some m ->
          let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
          Some
            ( match mir_node with
            (* TODO: we are currently only propagating constants for scalars.
             We could do the same for matrix and array expressions if we wanted. *)
            | Mir.Assignment ({texpr= Var s; _}, e) -> (
              match Partial_evaluator.eval_expr (subst_expr m e) with
              | {texpr= Mir.Lit (_, _); _} as e' -> Map.set m ~key:s ~data:e'
              | _ -> Map.remove m s )
            | Mir.Decl {decl_id= s; _}
             |Mir.Assignment ({texpr= Indexed ({texpr= Var s; _}, _); _}, _) ->
                Map.remove m s
            | Mir.Assignment (_, _) -> Errors.fatal_error ()
            | Mir.TargetPE _
             |Mir.NRFunApp (_, _)
             |Mir.Check _ | Mir.Break | Mir.Continue | Mir.Return _ | Mir.Skip
             |Mir.IfElse (_, _, _)
             |Mir.While (_, _)
             |Mir.For _ | Mir.Block _ | Mir.SList _ | Mir.FunDef _ ->
                m )
  end
  : TRANSFER_FUNCTION
    with type labels = int
     and type properties = (string, Mir.expr_typed_located) Map.Poly.t option
  )

(** The transfer function for an expression propagation analysis,
    AKA forward substitution (see page 396 of Muchnick) *)
let expression_propagation_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc_num) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string, Mir.expr_typed_located) Map.Poly.t option

    let transfer_function l p =
      match p with
      | None -> None
      | Some m ->
          let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
          Some
            ( match mir_node with
            (* TODO: we are currently only propagating constants for scalars.
             We could do the same for matrix and array expressions if we wanted. *)
            | Mir.Assignment ({texpr= Var s; _}, e) ->
                Map.set m ~key:s ~data:(subst_expr m e)
            | Mir.Decl {decl_id= s; _}
             |Mir.Assignment ({texpr= Indexed ({texpr= Var s; _}, _); _}, _) ->
                Map.remove m s
            | Mir.Assignment (_, _) -> Errors.fatal_error ()
            | Mir.TargetPE _
             |Mir.NRFunApp (_, _)
             |Mir.Check _ | Mir.Break | Mir.Continue | Mir.Return _ | Mir.Skip
             |Mir.IfElse (_, _, _)
             |Mir.While (_, _)
             |Mir.For _ | Mir.Block _ | Mir.SList _ | Mir.FunDef _ ->
                m )
  end
  : TRANSFER_FUNCTION
    with type labels = int
     and type properties = (string, Mir.expr_typed_located) Map.Poly.t option
  )

(** The transfer function for a copy propagation analysis *)
let copy_propagation_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc_num) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string, Mir.expr_typed_located) Map.Poly.t option

    let transfer_function l p =
      match p with
      | None -> None
      | Some m ->
          let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
          Some
            ( match mir_node with
            | Mir.Assignment
                ( {texpr= Mir.Var s; _}
                , {texpr= Mir.Var t; texpr_type; texpr_loc; texpr_adlevel} ) ->
                Map.set m ~key:s
                  ~data:
                    {Mir.texpr= Mir.Var t; texpr_type; texpr_loc; texpr_adlevel}
            | Mir.Decl {decl_id= s; _} | Mir.Assignment ({texpr= Var s; _}, _)
              ->
                Map.remove m s
            | Mir.Assignment (_, _)
             |Mir.TargetPE _
             |Mir.NRFunApp (_, _)
             |Mir.Check _ | Mir.Break | Mir.Continue | Mir.Return _ | Mir.Skip
             |Mir.IfElse (_, _, _)
             |Mir.While (_, _)
             |Mir.For _ | Mir.Block _ | Mir.SList _ | Mir.FunDef _ ->
                m )
  end
  : TRANSFER_FUNCTION
    with type labels = int
     and type properties = (string, Mir.expr_typed_located) Map.Poly.t option
  )

(** A helper function for building transfer functions from gen and kill sets *)
let transfer_gen_kill p gen kill = Set.union gen (Set.diff p kill)

(* TODO: from here *)

(** Calculate the set of variables that a statement can assign to or declare *)
let assigned_or_declared_vars_stmt
    (s : (Mir.expr_typed_located, 'a) Mir.statement) =
  match s with
  | Mir.Assignment ({texpr= Var x; _}, _)
   |Mir.Assignment ({texpr= Indexed ({texpr= Var x; _}, _); _}, _)
   |Mir.Decl {decl_id= x; _}
   |Mir.FunDef {fdname= x; _} ->
      Set.Poly.singleton x
  | Mir.Assignment (_, _) -> Errors.fatal_error ()
  | Mir.TargetPE _ -> Set.Poly.singleton "target"
  | Mir.NRFunApp (s, _) when String.suffix s 3 = "_lp" ->
      Set.Poly.singleton "target"
  | Mir.NRFunApp (_, _)
   |Mir.Check _ | Mir.Break | Mir.Continue | Mir.Return _ | Mir.Skip
   |Mir.IfElse (_, _, _)
   |Mir.While (_, _)
   |Mir.For _ | Mir.Block _ | Mir.SList _ ->
      Set.Poly.empty

(** The transfer function for a reaching definitions analysis *)
let reaching_definitions_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc_num) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string * labels option) Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
      let gen =
        Set.Poly.map
          ~f:(fun x -> (x, Some l))
          (assigned_or_declared_vars_stmt mir_node)
      in
      let kill =
        match mir_node with
        | Mir.Decl {decl_id= x; _}
         |Mir.Assignment ({texpr= Var x; _}, _)
         |Mir.Assignment ({texpr= Indexed ({texpr= Var x; _}, _); _}, _)
         |Mir.FunDef {fdname= x; _} ->
            Set.filter p ~f:(fun (y, _) -> y = x)
        | Mir.Assignment (_, _) -> Errors.fatal_error ()
        | Mir.TargetPE _ -> Set.filter p ~f:(fun (y, _) -> y = "target")
        | Mir.NRFunApp (s, _) when String.suffix s 3 = "_lp" ->
            Set.filter p ~f:(fun (y, _) -> y = "target")
        | Mir.NRFunApp (_, _)
         |Mir.Check _ | Mir.Break | Mir.Continue | Mir.Return _ | Mir.Skip
         |Mir.IfElse (_, _, _)
         |Mir.While (_, _)
         |Mir.For _ | Mir.Block _ | Mir.SList _ ->
            Set.Poly.empty
      in
      transfer_gen_kill p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int
     and type properties = (string * int option) Set.Poly.t )

(** Calculate the free (non-bound) variables in an expression *)
let rec free_vars_expr (e : Mir.expr_typed_located) =
  match e.texpr with
  | Mir.Var x -> Set.Poly.singleton x
  | Mir.Lit (_, _) -> Set.Poly.empty
  | Mir.FunApp (f, l) ->
      Set.Poly.union_list (Set.Poly.singleton f :: List.map ~f:free_vars_expr l)
  | Mir.TernaryIf (e1, e2, e3) ->
      Set.Poly.union_list (List.map ~f:free_vars_expr [e1; e2; e3])
  | Mir.Indexed (e, l) ->
      Set.Poly.union_list (free_vars_expr e :: List.map ~f:free_vars_idx l)

(** Calculate the free (non-bound) variables in an index*)
and free_vars_idx (i : Mir.expr_typed_located Mir.index) =
  match i with
  | Mir.All -> Set.Poly.empty
  | Mir.Single e | Mir.Upfrom e | Mir.Downfrom e | Mir.MultiIndex e ->
      free_vars_expr e
  | Mir.Between (e1, e2) ->
      Set.Poly.union (free_vars_expr e1) (free_vars_expr e2)

(** Calculate the free (non-bound) variables in a statement *)
let rec free_vars_stmt
    (s : (Mir.expr_typed_located, Mir.stmt_loc) Mir.statement) =
  match s with
  | Mir.Assignment ({texpr= Var _; _}, e)
   |Mir.Return (Some e)
   |Mir.TargetPE e ->
      free_vars_expr e
  | Mir.Assignment ({texpr= Indexed ({texpr= Var _; _}, l); _}, e) ->
      Set.Poly.union_list (free_vars_expr e :: List.map ~f:free_vars_idx l)
  | Mir.Assignment _ -> Errors.fatal_error ()
  | Mir.NRFunApp (f, l) ->
      Set.Poly.union_list (Set.Poly.singleton f :: List.map ~f:free_vars_expr l)
  | Mir.Check (f, l) ->
      Set.Poly.union_list (Set.Poly.singleton f :: List.map ~f:free_vars_expr l)
  | Mir.IfElse (e, b1, Some b2) ->
      Set.Poly.union_list
        [free_vars_expr e; free_vars_stmt b1.stmt; free_vars_stmt b2.stmt]
  | Mir.IfElse (e, b, None) | Mir.While (e, b) ->
      Set.Poly.union (free_vars_expr e) (free_vars_stmt b.stmt)
  | Mir.For {lower= e1; upper= e2; body= b; _} ->
      Set.Poly.union_list
        [free_vars_expr e1; free_vars_expr e2; free_vars_stmt b.stmt]
  | Mir.Block l | Mir.SList l ->
      Set.Poly.union_list (List.map ~f:(fun s -> free_vars_stmt s.stmt) l)
  | Mir.Decl _ | Mir.Break | Mir.Continue | Mir.Return None | Mir.Skip ->
      Set.Poly.empty
  | Mir.FunDef {fdargs= l; fdbody= b; _} ->
      Set.Poly.diff (free_vars_stmt b.stmt)
        (Set.Poly.of_list (List.map ~f:(fun (_, s, _) -> s) l))

(** A variation on free_vars_stmt, where we do not recursively count free
    variables in sub statements  *)
let top_free_vars_stmt (flowgraph_to_mir : (int, Mir.stmt_loc_num) Map.Poly.t)
    (s : (Mir.expr_typed_located, int) Mir.statement) =
  match s with
  | Mir.Assignment _ | Mir.Return _ | Mir.TargetPE _ | Mir.Check _
   |Mir.NRFunApp _ | Mir.FunDef _ | Mir.Decl _ | Mir.Break | Mir.Continue
   |Mir.Skip ->
      free_vars_stmt
        (Mir.statement_stmt_loc_of_statement_stmt_loc_num flowgraph_to_mir s)
  | Mir.While (e, _) | Mir.IfElse (e, _, _) -> free_vars_expr e
  | Mir.For {lower= e1; upper= e2; _} ->
      Set.Poly.union_list [free_vars_expr e1; free_vars_expr e2]
  | Mir.Block _ | Mir.SList _ -> Set.Poly.empty

(** The transfer function for a live variables analysis *)
let live_variables_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc_num) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = string Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
      let gen = top_free_vars_stmt flowgraph_to_mir mir_node in
      let kill =
        match mir_node with
        | Mir.Assignment ({texpr= Var x; _}, _) | Mir.Decl {decl_id= x; _} ->
            Set.Poly.singleton x
        | Mir.TargetPE _
         |Mir.NRFunApp (_, _)
         |Mir.Check _ | Mir.Break | Mir.Continue | Mir.Return _ | Mir.Skip
         |Mir.IfElse (_, _, _)
         |Mir.While (_, _)
         |Mir.For _ | Mir.Block _ | Mir.SList _ | Mir.FunDef _
         |Mir.Assignment ({texpr= Indexed ({texpr= Var _; _}, _); _}, _) ->
            Set.Poly.empty
        | Mir.Assignment _ ->
            raise_s
              [%sexp (mir_node : (Mir.expr_typed_located, int) Mir.statement)]
      in
      transfer_gen_kill p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int and type properties = string Set.Poly.t )

(** Calculate the set of sub-expressions of an expression *)
let rec used_subexpressions_expr (e : Mir.expr_typed_located) =
  Mir.ExprSet.union (Mir.ExprSet.singleton e)
    ( match e.texpr with
    | Mir.Var _ | Mir.Lit (_, _) -> Mir.ExprSet.empty
    | Mir.FunApp (_, l) ->
        Mir.ExprSet.union_list (List.map ~f:used_subexpressions_expr l)
    | Mir.TernaryIf (e1, e2, e3) ->
        Mir.ExprSet.union_list
          [ used_subexpressions_expr e1
          ; used_subexpressions_expr e2
          ; used_subexpressions_expr e3 ]
    | Mir.Indexed (e, l) ->
        Mir.ExprSet.union_list
          ( used_subexpressions_expr e
          :: List.map ~f:(used_expressions_idx_help used_subexpressions_expr) l
          ) )

and used_expressions_idx_help f (i : Mir.expr_typed_located Mir.index) =
  match i with
  | Mir.All -> Mir.ExprSet.empty
  | Mir.Single e | Mir.Upfrom e | Mir.Downfrom e | Mir.MultiIndex e -> f e
  | Mir.Between (e1, e2) -> Mir.ExprSet.union (f e1) (f e2)

(** Calculate the set of expressions of an expression *)
let used_expressions_expr e = Mir.ExprSet.singleton e

let rec used_expressions_stmt_help f
    (s : (Mir.expr_typed_located, Mir.stmt_loc) Mir.statement) =
  match s with
  | Mir.Assignment ({texpr= Var _; _}, e)
   |Mir.TargetPE e
   |Mir.Return (Some e) ->
      f e
  | Mir.Assignment ({texpr= Indexed ({texpr= Var _; _}, l); _}, e) ->
      Mir.ExprSet.union (f e)
        (Mir.ExprSet.union_list (List.map ~f:(used_expressions_idx_help f) l))
  | Mir.Assignment _ -> Errors.fatal_error ()
  | Mir.IfElse (e, b1, Some b2) ->
      Mir.ExprSet.union_list
        [ f e
        ; used_expressions_stmt_help f b1.stmt
        ; used_expressions_stmt_help f b2.stmt ]
  | Mir.NRFunApp (_, l) | Mir.Check (_, l) ->
      Mir.ExprSet.union_list (List.map ~f l)
  | Mir.Decl _
   |Mir.Return None
   |Mir.Break | Mir.Continue | Mir.FunDef _ | Mir.Skip ->
      Mir.ExprSet.empty
  | Mir.IfElse (e, b, None) | Mir.While (e, b) ->
      Mir.ExprSet.union (f e) (used_expressions_stmt_help f b.stmt)
  | Mir.For {lower= e1; upper= e2; body= b; loopvar= s} ->
      Mir.ExprSet.union_list
        [ f e1; f e2
        ; used_expressions_stmt_help f b.stmt
        ; Mir.ExprSet.singleton
            { Mir.texpr= Var s
            ; texpr_type= UInt
            ; texpr_adlevel= DataOnly
            ; texpr_loc= Mir.no_span } ]
  | Mir.Block l | Mir.SList l ->
      Mir.ExprSet.union_list
        (List.map ~f:(fun s -> used_expressions_stmt_help f s.stmt) l)

(** Calculate the set of sub-expressions in a statement *)
let used_subexpressions_stmt =
  used_expressions_stmt_help used_subexpressions_expr

(** Calculate the set of expressions in a statement *)
let used_expressions_stmt = used_expressions_stmt_help used_expressions_expr

let top_used_expressions_stmt_help f
    (s : (Mir.expr_typed_located, int) Mir.statement) =
  match s with
  | Mir.Assignment ({texpr= Var _; _}, e)
   |Mir.TargetPE e
   |Mir.Return (Some e) ->
      f e
  | Mir.Assignment ({texpr= Indexed ({texpr= Var _; _}, l); _}, e) ->
      Mir.ExprSet.union (f e)
        (Mir.ExprSet.union_list (List.map ~f:(used_expressions_idx_help f) l))
  | Mir.Assignment _ -> Errors.fatal_error ()
  | Mir.While (e, _) | Mir.IfElse (e, _, _) -> f e
  | Mir.NRFunApp (_, l) | Mir.Check (_, l) ->
      Mir.ExprSet.union_list (List.map ~f l)
  | Mir.Block _ | Mir.SList _ | Mir.Decl _
   |Mir.Return None
   |Mir.Break | Mir.Continue | Mir.FunDef _ | Mir.Skip ->
      Mir.ExprSet.empty
  | Mir.For {lower= e1; upper= e2; _} -> Mir.ExprSet.union_list [f e1; f e2]

(** Calculate the set of sub-expressions at the top level in a statement *)
let top_used_subexpressions_stmt =
  top_used_expressions_stmt_help used_subexpressions_expr

(** Calculate the set of expressions at the top level in a statement *)
let top_used_expressions_stmt =
  top_used_expressions_stmt_help used_expressions_expr

(** Calculate the subset (of p) of expressions that will need to be recomputed as a
    consequence of evaluating the statement s (because of writes to variables performed
    by s) *)
let killed_expressions_stmt (p : Mir.ExprSet.t)
    (s : (Mir.expr_typed_located, int) Mir.statement) =
  Mir.ExprSet.filter p ~f:(fun e ->
      let free_vars = free_vars_expr e in
      (* Note: a simple test for membership would be more efficient here,
         but it would require us to duplicate some code. *)
      let assigned_vars = assigned_or_declared_vars_stmt s in
      not (Set.Poly.is_empty (Set.Poly.inter free_vars assigned_vars)) )

(** Calculate the set of subexpressions that needs to be computed at each node
    in the flowgraph *)
let used (flowgraph_to_mir : (int, Mir.stmt_loc_num) Map.Poly.t) =
  Map.Poly.fold flowgraph_to_mir ~init:Map.Poly.empty
    ~f:(fun ~key ~data accum ->
      Map.Poly.set accum ~key ~data:(top_used_subexpressions_stmt data.stmtn)
  )

(* TODO: figure out whether we will also want to reuse the computation of killed *)

(** The transfer function for an anticipated expressions analysis (as a part of lazy
    code motion) *)
let anticipated_expressions_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc_num) Map.Poly.t)
    (used : (int, Mir.ExprSet.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Mir.ExprSet.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
      let gen = Map.Poly.find_exn used l in
      let kill = killed_expressions_stmt p mir_node in
      transfer_gen_kill p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int and type properties = Mir.ExprSet.t )

(** A helper function for defining transfer functions in terms of gen and kill sets
    in an alternative way, that is used in some of the subanalyses of lazy code motion *)
let transfer_gen_kill_alt p gen kill = Set.diff (Set.union p gen) kill

(* NOTE: we want to implement lazy code motion. Aho describes a slightly
   more general available expression pass for that that uses the anticipated
   expression pass.
   QUESTION: does this give the traditional available expressions analysis if
   anticipated expressions is empty? *)

(** An available expressions analysis, to be used in lazy code motion *)
let available_expressions_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc_num) Map.Poly.t)
    (anticipated_expressions : (int, Mir.ExprSet.t entry_exit) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Mir.ExprSet.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
      let gen = (Map.find_exn anticipated_expressions l).exit in
      let kill = killed_expressions_stmt (Mir.ExprSet.union p gen) mir_node in
      transfer_gen_kill_alt p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int and type properties = Mir.ExprSet.t )

(** Calculates the set of expressions that can be calculated for the first time
    at each node in the flow graph *)
let earliest
    (anticipated_expressions : (int, Mir.ExprSet.t entry_exit) Map.Poly.t)
    (available_expressions : (int, Mir.ExprSet.t entry_exit) Map.Poly.t) =
  Map.fold anticipated_expressions ~init:Map.Poly.empty
    ~f:(fun ~key ~data accum ->
      Map.set accum ~key
        ~data:
          (Set.diff data.exit (Map.find_exn available_expressions key).entry)
  )

(** The transfer function for a postponable expressions analysis (as a part of lazy code motion) *)
let postponable_expressions_transfer
    (earliest : (int, Mir.ExprSet.t) Map.Poly.t)
    (used : (int, Mir.ExprSet.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Mir.ExprSet.t

    let transfer_function l p =
      let gen = Map.find_exn earliest l in
      let kill = Map.find_exn used l in
      transfer_gen_kill_alt p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int and type properties = Mir.ExprSet.t )

(** Calculates the set of expressions that can be computed at the latest at each node *)
let latest (successors : (int, int Set.Poly.t) Map.Poly.t)
    (earliest : (int, Mir.ExprSet.t) Map.Poly.t)
    (postponable_expressions : (int, Mir.ExprSet.t entry_exit) Map.Poly.t)
    (used : (int, Mir.ExprSet.t) Map.Poly.t) =
  let earliest_or_postponable key =
    Mir.ExprSet.union
      (Map.Poly.find_exn earliest key)
      (Map.Poly.find_exn postponable_expressions key).entry
  in
  let latest key =
    Set.filter (earliest_or_postponable key) ~f:(fun e ->
        Set.mem (Map.Poly.find_exn used key) e
        || Set.Poly.exists (Map.Poly.find_exn successors key) ~f:(fun s ->
               not (Set.mem (earliest_or_postponable s) e) ) )
  in
  Map.fold successors ~init:Map.Poly.empty ~f:(fun ~key ~data:_ accum ->
      Map.set accum ~key ~data:(latest key) )

(** The transfer function for a used-not-latest expressions analysis, as a part of lazy code motion *)
let used_not_latest_expressions_transfer
    (used : (int, Mir.ExprSet.t) Map.Poly.t)
    (latest : (int, Mir.ExprSet.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Mir.ExprSet.t

    let transfer_function l p =
      let gen = Map.find_exn used l in
      let kill = Map.find_exn latest l in
      transfer_gen_kill_alt p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int and type properties = Mir.ExprSet.t )

(** The central definition of a monotone dataflow analysis framework.
    Given a compatible flowgraph, lattice and transfer function, we can
    run the mfp (maximal fixed point) algorithm, which computes a maximal
    fixed point (MFP) for the set of equations/inequalities of properties at the
    entry and exit of each node in the flow graph, as defined by the triple.
    Note that this gives a safe approximation to the MOP (meet over all paths)
    solution that we would really be interested in, but which is often incomputable.
    In case of a distributive lattice of properties, the MFP and MOP solutions coincide.
    *)
let monotone_framework (type l p) (module F : FLOWGRAPH with type labels = l)
    (module L : LATTICE with type properties = p)
    (module T : TRANSFER_FUNCTION with type labels = l and type properties = p)
    =
  ( module struct
    type labels = l
    type properties = p

    let mfp () =
      (* STEP 1: initialize data structures *)
      let workstack = Stack.create () in
      (* TODO: does the order matter a lot for efficiency here? *)
      let _ =
        Map.iteri F.successors ~f:(fun ~key ~data ->
            Set.iter data ~f:(fun succ -> Stack.push workstack (key, succ)) )
      in
      let analysis_in = Hashtbl.create (module F) in
      let _ =
        Map.iter_keys
          ~f:(fun l ->
            Hashtbl.add_exn analysis_in ~key:l
              ~data:(if Set.mem F.initials l then L.initial else L.bottom) )
          F.successors
      in
      (* STEP 2: iterate *)
      let _ =
        while Stack.length workstack <> 0 do
          let l, l' = Stack.pop_exn workstack in
          let old_analysis_in_l' = Hashtbl.find_exn analysis_in l' in
          let new_analysis_in_l' =
            T.transfer_function l (Hashtbl.find_exn analysis_in l)
          in
          if not (L.leq new_analysis_in_l' old_analysis_in_l') then
            let _ =
              Hashtbl.set analysis_in ~key:l'
                ~data:(L.lub old_analysis_in_l' new_analysis_in_l')
            in
            Set.iter (Map.find_exn F.successors l') ~f:(fun l'' ->
                Stack.push workstack (l', l'') )
        done
      in
      (* STEP 3: present final results *)
      let analysis_in_out =
        Map.fold ~init:Map.Poly.empty
          ~f:(fun ~key ~data:_ accum ->
            let analysis_in_data = Hashtbl.find_exn analysis_in key in
            Map.add_exn accum ~key
              ~data:
                { entry= analysis_in_data
                ; exit= T.transfer_function key analysis_in_data } )
          F.successors
      in
      analysis_in_out
  end
  : MONOTONE_FRAMEWORK
    with type labels = l and type properties = p )

let rec declared_variables_stmt
    (s : (Mir.expr_typed_located, Mir.stmt_loc) Mir.statement) =
  match s with
  | Mir.Decl {decl_id= x; _} -> Set.Poly.singleton x
  | Mir.Assignment (_, _)
   |Mir.TargetPE _
   |Mir.NRFunApp (_, _)
   |Mir.Check _ | Mir.Break | Mir.Continue | Mir.Return _ | Mir.Skip ->
      Set.Poly.empty
  | Mir.IfElse (_, b1, Some b2) ->
      Set.Poly.union
        (declared_variables_stmt b1.stmt)
        (declared_variables_stmt b2.stmt)
  | Mir.While (_, b) | Mir.IfElse (_, b, None) ->
      declared_variables_stmt b.stmt
  | Mir.For {loopvar= s; body= b; _} ->
      Set.Poly.add (declared_variables_stmt b.stmt) s
  | Mir.Block l | Mir.SList l ->
      Set.Poly.union_list
        (List.map ~f:(fun x -> declared_variables_stmt x.stmt) l)
  | Mir.FunDef {fdname= f; fdargs= l; fdbody= b; _} ->
      Set.Poly.union
        (Set.Poly.of_list (f :: List.map l ~f:(fun (_, x, _) -> x)))
        (declared_variables_stmt b.stmt)

let propagation_mfp (prog : Mir.typed_prog)
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int)
    (flowgraph_to_mir : (int, Mir.stmt_loc_num) Map.Poly.t)
    (propagation_transfer :
         (int, Mir.stmt_loc_num) Map.Poly.t
      -> (module
          TRANSFER_FUNCTION
            with type labels = int
             and type properties = (string, Mir.expr_typed_located) Map.Poly.t
                                   option)) =
  let mir = Map.find_exn flowgraph_to_mir 1 in
  let domain =
    ( module struct
      type vals = string

      let total =
        Set.Poly.union_list
          [ Set.Poly.of_map_keys prog.gen_quant_vars
          ; Set.Poly.of_map_keys prog.tdata_vars
          ; Set.Poly.of_map_keys prog.tparams
          ; Set.Poly.of_map_keys prog.params
          ; Set.Poly.of_map_keys prog.data_vars
          ; declared_variables_stmt
              (Mir.stmt_loc_of_stmt_loc_num flowgraph_to_mir mir).stmt ]
    end
    : TOTALTYPE
      with type vals = string )
  in
  let codomain =
    (module struct type vals = Mir.expr_typed_located
    end
    : TYPE
      with type vals = Mir.expr_typed_located )
  in
  let (module Lattice) =
    dual_partial_function_lattice_with_bot domain codomain
  in
  let (module Transfer) = propagation_transfer flowgraph_to_mir in
  let (module Mf) =
    monotone_framework (module Flowgraph) (module Lattice) (module Transfer)
  in
  Mf.mfp ()

let reaching_definitions_mfp (mir : Mir.typed_prog)
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int)
    (flowgraph_to_mir : (int, Mir.stmt_loc_num) Map.Poly.t) =
  let variables =
    ( module struct
      type vals = string

      let initial =
        Set.Poly.union_list
          [ Set.Poly.of_map_keys mir.gen_quant_vars
          ; Set.Poly.of_map_keys mir.tdata_vars
          ; Set.Poly.of_map_keys mir.tparams
          ; Set.Poly.of_map_keys mir.params
          ; Set.Poly.of_map_keys mir.data_vars ]
    end
    : INITIALTYPE
      with type vals = string )
  in
  let labels =
    (module struct type vals = int end : TYPE with type vals = int)
  in
  let (module Lattice) = reaching_definitions_lattice variables labels in
  let (module Transfer) = reaching_definitions_transfer flowgraph_to_mir in
  let (module Mf) =
    monotone_framework (module Flowgraph) (module Lattice) (module Transfer)
  in
  Mf.mfp ()

(** Monotone framework instance for live_variables analysis. Expects reverse
    flowgraph. *)
let live_variables_mfp (prog : Mir.typed_prog)
    (module Rev_Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int)
    (flowgraph_to_mir : (int, Mir.stmt_loc_num) Map.Poly.t) =
  let variables =
    ( module struct
      type vals = string

      (* NOTE: global generated quantities, (transformed) parameters and target are always observable
   so should be live. *)
      let initial =
        Set.Poly.add
          (Set.Poly.union
             (Set.of_map_keys prog.gen_quant_vars)
             (Set.of_map_keys (union_maps_left prog.params prog.tparams)))
          "target"
    end
    : INITIALTYPE
      with type vals = string )
  in
  let (module Lattice) = powerset_lattice variables in
  let (module Transfer) = live_variables_transfer flowgraph_to_mir in
  let (module Mf) =
    monotone_framework (module Rev_Flowgraph) (module Lattice) (module Transfer)
  in
  Mf.mfp ()

(** Instantiate all four instances of the monotone framework for lazy
    code motion, reusing code between them *)
let lazy_expressions_mfp
    (mir : (Mir.expr_typed_located, Mir.stmt_loc) Mir.prog)
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int)
    (module Rev_Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int)
    (flowgraph_to_mir : (int, Mir.stmt_loc_num) Map.Poly.t) =
  let all_expressions =
    Mir.ExprSet.union_list
      (List.map
         ~f:(fun x -> used_subexpressions_stmt x.stmt)
         (List.concat
            [ mir.functions_block; mir.generate_quantities; mir.prepare_params
            ; mir.log_prob; mir.prepare_data ]))
  in
  (* TODO: compute the above from the statement we are passing in
     (probably shouldn't use other blocks) *)
  let used_expr = used flowgraph_to_mir in
  let (module Lattice1) =
    dual_powerset_lattice_expressions Mir.ExprSet.empty all_expressions
  in
  let (module Lattice2) = powerset_lattice_expressions Mir.ExprSet.empty in
  let (module Transfer1) =
    anticipated_expressions_transfer flowgraph_to_mir used_expr
  in
  let (module Mf1) =
    monotone_framework
      (module Rev_Flowgraph)
      (module Lattice1)
      (module Transfer1)
  in
  let anticipated_expressions_mfp = Mf1.mfp () in
  let (module Transfer2) =
    available_expressions_transfer flowgraph_to_mir anticipated_expressions_mfp
  in
  let (module Mf2) =
    monotone_framework (module Flowgraph) (module Lattice1) (module Transfer2)
  in
  let available_expressions_mfp = Mf2.mfp () in
  let earliest_expr =
    earliest anticipated_expressions_mfp available_expressions_mfp
  in
  let (module Transfer3) =
    postponable_expressions_transfer earliest_expr used_expr
  in
  let (module Mf3) =
    monotone_framework (module Flowgraph) (module Lattice1) (module Transfer3)
  in
  let postponable_expressions_mfp = Mf3.mfp () in
  let latest_expr =
    latest Flowgraph.successors earliest_expr postponable_expressions_mfp
      used_expr
  in
  let (module Transfer4) =
    used_not_latest_expressions_transfer used_expr latest_expr
  in
  let (module Mf4) =
    monotone_framework
      (module Rev_Flowgraph)
      (module Lattice2)
      (module Transfer4)
  in
  let used_not_latest_expressions_mfp = Mf4.mfp () in
  (latest_expr, used_not_latest_expressions_mfp)
