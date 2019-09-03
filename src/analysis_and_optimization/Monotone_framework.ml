(** The common elements of a monotone framework *)

open Core_kernel
open Monotone_framework_sigs
open Mir_utils

(** Compute the inverse flowgraph of a Stan statement (for reverse analyses) *)
let inverse_flowgraph_of_stmt (stmt : Middle.stmt_loc) :
    (module FLOWGRAPH with type labels = int)
    * (int, Middle.stmt_loc_num) Map.Poly.t =
  let flowgraph_to_mir =
    Dataflow_utils.build_statement_map
      (fun x -> x.Middle.stmt)
      (fun x -> x.Middle.smeta)
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
  , Map.Poly.map
      ~f:(fun (stmtn, smetan) -> {Middle.stmtn; smetan})
      flowgraph_to_mir )

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

let powerset_lattice_expressions (initial : Middle.ExprSet.t) =
  ( module struct
    type properties = Middle.ExprSet.t

    let bottom = Middle.ExprSet.empty
    let lub s1 s2 = Middle.ExprSet.union s1 s2
    let leq s1 s2 = Middle.ExprSet.is_subset s1 ~of_:s2
    let initial = initial
  end
  : LATTICE
    with type properties = Middle.ExprSet.t )

let dual_powerset_lattice_expressions (initial : Middle.ExprSet.t)
    (total : Middle.ExprSet.t) =
  ( module struct
    type properties = Middle.ExprSet.t

    let bottom = total
    let lub s1 s2 = Middle.ExprSet.inter s1 s2
    let leq s1 s2 = Middle.ExprSet.is_subset s2 ~of_:s1
    let initial = initial
  end
  : LATTICE
    with type properties = Middle.ExprSet.t )

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

let initialized_vars_lattice =
  ( module struct
    type properties = string Set.Poly.t

    let bottom = Set.Poly.empty (*Set.Poly.of_list ["z"; "i"; "x"]*)
    let lub s1 s2 = Set.Poly.inter s1 s2
    let leq s1 s2 = Set.Poly.is_subset s1 ~of_:s2
    let initial = Set.Poly.empty
  end
  : LATTICE
    with type properties = string Set.Poly.t )


(* Autodiff-level lattice *)
let autodiff_level_lattice autodiff_variables =
  powerset_lattice
    (module struct type vals = string

                   let initial = autodiff_variables end)

(* The transfer function for a constant propagation analysis *)
let constant_propagation_transfer
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string, Middle.expr_typed_located) Map.Poly.t option

    let transfer_function l p =
      match p with
      | None -> None
      | Some m ->
          let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
          Some
            ( match mir_node with
            (* TODO: we are currently only propagating constants for scalars.
             We could do the same for matrix and array expressions if we wanted. *)
            | Middle.Assignment ((s, _, []), e) -> (
              match Partial_evaluator.eval_expr (subst_expr m e) with
              | {expr= Middle.Lit (_, _); _} as e' -> Map.set m ~key:s ~data:e'
              | _ -> Map.remove m s )
            | Middle.Decl {decl_id= s; _}
             |Middle.Assignment ((s, _, _ :: _), _) ->
                Map.remove m s
            | Middle.TargetPE _
             |Middle.NRFunApp (_, _, _)
             |Middle.Break | Middle.Continue | Middle.Return _ | Middle.Skip
             |Middle.IfElse (_, _, _)
             |Middle.While (_, _)
             |Middle.For _ | Middle.Block _ | Middle.SList _ ->
                m )
  end
  : TRANSFER_FUNCTION
    with type labels = int
     and type properties = (string, Middle.expr_typed_located) Map.Poly.t
                           option )

(** The transfer function for an expression propagation analysis,
    AKA forward substitution (see page 396 of Muchnick) *)
let expression_propagation_transfer
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string, Middle.expr_typed_located) Map.Poly.t option

    let transfer_function l p =
      match p with
      | None -> None
      | Some m ->
          let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
          Some
            ( match mir_node with
            (* TODO: we are currently only propagating constants for scalars.
             We could do the same for matrix and array expressions if we wanted. *)
            | Middle.Assignment ((s, _, []), e) ->
                Map.set m ~key:s ~data:(subst_expr m e)
            | Middle.Decl {decl_id= s; _}
             |Middle.Assignment ((s, _, _ :: _), _) ->
                Map.remove m s
            | Middle.TargetPE _
             |Middle.NRFunApp (_, _, _)
             |Middle.Break | Middle.Continue | Middle.Return _ | Middle.Skip
             |Middle.IfElse (_, _, _)
             |Middle.While (_, _)
             |Middle.For _ | Middle.Block _ | Middle.SList _ ->
                m )
  end
  : TRANSFER_FUNCTION
    with type labels = int
     and type properties = (string, Middle.expr_typed_located) Map.Poly.t
                           option )

(** The transfer function for a copy propagation analysis *)
let copy_propagation_transfer
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string, Middle.expr_typed_located) Map.Poly.t option

    let transfer_function l p =
      match p with
      | None -> None
      | Some m ->
          let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
          Some
            ( match mir_node with
            | Middle.Assignment ((s, _, []), {expr= Middle.Var t; emeta}) ->
                Map.set m ~key:s ~data:{Middle.expr= Middle.Var t; emeta}
            | Middle.Decl {decl_id= s; _}
             |Middle.Assignment ((s, _, _ :: _), _) ->
                Map.remove m s
            | Middle.Assignment (_, _)
             |Middle.TargetPE _
             |Middle.NRFunApp (_, _, _)
             |Middle.Break | Middle.Continue | Middle.Return _ | Middle.Skip
             |Middle.IfElse (_, _, _)
             |Middle.While (_, _)
             |Middle.For _ | Middle.Block _ | Middle.SList _ ->
                m )
  end
  : TRANSFER_FUNCTION
    with type labels = int
     and type properties = (string, Middle.expr_typed_located) Map.Poly.t
                           option )

(** A helper function for building transfer functions from gen and kill sets *)
let transfer_gen_kill p gen kill = Set.union gen (Set.diff p kill)

(* TODO: from here *)

(** Calculate the set of variables that a statement can assign to *)
let assigned_vars_stmt
    (s : (Middle.expr_typed_located, 'a) Middle.statement) =
  match s with
  | Middle.Assignment ((x, _, _), _)  ->
      Set.Poly.singleton x
  | Middle.TargetPE _ -> Set.Poly.singleton "target"
  | Middle.NRFunApp (_, s, _) when String.suffix s 3 = "_lp" ->
      Set.Poly.singleton "target"
  | Middle.For {loopvar= x; _} -> Set.Poly.singleton x
  | Middle.Decl {decl_id= _; _}
   |Middle.NRFunApp (_, _, _)
   |Middle.Break | Middle.Continue | Middle.Return _ | Middle.Skip
   |Middle.IfElse (_, _, _)
   |Middle.While (_, _)
   |Middle.Block _ | Middle.SList _ ->
      Set.Poly.empty

(** Calculate the set of variables that a statement can declare *)
let declared_vars_stmt
    (s : (Middle.expr_typed_located, 'a) Middle.statement) =
  match s with
  | Middle.Decl {decl_id= x; _} -> Set.Poly.singleton x
  | _ -> Set.Poly.empty

(** Calculate the set of variables that a statement can assign to or declare *)
let assigned_or_declared_vars_stmt
    (s : (Middle.expr_typed_located, 'a) Middle.statement) =
      Set.Poly.union (assigned_vars_stmt s) (declared_vars_stmt s)


(** The transfer function for a reaching definitions analysis *)
let reaching_definitions_transfer
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t) =
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
        | Middle.Decl {decl_id= x; _}
         |Middle.Assignment ((x, _, []), _)
         |Middle.For {loopvar= x; _} ->
            Set.filter p ~f:(fun (y, _) -> y = x)
        | Middle.TargetPE _ -> Set.filter p ~f:(fun (y, _) -> y = "target")
        | Middle.NRFunApp (_, s, _) when String.suffix s 3 = "_lp" ->
            Set.filter p ~f:(fun (y, _) -> y = "target")
        | Middle.NRFunApp (_, _, _)
         |Middle.Break | Middle.Continue | Middle.Return _ | Middle.Skip
         |Middle.IfElse (_, _, _)
         |Middle.While (_, _)
         |Middle.Block _ | Middle.SList _ | Middle.Assignment _ ->
            Set.Poly.empty
      in
      transfer_gen_kill p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int
     and type properties = (string * int option) Set.Poly.t )

(** The transfer function for a reaching definitions analysis *)
let initialized_vars_transfer
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = string Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
      let gen =
          assigned_vars_stmt mir_node
      in
      let () = match Set.Poly.min_elt gen with
        | None -> (Out_channel.output_string stdout) ("no gen " ^ string_of_int l)
        | Some s -> (Out_channel.output_string stdout) ("gen " ^ s ^ " " ^ string_of_int l)
      in
      let () = Out_channel.newline stdout in
      transfer_gen_kill p gen Set.Poly.empty
  end
  : TRANSFER_FUNCTION
    with type labels = int
     and type properties = string Set.Poly.t )


(** Calculate the free (non-bound) variables in an expression *)
let rec free_vars_expr (e : Middle.expr_typed_located) =
  match e.expr with
  | Middle.Var x -> Set.Poly.singleton x
  | Middle.Lit (_, _) -> Set.Poly.empty
  | Middle.FunApp (_, f, l) ->
      Set.Poly.union_list (Set.Poly.singleton f :: List.map ~f:free_vars_expr l)
  | Middle.TernaryIf (e1, e2, e3) ->
      Set.Poly.union_list (List.map ~f:free_vars_expr [e1; e2; e3])
  | Middle.Indexed (e, l) ->
      Set.Poly.union_list (free_vars_expr e :: List.map ~f:free_vars_idx l)
  | Middle.EAnd (e1, e2) | Middle.EOr (e1, e2) ->
      Set.Poly.union_list (List.map ~f:free_vars_expr [e1; e2])

(** Calculate the free (non-bound) variables in an index*)
and free_vars_idx (i : Middle.expr_typed_located Middle.index) =
  match i with
  | Middle.All -> Set.Poly.empty
  | Middle.Single e | Middle.Upfrom e | Middle.MultiIndex e -> free_vars_expr e
  | Middle.Between (e1, e2) ->
      Set.Poly.union (free_vars_expr e1) (free_vars_expr e2)

(** Calculate the free (non-bound) variables in a statement *)
let rec free_vars_stmt
    (s : (Middle.expr_typed_located, Middle.stmt_loc) Middle.statement) =
  match s with
  | Middle.Assignment ((_, _, []), e)
   |Middle.Return (Some e)
   |Middle.TargetPE e ->
      free_vars_expr e
  | Middle.Assignment ((_, _, l), e) ->
      Set.Poly.union_list (free_vars_expr e :: List.map ~f:free_vars_idx l)
  | Middle.NRFunApp (_, f, l) ->
      Set.Poly.union_list (Set.Poly.singleton f :: List.map ~f:free_vars_expr l)
  | Middle.IfElse (e, b1, Some b2) ->
      Set.Poly.union_list
        [free_vars_expr e; free_vars_stmt b1.stmt; free_vars_stmt b2.stmt]
  | Middle.IfElse (e, b, None) | Middle.While (e, b) ->
      Set.Poly.union (free_vars_expr e) (free_vars_stmt b.stmt)
  | Middle.For {lower= e1; upper= e2; body= b; _} ->
      Set.Poly.union_list
        [free_vars_expr e1; free_vars_expr e2; free_vars_stmt b.stmt]
  | Middle.Block l | Middle.SList l ->
      Set.Poly.union_list (List.map ~f:(fun s -> free_vars_stmt s.stmt) l)
  | Middle.Decl _ | Middle.Break | Middle.Continue
   |Middle.Return None
   |Middle.Skip ->
      Set.Poly.empty

(** A variation on free_vars_stmt, where we do not recursively count free
    variables in sub statements  *)
let top_free_vars_stmt
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t)
    (s : (Middle.expr_typed_located, int) Middle.statement) =
  match s with
  | Middle.Assignment _ | Middle.Return _ | Middle.TargetPE _
   |Middle.NRFunApp _ | Middle.Decl _ | Middle.Break | Middle.Continue
   |Middle.Skip ->
      free_vars_stmt
        (statement_stmt_loc_of_statement_stmt_loc_num flowgraph_to_mir s)
  | Middle.While (e, _) | Middle.IfElse (e, _, _) -> free_vars_expr e
  | Middle.For {lower= e1; upper= e2; _} ->
      Set.Poly.union_list [free_vars_expr e1; free_vars_expr e2]
  | Middle.Block _ | Middle.SList _ -> Set.Poly.empty

(** The transfer function for a live variables analysis *)
let live_variables_transfer
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = string Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
      let gen = top_free_vars_stmt flowgraph_to_mir mir_node in
      let kill =
        match mir_node with
        | Middle.Assignment ((x, _, []), _) | Middle.Decl {decl_id= x; _} ->
            Set.Poly.singleton x
        | Middle.TargetPE _
         |Middle.NRFunApp (_, _, _)
         |Middle.Break | Middle.Continue | Middle.Return _ | Middle.Skip
         |Middle.IfElse (_, _, _)
         |Middle.While (_, _)
         |Middle.For _ | Middle.Block _ | Middle.SList _
         |Middle.Assignment ((_, _, _ :: _), _) ->
            Set.Poly.empty
      in
      transfer_gen_kill p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int and type properties = string Set.Poly.t )

(** Calculate the set of sub-expressions of an expression *)
let rec used_subexpressions_expr (e : Middle.expr_typed_located) =
  Middle.ExprSet.union
    (Middle.ExprSet.singleton e)
    ( match e.expr with
    | Middle.Var _ | Middle.Lit (_, _) -> Middle.ExprSet.empty
    | Middle.FunApp (_, _, l) ->
        Middle.ExprSet.union_list (List.map ~f:used_subexpressions_expr l)
    | Middle.TernaryIf (e1, e2, e3) ->
        Middle.ExprSet.union_list
          [ used_subexpressions_expr e1
          ; used_subexpressions_expr e2
          ; used_subexpressions_expr e3 ]
    | Middle.Indexed (e, l) ->
        Middle.ExprSet.union_list
          ( used_subexpressions_expr e
          :: List.map ~f:(used_expressions_idx_help used_subexpressions_expr) l
          )
    | Middle.EAnd (e1, e2) | Middle.EOr (e1, e2) ->
        Middle.ExprSet.union_list
          [used_subexpressions_expr e1; used_subexpressions_expr e2] )

and used_expressions_idx_help f (i : Middle.expr_typed_located Middle.index) =
  match i with
  | Middle.All -> Middle.ExprSet.empty
  | Middle.Single e | Middle.Upfrom e | Middle.MultiIndex e -> f e
  | Middle.Between (e1, e2) -> Middle.ExprSet.union (f e1) (f e2)

(** Calculate the set of expressions of an expression *)
let used_expressions_expr e = Middle.ExprSet.singleton e

let rec used_expressions_stmt_help f
    (s : (Middle.expr_typed_located, Middle.stmt_loc) Middle.statement) =
  match s with
  | Middle.Assignment ((_, _, []), e)
   |Middle.TargetPE e
   |Middle.Return (Some e) ->
      f e
  | Middle.Assignment ((_, _, l), e) ->
      Middle.ExprSet.union (f e)
        (Middle.ExprSet.union_list
           (List.map ~f:(used_expressions_idx_help f) l))
  | Middle.IfElse (e, b1, Some b2) ->
      Middle.ExprSet.union_list
        [ f e
        ; used_expressions_stmt_help f b1.stmt
        ; used_expressions_stmt_help f b2.stmt ]
  | Middle.NRFunApp (_, _, l) -> Middle.ExprSet.union_list (List.map ~f l)
  | Middle.Decl _
   |Middle.Return None
   |Middle.Break | Middle.Continue | Middle.Skip ->
      Middle.ExprSet.empty
  | Middle.IfElse (e, b, None) | Middle.While (e, b) ->
      Middle.ExprSet.union (f e) (used_expressions_stmt_help f b.stmt)
  | Middle.For {lower= e1; upper= e2; body= b; loopvar= s} ->
      Middle.ExprSet.union_list
        [ f e1; f e2
        ; used_expressions_stmt_help f b.stmt
        ; Middle.ExprSet.singleton
            { Middle.expr= Var s
            ; Middle.emeta=
                {mtype= UInt; madlevel= DataOnly; mloc= Middle.no_span} } ]
  | Middle.Block l | Middle.SList l ->
      Middle.ExprSet.union_list
        (List.map ~f:(fun s -> used_expressions_stmt_help f s.stmt) l)

(** Calculate the set of sub-expressions in a statement *)
let used_subexpressions_stmt =
  used_expressions_stmt_help used_subexpressions_expr

(** Calculate the set of expressions in a statement *)
let used_expressions_stmt = used_expressions_stmt_help used_expressions_expr

let top_used_expressions_stmt_help f
    (s : (Middle.expr_typed_located, int) Middle.statement) =
  match s with
  | Middle.Assignment ((_, _, []), e)
   |Middle.TargetPE e
   |Middle.Return (Some e) ->
      f e
  | Middle.Assignment ((_, _, l), e) ->
      Middle.ExprSet.union (f e)
        (Middle.ExprSet.union_list
           (List.map ~f:(used_expressions_idx_help f) l))
  | Middle.While (e, _) | Middle.IfElse (e, _, _) -> f e
  | Middle.NRFunApp (_, _, l) -> Middle.ExprSet.union_list (List.map ~f l)
  | Middle.Block _ | Middle.SList _ | Middle.Decl _
   |Middle.Return None
   |Middle.Break | Middle.Continue | Middle.Skip ->
      Middle.ExprSet.empty
  | Middle.For {lower= e1; upper= e2; _} ->
      Middle.ExprSet.union_list [f e1; f e2]

(** Calculate the set of sub-expressions at the top level in a statement *)
let top_used_subexpressions_stmt =
  top_used_expressions_stmt_help used_subexpressions_expr

(** Calculate the set of expressions at the top level in a statement *)
let top_used_expressions_stmt =
  top_used_expressions_stmt_help used_expressions_expr

(** Calculate the subset (of p) of expressions that will need to be recomputed as a
    consequence of evaluating the statement s (because of writes to variables performed
    by s) *)
let killed_expressions_stmt (p : Middle.ExprSet.t)
    (s : (Middle.expr_typed_located, int) Middle.statement) =
  Middle.ExprSet.filter p ~f:(fun e ->
      let free_vars = free_vars_expr e in
      (* Note: a simple test for membership would be more efficient here,
         but it would require us to duplicate some code. *)
      let assigned_vars = assigned_or_declared_vars_stmt s in
      not (Set.Poly.is_empty (Set.Poly.inter free_vars assigned_vars)) )

(** Calculate the set of subexpressions that needs to be computed at each node
    in the flowgraph *)
let used (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t) =
  Map.Poly.fold flowgraph_to_mir ~init:Map.Poly.empty
    ~f:(fun ~key ~data accum ->
      Map.Poly.set accum ~key ~data:(top_used_subexpressions_stmt data.stmtn)
  )

(* TODO: figure out whether we will also want to reuse the computation of killed *)

(** The transfer function for an anticipated expressions analysis (as a part of lazy
    code motion) *)
let anticipated_expressions_transfer
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t)
    (used : (int, Middle.ExprSet.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Middle.ExprSet.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
      let gen = Map.Poly.find_exn used l in
      let kill = killed_expressions_stmt p mir_node in
      transfer_gen_kill p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int and type properties = Middle.ExprSet.t )

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
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t)
    (anticipated_expressions : (int, Middle.ExprSet.t entry_exit) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Middle.ExprSet.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
      let gen = (Map.find_exn anticipated_expressions l).exit in
      let kill =
        killed_expressions_stmt (Middle.ExprSet.union p gen) mir_node
      in
      transfer_gen_kill_alt p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int and type properties = Middle.ExprSet.t )

(** Calculates the set of expressions that can be calculated for the first time
    at each node in the flow graph *)
let earliest
    (anticipated_expressions : (int, Middle.ExprSet.t entry_exit) Map.Poly.t)
    (available_expressions : (int, Middle.ExprSet.t entry_exit) Map.Poly.t) =
  Map.fold anticipated_expressions ~init:Map.Poly.empty
    ~f:(fun ~key ~data accum ->
      Map.set accum ~key
        ~data:
          (Set.diff data.exit (Map.find_exn available_expressions key).entry)
  )

(** The transfer function for a postponable expressions analysis (as a part of lazy code motion) *)
let postponable_expressions_transfer
    (earliest : (int, Middle.ExprSet.t) Map.Poly.t)
    (used : (int, Middle.ExprSet.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Middle.ExprSet.t

    let transfer_function l p =
      let gen = Map.find_exn earliest l in
      let kill = Map.find_exn used l in
      transfer_gen_kill_alt p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int and type properties = Middle.ExprSet.t )

(** Calculates the set of expressions that can be computed at the latest at each node *)
let latest (successors : (int, int Set.Poly.t) Map.Poly.t)
    (earliest : (int, Middle.ExprSet.t) Map.Poly.t)
    (postponable_expressions : (int, Middle.ExprSet.t entry_exit) Map.Poly.t)
    (used : (int, Middle.ExprSet.t) Map.Poly.t) =
  let earliest_or_postponable key =
    Middle.ExprSet.union
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
    (used : (int, Middle.ExprSet.t) Map.Poly.t)
    (latest : (int, Middle.ExprSet.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Middle.ExprSet.t

    let transfer_function l p =
      let gen = Map.find_exn used l in
      let kill = Map.find_exn latest l in
      transfer_gen_kill_alt p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int and type properties = Middle.ExprSet.t )

(** The transfer function for the first forward analysis part of determining optimal ad-levels for variables *)
let autodiff_level_fwd1_transfer
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = string Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
      let gen =
        match mir_node with
        | Middle.Assignment ((x, _, _), e)
          when (update_expr_ad_levels p e).emeta.madlevel = Middle.AutoDiffable
          ->
            Set.Poly.singleton x
        | _ -> Set.Poly.empty
      in
      let kill =
        match mir_node with
        | Middle.Decl {decl_id; decl_adtype= DataOnly; _} ->
            Set.Poly.singleton decl_id
        | _ -> Set.Poly.empty
      in
      transfer_gen_kill p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int and type properties = string Set.Poly.t )

(** The transfer function for the reverse analysis part of determining optimal ad-levels for variables *)
let autodiff_level_rev_transfer
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t)
    (fwd_ad_levels : (int, string Set.Poly.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = string Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
      let gen = Map.find_exn fwd_ad_levels l in
      let kill =
        match mir_node with
        | Middle.Decl {decl_id; _} -> Set.Poly.singleton decl_id
        | _ -> Set.Poly.empty
      in
      transfer_gen_kill_alt p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int and type properties = string Set.Poly.t )

(** The transfer function for the second forward analysis part of determining optimal ad-levels for variables *)
let autodiff_level_fwd2_transfer
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t)
    (rev_ad_levels : (int, string Set.Poly.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = string Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmtn in
      let gen = Map.find_exn rev_ad_levels l in
      let kill =
        match mir_node with
        | Middle.Decl {decl_id; _} -> Set.Poly.singleton decl_id
        | _ -> Set.Poly.empty
      in
      transfer_gen_kill p gen kill
  end
  : TRANSFER_FUNCTION
    with type labels = int and type properties = string Set.Poly.t )

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
      Map.iteri F.successors ~f:(fun ~key ~data ->
          Set.iter data ~f:(fun succ -> Stack.push workstack (key, succ)) ) ;
      let analysis_in = Hashtbl.create (module F) in
      Map.iter_keys
        ~f:(fun l ->
          Hashtbl.add_exn analysis_in ~key:l
            ~data:(if Set.mem F.initials l then L.initial else L.bottom) )
        F.successors ;
      (* STEP 2: iterate *)
      while Stack.length workstack <> 0 do
        let l, l' = Stack.pop_exn workstack in
        let old_analysis_in_l' = Hashtbl.find_exn analysis_in l' in
        let new_analysis_in_l' =
          T.transfer_function l (Hashtbl.find_exn analysis_in l)
        in
        if not (L.leq new_analysis_in_l' old_analysis_in_l') then
          let () =
            Hashtbl.set analysis_in ~key:l'
              ~data:(L.lub old_analysis_in_l' new_analysis_in_l')
          in
          Set.iter (Map.find_exn F.successors l') ~f:(fun l'' ->
              Stack.push workstack (l', l'') )
      done ;
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
    (s : (Middle.expr_typed_located, Middle.stmt_loc) Middle.statement) =
  match s with
  | Middle.Decl {decl_id= x; _} -> Set.Poly.singleton x
  | Middle.Assignment (_, _)
   |Middle.TargetPE _
   |Middle.NRFunApp (_, _, _)
   |Middle.Break | Middle.Continue | Middle.Return _ | Middle.Skip ->
      Set.Poly.empty
  | Middle.IfElse (_, b1, Some b2) ->
      Set.Poly.union
        (declared_variables_stmt b1.stmt)
        (declared_variables_stmt b2.stmt)
  | Middle.While (_, b) | Middle.IfElse (_, b, None) ->
      declared_variables_stmt b.stmt
  | Middle.For {loopvar= s; body= b; _} ->
      Set.Poly.add (declared_variables_stmt b.stmt) s
  | Middle.Block l | Middle.SList l ->
      Set.Poly.union_list
        (List.map ~f:(fun x -> declared_variables_stmt x.stmt) l)

let propagation_mfp (prog : Middle.typed_prog)
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int)
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t)
    (propagation_transfer :
         (int, Middle.stmt_loc_num) Map.Poly.t
      -> (module
          TRANSFER_FUNCTION
            with type labels = int
             and type properties = ( string
                                   , Middle.expr_typed_located )
                                   Map.Poly.t
                                   option)) =
  let mir = Map.find_exn flowgraph_to_mir 1 in
  let domain =
    ( module struct
      type vals = string

      let total =
        Set.Poly.union_list
          [ Set.Poly.of_list (List.map ~f:fst prog.input_vars)
          ; Set.Poly.of_list (List.map ~f:fst prog.output_vars)
          ; declared_variables_stmt
              (stmt_loc_of_stmt_loc_num flowgraph_to_mir mir).stmt ]
    end
    : TOTALTYPE
      with type vals = string )
  in
  let codomain =
    (module struct type vals = Middle.expr_typed_located
    end
    : TYPE
      with type vals = Middle.expr_typed_located )
  in
  let (module Lattice) =
    dual_partial_function_lattice_with_bot domain codomain
  in
  let (module Transfer) = propagation_transfer flowgraph_to_mir in
  let (module Mf) =
    monotone_framework (module Flowgraph) (module Lattice) (module Transfer)
  in
  Mf.mfp ()

let reaching_definitions_mfp (mir : Middle.typed_prog)
    ?(uninitialized : string Set.Poly.t = Set.Poly.empty)
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int)
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t) =
  let variables =
    ( module struct
      type vals = string

      let initial =
        Set.Poly.union_list
          [ Set.Poly.of_list (List.map ~f:fst mir.input_vars)
          ; Set.Poly.of_list (List.map ~f:fst mir.output_vars)
          ; uninitialized
          ]
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

let initialized_vars_mfp
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int)
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t) =
  let (module Lattice) = initialized_vars_lattice in
  let (module Transfer) = initialized_vars_transfer flowgraph_to_mir in
  let (module Mf) =
    monotone_framework (module Flowgraph) (module Lattice) (module Transfer)
  in
  Mf.mfp ()

(** Monotone framework instance for live_variables analysis. Expects reverse
    flowgraph. *)
let live_variables_mfp (prog : Middle.typed_prog)
    (module Rev_Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int)
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t) =
  let variables =
    ( module struct
      type vals = string

      (* NOTE: global generated quantities, (transformed) parameters and target are always observable
   so should be live. *)
      let initial =
        Set.Poly.add
          (Set.Poly.of_list (List.map ~f:fst prog.output_vars))
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
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int)
    (module Rev_Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int)
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t) =
  let all_expressions =
    used_subexpressions_stmt
      (stmt_loc_of_stmt_loc_num flowgraph_to_mir
         (Map.Poly.find_exn flowgraph_to_mir 1))
        .stmt
  in
  (* TODO: this could probably be done in a nicer way *)
  let used_expr = used flowgraph_to_mir in
  let (module Lattice1) =
    dual_powerset_lattice_expressions Middle.ExprSet.empty all_expressions
  in
  let (module Lattice2) = powerset_lattice_expressions Middle.ExprSet.empty in
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

(** Perform the analysis for ad-levels, using both the fwd and reverse pass *)
let autodiff_level_mfp
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int)
    (module Rev_Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int)
    (flowgraph_to_mir : (int, Middle.stmt_loc_num) Map.Poly.t)
    (autodiff_variables : string Set.Poly.t) =
  let (module Lattice1) = autodiff_level_lattice autodiff_variables in
  let (module Lattice2) = autodiff_level_lattice Set.Poly.empty in
  let (module Transfer1) = autodiff_level_fwd1_transfer flowgraph_to_mir in
  let (module Mf1) =
    monotone_framework (module Flowgraph) (module Lattice1) (module Transfer1)
  in
  let fwd1_ad_levels_mfp = Mf1.mfp () in
  let (module Transfer2) =
    autodiff_level_rev_transfer flowgraph_to_mir
      (Map.map ~f:(fun x -> x.exit) fwd1_ad_levels_mfp)
  in
  let (module Mf2) =
    monotone_framework
      (module Rev_Flowgraph)
      (module Lattice2)
      (module Transfer2)
  in
  let rev_ad_levels_mfp = Mf2.mfp () in
  let (module Transfer3) =
    autodiff_level_fwd2_transfer flowgraph_to_mir
      (Map.map ~f:(fun x -> x.entry) rev_ad_levels_mfp)
  in
  let (module Mf3) =
    monotone_framework (module Flowgraph) (module Lattice2) (module Transfer3)
  in
  let fwd2_ad_levels_mfp = Mf3.mfp () in
  fwd2_ad_levels_mfp
