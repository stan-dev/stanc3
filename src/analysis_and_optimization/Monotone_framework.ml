(** The common elements of a monotone framework *)

open Core_kernel
open Core_kernel.Poly
open Monotone_framework_sigs
open Mir_utils
open Middle

(** Debugging tool to print out MFP sets **)
let print_mfp to_string (mfp : (int, 'a entry_exit) Map.Poly.t)
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t) : unit =
  let print_set s =
    [%sexp (Set.Poly.map ~f:to_string s : string Set.Poly.t)]
    |> Sexp.to_string_hum in
  let print_stmt s =
    [%sexp (s : Stmt.Located.Non_recursive.t)] |> Sexp.to_string_hum in
  Map.iteri mfp ~f:(fun ~key ~data ->
      print_endline
        ( string_of_int key ^ ":\n "
        ^ print_stmt (Map.Poly.find_exn flowgraph_to_mir key)
        ^ ":\n " ^ print_set data.entry ^ " \t-> " ^ print_set data.exit ) )

(** Calculate the free (non-bound) variables in an expression *)
let rec free_vars_expr (e : Expr.Typed.t) =
  match e.pattern with
  | Var x -> Set.Poly.singleton x
  | Promotion (expr, _, _) -> free_vars_expr expr
  | Lit (_, _) -> Set.Poly.empty
  | FunApp (kind, l) -> free_vars_fnapp kind l
  | TernaryIf (e1, e2, e3) ->
      Set.Poly.union_list (List.map ~f:free_vars_expr [e1; e2; e3])
  | Indexed (e, l) ->
      Set.Poly.union_list (free_vars_expr e :: List.map ~f:free_vars_idx l)
  | EAnd (e1, e2) | EOr (e1, e2) ->
      Set.Poly.union_list (List.map ~f:free_vars_expr [e1; e2])

(** Calculate the free (non-bound) variables in an index*)
and free_vars_idx (i : Expr.Typed.t Index.t) =
  match i with
  | All -> Set.Poly.empty
  | Single e | Upfrom e | MultiIndex e -> free_vars_expr e
  | Between (e1, e2) -> Set.Poly.union (free_vars_expr e1) (free_vars_expr e2)

and free_vars_fnapp kind l =
  let arg_vars = List.map ~f:free_vars_expr (l @ Fun_kind.collect_exprs kind) in
  match kind with
  | Fun_kind.UserDefined (f, _) ->
      Set.Poly.union_list (Set.Poly.singleton f :: List.map ~f:free_vars_expr l)
  | _ -> Set.Poly.union_list arg_vars

(** Calculate the free (non-bound) variables in a statement *)
let rec free_vars_stmt (s : (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t)
    =
  match s with
  | Assignment ((_, _, []), e) | Return (Some e) | TargetPE e ->
      free_vars_expr e
  | Assignment ((_, _, l), e) ->
      Set.Poly.union_list (free_vars_expr e :: List.map ~f:free_vars_idx l)
  | NRFunApp (kind, l) -> free_vars_fnapp kind l
  | IfElse (e, b1, Some b2) ->
      Set.Poly.union_list
        [free_vars_expr e; free_vars_stmt b1.pattern; free_vars_stmt b2.pattern]
  | IfElse (e, b, None) | While (e, b) ->
      Set.Poly.union (free_vars_expr e) (free_vars_stmt b.pattern)
  | For {lower= e1; upper= e2; body= b; _} ->
      Set.Poly.union_list
        [free_vars_expr e1; free_vars_expr e2; free_vars_stmt b.pattern]
  | Profile (_, l) | Block l | SList l ->
      Set.Poly.union_list (List.map ~f:(fun s -> free_vars_stmt s.pattern) l)
  | Decl _ | Break | Continue | Return None | Skip -> Set.Poly.empty

(** A variation on free_vars_stmt, where we do not recursively count free
    variables in sub statements  *)
let top_free_vars_stmt
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t)
    (s : (Expr.Typed.t, int) Stmt.Fixed.Pattern.t) =
  match s with
  | Assignment _ | Return _ | TargetPE _ | NRFunApp _ | Decl _ | Break
   |Continue | Skip ->
      free_vars_stmt
        (statement_stmt_loc_of_statement_stmt_loc_num flowgraph_to_mir s)
  | While (e, _) | IfElse (e, _, _) -> free_vars_expr e
  | For {lower= e1; upper= e2; _} ->
      Set.Poly.union_list [free_vars_expr e1; free_vars_expr e2]
  | Profile _ | Block _ | SList _ -> Set.Poly.empty

(** Compute the inverse flowgraph of a Stan statement (for reverse analyses) *)
let inverse_flowgraph_of_stmt ?(flatten_loops = false)
    ?(blocks_after_body = true) (stmt : Stmt.Located.t) :
    (module FLOWGRAPH with type labels = int)
    * (int, Stmt.Located.Non_recursive.t) Map.Poly.t =
  let flowgraph_to_mir =
    Dataflow_utils.build_statement_map
      (fun Stmt.Fixed.{pattern; _} -> pattern)
      (fun Stmt.Fixed.{meta; _} -> meta)
      stmt in
  let initials, successors =
    Dataflow_utils.build_predecessor_graph ~flatten_loops ~blocks_after_body
      flowgraph_to_mir in
  ( ( module struct
      type labels = int
      type t = labels

      let compare = Int.compare
      let hash = Int.hash
      let sexp_of_t = Int.sexp_of_t
      let initials = initials
      let successors = successors end : FLOWGRAPH
      with type labels = int )
  , Map.Poly.map
      ~f:(fun (pattern, meta) -> Stmt.Located.Non_recursive.{pattern; meta})
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
                ~data:(Set.add (Map.find_exn accum old_succ) old_pred) ) ) end
  : FLOWGRAPH
    with type labels = l )

(** Modify the end nodes of a flowgraph to depend on its inits
 * To force the monotone framework to run until the program never changes
 *  this function modifies the input `Flowgraph` so that it's end nodes
 *  depend on it's initial nodes. The inits of the reverse flowgraph are used
 *  for this since we normally have both the forward and reverse flowgraphs
 *  available.
 * @tparam l Type of the label for each flowgraph, most commonly an int
 * @param Flowgraph The flowgraph to modify
 * @param RevFlowgraph The same flowgraph as `Flowgraph` but reversed.
 *
 *)
let make_circular_flowgraph (type l)
    (module Flowgraph : FLOWGRAPH with type labels = l)
    (module RevFlowgraph : FLOWGRAPH with type labels = l) =
  ( module struct
    type labels = Flowgraph.labels
    type t = labels

    let compare = Flowgraph.compare
    let hash = Flowgraph.hash
    let sexp_of_t = Flowgraph.sexp_of_t
    let initials = Flowgraph.initials

    let successors =
      let set_exits_to_depend_on_inits ~key ~data =
        if Set.Poly.mem RevFlowgraph.initials key then
          Set.Poly.union data Flowgraph.initials
        else data in
      Map.mapi Flowgraph.successors ~f:set_exits_to_depend_on_inits end
  : FLOWGRAPH
    with type labels = l )

(** Compute the forward flowgraph of a Stan statement (for forward analyses) *)
let forward_flowgraph_of_stmt ?(flatten_loops = false)
    ?(blocks_after_body = true) stmt =
  let inv_flowgraph =
    inverse_flowgraph_of_stmt ~flatten_loops ~blocks_after_body stmt in
  (reverse (fst inv_flowgraph), snd inv_flowgraph)

(**  The lattice of sets of some values, with the inclusion order, set union
     and the empty set *)
let powerset_lattice (type v) (module S : INITIALTYPE with type vals = v) =
  ( module struct
    type properties = S.vals Set.Poly.t

    let bottom = Set.Poly.empty
    let lub s1 s2 = Set.Poly.union s1 s2
    let leq s1 s2 = Set.Poly.is_subset s1 ~of_:s2
    let initial = S.initial end : LATTICE
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
    let initial = S.initial end : LATTICE
    with type properties = v Set.Poly.t )

let powerset_lattice_expressions (initial : Expr.Typed.Set.t) =
  ( module struct
    type properties = Expr.Typed.Set.t

    let bottom = Expr.Typed.Set.empty
    let lub s1 s2 = Expr.Typed.Set.union s1 s2
    let leq s1 s2 = Expr.Typed.Set.is_subset s1 ~of_:s2
    let initial = initial end : LATTICE
    with type properties = Expr.Typed.Set.t )

let dual_powerset_lattice_expressions (initial : Expr.Typed.Set.t)
    (total : Expr.Typed.Set.t) =
  ( module struct
    type properties = Expr.Typed.Set.t

    let bottom = total
    let lub s1 s2 = Expr.Typed.Set.inter s1 s2
    let leq s1 s2 = Expr.Typed.Set.is_subset s2 ~of_:s1
    let initial = initial end : LATTICE
    with type properties = Expr.Typed.Set.t )

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

    let initial = Some L.initial end : LATTICE
    with type properties = p option )

(** The lattice (without bottom) of partial functions, ordered under
    inverse graph inclusion, with intersection *)
let dual_partial_function_lattice (type dv cv)
    (module Dom : TOTALTYPE with type vals = dv)
    (module Codom : TYPE with type vals = cv) =
  ( module struct
    type properties = (Dom.vals, Codom.vals) Map.Poly.t

    (* intersection *)
    let lub s1 s2 =
      let f ~key ~data = Map.find s2 key = Some data in
      Map.filteri ~f s1

    let leq s1 s2 =
      Set.for_all Dom.total ~f:(fun k ->
          match (Map.find s1 k, Map.find s2 k) with
          | Some x, Some y -> x = y
          | Some _, None | None, None -> true
          | None, Some _ -> false )

    let initial = Map.Poly.empty end : LATTICE_NO_BOT
    with type properties = (dv, cv) Map.Poly.t )

(** The lattice of partial functions, where we add a fresh bottom element,
   to represent an inconsistent combination of functions *)
let dual_partial_function_lattice_with_bot (type dv cv)
    (module Dom : TOTALTYPE with type vals = dv)
    (module Codom : TYPE with type vals = cv) =
  new_bot (dual_partial_function_lattice (module Dom) (module Codom))

(** A dual powerset lattice, where we set the initial set to be empty *)
let dual_powerset_lattice_empty_initial (type v)
    (module T : TOTALTYPE with type vals = v) =
  dual_powerset_lattice
    ( module struct
      type vals = T.vals

      let initial = Set.Poly.empty
      let total = T.total
    end )

(** A powerset lattice, where we set the initial set to be empty *)
let powerset_lattice_empty_initial (type v) (module T : TYPE with type vals = v)
    =
  powerset_lattice
    ( module struct
      type vals = T.vals

      let initial = Set.Poly.empty
    end )

(** The specific powerset lattice we use for reaching definitions analysis *)
let reaching_definitions_lattice (type v l)
    (module Variables : INITIALTYPE with type vals = v)
    (module Labels : TYPE with type vals = l) =
  powerset_lattice
    ( module struct
      type vals = Variables.vals * Labels.vals option

      let initial = Set.Poly.map ~f:(fun x -> (x, None)) Variables.initial
    end )

(** Lattice for finding the smallest set that satisfies some criterion *)
let minimal_variables_lattice initial_variables =
  powerset_lattice
    ( module struct
      type vals = string

      let initial = initial_variables
    end )

(* The transfer function for a constant propagation analysis *)
let constant_propagation_transfer ?(preserve_stability = false)
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string, Expr.Typed.t) Map.Poly.t option

    let transfer_function l p =
      match p with
      | None -> None
      | Some m ->
          let mir_node = (Map.find_exn flowgraph_to_mir l).pattern in
          Some
            ( match mir_node with
            (* TODO: we are currently only propagating constants for scalars.
               We could do the same for matrix and array expressions if we wanted. *)
            | Assignment ((s, t, []), e) -> (
              match Partial_evaluator.try_eval_expr (subst_expr m e) with
              | { pattern=
                    Promotion ({pattern= Lit (_, _); _}, _, _) | Lit (_, _)
                ; _ } as e'
                when not (preserve_stability && UnsizedType.is_autodiffable t)
                ->
                  Map.set m ~key:s ~data:e'
              | _ -> Map.remove m s )
            | Decl {decl_id= s; _} | Assignment ((s, _, _ :: _), _) ->
                Map.remove m s
            | TargetPE _
             |NRFunApp (_, _)
             |Break | Continue | Return _ | Skip
             |IfElse (_, _, _)
             |While (_, _)
             |For _ | Profile _ | Block _ | SList _ ->
                m ) end : TRANSFER_FUNCTION
    with type labels = int
     and type properties = (string, Expr.Typed.t) Map.Poly.t option )

let label_top_decls
    (flowgraph_to_mir : (int, Middle.Stmt.Located.Non_recursive.t) Map.Poly.t)
    label : string Set.Poly.t =
  let stmt = Map.Poly.find_exn flowgraph_to_mir label in
  match stmt.pattern with
  | Decl {decl_id= s; _} -> Set.Poly.singleton s
  | _ -> Set.Poly.empty

(** The transfer function for an expression propagation analysis,
    AKA forward substitution (see page 396 of Muchnick) *)
let expression_propagation_transfer ?(preserve_stability = false)
    (can_side_effect_expr : Middle.Expr.Typed.t -> bool)
    (flowgraph_to_mir : (int, Middle.Stmt.Located.Non_recursive.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string, Expr.Typed.t) Map.Poly.t option

    let transfer_function l p =
      match p with
      | None -> None
      | Some m ->
          let mir_node = (Map.find_exn flowgraph_to_mir l).pattern in
          let kill_var m v =
            Map.filteri m ~f:(fun ~key ~data ->
                not (key = v || Set.Poly.mem (free_vars_expr data) v) ) in
          Some
            ( match mir_node with
            (* TODO: we are currently only propagating constants for scalars.
               We could do the same for matrix and array expressions if we wanted. *)
            | Middle.Stmt.Fixed.Pattern.Assignment ((s, t, []), e) ->
                let m' = kill_var m s in
                if
                  can_side_effect_expr e
                  || Set.Poly.mem (free_vars_expr e) s
                  || (preserve_stability && UnsizedType.is_autodiffable t)
                then m'
                else Map.set m ~key:s ~data:(subst_expr m e)
            | Decl {decl_id= s; _} | Assignment ((s, _, _ :: _), _) ->
                kill_var m s
            | Profile (_, b) | Block b ->
                let kills =
                  Set.Poly.union_list
                    (List.map ~f:(label_top_decls flowgraph_to_mir) b) in
                Set.Poly.fold kills ~init:m ~f:kill_var
            | TargetPE _
             |NRFunApp (_, _)
             |Break | Continue | Return _ | Skip
             |IfElse (_, _, _)
             |While (_, _)
             |For _ | SList _ ->
                m ) end : TRANSFER_FUNCTION
    with type labels = int
     and type properties = (string, Expr.Typed.t) Map.Poly.t option )

(** The transfer function for a copy propagation analysis *)
let copy_propagation_transfer (globals : string Set.Poly.t)
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string, Expr.Typed.t) Map.Poly.t option

    let transfer_function l p =
      match p with
      | None -> None
      | Some m ->
          let mir_node = (Map.find_exn flowgraph_to_mir l).pattern in
          let kill_var m v =
            Map.filteri m ~f:(fun ~key ~(data : Expr.Typed.t) ->
                not (key = v || data.pattern = Var v) ) in
          Some
            ( match mir_node with
            | Assignment ((s, _, []), {pattern= Var t; meta}) ->
                let m' = kill_var m s in
                if Set.Poly.mem globals s then m'
                else Map.set m' ~key:s ~data:Expr.Fixed.{pattern= Var t; meta}
            | Decl {decl_id= s; _} | Assignment ((s, _, _), _) -> kill_var m s
            | Profile (_, b) | Block b ->
                let kills =
                  Set.Poly.union_list
                    (List.map ~f:(label_top_decls flowgraph_to_mir) b) in
                Set.Poly.fold kills ~init:m ~f:kill_var
            | TargetPE _
             |NRFunApp (_, _)
             |Break | Continue | Return _ | Skip
             |IfElse (_, _, _)
             |While (_, _)
             |For _ | SList _ ->
                m ) end : TRANSFER_FUNCTION
    with type labels = int
     and type properties = (string, Expr.Typed.t) Map.Poly.t option )

(** A helper function for building transfer functions from gen and kill sets *)
let transfer_gen_kill p gen kill = Set.union gen (Set.diff p kill)

(* TODO: from here *)

(** Calculate the set of variables that a statement can assign to *)
let assigned_vars_stmt (s : (Expr.Typed.t, 'a) Stmt.Fixed.Pattern.t) =
  match s with
  | Assignment ((x, _, _), _) -> Set.Poly.singleton x
  | TargetPE _ -> Set.Poly.singleton "target"
  | NRFunApp ((UserDefined (_, FnTarget) | StanLib (_, FnTarget, _)), _) ->
      Set.Poly.singleton "target"
  | For {loopvar= x; _} -> Set.Poly.singleton x
  | Decl {decl_id= _; _}
   |NRFunApp (_, _)
   |Break | Continue | Return _ | Skip
   |IfElse (_, _, _)
   |While (_, _)
   |Profile _ | Block _ | SList _ ->
      Set.Poly.empty

(** Calculate the set of variables that a statement can declare *)
let declared_vars_stmt (s : (Expr.Typed.t, 'a) Stmt.Fixed.Pattern.t) =
  match s with
  | Decl {decl_id= x; _} -> Set.Poly.singleton x
  | _ -> Set.Poly.empty

(** Calculate the set of variables that a statement can assign to or declare *)
let assigned_or_declared_vars_stmt (s : (Expr.Typed.t, 'a) Stmt.Fixed.Pattern.t)
    =
  Set.Poly.union (assigned_vars_stmt s) (declared_vars_stmt s)

(** The transfer function for a reaching definitions analysis *)
let reaching_definitions_transfer
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string * labels option) Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).pattern in
      let gen =
        Set.Poly.map
          ~f:(fun x -> (x, Some l))
          (assigned_or_declared_vars_stmt mir_node) in
      let kill =
        match mir_node with
        | Decl {decl_id= x; _} | Assignment ((x, _, []), _) | For {loopvar= x; _}
          ->
            Set.filter p ~f:(fun (y, _) -> y = x)
        | TargetPE _ -> Set.filter p ~f:(fun (y, _) -> y = "target")
        | NRFunApp ((UserDefined (_, FnTarget) | StanLib (_, FnTarget, _)), _)
          ->
            Set.filter p ~f:(fun (y, _) -> y = "target")
        | NRFunApp (_, _)
         |Break | Continue | Return _ | Skip
         |IfElse (_, _, _)
         |While (_, _)
         |Profile _ | Block _ | SList _ | Assignment _ ->
            Set.Poly.empty in
      transfer_gen_kill p gen kill end : TRANSFER_FUNCTION
    with type labels = int
     and type properties = (string * int option) Set.Poly.t )

(** The transfer function for an initialized variables analysis *)
let initialized_vars_transfer
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = string Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).pattern in
      let gen = assigned_vars_stmt mir_node in
      transfer_gen_kill p gen Set.Poly.empty end : TRANSFER_FUNCTION
    with type labels = int
     and type properties = string Set.Poly.t )

(** The transfer function for a live variables analysis *)
let live_variables_transfer (never_kill : string Set.Poly.t)
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = string Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).pattern in
      let gen = top_free_vars_stmt flowgraph_to_mir mir_node in
      let kill =
        match mir_node with
        | Assignment ((x, _, []), _) | Decl {decl_id= x; _} ->
            Set.Poly.singleton x
        | TargetPE _
         |NRFunApp (_, _)
         |Break | Continue | Return _ | Skip
         |IfElse (_, _, _)
         |While (_, _)
         |For _ | Profile _ | Block _ | SList _
         |Assignment ((_, _, _ :: _), _) ->
            Set.Poly.empty in
      transfer_gen_kill p gen (Set.Poly.diff kill never_kill) end
  : TRANSFER_FUNCTION
    with type labels = int
     and type properties = string Set.Poly.t )

(** Calculate the set of sub-expressions of an expression *)
let rec used_subexpressions_expr (e : Expr.Typed.t) =
  Expr.Typed.Set.union
    (Expr.Typed.Set.singleton e)
    ( match e.pattern with
    | Var _ | Lit (_, _) -> Expr.Typed.Set.empty
    | Promotion (expr, _, _) -> used_subexpressions_expr expr
    | FunApp (k, l) ->
        Expr.Typed.Set.union_list
          (List.map ~f:used_subexpressions_expr (l @ Fun_kind.collect_exprs k))
    | TernaryIf (e1, e2, e3) ->
        Expr.Typed.Set.union_list
          [ used_subexpressions_expr e1; used_subexpressions_expr e2
          ; used_subexpressions_expr e3 ]
    | Indexed (e, l) ->
        Expr.Typed.Set.union_list
          ( used_subexpressions_expr e
          :: List.map ~f:(used_expressions_idx_help used_subexpressions_expr) l
          )
    | EAnd (e1, e2) | EOr (e1, e2) ->
        Expr.Typed.Set.union_list
          [used_subexpressions_expr e1; used_subexpressions_expr e2] )

and used_expressions_idx_help f (i : Expr.Typed.t Index.t) =
  match i with
  | All -> Expr.Typed.Set.empty
  | Single e | Upfrom e | MultiIndex e -> f e
  | Between (e1, e2) -> Expr.Typed.Set.union (f e1) (f e2)

(** Calculate the set of expressions of an expression *)
let used_expressions_expr e = Expr.Typed.Set.singleton e

let rec used_expressions_stmt_help f
    (s : (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t) =
  match s with
  | Assignment ((_, _, []), e) | TargetPE e | Return (Some e) -> f e
  | Assignment ((_, _, l), e) ->
      Expr.Typed.Set.union (f e)
        (Expr.Typed.Set.union_list
           (List.map ~f:(used_expressions_idx_help f) l) )
  | IfElse (e, b1, Some b2) ->
      Expr.Typed.Set.union_list
        [ f e; used_expressions_stmt_help f b1.pattern
        ; used_expressions_stmt_help f b2.pattern ]
  | NRFunApp (k, l) ->
      Expr.Typed.Set.union_list (List.map ~f (l @ Fun_kind.collect_exprs k))
  | Decl _ | Return None | Break | Continue | Skip -> Expr.Typed.Set.empty
  | IfElse (e, b, None) | While (e, b) ->
      Expr.Typed.Set.union (f e) (used_expressions_stmt_help f b.pattern)
  | For {lower= e1; upper= e2; body= b; loopvar= s} ->
      Expr.Typed.Set.union_list
        [ f e1; f e2; used_expressions_stmt_help f b.pattern
        ; Expr.Typed.Set.singleton
            { pattern= Var s
            ; meta=
                Expr.Typed.Meta.
                  {type_= UInt; adlevel= DataOnly; loc= Location_span.empty} }
        ]
  | Profile (_, l) | Block l | SList l ->
      Expr.Typed.Set.union_list
        (List.map ~f:(fun s -> used_expressions_stmt_help f s.pattern) l)

(** Calculate the set of sub-expressions in a statement *)
let used_subexpressions_stmt =
  used_expressions_stmt_help used_subexpressions_expr

(** Calculate the set of expressions in a statement *)
let used_expressions_stmt = used_expressions_stmt_help used_expressions_expr

let top_used_expressions_stmt_help f
    (s : (Expr.Typed.t, int) Stmt.Fixed.Pattern.t) =
  match s with
  | Assignment ((_, _, []), e) | TargetPE e | Return (Some e) -> f e
  | Assignment ((_, _, l), e) ->
      Expr.Typed.Set.union (f e)
        (Expr.Typed.Set.union_list
           (List.map ~f:(used_expressions_idx_help f) l) )
  | While (e, _) | IfElse (e, _, _) -> f e
  | NRFunApp (k, l) ->
      Expr.Typed.Set.union_list (List.map ~f (l @ Fun_kind.collect_exprs k))
  | Profile _ | Block _ | SList _ | Decl _
   |Return None
   |Break | Continue | Skip ->
      Expr.Typed.Set.empty
  | For {lower= e1; upper= e2; _} -> Expr.Typed.Set.union_list [f e1; f e2]

(** Calculate the set of sub-expressions at the top level in a statement *)
let top_used_subexpressions_stmt =
  top_used_expressions_stmt_help used_subexpressions_expr

(** Calculate the set of expressions at the top level in a statement *)
let top_used_expressions_stmt =
  top_used_expressions_stmt_help used_expressions_expr

(** Calculate the subset (of p) of expressions that will need to be recomputed as a
    consequence of evaluating the statement s (because of writes to variables performed
    by s) *)
let killed_expressions_stmt (p : Expr.Typed.Set.t)
    (s : (Expr.Typed.t, int) Stmt.Fixed.Pattern.t) =
  Expr.Typed.Set.filter p ~f:(fun e ->
      let free_vars = free_vars_expr e in
      (* Note: a simple test for membership would be more efficient here,
         but it would require us to duplicate some code. *)
      let assigned_vars = assigned_or_declared_vars_stmt s in
      not (Set.Poly.is_empty (Set.Poly.inter free_vars assigned_vars)) )

(** Calculate the set of subexpressions that needs to be computed at each node
    in the flowgraph *)
let used (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t) =
  Map.Poly.fold flowgraph_to_mir ~init:Map.Poly.empty
    ~f:(fun ~key ~data accum ->
      Map.Poly.set accum ~key ~data:(top_used_subexpressions_stmt data.pattern) )

(* TODO: figure out whether we will also want to reuse the computation of killed *)

(** The transfer function for an anticipated expressions analysis (as a part of lazy
    code motion) *)
let anticipated_expressions_transfer
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t)
    (used : (int, Expr.Typed.Set.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Expr.Typed.Set.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).pattern in
      let gen = Map.Poly.find_exn used l in
      let kill = killed_expressions_stmt p mir_node in
      transfer_gen_kill p gen kill end : TRANSFER_FUNCTION
    with type labels = int
     and type properties = Expr.Typed.Set.t )

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
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t)
    (anticipated_expressions : (int, Expr.Typed.Set.t entry_exit) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Expr.Typed.Set.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).pattern in
      let gen = (Map.find_exn anticipated_expressions l).exit in
      let kill = killed_expressions_stmt (Expr.Typed.Set.union p gen) mir_node in
      transfer_gen_kill_alt p gen kill end : TRANSFER_FUNCTION
    with type labels = int
     and type properties = Expr.Typed.Set.t )

(** Calculates the set of expressions that can be calculated for the first time
    at each node in the flow graph *)
let earliest
    (anticipated_expressions : (int, Expr.Typed.Set.t entry_exit) Map.Poly.t)
    (available_expressions : (int, Expr.Typed.Set.t entry_exit) Map.Poly.t) =
  Map.fold anticipated_expressions ~init:Map.Poly.empty
    ~f:(fun ~key ~data accum ->
      Map.set accum ~key
        ~data:
          (Set.diff data.exit (Map.find_exn available_expressions key).entry) )

(** The transfer function for a postponable expressions analysis (as a part of lazy code motion) *)
let postponable_expressions_transfer
    (earliest : (int, Expr.Typed.Set.t) Map.Poly.t)
    (used : (int, Expr.Typed.Set.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Expr.Typed.Set.t

    let transfer_function l p =
      let gen = Map.find_exn earliest l in
      let kill = Map.find_exn used l in
      transfer_gen_kill_alt p gen kill end : TRANSFER_FUNCTION
    with type labels = int
     and type properties = Expr.Typed.Set.t )

(** Calculates the set of expressions that can be computed at the latest at each node *)
let latest (successors : (int, int Set.Poly.t) Map.Poly.t)
    (earliest : (int, Expr.Typed.Set.t) Map.Poly.t)
    (postponable_expressions : (int, Expr.Typed.Set.t entry_exit) Map.Poly.t)
    (used : (int, Expr.Typed.Set.t) Map.Poly.t) =
  let earliest_or_postponable key =
    Expr.Typed.Set.union
      (Map.Poly.find_exn earliest key)
      (Map.Poly.find_exn postponable_expressions key).entry in
  let latest key =
    Set.filter (earliest_or_postponable key) ~f:(fun e ->
        Set.mem (Map.Poly.find_exn used key) e
        || Set.Poly.exists (Map.Poly.find_exn successors key) ~f:(fun s ->
               not (Set.mem (earliest_or_postponable s) e) ) ) in
  Map.fold successors ~init:Map.Poly.empty ~f:(fun ~key ~data:_ accum ->
      Map.set accum ~key ~data:(latest key) )

(** The transfer function for a used-not-latest expressions analysis, as a part of lazy code motion *)
let used_not_latest_expressions_transfer
    (used : (int, Expr.Typed.Set.t) Map.Poly.t)
    (latest : (int, Expr.Typed.Set.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Expr.Typed.Set.t

    let transfer_function l p =
      let gen = Map.find_exn used l in
      let kill = Map.find_exn latest l in
      transfer_gen_kill_alt p gen kill end : TRANSFER_FUNCTION
    with type labels = int
     and type properties = Expr.Typed.Set.t )

(** The transfer function for the first forward analysis part of determining optimal ad-levels for variables *)
let minimal_variables_fwd_transfer
    (gen_variable :
         (int, Stmt.Located.Non_recursive.t) Map.Poly.t
      -> int
      -> string Set.Poly.t
      -> string Set.Poly.t )
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = string Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).pattern in
      let gen = gen_variable flowgraph_to_mir l p in
      let kill =
        match mir_node with
        (* This probably isn't necessary because Stan doesn't allow shadowing, right? *)
        | Decl {decl_id; decl_adtype= DataOnly; _} -> Set.Poly.singleton decl_id
        | _ -> Set.Poly.empty in
      transfer_gen_kill p gen kill end : TRANSFER_FUNCTION
    with type labels = int
     and type properties = string Set.Poly.t )

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
          T.transfer_function l (Hashtbl.find_exn analysis_in l) in
        if not (L.leq new_analysis_in_l' old_analysis_in_l') then
          let () =
            Hashtbl.set analysis_in ~key:l'
              ~data:(L.lub old_analysis_in_l' new_analysis_in_l') in
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
          F.successors in
      analysis_in_out end : MONOTONE_FRAMEWORK
    with type labels = l
     and type properties = p )

let rec declared_variables_stmt
    (s : (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t) =
  match s with
  | Decl {decl_id= x; _} -> Set.Poly.singleton x
  | Assignment (_, _)
   |TargetPE _
   |NRFunApp (_, _)
   |Break | Continue | Return _ | Skip ->
      Set.Poly.empty
  | IfElse (_, b1, Some b2) ->
      Set.Poly.union
        (declared_variables_stmt b1.pattern)
        (declared_variables_stmt b2.pattern)
  | While (_, b) | IfElse (_, b, None) -> declared_variables_stmt b.pattern
  | For {loopvar= s; body= b; _} ->
      Set.Poly.add (declared_variables_stmt b.pattern) s
  | Profile (_, l) | Block l | SList l ->
      Set.Poly.union_list
        (List.map ~f:(fun x -> declared_variables_stmt x.pattern) l)

let propagation_mfp (prog : Program.Typed.t)
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH with type labels = int)
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t)
    (propagation_transfer :
         (int, Stmt.Located.Non_recursive.t) Map.Poly.t
      -> (module TRANSFER_FUNCTION
            with type labels = int
             and type properties = (string, Expr.Typed.t) Map.Poly.t option ) )
    =
  let mir = Map.find_exn flowgraph_to_mir 1 in
  let domain =
    ( module struct
      type vals = string

      let total =
        Set.Poly.union_list
          [ Set.Poly.of_list (List.map ~f:fst prog.input_vars)
          ; Set.Poly.of_list (List.map ~f:fst prog.output_vars)
          ; declared_variables_stmt
              (stmt_loc_of_stmt_loc_num flowgraph_to_mir mir).pattern ] end
    : TOTALTYPE
      with type vals = string ) in
  let codomain =
    (module struct type vals = Expr.Typed.t end : TYPE
      with type vals = Expr.Typed.t ) in
  let (module Lattice) =
    dual_partial_function_lattice_with_bot domain codomain in
  let (module Transfer) = propagation_transfer flowgraph_to_mir in
  let (module Mf) =
    monotone_framework (module Flowgraph) (module Lattice) (module Transfer)
  in
  Mf.mfp ()

let reaching_definitions_mfp (mir : Program.Typed.t)
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH with type labels = int)
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t) =
  let variables =
    ( module struct
      type vals = string

      let initial =
        Set.Poly.union_list
          [ Set.Poly.of_list (List.map ~f:fst mir.input_vars)
          ; Set.Poly.of_list (List.map ~f:fst mir.output_vars) ] end
    : INITIALTYPE
      with type vals = string ) in
  let labels = (module struct type vals = int end : TYPE with type vals = int) in
  let (module Lattice) = reaching_definitions_lattice variables labels in
  let (module Transfer) = reaching_definitions_transfer flowgraph_to_mir in
  let (module Mf) =
    monotone_framework (module Flowgraph) (module Lattice) (module Transfer)
  in
  Mf.mfp ()

let initialized_vars_mfp (total : string Set.Poly.t)
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH with type labels = int)
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t) =
  let (module Lattice) =
    dual_powerset_lattice_empty_initial
      ( module struct
        type vals = string

        let total = total
      end ) in
  let (module Transfer) = initialized_vars_transfer flowgraph_to_mir in
  let (module Mf) =
    monotone_framework (module Flowgraph) (module Lattice) (module Transfer)
  in
  Mf.mfp ()

let globals (prog : Program.Typed.t) =
  Set.Poly.union_list
    [ Set.Poly.of_list (List.map ~f:fst prog.output_vars)
      (* It is not strictly necessary to exclude data variables from DCE.
         However,
         1. We don't currently check for usage of data variables in
            corners of the MIR, such as in the sizes of parameters
         2. There is code added in codegen that is never represented in
            the MIR that may use data variables as if they're initialized
      *); Set.Poly.of_list (List.map ~f:fst prog.input_vars)
    ; Set.Poly.union_list (List.map ~f:var_declarations prog.prepare_data)
    ; Set.Poly.singleton "target" ]

(** Monotone framework instance for live_variables analysis. Expects reverse
    flowgraph. *)
let live_variables_mfp (prog : Program.Typed.t)
    (module Rev_Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int )
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t) =
  let never_kill = globals prog in
  let variables =
    ( module struct
      type vals = string

      (* NOTE: global generated quantities, (transformed) parameters and target are always observable
         so should be live. *)
      let initial = never_kill end : INITIALTYPE
      with type vals = string ) in
  let (module Lattice) = powerset_lattice variables in
  let (module Transfer) = live_variables_transfer never_kill flowgraph_to_mir in
  let (module Mf) =
    monotone_framework (module Rev_Flowgraph) (module Lattice) (module Transfer)
  in
  Mf.mfp ()

(** Instantiate all four instances of the monotone framework for lazy
    code motion, reusing code between them *)
let lazy_expressions_mfp
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH with type labels = int)
    (module Rev_Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int )
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t) =
  let all_expressions =
    used_subexpressions_stmt
      (stmt_loc_of_stmt_loc_num flowgraph_to_mir
         (Map.Poly.find_exn flowgraph_to_mir 1) )
        .pattern in
  (* TODO: this could probably be done in a nicer way *)
  let used_expr = used flowgraph_to_mir in
  let (module Lattice1) =
    dual_powerset_lattice_expressions Expr.Typed.Set.empty all_expressions in
  let (module Lattice2) = powerset_lattice_expressions Expr.Typed.Set.empty in
  let (module Transfer1) =
    anticipated_expressions_transfer flowgraph_to_mir used_expr in
  let (module Mf1) =
    monotone_framework
      (module Rev_Flowgraph)
      (module Lattice1)
      (module Transfer1) in
  let anticipated_expressions_mfp = Mf1.mfp () in
  let (module Transfer2) =
    available_expressions_transfer flowgraph_to_mir anticipated_expressions_mfp
  in
  let (module Mf2) =
    monotone_framework (module Flowgraph) (module Lattice1) (module Transfer2)
  in
  let available_expressions_mfp = Mf2.mfp () in
  let earliest_expr =
    earliest anticipated_expressions_mfp available_expressions_mfp in
  let (module Transfer3) =
    postponable_expressions_transfer earliest_expr used_expr in
  let (module Mf3) =
    monotone_framework (module Flowgraph) (module Lattice1) (module Transfer3)
  in
  let postponable_expressions_mfp = Mf3.mfp () in
  let latest_expr =
    latest Flowgraph.successors earliest_expr postponable_expressions_mfp
      used_expr in
  let (module Transfer4) =
    used_not_latest_expressions_transfer used_expr latest_expr in
  let (module Mf4) =
    monotone_framework
      (module Rev_Flowgraph)
      (module Lattice2)
      (module Transfer4) in
  let used_not_latest_expressions_mfp = Mf4.mfp () in
  (latest_expr, used_not_latest_expressions_mfp)

(** Run the minimal fixed point algorithm to deduce the smallest set of
 *   variables that satisfy a set of conditions.
 * @param Flowgraph The set of nodes to analyze
 * @param flowgraph_to_mir Map of nodes to their actual values in the MIR
 * @param initial_variables The set of variables to start in the set
 * @param gen_variable Used in the transfer function to deduce variables
 *  that should be in the set
 *
 *)
let minimal_variables_mfp
    (module Circular_Fwd_Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int )
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t)
    (initial_variables : string Set.Poly.t)
    (gen_variable :
         (int, Stmt.Located.Non_recursive.t) Map.Poly.t
      -> int
      -> string Set.Poly.t
      -> string Set.Poly.t ) =
  let (module Lattice) = minimal_variables_lattice initial_variables in
  let (module Transfer) =
    minimal_variables_fwd_transfer gen_variable flowgraph_to_mir in
  let (module Mf) =
    monotone_framework
      (module Circular_Fwd_Flowgraph)
      (module Lattice)
      (module Transfer) in
  Mf.mfp ()
