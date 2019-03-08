(** The common elements of a monotone framework *)

open Core_kernel
open Monotone_framework_sigs

(* TODO: write instance of FLOWGRAPH for Stan flowgraph of Stan MIR *)

(** Reverse flowgraphs to be used for reverse analyses.
    Observe that this respects the invariants listed for a FLOWGRAPH *)
module Reverse (F : FLOWGRAPH) : FLOWGRAPH = struct
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

module Powerset_lattice (S : INITIALTYPE) : LATTICE = struct
  type properties = S.vals Set.Poly.t

  let bottom = Set.Poly.empty
  let lub s1 s2 = Set.Poly.union s1 s2
  let leq s1 s2 = Set.Poly.is_subset s1 ~of_:s2
  let initial = S.initial
end

module Dual_powerset_lattice (S : INITIALTOTALTYPE) : LATTICE = struct
  type properties = S.vals Set.Poly.t

  let bottom = S.total
  let lub s1 s2 = Set.Poly.inter s1 s2
  let leq s1 s2 = Set.Poly.is_subset s2 ~of_:s1
  let initial = S.initial
end

module New_bot (L : LATTICE) : LATTICE = struct
  type properties = L.properties option

  let bottom = None

  let lub = function
    | Some s1 -> ( function Some s2 -> Some (L.lub s1 s2) | None -> Some s1 )
    | None -> fun x -> x

  let leq = function
    | Some s1 -> ( function Some s2 -> L.leq s1 s2 | None -> false )
    | None -> fun _ -> true

  let initial = Some L.initial
end

module Dual_partial_function_lattice (Dom : INITIALTYPE) (Codom : TYPE) :
  LATTICE = struct
  type properties = (Dom.vals, Codom.vals) Map.Poly.t

  let bottom = Errors.fatal_error ()

  let lub s1 s2 =
    let f ~key ~data = Map.find s2 key = Some data in
    Map.filteri ~f s1

  let leq s1 s2 =
    Set.for_all Dom.initial ~f:(fun k ->
        match (Map.find s1 k, Map.find s2 k) with
        | Some x, Some y -> x = y
        | Some _, None | None, None -> true
        | None, Some _ -> false )

  let initial = Map.Poly.empty
end

(* To use for constant propagation analysis *)
module Dual_partial_function_lattice_with_bot
    (Dom : INITIALTYPE)
    (Codom : TYPE) : LATTICE =
  New_bot (Dual_partial_function_lattice (Dom) (Codom))

(* To use for very busy expressions (anticipated expressions)
              available expressions
              postponable expresions
   analyses *)
module Dual_powerset_lattice_empty_initial (T : TOTALTYPE) : LATTICE =
Dual_powerset_lattice (struct
  type vals = T.vals

  let initial = Set.Poly.empty
  let total = T.total
end)

(* To use for used expressions
              live variables
   analyses *)
module Powerset_lattice_empty_initial (T : TYPE) : LATTICE =
Powerset_lattice (struct
  type vals = T.vals

  let initial = Set.Poly.empty
end)

(* To use for reaching definitions analysis *)
module Reaching_definitions_lattice (Variables : INITIALTYPE) (Labels : TYPE) :
  LATTICE = Powerset_lattice (struct
  type vals = Variables.vals * Labels.vals option

  let initial = Set.Poly.map ~f:(fun x -> (x, None)) Variables.initial
end)

let constant_propagation_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string, Mir.expr) Map.Poly.t option

    let transfer_function l p =
      match p with
      | None -> None
      | Some m ->
          let mir_node = (Map.find_exn flowgraph_to_mir l).stmt in
          Some
            ( match mir_node with
            (* TODO: we are currently only propagating constants for scalars.
             We could do the same for matrix and array expressions if we wanted. *)
            | Mir.Assignment (Var s, Mir.Lit (t, v)) ->
                Map.set m ~key:s ~data:(Mir.Lit (t, v))
            | Mir.Decl {decl_id= s; _} | Mir.Assignment (Var s, _) ->
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
  : TRANSFER_FUNCTION )

let copy_propagation_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string, string) Map.Poly.t option

    let transfer_function l p =
      match p with
      | None -> None
      | Some m ->
          let mir_node = (Map.find_exn flowgraph_to_mir l).stmt in
          Some
            ( match mir_node with
            | Mir.Assignment (Var s, Mir.Var t) -> Map.set m ~key:s ~data:t
            | Mir.Decl {decl_id= s; _} | Mir.Assignment (Var s, _) ->
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
  : TRANSFER_FUNCTION )

let transfer_gen_kill p gen kill = Set.union gen (Set.diff p kill)

let assigned_vars_stmt (s : Mir.stmt_loc Mir.statement) =
  match s with
  | Mir.Assignment (Var x, _)
   |Mir.Assignment (Indexed (Var x, _), _)
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
   |Mir.For _ | Mir.Block _ | Mir.SList _ | Mir.Decl _ ->
      Set.Poly.empty

let reaching_definitions_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = (string * labels option) Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmt in
      let gen =
        Set.Poly.map ~f:(fun x -> (x, Some l)) (assigned_vars_stmt mir_node)
      in
      let kill =
        match mir_node with
        | Mir.Decl {decl_id= x; _}
         |Mir.Assignment (Var x, _)
         |Mir.Assignment (Indexed (Var x, _), _)
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
  : TRANSFER_FUNCTION )

(* TODO: insert Ryan's implementations here? *)
let rec free_vars_expr (e : Mir.expr) =
  match e with
  | Mir.Var x -> Set.Poly.singleton x
  | Mir.Lit (_, _) -> Set.Poly.empty
  | Mir.FunApp (f, l) ->
      Set.Poly.union_list (Set.Poly.singleton f :: List.map ~f:free_vars_expr l)
  | Mir.BinOp (e1, _, e2) ->
      Set.Poly.union (free_vars_expr e1) (free_vars_expr e2)
  | Mir.TernaryIf (e1, e2, e3) ->
      Set.Poly.union_list (List.map ~f:free_vars_expr [e1; e2; e3])
  | Mir.Indexed (e, l) ->
      Set.Poly.union_list (free_vars_expr e :: List.map ~f:free_vars_idx l)

and free_vars_idx (i : Mir.index) =
  match i with
  | Mir.All -> Set.Poly.empty
  | Mir.Single e | Mir.Upfrom e | Mir.Downfrom e | Mir.MultiIndex e ->
      free_vars_expr e
  | Mir.Between (e1, e2) ->
      Set.Poly.union (free_vars_expr e1) (free_vars_expr e2)

let rec free_vars_stmt (s : Mir.stmt_loc Mir.statement) =
  match s with
  | Mir.Assignment (Var _, e) | Mir.Return (Some e) | Mir.TargetPE e ->
      free_vars_expr e
  | Mir.Assignment (Indexed (Var _, l), e) ->
      Set.Poly.union_list (free_vars_expr e :: List.map ~f:free_vars_idx l)
  | Mir.Assignment _ -> Errors.fatal_error ()
  | Mir.NRFunApp (f, l) ->
      Set.Poly.union_list (Set.Poly.singleton f :: List.map ~f:free_vars_expr l)
  | Mir.Check {ccfunname= f; ccvid= g; ccargs= l; _} ->
      Set.Poly.union_list
        ( Set.Poly.singleton f :: Set.Poly.singleton g
        :: List.map ~f:free_vars_expr l )
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
let top_free_vars_stmt (s : Mir.stmt_loc Mir.statement) =
  match s with
  | Mir.Assignment _ | Mir.Return _ | Mir.TargetPE _ | Mir.Check _
   |Mir.NRFunApp _ | Mir.FunDef _ | Mir.Decl _ | Mir.Break | Mir.Continue
   |Mir.Skip ->
      free_vars_stmt s
  | Mir.While (e, _) | Mir.IfElse (e, _, _) -> free_vars_expr e
  | Mir.For {lower= e1; upper= e2; _} ->
      Set.Poly.union_list [free_vars_expr e1; free_vars_expr e2]
  | Mir.Block _ | Mir.SList _ -> Set.Poly.empty

(* TODO: in the live variables analysis, we probably want to assume that
   target, parameters, transformed parameters and generated quantities are
   always live.
   We could either do that by initializing suitably or by just adding
   them later in the dead code elimination pass. *)
let live_variables_transfer (flowgraph_to_mir : (int, Mir.stmt_loc) Map.Poly.t)
    =
  ( module struct
    type labels = int
    type properties = string Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmt in
      let gen = top_free_vars_stmt mir_node in
      let kill =
        match mir_node with
        | Mir.Assignment (Var x, _)
         |Mir.Assignment (Indexed (Var x, _), _)
         |Mir.Decl {decl_id= x; _} ->
            Set.Poly.singleton x
        | Mir.Assignment _ -> Errors.fatal_error ()
        | Mir.TargetPE _
         |Mir.NRFunApp (_, _)
         |Mir.Check _ | Mir.Break | Mir.Continue | Mir.Return _ | Mir.Skip
         |Mir.IfElse (_, _, _)
         |Mir.While (_, _)
         |Mir.For _ | Mir.Block _ | Mir.SList _ | Mir.FunDef _ ->
            Set.Poly.empty
      in
      transfer_gen_kill p gen kill
  end
  : TRANSFER_FUNCTION )

(* Note: we do not count constants or variables are expressions
   (as there is no computation needed for them, so they do not
   need to take part in any optimizations for expression placement).*)
let rec used_expressions_expr (e : Mir.expr) =
  Set.Poly.union (Set.Poly.singleton e)
    ( match e with
    | Mir.Var _ | Mir.Lit (_, _) -> Set.Poly.empty
    | Mir.FunApp (_, l) ->
        Set.Poly.union_list (List.map ~f:used_expressions_expr l)
    | Mir.BinOp (e1, _, e2) ->
        Set.Poly.union_list [used_expressions_expr e1; used_expressions_expr e2]
    | Mir.TernaryIf (e1, e2, e3) ->
        Set.Poly.union_list
          [ used_expressions_expr e1; used_expressions_expr e2
          ; used_expressions_expr e3 ]
    | Mir.Indexed (e, l) ->
        Set.Poly.union_list
          (used_expressions_expr e :: List.map ~f:used_expressions_idx l) )

and used_expressions_idx (i : Mir.index) =
  match i with
  | Mir.All -> Set.Poly.empty
  | Mir.Single e | Mir.Upfrom e | Mir.Downfrom e | Mir.MultiIndex e ->
      used_expressions_expr e
  | Mir.Between (e1, e2) ->
      Set.Poly.union (used_expressions_expr e1) (used_expressions_expr e2)

let rec used_expressions_stmt (s : Mir.stmt_loc Mir.statement) =
  match s with
  | Mir.Assignment (Var _, e) | Mir.TargetPE e | Mir.Return (Some e) ->
      used_expressions_expr e
  | Mir.Assignment (Indexed (Var _, l), e) ->
      Set.Poly.union (used_expressions_expr e)
        (Set.Poly.union_list (List.map ~f:used_expressions_idx l))
  | Mir.Assignment _ -> Errors.fatal_error ()
  | Mir.IfElse (e, b1, Some b2) ->
      Set.Poly.union_list
        [ used_expressions_expr e
        ; used_expressions_stmt b1.stmt
        ; used_expressions_stmt b2.stmt ]
  | Mir.NRFunApp (_, l) | Mir.Check {ccargs= l; _} ->
      Set.Poly.union_list (List.map ~f:used_expressions_expr l)
  | Mir.Decl _
   |Mir.Return None
   |Mir.Break | Mir.Continue | Mir.FunDef _ | Mir.Skip ->
      Set.Poly.empty
  | Mir.IfElse (e, b, None) | Mir.While (e, b) ->
      Set.Poly.union (used_expressions_expr e) (used_expressions_stmt b.stmt)
  | Mir.For {lower= e1; upper= e2; body= b; loopvar= s} ->
      Set.Poly.union_list
        [ used_expressions_expr e1; used_expressions_expr e2
        ; used_expressions_stmt b.stmt
        ; Set.Poly.singleton s ]
  | Mir.Block l | Mir.SList l ->
      Set.Poly.union_list
        (List.map ~f:(fun s -> used_expressions_stmt s.stmt) l)

(** A variant of used_expressions_stmt where we do not count uses of expressions
    recursively within substatements *)
let top_used_expressions_stmt (s : Mir.stmt_loc Mir.statement) =
  match s with
  | Mir.Assignment _ | Mir.Return _ | Mir.TargetPE _ | Mir.Check _
   |Mir.NRFunApp _ | Mir.FunDef _ | Mir.Decl _ | Mir.Break | Mir.Continue
   |Mir.Skip ->
      used_expressions_stmt s
  | Mir.While (e, _) | Mir.IfElse (e, _, _) -> used_expressions_expr e
  | Mir.For {lower= e1; upper= e2; _} ->
      Set.Poly.union_list [used_expressions_expr e1; used_expressions_expr e2]
  | Mir.Block _ | Mir.SList _ -> Set.Poly.empty

let killed_expressions_stmt (p : Mir.expr Set.Poly.t)
    (s : Mir.stmt_loc Mir.statement) =
  Set.Poly.filter p ~f:(fun e ->
      let free_vars = free_vars_expr e in
      let assigned_vars = assigned_vars_stmt s in
      not (Set.is_empty (Set.Poly.inter free_vars assigned_vars)) )

let anticipated_expressions_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Mir.expr Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmt in
      let gen = top_used_expressions_stmt mir_node in
      let kill = killed_expressions_stmt p mir_node in
      transfer_gen_kill p gen kill
  end
  : TRANSFER_FUNCTION )

let transfer_gen_kill_alt p gen kill =
  Set.Poly.diff (Set.Poly.union p gen) kill

(* NOTE: we want to implement lazy code motion. Aho describes a slightly
   more general available expression pass for that that uses the anticipated
   expression pass. *)
let available_expressions_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc) Map.Poly.t)
    (anticipated_expressions :
      (int, Mir.expr Set.Poly.t entry_exit) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Mir.expr Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmt in
      let gen = (Map.find_exn anticipated_expressions l).entry in
      let kill = killed_expressions_stmt p mir_node in
      transfer_gen_kill_alt p gen kill
  end
  : TRANSFER_FUNCTION )

let postponable_expressions_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc) Map.Poly.t)
    (earliest : (int, Mir.expr Set.Poly.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Mir.expr Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmt in
      let gen = Map.find_exn earliest l in
      let kill = top_used_expressions_stmt mir_node in
      transfer_gen_kill_alt p gen kill
  end
  : TRANSFER_FUNCTION )

let used_expressions_transfer
    (flowgraph_to_mir : (int, Mir.stmt_loc) Map.Poly.t)
    (latest : (int, Mir.expr Set.Poly.t) Map.Poly.t) =
  ( module struct
    type labels = int
    type properties = Mir.expr Set.Poly.t

    let transfer_function l p =
      let mir_node = (Map.find_exn flowgraph_to_mir l).stmt in
      let gen = top_used_expressions_stmt mir_node in
      let kill = Map.find_exn latest l in
      transfer_gen_kill_alt p gen kill
  end
  : TRANSFER_FUNCTION )

module Monotone_framework : MONOTONE_FRAMEWORK =
functor
  (F : FLOWGRAPH)
  (L : LATTICE)
  (T :
     TRANSFER_FUNCTION
     with type labels = F.labels
      and type properties = L.properties)
  ->
  struct
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
          let old_analysis_out_l' = Hashtbl.find_exn analysis_in l' in
          let new_analysis_out_l' =
            T.transfer_function l (Hashtbl.find_exn analysis_in l)
          in
          let _ =
            if not (L.leq new_analysis_out_l' old_analysis_out_l') then
              Hashtbl.set analysis_in ~key:l'
                ~data:(L.lub old_analysis_out_l' new_analysis_out_l')
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
