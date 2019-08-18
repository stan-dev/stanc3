(** Available Expressions Analysis

For each program point, which expressions _must_ have already been computed, and
not later modified, on all paths to the program point
*)
open Core_kernel

open Common

type t = Stmt.Labelled.t

module Label = Int_label

type property = Expr.Labelled.Set.t

module KillGen :
  Transfer_function.Kill_gen
  with type t = Stmt.Labelled.t
   and type property = Expr.Labelled.Set.t = struct
  type t = Stmt.Labelled.t
  type property = Expr.Labelled.Set.t

  let gen (stmt : t) =
    match Stmt.Fixed.pattern stmt with
    | IfElse (test, _, _) | While (test, _) ->
        Expr.Labelled.associate test
        |> Int_label.Map.data
        |> List.filter_map ~f:(fun expr ->
               if Expr_helpers.is_trivial expr then None else Some expr )
        |> Expr.Labelled.Set.of_list
    | Assignment ((x, []), rhs) ->
        Expr.Labelled.associate rhs
        |> Int_label.Map.to_alist
        |> List.filter_map ~f:(fun (_, expr) ->
               if Expr_helpers.is_trivial expr then None
               else
                 let free_vars = Expr_helpers.free_vars expr in
                 if String.Set.mem free_vars x then None else Some expr )
        |> Expr.Labelled.Set.of_list
    | _ -> Expr.Labelled.Set.empty

  let all_properties_of stmt =
    let assocs = Stmt.Labelled.associate stmt in
    Int_label.Map.to_alist assocs.exprs
    |> List.map ~f:snd |> Expr.Labelled.Set.of_list

  let kill prop_star stmt =
    match Stmt.Fixed.pattern stmt with
    | Assignment ((x, []), _) ->
        prop_star
        |> Expr.Labelled.Set.filter ~f:(fun expr ->
               let free_vars = Expr_helpers.free_vars expr in
               String.Set.mem free_vars x )
    | _ -> Expr.Labelled.Set.empty

  let diff = Expr.Labelled.Set.diff
  let union = Expr.Labelled.Set.union
  let extremal_value_of (_ : t) = Expr.Labelled.Set.empty
end

module TF = Transfer_function.Make_using_kill_gen (KillGen)

module L = Lattice.Make_dual_powerset (struct
  type t = Stmt.Labelled.t

  module Property = Expr.Labelled

  let greatest_element_of stmt =
    let assocs = Stmt.Labelled.associate stmt in
    Int_label.Map.to_alist assocs.exprs
    |> List.map ~f:snd |> Expr.Labelled.Set.of_list
end)

include Monotone_framework.Make (Stmt_flowgraph.Forward) (L) (TF)
