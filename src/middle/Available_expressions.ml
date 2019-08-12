open Core_kernel
open Common

type t = Stmt.Labelled.t

module Label = Int_label
module Property = Expr.Labelled.Set

type property = Property.t

module KillGen :
  Transfer_function.Kill_gen
  with type t = Stmt.Labelled.t
   and type property = Property.t = struct
  type t = Stmt.Labelled.t
  type property = Property.t

  let gen (stmt : t) =
    match Stmt.Fixed.pattern stmt with
    | IfElse (test, _, _) | While (test, _) ->
        Expr.Labelled.associate test
        |> Int_label.Map.to_alist
        |> List.filter_map ~f:(fun (_, expr) ->
               if Expr_helpers.is_trivial expr then None else Some expr )
        |> Property.of_list
    | Assignment ((x, []), rhs) ->
        Expr.Labelled.associate rhs
        |> Int_label.Map.to_alist
        |> List.filter_map ~f:(fun (_, expr) ->
               if Expr_helpers.is_trivial expr then None
               else
                 let free_vars = Expr_helpers.free_vars expr in
                 if String.Set.mem free_vars x then None else Some expr )
        |> Property.of_list
    | _ -> Property.empty

  let all_properties_of stmt =
    let assocs = Stmt.Labelled.associate stmt in
    Int_label.Map.to_alist assocs.exprs |> List.map ~f:snd |> Property.of_list

  let kill prop_star stmt =
    match Stmt.Fixed.pattern stmt with
    | Assignment ((x, []), _) ->
        prop_star
        |> Property.filter ~f:(fun expr ->
               let free_vars = Expr_helpers.free_vars expr in
               String.Set.mem free_vars x )
    | _ -> Property.empty

  let diff = Property.diff
  let union = Property.union
end

module TF = Transfer_function.Make_using_kill_gen (KillGen)

module L :
  Lattice.S with type t = Stmt.Labelled.t and type property = Property.t =
struct
  type t = Stmt.Labelled.t
  type property = Property.t

  let least_element_of x =
    let assocs = Stmt.Labelled.associate x in
    Int_label.Map.to_alist assocs.exprs |> List.map ~f:snd |> Property.of_list

  let extremal_value_of (_ : t) = Property.empty
  let leq a b = Property.is_subset b ~of_:a
  let lub a b = Property.inter a b
end

include Monotone_framework.Make (Stmt_flowgraph.Forward) (L) (TF)
