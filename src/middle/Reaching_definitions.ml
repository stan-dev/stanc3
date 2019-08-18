(** 
Reaching Definitions Analysis

For each program point, which assignments _may_ have been made and not 
overwritten, when program execution reaches this point along some path.
*)

open Core_kernel
open Common

type t = Stmt.Labelled.t

module Assignment : Label.S with type t = string * int option = struct
  module Basic = struct
    type t = string * int option [@@deriving compare, hash, sexp]

    let init = ("a", None)
    let next label = label

    let pp ppf (nm, ix) =
      match ix with
      | Some i -> Fmt.(pf ppf {|%s %d|} nm i)
      | _ -> Fmt.(pf ppf {|%s (none)|} nm)

    include Comparator.Make (struct
      type nonrec t = t

      let compare = compare
      let sexp_of_t = sexp_of_t
    end)
  end

  include Basic
  include Comparable.Make_using_comparator (Basic)
end

type property = Assignment.Set.t

module Kill_gen :
  Transfer_function.Kill_gen
  with type t = Stmt.Labelled.t
   and type property = Assignment.Set.t = struct
  type t = Stmt.Labelled.t
  type property = Assignment.Set.t

  let kill prop_star stmt =
    match Stmt.Fixed.pattern stmt with
    | Decl {decl_id= x; _} | Assignment ((x, []), _) | For {loopvar= x; _} ->
        Assignment.Set.(add (filter prop_star ~f:(fun (y, _) -> y = x)))
          (x, None)
    | TargetPE _ ->
        Assignment.Set.(
          add
            (filter prop_star ~f:(fun (y, _) -> y = "target"))
            ("target", None))
    | NRFunApp (_, name, _) when String.suffix name 3 = "_lp" ->
        Assignment.Set.(
          add
            (filter prop_star ~f:(fun (y, _) -> y = "target"))
            ("target", None))
    | NRFunApp _ | Break | Continue | Skip | Return _ | IfElse _ | While _
     |Block _ | SList _ | Assignment _ ->
        Assignment.Set.empty

  let gen stmt =
    match Stmt.Fixed.pattern stmt with
    | Decl {decl_id= x; _} | Assignment ((x, []), _) | For {loopvar= x; _} ->
        Assignment.Set.singleton (x, Some (Stmt.Labelled.label_of stmt))
    | TargetPE _ ->
        Assignment.Set.singleton ("target", Some (Stmt.Labelled.label_of stmt))
    | NRFunApp (_, name, _) when String.suffix name 3 = "_lp" ->
        Assignment.Set.singleton ("target", Some (Stmt.Labelled.label_of stmt))
    | NRFunApp _ | Break | Continue | Skip | Return _ | IfElse _ | While _
     |Block _ | SList _ | Assignment _ ->
        Assignment.Set.empty

  let all_properties_of stmt =
    let expr_algebra _ = Assignment.Set.empty
    and stmt_algebra : ('a, 'b, Assignment.Set.t) Stmt.Fixed.algebra = function
      | meta, Decl {decl_id= x; _}
       |meta, Assignment ((x, []), _)
       |meta, For {loopvar= x; _} ->
          let label = Stmt.Labelled.Meta.label meta in
          Assignment.Set.singleton (x, Some label)
      | meta, TargetPE _ ->
          let label = Stmt.Labelled.Meta.label meta in
          Assignment.Set.singleton ("target", Some label)
      | meta, NRFunApp (_, name, _) when String.suffix name 3 = "_lp" ->
          let label = Stmt.Labelled.Meta.label meta in
          Assignment.Set.singleton ("target", Some label)
      | _, NRFunApp _
       |_, Break
       |_, Continue
       |_, Skip
       |_, Return _
       |_, IfElse _
       |_, While _
       |_, Block _
       |_, SList _
       |_, Assignment _ ->
          Assignment.Set.empty
    in
    Stmt.Fixed.cata expr_algebra stmt_algebra stmt

  let diff p1 p2 = Assignment.Set.diff p1 p2
  let union p1 p2 = Assignment.Set.union p1 p2

  let extremal_value_of stmt =
    Stmt_helpers.free_vars stmt
    |> String.Set.to_list
    |> List.map ~f:(fun var -> (var, None))
    |> Assignment.Set.of_list
end

module TF = Transfer_function.Make_using_kill_gen (Kill_gen)

module L = Lattice.Make_powerset (struct
  type t = Stmt.Labelled.t

  module Property = Assignment
end)

include Monotone_framework.Make (Stmt_flowgraph.Forward) (L) (TF)
