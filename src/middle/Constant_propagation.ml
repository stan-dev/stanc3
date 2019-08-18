(** Constant Propagation Analysis

For each program point, whether or not a variable has a constant value whenever
execution reaches that point

*)
open Core_kernel

open Common

type t = Stmt.Labelled.t
type property = Expr.Labelled.t String.Map.t option

module L = Lattice.Make_from_unbounded (struct
  type t = Stmt.Labelled.t
  type property = Expr.Labelled.t String.Map.t

  let join s1 s2 =
    let f ~key ~data =
      Map.find s2 key |> Option.value_map ~default:false ~f:(fun x -> x = data)
    in
    Map.filteri ~f s1

  let leq s1 s2 =
    let vs = String.Map.keys s1 @ String.Map.keys s2 in
    List.for_all vs ~f:(fun k ->
        match (Map.find s1 k, Map.find s2 k) with
        | Some x, Some y -> x = y
        | Some _, None | None, None -> true
        | None, Some _ -> false )
end)

module TF :
  Transfer_function.S
  with type t = Stmt.Labelled.t
   and type property = Expr.Labelled.t String.Map.t option = struct
  type t = Stmt.Labelled.t
  type property = Expr.Labelled.t String.Map.t option

  let all_properties_of _ = None
  let extremal_value_of _ = Some String.Map.empty

  let apply (_ : property) (t : t) (property : property) : property =
    let open Expr.Labelled in
    Option.map property ~f:(fun env ->
        match Stmt.Fixed.pattern t with
        | Assignment ((vbl, []), expr) -> (
          match Expr_helpers.eval ~type_of ~adlevel_of ~equal ~env expr with
          | expr' when Expr_helpers.is_lit expr' ->
              String.Map.set env ~key:vbl ~data:expr'
          | _ -> String.Map.remove env vbl )
        | Decl {decl_id= vbl; _} | Assignment ((vbl, _), _) ->
            String.Map.remove env vbl
        | _ -> env )
end

include Monotone_framework.Make (Stmt_flowgraph.Forward) (L) (TF)

(** Substitute arithmetic expressions within a statement and partially evaluate *)
let apply_constants stmt env =
  let open Expr.Labelled in
  Stmt.Fixed.transform_bottom_up
    Expr_helpers.(eval ~type_of ~adlevel_of ~equal ~env)
    Fn.id stmt

(** Apply constant propagation, recovering the transformed statement *)
let apply (associations, analysis) =
  associations
  |> Int_label.Map.mapi ~f:(fun ~key ~data ->
         Int_label.Map.find analysis key
         |> Option.bind ~f:Monotone_framework.entry
         |> Option.value_map ~default:data ~f:(apply_constants data) )
  |> Stmt_flowgraph.Forward.t_of_associations

let optimize flowgraph_info stmt =
  solve flowgraph_info stmt |> apply |> Option.value ~default:stmt
