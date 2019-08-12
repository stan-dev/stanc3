open Core_kernel
open Common

type t = Stmt.Labelled.t
type property = Expr.Labelled.t String.Map.t option

module L :
  Lattice.S
  with type t = Stmt.Labelled.t
   and type property = Expr.Labelled.t String.Map.t option = struct
  type t = Stmt.Labelled.t
  type property = Expr.Labelled.t String.Map.t option

  let lub_helper s1 s2 =
    let f ~key ~data =
      Map.find s2 key |> Option.value_map ~default:false ~f:(fun x -> x = data)
    in
    Map.filteri ~f s1

  let lub p1 p2 =
    match Option.map2 ~f:lub_helper p1 p2 with
    | Some _ as x -> x
    | _ -> Option.first_some p1 p2

  let extremal_value_of _ = Some String.Map.empty
  let least_element_of _ = None

  let leq_helper vs s1 s2 =
    List.for_all vs ~f:(fun k ->
        match (Map.find s1 k, Map.find s2 k) with
        | Some x, Some y -> x = y
        | Some _, None | None, None -> true
        | None, Some _ -> false )

  let leq p1 p2 =
    match (p1, p2) with
    | None, _ -> true
    | Some _, None -> false
    | Some m1, Some m2 ->
        let vs = String.Map.keys m1 @ String.Map.keys m2 in
        leq_helper vs m1 m2
end

module TF :
  Transfer_function.S
  with type t = Stmt.Labelled.t
   and type property = Expr.Labelled.t String.Map.t option = struct
  type t = Stmt.Labelled.t
  type property = Expr.Labelled.t String.Map.t option

  let all_properties_of _ = None

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
