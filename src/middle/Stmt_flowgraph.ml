open Core_kernel
open Common
open Stmt

(* -- Helpers --------------------------------------------------------------- *)

(** Get the lables of the initial statement in a statement *)
let rec initial stmt =
  let cur_label = Labelled.label_of stmt in
  match Fixed.pattern stmt with
  | Assignment _ | Skip | IfElse (_, _, _) | While (_, _) -> cur_label
  | Block xs -> initial @@ List.hd_exn xs
  | _ -> cur_label

(** Get the labels of the possible final statements in a statement *)
let rec finals ?init:(accu = Int_label.Set.empty) stmt =
  let cur_label = Labelled.label_of stmt in
  match Fixed.pattern stmt with
  | Assignment _ | Skip -> Int_label.Set.add accu cur_label
  | IfElse (_, ts, Some fs) -> finals ~init:(finals ~init:accu fs) ts
  | IfElse (_, ts, _) -> finals ~init:accu ts
  | While (_, _) -> Int_label.Set.add accu cur_label
  | Block xs -> (
    match List.last xs with Some x -> finals ~init:accu x | _ -> accu )
  | _ -> accu

(** Build the flowgraph of a statment in terms of `initial` and `finals` *)
let rec flow ?init:(accu = []) stmt =
  let cur_label = Labelled.label_of stmt in
  match Fixed.pattern stmt with
  | Assignment _ | Skip -> accu
  | IfElse (_, ts, Some fs) ->
      let init_ts = initial ts and init_fs = initial fs in
      flow
        ~init:
          (flow ~init:((cur_label, init_ts) :: (cur_label, init_fs) :: accu) fs)
        ts
  | IfElse (_, ts, _) ->
      let init_ts = initial ts in
      flow ~init:((cur_label, init_ts) :: accu) ts
  | While (_, body) ->
      let init_body = initial body and finals_body = finals body in
      let finals =
        List.map ~f:(fun l' -> (l', cur_label))
        @@ Int_label.Set.to_list finals_body
      in
      flow ~init:(((cur_label, init_body) :: finals) @ accu) body
  | Block xs -> pairwise ~init:accu xs
  | _ -> accu

and pairwise ~init = function
  | x :: y :: xs ->
      let init_y = initial y in
      let pairs =
        finals x |> Int_label.Set.to_list |> List.map ~f:(fun l -> (l, init_y))
      in
      pairwise ~init:(flow ~init:(init @ pairs) x) (y :: xs)
  | [x] -> flow ~init x
  | [] -> init

(** Subsitute statements within a statement *)
let subst_stmt env stmt =
  Fixed.transform_bottom_up Fn.id
    (fun stmt ->
      Int_label.Map.find env @@ Labelled.label_of stmt
      |> Option.value ~default:stmt )
    stmt

(** Recover a statement from it's flowgraph *)
let recover assocs =
  Int_label.Map.fold_right assocs
    ~f:(fun ~key ~data accu_opt ->
      match accu_opt with
      | Some (_, env) ->
          let stmt' = subst_stmt env data in
          let env' = Int_label.Map.add_exn env ~key ~data in
          Some (stmt', env')
      | _ -> Some (data, Int_label.Map.add_exn Int_label.Map.empty ~key ~data)
      )
    ~init:None
  |> Option.map ~f:fst

(* -- Forward and reverse flowgraphs ---------------------------------------- *)

module Basic :
  Flowgraph.Basic with type t = Labelled.t and module Label = Int_label =
struct
  type t = Labelled.t

  module Label = Int_label

  let initial_label_of_t x = initial x
  let final_labels_of_t x = finals x
  let flow_of_t x = flow x
  let t_of_associations f = recover f

  let associations_of_t x =
    let {Labelled.stmts; _} = Labelled.associate x in
    stmts
end

module Forward = Flowgraph.Make (Basic)
module Reverse = Flowgraph.Make_reverse (Basic)
