open Core_kernel
open Common
open Stmt

(* -- Helpers --------------------------------------------------------------- *)

(** Get the lables of the initial statement in a statement 
initial ( [Assignment ((id,index), rhs)]^l ) = l 
initial ( [TargetPE(e)]^l)                   = l
initial ( [NRFunApp(kind,name,args)]^l )     = l
initial ( [Break]^l )                        = l
initial ( [Continue]^l )                     = l 
initial ( [Skip]^l )                         = l
initial ( [Return (e?) ]^l )                 = l 
initial ( [IfElse (b, S1 , S2?)]^l )         = l
initial ( [While (b, S)]^l                   = l
initial ( [Block (S::_)]^l )                 = l
initial ( [SList (S::_)]^l )                 = l
initial ( [For { v ; lb; ub ; S }]^l )       = l

*)
let rec initial stmt =
  let cur_label = Labelled.label_of stmt in
  match Fixed.pattern stmt with
  | SList xs | Block xs -> initial @@ List.hd_exn xs
  | Assignment _ | TargetPE _ | NRFunApp _ | Break | Continue | Return _
   |Decl _ | Skip | IfElse _ | While _ | For _ ->
      cur_label

(** 
Get the labels of the possible final statements in a statement 

Using the Nielsen et al notation, we have:

final ( [Assignment ((id,index), rhs)]^l ) = { l } 
final ( [TargetPE(e)]^l)                   = { l }
final ( [NRFunApp(kind,name,args)]^l )     = { l }
final ( [Decl{adtype;id;type}]^l )         = { l }
final ( [Break]^l )                        = { l }
final ( [Continue]^l )                     = { l }
final ( [Skip]^l )                         = { l }
final ( [Return (e?) ]^l )                 = { l }
final ( [IfElse (b, S1 , S2?)]^l )         = final (S1) ∪ final (S2)
final ( [Block XS]^l )                     = final (last XS) ∪ continue(Block XS)
final ( [SList  XS]^l )                    = final (last XS) ∪ continue(SList XS)
final ( [While (b, S) )]^l                 = { l } ∪ break-ret(S)
final ( [For { v ; lb; ub; S }]^l )        = { l } ∪ break-ret(S)

*)
let rec finals ?init:(accu = Int_label.Set.empty) stmt =
  let cur_label = Labelled.label_of stmt in
  match Fixed.pattern stmt with
  | IfElse (_, ts, fs_opt) ->
      finals ts
        ~init:(Option.value_map ~default:accu ~f:(finals ~init:accu) fs_opt)
  | While (_, body) | For {body; _} ->
      break_return (Int_label.Set.add accu cur_label) body
  | SList xs | Block xs -> (
    match List.last xs with
    | Some x -> continue (finals ~init:accu x) stmt
    | _ -> accu )
  | Assignment _ | TargetPE _ | NRFunApp _ | Break | Continue | Return _
   |Decl _ | Skip ->
      Int_label.Set.add accu cur_label

(**
break-ret ( [Assignment ((id,index), rhs)]^l ) = { } 
break-ret ( [TargetPE(e)]^l)                   = { }
break-ret ( [NRFunApp(kind,name,args)]^l )     = { }
break-ret ( [Break]^l )                        = { l }
break-ret ( [Continue]^l )                     = { }
break-ret ( [Skip]^l )                         = { }
break-ret ( [Return (e?) ]^l )                 = { l }
break-ret ( [IfElse (b, S1 , S2?)]^l )         = break-ret (S1) ∪ break-ret (S2)
break-ret ( [Block XS]^l )                     =  ∪ {break-ret(S) | S in XS}
break-ret ( [SList  XS]^l )                    =  ∪ {break-ret(S) | S in XS}
break-ret ( [While (b, S) )]^l                 = { }
break-ret ( [For { v ; lb; ub ; S }]^l )       = { }
 *)
and break_return accu stmt =
  let cur_label = Labelled.label_of stmt in
  match Fixed.pattern stmt with
  | Break | Return _ -> Int_label.Set.add accu cur_label
  | IfElse (_, ts, fs_opt) ->
      break_return
        (Option.value_map ~default:accu ~f:(break_return accu) fs_opt)
        ts
  | SList xs | Block xs -> List.fold_left ~init:accu xs ~f:break_return
  | Assignment _ | TargetPE _ | NRFunApp _ | Continue | Decl _ | While _
   |For _ | Skip ->
      accu

(** 
continue ( [Assignment ((id,index), rhs)]^l ) = { } 
continue ( [TargetPE(e)]^l)                   = { }
continue ( [NRFunApp(kind,name,args)]^l )     = { }
continue ( [Break]^l )                        = { }
continue ( [Continue]^l )                     = { l }
continue ( [Skip]^l )                         = { }
continue ( [Return (e?) ]^l )                 = { }
continue ( [IfElse (b, S1 , S2?)]^l )         = continue (S1) ∪ continue (S2)
continue ( [Block XS]^l )                     =  ∪ {continue(S) | S in XS}
continue ( [SList  XS]^l )                    =  ∪ {continue(S) | S in XS}
continue ( [While (b, S) )]^l                 = { }
continue ( [For { v ; lb; ub ; S }]^l )       = { }
*)
and continue accu stmt =
  let cur_label = Labelled.label_of stmt in
  match Fixed.pattern stmt with
  | Continue -> Int_label.Set.add accu cur_label
  | IfElse (_, ts, fs_opt) ->
      continue (Option.value_map ~default:accu ~f:(continue accu) fs_opt) ts
  | SList xs | Block xs -> List.fold_left ~init:accu xs ~f:continue
  | Assignment _ | TargetPE _ | NRFunApp _ | Decl _ | Break | Return _
   |While _ | For _ | Skip ->
      accu

(** 

Build the flowgraph of a statment in terms of `initial` and `finals` 

flow ( [Assignment ((id,index), rhs)]^l ) = {  } 
flow ( [TargetPE(e)]^l)                   = {  }
flow ( [NRFunApp(kind,name,args)]^l )     = {  }
flow ( [Break]^l )                        = {  }
flow ( [Continue]^l )                     = {  }
flow ( [Skip]^l )                         = {  }
flow ( [Return (e?) ]^l )                 = {  }

flow ( [While (b, S) )]^l         
  = flow (S) ∪ {(l, l') | l' in initial(S) } ∪ { (l', l) | l' in final(S) }
flow ( [For { v ; elower; eupper ; S }]^l )  
  =  flow (S) ∪ {(l, l') | l' in initial(S) } ∪ { (l', l) | l' in final(S) }

flow ( [IfElse (b, S1 , S2?)]^l )
  = flow (S1) ∪ flow (S2) ∪ {(l, l') | l' in initial(S1) ∪ initial(S2)}

flow ( [Block XS]^l )
flow ( [SList  XS]^l )                    
  = ∪ {flow S | S in XS } 
    ∪ {(l, l') | l' in initial(head XS) } 
    ∪ { (l1, l2) | 
          l1 in final(S1) and l2 in initial(S2) for S1 S2 consecutive in XS
      }
*)
let rec flow ?init:(accu = []) stmt =
  let cur_label = Labelled.label_of stmt in
  match Fixed.pattern stmt with
  | Assignment _ | TargetPE _ | NRFunApp _ | Break | Continue | Return _
   |Skip | Decl _ ->
      accu
  | For {body; _} | While (_, body) ->
      let init_body = initial body and finals_body = finals body in
      let initials = [(cur_label, init_body)]
      and finals =
        List.map ~f:(fun l' -> (l', cur_label))
        @@ Int_label.Set.to_list finals_body
      in
      let accu' = initials @ finals @ accu in
      flow ~init:accu' body
  | IfElse (_, ts, Some fs) ->
      let init_ts = initial ts and init_fs = initial fs in
      let accu' = (cur_label, init_ts) :: (cur_label, init_fs) :: accu in
      flow ~init:(flow ~init:accu' fs) ts
  | IfElse (_, ts, _) ->
      let init_ts = initial ts in
      flow ~init:((cur_label, init_ts) :: accu) ts
  | SList xs | Block xs ->
      Option.value_map ~default:accu (List.hd xs) ~f:(fun next ->
          let init_next = initial next in
          let accu' = (cur_label, init_next) :: accu in
          pairwise ~init:accu' xs )

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
