(* Some semantic actions to aid legibility of the grammar file *)

(* TODO: inline these in the grammar *)

open Syntax

let construct_program obf obd obtd obp obtp obm obg =
  let rbf = match obf with Some bf -> bf | _ -> None in
  let rbd = match obd with Some bd -> bd | _ -> None in
  let rbtd = match obtd with Some btd -> btd | _ -> None in
  let rbp = match obp with Some bp -> bp | _ -> None in
  let rbtp = match obtp with Some btp -> btp | _ -> None in
  let rbm = match obm with Some bm -> bm | _ -> None in
  let rbg = match obg with Some bg -> bg | _ -> None in
  { functionblock= rbf
  ; datablock= rbd
  ; transformeddatablock= rbtd
  ; parametersblock= rbp
  ; transformedparametersblock= rbtp
  ; modelblock= rbm
  ; generatedquantitiesblock= rbg }

let rec repeat n f x =
  if n <= Int64.zero then x else repeat (Int64.pred n) f (f x)

let reparray n bt = repeat n (fun y -> Array y) bt

let rec reduce l f x =
  if List.length l = 0 then x else reduce (List.tl l) f (f x (List.hd l))

let reducearray (sbt, l) = reduce l (fun y z -> SArray (y, z)) sbt

let construct_unsized_type bt ud =
  let size =
    match ud with Some d -> Int64.succ (Int64.of_int d) | _ -> Int64.zero
  in
  reparray size bt

let construct_arg_decl od ut id =
  match od with None -> Arg (ut, id) | _ -> DataArg (ut, id)

let construct_var_decl sbt id d ae =
  let sizes = match d with None -> [] | Some l -> l in
  match ae with
  | Some a ->
      [ VDecl (reducearray (sbt, sizes), id)
      ; Stmt (Assignment ((id, []), Assign, snd a)) ]
  | _ -> [VDecl (reducearray (sbt, sizes), id)]

let construct_top_var_decl_no_assign tvt id d =
  let sizes = match d with None -> [] | Some l -> l in
  (reducearray (fst tvt, sizes), snd tvt, id)

let construct_top_var_decl tvt id d ass =
  let sizes = match d with None -> [] | Some l -> l in
  match ass with
  | Some a ->
      [ TVDecl (reducearray (fst tvt, sizes), snd tvt, id)
      ; TStmt (Assignment ((id, []), Assign, snd a)) ]
  | _ -> [TVDecl (reducearray (fst tvt, sizes), snd tvt, id)]

let construct_truncation e1 e2 =
  match (e1, e2) with
  | Some tt1, Some tt2 -> TruncateBetween (tt1, tt2)
  | Some tt1, None -> TruncateUpFrom tt1
  | None, Some tt2 -> TruncateDownFrom tt2
  | _ -> NoTruncate

let construct_tilde_statement e id es ot =
  let t = match ot with Some tt -> tt | _ -> NoTruncate in
  Tilde {arg = e; distribution = id; args = es; truncation = t;}
