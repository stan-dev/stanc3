(* Some semantic actions to aid legibility of the grammar file *)

open Syntax

let construct_program obf obd obtd obp obtp obm obg = let rbf = match obf with Some bf -> bf
                                                                             | _ -> EmptyFunBlock in
                                                      let rbd = match obd with Some bd -> bd
                                                                             | _ -> EmptyDataBlock in
                                                      let rbtd = match obtd with Some btd -> btd
                                                                             | _ -> EmptyTDataBlock in
                                                      let rbp = match obp with Some bp -> bp
                                                                             | _ -> EmptyParamBlock in
                                                      let rbtp = match obtp with Some btp -> btp
                                                                             | _ -> EmptyTParamBlock in
                                                      let rbm = match obm with Some bm -> bm
                                                                             | _ -> EmptyModelBlock in
                                                      let rbg = match obg with Some bg -> bg
                                                                             | _ -> EmptyGQBlock

                                                      in Program (rbf, rbd, rbtd, rbp, rbtp, rbm, rbg)
                                                      
let rec repeat n f x = if n <= Int64.zero then x else repeat (Int64.pred n) f (f x)

let construct_unsized_type bt ud = let size = match ud with Some d -> Int64.succ(Int64.of_int(d)) 
                                                          | _ -> Int64.zero
                                   in repeat size (fun y -> Array y) bt

let construct_arg_decl od ut id = match od with None -> Arg (ut, id)
                                              | _    -> DataArg (ut, id)

let construct_var_decl sbt id d ae = let sizes = match d with None -> [] | Some l -> l in
                                     match ae with Some a -> [VDecl ((sbt, sizes), id); Stmt (Assignment ((id, []), Assign, snd a))]   
                                                 | _ -> [VDecl ((sbt, sizes), id)]
                                                       
let construct_top_var_decl_no_assign tvt id d = let sizes = match d with None -> [] | Some l -> l
                                                in ((tvt, sizes), id)

let construct_top_var_decl tvt id d ass = let sizes = match d with None -> [] | Some l -> l in
                                          match ass with Some a -> [TVDecl ((tvt, sizes), id); TStmt (Assignment ((id, []), Assign, snd a))]   
                                                       | _ -> [TVDecl ((tvt, sizes), id)]

let construct_truncation e1 e2 = match (e1, e2) with (Some tt1, Some tt2) -> TruncateBetween (tt1, tt2)
                                                   | (Some tt1, None)     -> TruncateUpFrom tt1
                                                   | (None, Some tt2)     -> TruncateDownFrom tt2
                                                   | _ -> NoTruncate                                                   

let construct_tilde_statement e id es ot = let t = match ot with Some tt -> tt | _ -> NoTruncate
                                           in Tilde (e, id, es, t)