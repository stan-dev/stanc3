(** The common elements of a monotone framework *)

open Core_kernel
open Monotone_framework_sigs

module Monotone_framework
    (F : FLOWGRAPH)
    (L : LATTICE)
    (T : TRANSFER_FUNCTION
         with type labels = F.labels
          and type properties = L.properties) =
struct
  let mfp () =
    (* STEP 1 *)
    let workstack = Stack.of_list (F.list_edges ()) in
    let analysis_in = Hashtbl.create (module F) in
    let _ =
      List.iter
        ~f:(fun l ->
          Hashtbl.add_exn analysis_in ~key:l
            ~data:(if Set.mem F.initials l then L.extremal else L.bottom) )
        (F.list_nodes ())
    in
    (* STEP 2 *)
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
        Set.iter (F.sucessors l') ~f:(fun l'' -> Stack.push workstack (l', l''))
      done
    in
    (* STEP 3 *)
    let analysis_out = Hashtbl.create (module F) in
    let _ =
      List.iter
        ~f:(fun l ->
          Hashtbl.add_exn analysis_in ~key:l
            ~data:(T.transfer_function l (Hashtbl.find_exn analysis_in l)) )
        (F.list_nodes ())
    in
    (analysis_in, analysis_out)
end
