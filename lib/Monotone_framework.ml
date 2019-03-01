(** The common elements of a monotone framework *)

open Core_kernel
open Monotone_framework_sigs


(* TODO: write instances of LATTICE for powerset
                                        dual powerset
                                        function type
                                        new top element
                                        new bottom element
                                        reaching def example
         write instance of FLOWGRAPH for Stan flowgraph of Stan MIR
                                         inverse flow graph of flow graph
         write instance of TRANSFER_FUNCTION for available expressions
                                                 reaching definitions
                                                 live variables
                                                 constant propagation
                                                 very busy expressions *)

module Monotone_framework : MONOTONE_FRAMEWORK =
functor
  (F : FLOWGRAPH)
  (L : LATTICE)
  (T :
     TRANSFER_FUNCTION
     with type labels = F.labels
      and type properties = L.properties)
  ->
  struct
    let mfp () =
      (* STEP 1 *)
      let workstack = Stack.of_list (Set.to_list F.edges) in
      let analysis_in = Hashtbl.create (module F) in
      let _ =
        Set.iter
          ~f:(fun l ->
            Hashtbl.add_exn analysis_in ~key:l
              ~data:(if Set.mem F.initials l then L.extreme else L.bottom) )
          F.nodes
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
          Set.iter (F.sucessors l') ~f:(fun l'' ->
              Stack.push workstack (l', l'') )
        done
      in
      (* STEP 3 *)
      let analysis_out = Hashtbl.create (module F) in
      let _ =
        Set.iter
          ~f:(fun l ->
            Hashtbl.add_exn analysis_in ~key:l
              ~data:(T.transfer_function l (Hashtbl.find_exn analysis_in l)) )
          F.nodes
      in
      (analysis_in, analysis_out)
  end
