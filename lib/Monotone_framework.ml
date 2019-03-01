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
    let worklist = ref (F.list_edges ()) in
    let analysis = Hashtbl.create (module F) in
    let _ =
      List.iter
        ~f:(fun l ->
          Hashtbl.add_exn analysis ~key:l
            ~data:(if Set.mem F.initials l then L.extremal else L.bottom) )
        (F.list_nodes ())
    in
    (* STEP 2 *)
    let transfer_function = T.transfer_function in
    let _ =
      while List.length !worklist <> 0 do
        ()
      done
    in
    (* STEP 3 *)
    let analysis_out = Hashtbl.create (module F) in
    let _ =
      List.iter
        ~f:(fun l ->
          Hashtbl.add_exn analysis ~key:l
            ~data:(transfer_function l (Hashtbl.find_exn analysis l)) )
        (F.list_nodes ())
    in
    (analysis, analysis_out)
end
