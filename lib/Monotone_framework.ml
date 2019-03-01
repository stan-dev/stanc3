(** The common elements of a monotone framework *)

open Core_kernel
open Monotone_framework_sigs

module Monotone_framework (F : FLOWGRAPH) (L : LATTICE) (T : TRANSFER_FUNCTION) =
struct
  let mfp () =
    (* STEP 1 *)
    let _ = ref (F.list_edges ()) in
    let analysis = Hashtbl.create (module F) in
    let _ =
      List.iter
        ~f:(fun l -> Hashtbl.add_exn analysis ~key:l ~data:
        (if Set.mem F.initials l then L.extremal else L.bottom))
        (F.list_nodes ())
    in
    ()

  (* STEP 2 *)
  
  (* STEP 3 *)
end
