(** Symbol table to implement var map *)

open Core_kernel

(* TODO: I'm sure this implementation could be made more efficient if that's necessary. There's no need for all the string comparison.
We could just keep track of the count of the entry into the hash table and use that for comparison. *)
type 'a state =
  { table: (string, 'a) Hashtbl.t
  ; stack: string Stack.t
  ; scopedepth: int ref
  ; readonly: (string, unit) Hashtbl.t
  ; isunassigned: (string, unit) Hashtbl.t
  ; globals: (string, unit) Hashtbl.t }

let initialize () =
  { table= String.Table.create ()
  ; stack= Stack.create ()
  ; scopedepth= ref 0
  ; readonly= String.Table.create ()
  ; isunassigned= String.Table.create ()
  ; globals= String.Table.create () }

let enter s str ty =
  let _ : [`Duplicate | `Ok] =
    if !(s.scopedepth) = 0 then Hashtbl.add s.globals ~key:str ~data:()
    else `Ok
  in
  let _ : [`Duplicate | `Ok] = Hashtbl.add s.table ~key:str ~data:ty in
  Stack.push s.stack str

let look s str = Hashtbl.find s.table str

let begin_scope s =
  s.scopedepth := !(s.scopedepth) + 1 ;
  Stack.push s.stack "-sentinel-new-scope-"

(* using a string "-sentinel-new-scope-" here that can never be used as an identifier to indicate that new scope is entered *)
let end_scope s =
  s.scopedepth := !(s.scopedepth) - 1 ;
  while Stack.top_exn s.stack <> "-sentinel-new-scope-" do
    (* we pop the stack down to where we entered the current scope and remove all variables defined since from the var map *)
    Hashtbl.remove s.table (Stack.top_exn s.stack) ;
    Hashtbl.remove s.readonly (Stack.top_exn s.stack) ;
    Hashtbl.remove s.isunassigned (Stack.top_exn s.stack) ;
    let _ : string = Stack.pop_exn s.stack in
    ()
  done ;
  let _ : string = Stack.pop_exn s.stack in
  ()

let set_read_only s str =
  let _ : [`Duplicate | `Ok] = Hashtbl.add s.readonly ~key:str ~data:() in
  ()

let get_read_only s str =
  match Hashtbl.find s.readonly str with Some () -> true | _ -> false

let set_is_assigned s str = Hashtbl.remove s.isunassigned str

let set_is_unassigned s str =
  let _ : [`Duplicate | `Ok] =
    if Hashtbl.mem s.isunassigned str then `Ok
    else Hashtbl.add s.isunassigned ~key:str ~data:()
  in
  ()

let check_is_unassigned s str = Hashtbl.mem s.isunassigned str
let check_some_id_is_unassigned s = not (Hashtbl.length s.isunassigned = 0)

let is_global s str =
  match Hashtbl.find s.globals str with Some _ -> true | _ -> false

let unsafe_clear_symbol_table s =
  Hashtbl.clear s.table ;
  Stack.clear s.stack ;
  s.scopedepth := 0 ;
  Hashtbl.clear s.readonly ;
  Hashtbl.clear s.isunassigned ;
  Hashtbl.clear s.globals
