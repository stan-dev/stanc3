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
  ; locals: (string, unit) Hashtbl.t ref
  ; captures: String.Set.t ref
  ; globals: (string, unit) Hashtbl.t }

let initialize () =
  { table= String.Table.create ()
  ; stack= Stack.create ()
  ; scopedepth= ref 0
  ; readonly= String.Table.create ()
  ; isunassigned= String.Table.create ()
  ; locals= ref (String.Table.create ())
  ; captures= ref String.Set.empty
  ; globals= String.Table.create () }

let enter s str ty =
  if !(s.scopedepth) = 0 then
    ignore (Hashtbl.add s.globals ~key:str ~data:() : [`Duplicate | `Ok]) ;
  let _ : [`Duplicate | `Ok] = Hashtbl.add !(s.locals) ~key:str ~data:() in
  let _ : [`Duplicate | `Ok] = Hashtbl.add s.table ~key:str ~data:ty in
  Stack.push s.stack str

let look s str =
  ( match Hashtbl.find !(s.locals) str with
  | None -> s.captures := Set.add !(s.captures) str
  | Some () -> () ) ;
  Hashtbl.find s.table str

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
    Hashtbl.remove !(s.locals) (Stack.top_exn s.stack) ;
    Hashtbl.remove s.isunassigned (Stack.top_exn s.stack) ;
    Stack.pop_exn s.stack |> (ignore : string -> unit)
  done ;
  Stack.pop_exn s.stack |> (ignore : string -> unit)

let with_scope s f x =
  begin_scope s ;
  let x = f x in
  end_scope s ; x

let with_closure s f x =
  let locals = !(s.locals) in
  let captures = !(s.captures) in
  s.locals := String.Table.create () ;
  s.captures := String.Set.empty ;
  begin_scope s ;
  let x = f s.captures x in
  end_scope s ;
  s.locals := locals ;
  s.captures := captures ;
  x

let set_read_only s str =
  Hashtbl.add s.readonly ~key:str ~data:()
  |> (ignore : [`Duplicate | `Ok] -> unit)

let get_read_only s str =
  Option.(
    is_some (Hashtbl.find s.readonly str)
    || not (is_some (Hashtbl.find !(s.locals) str)))

let set_is_assigned s str = Hashtbl.remove s.isunassigned str

let set_is_unassigned s str =
  Hashtbl.add s.isunassigned ~key:str ~data:()
  |> (ignore : [`Duplicate | `Ok] -> unit)

let check_is_unassigned s str = Hashtbl.mem s.isunassigned str
let check_some_id_is_unassigned s = not (Hashtbl.length s.isunassigned = 0)
let is_global s str = Option.is_some (Hashtbl.find s.globals str)

let unsafe_clear_symbol_table s =
  Hashtbl.clear s.table ;
  Stack.clear s.stack ;
  s.scopedepth := 0 ;
  Hashtbl.clear s.readonly ;
  Hashtbl.clear s.isunassigned ;
  s.captures := String.Set.empty ;
  Hashtbl.clear !(s.locals) ;
  Hashtbl.clear s.globals
