(** Symbol table to implement var map *)

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
  { table= Hashtbl.create 123456
  ; stack= Stack.create ()
  ; scopedepth= ref 0
  ; readonly= Hashtbl.create 123456
  ; isunassigned= Hashtbl.create 123456
  ; globals= Hashtbl.create 123456 }

(* We just pick some initial size. Hash tables get resized dynamically if necessary, so it doesn't hugely matter. *)
let enter s str ty =
  let _ = if !(s.scopedepth) = 0 then Hashtbl.add s.globals str () in
  Hashtbl.add s.table str ty ; Stack.push str s.stack

(* recall that OCaml hash tables store a stack of all the values for each key; this would allow us to use shadowing; if we don't want shadowing we can add an extra check here. *)
let look s str = Hashtbl.find_opt s.table str

let begin_scope s =
  let _ = s.scopedepth := !(s.scopedepth) + 1 in
  Stack.push "-sentinel-new-scope-" s.stack

(* using a string "-sentinel-new-scope-" here that can never be used as an identifier to indicate that new scope is entered *)
let end_scope s =
  let _ = s.scopedepth := !(s.scopedepth) - 1 in
  while Stack.top s.stack <> "-sentinel-new-scope-" do
    (* we pop the stack down to where we entered the current scope and remove all variables defined since from the var map *)
    Hashtbl.remove s.table (Stack.top s.stack) ;
    Hashtbl.remove s.readonly (Stack.top s.stack) ;
    Hashtbl.remove s.isunassigned (Stack.top s.stack) ;
    let _ = Stack.pop s.stack in
    ()
  done ;
  let _ = Stack.pop s.stack in
  ()

let set_read_only s str = Hashtbl.add s.readonly str ()

let get_read_only s str =
  match Hashtbl.find_opt s.readonly str with Some () -> true | _ -> false

let set_is_assigned s str = Hashtbl.remove s.isunassigned str

let set_is_unassigned s str =
  if Hashtbl.mem s.isunassigned str then ()
  else Hashtbl.add s.isunassigned str ()

let check_is_unassigned s str = Hashtbl.mem s.isunassigned str
let check_some_id_is_unassigned s = not (Hashtbl.length s.isunassigned = 0)

let is_global s str =
  match Hashtbl.find_opt s.globals str with Some _ -> true | _ -> false

let unsafe_clear_symbol_table s =
  Hashtbl.clear s.table ;
  Stack.clear s.stack ;
  s.scopedepth := 0 ;
  Hashtbl.clear s.readonly ;
  Hashtbl.clear s.isunassigned ;
  Hashtbl.clear s.globals

(* TODO: the following is very ugly, but we seem to need something like it to
 reproduce the (strange) behaviour in the current Stan that local variables
 have a block level that is determined by what has been assigned to them
 rather than by where they were declared. *)
let unsafe_replace s str ty =
  Hashtbl.remove s.table str ; Hashtbl.add s.table str ty
