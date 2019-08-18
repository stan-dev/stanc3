(** Symbol table to implement var map *)

open Core_kernel

(* TODO: I'm sure this implementation could be made more efficient if that's necessary. There's no need for all the string comparison.
We could just keep track of the count of the entry into the hash table and use that for comparison. *)
type 'a state =
  { table: (string, 'a, String.comparator_witness) Map.t
  ; stack: string List.t
  ; scopedepth: int
  ; readonly: (string, String.comparator_witness) Set.t
  ; isunassigned: (string, String.comparator_witness) Set.t
  ; globals: (string, String.comparator_witness) Set.t }

let empty =
  { table= Map.empty (module String)
  ; stack= []
  ; scopedepth= 0
  ; readonly= Set.empty (module String)
  ; isunassigned= Set.empty (module String)
  ; globals= Set.empty (module String) }

let sentinel_new_scope = "-sentinel-new-scope-"

let add_ignoring_dup map k v = match Map.add map ~key:k ~data:v with
  | `Duplicate -> map
  | `Ok new_map -> new_map

let enter s str ty =
  let new_globals =
    if s.scopedepth = 0
    then Set.add s.globals str
    else s.globals
  in
  let new_table = add_ignoring_dup s.table str ty in
  { s with table= new_table; globals= new_globals; stack= str :: s.stack }

let look s str = Map.find s.table str

let debug s =
  Debug.eprint "Symbol table keys:";
  Map.keys s.table |> List.to_string ~f:(fun s -> s) |> Debug.eprint ;
  Debug.eprint "Symbol table stack:";
  s.stack |> List.to_string ~f:(fun s -> s) |> Debug.eprint ;
  ()

let begin_scope s =
  { s with scopedepth = s.scopedepth + 1; stack= sentinel_new_scope :: s.stack }

(* using a string "-sentinel-new-scope-" here that can never be used as an identifier to indicate that new scope is entered *)
let end_scope s =
  let new_scopedepth = s.scopedepth - 1 in
  let (old_vars_list, sentinel_and_keep_vars) = List.split_while s.stack ~f:(fun var -> var <> sentinel_new_scope) in
  let old_vars_set = String.Set.of_list old_vars_list in
  let pop_old_vars_map map = Map.filter_keys map ~f:(fun key -> Set.mem old_vars_set key) in
  let pop_old_vars_set set = Set.diff set old_vars_set in
  let new_table = pop_old_vars_map s.table in
  let new_readonly = pop_old_vars_set s.readonly in
  let new_isunassigned = pop_old_vars_set s.isunassigned in
  let new_stack = List.tl_exn sentinel_and_keep_vars in
  { s with table= new_table
         ; readonly= new_readonly
         ; isunassigned= new_isunassigned
         ; scopedepth= new_scopedepth
         ; stack= new_stack }

let set_read_only s str =
  { s with readonly= Set.add s.readonly str }

let get_read_only s str =
  Set.mem s.readonly str

let set_is_assigned s str =
  { s with isunassigned= Set.remove s.isunassigned str }

let set_is_unassigned s str =
  { s with isunassigned= Set.add s.isunassigned str }

let check_is_unassigned s str = Set.mem s.isunassigned str
let check_some_id_is_unassigned s = not (Set.length s.isunassigned = 0)

let is_global s str =
  Set.mem s.globals str
