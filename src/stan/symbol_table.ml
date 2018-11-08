(* Symbol table to implement var map *)

module type SYMBOL = sig
  type 'a state

  val initialize : unit -> 'a state

  val enter : 'a state -> string -> 'a -> unit

  val look : 'a state -> string -> 'a option

  val begin_scope : 'a state -> unit

  val end_scope : 'a state -> unit

  val set_read_only : 'a state -> string -> unit

  val get_read_only : 'a state -> string -> bool

  val set_global : 'a state -> string -> unit

  val get_global : 'a state -> string -> bool

  val unsafe_remove : 'a state -> string -> unit

  val unsafe_add : 'a state -> string -> 'a -> unit
end

(* TODO: I'm sure this implementation could be made more efficient if that's necessary. There's no need for all the string comparison.
We could just keep track of the count of the entry into the hash table and use that for comparison. *)
module Symbol : SYMBOL = struct
  type 'a state =
    { table: (string, 'a) Hashtbl.t
    ; stack: string Stack.t
    ; readonly: (string, bool) Hashtbl.t
    ; global: (string, bool) Hashtbl.t }

  let initialize () =
    { table= Hashtbl.create 123456
    ; stack= Stack.create ()
    ; readonly= Hashtbl.create 123456
    ; global= Hashtbl.create 123456 }

  (* We just pick some initial size. Hash tables get resized dynamically if necessary, so it doesn't hugely matter. *)
  let enter s str ty = Hashtbl.add s.table str ty ; Stack.push str s.stack

  (* recall that OCaml hash tables store a stack of all the values for each key; this would allow us to use shadowing; if we don't want shadowing we can add an extra check here. *)
  let look s str = Hashtbl.find_opt s.table str

  let begin_scope s = Stack.push "-sentinel-new-scope-" s.stack

  (* using a string '-' here that can never be used as an identifier to indicate that new scope is entered *)
  let end_scope s =
    while Stack.top s.stack <> "-sentinel-new-scope-" do
      (* we pop the stack down to where we entered the current scope and remove all variables defined since from the var map *)
      Hashtbl.remove s.table (Stack.top s.stack) ;
      Hashtbl.remove s.readonly (Stack.top s.stack) ;
      let _ = Stack.pop s.stack in
      ()
    done ;
    let _ = Stack.pop s.stack in
    ()

  let set_read_only s str = Hashtbl.add s.readonly str true

  let get_read_only s str =
    match Hashtbl.find_opt s.readonly str with Some true -> true | _ -> false

  (* TODO: the following is very ugly, but we seem to need something like it to
   reproduce the (strange) behaviour in the current Stan that local variables
   have a block level that is determined by what has been assigned to them
   rather than by where they were declared. I'm not sure that behaviour makes
   sense unless we use static analysis as well to make sure these assignments
   actually get evaluated in that phase. *)
  let set_global s str = Hashtbl.add s.global str true

  let get_global s str =
    match Hashtbl.find_opt s.global str with Some true -> true | _ -> false

  let unsafe_remove s str = Hashtbl.remove s.table str

  let unsafe_add s str ty = Hashtbl.add s.table str ty
end
