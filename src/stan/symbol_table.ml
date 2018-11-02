(* Symbol table to implement var map *)

module type SYMBOL = sig
  type 'a state

  val initialize : (string -> 'a -> bool) -> 'a state

  val enter : 'a state -> string -> 'a -> unit

  val look : 'a state -> string -> 'a option

  val begin_scope : 'a state -> unit

  val end_scope : 'a state -> unit

  val is_primitive : 'a state -> string -> 'a -> bool
end

(* TODO: I'm sure this implementation could be made more efficient if that's necessary. There's no need for all the string comparison.
We could just keep track of the count of the entry into the hash table and use that for comparison. *)
module Symbol : SYMBOL = struct
  type 'a state =
    { table: (string, 'a) Hashtbl.t
    ; stack: string Stack.t
    ; is_primitive: string -> 'a -> bool }

  let initialize f =
    {table= Hashtbl.create 123456; stack= Stack.create (); is_primitive= f}

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
      let _ = Stack.pop s.stack in
      ()
    done ;
    let _ = Stack.pop s.stack in
    ()

  let is_primitive s name signature = s.is_primitive name signature
end
