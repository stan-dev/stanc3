(* Symbol table to implement var map *)

module type SYMBOL = sig
  type 'a table

  val initialize : unit -> 'a table

  val enter : 'a table -> string -> 'a -> unit

  val look : 'a table -> string -> 'a option

  val begin_scope : 'a table -> unit

  val end_scope : 'a table -> unit
end

(* TODO: I'm sure this implementation could be made more efficient if that's necessary. There's no need for all the string comparison.
We could just keep track of the count of the entry into the hash table and use that for comparison. *)
module Symbol : SYMBOL = struct
  type 'a table = (string, 'a) Hashtbl.t * string list ref

  let initialize _ = (Hashtbl.create 123456, ref [])

  (* We just pick some initial size. Hash tables get resized dynamically if necessary, so it doesn't hugely matter. *)
  let enter tab str ty =
    Hashtbl.add (fst tab) str ty ;
    snd tab := str :: !(snd tab)

  (* recall that OCaml hash tables store a stack of all the values for each key; this would allow us to use shadowing; if we don't want shadowing we can add an extra check here. *)
  let look tab str = Hashtbl.find_opt (fst tab) str

  let begin_scope tab = snd tab := "-" :: !(snd tab)

  (* using a string '-' here that can never be used as an identifier to indicate that new scope is entered *)
  let end_scope tab =
    while List.hd !(snd tab) <> "-" do
      (* we pop the stack down to where we entered the current scope and remove all variables defined since from the var map *)
      Hashtbl.remove (fst tab) (List.hd !(snd tab)) ;
      snd tab := List.tl !(snd tab)
    done ;
    snd tab := List.tl !(snd tab)
end
