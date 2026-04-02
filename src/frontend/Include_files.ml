open Core

type t = FileSystemPaths of string list | InMemory of string String.Map.t

(** Where and how to look for #include-d files *)
let include_provider : t ref = ref (FileSystemPaths [])
