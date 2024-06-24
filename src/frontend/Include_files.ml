open Core

type include_provider_t =
  | FileSystemPaths of string list
  | InMemory of string String.Map.t

(** Where and how to look for #include-d files *)
let include_provider : include_provider_t ref = ref (FileSystemPaths [])
