(** The main entrypoint for Stan -> C++ compilation *)

open Frontend

(** Either the C++ a model compiled to, or an error *)
type compilation_result = (string, Errors.t) result

(** The type of all non-C++-code outputs from the compiler *)
type other_output =
  | Formatted of string
  | DebugOutput of string
  | Memory_patterns of string
  | Info of string
  | Version of string
  | Generated of string
  | Warnings of Warnings.t list

val stan2cpp :
     string
  -> [`Code of string | `File of string]
  -> Flags.t
  -> (other_output -> unit)
  -> compilation_result
(** The main function of the compiler. Takes in the model's name,
  the model code, compiler settings, and a callback for all non-C++
  output
  *)
