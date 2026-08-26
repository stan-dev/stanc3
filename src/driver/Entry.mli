(** Stan compiler entrypoints *)

open Frontend

(** Either the C++ a model compiled to, or an error *)
type compilation_result = (string, Errors.t) result

(** Either a model compiled to Stan Math backend-transformed and optimized MIR,
    or an error *)
type mir_compilation_result = (Middle.Program.Typed.t, Errors.t) result

(** The type of all auxiliary outputs from the compiler *)
type other_output =
  | Formatted of string
  | DebugOutput of string
  | Memory_patterns of string
  | Info of string
  | Version of string
  | Generated of string
  | Warnings of Warnings.t list

val stan2mir :
     string
  -> [`Code of string | `File of string]
  -> Flags.t
  -> (other_output -> unit)
  -> mir_compilation_result
(** Compile a model through Stan Math backend transformation and MIR
    optimization, without lowering it to C++. Takes the model's name, model
    code, compiler settings, and a callback for auxiliary output. *)

val stan2cpp :
     string
  -> [`Code of string | `File of string]
  -> Flags.t
  -> (other_output -> unit)
  -> compilation_result
(** The main function of the compiler. Takes in the model's name, the model
    code, compiler settings, and a callback for all non-C++ output *)
