open Std
open Js_of_ocaml

val get_includes_lenient : 'a Js.t Js.opt -> string String.Map.t * string list
(** Converts from a [{ [s:string]:string }] JS object type to an OCaml map, with
    warnings for bad input. *)

type flags =
  {name: string; code: string; driver_flags: Driver.Flags.t; color_output: bool}

val process_flags :
     Js.js_string Js.t
  -> Js.js_string Js.t
  -> Js.js_string Js.t Js.js_array Js.t Js.opt
  -> string String.Map.t
  -> (flags, string) result
(** Turn function inputs into a [Driver.Flags.t] *)

val str_color :
  color_output:bool -> ('a, Format.formatter, unit, string) format4 -> 'a
(** similar to [Fmt.str_like] but directly sets style rendering rather than
    copying from another ppf *)

class type stancReturn = object
  method errors : Js.js_string Js.t Js.js_array Js.t Js.optdef_prop
  method result : Js.js_string Js.t Js.optdef_prop
  method warnings : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
end

val wrap_error : warnings:string list -> string -> stancReturn Js.t

val wrap_result :
     ?printed_filename:string
  -> code:string
  -> color_output:bool
  -> warnings:string list
  -> (string, Frontend.Errors.t) result
  -> stancReturn Js.t
