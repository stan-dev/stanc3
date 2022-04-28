(** General functions and signatures for a Standard Library *)

open Middle
open Core_kernel

(* Types for the module representing the standard library *)
type fun_arg = UnsizedType.autodifftype * UnsizedType.t

type signature =
  UnsizedType.returntype * fun_arg list * Common.Helpers.mem_pattern

type deprecation_info =
  { replacement: string
  ; version: string
  ; extra_message: string
  ; canonicalize_away: bool }
[@@deriving sexp]

module type Library = sig
  (** This module is used as a parameter for many functors which
    rely on information about a backend-specific Stan library. *)

  val function_signatures : (string, signature list) Hashtbl.t
  (** Mapping from names to signature(s) of functions *)

  val distribution_families : string list

  val is_stdlib_function_name : string -> bool
  (** Equivalent to [Hashtbl.mem stan_math_signatures s]*)

  val get_signatures : string -> signature list
  val get_operator_signatures : Operator.t -> signature list
  val get_assignment_operator_signatures : Operator.t -> signature list
  val is_not_overloadable : string -> bool
  val is_variadic_function_name : string -> bool
  val variadic_function_returntype : string -> UnsizedType.returntype option

  val check_variadic_fn :
       Ast.identifier
    -> is_cond_dist:bool
    -> Location_span.t
    -> Environment.originblock
    -> Environment.t
    -> Ast.typed_expression list
    -> Ast.typed_expression

  val operator_to_function_names : Operator.t -> string list
  val string_operator_to_function_name : string -> string
  val deprecated_distributions : deprecation_info String.Map.t
  val deprecated_functions : deprecation_info String.Map.t
end

module NullLibrary : Library = struct
  (** A "standard library" for stan which contains no functions.
      Useful only for testing
   *)

  let function_signatures : (string, signature list) Hashtbl.t =
    String.Table.create ()

  let distribution_families : string list = []
  let is_stdlib_function_name _ = false
  let get_signatures _ = []
  let get_assignment_operator_signatures _ = []
  let get_operator_signatures _ = []
  let is_not_overloadable _ = false
  let is_variadic_function_name _ = false
  let variadic_function_returntype _ = None

  let check_variadic_fn _ ~is_cond_dist _ _ _ _ : Ast.typed_expression =
    ignore (is_cond_dist : bool) ;
    Common.FatalError.fatal_error_msg [%message "Impossible"]

  let operator_to_function_names _ = []
  let string_operator_to_function_name s = s
  let deprecated_distributions = String.Map.empty
  let deprecated_functions = String.Map.empty
end

let pp_math_sig ppf (rt, args, mem_pattern) =
  UnsizedType.pp ppf (UFun (args, rt, FnPlain, mem_pattern))

let pp_math_sigs ppf sigs = (Fmt.list ~sep:Fmt.cut pp_math_sig) ppf sigs
let pretty_print_math_sigs = Fmt.str "@[<v>@,%a@]" pp_math_sigs

let dist_name_suffix (module StdLibrary : Library) udf_names name =
  let is_udf_name s =
    List.exists ~f:(fun (n, _) -> String.equal s n) udf_names in
  Utils.distribution_suffices
  |> List.filter ~f:(fun sfx ->
         StdLibrary.is_stdlib_function_name (name ^ sfx)
         || is_udf_name (name ^ sfx) )
  |> List.hd_exn