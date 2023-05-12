(** General functions and signatures for a Standard Library *)

open Middle
open Core_kernel

(* Types for the module representing the standard library *)
type fun_arg = UnsizedType.autodifftype * UnsizedType.t
type signature = UnsizedType.returntype * fun_arg list * Mem_pattern.t

type variadic_signature =
  { return_type: UnsizedType.t
  ; control_args: fun_arg list
  ; required_fn_rt: UnsizedType.t
  ; required_fn_args: fun_arg list }
[@@deriving create]

type deprecation_info =
  { replacement: string
  ; version: string
  ; extra_message: string
  ; canonicalize_away: bool }
[@@deriving sexp]

(* We could consider breaking up this module more, so we would have
   more type-level guarantees about what each Functor is able to do
   with the library. Most of them only need is_stdlib_function_name,
   maybe get_signatures.

   The Stan_math_library could still satisfy all of them by
   using [include]
*)

module type Library = sig
  (** This module is used as a parameter for many functors which
    rely on information about a backend-specific Stan library. *)

  val function_signatures : (string, signature list) Hashtbl.t
  (** Mapping from names to signature(s) of functions *)

  val variadic_signatures : (string, variadic_signature) Hashtbl.t
  (** Mapping from names to description of a variadic function.
  Note that these function names cannot be overloaded, and usually require
  customized code-gen in the backend.
*)

  val distribution_families : string list

  val is_stdlib_function_name : string -> bool
  (** Equivalent to [Hashtbl.mem function_signatures s]*)

  val get_signatures : string -> signature list
  (** Equivalent to [Hashtbl.find_multi function_signatures s]*)

  val get_operator_signatures : Operator.t -> signature list
  val get_assignment_operator_signatures : Operator.t -> signature list
  val is_not_overloadable : string -> bool
  val is_variadic_function_name : string -> bool
  val is_special_function_name : string -> bool
  val special_function_returntype : string -> UnsizedType.returntype option

  val check_special_fn :
       is_cond_dist:bool
    -> Location_span.t
    -> Environment.originblock
    -> Environment.t
    -> Ast.identifier
    -> Ast.typed_expression list
    -> Ast.typed_expression
  (** This function is responsible for typechecking varadic function
    calls. It needs to live in the Library since this is usually
    bespoke per-function. *)

  val operator_to_function_names : Operator.t -> string list
  val string_operator_to_function_name : string -> string
  val deprecated_distributions : deprecation_info String.Map.t
  val deprecated_functions : deprecation_info String.Map.t
end

(** A "standard library" for Stan which contains no functions.
      Useful only for testing
   *)
module NullLibrary : Library = struct
  let function_signatures : (string, signature list) Hashtbl.t =
    String.Table.create ()

  let variadic_signatures : (string, variadic_signature) Hashtbl.t =
    String.Table.create ()

  let distribution_families : string list = []
  let is_stdlib_function_name _ = false
  let get_signatures _ = []
  let get_assignment_operator_signatures _ = []
  let get_operator_signatures _ = []
  let is_not_overloadable _ = false
  let is_variadic_function_name _ = false
  let is_special_function_name _ = false
  let special_function_returntype _ = None

  let check_special_fn ~is_cond_dist _ _ _ _ _ : Ast.typed_expression =
    ignore (is_cond_dist : bool) ;
    Common.FatalError.fatal_error_msg [%message "Impossible"]

  let operator_to_function_names _ = []
  let string_operator_to_function_name s = s
  let deprecated_distributions = String.Map.empty
  let deprecated_functions = String.Map.empty
end

let pp_math_sig ppf ((rt, args, mem_pattern) : signature) =
  UnsizedType.pp ppf (UFun (args, rt, FnPlain, mem_pattern))

let pp_math_sigs ppf (sigs : signature list) =
  (Fmt.list ~sep:Fmt.cut pp_math_sig) ppf sigs

let pretty_print_math_sigs = Fmt.str "@[<v>@,%a@]" pp_math_sigs

let dist_name_suffix (module StdLibrary : Library) udf_names name =
  let is_udf_name s =
    List.exists ~f:(fun (n, _) -> String.equal s n) udf_names in
  Utils.distribution_suffices
  |> List.filter ~f:(fun sfx ->
         StdLibrary.is_stdlib_function_name (name ^ sfx)
         || is_udf_name (name ^ sfx) )
  |> List.hd_exn
