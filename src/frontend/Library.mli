(** This is a {{:https://dune.readthedocs.io/en/latest/variants.html}virtual module}
    which is filled in at link time with a module specifying a backend-specific
    Stan library. *)

open Middle
open Core_kernel
open Std_library_utils

val function_signatures : (string, signature list) Hashtbl.t
(** Mapping from names to signature(s) of functions
    Used in [Environment] to produce the base type environment
*)

val variadic_signatures : (string, variadic_signature) Hashtbl.t
(** Mapping from names to description of a variadic function.
Note that these function names cannot be overloaded, and usually require
customized code-gen in the backend.
*)

val distribution_families : string list
(** A list of the families of distribution are available,
    e.g. "normal", "bernoulli". Used to produce better
    errors in typechecking *)

val is_stdlib_function_name : string -> bool
(** Equivalent to [Hashtbl.mem function_signatures s] *)

val get_signatures : string -> signature list
(** Equivalent to [Hashtbl.find_multi function_signatures s]*)

val get_operator_signatures : Operator.t -> signature list
(* Much like get_signatures, this returns the available
   signatures for a given operator, which may represent
   multiple internal functions *)

val get_assignment_operator_signatures : Operator.t -> signature list
(* Get the signatures allowed when this operator is used as an infix
   assignment, e.g. [a += b]. By convention these have returntype [Void]
*)

val is_not_overloadable : string -> bool
(** This controls which functions the typechecker will not allow
    to be overloaded with additional signatures from user defined functions.
    In the Stan C++ library, this is equal to [is_variadic_function_name],
    but it could be more or less broad, to the limit of disallowing all overloading
    by setting it equal to [is_stdlib_function_name]
*)

val is_variadic_function_name : string -> bool
(** Variadic functions are handled as generally as possible
    using the above hashtable
*)

val is_special_function_name : string -> bool
(** Special functions like [reduce_sum] are {b not} included in the normal signatures
    above, but instead recognized by this function and special-cased during
    typechecking
*)

val special_function_returntype : string -> UnsizedType.returntype option
(** We currently have the restriction that variadic functions must have the same
    return type regardless of their argument types. This function should return that type,
    or None if it is given a name that is not a variadic function. *)

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
(** This should return the name of function(s) (in the above signature table)
    which handle the given operator. For example, in Stan's C++ backend,
    [Operator.Plus] is represented by the functions [["add"]]
*)

val string_operator_to_function_name : string -> string
(** Serves the same role as [operator_to_function_names],
    but the input is the serialized string version used later
    in the compiler, e.g. [Operator.PMinus] is ["PMinus__"]
    *)

val deprecated_distributions : deprecation_info String.Map.t
(** This should map any deprecated distribution functions, e.g. "normal_log"
    to information about their replacements and removal version.
*)

val deprecated_functions : deprecation_info String.Map.t
(** This should map any deprecated distribution functions, e.g. "cov_exp_quad"
    to information about their replacements and removal version.
*)
