(** General functions and signatures for a Standard Library *)

open Middle

(* Types for the module representing the standard library *)
type fun_arg = UnsizedType.autodifftype * UnsizedType.t

type signature =
  UnsizedType.returntype * fun_arg list * Common.Helpers.mem_pattern

type variadic_checker =
     is_cond_dist:bool
  -> Location_span.t
  -> Environment.originblock
  -> Environment.t
  -> Ast.identifier
  -> Ast.typed_expression list
  -> Ast.typed_expression

let pp_math_sig ppf (rt, args, mem_pattern) =
  UnsizedType.pp ppf (UFun (args, rt, FnPlain, mem_pattern))

let pp_math_sigs ppf sigs = (Fmt.list ~sep:Fmt.cut pp_math_sig) ppf sigs
let pretty_print_math_sigs = Fmt.str "@[<v>@,%a@]" pp_math_sigs

module type Library = sig
  val stan_math_signatures : (string, signature list) Hashtbl.t
  (** Mapping from names to signature(s) of functions *)

  val distribution_families : string list

  val is_stan_math_function_name : string -> bool
  (** Equivalent to [Hashtbl.mem stan_math_signatures s]*)

  val is_not_overloadable : string -> bool
  val is_variadic_function_name : string -> bool
  val operator_to_function_names : Operator.t -> string list
end
