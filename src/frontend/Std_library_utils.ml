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

let pp_math_sig ppf ((rt, args, mem_pattern) : signature) =
  UnsizedType.pp ppf (UFun (args, rt, FnPlain, mem_pattern))

let pp_math_sigs ppf (sigs : signature list) =
  (Fmt.list ~sep:Fmt.cut pp_math_sig) ppf sigs

let pretty_print_math_sigs = Fmt.str "@[<v>@,%a@]" pp_math_sigs

let dist_name_suffix (check : string -> bool) udf_names name =
  let is_udf_name s =
    List.exists ~f:(fun (n, _) -> String.equal s n) udf_names in
  Utils.distribution_suffices
  |> List.filter ~f:(fun sfx ->
         check (name ^ sfx) || is_udf_name (name ^ sfx) )
  |> List.hd_exn
