(** Language functions defined internally by the compiler *)

open Core_kernel

type 'expr t =
  | FnLength
  | FnMakeArray
  | FnMakeRowVec
  | FnNegInf
  (* In AST_to_MIR being used as StanLib *)
  | FnReadData
  | FnReadDataSerializer
  (* XXX move these to a backend specific file?*)
  | FnReadParam of
      { constrain: 'expr Transformation.t
      ; dims: 'expr list
      ; mem_pattern: Common.Helpers.mem_pattern }
  | FnWriteParam of {unconstrain_opt: 'expr Transformation.t option; var: 'expr}
  | FnValidateSize
  | FnValidateSizeSimplex
  | FnValidateSizeUnitVector
  | FnCheck of {trans: 'expr Transformation.t; var_name: string; var: 'expr}
  | FnPrint
  | FnReject
  | FnResizeToMatch
  | FnNaN
  | FnDeepCopy
  | FnReadWriteEventsOpenCL of string
[@@deriving sexp, hash, compare, map, fold]

let to_string
    ?(expr_to_string =
      fun _ ->
        raise
          (Failure
             "Should not be parsing expression from string in function renaming"))
    x =
  Sexp.to_string (sexp_of_t expr_to_string x) ^ "__"

let pp (pp_expr : 'a Fmt.t) ppf internal =
  Fmt.string ppf
    (to_string
       ~expr_to_string:(fun expr -> sexp_of_string (Fmt.strf "%a" pp_expr expr))
       internal)

(* Does this function call change state? Can we call it twice with the same results?

   E.g., FnReadDataSerializer moves the serializer forward, so calling it again has
   different results

   Useful for optimizations
*)
let can_side_effect = function
  | FnReadParam _ | FnReadData | FnReadDataSerializer | FnWriteParam _
   |FnValidateSize | FnValidateSizeSimplex | FnValidateSizeUnitVector
   |FnReadWriteEventsOpenCL _ ->
      true
  | FnLength | FnMakeArray | FnMakeRowVec | FnNegInf | FnPrint | FnReject
   |FnResizeToMatch | FnNaN | FnDeepCopy | FnCheck _ ->
      false

let collect_exprs fn = fold (fun accum e -> e :: accum) [] fn
