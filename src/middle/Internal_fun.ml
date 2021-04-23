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
      { constrain: 'expr Transformation.transformation
      ; dims: 'expr list }
  | FnWriteParam of
      { unconstrain_opt: 'expr Transformation.transformation option
      ; dims: 'expr list
      ; var: 'expr }
  | FnValidateSize
  | FnValidateSizeSimplex
  | FnValidateSizeUnitVector
  | FnConstrain of string
  | FnUnconstrain of string
  | FnCheck of
      { trans: 'expr Transformation.transformation
      ; var_name: string
      ; var: 'expr }
  | FnPrint
  | FnReject
  | FnResizeToMatch
  | FnNaN
  | FnDeepCopy
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

(* let of_string_opt x =
 *   try
 *     String.chop_suffix_exn ~suffix:"__" x
 *     |> Sexp.of_string |> t_of_sexp |> Some
 *   with
 *   | Sexplib.Conv.Of_sexp_error _ -> None
 *   | Invalid_argument _ -> None *)
