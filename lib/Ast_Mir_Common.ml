(** Types common to MIR and AST *)

open Core_kernel

(** Source code locations *)
type location =
  { filename: string
  ; line_num: int
  ; col_num: int
  ; included_from: location option }

(** Delimited locations *)
type location_span = {begin_loc: location; end_loc: location}

let merge_spans left right = {begin_loc= left.begin_loc; end_loc= right.end_loc}

(** Flags for data only arguments to functions *)
type autodifftype = DataOnly | AutoDiffable [@@deriving sexp, hash, compare]

(** Unsized types for function arguments and for decorating expressions
    during type checking; we have a separate type here for Math library
    functions as these functions can be overloaded, so do not have a unique
    type in the usual sense. Still, we want to assign a unique type to every
    expression during type checking.  *)
and unsizedtype =
  | UInt
  | UReal
  | UVector
  | URowVector
  | UMatrix
  | UArray of unsizedtype
  | UFun of (autodifftype * unsizedtype) list * returntype
  | UMathLibraryFunction
[@@deriving sexp, hash]

and returntype = Void | ReturnType of unsizedtype [@@deriving sexp, hash]

(** Sized types, for variable declarations *)
type 'e sizedtype =
  | SInt
  | SReal
  | SVector of 'e
  | SRowVector of 'e
  | SMatrix of 'e * 'e
  | SArray of 'e sizedtype * 'e
[@@deriving sexp, compare, map, hash]

(** remove_size [st] discards size information from a sizedtype
    to return an unsizedtype. *)
let rec remove_size = function
  | SInt -> UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SArray (t, _) -> UArray (remove_size t)

(** Transformations (constraints) for global variable declarations *)
type 'e transformation =
  | Identity
  | Lower of 'e
  | Upper of 'e
  | LowerUpper of 'e * 'e
  | Offset of 'e
  | Multiplier of 'e
  | OffsetMultiplier of 'e * 'e
  | Ordered
  | PositiveOrdered
  | Simplex
  | UnitVector
  | CholeskyCorr
  | CholeskyCov
  | Correlation
  | Covariance
[@@deriving sexp, compare, map, hash]

(* -- Pretty printers ------------------------------------------------------- *)

let pp_keyword = Fmt.(string |> styled `Blue)
let angle_brackets pp_v ppf v = Fmt.pf ppf "@[<1><%a>@]" pp_v v
let label str pp_v ppf v = Fmt.pf ppf "%s=%a" str pp_v v

let pp_autodifftype ppf = function
  | DataOnly -> pp_keyword ppf "data "
  | AutoDiffable -> ()

let rec pp_unsizedtype ppf = function
  | UInt -> pp_keyword ppf "int"
  | UReal -> pp_keyword ppf "real"
  | UVector -> pp_keyword ppf "vector"
  | URowVector -> pp_keyword ppf "row_vector"
  | UMatrix -> pp_keyword ppf "matrix"
  | UArray ut -> (Fmt.brackets pp_unsizedtype) ppf ut
  | UFun (argtypes, rt) ->
      Fmt.pf ppf {|%a => %a|}
        Fmt.(list (pair ~sep:comma pp_autodifftype pp_unsizedtype) ~sep:comma)
        argtypes pp_returntype rt
  | UMathLibraryFunction ->
      (angle_brackets Fmt.string) ppf "Stan Math function"

and pp_returntype ppf = function
  | Void -> Fmt.string ppf "void"
  | ReturnType ut -> pp_unsizedtype ppf ut

let pp_transformation pp_e ppf = function
  | Identity -> ()
  | Lower expr -> (pp_e |> label "lower" |> angle_brackets) ppf expr
  | Upper expr -> (pp_e |> label "upper" |> angle_brackets) ppf expr
  | LowerUpper (lower_expr, upper_expr) ->
      ( Fmt.(pair ~sep:comma (pp_e |> label "lower") (pp_e |> label "upper"))
      |> angle_brackets )
        ppf (lower_expr, upper_expr)
  | Offset expr -> (pp_e |> label "offet" |> angle_brackets) ppf expr
  | Multiplier expr -> (pp_e |> label "multiplier" |> angle_brackets) ppf expr
  | OffsetMultiplier (offset_expr, mult_expr) ->
      ( Fmt.(
          pair ~sep:comma (pp_e |> label "offset") (pp_e |> label "multiplier"))
      |> angle_brackets )
        ppf (offset_expr, mult_expr)
  | Ordered -> (angle_brackets Fmt.string) ppf "ordered"
  | PositiveOrdered -> (angle_brackets Fmt.string) ppf "positive_ordered"
  | Simplex -> (angle_brackets Fmt.string) ppf "simplex"
  | UnitVector -> (angle_brackets Fmt.string) ppf "unit_vector"
  | CholeskyCorr -> (angle_brackets Fmt.string) ppf "cholesky_factor_corr"
  | CholeskyCov -> (angle_brackets Fmt.string) ppf "cholesky_factor_cov"
  | Correlation -> (angle_brackets Fmt.string) ppf "corr_matrix"
  | Covariance -> (angle_brackets Fmt.string) ppf "cov_matrix"

let rec pp_sizedtype pp_e ppf (st, trans) =
  match st with
  | SInt -> Fmt.pf ppf {|%s%a|} "int" (pp_transformation pp_e) trans
  | SReal -> Fmt.pf ppf {|%s%a|} "real" (pp_transformation pp_e) trans
  | SVector expr ->
      Fmt.pf ppf {|vector%a%a|} (pp_transformation pp_e) trans
        (Fmt.brackets pp_e) expr
  | SRowVector expr ->
      Fmt.pf ppf {|row_vector%a%a|} (pp_transformation pp_e) trans
        (Fmt.brackets pp_e) expr
  | SMatrix (d1_expr, d2_expr) ->
      Fmt.pf ppf {|matrix%a%a|} (pp_transformation pp_e) trans
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets)
        (d1_expr, d2_expr)
  | SArray (st, expr) ->
      Fmt.pf ppf {|array%a%a|} (pp_transformation pp_e) trans
        Fmt.(
          pair ~sep:comma
            (fun ppf st -> pp_sizedtype pp_e ppf (st, Identity))
            pp_e
          |> brackets)
        (st, expr)
