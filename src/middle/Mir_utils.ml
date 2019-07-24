open Core_kernel
open Mir_pattern

(* ===================== Some helper functions and values ====================== *)

(* let expr_from_idx (i : expr_typed_located index) =
  match i with
  | All -> []
  | Single e | Upfrom e | MultiIndex e -> [e]
  | Between (e1, e2) -> [e1; e2] *)

(** remove_size [st] discards size information from a sizedtype
    to return an unsizedtype. *)
let rec remove_size = function
  | SInt -> UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SArray (t, _) -> UArray (remove_size t)

let remove_possible_size = function Sized t -> remove_size t | Unsized t -> t
let mk_string_of sexp_of x = Sexp.to_string (sexp_of x) ^ "__"
let string_of_internal_fn = mk_string_of sexp_of_internal_fn

let mk_of_string of_sexp x =
  try
    String.chop_suffix_exn ~suffix:"__" x |> Sexp.of_string |> of_sexp |> Some
  with
  | Sexplib.Conv.Of_sexp_error _ -> None
  | Invalid_argument _ -> None

let internal_fn_of_string = mk_of_string internal_fn_of_sexp

(* let internal_funapp ifn args emeta =
  {expr= FunApp (CompilerInternal, string_of_internal_fn ifn, args); emeta}

let internal_meta = {mloc= no_span; mtype= UInt; madlevel= DataOnly}
let zero = {expr= Lit (Int, "0"); emeta= internal_meta}
let loop_bottom = {expr= Lit (Int, "1"); emeta= internal_meta} *)
let string_of_operator = mk_string_of sexp_of_operator
let operator_of_string = mk_of_string operator_of_sexp

let%test "bad op name" = phys_equal (operator_of_string "Pluss__") None
let%test "good op name" = operator_of_string "Plus__" = Some Plus

(* let binop e1 binop e2 =
  { expr= FunApp (StanLib, string_of_operator binop, [e1; e2])
  ; emeta= internal_meta } *)

(** remove_size [st] discards size information from a sizedtype
    to return an unsizedtype. *)
let rec remove_size = function
  | SInt -> UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SArray (t, _) -> UArray (remove_size t)

let ternary_if = "TernaryIf__"

(** A hash table to hold some name conversions between the AST nodes and the
    Stan Math name of the operator *)
let string_of_operators =
  Map.Poly.of_alist_multi
    [ (string_of_operator Plus, "add")
    ; (string_of_operator PPlus, "plus")
    ; (string_of_operator Minus, "subtract")
    ; (string_of_operator PMinus, "minus")
    ; (string_of_operator Times, "multiply")
    ; (string_of_operator Divide, "mdivide_right")
    ; (string_of_operator Divide, "divide")
    ; (string_of_operator Modulo, "modulus")
    ; (string_of_operator LDivide, "mdivide_left")
    ; (string_of_operator EltTimes, "elt_multiply")
    ; (string_of_operator EltDivide, "elt_divide")
    ; (string_of_operator Pow, "pow")
    ; (string_of_operator Or, "logical_or")
    ; (string_of_operator And, "logical_and")
    ; (string_of_operator Equals, "logical_eq")
    ; (string_of_operator NEquals, "logical_neq")
    ; (string_of_operator Less, "logical_lt")
    ; (string_of_operator Leq, "logical_lte")
    ; (string_of_operator Greater, "logical_gt")
    ; (string_of_operator Geq, "logical_gte")
    ; (string_of_operator PNot, "logical_negation")
    ; (string_of_operator Transpose, "transpose")
    ; (ternary_if, "if_else")
      (* XXX I don't think the following are able to be looked up at all as they aren't Ast.operators *)
    ; ("(OperatorAssign Plus)", "assign_add")
    ; ("(OperatorAssign Minus)", "assign_subtract")
    ; ("(OperatorAssign Times)", "assign_multiply")
    ; ("(OperatorAssign Divide)", "assign_divide")
    ; ("(OperatorAssign EltTimes)", "assign_elt_times")
    ; ("(OperatorAssign EltDivide)", "assign_elt_divide") ]
