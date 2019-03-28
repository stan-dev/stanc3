(* -- Helpers for treatment of operators -- *)

open Core_kernel
open Type_conversion
open Ast

let ternary_if = "TernaryIf__"
let operator_name op = Sexp.to_string [%sexp (op : Ast.operator)] ^ "__"

(** A hash table to hold some name conversions between the AST nodes and the
    Stan Math name of the operator *)
let operator_names =
  Map.Poly.of_alist_multi
    [ (operator_name Plus, "add")
    ; (operator_name PPlus, "plus")
    ; (operator_name Minus, "subtract")
    ; (operator_name PMinus, "minus")
    ; (operator_name Times, "multiply")
    ; (operator_name Divide, "mdivide_right")
    ; (operator_name Divide, "divide")
    ; (operator_name Modulo, "modulus")
    ; (operator_name LDivide, "mdivide_left")
    ; (operator_name EltTimes, "elt_multiply")
    ; (operator_name EltDivide, "elt_divide")
    ; (operator_name Pow, "pow")
    ; (operator_name Or, "logical_or")
    ; (operator_name And, "logical_and")
    ; (operator_name Equals, "logical_eq")
    ; (operator_name NEquals, "logical_neq")
    ; (operator_name Less, "logical_lt")
    ; (operator_name Leq, "logical_lte")
    ; (operator_name Greater, "logical_gt")
    ; (operator_name Geq, "logical_gte")
    ; (operator_name PNot, "logical_negation")
    ; (operator_name Transpose, "transpose")
    ; (ternary_if, "if_else")
      (* XXX I don't think the following are able to be looked up at all as they aren't Ast.operators *)
    ; ("(OperatorAssign Plus)", "assign_add")
    ; ("(OperatorAssign Minus)", "assign_subtract")
    ; ("(OperatorAssign Times)", "assign_multiply")
    ; ("(OperatorAssign Divide)", "assign_divide")
    ; ("(OperatorAssign EltTimes)", "assign_elt_times")
    ; ("(OperatorAssign EltDivide)", "assign_elt_divide") ]

(** Querying stan_math_signatures for operator signatures by string name *)
let operator_return_type_from_string op_name args =
  if op_name = "Assign" || op_name = "ArrowAssign" then
    match args with
    | [{expr_typed_type= ut1; _}; {expr_typed_type= ut2; _}]
      when check_of_same_type_mod_array_conv "" ut1 ut2 ->
        Some Void
    | _ -> None
  else
    Map.Poly.find_multi operator_names op_name
    |> List.find_map ~f:(fun name ->
           Stan_math_signatures.stan_math_returntype name args )

let operator_return_type op =
  operator_return_type_from_string (operator_name op)

(** Print all the signatures of a stan math operator, for the purposes of error messages. *)
let pretty_print_all_operator_signatures name =
  Map.Poly.find_multi operator_names name
  |> List.map ~f:Stan_math_signatures.pretty_print_all_math_lib_fn_sigs
  |> String.concat ~sep:"\n"
