(* -- Helpers for treatment of operators -- *)

open Type_conversion
open Ast

(** A hash table to hold some name conversions between the AST nodes and the
    Stan Math name of the operator *)
let operator_names = Hashtbl.create 50

let _ = Hashtbl.add operator_names "Plus" "add"
let _ = Hashtbl.add operator_names "Minus" "subtract"
let _ = Hashtbl.add operator_names "Times" "multiply"
let _ = Hashtbl.add operator_names "Divide" "divide"
let _ = Hashtbl.add operator_names "Divide" "mdivide_right"
let _ = Hashtbl.add operator_names "Modulo" "modulus"
let _ = Hashtbl.add operator_names "LDivide" "mdivide_left"
let _ = Hashtbl.add operator_names "EltTimes" "elt_multiply"
let _ = Hashtbl.add operator_names "EltDivide" "elt_divide"
let _ = Hashtbl.add operator_names "Exp" "pow"
let _ = Hashtbl.add operator_names "Or" "logical_or"
let _ = Hashtbl.add operator_names "And" "logical_and"
let _ = Hashtbl.add operator_names "Equals" "logical_eq"
let _ = Hashtbl.add operator_names "NEquals" "logical_neq"
let _ = Hashtbl.add operator_names "Less" "logical_lt"
let _ = Hashtbl.add operator_names "Leq" "logical_lte"
let _ = Hashtbl.add operator_names "Greater" "logical_gt"
let _ = Hashtbl.add operator_names "Geq" "logical_gte"
let _ = Hashtbl.add operator_names "Not" "logical_negation"
let _ = Hashtbl.add operator_names "UMinus" "minus"
let _ = Hashtbl.add operator_names "UPlus" "plus"
let _ = Hashtbl.add operator_names "Transpose" "transpose"
let _ = Hashtbl.add operator_names "Conditional" "if_else"
let _ = Hashtbl.add operator_names "(OperatorAssign Plus)" "assign_add"
let _ = Hashtbl.add operator_names "(OperatorAssign Minus)" "assign_subtract"
let _ = Hashtbl.add operator_names "(OperatorAssign Times)" "assign_multiply"
let _ = Hashtbl.add operator_names "(OperatorAssign Divide)" "assign_divide"

let _ =
  Hashtbl.add operator_names "(OperatorAssign EltTimes)" "assign_elt_times"

let _ =
  Hashtbl.add operator_names "(OperatorAssign EltDivide)" "assign_elt_divide"

(** Querying stan_math_signatures for operator signatures *)
let get_operator_return_type_opt op_name argtypes =
  if op_name = "Assign" || op_name = "ArrowAssign" then
    match argtypes with
    | [(_, ut1); (_, ut2)] ->
        if check_of_same_type_mod_array_conv "" ut1 ut2 then Some Void
        else None
    | _ -> None
  else
    let rec try_recursive_find = function
      | [] -> None
      | name :: names -> (
        match
          Stan_math_signatures.get_stan_math_function_return_type_opt name
            argtypes
        with
        | None -> try_recursive_find names
        | Some ut -> Some ut )
    in
    try_recursive_find (Hashtbl.find_all operator_names op_name)

(** Print all the signatures of a stan math operator, for the purposes of error messages. *)
let pretty_print_all_operator_signatures name =
  let all_names = Hashtbl.find_all operator_names name in
  String.concat "\n"
    (List.map
       Stan_math_signatures.pretty_print_all_stan_math_function_signatures
       all_names)
