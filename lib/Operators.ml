(* -- Helpers for treatment of operators -- *)

open Core_kernel
open Type_conversion
open Ast

let ternary_if = "TernaryIf__"

(** A hash table to hold some name conversions between the AST nodes and the
    Stan Math name of the operator *)
let operator_names = String.Table.create ()

let _ = Hashtbl.add_multi operator_names ~key:"Plus__" ~data:"add"

(*XXX is there a difference in Stan math for unary plus?*)
let _ = Hashtbl.add_multi operator_names ~key:"PPlus__" ~data:"plus"
let _ = Hashtbl.add_multi operator_names ~key:"Minus__" ~data:"subtract"

(*XXX is there a difference in Stan math for unary minus?*)
let _ = Hashtbl.add_multi operator_names ~key:"PMinus__" ~data:"minus"
let _ = Hashtbl.add_multi operator_names ~key:"Times__" ~data:"multiply"
let _ = Hashtbl.add_multi operator_names ~key:"Divide__" ~data:"divide"
let _ = Hashtbl.add_multi operator_names ~key:"Divide__" ~data:"mdivide_right"
let _ = Hashtbl.add_multi operator_names ~key:"Modulo__" ~data:"modulus"
let _ = Hashtbl.add_multi operator_names ~key:"LDivide__" ~data:"mdivide_left"
let _ = Hashtbl.add_multi operator_names ~key:"EltTimes__" ~data:"elt_multiply"
let _ = Hashtbl.add_multi operator_names ~key:"EltDivide__" ~data:"elt_divide"
let _ = Hashtbl.add_multi operator_names ~key:"Pow__" ~data:"pow"
let _ = Hashtbl.add_multi operator_names ~key:"Or__" ~data:"logical_or"
let _ = Hashtbl.add_multi operator_names ~key:"And__" ~data:"logical_and"
let _ = Hashtbl.add_multi operator_names ~key:"Equals__" ~data:"logical_eq"
let _ = Hashtbl.add_multi operator_names ~key:"NEquals__" ~data:"logical_neq"
let _ = Hashtbl.add_multi operator_names ~key:"Less__" ~data:"logical_lt"
let _ = Hashtbl.add_multi operator_names ~key:"Leq__" ~data:"logical_lte"
let _ = Hashtbl.add_multi operator_names ~key:"Greater__" ~data:"logical_gt"
let _ = Hashtbl.add_multi operator_names ~key:"Geq__" ~data:"logical_gte"
let _ = Hashtbl.add_multi operator_names ~key:"PNot__" ~data:"logical_negation"
let _ = Hashtbl.add_multi operator_names ~key:"Transpose__" ~data:"transpose"
let _ = Hashtbl.add_multi operator_names ~key:ternary_if ~data:"if_else"

let _ =
  Hashtbl.add_multi operator_names ~key:"(OperatorAssign Plus)"
    ~data:"assign_add"

let _ =
  Hashtbl.add_multi operator_names ~key:"(OperatorAssign Minus)"
    ~data:"assign_subtract"

let _ =
  Hashtbl.add_multi operator_names ~key:"(OperatorAssign Times)"
    ~data:"assign_multiply"

let _ =
  Hashtbl.add_multi operator_names ~key:"(OperatorAssign Divide)"
    ~data:"assign_divide"

let _ =
  Hashtbl.add_multi operator_names ~key:"(OperatorAssign EltTimes)"
    ~data:"assign_elt_times"

let _ =
  Hashtbl.add_multi operator_names ~key:"(OperatorAssign EltDivide)"
    ~data:"assign_elt_divide"

(** Querying stan_math_signatures for operator signatures by string name *)
let operator_return_type_from_string op_name args =
  if op_name = "Assign" || op_name = "ArrowAssign" then
    match args with
    | [{expr_typed_type= ut1; _}; {expr_typed_type= ut2; _}]
      when check_of_same_type_mod_array_conv "" ut1 ut2 ->
        Some Void
    | _ -> None
  else
    let rec try_recursive_find = function
      | [] -> None
      | name :: names -> (
        match
          Stan_math_signatures.get_stan_math_function_return_type_opt name args
        with
        | None -> try_recursive_find names
        | Some ut -> Some ut )
    in
    try_recursive_find (Hashtbl.find_multi operator_names op_name)

let operator_name op = Sexp.to_string [%sexp (op : Ast.operator)] ^ "__"

let operator_return_type op =
  operator_return_type_from_string (operator_name op)

let operator_return_type_prefix op =
  operator_return_type_from_string ("P" ^ operator_name op)

(** Print all the signatures of a stan math operator, for the purposes of error messages. *)
let pretty_print_all_operator_signatures name =
  let all_names = Hashtbl.find_multi operator_names name in
  String.concat ~sep:"\n"
    (List.map
       ~f:Stan_math_signatures.pretty_print_all_stan_math_function_signatures
       all_names)

(** Print all the signatures of a stan math operator, for the purposes of error messages. *)
let pretty_print_all_operator_signatures_prefix name =
  pretty_print_all_operator_signatures ("P" ^ name)
