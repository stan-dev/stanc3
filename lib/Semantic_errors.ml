open Core_kernel
open Pretty_printing
open Mir

(** Low-level semantic errors used for validation *)
type t =
  | IdentifierIsKeyword of location_span * string
  | IdentifierIsModelName of location_span * string
  | IdentifierIsStanMathName of location_span * string
  | IdentifierInUse of location_span * string
  | IdentifierNotInScope of location_span * string
  | InvalidIndex of location_span * unsizedtype
  | TargetOutsideModelBlock of location_span
  | IllTypedIfReturnTypes of location_span * returntype * returntype
  | IllTypedTernaryIf of
      location_span * unsizedtype * unsizedtype * unsizedtype
  | IllTypedReturningFunction of location_span * string
  | IllTypedNonReturningFunction of location_span * string
  | IllTypedStanLibFunApp of location_span * string * unsizedtype list
  | IllTypedUserFunApp of
      location_span
      * string
      * (autodifftype * unsizedtype) list
      * returntype
      * unsizedtype list
  | IllTypedNotAFunction of location_span * string
  | IllTypedNotANRFunction of location_span * string
  | IllTypedNoSuchFunction of location_span * string
  | IllTypedNoSuchNRFunction of location_span * string
  | IllTypedBinOp of location_span * Ast.operator * unsizedtype * unsizedtype
  | IllTypedPrefixOp of location_span * Ast.operator * unsizedtype
  | IllTypedPostfixOp of location_span * Ast.operator * unsizedtype
  | FnMapRect of location_span * string
  | FnConditioning of location_span
  | FnTargetPlusEquals of location_span
  | FnRng of location_span

let string_of_operator = mk_string_of Ast.sexp_of_operator
let ternary_if = "TernaryIf__"

(** A hash table to hold some name conversions between the AST nodes and the
    Stan Math name of the operator *)
let string_of_operators =
  Map.Poly.of_alist_multi
    [ (string_of_operator Ast.Plus, "add")
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

let pretty_print_all_operator_signatures name =
  Map.Poly.find_multi string_of_operators name
  |> List.map ~f:Stan_math_signatures.pretty_print_all_math_lib_fn_sigs
  |> String.concat ~sep:"\n"

let to_exception = function
  | IdentifierIsStanMathName (loc, name) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "Identifier '%s' clashes with Stan Math library function." name)
  | IdentifierInUse (loc, name) ->
      Errors.semantic_error ~loc
        (Format.sprintf "Identifier '%s' is already in use." name)
  | IdentifierIsModelName (loc, name) ->
      Errors.semantic_error ~loc
        (Format.sprintf "Identifier '%s' clashes with model name." name)
  | IdentifierIsKeyword (loc, name) ->
      Errors.semantic_error ~loc
        (Format.sprintf "Identifier '%s' clashes with reserved keyword." name)
  | IdentifierNotInScope (loc, name) ->
      Errors.semantic_error ~loc
        (Format.sprintf "Identifier '%s' not in scope." name)
  | InvalidIndex (loc, ut) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "Only expressions of array, matrix, row_vector and vector type may \
            be indexed. Instead, found type %s."
           (pretty_print_unsizedtype ut))
  | TargetOutsideModelBlock loc ->
      Errors.semantic_error ~loc
        "Target can only be accessed in the model block or in definitions of \
         functions with the suffix _lp."
  | IllTypedIfReturnTypes (loc, rt1, rt2) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "Branches of function definition need to have the same return \
            type. Instead, found return types %s and %s."
           (pretty_print_returntype rt1)
           (pretty_print_returntype rt2))
  | IllTypedTernaryIf (loc, ut1, ut2, ut3) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "Ill-typed arguments supplied to ? : operator. Available \
            signatures: %s\n\
            Instead supplied arguments of incompatible type: %s,%s,%s"
           (pretty_print_all_operator_signatures ternary_if)
           (pretty_print_unsizedtype ut1)
           (pretty_print_unsizedtype ut2)
           (pretty_print_unsizedtype ut3))
  | IllTypedReturningFunction (loc, name) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "A returning function was expected but a non-returning function \
            '%s' was supplied."
           name)
  | IllTypedNonReturningFunction (loc, name) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "A non-returning function was expected but a returning function \
            '%s' was supplied."
           name)
  | IllTypedStanLibFunApp (loc, name, arg_tys) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "Ill-typed arguments supplied to function '%s'. Available \
            signatures: %s\n\
            Instead supplied arguments of incompatible type: %s."
           name
           (Stan_math_signatures.pretty_print_all_math_lib_fn_sigs name)
           (pretty_print_unsizedtypes arg_tys))
  | IllTypedUserFunApp (loc, name, listed_tys, return_ty, arg_tys) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "Ill-typed arguments supplied to function '%s'. Available \
            signatures:\n\
            %s\n\
            Instead supplied arguments of incompatible type: %s."
           name
           (pretty_print_unsizedtype (UFun (listed_tys, return_ty)))
           (pretty_print_unsizedtypes arg_tys))
  | IllTypedNotAFunction (loc, name) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "A non-returning function was expected but a non-function value \
            '%s' was supplied."
           name)
  | IllTypedNotANRFunction (loc, name) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "A returning function was expected but a non-function value '%s' \
            was supplied."
           name)
  | IllTypedNoSuchFunction (loc, name) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "A returning function was expected but an undeclared identifier \
            '%s' was supplied."
           name)
  | IllTypedNoSuchNRFunction (loc, name) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "A non-returning function was expected but an undeclared \
            identifier '%s' was supplied."
           name)
  | IllTypedBinOp (loc, op, lt, rt) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "Ill-typed arguments supplied to infix operator %s. Available \
            signatures: %s\n\
            Instead supplied arguments of incompatible type: %s,%s."
           (pretty_print_operator op)
           (pretty_print_all_operator_signatures (string_of_operator op))
           (pretty_print_unsizedtype lt)
           (pretty_print_unsizedtype rt))
  | IllTypedPrefixOp (loc, op, ut) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "Ill-typed arguments supplied to prefix operator %s. Available \
            signatures %s\n\
            Instead supplied argument of incompatible type: %s."
           (pretty_print_operator op)
           (pretty_print_all_operator_signatures (string_of_operator op))
           (pretty_print_unsizedtype ut))
  | IllTypedPostfixOp (loc, op, ut) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "Ill-typed arguments supplied to postfix operator %s. Available \
            signatures %s\n\
            Instead supplied argument of incompatible type: %s."
           (pretty_print_operator op)
           (pretty_print_all_operator_signatures (string_of_operator op))
           (pretty_print_unsizedtype ut))
  | FnMapRect (loc, name) ->
      Errors.semantic_error ~loc
        (Format.sprintf
           "Mapped function cannot be an _rng or _lp function, found function \
            name: %s"
           name)
  | FnConditioning loc ->
      Errors.semantic_error ~loc
        "Probabilty functions with suffixes _lpdf, _lpmf, _lcdf, and _lccdf, \
         require a vertical bar (|) between the first two arguments."
  | FnTargetPlusEquals loc ->
      Errors.semantic_error ~loc
        "Target can only be accessed in the model block or in definitions of \
         functions with the suffix _lp."
  | FnRng loc ->
      Errors.semantic_error ~loc
        "Random number generators are only allowed in transformed data block, \
         generated quantities block or user-defined functions with names \
         ending in _rng."
