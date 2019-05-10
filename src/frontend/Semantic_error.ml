open Core_kernel
open Middle

(** Type errors that may arise during semantic check *)
module TypeError = struct
  type t =
    | MismatchedReturnTypes of returntype * returntype
    | MismatchedArrayTypes
    | RowVectorTypes
    | IntExpected of string * unsizedtype
    | IntOrRealExpected of string * unsizedtype
    | IntIntArrayOrRangeExpected of unsizedtype
    | IntOrRealContainerExpected of unsizedtype
    | ArrayVectorRowVectorMatrixExpected of unsizedtype
    | Assignment of Ast.assignmentoperator * unsizedtype * unsizedtype
    | TernaryIf of unsizedtype * unsizedtype * unsizedtype
    | ReturningFnExpectedNonReturningFound of string
    | ReturningFnExpectedNonFnFound of string
    | ReturningFnExpectedUndeclaredIdentFound of string
    | NonReturningFnExpectedReturningFound of string
    | NonReturningFnExpectedNonFnFound of string
    | NonReturningFnExpectedUndeclaredIdentFound of string
    | StanLibFunctionApp of string * unsizedtype list
    | UserDefinedFunctionApp of
        string
        * (autodifftype * unsizedtype) list
        * returntype
        * unsizedtype list
    | BinaryOperator of operator * unsizedtype * unsizedtype
    | PrefixOperator of operator * unsizedtype
    | PostfixOperator of operator * unsizedtype
    | NotIndexable of unsizedtype

  let assignmentoperator_to_stan_math_fn = function
    | Plus -> Some "assign_add"
    | Minus -> Some "assign_subtract"
    | Times -> Some "assign_multiply"
    | Divide -> Some "assign_divide"
    | EltTimes -> Some "assign_elt_times"
    | EltDivide -> Some "assign_elt_divide"
    | _ -> None

  let operator_to_stan_math_fns = function
    | Plus -> ["add"]
    | PPlus -> ["plus"]
    | Minus -> ["subtract"]
    | PMinus -> ["minus"]
    | Times -> ["multiply"]
    | Divide -> ["mdivide_right"; "divide"]
    | Modulo -> ["modulus"]
    | LDivide -> ["mdivide_left"]
    | EltTimes -> ["elt_multiply"]
    | EltDivide -> ["elt_divide"]
    | Pow -> ["pow"]
    | Or -> ["logical_or"]
    | And -> ["logical_and"]
    | Equals -> ["logical_eq"]
    | NEquals -> ["logical_neq"]
    | Less -> ["logical_lt"]
    | Leq -> ["logical_lte"]
    | Greater -> ["logical_gt"]
    | Geq -> ["logical_gte"]
    | PNot -> ["logical_negation"]
    | Transpose -> ["transpose"]

  let pp ppf = function
    | MismatchedReturnTypes (rt1, rt2) ->
        Fmt.pf ppf
          "Branches of function definition need to have the same return type. \
           Instead, found return types %a and %a."
          pp_returntype rt1 pp_returntype rt2
    | MismatchedArrayTypes ->
        Fmt.pf ppf "Array expression must have entries of consistent type."
    | RowVectorTypes ->
        Fmt.pf ppf
          "Row_vector expression must have all int and real entries or all \
           row_vector entries."
    | IntExpected (name, ut) ->
        Fmt.pf ppf "%s must be of type int. Instead found type %a." name
          pp_unsizedtype ut
    | IntOrRealExpected (name, ut) ->
        Fmt.pf ppf "%s must be of type int or real. Instead found type %a."
          name pp_unsizedtype ut
    | IntOrRealContainerExpected ut ->
        Fmt.pf ppf
          "A (container of) real or int was expected. Instead found type %a."
          pp_unsizedtype ut
    | IntIntArrayOrRangeExpected ut ->
        Fmt.pf ppf
          "Index must be of type int or int[] or must be a range. Instead \
           found type %a."
          pp_unsizedtype ut
    | ArrayVectorRowVectorMatrixExpected ut ->
        Fmt.pf ppf
          "Foreach-loop must be over array, vector, row_vector or matrix. \
           Instead found expression of type %a."
          pp_unsizedtype ut
    | Assignment ((OperatorAssign op as assignop), lt, rt) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to assignment operator %s :lhs has \
           type %a and rhs has type %a. Available signatures: %s."
          (Pretty_printing.pretty_print_assignmentoperator assignop)
          pp_unsizedtype lt pp_unsizedtype rt
          ( assignmentoperator_to_stan_math_fn op
          |> Option.map
               ~f:Stan_math_signatures.pretty_print_all_math_lib_fn_sigs
          |> Option.value ~default:"no matching signatures" )
    | Assignment (assignop, lt, rt) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to assignment operator %s :lhs has \
           type %a and rhs has type %a."
          (Pretty_printing.pretty_print_assignmentoperator assignop)
          pp_unsizedtype lt pp_unsizedtype rt
    | TernaryIf (ut1, ut2, ut3) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to ? : operator. Available \
           signatures: %s\n\
           Instead supplied arguments of incompatible type: %a,%a,%a"
          (Stan_math_signatures.pretty_print_all_math_lib_fn_sigs "if_else")
          pp_unsizedtype ut1 pp_unsizedtype ut2 pp_unsizedtype ut3
    | NotIndexable ut ->
        Fmt.pf ppf
          "Only expressions of array, matrix, row_vector and vector type may \
           be indexed. Instead, found type %a."
          pp_unsizedtype ut
    | ReturningFnExpectedNonReturningFound fn_name ->
        Fmt.pf ppf
          "A returning function was expected but a non-returning function \
           '%s' was supplied."
          fn_name
    | NonReturningFnExpectedReturningFound fn_name ->
        Fmt.pf ppf
          "A non-returning function was expected but a returning function \
           '%s' was supplied."
          fn_name
    | ReturningFnExpectedNonFnFound fn_name ->
        Fmt.pf ppf
          "A returning function was expected but a non-function value '%s' \
           was supplied."
          fn_name
    | NonReturningFnExpectedNonFnFound fn_name ->
        Fmt.pf ppf
          "A non-returning function was expected but a non-function value \
           '%s' was supplied."
          fn_name
    | ReturningFnExpectedUndeclaredIdentFound fn_name ->
        Fmt.pf ppf
          "A returning function was expected but an undeclared identifier \
           '%s' was supplied."
          fn_name
    | NonReturningFnExpectedUndeclaredIdentFound fn_name ->
        Fmt.pf ppf
          "A non-returning function was expected but an undeclared identifier \
           '%s' was supplied."
          fn_name
    | StanLibFunctionApp (name, arg_tys) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to function '%s'. Available \
           signatures: %s\n\
           Instead supplied arguments of incompatible type: %a."
          name
          (Stan_math_signatures.pretty_print_all_math_lib_fn_sigs name)
          Fmt.(list pp_unsizedtype ~sep:comma)
          arg_tys
    | UserDefinedFunctionApp (name, listed_tys, return_ty, arg_tys) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to function '%s'. Available signatures:\n\
           %a\n\
           Instead supplied arguments of incompatible type: %a."
          name pp_unsizedtype
          (UFun (listed_tys, return_ty))
          Fmt.(list pp_unsizedtype ~sep:comma)
          arg_tys
    | BinaryOperator (op, lt, rt) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to infix operator %a. Available \
           signatures: %s\n\
           Instead supplied arguments of incompatible type: %a,%a."
          pp_operator op
          ( operator_to_stan_math_fns op
          |> List.map ~f:Stan_math_signatures.pretty_print_all_math_lib_fn_sigs
          |> String.concat ~sep:"\n" )
          pp_unsizedtype lt pp_unsizedtype rt
    | PrefixOperator (op, ut) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to prefix operator %a. Available \
           signatures %s\n\
           Instead supplied argument of incompatible type: %a."
          pp_operator op
          ( operator_to_stan_math_fns op
          |> List.map ~f:Stan_math_signatures.pretty_print_all_math_lib_fn_sigs
          |> String.concat ~sep:"\n" )
          pp_unsizedtype ut
    | PostfixOperator (op, ut) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to postfix operator %a. Available \
           signatures %s\n\
           Instead supplied argument of incompatible type: %a."
          pp_operator op
          ( operator_to_stan_math_fns op
          |> List.map ~f:Stan_math_signatures.pretty_print_all_math_lib_fn_sigs
          |> String.concat ~sep:"\n" )
          pp_unsizedtype ut
end

module IdentifierError = struct
  type t =
    | IsKeyword of string
    | IsModelName of string
    | IsStanMathName of string
    | InUse of string
    | NotInScope of string

  let pp ppf = function
    | IsStanMathName name ->
        Fmt.pf ppf "Identifier '%s' clashes with Stan Math library function."
          name
    | InUse name -> Fmt.pf ppf "Identifier '%s' is already in use." name
    | IsModelName name ->
        Fmt.pf ppf "Identifier '%s' clashes with model name." name
    | IsKeyword name ->
        Fmt.pf ppf "Identifier '%s' clashes with reserved keyword." name
    | NotInScope name -> Fmt.pf ppf "Identifier '%s' not in scope." name
end

module ExpressionError = struct
  type t =
    | MapRect of string
    | RngFunction
    | ConditionalNotationNotAllowed
    | ConditioningRequired
    | NotPrintable

  let pp ppf = function
    | MapRect fn_name ->
        Fmt.pf ppf
          "Mapped function cannot be an _rng or _lp function, found function \
           name: %s"
          fn_name
    | RngFunction ->
        Fmt.pf ppf
          "Random number generators are only allowed in transformed data \
           block, generated quantities block or user-defined functions with \
           names ending in _rng."
    | ConditionalNotationNotAllowed ->
        Fmt.pf ppf
          "Only functions with names ending in _lpdf, _lpmf, _lcdf, _lccdf \
           can make use of conditional notation."
    | ConditioningRequired ->
        Fmt.pf ppf
          "Probabilty functions with suffixes _lpdf, _lpmf, _lcdf, and \
           _lccdf, require a vertical bar (|) between the first two arguments."
    | NotPrintable -> Fmt.pf ppf "Functions cannot be printed."
end

module StatementError = struct
  type t =
    | CannotAssignToReadOnly of string
    | CannotAssignToGlobal of string
    | SamplingPDForPMF
    | SamplingCDForCCDF of string
    | SamplingNoSuchDistribution of string
    | TargetPlusEqualsOutsideModelOrLogProb
    | TruncationCDForCCDF
    | BreakOutsideLoop
    | ContinueOutsideLoop
    | ExpressionReturnOutsideReturningFn
    | VoidReturnOutsideNonReturningFn
    | NonDataVariableDecl
    | NonIntBounds
    | TransformedParamsInt
    | MismatchFunDefDecl of string * unsizedtype option
    | FunDeclExists of string
    | FunDeclNoDefn
    | NonRealProbFunDef
    | ProbDensityNonRealVariate of unsizedtype option
    | ProbMassNonIntVariate of unsizedtype option
    | DuplicateArgNames
    | IncompatibleReturnType

  let pp ppf = function
    | CannotAssignToReadOnly name ->
        Fmt.pf ppf
          "Cannot assign to function argument or loop identifier '%s'." name
    | CannotAssignToGlobal name ->
        Fmt.pf ppf
          "Cannot assign to global variable '%s' declared in previous blocks."
          name
    | TargetPlusEqualsOutsideModelOrLogProb ->
        Fmt.pf ppf
          "Target can only be accessed in the model block or in definitions \
           of functions with the suffix _lp."
    | SamplingPDForPMF ->
        Fmt.pf ppf
          "Sampling statement expects a distribution name without '_lpdf' or \
           '_lpmf' suffix."
    | SamplingCDForCCDF name ->
        Fmt.pf ppf
          "CDF or CCDF functions may not be used with sampling notation. Use \
           'increment_log_prob(%s_log(...))' instead."
          name
    | SamplingNoSuchDistribution name ->
        Fmt.pf ppf
          "Ill-typed argument to '~' statement. No distributon '%s' was found \
           with the correct signature."
          name
    | TruncationCDForCCDF ->
        Fmt.pf ppf
          "Truncation is only defined if distribution has _lcdf and _lccdf \
           functions implemented with appropriate signature."
    | BreakOutsideLoop ->
        Fmt.pf ppf "Break statements may only be used in loops."
    | ContinueOutsideLoop ->
        Fmt.pf ppf "Continue statements may only be used in loops."
    | ExpressionReturnOutsideReturningFn ->
        Fmt.pf ppf
          "Expression return statements may only be used inside returning \
           function definitions."
    | VoidReturnOutsideNonReturningFn ->
        Fmt.pf ppf
          "Void return statements may only be used inside non-returning \
           function definitions."
    | NonDataVariableDecl ->
        Fmt.pf ppf
          "Non-data variables are not allowed in top level size declarations."
    | NonIntBounds ->
        Fmt.pf ppf
          "Bounds of integer variable must be of type int. Found type real."
    | TransformedParamsInt ->
        Fmt.pf ppf "(Transformed) Parameters cannot be integers."
    | MismatchFunDefDecl (name, Some ut) ->
        Fmt.pf ppf "Function '%s' has already been declared to have type %a."
          name pp_unsizedtype ut
    | MismatchFunDefDecl (name, None) ->
        Fmt.pf ppf
          "Function '%s' has already been declared but type cannot be \
           determined."
          name
    | FunDeclExists name ->
        Fmt.pf ppf
          "Function '%s' has already been declared. A definition is expected."
          name
    | FunDeclNoDefn ->
        Fmt.pf ppf "A function is declared without a corresponding definition."
    | NonRealProbFunDef ->
        Fmt.pf ppf
          "Real return type required for probability functions ending in \
           _log, _lpdf, _lpmf, _lcdf, or _lccdf."
    | ProbDensityNonRealVariate (Some ut) ->
        Fmt.pf ppf
          "Probability density functions require real variates (first \
           arguments). Instead found %a."
          pp_unsizedtype ut
    | ProbDensityNonRealVariate _ ->
        Fmt.pf ppf
          "Probability density functions require real variates (first \
           arguments)."
    | ProbMassNonIntVariate (Some ut) ->
        Fmt.pf ppf
          "Probability mass functions require integer variates (first \
           arguments). Instead found %a."
          pp_unsizedtype ut
    | ProbMassNonIntVariate _ ->
        Fmt.pf ppf
          "Probability mass functions require integer variates (first \
           arguments)."
    | DuplicateArgNames ->
        Fmt.pf ppf "All function arguments must have distinct identifiers."
    | IncompatibleReturnType ->
        Fmt.pf ppf
          "Function bodies must contain a return statement of correct type in \
           every branch."
end

type t =
  | TypeError of location_span * TypeError.t
  | IdentifierError of location_span * IdentifierError.t
  | ExpressionError of location_span * ExpressionError.t
  | StatementError of location_span * StatementError.t
