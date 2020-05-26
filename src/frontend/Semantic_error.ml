open Core_kernel
open Middle

(** Type errors that may arise during semantic check *)
module TypeError = struct
  type t =
    | MismatchedReturnTypes of UnsizedType.returntype * UnsizedType.returntype
    | MismatchedArrayTypes
    | InvalidRowVectorTypes
    | IntExpected of string * UnsizedType.t
    | IntOrRealExpected of string * UnsizedType.t
    | IntIntArrayOrRangeExpected of UnsizedType.t
    | IntOrRealContainerExpected of UnsizedType.t
    | ArrayVectorRowVectorMatrixExpected of UnsizedType.t
    | IllTypedAssignment of
        Ast.assignmentoperator * UnsizedType.t * UnsizedType.t
    | IllTypedTernaryIf of UnsizedType.t * UnsizedType.t * UnsizedType.t
    | IllTypedReduceSum of
        string
        * UnsizedType.t list
        * (UnsizedType.autodifftype * UnsizedType.t) list
    | IllTypedReduceSumGeneric of string * UnsizedType.t list
    | ReturningFnExpectedNonReturningFound of string
    | ReturningFnExpectedNonFnFound of string
    | ReturningFnExpectedUndeclaredIdentFound of string
    | NonReturningFnExpectedReturningFound of string
    | NonReturningFnExpectedNonFnFound of string
    | NonReturningFnExpectedUndeclaredIdentFound of string
    | IllTypedStanLibFunctionApp of string * UnsizedType.t list
    | IllTypedUserDefinedFunctionApp of
        string
        * (UnsizedType.autodifftype * UnsizedType.t) list
        * UnsizedType.returntype
        * UnsizedType.t list
    | IllTypedBinaryOperator of Operator.t * UnsizedType.t * UnsizedType.t
    | IllTypedPrefixOperator of Operator.t * UnsizedType.t
    | IllTypedPostfixOperator of Operator.t * UnsizedType.t
    | NotIndexable of UnsizedType.t

  let pp ppf = function
    | MismatchedReturnTypes (rt1, rt2) ->
        Fmt.pf ppf
          "Branches of function definition need to have the same return type. \
           Instead, found return types %a and %a."
          UnsizedType.pp_returntype rt1 UnsizedType.pp_returntype rt2
    | MismatchedArrayTypes ->
        Fmt.pf ppf "Array expression must have entries of consistent type."
    | InvalidRowVectorTypes ->
        Fmt.pf ppf
          "Row_vector expression must have all int and real entries or all \
           row_vector entries."
    | IntExpected (name, ut) ->
        Fmt.pf ppf "%s must be of type int. Instead found type %a." name
          UnsizedType.pp ut
    | IntOrRealExpected (name, ut) ->
        Fmt.pf ppf "%s must be of type int or real. Instead found type %a."
          name UnsizedType.pp ut
    | IntOrRealContainerExpected ut ->
        Fmt.pf ppf
          "A (container of) real or int was expected. Instead found type %a."
          UnsizedType.pp ut
    | IntIntArrayOrRangeExpected ut ->
        Fmt.pf ppf
          "Index must be of type int or int[] or must be a range. Instead \
           found type %a."
          UnsizedType.pp ut
    | ArrayVectorRowVectorMatrixExpected ut ->
        Fmt.pf ppf
          "Foreach-loop must be over array, vector, row_vector or matrix. \
           Instead found expression of type %a."
          UnsizedType.pp ut
    | IllTypedAssignment ((OperatorAssign op as assignop), lt, rt) ->
        Fmt.pf ppf
          "@[<h>Ill-typed arguments supplied to assignment operator %s: lhs \
           has type %a and rhs has type %a. Available signatures:@]%s"
          (Pretty_printing.pretty_print_assignmentoperator assignop)
          UnsizedType.pp lt UnsizedType.pp rt
          ( Stan_math_signatures.pretty_print_math_lib_assignmentoperator_sigs
              op
          |> Option.value ~default:"no matching signatures" )
    | IllTypedAssignment (assignop, lt, rt) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to assignment operator %s: lhs has \
           type %a and rhs has type %a"
          (Pretty_printing.pretty_print_assignmentoperator assignop)
          UnsizedType.pp lt UnsizedType.pp rt
    | IllTypedTernaryIf (UInt, ut2, ut3) ->
        Fmt.pf ppf
          "Type mismatch in ternary expression, expression when true is: %a; \
           expression when false is: %a"
          UnsizedType.pp ut2 UnsizedType.pp ut3
    | IllTypedTernaryIf (ut1, _, _) ->
        Fmt.pf ppf
          "Condition in ternary expression must be primitive int; found type=%a"
          UnsizedType.pp ut1
    | IllTypedReduceSum (name, arg_tys, args) ->
        let arg_types = List.map ~f:(fun (_, t) -> t) args in
        let first, rest = List.split_n arg_types 1 in
        let generate_reduce_sum_sig =
          List.concat
            [ [ UnsizedType.UFun
                  ( List.hd_exn args :: (AutoDiffable, UInt)
                    :: (AutoDiffable, UInt) :: List.tl_exn args
                  , ReturnType UReal ) ]
            ; first; [UInt]; rest ]
        in
        Fmt.pf ppf
          "Ill-typed arguments supplied to function '%s'. Expected \
           arguments:@[<h>%a@]\n\
           @[<h>Instead supplied arguments of incompatible type: %a@]"
          name
          Fmt.(list UnsizedType.pp ~sep:comma)
          generate_reduce_sum_sig
          Fmt.(list UnsizedType.pp ~sep:comma)
          arg_tys
    | IllTypedReduceSumGeneric (name, arg_tys) ->
        let rec n_commas n = if n = 0 then "" else "," ^ n_commas (n - 1) in
        let type_string (a, b, c, d) i =
          Fmt.strf "(T[%s], %a, %a, ...) => %a, T[%s], %a, ...\n"
            (n_commas (i - 1))
            Pretty_printing.pp_unsizedtype a Pretty_printing.pp_unsizedtype b
            Pretty_printing.pp_unsizedtype c
            (n_commas (i - 1))
            Pretty_printing.pp_unsizedtype d
        in
        let lines =
          List.map
            ~f:(fun i -> type_string (UInt, UInt, UReal, UInt) i)
            Stan_math_signatures.reduce_sum_allowed_dimensionalities
        in
        Fmt.pf ppf
          "Ill-typed arguments supplied to function '%s'. Available arguments:\n\
           %sWhere T is any one of int, real, vector, row_vector or \
           matrix.@[<h>Instead supplied arguments of incompatible type: %a@]"
          name
          (String.concat ~sep:"" lines)
          Fmt.(list UnsizedType.pp ~sep:comma)
          arg_tys
    | NotIndexable ut ->
        Fmt.pf ppf
          "Only expressions of array, matrix, row_vector and vector type may \
           be indexed. Instead, found type %a."
          UnsizedType.pp ut
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
    | IllTypedStanLibFunctionApp (name, arg_tys) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to function '%s'. Available \
           signatures: %s@[<h>Instead supplied arguments of incompatible \
           type: %a.@]"
          name
          (Stan_math_signatures.pretty_print_math_sigs name)
          Fmt.(list UnsizedType.pp ~sep:comma)
          arg_tys
    | IllTypedUserDefinedFunctionApp (name, listed_tys, return_ty, arg_tys) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to function '%s'. Available \
           signatures:%a\n\
           @[<h>Instead supplied arguments of incompatible type: %a.@]"
          name UnsizedType.pp
          (UFun (listed_tys, return_ty))
          Fmt.(list UnsizedType.pp ~sep:comma)
          arg_tys
    | IllTypedBinaryOperator (op, lt, rt) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to infix operator %a. Available \
           signatures: %s@[<h>Instead supplied arguments of incompatible \
           type: %a, %a.@]"
          Operator.pp op
          ( Stan_math_signatures.pretty_print_math_lib_operator_sigs op
          |> String.concat ~sep:"\n" )
          UnsizedType.pp lt UnsizedType.pp rt
    | IllTypedPrefixOperator (op, ut) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to prefix operator %a. Available \
           signatures: %s@[<h>Instead supplied argument of incompatible type: \
           %a.@]"
          Operator.pp op
          ( Stan_math_signatures.pretty_print_math_lib_operator_sigs op
          |> String.concat ~sep:"\n" )
          UnsizedType.pp ut
    | IllTypedPostfixOperator (op, ut) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to postfix operator %a. Available \
           signatures: %s\n\
           Instead supplied argument of incompatible type: %a."
          Operator.pp op
          ( Stan_math_signatures.pretty_print_math_lib_operator_sigs op
          |> String.concat ~sep:"\n" )
          UnsizedType.pp ut
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
    | InvalidMapRectFn of string
    | InvalidRngFunction
    | ConditionalNotationNotAllowed
    | ConditioningRequired
    | NotPrintable
    | EmptyArray

  let pp ppf = function
    | InvalidMapRectFn fn_name ->
        Fmt.pf ppf
          "Mapped function cannot be an _rng or _lp function, found function \
           name: %s"
          fn_name
    | InvalidRngFunction ->
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
    | EmptyArray ->
        Fmt.pf ppf "Array expressions must contain at least one element."
end

module StatementError = struct
  type t =
    | CannotAssignToReadOnly of string
    | CannotAssignToGlobal of string
    | InvalidSamplingPDForPMF
    | InvalidSamplingCDForCCDF of string
    | InvalidSamplingNoSuchDistribution of string
    | TargetPlusEqualsOutsideModelOrLogProb
    | InvalidTruncationCDForCCDF
    | MultivariateTruncation
    | BreakOutsideLoop
    | ContinueOutsideLoop
    | ExpressionReturnOutsideReturningFn
    | VoidReturnOutsideNonReturningFn
    | NonDataVariableSizeDecl
    | NonIntBounds
    | TransformedParamsInt
    | MismatchFunDefDecl of string * UnsizedType.t option
    | FunDeclExists of string
    | FunDeclNoDefn
    | FunDeclNeedsBlock
    | NonRealProbFunDef
    | ProbDensityNonRealVariate of UnsizedType.t option
    | ProbMassNonIntVariate of UnsizedType.t option
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
    | InvalidSamplingPDForPMF ->
        Fmt.pf ppf
          {|
~ statement should refer to a distribution without its "_lpdf" or "_lpmf" suffix.
For example, "target += normal_lpdf(y, 0, 1)" should become "y ~ normal(0, 1)."
|}
    | InvalidSamplingCDForCCDF name ->
        Fmt.pf ppf
          "CDF and CCDF functions may not be used with sampling notation. Use \
           increment_log_prob(%s_log(...)) instead."
          name
    | InvalidSamplingNoSuchDistribution name ->
        Fmt.pf ppf
          "Ill-typed arguments to '~' statement. No distribution '%s' was \
           found with the correct signature."
          name
    | InvalidTruncationCDForCCDF ->
        Fmt.pf ppf
          "Truncation is only defined if distribution has _lcdf and _lccdf \
           functions implemented with appropriate signature."
    | MultivariateTruncation ->
        Fmt.pf ppf "Outcomes in truncated distributions must be univariate."
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
    | NonDataVariableSizeDecl ->
        Fmt.pf ppf
          "Non-data variables are not allowed in top level size declarations."
    | NonIntBounds ->
        Fmt.pf ppf
          "Bounds of integer variable must be of type int. Found type real."
    | TransformedParamsInt ->
        Fmt.pf ppf "(Transformed) Parameters cannot be integers."
    | MismatchFunDefDecl (name, Some ut) ->
        Fmt.pf ppf "Function '%s' has already been declared to have type %a"
          name UnsizedType.pp ut
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
        Fmt.pf ppf "Some function is declared without specifying a definition."
    | FunDeclNeedsBlock ->
        Fmt.pf ppf "Function definitions must be wrapped in curly braces."
    | NonRealProbFunDef ->
        Fmt.pf ppf
          "Real return type required for probability functions ending in \
           _log, _lpdf, _lpmf, _lcdf, or _lccdf."
    | ProbDensityNonRealVariate (Some ut) ->
        Fmt.pf ppf
          "Probability density functions require real variates (first \
           argument). Instead found type %a."
          UnsizedType.pp ut
    | ProbDensityNonRealVariate _ ->
        Fmt.pf ppf
          "Probability density functions require real variates (first \
           argument)."
    | ProbMassNonIntVariate (Some ut) ->
        Fmt.pf ppf
          "Probability mass functions require integer variates (first \
           argument). Instead found type %a."
          UnsizedType.pp ut
    | ProbMassNonIntVariate _ ->
        Fmt.pf ppf
          "Probability mass functions require integer variates (first \
           argument)."
    | DuplicateArgNames ->
        Fmt.pf ppf "All function arguments must have distinct identifiers."
    | IncompatibleReturnType ->
        Fmt.pf ppf
          "Function bodies must contain a return statement of correct type in \
           every branch."
end

type t =
  | TypeError of Location_span.t * TypeError.t
  | IdentifierError of Location_span.t * IdentifierError.t
  | ExpressionError of Location_span.t * ExpressionError.t
  | StatementError of Location_span.t * StatementError.t

let pp ppf = function
  | TypeError (_, err) -> TypeError.pp ppf err
  | IdentifierError (_, err) -> IdentifierError.pp ppf err
  | ExpressionError (_, err) -> ExpressionError.pp ppf err
  | StatementError (_, err) -> StatementError.pp ppf err

let location = function
  | TypeError (loc, _) -> loc
  | IdentifierError (loc, _) -> loc
  | ExpressionError (loc, _) -> loc
  | StatementError (loc, _) -> loc

(* -- Constructors ---------------------------------------------------------- *)

let mismatched_return_types loc rt1 rt2 =
  TypeError (loc, TypeError.MismatchedReturnTypes (rt1, rt2))

let mismatched_array_types loc = TypeError (loc, TypeError.MismatchedArrayTypes)

let invalid_row_vector_types loc =
  TypeError (loc, TypeError.InvalidRowVectorTypes)

let int_expected loc name ut = TypeError (loc, TypeError.IntExpected (name, ut))

let int_or_real_expected loc name ut =
  TypeError (loc, TypeError.IntOrRealExpected (name, ut))

let int_intarray_or_range_expected loc ut =
  TypeError (loc, TypeError.IntIntArrayOrRangeExpected ut)

let int_or_real_container_expected loc ut =
  TypeError (loc, TypeError.IntOrRealContainerExpected ut)

let array_vector_rowvector_matrix_expected loc ut =
  TypeError (loc, TypeError.ArrayVectorRowVectorMatrixExpected ut)

let illtyped_assignment loc assignop lt rt =
  TypeError (loc, TypeError.IllTypedAssignment (assignop, lt, rt))

let illtyped_ternary_if loc predt lt rt =
  TypeError (loc, TypeError.IllTypedTernaryIf (predt, lt, rt))

let returning_fn_expected_nonreturning_found loc name =
  TypeError (loc, TypeError.ReturningFnExpectedNonReturningFound name)

let illtyped_reduce_sum loc name arg_tys args =
  TypeError (loc, TypeError.IllTypedReduceSum (name, arg_tys, args))

let illtyped_reduce_sum_generic loc name arg_tys =
  TypeError (loc, TypeError.IllTypedReduceSumGeneric (name, arg_tys))

let returning_fn_expected_nonfn_found loc name =
  TypeError (loc, TypeError.ReturningFnExpectedNonFnFound name)

let returning_fn_expected_undeclaredident_found loc name =
  TypeError (loc, TypeError.ReturningFnExpectedUndeclaredIdentFound name)

let nonreturning_fn_expected_returning_found loc name =
  TypeError (loc, TypeError.NonReturningFnExpectedReturningFound name)

let nonreturning_fn_expected_nonfn_found loc name =
  TypeError (loc, TypeError.NonReturningFnExpectedNonFnFound name)

let nonreturning_fn_expected_undeclaredident_found loc name =
  TypeError (loc, TypeError.NonReturningFnExpectedUndeclaredIdentFound name)

let illtyped_stanlib_fn_app loc name arg_tys =
  TypeError (loc, TypeError.IllTypedStanLibFunctionApp (name, arg_tys))

let illtyped_userdefined_fn_app loc name decl_arg_tys decl_return_ty arg_tys =
  TypeError
    ( loc
    , TypeError.IllTypedUserDefinedFunctionApp
        (name, decl_arg_tys, decl_return_ty, arg_tys) )

let illtyped_binary_op loc op lt rt =
  TypeError (loc, TypeError.IllTypedBinaryOperator (op, lt, rt))

let illtyped_prefix_op loc op ut =
  TypeError (loc, TypeError.IllTypedPrefixOperator (op, ut))

let illtyped_postfix_op loc op ut =
  TypeError (loc, TypeError.IllTypedPostfixOperator (op, ut))

let not_indexable loc ut = TypeError (loc, TypeError.NotIndexable ut)

let ident_is_keyword loc name =
  IdentifierError (loc, IdentifierError.IsKeyword name)

let ident_is_model_name loc name =
  IdentifierError (loc, IdentifierError.IsModelName name)

let ident_is_stanmath_name loc name =
  IdentifierError (loc, IdentifierError.IsStanMathName name)

let ident_in_use loc name = IdentifierError (loc, IdentifierError.InUse name)

let ident_not_in_scope loc name =
  IdentifierError (loc, IdentifierError.NotInScope name)

let invalid_map_rect_fn loc name =
  ExpressionError (loc, ExpressionError.InvalidMapRectFn name)

let invalid_rng_fn loc =
  ExpressionError (loc, ExpressionError.InvalidRngFunction)

let conditional_notation_not_allowed loc =
  ExpressionError (loc, ExpressionError.ConditionalNotationNotAllowed)

let conditioning_required loc =
  ExpressionError (loc, ExpressionError.ConditioningRequired)

let not_printable loc = ExpressionError (loc, ExpressionError.NotPrintable)
let empty_array loc = ExpressionError (loc, ExpressionError.EmptyArray)

let cannot_assign_to_read_only loc name =
  StatementError (loc, StatementError.CannotAssignToReadOnly name)

let cannot_assign_to_global loc name =
  StatementError (loc, StatementError.CannotAssignToGlobal name)

let invalid_sampling_pdf_or_pmf loc =
  StatementError (loc, StatementError.InvalidSamplingPDForPMF)

let invalid_sampling_cdf_or_ccdf loc name =
  StatementError (loc, StatementError.InvalidSamplingCDForCCDF name)

let invalid_sampling_no_such_dist loc name =
  StatementError (loc, StatementError.InvalidSamplingNoSuchDistribution name)

let target_plusequals_outisde_model_or_logprob loc =
  StatementError (loc, StatementError.TargetPlusEqualsOutsideModelOrLogProb)

let invalid_truncation_cdf_or_ccdf loc =
  StatementError (loc, StatementError.InvalidTruncationCDForCCDF)

let multivariate_truncation loc =
  StatementError (loc, StatementError.MultivariateTruncation)

let break_outside_loop loc =
  StatementError (loc, StatementError.BreakOutsideLoop)

let continue_outside_loop loc =
  StatementError (loc, StatementError.ContinueOutsideLoop)

let expression_return_outside_returning_fn loc =
  StatementError (loc, StatementError.ExpressionReturnOutsideReturningFn)

let void_ouside_nonreturning_fn loc =
  StatementError (loc, StatementError.VoidReturnOutsideNonReturningFn)

let non_data_variable_size_decl loc =
  StatementError (loc, StatementError.NonDataVariableSizeDecl)

let non_int_bounds loc = StatementError (loc, StatementError.NonIntBounds)

let transformed_params_int loc =
  StatementError (loc, StatementError.TransformedParamsInt)

let mismatched_fn_def_decl loc name ut_opt =
  StatementError (loc, StatementError.MismatchFunDefDecl (name, ut_opt))

let fn_decl_exists loc name =
  StatementError (loc, StatementError.FunDeclExists name)

let fn_decl_without_def loc = StatementError (loc, StatementError.FunDeclNoDefn)

let fn_decl_needs_block loc =
  StatementError (loc, StatementError.FunDeclNeedsBlock)

let non_real_prob_fn_def loc =
  StatementError (loc, StatementError.NonRealProbFunDef)

let prob_density_non_real_variate loc ut_opt =
  StatementError (loc, StatementError.ProbDensityNonRealVariate ut_opt)

let prob_mass_non_int_variate loc ut_opt =
  StatementError (loc, StatementError.ProbMassNonIntVariate ut_opt)

let duplicate_arg_names loc =
  StatementError (loc, StatementError.DuplicateArgNames)

let incompatible_return_types loc =
  StatementError (loc, StatementError.IncompatibleReturnType)
