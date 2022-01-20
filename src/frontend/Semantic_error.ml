open Core_kernel
open Middle

(** Type errors that may arise during semantic check *)
module TypeError = struct
  type t =
    | MismatchedReturnTypes of UnsizedType.returntype * UnsizedType.returntype
    | MismatchedArrayTypes of UnsizedType.t * UnsizedType.t
    | InvalidRowVectorTypes of UnsizedType.t
    | InvalidMatrixTypes of UnsizedType.t
    | IntExpected of string * UnsizedType.t
    | IntOrRealExpected of string * UnsizedType.t
    | TypeExpected of string * UnsizedType.t * UnsizedType.t
    | IntIntArrayOrRangeExpected of UnsizedType.t
    | IntOrRealContainerExpected of UnsizedType.t
    | ArrayVectorRowVectorMatrixExpected of UnsizedType.t
    | IllTypedAssignment of Operator.t * UnsizedType.t * UnsizedType.t
    | IllTypedTernaryIf of UnsizedType.t * UnsizedType.t * UnsizedType.t
    | IllTypedReduceSum of
        string
        * UnsizedType.t list
        * (UnsizedType.autodifftype * UnsizedType.t) list
        * SignatureMismatch.function_mismatch
    | IllTypedReduceSumGeneric of
        string
        * UnsizedType.t list
        * (UnsizedType.autodifftype * UnsizedType.t) list
        * SignatureMismatch.function_mismatch
    | IllTypedVariadicDE of
        string
        * UnsizedType.t list
        * (UnsizedType.autodifftype * UnsizedType.t) list
        * SignatureMismatch.function_mismatch
        * UnsizedType.t
    | AmbiguousFunctionPromotion of
        string
        * UnsizedType.t list option
        * ( UnsizedType.returntype
          * (UnsizedType.autodifftype * UnsizedType.t) list )
          list
    | ReturningFnExpectedNonReturningFound of string
    | ReturningFnExpectedNonFnFound of string
    | ReturningFnExpectedUndeclaredIdentFound of string * string option
    | ReturningFnExpectedUndeclaredDistSuffixFound of string * string
    | ReturningFnExpectedWrongDistSuffixFound of string * string
    | NonReturningFnExpectedReturningFound of string
    | NonReturningFnExpectedNonFnFound of string
    | NonReturningFnExpectedUndeclaredIdentFound of string * string option
    | IllTypedFunctionApp of
        string
        * UnsizedType.t list
        * (SignatureMismatch.signature_error list * bool)
    | IllTypedBinaryOperator of Operator.t * UnsizedType.t * UnsizedType.t
    | IllTypedPrefixOperator of Operator.t * UnsizedType.t
    | IllTypedPostfixOperator of Operator.t * UnsizedType.t
    | NotIndexable of UnsizedType.t * int

  let pp ppf = function
    | MismatchedReturnTypes (rt1, rt2) ->
        Fmt.pf ppf
          "Branches of function definition need to have the same return type. \
           Instead, found return types %a and %a."
          UnsizedType.pp_returntype rt1 UnsizedType.pp_returntype rt2
    | MismatchedArrayTypes (t1, t2) ->
        Fmt.pf ppf
          "Array expression must have entries of consistent type. Expected %a \
           but found %a."
          UnsizedType.pp t1 UnsizedType.pp t2
    | InvalidRowVectorTypes ty ->
        Fmt.pf ppf
          "Row_vector expression must have all int or real entries. Found type \
           %a."
          UnsizedType.pp ty
    | InvalidMatrixTypes ty ->
        Fmt.pf ppf
          "Matrix expression must have all row_vector entries. Found type %a."
          UnsizedType.pp ty
    | IntExpected (name, ut) ->
        Fmt.pf ppf "%s must be of type int. Instead found type %a." name
          UnsizedType.pp ut
    | IntOrRealExpected (name, ut) ->
        Fmt.pf ppf "%s must be of type int or real. Instead found type %a." name
          UnsizedType.pp ut
    | TypeExpected (name, (UInt | UReal | UComplex), ut) ->
        Fmt.pf ppf "%s must be a scalar. Instead found type %a." name
          UnsizedType.pp ut
    | TypeExpected (name, et, ut) ->
        Fmt.pf ppf "%s must be a scalar or of type %a. Instead found type %a."
          name UnsizedType.pp et UnsizedType.pp ut
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
    | IllTypedAssignment (Operator.Equals, lt, rt) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to assignment operator %s: lhs has \
           type %a and rhs has type %a"
          "=" UnsizedType.pp lt UnsizedType.pp rt
    | IllTypedAssignment (op, lt, rt) ->
        Fmt.pf ppf
          "@[<h>Ill-typed arguments supplied to assignment operator %s: lhs \
           has type %a and rhs has type %a. Available signatures:@]%s"
          (Fmt.str "%a=" Operator.pp op)
          UnsizedType.pp lt UnsizedType.pp rt
          ( Stan_math_signatures.pretty_print_math_lib_assignmentoperator_sigs op
          |> Option.value ~default:"no matching signatures" )
    | IllTypedTernaryIf (UInt, ut, _) when UnsizedType.is_fun_type ut ->
        Fmt.pf ppf "Ternary expression cannot have a function type: %a"
          UnsizedType.pp ut
    | IllTypedTernaryIf (UInt, ut2, ut3) ->
        Fmt.pf ppf
          "Type mismatch in ternary expression, expression when true is: %a; \
           expression when false is: %a"
          UnsizedType.pp ut2 UnsizedType.pp ut3
    | IllTypedTernaryIf (ut1, _, _) ->
        Fmt.pf ppf
          "Condition in ternary expression must be primitive int; found type=%a"
          UnsizedType.pp ut1
    | IllTypedReduceSum (name, arg_tys, expected_args, error) ->
        SignatureMismatch.pp_signature_mismatch ppf
          (name, arg_tys, ([((ReturnType UReal, expected_args), error)], false))
    | IllTypedReduceSumGeneric (name, arg_tys, expected_args, error) ->
        SignatureMismatch.pp_signature_mismatch ppf
          (name, arg_tys, ([((ReturnType UReal, expected_args), error)], false))
    | IllTypedVariadicDE (name, arg_tys, args, error, return_type) ->
        SignatureMismatch.pp_signature_mismatch ppf
          ( name
          , arg_tys
          , ([((UnsizedType.ReturnType return_type, args), error)], false) )
    | AmbiguousFunctionPromotion (name, arg_tys, signatures) ->
        let pp_sig ppf (rt, args) =
          Fmt.pf ppf "@[<hov>(@[<hov>%a@]) => %a@]"
            Fmt.(list ~sep:comma UnsizedType.pp_fun_arg)
            args UnsizedType.pp_returntype rt in
        Fmt.pf ppf
          "No unique minimum promotion found for function '%s'.@ Overloaded \
           functions must not have multiple equally valid promotion paths.@ %a \
           function has several:@ @[<v>%a@]@ Consider defining a new signature \
           for the exact types needed or@ re-thinking existing definitions."
          name
          (Fmt.option
             ~none:(fun ppf () -> Fmt.pf ppf "This")
             (fun ppf tys ->
               Fmt.pf ppf "For args @[(%a)@], this"
                 (Fmt.list ~sep:Fmt.comma UnsizedType.pp)
                 tys ) )
          arg_tys
          (Fmt.list ~sep:Fmt.cut pp_sig)
          signatures
    | NotIndexable (ut, nidcs) ->
        Fmt.pf ppf
          "Too many indexes, expression dimensions=%d, indexes found=%d."
          (UnsizedType.count_dims ut)
          nidcs
    | ReturningFnExpectedNonReturningFound fn_name ->
        Fmt.pf ppf
          "A returning function was expected but a non-returning function '%s' \
           was supplied."
          fn_name
    | NonReturningFnExpectedReturningFound fn_name ->
        Fmt.pf ppf
          "A non-returning function was expected but a returning function '%s' \
           was supplied."
          fn_name
    | ReturningFnExpectedNonFnFound fn_name ->
        Fmt.pf ppf
          "A returning function was expected but a non-function value '%s' was \
           supplied."
          fn_name
    | NonReturningFnExpectedNonFnFound fn_name ->
        Fmt.pf ppf
          "A non-returning function was expected but a non-function value '%s' \
           was supplied."
          fn_name
    | ReturningFnExpectedUndeclaredIdentFound (fn_name, sug) -> (
      match sug with
      | None ->
          Fmt.pf ppf
            "A returning function was expected but an undeclared identifier \
             '%s' was supplied."
            fn_name
      | Some s ->
          Fmt.pf ppf
            "A returning function was expected but an undeclared identifier \
             '%s' was supplied.@ A similar known identifier is '%s'"
            fn_name s )
    | NonReturningFnExpectedUndeclaredIdentFound (fn_name, sug) -> (
      match sug with
      | None ->
          Fmt.pf ppf
            "A non-returning function was expected but an undeclared \
             identifier '%s' was supplied."
            fn_name
      | Some s ->
          Fmt.pf ppf
            "A non-returning function was expected but an undeclared \
             identifier '%s' was supplied.@ A nearby known identifier is '%s'"
            fn_name s )
    | ReturningFnExpectedUndeclaredDistSuffixFound (prefix, suffix) ->
        Fmt.pf ppf "Function '%s_%s' is not implemented for distribution '%s'."
          prefix suffix prefix
    | ReturningFnExpectedWrongDistSuffixFound (prefix, suffix) ->
        let newsuffix =
          match suffix with
          | "lpdf" -> "lpmf"
          | "lupdf" -> "lupmf"
          | "lpmf" -> "lpdf"
          | "lupmf" -> "lupdf"
          | _ ->
              Common.FatalError.fatal_error_msg
                [%message "Bad suffix:" (suffix : string)] in
        Fmt.pf ppf
          "Function '%s_%s' is not implemented for distribution '%s', use \
           '%s_%s' instead."
          prefix suffix prefix prefix newsuffix
    | IllTypedFunctionApp (name, arg_tys, errors) ->
        SignatureMismatch.pp_signature_mismatch ppf (name, arg_tys, errors)
    | IllTypedBinaryOperator (op, lt, rt) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to infix operator %a. Available \
           signatures: %s@[<h>Instead supplied arguments of incompatible type: \
           %a, %a.@]"
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
           Instead supplied argument of incompatible type: %a." Operator.pp op
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
    | NotInScope of string * string option
    | UnnormalizedSuffix of string

  let pp ppf = function
    | IsStanMathName name ->
        Fmt.pf ppf
          "Identifier '%s' clashes with a non-overloadable Stan Math library \
           function."
          name
    | InUse name -> Fmt.pf ppf "Identifier '%s' is already in use." name
    | IsModelName name ->
        Fmt.pf ppf "Identifier '%s' clashes with model name." name
    | IsKeyword name ->
        Fmt.pf ppf "Identifier '%s' clashes with reserved keyword." name
    | NotInScope (name, sug) -> (
      match sug with
      | None -> Fmt.pf ppf "Identifier '%s' not in scope." name
      | Some s ->
          Fmt.pf ppf "Identifier '%s' not in scope. Did you mean '%s'?" name s )
    | UnnormalizedSuffix name ->
        Fmt.pf ppf
          "Identifier '%s' has a _lupdf/_lupmf suffix, which is only allowed \
           for functions."
          name
end

module ExpressionError = struct
  type t =
    | InvalidMapRectFn of string
    | InvalidSizeDeclRng
    | InvalidRngFunction
    | InvalidUnnormalizedFunction
    | InvalidUnnormalizedUDF of string
    | ConditionalNotationNotAllowed
    | ConditioningRequired
    | NotPrintable
    | EmptyArray
    | IntTooLarge

  let pp ppf = function
    | InvalidMapRectFn fn_name ->
        Fmt.pf ppf
          "Mapped function cannot be an _rng or _lp function, found function \
           name: %s"
          fn_name
    | InvalidSizeDeclRng ->
        Fmt.pf ppf
          "Random number generators are not allowed in top level size \
           declarations."
    | InvalidRngFunction ->
        Fmt.pf ppf
          "Random number generators are only allowed in transformed data \
           block, generated quantities block or user-defined functions with \
           names ending in _rng."
    | InvalidUnnormalizedFunction ->
        Fmt.pf ppf
          "Functions with names ending in _lupdf and _lupmf can only be used \
           in the model block or user-defined functions with names ending in \
           _lpdf or _lpmf."
    | InvalidUnnormalizedUDF fname ->
        Fmt.pf ppf
          "%s is an invalid user-defined function name. User-defined \
           probability mass and density functions must be defined as \
           normalized (function names should end with _lpdf/_lpmf not \
           _lupdf/_lupmf)."
          fname
    | ConditionalNotationNotAllowed ->
        Fmt.pf ppf
          "Only functions with names ending in _lpdf, _lupdf, _lpmf, _lupmf, \
           _cdf, _lcdf, _lccdf can make use of conditional notation."
    | ConditioningRequired ->
        Fmt.pf ppf
          "Probability functions with suffixes _lpdf, _lupdf, _lpmf, _lupmf, \
           _cdf, _lcdf and _lccdf, require a vertical bar (|) between the \
           first two arguments."
    | NotPrintable -> Fmt.pf ppf "Functions cannot be printed."
    | EmptyArray ->
        Fmt.pf ppf "Array expressions must contain at least one element."
    | IntTooLarge ->
        Fmt.pf ppf "Integer literal cannot be larger than 2_147_483_647."
end

module StatementError = struct
  type t =
    | CannotAssignToReadOnly of string
    | CannotAssignToGlobal of string
    | LValueMultiIndexing
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
    | ComplexTransform
    | TransformedParamsInt
    | FuncOverloadRtOnly of
        string * UnsizedType.returntype * UnsizedType.returntype
    | FuncDeclRedefined of string * UnsizedType.t * bool
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
        Fmt.pf ppf "Cannot assign to function argument or loop identifier '%s'."
          name
    | CannotAssignToGlobal name ->
        Fmt.pf ppf
          "Cannot assign to global variable '%s' declared in previous blocks."
          name
    | LValueMultiIndexing ->
        Fmt.pf ppf
          "Left hand side of an assignment cannot have nested multi-indexing."
    | TargetPlusEqualsOutsideModelOrLogProb ->
        Fmt.pf ppf
          "Target can only be accessed in the model block or in definitions of \
           functions with the suffix _lp."
    | InvalidSamplingPDForPMF ->
        Fmt.pf ppf
          "~ statement should refer to a distribution without its \
           \"_lpdf/_lupdf\" or \"_lpmf/_lupmf\" suffix.\n\
           For example, \"target += normal_lpdf(y, 0, 1)\" should become \"y ~ \
           normal(0, 1).\""
    | InvalidSamplingCDForCCDF name ->
        Fmt.pf ppf
          "CDF and CCDF functions may not be used with sampling notation. Use \
           target += %s_log(...) instead."
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
    | ComplexTransform ->
        Fmt.pf ppf "Complex types do not support transformations."
    | TransformedParamsInt ->
        Fmt.pf ppf "(Transformed) Parameters cannot be integers."
    | FuncOverloadRtOnly (name, _, rt') ->
        Fmt.pf ppf
          "Function '%s' cannot be overloaded by return type only. Previously \
           used return type %a"
          name UnsizedType.pp_returntype rt'
    | FuncDeclRedefined (name, ut, stan_math) ->
        Fmt.pf ppf "Function '%s' %s signature %a" name
          ( if stan_math then "is already declared in the Stan Math library with"
          else "has already been declared to for" )
          UnsizedType.pp ut
    | FunDeclExists name ->
        Fmt.pf ppf
          "Function '%s' has already been declared. A definition is expected."
          name
    | FunDeclNoDefn ->
        Fmt.pf ppf "Function is declared without specifying a definition."
    | FunDeclNeedsBlock ->
        Fmt.pf ppf "Function definitions must be wrapped in curly braces."
    | NonRealProbFunDef ->
        Fmt.pf ppf
          "Real return type required for probability functions ending in _log, \
           _lpdf, _lupdf, _lpmf, _lupmf, _cdf, _lcdf, or _lccdf."
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

let mismatched_array_types loc t1 t2 =
  TypeError (loc, TypeError.MismatchedArrayTypes (t1, t2))

let invalid_row_vector_types loc ty =
  TypeError (loc, TypeError.InvalidRowVectorTypes ty)

let invalid_matrix_types loc ty =
  TypeError (loc, TypeError.InvalidMatrixTypes ty)

let int_expected loc name ut = TypeError (loc, TypeError.IntExpected (name, ut))

let int_or_real_expected loc name ut =
  TypeError (loc, TypeError.IntOrRealExpected (name, ut))

let scalar_or_type_expected loc name et ut =
  TypeError (loc, TypeError.TypeExpected (name, et, ut))

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

let illtyped_reduce_sum loc name arg_tys args error =
  TypeError (loc, TypeError.IllTypedReduceSum (name, arg_tys, args, error))

let illtyped_reduce_sum_generic loc name arg_tys expected_args error =
  TypeError
    ( loc
    , TypeError.IllTypedReduceSumGeneric (name, arg_tys, expected_args, error)
    )

let illtyped_variadic_ode loc name arg_tys args error =
  TypeError
    ( loc
    , TypeError.IllTypedVariadicDE
        ( name
        , arg_tys
        , args
        , error
        , Stan_math_signatures.variadic_ode_fun_return_type ) )

let illtyped_variadic_dae loc name arg_tys args error =
  TypeError
    ( loc
    , TypeError.IllTypedVariadicDE
        ( name
        , arg_tys
        , args
        , error
        , Stan_math_signatures.variadic_dae_fun_return_type ) )

let ambiguous_function_promotion loc name arg_tys signatures =
  TypeError
    (loc, TypeError.AmbiguousFunctionPromotion (name, arg_tys, signatures))

let returning_fn_expected_nonfn_found loc name =
  TypeError (loc, TypeError.ReturningFnExpectedNonFnFound name)

let returning_fn_expected_undeclaredident_found loc name sug =
  TypeError (loc, TypeError.ReturningFnExpectedUndeclaredIdentFound (name, sug))

let returning_fn_expected_undeclared_dist_suffix_found loc (prefix, suffix) =
  TypeError
    ( loc
    , TypeError.ReturningFnExpectedUndeclaredDistSuffixFound (prefix, suffix) )

let returning_fn_expected_wrong_dist_suffix_found loc (prefix, suffix) =
  TypeError
    (loc, TypeError.ReturningFnExpectedWrongDistSuffixFound (prefix, suffix))

let nonreturning_fn_expected_returning_found loc name =
  TypeError (loc, TypeError.NonReturningFnExpectedReturningFound name)

let nonreturning_fn_expected_nonfn_found loc name =
  TypeError (loc, TypeError.NonReturningFnExpectedNonFnFound name)

let nonreturning_fn_expected_undeclaredident_found loc name sug =
  TypeError
    (loc, TypeError.NonReturningFnExpectedUndeclaredIdentFound (name, sug))

let illtyped_fn_app loc name errors arg_tys =
  TypeError (loc, TypeError.IllTypedFunctionApp (name, arg_tys, errors))

let illtyped_binary_op loc op lt rt =
  TypeError (loc, TypeError.IllTypedBinaryOperator (op, lt, rt))

let illtyped_prefix_op loc op ut =
  TypeError (loc, TypeError.IllTypedPrefixOperator (op, ut))

let illtyped_postfix_op loc op ut =
  TypeError (loc, TypeError.IllTypedPostfixOperator (op, ut))

let not_indexable loc ut nidcs =
  TypeError (loc, TypeError.NotIndexable (ut, nidcs))

let ident_is_keyword loc name =
  IdentifierError (loc, IdentifierError.IsKeyword name)

let ident_is_model_name loc name =
  IdentifierError (loc, IdentifierError.IsModelName name)

let ident_is_stanmath_name loc name =
  IdentifierError (loc, IdentifierError.IsStanMathName name)

let ident_in_use loc name = IdentifierError (loc, IdentifierError.InUse name)

let ident_not_in_scope loc name sug =
  IdentifierError (loc, IdentifierError.NotInScope (name, sug))

let ident_has_unnormalized_suffix loc name =
  IdentifierError (loc, IdentifierError.UnnormalizedSuffix name)

let invalid_map_rect_fn loc name =
  ExpressionError (loc, ExpressionError.InvalidMapRectFn name)

let invalid_decl_rng_fn loc =
  ExpressionError (loc, ExpressionError.InvalidSizeDeclRng)

let invalid_rng_fn loc =
  ExpressionError (loc, ExpressionError.InvalidRngFunction)

let invalid_unnormalized_fn loc =
  ExpressionError (loc, ExpressionError.InvalidUnnormalizedFunction)

let udf_is_unnormalized_fn loc name =
  ExpressionError (loc, ExpressionError.InvalidUnnormalizedUDF name)

let conditional_notation_not_allowed loc =
  ExpressionError (loc, ExpressionError.ConditionalNotationNotAllowed)

let conditioning_required loc =
  ExpressionError (loc, ExpressionError.ConditioningRequired)

let not_printable loc = ExpressionError (loc, ExpressionError.NotPrintable)
let empty_array loc = ExpressionError (loc, ExpressionError.EmptyArray)
let bad_int_literal loc = ExpressionError (loc, ExpressionError.IntTooLarge)

let cannot_assign_to_read_only loc name =
  StatementError (loc, StatementError.CannotAssignToReadOnly name)

let cannot_assign_to_global loc name =
  StatementError (loc, StatementError.CannotAssignToGlobal name)

let cannot_assign_to_multiindex loc =
  StatementError (loc, StatementError.LValueMultiIndexing)

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
let complex_transform loc = StatementError (loc, StatementError.ComplexTransform)

let transformed_params_int loc =
  StatementError (loc, StatementError.TransformedParamsInt)

let fn_overload_rt_only loc name rt1 rt2 =
  StatementError (loc, StatementError.FuncOverloadRtOnly (name, rt1, rt2))

let fn_decl_redefined loc name ~stan_math ut =
  StatementError (loc, StatementError.FuncDeclRedefined (name, ut, stan_math))

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
