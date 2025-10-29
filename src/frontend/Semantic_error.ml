open Core
open Middle

(* The following categories (type, identifier, expression, statement) are fairly
   loose, the main idea is to keep similar errors close to each other and still
   be semi-organized *)

let ellipsis ppf = Fmt.styled `Faint Fmt.string ppf "..."
let expected_style = SignatureMismatch.expected_style
let actual_style = SignatureMismatch.actual_style
let arguments = SignatureMismatch.arguments
let quoted = SignatureMismatch.quoted

let found_type ppf =
  Fmt.pf ppf "@ Instead found type %a." (actual_style UnsizedType.pp)

module TypeError = struct
  type t =
    | IncorrectReturnType of UnsizedType.t * UnsizedType.t
    | MismatchedArrayTypes of UnsizedType.t * UnsizedType.t
    | InvalidRowVectorTypes of UnsizedType.t
    | InvalidMatrixTypes of UnsizedType.t
    | IntExpected of string * UnsizedType.t
    | IntOrRealExpected of string * UnsizedType.t
    | TupleExpected of string * UnsizedType.t
    | TypeExpected of string * UnsizedType.t * UnsizedType.t
    | IntIntArrayOrRangeExpected of UnsizedType.t
    | IntOrRealContainerExpected of UnsizedType.t
    | ArrayVectorRowVectorMatrixExpected of UnsizedType.t
    | IllTypedReduceSumNotArray of UnsizedType.t
    | IllTypedReduceSumSlice of UnsizedType.t
    | IllTypedReduceSum of
        string
        * UnsizedType.t list
        * UnsizedType.argumentlist
        * SignatureMismatch.function_mismatch
    | IllTypedVariadic of
        string
        * UnsizedType.t list
        * UnsizedType.argumentlist
        * SignatureMismatch.function_mismatch
        * UnsizedType.t
    | IllTypedForwardedFunctionSignature of
        string * string * SignatureMismatch.details
    | IllTypedForwardedFunctionApp of
        string * string * string list * SignatureMismatch.details
    | IllTypedLaplaceHelperArgs of
        string * UnsizedType.argumentlist * SignatureMismatch.details
    | IllTypedLaplaceMarginal of string * bool * UnsizedType.argumentlist
    | LaplaceCompatibilityIssue of string
    | IlltypedLaplaceTooMany of string * int
    | IlltypedLaplaceTolArgs of string * SignatureMismatch.function_mismatch
    | AmbiguousFunctionPromotion of
        string
        * UnsizedType.t list option
        * (UnsizedType.returntype * UnsizedType.argumentlist) list
    | ReturningFnExpectedNonReturningFound of string
    | ReturningFnExpectedNonFnFound of string
    | ReturningFnExpectedUndeclaredDistSuffixFound of string * string
    | ReturningFnExpectedWrongDistSuffixFound of string * string
    | NonReturningFnExpectedReturningFound of string
    | NonReturningFnExpectedNonFnFound of string
    | FuncOverloadRtOnly of
        string * UnsizedType.returntype * UnsizedType.returntype
    | FuncDeclRedefined of string * UnsizedType.t * bool
    | FunDeclExists of string
    | FunDeclNoDefn of string
    | FunDeclNeedsBlock
    | NonRealProbFunDef of UnsizedType.returntype
    | ProbDensityNonRealVariate of UnsizedType.t option
    | ProbMassNonIntVariate of UnsizedType.t option
    | IncompatibleReturnType
    | IllTypedFunctionApp of
        string
        * UnsizedType.t list
        * (SignatureMismatch.signature_error list * bool)

  let laplace_tolerance_arg_name n =
    match n with
    | 1 -> "first control parameter (initial guess)"
    | 2 -> "second control parameter (tolerance)"
    | 3 -> "third control parameter (max_num_steps)"
    | 4 -> "fourth control parameter (hessian_block_size)"
    | 5 -> "fifth control parameter (solver)"
    | 6 -> "sixth control parameter (max_steps_line_search)"
    | n -> Fmt.str "%a control parameter" (Fmt.ordinal ()) n

  let rec expected_types : UnsizedType.t Common.Nonempty_list.t Fmt.t =
    let ust = expected_style UnsizedType.pp in
    fun ppf l ->
      match l with
      | [t] -> ust ppf t
      | [t1; t2] -> Fmt.pf ppf "%a or %a" ust t1 ust t2
      | [t1; t2; t3] -> Fmt.pf ppf "%a,@ %a,@ or %a" ust t1 ust t2 ust t3
      | t :: ts ->
          Fmt.pf ppf "%a,@ %a" ust t expected_types
            (ts |> Common.Nonempty_list.of_list_exn)

  let pp ppf = function
    | IncorrectReturnType (t1, t2) ->
        Fmt.pf ppf
          "Invalid return statement. Function is declared to return %a, but \
           this statement returns %a instead."
          expected_types [t1]
          (actual_style UnsizedType.pp)
          t2
    | MismatchedArrayTypes (t1, t2) ->
        Fmt.pf ppf
          "Array expression must have entries of consistent type. Expected %a \
           but found %a."
          expected_types [t1]
          (actual_style UnsizedType.pp)
          t2
    | InvalidRowVectorTypes ty ->
        Fmt.pf ppf "@[Row vector expression must have all %a entries.%a@]"
          expected_types [UInt; UReal; UComplex] found_type ty
    | InvalidMatrixTypes ty ->
        Fmt.pf ppf "@[Matrix expression must have all %a entries.%a@]"
          expected_types
          [URowVector; UComplexRowVector]
          found_type ty
    | IntExpected (name, ut) ->
        Fmt.pf ppf "@[%s must be of type %a.%a@]" name expected_types [UInt]
          found_type ut
    | IntOrRealExpected (name, ut) ->
        Fmt.pf ppf "@[%s must be of type %a.%a@]" name expected_types
          [UInt; UReal] found_type ut
    | TupleExpected (name, ut) ->
        Fmt.pf ppf "@[%s must be a %a.%a@]" name
          (expected_style Fmt.string)
          "tuple" found_type ut
    | TypeExpected (name, (UInt | UReal | UComplex), ut) ->
        Fmt.pf ppf "@[%s must be a %a.%a@]" name
          (expected_style Fmt.string)
          "scalar" found_type ut
    | TypeExpected (name, et, ut) ->
        Fmt.pf ppf "@[%s must be a %a or of type %a.%a@]" name
          (expected_style Fmt.string)
          "scalar" expected_types [et] found_type ut
    | IntOrRealContainerExpected ut ->
        Fmt.pf ppf "@[A (container of) %a was expected.%a@]" expected_types
          [UReal; UInt] found_type ut
    | IntIntArrayOrRangeExpected ut ->
        Fmt.pf ppf
          "@[Index must be of type %a or must be a range (int:int).%a@]"
          expected_types [UInt; UArray UInt] found_type ut
    | ArrayVectorRowVectorMatrixExpected ut ->
        Fmt.pf ppf "@[Foreach-loop must be over %a, %a.%a@]"
          (expected_style Fmt.string)
          "array" expected_types
          [UVector; URowVector; UMatrix]
          found_type ut
    | IllTypedReduceSumNotArray ty ->
        Fmt.pf ppf "The second argument to reduce_sum must be an array.%a"
          found_type ty
    | IllTypedReduceSumSlice ty ->
        Fmt.pf ppf "The inner type in reduce_sum array must be %a.%a"
          expected_types
          (Stan_math_signatures.reduce_sum_slice_types
         |> Common.Nonempty_list.of_list_exn)
          found_type ty
    | IllTypedReduceSum (name, arg_tys, expected_args, error) ->
        SignatureMismatch.pp_signature_mismatch ppf
          (name, arg_tys, ([((ReturnType UReal, expected_args), error)], false))
    | IllTypedVariadic (name, arg_tys, args, error, return_type) ->
        SignatureMismatch.pp_signature_mismatch ppf
          ( name
          , arg_tys
          , ([((UnsizedType.ReturnType return_type, args), error)], false) )
    | IllTypedFunctionApp (name, arg_tys, errors) ->
        SignatureMismatch.pp_signature_mismatch ppf (name, arg_tys, errors)
    | IllTypedForwardedFunctionApp (caller, name, skipped, details) ->
        Fmt.pf ppf
          "Cannot call %a@ with arguments forwarded from call to@ %a:@ %a"
          quoted name quoted caller
          (SignatureMismatch.pp_mismatch_details ~skipped)
          details
    | IllTypedForwardedFunctionSignature (caller, name, details) ->
        Fmt.pf ppf
          "Function %a does not have a valid signature for use in %a:@ %a"
          quoted name quoted caller
          (SignatureMismatch.pp_mismatch_details ~skipped:[])
          details
    | IllTypedLaplaceHelperArgs (name, expected, details) ->
        Fmt.pf ppf
          "@[<v>Ill-typed arguments supplied to function %a@ for the \
           likelihood:@ %a@ Expected the arguments to start with:@ @[(%a)@]@]"
          quoted name
          (SignatureMismatch.pp_mismatch_details ~skipped:[])
          details
          Fmt.(list ~sep:comma (expected_style UnsizedType.pp_fun_arg))
          expected
    | IllTypedLaplaceMarginal (name, early, supplied) ->
        let req = Stan_math_signatures.laplace_helper_param_types name in
        let is_helper = not @@ List.is_empty req in
        let info =
          if early then
            "We were unable to start more in-depth checking. Please ensure you \
             are passing enough arguments and that the first argument is a \
             function."
          else
            let n = if is_helper then List.length req else 2 in
            Fmt.str
              "Typechecking failed after checking the first %d arguments. \
               Please ensure you are passing enough arguments and that the %a \
               is a function."
              n (Fmt.ordinal ()) (n + 1) in
        let pp_lik_args ppf =
          if is_helper then Fmt.(list ~sep:comma UnsizedType.pp_fun_arg) ppf req
          else
            Fmt.pf ppf "(vector, T_l%t) => real,@ tuple(T_l%t)" ellipsis
              ellipsis in
        let pp_laplace_tols ppf =
          if String.is_substring ~substring:"_tol" name then
            Fmt.pf ppf ", %a"
              Fmt.(list ~sep:comma UnsizedType.pp_fun_arg)
              Stan_math_signatures.laplace_tolerance_argument_types in
        Fmt.pf ppf
          "@[<v>Ill-typed arguments supplied to function %a.@ The valid \
           signature of this function is@ @[<hov 2>%s(%t,@ vector,@ (T_k%t) => \
           matrix,@ tuple(T_k%t)%t)@]@ However, we received the types:@ @[<hov \
           2>(%a)@]@ @[%a@]@]"
          quoted name name pp_lik_args ellipsis ellipsis pp_laplace_tols
          Fmt.(list ~sep:comma UnsizedType.pp_fun_arg)
          supplied Fmt.text info
    | LaplaceCompatibilityIssue banned_function ->
        Fmt.pf ppf
          "The function %a, called by this likelihood function,@ does not \
           currently support higher-order derivatives, and@ cannot be used in \
           an embedded Laplace approximation."
          quoted banned_function
    | IlltypedLaplaceTooMany (name, n_args) ->
        Fmt.pf ppf
          "Received %d extra %a at the end of the call to %a.@ Did you mean to \
           call the _tol version?"
          n_args arguments n_args quoted name
    (* For tolerances, because these come at the end, we want to update their
       position number accordingly, which is why these reimplement some of the
       printing from [SignatureMismatch] *)
    | IlltypedLaplaceTolArgs (name, ArgNumMismatch (_, found)) ->
        Fmt.pf ppf
          "@[<v>Received %a control %a at the end of the call to %a.@ Expected \
           %a arguments for the control parameters instead.@]"
          (actual_style Fmt.int) found arguments found quoted name
          (expected_style Fmt.int)
          (List.length Stan_math_signatures.laplace_tolerance_argument_types)
    | IlltypedLaplaceTolArgs (name, ArgError (n, DataOnlyError)) ->
        Fmt.pf ppf
          "@[<hov>The control parameters to %a@ must all be data-only,@ but \
           the %s here is not.@ %a@]"
          quoted name
          (laplace_tolerance_arg_name n)
          SignatureMismatch.data_only_msg ()
    | IlltypedLaplaceTolArgs
        (name, ArgError (n, TypeMismatch (expected, found, _))) ->
        Fmt.pf ppf "@[<hov>The %s to %a@ must be@ %a.%a@]"
          (laplace_tolerance_arg_name n)
          quoted name expected_types [expected] found_type found
    | AmbiguousFunctionPromotion (name, arg_tys, signatures) ->
        let pp_sig ppf (rt, args) =
          Fmt.pf ppf "@[<hov>(@[<hov>%a@]) => %a@]"
            Fmt.(list ~sep:comma UnsizedType.pp_fun_arg)
            args UnsizedType.pp_returntype rt in
        Fmt.pf ppf
          "No unique minimum promotion found for function %a.@ Overloaded \
           functions must not have multiple equally valid promotion paths.@ %a \
           function has several:@ @[<v>%a@]@ Consider defining a new signature \
           for the exact types needed or@ re-thinking existing definitions."
          quoted name
          (Fmt.option ~none:(Fmt.any "This") (fun ppf tys ->
               Fmt.pf ppf "For args @[(%a)@], this"
                 (Fmt.list ~sep:Fmt.comma UnsizedType.pp)
                 tys))
          arg_tys
          (Fmt.list ~sep:Fmt.cut pp_sig)
          signatures
    | ReturningFnExpectedNonReturningFound fn_name ->
        Fmt.pf ppf
          "A returning function was expected but a non-returning function %a \
           was supplied."
          quoted fn_name
    | NonReturningFnExpectedReturningFound fn_name ->
        Fmt.pf ppf
          "A non-returning function was expected but a returning function %a \
           was supplied."
          quoted fn_name
    | ReturningFnExpectedNonFnFound fn_name ->
        Fmt.pf ppf
          "A returning function was expected but a non-function value %a was \
           supplied."
          quoted fn_name
    | NonReturningFnExpectedNonFnFound fn_name ->
        Fmt.pf ppf
          "A non-returning function was expected but a non-function value %a \
           was supplied."
          quoted fn_name
    | ReturningFnExpectedUndeclaredDistSuffixFound (prefix, suffix) ->
        Fmt.pf ppf "Function %a is not implemented for distribution %a." quoted
          (prefix ^ "_" ^ suffix)
          quoted prefix
    | ReturningFnExpectedWrongDistSuffixFound (prefix, suffix) ->
        let newsuffix =
          match suffix with
          | "lpdf" -> "lpmf"
          | "lupdf" -> "lupmf"
          | "lpmf" -> "lpdf"
          | "lupmf" -> "lupdf"
          | _ ->
              Common.ICE.internal_compiler_error
                [%message "Bad suffix:" (suffix : string)] in
        Fmt.pf ppf
          "Function %a is not implemented for distribution %a, use %a instead."
          quoted
          (prefix ^ "_" ^ suffix)
          quoted prefix quoted
          (prefix ^ "_" ^ newsuffix)
    | FuncOverloadRtOnly (name, _, rt') ->
        Fmt.pf ppf
          "Function %a cannot be overloaded by return type only. Previously \
           used return type %a"
          quoted name UnsizedType.pp_returntype rt'
    | FuncDeclRedefined (name, ut, stan_math) ->
        Fmt.pf ppf "Function %a %s signature %a" quoted name
          (if stan_math then "is already declared in the Stan Math library with"
           else "has already been declared for")
          UnsizedType.pp ut
    | FunDeclExists name ->
        Fmt.pf ppf
          "Function %a has already been declared. A definition is expected."
          quoted name
    | FunDeclNoDefn name ->
        Fmt.pf ppf "Function %a is declared without specifying a definition."
          quoted name
    | FunDeclNeedsBlock ->
        Fmt.pf ppf "Function definitions must be wrapped in curly braces."
    | NonRealProbFunDef Void ->
        Fmt.pf ppf
          "@[Real return type required for probability functions ending in \
           _lpdf, _lupdf, _lpmf, _lupmf, _cdf, _lcdf, or _lccdf.@ Instead \
           found a void function.@]"
    | NonRealProbFunDef (ReturnType t) ->
        Fmt.pf ppf
          "@[Real return type required for probability functions ending in \
           _lpdf, _lupdf, _lpmf, _lupmf, _cdf, _lcdf, or _lccdf.%a@]"
          found_type t
    | ProbDensityNonRealVariate ut ->
        Fmt.pf ppf
          "@[Probability density functions require real variates (first \
           argument).%a@]"
          Fmt.(option found_type)
          ut
    | ProbMassNonIntVariate ut ->
        Fmt.pf ppf
          "@[Probability mass functions require integer variates (first \
           argument).%a@]"
          Fmt.(option found_type)
          ut
    | IncompatibleReturnType ->
        Fmt.pf ppf
          "Function bodies must contain a return statement of correct type in \
           every branch."
end

module IdentifierError = struct
  type t =
    | IsKeyword of string
    | IsModelName of string
    | IsStanMathName of string
    | InUse of string
    | NotInScope of string * string option
    | ReturningFnExpectedUndeclaredIdentFound of string * string option
    | NonReturningFnExpectedUndeclaredIdentFound of string * string option
    | UnnormalizedSuffix of string
    | DuplicateArgNames

  let did_you_mean : string option Fmt.t =
    Fmt.option @@ fun ppf s -> Fmt.pf ppf "@ Did you mean %a?" quoted s

  let pp ppf = function
    | IsStanMathName name ->
        Fmt.pf ppf
          "Identifier %a clashes with a non-overloadable Stan Math library \
           function."
          quoted name
    | InUse name -> Fmt.pf ppf "Identifier %a is already in use." quoted name
    | IsModelName name ->
        Fmt.pf ppf "Identifier %a clashes with model name." quoted name
    | IsKeyword name ->
        Fmt.pf ppf "Identifier %a clashes with reserved keyword." quoted name
    | NotInScope (name, sug) ->
        Fmt.pf ppf "@[Identifier %a not in scope.%a@]" quoted name did_you_mean
          sug
    | ReturningFnExpectedUndeclaredIdentFound (fn_name, sug) ->
        Fmt.pf ppf
          "@[A returning function was expected but an undeclared identifier %a \
           was supplied.%a@]"
          quoted fn_name did_you_mean sug
    | NonReturningFnExpectedUndeclaredIdentFound (fn_name, sug) ->
        Fmt.pf ppf
          "@[A non-returning function was expected but an undeclared \
           identifier %a was supplied.%a@]"
          quoted fn_name did_you_mean sug
    | UnnormalizedSuffix name ->
        Fmt.pf ppf
          "Identifier %a has a _lupdf/_lupmf suffix, which is only allowed for \
           functions."
          quoted name
    | DuplicateArgNames ->
        Fmt.pf ppf "All function arguments must have distinct identifiers."
end

module ExpressionError = struct
  type t =
    | InvalidSizeDeclRng
    | InvalidRngFunction
    | InvalidUnnormalizedFunction
    | InvalidUnnormalizedUDF of string
    | ConditionalNotationNotAllowed
    | ConditioningRequired
    | NotPrintable
    | EmptyArray
    | EmptyTuple
    | IntTooLarge
    | TupleIndexInvalidIndex of int * int
    | TupleIndexNotTuple of UnsizedType.t
    | NotIndexable of UnsizedType.t * int
    | IllTypedTernaryIf of UnsizedType.t * UnsizedType.t * UnsizedType.t
    | IllTypedBinaryOperator of Operator.t * UnsizedType.t * UnsizedType.t
    | IllTypedPrefixOperator of Operator.t * UnsizedType.t
    | IllTypedPostfixOperator of Operator.t * UnsizedType.t

  let pp ppf = function
    | InvalidSizeDeclRng ->
        Fmt.pf ppf
          "Random number generators are not allowed in top level size \
           declarations."
    | InvalidRngFunction ->
        Fmt.text ppf
          "Random number generators are only allowed in transformed data \
           block, generated quantities block or user-defined functions with \
           names ending in _rng."
    | InvalidUnnormalizedFunction ->
        Fmt.text ppf
          "Functions with names ending in _lupdf and _lupmf can only be used \
           in the model block or user-defined functions with names ending in \
           _lpdf or _lpmf."
    | InvalidUnnormalizedUDF fname ->
        Fmt.pf ppf
          "@[%a is an invalid user-defined function name.@ User-defined \
           probability mass and density functions must@ be defined as \
           normalized@ (function names should end@ with@ _lpdf/_lpmf not \
           _lupdf/_lupmf).@]"
          quoted fname
    | ConditionalNotationNotAllowed ->
        Fmt.text ppf
          "Only functions with names ending in _lpdf, _lupdf, _lpmf, _lupmf, \
           _cdf, _lcdf, _lccdf can make use of conditional notation."
    | ConditioningRequired ->
        Fmt.text ppf
          "Probability functions with suffixes _lpdf, _lupdf, _lpmf, _lupmf, \
           _cdf, _lcdf and _lccdf, require a vertical bar (|) between the \
           first two arguments."
    | NotPrintable -> Fmt.pf ppf "Functions cannot be printed."
    | EmptyArray ->
        Fmt.pf ppf "Array expressions must contain at least one element."
    | EmptyTuple ->
        Fmt.pf ppf "Tuple expressions must contain at least one element."
    | IntTooLarge ->
        Fmt.pf ppf "Integer literal cannot be larger than 2_147_483_647."
    | TupleIndexInvalidIndex (ix_max, ix) ->
        Fmt.pf ppf
          "Tried to access index %d for a tuple of length %d.@ Only indices \
           indices between 1 and %d are valid."
          ix ix_max ix_max
    | TupleIndexNotTuple ut ->
        Fmt.pf ppf "Tried to index a non-tuple type. Expression has type %a."
          (actual_style UnsizedType.pp)
          ut
    | NotIndexable (ut, _) when UnsizedType.is_scalar_type ut ->
        Fmt.pf ppf "Tried to index a scalar type. Expression has type %a."
          (actual_style UnsizedType.pp)
          ut
    | NotIndexable (ut, nidcs) ->
        Fmt.pf ppf
          "Too many indexes, expression dimensions=%d, indexes found=%d."
          (UnsizedType.count_dims ut)
          nidcs
    | IllTypedTernaryIf (UInt, ut, _) when UnsizedType.is_fun_type ut ->
        Fmt.pf ppf "Ternary expression cannot have a function type: %a"
          (actual_style UnsizedType.pp)
          ut
    | IllTypedTernaryIf (UInt, ut2, ut3) ->
        Fmt.pf ppf
          "Type mismatch in ternary expression, expression when true is: %a; \
           expression when false is: %a"
          (actual_style UnsizedType.pp)
          ut2
          (actual_style UnsizedType.pp)
          ut3
    | IllTypedTernaryIf (ut1, _, _) ->
        Fmt.pf ppf "@[Condition in ternary expression must be type %a.%a@]"
          (expected_style UnsizedType.pp)
          UInt found_type ut1
    | IllTypedBinaryOperator (op, lt, rt) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to infix operator %a. Available \
           signatures: %s@[<h>Instead supplied arguments of incompatible type: \
           %a, %a.@]"
          Operator.pp op
          (Stan_math_signatures.pretty_print_math_lib_operator_sigs op
          |> String.concat ~sep:"\n")
          UnsizedType.pp lt UnsizedType.pp rt
    | IllTypedPrefixOperator (op, ut) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to prefix operator %a. Available \
           signatures: %s@[<h>Instead supplied argument of incompatible type: \
           %a.@]"
          Operator.pp op
          (Stan_math_signatures.pretty_print_math_lib_operator_sigs op
          |> String.concat ~sep:"\n")
          UnsizedType.pp ut
    | IllTypedPostfixOperator (op, ut) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to postfix operator %a. Available \
           signatures: %s\n\
           Instead supplied argument of incompatible type: %a."
          Operator.pp op
          (Stan_math_signatures.pretty_print_math_lib_operator_sigs op
          |> String.concat ~sep:"\n")
          UnsizedType.pp ut
end

module StatementError = struct
  type t =
    | CannotAssignToReadOnly of string
    | CannotAssignToGlobal of string
    | CannotAssignFunction of string * UnsizedType.t
    | LValueMultiIndexing
    | LValueTupleUnpackDuplicates of Ast.untyped_lval list
    | LValueTupleReadAndWrite of string list
    | InvalidTildePDForPMF
    | InvalidTildeCDForCCDF of string
    | InvalidTildeNoSuchDistribution of string * bool
    | TargetPlusEqualsOutsideModelOrLogProb
    | JacobianPlusEqualsNotAllowed
    | InvalidTruncationCDForCCDF of UnsizedType.argumentlist
    | BreakOutsideLoop
    | ContinueOutsideLoop
    | ExpressionReturnOutsideReturningFn
    | VoidReturnOutsideNonReturningFn
    | NonDataVariableSizeDecl
    | NonIntBounds
    | ComplexTransform
    | TransformedParamsInt
    | IllTypedAssignment of Operator.t * UnsizedType.t * UnsizedType.t

  let pp ppf = function
    | CannotAssignToReadOnly name ->
        Fmt.pf ppf "Cannot assign to function argument or loop identifier %a."
          quoted name
    | CannotAssignToGlobal name ->
        Fmt.pf ppf
          "Cannot assign to global variable %a declared in previous blocks."
          quoted name
    | CannotAssignFunction (name, ut) ->
        Fmt.pf ppf "Cannot assign a function type \"%a\" to variable %a."
          (SignatureMismatch.actual_style UnsizedType.pp)
          ut quoted name
    | LValueMultiIndexing ->
        Fmt.pf ppf
          "Left hand side of an assignment cannot have nested multi-indexing."
    | LValueTupleUnpackDuplicates lvs ->
        let rec pp_lvalue ppf (l : Ast.untyped_lval) =
          let open Fmt in
          match l.lval with
          | LVariable id -> string ppf id.name
          | LIndexed (l, _) -> pf ppf "%a[%t]" pp_lvalue l ellipsis
          | LTupleProjection (l, ix) -> pf ppf "%a.%n" pp_lvalue l ix in
        Fmt.pf ppf
          "@[<v2>The same value cannot be assigned to multiple times in one \
           assignment:@ @[%a@]@]"
          Fmt.(list ~sep:comma pp_lvalue)
          lvs
    | LValueTupleReadAndWrite ids ->
        Fmt.pf ppf
          "@[<v2>The same variable cannot be both assigned to and read from on \
           the left hand side of an assignment:@ @[%a@]@]"
          Fmt.(list ~sep:comma string)
          ids
    | TargetPlusEqualsOutsideModelOrLogProb ->
        Fmt.string ppf
          "Target can only be accessed in the model block or in definitions of \
           functions with the suffix _lp."
    | JacobianPlusEqualsNotAllowed ->
        Fmt.string ppf
          "The jacobian adjustment can only be applied in the transformed \
           parameters block or in functions ending with _jacobian"
    | InvalidTildePDForPMF ->
        Fmt.string ppf
          "~ statement should refer to a distribution without its \
           \"_lpdf/_lupdf\" or \"_lpmf/_lupmf\" suffix.\n\
           For example, \"target += normal_lpdf(y, 0, 1)\" should become \"y ~ \
           normal(0, 1).\""
    | InvalidTildeCDForCCDF name ->
        let name =
          match String.chop_suffix name ~suffix:"_cdf" with
          | Some n -> n ^ "_lcdf"
          | None -> (
              match String.chop_suffix name ~suffix:"_ccdf" with
              | Some n -> n ^ "_lccdf"
              | None -> name) in
        Fmt.pf ppf
          "CDF and CCDF functions may not be used with distribution notation \
           (~). Use target += %s(%t) instead."
          name ellipsis
    | InvalidTildeNoSuchDistribution (name, true) ->
        Fmt.pf ppf
          "Ill-typed arguments to distribution statement (~). No function %a \
           or %a was found when looking for distribution %a."
          quoted (name ^ "_lpmf") quoted (name ^ "_lpdf") quoted name
    | InvalidTildeNoSuchDistribution (name, false) ->
        Fmt.pf ppf
          "Ill-typed arguments to %a-statement. No function %a was found when \
           looking for distribution %a."
          quoted "~" quoted (name ^ "_lpdf") quoted name
    | InvalidTruncationCDForCCDF args ->
        Fmt.pf ppf
          "Truncation is only defined if distribution has _lcdf and _lccdf \
           functions implemented with appropriate signature.\n\
           No matching signature for arguments: @[(%a)@]"
          Fmt.(list ~sep:comma UnsizedType.pp_fun_arg)
          args
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
        Fmt.pf ppf "@[Bounds of integer variable must be of type %a.%a@]"
          (expected_style UnsizedType.pp)
          UInt found_type UReal
    | ComplexTransform ->
        Fmt.pf ppf "Complex types do not support transformations."
    | TransformedParamsInt ->
        Fmt.pf ppf "(Transformed) Parameters cannot be integers."
    | IllTypedAssignment (Operator.Equals, lt, rt) ->
        Fmt.pf ppf
          "Ill-typed arguments supplied to assignment operator =:@ @[<v2>The \
           left hand side has type@ @[%a@]@]@ @[<v2>and the right hand side \
           has type@ @[%a@]@]"
          UnsizedType.pp lt UnsizedType.pp rt
    | IllTypedAssignment (op, lt, rt) ->
        Fmt.pf ppf
          "@[<v>Ill-typed arguments supplied to assignment operator %a=:@ \
           @[<v2>The left hand side has type@ @[%a@]@]@ @[<v2>and the right \
           hand side has type@ @[%a@]@]@ Available signatures for given \
           lhs:@]@ %a"
          Operator.pp op UnsizedType.pp lt UnsizedType.pp rt
          SignatureMismatch.pp_math_lib_assignmentoperator_sigs (lt, op)
end

type err =
  | TypeError of TypeError.t
  | IdentifierError of IdentifierError.t
  | ExpressionError of ExpressionError.t
  | StatementError of StatementError.t

type t = Location_span.t * err

let pp ppf (_, err) =
  match err with
  | TypeError err -> TypeError.pp ppf err
  | IdentifierError err -> IdentifierError.pp ppf err
  | ExpressionError err -> ExpressionError.pp ppf err
  | StatementError err -> StatementError.pp ppf err

let location = fst

(* -- Constructors ---------------------------------------------------------- *)

let invalid_return loc t1 t2 =
  (loc, TypeError (TypeError.IncorrectReturnType (t1, t2)))

let mismatched_array_types loc t1 t2 =
  (loc, TypeError (TypeError.MismatchedArrayTypes (t1, t2)))

let invalid_row_vector_types loc ty =
  (loc, TypeError (TypeError.InvalidRowVectorTypes ty))

let invalid_matrix_types loc ty =
  (loc, TypeError (TypeError.InvalidMatrixTypes ty))

let int_expected loc name ut =
  (loc, TypeError (TypeError.IntExpected (name, ut)))

let int_or_real_expected loc name ut =
  (loc, TypeError (TypeError.IntOrRealExpected (name, ut)))

let tuple_expected loc name ut =
  (loc, TypeError (TypeError.TupleExpected (name, ut)))

let scalar_or_type_expected loc name et ut =
  (loc, TypeError (TypeError.TypeExpected (name, et, ut)))

let int_intarray_or_range_expected loc ut =
  (loc, TypeError (TypeError.IntIntArrayOrRangeExpected ut))

let int_or_real_container_expected loc ut =
  (loc, TypeError (TypeError.IntOrRealContainerExpected ut))

let array_vector_rowvector_matrix_expected loc ut =
  (loc, TypeError (TypeError.ArrayVectorRowVectorMatrixExpected ut))

let illtyped_assignment loc assignop lt rt =
  (loc, StatementError (StatementError.IllTypedAssignment (assignop, lt, rt)))

let illtyped_ternary_if loc predt lt rt =
  (loc, ExpressionError (ExpressionError.IllTypedTernaryIf (predt, lt, rt)))

let returning_fn_expected_nonreturning_found loc name =
  (loc, TypeError (TypeError.ReturningFnExpectedNonReturningFound name))

let illtyped_reduce_sum_not_array loc ty =
  (loc, TypeError (TypeError.IllTypedReduceSumNotArray ty))

let illtyped_reduce_sum_slice loc ty =
  (loc, TypeError (TypeError.IllTypedReduceSumSlice ty))

let illtyped_reduce_sum loc name arg_tys args error =
  (loc, TypeError (TypeError.IllTypedReduceSum (name, arg_tys, args, error)))

let illtyped_variadic loc name arg_tys args fn_rt error =
  ( loc
  , TypeError (TypeError.IllTypedVariadic (name, arg_tys, args, error, fn_rt))
  )

let forwarded_function_application_error loc caller name required_args details =
  ( loc
  , TypeError
      (TypeError.IllTypedForwardedFunctionApp
         (caller, name, required_args, details)) )

let forwarded_function_signature_error loc caller name details =
  ( loc
  , TypeError
      (TypeError.IllTypedForwardedFunctionSignature (caller, name, details)) )

let illtyped_laplace_helper_args loc name lik_args details =
  ( loc
  , TypeError (TypeError.IllTypedLaplaceHelperArgs (name, lik_args, details)) )

let illtyped_laplace_generic loc name early supplied =
  (loc, TypeError (TypeError.IllTypedLaplaceMarginal (name, early, supplied)))

let laplace_compatibility loc banned_function =
  (loc, TypeError (TypeError.LaplaceCompatibilityIssue banned_function))

let illtyped_laplace_extra_args loc name args =
  (loc, TypeError (TypeError.IlltypedLaplaceTooMany (name, args)))

let illtyped_laplace_tolerance_args loc name mismatch =
  (loc, TypeError (TypeError.IlltypedLaplaceTolArgs (name, mismatch)))

let ambiguous_function_promotion loc name arg_tys signatures =
  ( loc
  , TypeError (TypeError.AmbiguousFunctionPromotion (name, arg_tys, signatures))
  )

let returning_fn_expected_nonfn_found loc name =
  (loc, TypeError (TypeError.ReturningFnExpectedNonFnFound name))

let returning_fn_expected_undeclaredident_found loc name sug =
  ( loc
  , IdentifierError
      (IdentifierError.ReturningFnExpectedUndeclaredIdentFound (name, sug)) )

let returning_fn_expected_undeclared_dist_suffix_found loc (prefix, suffix) =
  ( loc
  , TypeError
      (TypeError.ReturningFnExpectedUndeclaredDistSuffixFound (prefix, suffix))
  )

let returning_fn_expected_wrong_dist_suffix_found loc (prefix, suffix) =
  ( loc
  , TypeError
      (TypeError.ReturningFnExpectedWrongDistSuffixFound (prefix, suffix)) )

let nonreturning_fn_expected_returning_found loc name =
  (loc, TypeError (TypeError.NonReturningFnExpectedReturningFound name))

let nonreturning_fn_expected_nonfn_found loc name =
  (loc, TypeError (TypeError.NonReturningFnExpectedNonFnFound name))

let nonreturning_fn_expected_undeclaredident_found loc name sug =
  ( loc
  , IdentifierError
      (IdentifierError.NonReturningFnExpectedUndeclaredIdentFound (name, sug))
  )

let illtyped_fn_app loc name errors arg_tys =
  (loc, TypeError (TypeError.IllTypedFunctionApp (name, arg_tys, errors)))

let illtyped_binary_op loc op lt rt =
  (loc, ExpressionError (ExpressionError.IllTypedBinaryOperator (op, lt, rt)))

let illtyped_prefix_op loc op ut =
  (loc, ExpressionError (ExpressionError.IllTypedPrefixOperator (op, ut)))

let illtyped_postfix_op loc op ut =
  (loc, ExpressionError (ExpressionError.IllTypedPostfixOperator (op, ut)))

let not_indexable loc ut nidcs =
  (loc, ExpressionError (ExpressionError.NotIndexable (ut, nidcs)))

let tuple_index_invalid_index loc ix_max ix =
  (loc, ExpressionError (ExpressionError.TupleIndexInvalidIndex (ix_max, ix)))

let tuple_index_not_tuple loc ut =
  (loc, ExpressionError (ExpressionError.TupleIndexNotTuple ut))

let ident_is_keyword loc name =
  (loc, IdentifierError (IdentifierError.IsKeyword name))

let ident_is_model_name loc name =
  (loc, IdentifierError (IdentifierError.IsModelName name))

let ident_is_stanmath_name loc name =
  (loc, IdentifierError (IdentifierError.IsStanMathName name))

let ident_in_use loc name = (loc, IdentifierError (IdentifierError.InUse name))

let ident_not_in_scope loc name sug =
  (loc, IdentifierError (IdentifierError.NotInScope (name, sug)))

let ident_has_unnormalized_suffix loc name =
  (loc, IdentifierError (IdentifierError.UnnormalizedSuffix name))

let invalid_decl_rng_fn loc =
  (loc, ExpressionError ExpressionError.InvalidSizeDeclRng)

let invalid_rng_fn loc =
  (loc, ExpressionError ExpressionError.InvalidRngFunction)

let invalid_unnormalized_fn loc =
  (loc, ExpressionError ExpressionError.InvalidUnnormalizedFunction)

let udf_is_unnormalized_fn loc name =
  (loc, ExpressionError (ExpressionError.InvalidUnnormalizedUDF name))

let conditional_notation_not_allowed loc =
  (loc, ExpressionError ExpressionError.ConditionalNotationNotAllowed)

let conditioning_required loc =
  (loc, ExpressionError ExpressionError.ConditioningRequired)

let not_printable loc = (loc, ExpressionError ExpressionError.NotPrintable)
let empty_array loc = (loc, ExpressionError ExpressionError.EmptyArray)
let empty_tuple loc = (loc, ExpressionError ExpressionError.EmptyTuple)
let bad_int_literal loc = (loc, ExpressionError ExpressionError.IntTooLarge)

let cannot_assign_to_read_only loc name =
  (loc, StatementError (StatementError.CannotAssignToReadOnly name))

let cannot_assign_to_global loc name =
  (loc, StatementError (StatementError.CannotAssignToGlobal name))

let cannot_assign_function loc name ut =
  (loc, StatementError (StatementError.CannotAssignFunction (name, ut)))

let cannot_assign_to_multiindex loc =
  (loc, StatementError StatementError.LValueMultiIndexing)

let cannot_assign_duplicate_unpacking loc names =
  (loc, StatementError (StatementError.LValueTupleUnpackDuplicates names))

let cannot_access_assigning_var loc names =
  (loc, StatementError (StatementError.LValueTupleReadAndWrite names))

let invalid_tilde_pdf_or_pmf loc =
  (loc, StatementError StatementError.InvalidTildePDForPMF)

let invalid_tilde_cdf_or_ccdf loc name =
  (loc, StatementError (StatementError.InvalidTildeCDForCCDF name))

let invalid_tilde_no_such_dist loc name is_int =
  ( loc
  , StatementError
      (StatementError.InvalidTildeNoSuchDistribution (name, is_int)) )

let target_plusequals_outside_model_or_logprob loc =
  (loc, StatementError StatementError.TargetPlusEqualsOutsideModelOrLogProb)

let jacobian_plusequals_not_allowed loc =
  (loc, StatementError StatementError.JacobianPlusEqualsNotAllowed)

let invalid_truncation_cdf_or_ccdf loc args =
  (loc, StatementError (StatementError.InvalidTruncationCDForCCDF args))

let break_outside_loop loc =
  (loc, StatementError StatementError.BreakOutsideLoop)

let continue_outside_loop loc =
  (loc, StatementError StatementError.ContinueOutsideLoop)

let expression_return_outside_returning_fn loc =
  (loc, StatementError StatementError.ExpressionReturnOutsideReturningFn)

let void_outside_nonreturning_fn loc =
  (loc, StatementError StatementError.VoidReturnOutsideNonReturningFn)

let non_data_variable_size_decl loc =
  (loc, StatementError StatementError.NonDataVariableSizeDecl)

let non_int_bounds loc = (loc, StatementError StatementError.NonIntBounds)
let complex_transform loc = (loc, StatementError StatementError.ComplexTransform)

let transformed_params_int loc =
  (loc, StatementError StatementError.TransformedParamsInt)

let fn_overload_rt_only loc name rt1 rt2 =
  (loc, TypeError (TypeError.FuncOverloadRtOnly (name, rt1, rt2)))

let fn_decl_redefined loc name ~stan_math ut =
  (loc, TypeError (TypeError.FuncDeclRedefined (name, ut, stan_math)))

let fn_decl_exists loc name = (loc, TypeError (TypeError.FunDeclExists name))

let fn_decl_without_def loc name =
  (loc, TypeError (TypeError.FunDeclNoDefn name))

let fn_decl_needs_block loc = (loc, TypeError TypeError.FunDeclNeedsBlock)

let non_real_prob_fn_def loc rt =
  (loc, TypeError (TypeError.NonRealProbFunDef rt))

let prob_density_non_real_variate loc ut_opt =
  (loc, TypeError (TypeError.ProbDensityNonRealVariate ut_opt))

let prob_mass_non_int_variate loc ut_opt =
  (loc, TypeError (TypeError.ProbMassNonIntVariate ut_opt))

let duplicate_arg_names loc =
  (loc, IdentifierError IdentifierError.DuplicateArgNames)

let incompatible_return_types loc =
  (loc, TypeError TypeError.IncompatibleReturnType)
