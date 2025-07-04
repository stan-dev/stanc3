open Core
open Middle

(** Type errors that may arise during semantic check *)
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
    | IllTypedAssignment of Operator.t * UnsizedType.t * UnsizedType.t
    | IllTypedTernaryIf of UnsizedType.t * UnsizedType.t * UnsizedType.t
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
    | TupleIndexInvalidIndex of int * int
    | TupleIndexNotTuple of UnsizedType.t
    | NotIndexable of UnsizedType.t * int

  let pp_laplace_tols ppf () =
    Fmt.pf ppf ", %a"
      Fmt.(list ~sep:comma UnsizedType.pp_fun_arg)
      Stan_math_signatures.laplace_tolerance_argument_types

  (* this function will be in [Fmt] version 0.11 *)
  let ordinal =
    let open Fmt in
    let one ppf i =
      int ppf i;
      string ppf "st" in
    let two ppf i =
      int ppf i;
      string ppf "nd" in
    let three ppf i =
      int ppf i;
      string ppf "rd" in
    let other ppf i =
      int ppf i;
      string ppf "th" in
    fun ?zero ?(one = one) ?(two = two) ?(three = three) ?(other = other) () ->
      let zero = Option.value ~default:other zero in
      fun ppf i ->
        if i = 0 then zero ppf i
        else
          let n = Int.abs i in
          let mod10 = n mod 10 in
          let mod100 = n mod 100 in
          if mod10 = 1 && mod100 <> 11 then one ppf i
          else if mod10 = 2 && mod100 <> 12 then two ppf i
          else if mod10 = 3 && mod100 <> 13 then three ppf i
          else other ppf i

  let laplace_tolerance_arg_name n =
    match n with
    | 1 -> "first control parameter (initial guess)"
    | 2 -> "second control parameter (tolerance)"
    | 3 -> "third control parameter (max_num_steps)"
    | 4 -> "fourth control parameter (hessian_block_size)"
    | 5 -> "fifth control parameter (solver)"
    | 6 -> "sixth control parameter (max_steps_line_search)"
    | n -> Fmt.str "%a control parameter" (ordinal ()) n

  let trailing_s n pp = Fmt.(pp ++ if' (n <> 1) (const string "s"))

  let pp ppf = function
    | IncorrectReturnType (t1, t2) ->
        Fmt.pf ppf
          "Invalid return statement. Function is declared to return %a, but \
           this statement returns %a instead."
          UnsizedType.pp t1 UnsizedType.pp t2
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
    | TupleExpected (name, ut) ->
        Fmt.pf ppf "%s must be a tuple.@ Instead found type %a." name
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
    | IllTypedReduceSumNotArray ty ->
        Fmt.pf ppf
          "The second argument to reduce_sum must be an array but found %a"
          UnsizedType.pp ty
    | IllTypedReduceSumSlice ty ->
        let rec pp ppf = function
          | [] -> Fmt.pf ppf "<error>"
          | [t] -> UnsizedType.pp ppf t
          | [t1; t2] ->
              Fmt.pf ppf "%a, or %a" UnsizedType.pp t1 UnsizedType.pp t2
          | t :: ts -> Fmt.pf ppf "%a, %a" UnsizedType.pp t pp ts in
        Fmt.pf ppf "The inner type in reduce_sum array must be %a but found %a"
          pp Stan_math_signatures.reduce_sum_slice_types UnsizedType.pp ty
    | IllTypedReduceSum (name, arg_tys, expected_args, error) ->
        SignatureMismatch.pp_signature_mismatch ppf
          (name, arg_tys, ([((ReturnType UReal, expected_args), error)], false))
    | IllTypedVariadic (name, arg_tys, args, error, return_type) ->
        SignatureMismatch.pp_signature_mismatch ppf
          ( name
          , arg_tys
          , ([((UnsizedType.ReturnType return_type, args), error)], false) )
    | IllTypedForwardedFunctionApp (caller, name, skipped, details) ->
        Fmt.pf ppf
          "Cannot call '%s'@ with arguments forwarded from call to@ '%s':@ %a"
          name caller
          (SignatureMismatch.pp_mismatch_details ~skipped)
          details
    | IllTypedForwardedFunctionSignature (caller, name, details) ->
        Fmt.pf ppf
          "Function '%s' does not have a valid signature for use in '%s':@ %a"
          name caller
          (SignatureMismatch.pp_mismatch_details ~skipped:[])
          details
    | IllTypedLaplaceHelperArgs (name, expected, details) ->
        Fmt.pf ppf
          "@[<v>Ill-typed arguments supplied to function '%s'@ for the \
           likelihood:@ %a@ Expected the arguments to start with:@ @[(%a)@]@]"
          name
          (SignatureMismatch.pp_mismatch_details ~skipped:[])
          details
          Fmt.(list ~sep:comma UnsizedType.pp_fun_arg)
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
              n (ordinal ()) (n + 1) in
        let pp_lik_args ppf () =
          if is_helper then Fmt.(list ~sep:comma UnsizedType.pp_fun_arg) ppf req
          else Fmt.pf ppf "(vector, T_l...) => real,@ tuple(T_l...)" in
        Fmt.pf ppf
          "@[<v>Ill-typed arguments supplied to function '%s'.@ The valid \
           signature of this function is@ @[<hov 2>%s(%a,@ vector,@ (T_k...) \
           => matrix,@ tuple(T_k...)%a)@]@ However, we recieved the types:@ \
           @[<hov 2>(%a)@]@ @[%a@]@]"
          name name pp_lik_args ()
          Fmt.(if' (String.is_substring ~substring:"_tol" name) pp_laplace_tols)
          ()
          Fmt.(list ~sep:comma UnsizedType.pp_fun_arg)
          supplied Fmt.text info
    | LaplaceCompatibilityIssue banned_function ->
        Fmt.pf ppf
          "The function '%s', called by this likelihood function,@ does not \
           currently support higher-order derivatives, and@ cannot be used in \
           an embedded Laplace approximation."
          banned_function
    | IlltypedLaplaceTooMany (name, n_args) ->
        Fmt.pf ppf
          "Recieved %d extra %a at the end of the call to '%s'.@ Did you mean \
           to call the _tol version?"
          n_args
          (trailing_s n_args Fmt.string)
          "argument" name
    (* For tolerances, because these come at the end, we want to update their
       position number accordingly, which is why these reimplement some of the
       printing from [SignatureMismatch] *)
    | IlltypedLaplaceTolArgs (name, ArgNumMismatch (_, found)) ->
        Fmt.pf ppf
          "@[<v>Recieved %d control %a at the end of the call to '%s'.@ \
           Expected %d arguments for the control parameters instead.@]"
          found
          (trailing_s found Fmt.string)
          "argument" name
          (List.length Stan_math_signatures.laplace_tolerance_argument_types)
    | IlltypedLaplaceTolArgs (name, ArgError (n, DataOnlyError)) ->
        Fmt.pf ppf
          "@[<hov>The control parameters to '%s'@ must all be data-only,@ but \
           the %a here is not.@ %a@]"
          name Fmt.string
          (laplace_tolerance_arg_name n)
          Fmt.text SignatureMismatch.data_only_msg
    | IlltypedLaplaceTolArgs
        (name, ArgError (n, TypeMismatch (expected, found, _))) ->
        Fmt.pf ppf "@[<hov>The %a to '%s'@ must be@ %a but got@ %a.@]"
          Fmt.string
          (laplace_tolerance_arg_name n)
          name UnsizedType.pp expected UnsizedType.pp found
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
                 tys))
          arg_tys
          (Fmt.list ~sep:Fmt.cut pp_sig)
          signatures
    | TupleIndexInvalidIndex (ix_max, ix) ->
        Fmt.pf ppf
          "Tried to access index %d for a tuple of length %d.@ Only indices \
           indices between 1 and %d are valid."
          ix ix_max ix_max
    | TupleIndexNotTuple ut ->
        Fmt.pf ppf "Tried to index a non-tuple type. Expression has type %a."
          UnsizedType.pp ut
    | NotIndexable (ut, _) when UnsizedType.is_scalar_type ut ->
        Fmt.pf ppf "Tried to index a scalar type. Expression has type %a."
          UnsizedType.pp ut
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
              fn_name s)
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
               identifier '%s' was supplied.@ A nearby known identifier is \
               '%s'"
              fn_name s)
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
              Common.ICE.internal_compiler_error
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
           Instead supplied argument of incompatible type: %a." Operator.pp op
          (Stan_math_signatures.pretty_print_math_lib_operator_sigs op
          |> String.concat ~sep:"\n")
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
            Fmt.pf ppf "Identifier '%s' not in scope. Did you mean '%s'?" name s
        )
    | UnnormalizedSuffix name ->
        Fmt.pf ppf
          "Identifier '%s' has a _lupdf/_lupmf suffix, which is only allowed \
           for functions."
          name
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

  let pp ppf = function
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
    | EmptyTuple ->
        Fmt.pf ppf "Tuple expressions must contain at least one element."
    | IntTooLarge ->
        Fmt.pf ppf "Integer literal cannot be larger than 2_147_483_647."
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
    | FuncOverloadRtOnly of
        string * UnsizedType.returntype * UnsizedType.returntype
    | FuncDeclRedefined of string * UnsizedType.t * bool
    | FunDeclExists of string
    | FunDeclNoDefn of string
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
    | CannotAssignFunction (name, ut) ->
        Fmt.pf ppf "Cannot assign a function type '%a' to variable '%s'."
          UnsizedType.pp ut name
    | LValueMultiIndexing ->
        Fmt.pf ppf
          "Left hand side of an assignment cannot have nested multi-indexing."
    | LValueTupleUnpackDuplicates lvs ->
        let rec pp_lvalue ppf (l : Ast.untyped_lval) =
          let open Fmt in
          match l.lval with
          | LVariable id -> string ppf id.name
          | LIndexed (l, _) -> pf ppf "%a[...]" pp_lvalue l
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
        Fmt.pf ppf
          "CDF and CCDF functions may not be used with distribution notation \
           (~). Use target += %s_log(...) instead."
          name
    | InvalidTildeNoSuchDistribution (name, true) ->
        Fmt.pf ppf
          "Ill-typed arguments to distribution statement (~). No function \
           '%s_lpmf' or '%s_lpdf' was found when looking for distribution \
           '%s'."
          name name name
    | InvalidTildeNoSuchDistribution (name, false) ->
        Fmt.pf ppf
          "Ill-typed arguments to '~' statement. No function '%s_lpdf' was \
           found when looking for distribution '%s'."
          name name
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
          (if stan_math then "is already declared in the Stan Math library with"
           else "has already been declared for")
          UnsizedType.pp ut
    | FunDeclExists name ->
        Fmt.pf ppf
          "Function '%s' has already been declared. A definition is expected."
          name
    | FunDeclNoDefn name ->
        Fmt.pf ppf "Function '%s' is declared without specifying a definition."
          name
    | FunDeclNeedsBlock ->
        Fmt.pf ppf "Function definitions must be wrapped in curly braces."
    | NonRealProbFunDef ->
        Fmt.pf ppf
          "Real return type required for probability functions ending in \
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

let invalid_return loc t1 t2 =
  TypeError (loc, TypeError.IncorrectReturnType (t1, t2))

let mismatched_array_types loc t1 t2 =
  TypeError (loc, TypeError.MismatchedArrayTypes (t1, t2))

let invalid_row_vector_types loc ty =
  TypeError (loc, TypeError.InvalidRowVectorTypes ty)

let invalid_matrix_types loc ty =
  TypeError (loc, TypeError.InvalidMatrixTypes ty)

let int_expected loc name ut = TypeError (loc, TypeError.IntExpected (name, ut))

let int_or_real_expected loc name ut =
  TypeError (loc, TypeError.IntOrRealExpected (name, ut))

let tuple_expected loc name ut =
  TypeError (loc, TypeError.TupleExpected (name, ut))

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

let illtyped_reduce_sum_not_array loc ty =
  TypeError (loc, TypeError.IllTypedReduceSumNotArray ty)

let illtyped_reduce_sum_slice loc ty =
  TypeError (loc, TypeError.IllTypedReduceSumSlice ty)

let illtyped_reduce_sum loc name arg_tys args error =
  TypeError (loc, TypeError.IllTypedReduceSum (name, arg_tys, args, error))

let illtyped_variadic loc name arg_tys args fn_rt error =
  TypeError (loc, TypeError.IllTypedVariadic (name, arg_tys, args, error, fn_rt))

let forwarded_function_application_error loc caller name required_args details =
  TypeError
    ( loc
    , TypeError.IllTypedForwardedFunctionApp
        (caller, name, required_args, details) )

let forwarded_function_signature_error loc caller name details =
  TypeError
    (loc, TypeError.IllTypedForwardedFunctionSignature (caller, name, details))

let illtyped_laplace_helper_args loc name lik_args details =
  TypeError (loc, TypeError.IllTypedLaplaceHelperArgs (name, lik_args, details))

let illtyped_laplace_generic loc name early supplied =
  TypeError (loc, TypeError.IllTypedLaplaceMarginal (name, early, supplied))

let laplace_compatibility loc banned_function =
  TypeError (loc, TypeError.LaplaceCompatibilityIssue banned_function)

let illtyped_laplace_extra_args loc name args =
  TypeError (loc, TypeError.IlltypedLaplaceTooMany (name, args))

let illtyped_laplace_tolerance_args loc name mismatch =
  TypeError (loc, TypeError.IlltypedLaplaceTolArgs (name, mismatch))

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

let tuple_index_invalid_index loc ix_max ix =
  TypeError (loc, TypeError.TupleIndexInvalidIndex (ix_max, ix))

let tuple_index_not_tuple loc ut =
  TypeError (loc, TypeError.TupleIndexNotTuple ut)

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
let empty_tuple loc = ExpressionError (loc, ExpressionError.EmptyTuple)
let bad_int_literal loc = ExpressionError (loc, ExpressionError.IntTooLarge)

let cannot_assign_to_read_only loc name =
  StatementError (loc, StatementError.CannotAssignToReadOnly name)

let cannot_assign_to_global loc name =
  StatementError (loc, StatementError.CannotAssignToGlobal name)

let cannot_assign_function loc name ut =
  StatementError (loc, StatementError.CannotAssignFunction (name, ut))

let cannot_assign_to_multiindex loc =
  StatementError (loc, StatementError.LValueMultiIndexing)

let cannot_assign_duplicate_unpacking loc names =
  StatementError (loc, StatementError.LValueTupleUnpackDuplicates names)

let cannot_access_assigning_var loc names =
  StatementError (loc, StatementError.LValueTupleReadAndWrite names)

let invalid_tilde_pdf_or_pmf loc =
  StatementError (loc, StatementError.InvalidTildePDForPMF)

let invalid_tilde_cdf_or_ccdf loc name =
  StatementError (loc, StatementError.InvalidTildeCDForCCDF name)

let invalid_tilde_no_such_dist loc name is_int =
  StatementError
    (loc, StatementError.InvalidTildeNoSuchDistribution (name, is_int))

let target_plusequals_outside_model_or_logprob loc =
  StatementError (loc, StatementError.TargetPlusEqualsOutsideModelOrLogProb)

let jacobian_plusequals_not_allowed loc =
  StatementError (loc, StatementError.JacobianPlusEqualsNotAllowed)

let invalid_truncation_cdf_or_ccdf loc args =
  StatementError (loc, StatementError.InvalidTruncationCDForCCDF args)

let break_outside_loop loc =
  StatementError (loc, StatementError.BreakOutsideLoop)

let continue_outside_loop loc =
  StatementError (loc, StatementError.ContinueOutsideLoop)

let expression_return_outside_returning_fn loc =
  StatementError (loc, StatementError.ExpressionReturnOutsideReturningFn)

let void_outside_nonreturning_fn loc =
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

let fn_decl_without_def loc name =
  StatementError (loc, StatementError.FunDeclNoDefn name)

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
