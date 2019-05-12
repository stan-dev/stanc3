(** Semantic validation of AST*)

(* Idea: check many of things related to identifiers that are hard to check
   during parsing and are in fact irrelevant for building up the parse tree *)

open Core_kernel
open Symbol_table
open Ast
open Stan_math_signatures
open Errors
open Type_conversion
open Pretty_printing

(* There is a semantic checking function for each AST node that calls
   the checking functions for its children left to right. *)

module Validate = Middle.Validation.Make (Semantic_error)

(* Top level function semantic_check_program declares the AST while operating
   on (1) a global symbol table vm, and (2) structure of type context_flags_record
   to communicate information down the AST. *)

let ternary_if = "TernaryIf__"

let%test "bad op name" = phys_equal (Middle.operator_of_string "Pluss__") None
let%test "good op name" = Middle.operator_of_string "Plus__" = Some Plus

(** A hash table to hold some name conversions between the AST nodes and the
    Stan Math name of the operator *)
let string_of_operators =
  Map.Poly.of_alist_multi
    [ (Middle.string_of_operator Middle.Plus, "add")
    ; (Middle.string_of_operator PPlus, "plus")
    ; (Middle.string_of_operator Minus, "subtract")
    ; (Middle.string_of_operator PMinus, "minus")
    ; (Middle.string_of_operator Times, "multiply")
    ; (Middle.string_of_operator Divide, "mdivide_right")
    ; (Middle.string_of_operator Divide, "divide")
    ; (Middle.string_of_operator Modulo, "modulus")
    ; (Middle.string_of_operator LDivide, "mdivide_left")
    ; (Middle.string_of_operator EltTimes, "elt_multiply")
    ; (Middle.string_of_operator EltDivide, "elt_divide")
    ; (Middle.string_of_operator Pow, "pow")
    ; (Middle.string_of_operator Or, "logical_or")
    ; (Middle.string_of_operator And, "logical_and")
    ; (Middle.string_of_operator Equals, "logical_eq")
    ; (Middle.string_of_operator NEquals, "logical_neq")
    ; (Middle.string_of_operator Less, "logical_lt")
    ; (Middle.string_of_operator Leq, "logical_lte")
    ; (Middle.string_of_operator Greater, "logical_gt")
    ; (Middle.string_of_operator Geq, "logical_gte")
    ; (Middle.string_of_operator PNot, "logical_negation")
    ; (Middle.string_of_operator Transpose, "transpose")
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

(** Querying stan_math_signatures for operator signatures by string name *)
let operator_return_type_from_string op_name argtypes =
  if op_name = "Assign" || op_name = "ArrowAssign" then
    match List.map ~f:snd argtypes with
    | [ut1; ut2] when check_of_same_type_mod_array_conv "" ut1 ut2 ->
        Some Middle.Void
    | _ -> None
  else
    Map.Poly.find_multi string_of_operators op_name
    |> List.find_map ~f:(fun name ->
           Stan_math_signatures.stan_math_returntype name argtypes )

let operator_return_type op =
  operator_return_type_from_string (Middle.string_of_operator op)

(** Origin blocks, to keep track of where variables are declared *)
type originblock =
  | MathLibrary
  | Functions
  | Data
  | TData
  | Param
  | TParam
  | Model
  | GQuant

(** Print all the signatures of a stan math operator, for the purposes of error messages. *)
let check_that_all_functions_have_definition = ref true

let model_name = ref ""
let vm = Symbol_table.initialize ()

(* Record structure holding flags and other markers about context to be
   used for error reporting. *)
type context_flags_record =
  { current_block: originblock
  ; in_fun_def: bool
  ; in_returning_fun_def: bool
  ; in_rng_fun_def: bool
  ; in_lp_fun_def: bool
  ; loop_depth: int }

(* Some helper functions *)
let dup_exists l =
  match List.find_a_dup ~compare:String.compare l with
  | Some _ -> true
  | None -> false

let type_of_expr_typed ue = ue.emeta.type_

let rec unsizedtype_contains_int ut =
  match ut with
  | Middle.UInt -> true
  | UArray ut -> unsizedtype_contains_int ut
  | _ -> false

let rec unsizedtype_of_sizedtype = function
  | Middle.SInt -> Middle.UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix (_, _) -> UMatrix
  | SArray (st, _) -> UArray (unsizedtype_of_sizedtype st)

let rec lub_ad_type = function
  | [] -> Middle.DataOnly
  | x :: xs ->
      let y = lub_ad_type xs in
      if Middle.compare_autodifftype x y < 0 then y else x

let calculate_autodifftype at ut =
  match at with
  | (Param | TParam | Model) when not (unsizedtype_contains_int ut) ->
      Middle.AutoDiffable
  | _ -> DataOnly

let has_int_type ue = ue.emeta.type_ = UInt
let has_int_array_type ue = ue.emeta.type_ = UArray UInt

let has_int_or_real_type ue =
  match ue.emeta.type_ with UInt | UReal -> true | _ -> false

let probability_distribution_name_variants id =
  let name = id.name in
  let open String in
  let open Pervasives in
  List.map
    ~f:(fun n -> {name= n; id_loc= id.id_loc})
    ( if name = "multiply_log" || name = "binomial_coefficient_log" then [name]
    else if is_suffix ~suffix:"_lpmf" name then
      [name; drop_suffix name 5 ^ "_lpdf"; drop_suffix name 5 ^ "_log"]
    else if is_suffix ~suffix:"_lpdf" name then
      [name; drop_suffix name 5 ^ "_lpmf"; drop_suffix name 5 ^ "_log"]
    else if is_suffix ~suffix:"_lcdf" name then
      [name; drop_suffix name 5 ^ "_cdf_log"]
    else if is_suffix ~suffix:"_lccdf" name then
      [name; drop_suffix name 6 ^ "_ccdf_log"]
    else if is_suffix ~suffix:"_cdf_log" name then
      [name; drop_suffix name 8 ^ "_lcdf"]
    else if is_suffix ~suffix:"_ccdf_log" name then
      [name; drop_suffix name 9 ^ "_lccdf"]
    else if is_suffix ~suffix:"_log" name then
      [name; drop_suffix name 4 ^ "_lpmf"; drop_suffix name 4 ^ "_lpdf"]
    else [name] )

let lub_rt loc rt1 rt2 =
  match (rt1, rt2) with
  | Middle.ReturnType UReal, Middle.ReturnType UInt
   |ReturnType UInt, ReturnType UReal ->
      Validate.ok (Middle.ReturnType UReal)
  | _, _ when rt1 = rt2 -> Validate.ok rt2
  | _ -> Semantic_error.mismatched_return_types loc rt1 rt2 |> Validate.error



let try_compute_block_statement_returntype loc srt1 srt2 =
  match (srt1, srt2) with
  | Complete rt1, Complete rt2 | Incomplete rt1, Complete rt2 ->
      lub_rt loc rt1 rt2 |> Validate.map ~f:(fun t -> Complete t)
  | Incomplete rt1, Incomplete rt2 | Complete rt1, Incomplete rt2 ->
      lub_rt loc rt1 rt2 |> Validate.map ~f:(fun t -> Incomplete t)
  | NoReturnType, NoReturnType -> Validate.ok NoReturnType
  | AnyReturnType, Incomplete rt
   |Complete rt, NoReturnType
   |NoReturnType, Incomplete rt
   |Incomplete rt, NoReturnType ->
      Validate.ok @@ Incomplete rt
  | NoReturnType, Complete rt
   |Complete rt, AnyReturnType
   |Incomplete rt, AnyReturnType
   |AnyReturnType, Complete rt ->
      Validate.ok @@ Complete rt
  | AnyReturnType, NoReturnType
   |NoReturnType, AnyReturnType
   |AnyReturnType, AnyReturnType ->
      Validate.ok AnyReturnType

let check_fresh_variable_basic id is_nullary_function =
  (* No shadowing! *)
  (* For some strange reason, Stan allows user declared identifiers that are
     not of nullary function types to clash with nullary library functions.
     No other name clashes are tolerated. Here's the logic to
     achieve that. *)
  if
    is_stan_math_function_name id.name
    && (is_nullary_function || stan_math_returntype id.name [] = None)
  then
    Semantic_error.ident_is_stanmath_name id.id_loc id.name |> Validate.error
  else
    match Symbol_table.look vm id.name with
    | Some _ -> Semantic_error.ident_in_use id.id_loc id.name |> Validate.error
    | None -> Validate.ok ()

let check_fresh_variable id is_nullary_function =
  List.fold ~init:(Validate.ok ())
    ~f:(fun v0 name ->
      check_fresh_variable_basic name is_nullary_function
      |> Validate.apply_const v0 )
    (probability_distribution_name_variants id)

(** Least upper bound of expression autodiff types *)
let lub_ad_e exprs =
  exprs |> List.map ~f:(fun x -> x.emeta.ad_level) |> lub_ad_type

(* == SEMANTIC CHECK OF PROGRAM ELEMENTS ==================================== *)

(* Probably nothing to do here *)
let semantic_check_assignmentoperator op = Validate.ok op

(* Probably nothing to do here *)
let semantic_check_autodifftype at = Validate.ok at

(* Probably nothing to do here *)
let rec semantic_check_unsizedtype : Middle.unsizedtype -> unit Validate.t =
  function
  | Middle.UFun (l, rt) ->
      (* fold over argument types accumulating errors with initial state 
          given by validating the return type *)
      List.fold
        ~f:(fun v0 (at, ut) ->
          Validate.(
            apply_const
              (apply_const v0 (semantic_check_autodifftype at))
              (semantic_check_unsizedtype ut)) )
        ~init:(semantic_check_returntype rt)
        l
  | Middle.UArray ut -> semantic_check_unsizedtype ut
  | _ -> Validate.ok ()

and semantic_check_returntype : Middle.returntype -> unit Validate.t = function
  | Middle.Void -> Validate.ok ()
  | ReturnType ut -> semantic_check_unsizedtype ut

let semantic_error_e ({emeta; _} : Ast.typed_expression) msg =
  semantic_error ~loc:emeta.loc msg

(* -- Indentifiers ---------------------------------------------------------- *)
let reserved_keywords =
  [ "true"; "false"; "repeat"; "until"; "then"; "var"; "fvar"; "STAN_MAJOR"
  ; "STAN_MINOR"; "STAN_PATCH"; "STAN_MATH_MAJOR"; "STAN_MATH_MINOR"
  ; "STAN_MATH_PATCH"; "alignas"; "alignof"; "and"; "and_eq"; "asm"; "auto"
  ; "bitand"; "bitor"; "bool"; "break"; "case"; "catch"; "char"; "char16_t"
  ; "char32_t"; "class"; "compl"; "const"; "constexpr"; "const_cast"
  ; "continue"; "decltype"; "default"; "delete"; "do"; "double"; "dynamic_cast"
  ; "else"; "enum"; "explicit"; "export"; "extern"; "false"; "float"; "for"
  ; "friend"; "goto"; "if"; "inline"; "int"; "long"; "mutable"; "namespace"
  ; "new"; "noexcept"; "not"; "not_eq"; "nullptr"; "operator"; "or"; "or_eq"
  ; "private"; "protected"; "public"; "register"; "reinterpret_cast"; "return"
  ; "short"; "signed"; "sizeof"; "static"; "static_assert"; "static_cast"
  ; "struct"; "switch"; "template"; "this"; "thread_local"; "throw"; "true"
  ; "try"; "typedef"; "typeid"; "typename"; "union"; "unsigned"; "using"
  ; "virtual"; "void"; "volatile"; "wchar_t"; "while"; "xor"; "xor_eq" ]

let semantic_check_identifier id =
  if id.name = !model_name then
    Semantic_error.ident_is_model_name id.id_loc id.name |> Validate.error
  else if
    String.is_suffix id.name ~suffix:"__"
    || List.exists ~f:(fun str -> str = id.name) reserved_keywords
  then Semantic_error.ident_is_keyword id.id_loc id.name |> Validate.error
  else Validate.ok ()

(* -- Operators ------------------------------------------------------------- *)
let semantic_check_operator i = Validate.ok ()

(* == Expressions =========================================================== *)

let arg_type x = (x.emeta.ad_level, x.emeta.type_)
let get_arg_types = List.map ~f:arg_type

(* -- Function application -------------------------------------------------- *)

let semantic_check_fn_map_rect ~loc id es =
  match (id.name, es) with
  | "map_rect", {expr= Variable arg1; _} :: _
    when String.(
           is_suffix arg1.name ~suffix:"_lp"
           || is_suffix arg1.name ~suffix:"_rng") ->
      Semantic_error.invalid_map_rect_fn loc arg1.name |> Validate.error
  | _ -> Validate.ok ()

let semantic_check_fn_conditioning ~loc id =
  if
    List.exists ["_lpdf"; "_lpmf"; "_lcdf"; "_lccdf"] ~f:(fun x ->
        String.is_suffix id.name ~suffix:x )
  then Semantic_error.conditioning_required loc |> Validate.error
  else Validate.ok ()

(** `Target+=` can only be used in model and functions
    with right suffix (same for tilde etc)
*)
let semantic_check_fn_target_plus_equals cf ~loc id =
  if
    String.is_suffix id.name ~suffix:"_lp"
    && not (cf.in_lp_fun_def || cf.current_block = Model)
  then
    Semantic_error.target_plusequals_outisde_model_or_logprob loc
    |> Validate.error
  else Validate.ok ()

(** Rng functions cannot be used in Tp or Model and only
    in funciton defs with the right suffix
*)
let semantic_check_fn_rng cf ~loc id =
  if
    String.is_suffix id.name ~suffix:"_rng"
    && ( (cf.in_fun_def && not cf.in_rng_fun_def)
       || cf.current_block = TParam || cf.current_block = Model )
  then Semantic_error.invalid_rng_fn loc |> Validate.error
  else Validate.ok ()

(* Regular function application *)
let semantic_check_fn_normal ~loc id es =
  match Symbol_table.look vm id.name with
  | Some (_, Middle.UFun (_, Void)) ->
      Semantic_error.returning_fn_expected_nonreturning_found loc id.name
      |> Validate.error
  | Some (_, UFun (listed_tys, rt))
    when not
           (check_compatible_arguments_mod_conv id.name listed_tys
              (get_arg_types es)) ->
      es
      |> List.map ~f:(fun e -> e.emeta.type_)
      |> Semantic_error.illtyped_userdefined_fn_app loc id.name listed_tys rt
      |> Validate.error
  | Some (_, UFun (_, ReturnType ut)) ->
      mk_typed_expression
        ~expr:(FunApp (UserDefined, id, es))
        ~ad_level:(lub_ad_e es) ~type_:ut ~loc
      |> Validate.ok
  | Some _ ->
      (* Check that Funaps are actually functions *)
      Semantic_error.returning_fn_expected_nonreturning_found loc id.name
      |> Validate.error
  | None ->
      Semantic_error.returning_fn_expected_undeclaredident_found loc id.name
      |> Validate.error

(* Stan-Math function application *)
let semantic_check_fn_stan_math ~loc id es =
  match stan_math_returntype id.name (get_arg_types es) with
  | Some Void ->
      Semantic_error.returning_fn_expected_nonreturning_found loc id.name
      |> Validate.error
  | Some (ReturnType ut) ->
      mk_typed_expression
        ~expr:(FunApp (StanLib, id, es))
        ~ad_level:(lub_ad_e es) ~type_:ut ~loc
      |> Validate.ok
  | _ ->
      es
      |> List.map ~f:(fun e -> e.emeta.type_)
      |> Semantic_error.illtyped_stanlib_fn_app loc id.name
      |> Validate.error

let fn_kind_from_identifier id =
  if is_stan_math_function_name id.name then StanLib else UserDefined

(** Determines the function kind based on the identifier and performs the
    corresponding semantic check
*)
let semantic_check_fn ~loc id es =
  match fn_kind_from_identifier id with
  | StanLib -> semantic_check_fn_stan_math ~loc id es
  | UserDefined -> semantic_check_fn_normal ~loc id es

(* -- Ternary If ------------------------------------------------------------ *)

let semantic_check_ternary_if loc (pe, te, fe) :
    Ast.typed_expression Validate.t =
  match
    operator_return_type_from_string ternary_if (get_arg_types [pe; te; fe])
  with
  | Some (Middle.ReturnType ut) ->
      mk_typed_expression
        ~expr:(TernaryIf (pe, te, fe))
        ~ad_level:(lub_ad_e [pe; te; fe])
        ~type_:ut ~loc
      |> Validate.ok
  | Some Middle.Void | None ->
      Semantic_error.illtyped_ternary_if loc pe.emeta.type_ te.emeta.type_
        fe.emeta.type_
      |> Validate.error

(* -- Binary (Infix) Operators ---------------------------------------------- *)

let semantic_check_binop loc op (le, re) =
  match operator_return_type op (get_arg_types [le; re]) with
  | Some (Middle.ReturnType ut) ->
      mk_typed_expression
        ~expr:(BinOp (le, op, re))
        ~ad_level:(lub_ad_e [le; re])
        ~type_:ut ~loc
      |> Validate.ok
  | Some Middle.Void | None ->
      Semantic_error.illtyped_binary_op loc op le.emeta.type_ re.emeta.type_
      |> Validate.error

(* -- Prefix Operators ------------------------------------------------------ *)

let semantic_check_prefixop loc op e =
  match operator_return_type op (get_arg_types [e]) with
  | Some (Middle.ReturnType ut) ->
      mk_typed_expression
        ~expr:(PrefixOp (op, e))
        ~ad_level:(lub_ad_e [e])
        ~type_:ut ~loc
      |> Validate.ok
  | Some Middle.Void | None ->
      Semantic_error.illtyped_prefix_op loc op e.emeta.type_ |> Validate.error

(* -- Postfix operators ----------------------------------------------------- *)

let semantic_check_postfixop loc op e =
  match operator_return_type op (get_arg_types [e]) with
  | Some (Middle.ReturnType ut) ->
      mk_typed_expression
        ~expr:(PostfixOp (e, op))
        ~ad_level:(lub_ad_e [e])
        ~type_:ut ~loc
      |> Validate.ok
  | Some Middle.Void | None ->
      Semantic_error.illtyped_postfix_op loc op e.emeta.type_ |> Validate.error

(* -- Variables ------------------------------------------------------------- *)
let semantic_check_variable loc id =
  match Symbol_table.look vm id.name with
  | None when not (is_stan_math_function_name id.name) ->
      Semantic_error.ident_not_in_scope loc id.name |> Validate.error
  | None ->
      mk_typed_expression ~expr:(Variable id)
        ~ad_level:
          (calculate_autodifftype MathLibrary Middle.UMathLibraryFunction)
        ~type_:Middle.UMathLibraryFunction ~loc
      |> Validate.ok
  | Some (originblock, type_) ->
      mk_typed_expression ~expr:(Variable id)
        ~ad_level:(calculate_autodifftype originblock type_)
        ~type_ ~loc
      |> Validate.ok

(* -- Conditioned Distribution Application ---------------------------------- *)

let semantic_check_conddist_name loc id =
  if
    List.exists
      ~f:(fun x -> String.is_suffix id.name ~suffix:x)
      ["_lpdf"; "_lpmf"; "_lcdf"; "_lccdf"]
  then Validate.ok ()
  else Semantic_error.conditional_notation_not_allowed loc |> Validate.error

let semantic_check_target_pe loc cf id =
  if
    String.is_suffix id.name ~suffix:"_lp"
    && not (cf.in_lp_fun_def || cf.current_block = Model)
  then
    Semantic_error.target_plusequals_outisde_model_or_logprob loc
    |> Validate.error
  else Validate.ok ()

let semantic_check_cond_dist_app_rt_stanmath_fn ~loc ~returnblock id es =
  function
  | Middle.Void ->
      Semantic_error.returning_fn_expected_nonreturning_found loc id.name
      |> Validate.error
  | ReturnType ut ->
      mk_typed_expression
        ~expr:(CondDistApp (id, es))
        ~ad_level:returnblock ~type_:ut ~loc
      |> Validate.ok

let semantic_check_cond_dist_nonrt_stanmath_fn ~loc id es =
  if is_stan_math_function_name id.name then
    es
    |> List.map ~f:type_of_expr_typed
    |> Semantic_error.illtyped_stanlib_fn_app loc id.name
    |> Validate.error
  else Validate.ok ()

(* Check that function arguments match signature  *)
(* Also check whether function arguments meet data requirement. *)
let semantic_check_cond_dist_normal_fn ~loc ~returnblock id es =
  match Symbol_table.look vm id.name with
  | Some (_, UFun (_, Void)) ->
      Semantic_error.returning_fn_expected_nonreturning_found loc id.name
      |> Validate.error
  | Some (_, UFun (listedtypes, rt))
    when not
           (check_compatible_arguments_mod_conv id.name listedtypes
              (get_arg_types es)) ->
      es
      |> List.map ~f:type_of_expr_typed
      |> Semantic_error.illtyped_userdefined_fn_app loc id.name listedtypes rt
      |> Validate.error
  | Some (_, UFun (listedtypes, ReturnType ut)) ->
      mk_typed_expression
        ~expr:(CondDistApp (id, es))
        ~ad_level:returnblock ~type_:ut ~loc
      |> Validate.ok
  | Some _ ->
      (* Check that Funaps are actually functions *)
      Semantic_error.returning_fn_expected_nonfn_found loc id.name
      |> Validate.error
  | None ->
      Semantic_error.returning_fn_expected_undeclaredident_found loc id.name
      |> Validate.error

let semantic_check_cond_dist_app ~loc ~returnblock id es =
  match stan_math_returntype id.name (get_arg_types es) with
  | Some rt ->
      semantic_check_cond_dist_app_rt_stanmath_fn ~loc ~returnblock id es rt
  | _ ->
      semantic_check_cond_dist_normal_fn ~loc ~returnblock id es
      |> Validate.apply_const
           (semantic_check_cond_dist_nonrt_stanmath_fn ~loc id es)

(* -- Array Expressions ----------------------------------------------------- *)

(* Array expressions must be of uniform type. (Or mix of int and real) *)
let semantic_check_array_expr_type ~loc es =
  match es with
  | next :: _ ->
      let ty = next.emeta.type_ in
      if
        List.exists
          ~f:(fun x ->
            not
              ( check_of_same_type_mod_array_conv "" x.emeta.type_ ty
              || check_of_same_type_mod_array_conv "" ty x.emeta.type_ ) )
          es
      then Semantic_error.mismatched_array_types loc |> Validate.error
      else Validate.ok ()
  | _ -> Semantic_error.empty_array loc |> Validate.error

let semantic_check_array_expr ~loc es =
  match List.map ~f:type_of_expr_typed es with
  | [] -> Semantic_error.empty_array loc |> Validate.error
  | ty :: _ as elementtypes ->
      let type_ =
        if List.exists ~f:(fun x -> ty <> x) elementtypes then
          Middle.UArray UReal
        else UArray ty
      and ad_level = lub_ad_e es in
      mk_typed_expression ~expr:(ArrayExpr es) ~ad_level ~type_ ~loc
      |> Validate.ok

(* -- Row Vector Expresssion ------------------------------------------------ *)

let semantic_check_rowvector ~loc es =
  let elementtypes = List.map ~f:(fun y -> y.emeta.type_) es
  and ad_level = lub_ad_e es in
  if List.for_all ~f:(fun x -> x = UReal || x = UInt) elementtypes then
    mk_typed_expression ~expr:(RowVectorExpr es) ~ad_level
      ~type_:Middle.URowVector ~loc
    |> Validate.ok
  else if List.for_all ~f:(fun x -> x = URowVector) elementtypes then
    mk_typed_expression ~expr:(RowVectorExpr es) ~ad_level
      ~type_:Middle.UMatrix ~loc
    |> Validate.ok
  else Semantic_error.invalid_row_vector_types loc |> Validate.error

(* -- Indexed Expressions --------------------------------------------------- *)
let compose f g x = f @@ g x
let tuple2 a b = (a, b)
let tuple3 a b c = (a, b, c)

let inferred_unsizedtype_of_indexed ~loc ut typed_idxs =
  let rec aux k ut xs =
    match (ut, xs) with
    | Middle.UMatrix, [(All, _); (Single _, Middle.UInt)]
     |UMatrix, [(Upfrom _, _); (Single _, UInt)]
     |UMatrix, [(Downfrom _, _); (Single _, UInt)]
     |UMatrix, [(Between _, _); (Single _, UInt)]
     |UMatrix, [(Single _, UArray UInt); (Single _, UInt)] ->
        k @@ Validate.ok Middle.UVector
    | _, [] -> k @@ Validate.ok ut
    | _, next :: rest -> (
      match next with
      | Single _, UInt -> (
        match ut with
        | Middle.UArray inner_ty -> aux k inner_ty rest
        | UVector | URowVector -> aux k UReal rest
        | UMatrix -> aux k URowVector rest
        | _ -> Semantic_error.not_indexable loc ut |> Validate.error )
      | _ -> (
        match ut with
        | Middle.UArray inner_ty ->
            let k' = compose k (Validate.map ~f:(fun t -> Middle.UArray t)) in
            aux k' inner_ty rest
        | UVector | URowVector | UMatrix -> aux k ut rest
        | _ -> Semantic_error.not_indexable loc ut |> Validate.error ) )
  in
  aux (fun x -> x) ut typed_idxs

let inferred_ad_type_of_indexed at uindices =
  lub_ad_type
    ( at
    :: List.map
         ~f:(function
           | All -> Middle.DataOnly
           | Single ue1 | Upfrom ue1 | Downfrom ue1 ->
               lub_ad_type [at; ue1.emeta.ad_level]
           | Between (ue1, ue2) ->
               lub_ad_type [at; ue1.emeta.ad_level; ue2.emeta.ad_level])
         uindices )

let index_with_type idx =
  match idx with Single e -> (idx, e.emeta.type_) | _ -> (idx, Middle.UInt)

let rec semantic_check_indexed ~loc ~cf e indices = Validate.(
    indices
    |> List.map ~f:(semantic_check_index cf)
    |> sequence
    |> liftA2 tuple2 (semantic_check_expression cf e)
    >>= fun (ue, uindices) ->
    let at = inferred_ad_type_of_indexed ue.emeta.ad_level uindices in
    uindices
    |> List.map ~f:index_with_type
    |> inferred_unsizedtype_of_indexed ~loc ue.emeta.type_
    |> map ~f:(fun ut -> 
            
            
            mk_typed_expression
              ~expr:(Indexed (ue, uindices))
              ~ad_level:at ~type_:ut ~loc)
)

and semantic_check_index cf idx =
  match idx with
  | All -> Validate.ok All
  (* Check that indexes have int (container) type *)
  | Single e ->
      Validate.(
        semantic_check_expression cf e
        >>= fun ue ->
        if has_int_type ue || has_int_array_type ue then ok @@ Single ue
        else
          Semantic_error.int_intarray_or_range_expected ue.emeta.loc
            ue.emeta.type_
          |> error)
  | Upfrom e ->
      semantic_check_expression_of_int_type cf e "Range bound"
      |> Validate.map ~f:(fun e -> Upfrom e)
  | Downfrom e ->
      semantic_check_expression_of_int_type cf e "Range bound"
      |> Validate.map ~f:(fun e -> Downfrom e)
  | Between (e1, e2) ->
      let le = semantic_check_expression_of_int_type cf e1 "Range bound"
      and ue = semantic_check_expression_of_int_type cf e2 "Range bound" in
      Validate.liftA2 (fun l u -> Between (l, u)) le ue

(* -- Top-level expressions ------------------------------------------------- *)
and semantic_check_expression cf ({emeta; expr} : Ast.untyped_expression) :
    Ast.typed_expression Validate.t =
  match expr with
  | TernaryIf (e1, e2, e3) ->
      let pe = semantic_check_expression cf e1
      and te = semantic_check_expression cf e2
      and fe = semantic_check_expression cf e3 in
      Validate.(liftA3 tuple3 pe te fe >>= semantic_check_ternary_if emeta.loc)
  | BinOp (e1, op, e2) ->
      let le = semantic_check_expression cf e1
      and re = semantic_check_expression cf e2 in
      Validate.(
        liftA2 tuple2 le re
        |> apply_const (semantic_check_operator op)
        >>= semantic_check_binop emeta.loc op)
  | PrefixOp (op, e) ->
      Validate.(
        semantic_check_expression cf e
        |> apply_const (semantic_check_operator op)
        >>= semantic_check_prefixop emeta.loc op)
  | PostfixOp (e, op) ->
      Validate.(
        semantic_check_expression cf e
        |> apply_const (semantic_check_operator op)
        >>= semantic_check_prefixop emeta.loc op)
  | Variable id ->
      semantic_check_variable emeta.loc id
      |> Validate.apply_const (semantic_check_identifier id)
  | IntNumeral s ->
      mk_typed_expression ~expr:(IntNumeral s) ~ad_level:DataOnly ~type_:UInt
        ~loc:emeta.loc
      |> Validate.ok
  | RealNumeral s ->
      mk_typed_expression ~expr:(RealNumeral s) ~ad_level:DataOnly ~type_:UReal
        ~loc:emeta.loc
      |> Validate.ok
  | FunApp (_, id, es) ->
      Validate.(
        es
        |> List.map ~f:(semantic_check_expression cf)
        |> sequence
        >>= fun ues ->
        semantic_check_fn ~loc:emeta.loc id ues
        |> apply_const (semantic_check_identifier id)
        |> apply_const (semantic_check_fn_map_rect ~loc:emeta.loc id ues)
        |> apply_const (semantic_check_fn_conditioning ~loc:emeta.loc id)
        |> apply_const
             (semantic_check_fn_target_plus_equals cf ~loc:emeta.loc id)
        |> apply_const (semantic_check_fn_rng cf ~loc:emeta.loc id))
  | CondDistApp (id, es) ->
      Validate.(
        es
        |> List.map ~f:(semantic_check_expression cf)
        |> sequence
        >>= fun ues ->
        let returnblock = lub_ad_e ues in
        semantic_check_cond_dist_app ~loc:emeta.loc ~returnblock id ues
        |> apply_const (semantic_check_identifier id))
  | GetLP ->
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      if
        not
          ( cf.in_lp_fun_def || cf.current_block = Model
          || cf.current_block = TParam )
      then
        Semantic_error.target_plusequals_outisde_model_or_logprob emeta.loc
        |> Validate.error
      else
        mk_typed_expression ~expr:GetLP
          ~ad_level:(calculate_autodifftype cf.current_block UReal)
          ~type_:UReal ~loc:emeta.loc
        |> Validate.ok
  | GetTarget ->
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      if
        not
          ( cf.in_lp_fun_def || cf.current_block = Model
          || cf.current_block = TParam )
      then
        Semantic_error.target_plusequals_outisde_model_or_logprob emeta.loc
        |> Validate.error
      else
        mk_typed_expression ~expr:GetTarget
          ~ad_level:(calculate_autodifftype cf.current_block UReal)
          ~type_:UReal ~loc:emeta.loc
        |> Validate.ok
  | ArrayExpr es ->
      Validate.(
        es
        |> List.map ~f:(semantic_check_expression cf)
        |> sequence
        >>= fun ues ->
        semantic_check_array_expr ~loc:emeta.loc ues
        |> apply_const (semantic_check_array_expr_type ~loc:emeta.loc ues))
  | RowVectorExpr es ->
      Validate.(
        es
        |> List.map ~f:(semantic_check_expression cf)
        |> sequence
        >>= semantic_check_rowvector ~loc:emeta.loc)
  | Paren e ->
      semantic_check_expression cf e
      |> Validate.map ~f:(fun ue ->
             mk_typed_expression ~expr:(Paren ue) ~ad_level:ue.emeta.ad_level
               ~type_:ue.emeta.type_ ~loc:emeta.loc )
  | Indexed (e, indices) ->
      semantic_check_indexed ~loc:emeta.loc ~cf e indices
      
and semantic_check_expression_of_int_type cf e name =
  Validate.(
    semantic_check_expression cf e
    >>= fun ue ->
    if has_int_type ue then ok ue
    else Semantic_error.int_expected ue.emeta.loc name ue.emeta.type_ |> error)

and semantic_check_expression_of_int_or_real_type cf e name =
  Validate.(
    semantic_check_expression cf e
    >>= fun ue ->
    if has_int_or_real_type ue then ok ue
    else
      Semantic_error.int_or_real_expected ue.emeta.loc name ue.emeta.type_
      |> error)


(* -- Sized Types ----------------------------------------------------------- *)
let rec semantic_check_sizedtype cf = function
  | Middle.SInt -> Validate.ok Middle.SInt
  | SReal -> Validate.ok Middle.SReal
  | SVector e ->
      semantic_check_expression_of_int_type cf e "Vector sizes"
      |> Validate.map ~f:(fun ue -> Middle.SVector ue)
  | SRowVector e ->
      semantic_check_expression_of_int_type cf e "Row vector sizes"
      |> Validate.map ~f:(fun ue -> Middle.SRowVector ue)
  | SMatrix (e1, e2) ->
      let ue1 = semantic_check_expression_of_int_type cf e1 "Matrix sizes"
      and ue2 = semantic_check_expression_of_int_type cf e2 "Matrix sizes" in
      Validate.liftA2 (fun ue1 ue2 -> Middle.SMatrix (ue1, ue2)) ue1 ue2
  | SArray (st, e) ->
      let ust = semantic_check_sizedtype cf st
      and ue = semantic_check_expression_of_int_type cf e "Array sizes" in
      Validate.liftA2 (fun ust ue -> Middle.SArray (ust, ue)) ust ue

(* -- Transformations ------------------------------------------------------- *)
let semantic_check_transformation cf = function
  | Identity -> Validate.ok Identity
  | Lower e ->
      semantic_check_expression_of_int_or_real_type cf e "Lower bound"
      |> Validate.map ~f:(fun ue -> Lower ue)
  | Upper e ->
      semantic_check_expression_of_int_or_real_type cf e "Upper bound"
      |> Validate.map ~f:(fun ue -> Upper ue)
  | LowerUpper (e1, e2) ->
      let ue1 =
        semantic_check_expression_of_int_or_real_type cf e1 "Lower bound"
      and ue2 =
        semantic_check_expression_of_int_or_real_type cf e2 "Upper bound"
      in
      Validate.liftA2 (fun ue1 ue2 -> LowerUpper (ue1, ue2)) ue1 ue2
  | Offset e ->
      semantic_check_expression_of_int_or_real_type cf e "Offset"
      |> Validate.map ~f:(fun ue -> Offset ue)
  | Multiplier e ->
      semantic_check_expression_of_int_or_real_type cf e "Multiplier"
      |> Validate.map ~f:(fun ue -> Multiplier ue)
  | OffsetMultiplier (e1, e2) ->
      let ue1 = semantic_check_expression_of_int_or_real_type cf e1 "Offset"
      and ue2 =
        semantic_check_expression_of_int_or_real_type cf e2 "Multiplier"
      in
      Validate.liftA2 (fun ue1 ue2 -> OffsetMultiplier (ue1, ue2)) ue1 ue2
  | Ordered -> Validate.ok Ordered
  | PositiveOrdered -> Validate.ok PositiveOrdered
  | Simplex -> Validate.ok Simplex
  | UnitVector -> Validate.ok UnitVector
  | CholeskyCorr -> Validate.ok CholeskyCorr
  | CholeskyCov -> Validate.ok CholeskyCov
  | Correlation -> Validate.ok Correlation
  | Covariance -> Validate.ok Covariance

(* -- Printables ------------------------------------------------------------ *)

let semantic_check_printable cf = function
  | PString s -> Validate.ok @@ PString s
  (* Print/reject expressions cannot be of function type. *)
  | PExpr e -> (
      Validate.(
        semantic_check_expression cf e
        >>= fun ue ->
        match ue.emeta.type_ with
        | UFun _ | UMathLibraryFunction ->
            Semantic_error.not_printable ue.emeta.loc |> error
        | _ -> ok @@ PExpr ue) )

(* -- Truncations ----------------------------------------------------------- *)

let semantic_check_truncation cf = function
  | NoTruncate -> Validate.ok NoTruncate
  | TruncateUpFrom e ->
      semantic_check_expression_of_int_or_real_type cf e "Truncation bound"
      |> Validate.map ~f:(fun ue -> TruncateUpFrom ue)
  | TruncateDownFrom e ->
      semantic_check_expression_of_int_or_real_type cf e "Truncation bound"
      |> Validate.map ~f:(fun ue -> TruncateDownFrom ue)
  | TruncateBetween (e1, e2) ->
      let ue1 =
        semantic_check_expression_of_int_or_real_type cf e1 "Truncation bound"
      and ue2 =
        semantic_check_expression_of_int_or_real_type cf e2 "Truncation bound"
      in
      Validate.liftA2 (fun ue1 ue2 -> TruncateBetween (ue1, ue2)) ue1 ue2

(* == Statements ============================================================ *)

(* -- Non-returning function application ------------------------------------ *)

let semantic_check_nrfn_target ~loc ~cf id =
  if
    String.is_suffix id.name ~suffix:"_lp"
    && not (cf.in_lp_fun_def || cf.current_block = Model)
  then
    Semantic_error.target_plusequals_outisde_model_or_logprob loc
    |> Validate.error
  else Validate.ok ()

let semantic_check_nrfn_normal ~loc id es =
  match Symbol_table.look vm id.name with
  | Some (_, UFun (listedtypes, Void))
    when check_compatible_arguments_mod_conv id.name listedtypes
           (get_arg_types es) ->
      mk_typed_statement
        ~stmt:(NRFunApp (UserDefined, id, es))
        ~return_type:NoReturnType ~loc
      |> Validate.ok
  | Some (_, UFun (listedtypes, Void)) ->
      es
      |> List.map ~f:type_of_expr_typed
      |> Semantic_error.illtyped_userdefined_fn_app loc id.name listedtypes
           Void
      |> Validate.error
  | Some (_, UFun (_, ReturnType _)) ->
      Semantic_error.nonreturning_fn_expected_returning_found loc id.name
      |> Validate.error
  | Some _ ->
      Semantic_error.nonreturning_fn_expected_nonfn_found loc id.name
      |> Validate.error
  | None ->
      Semantic_error.nonreturning_fn_expected_undeclaredident_found loc id.name
      |> Validate.error

let semantic_check_nrfn_stan_math ~loc id es =
  match stan_math_returntype id.name (get_arg_types es) with
  | Some Void ->
      mk_typed_statement
        ~stmt:(NRFunApp (StanLib, id, es))
        ~return_type:NoReturnType ~loc
      |> Validate.ok
  | Some (ReturnType _) ->
      Semantic_error.nonreturning_fn_expected_returning_found loc id.name
      |> Validate.error
  | None ->
      es
      |> List.map ~f:type_of_expr_typed
      |> Semantic_error.illtyped_stanlib_fn_app loc id.name
      |> Validate.error

let semantic_check_nr_fnkind ~loc id es =
  match fn_kind_from_identifier id with
  | StanLib -> semantic_check_nrfn_stan_math ~loc id es
  | UserDefined -> semantic_check_nrfn_normal ~loc id es

let semantic_check_nr_fn_app ~loc ~cf id es = Validate.(
    es
    |> List.map ~f:(semantic_check_expression cf)
    |> sequence
    |> apply_const (semantic_check_identifier id)
    |> apply_const (semantic_check_nrfn_target ~loc ~cf id)
    >>= semantic_check_nr_fnkind ~loc id
)

(* -- Assignment ------------------------------------------------------------ *)

let semantic_check_assignment_read_only ~loc id = Validate.(
  if Symbol_table.get_read_only vm id.name then
          Semantic_error.cannot_assign_to_read_only loc id.name 
          |> error
  else  ok ()
)

(* Variables from previous blocks are read-only. 
   In particular, data and parameters never assigned to 
*)
let semantic_check_assignment_global ~loc ~cf ~block id = Validate.(
  if (not (Symbol_table.is_global vm id.name)) || block = cf.current_block then 
    ok ()
  else
    Semantic_error.cannot_assign_to_global loc id.name 
    |> error
)
 

let mk_assignment_from_indexed_expr assop lhs rhs = 
  match lhs with 
  | {expr= Indexed ({expr= Variable id; _}, idx); _} -> 
    Assignment
      { assign_identifier= id
      ; assign_indices= idx
      ; assign_op= assop
      ; assign_rhs= rhs }
  | _ -> 
    fatal_error ()

let semantic_check_assignment_operator ~loc assop lhs rhs = Validate.(
  let opname = Sexp.to_string (sexp_of_assignmentoperator assop) in
  match
        operator_return_type_from_string opname (get_arg_types [lhs; rhs])
  with
  | Some Void ->
          mk_typed_statement ~return_type:NoReturnType ~loc
            ~stmt:(mk_assignment_from_indexed_expr assop lhs rhs)
          |> ok 
  (* Check that assignments are type consistent *)
  | None | Some (ReturnType _) ->
      Semantic_error.illtyped_assignment loc assop lhs.emeta.type_ rhs.emeta.type_
      |> error
)
   
(* -- Target plus-equals / Increment log-prob ------------------------------- *)

let semantic_check_target_pe_expr_type ~loc e = 
  match e.emeta.type_ with
  | UFun _ | UMathLibraryFunction ->
      Semantic_error.int_or_real_container_expected loc e.emeta.type_
      |> Validate.error
  | _ -> Validate.ok ()

let semantic_check_target_pe_usage ~loc ~cf =
if (cf.in_lp_fun_def || cf.current_block = Model) then
  Validate.ok () 
else 
    Semantic_error.target_plusequals_outisde_model_or_logprob loc
    |> Validate.error


let semantic_check_target_pe ~loc ~cf e = Validate.(
      semantic_check_expression cf e
      |> apply_const (semantic_check_target_pe_usage ~loc ~cf)
      >>= (fun ue -> 
          semantic_check_target_pe_expr_type ~loc ue
          |> map  ~f:(fun _ -> 
                mk_typed_statement ~stmt:(TargetPE ue) ~return_type:NoReturnType ~loc
          )
      ))

let semantic_check_incr_logprob ~loc ~cf e = Validate.(
      semantic_check_expression cf e
      |> apply_const (semantic_check_target_pe_usage ~loc ~cf)
      >>= (fun ue -> 
          semantic_check_target_pe_expr_type ~loc ue
          |> map  ~f:(fun _ -> 
                mk_typed_statement ~stmt:(IncrementLogProb ue) ~return_type:NoReturnType ~loc
          )
      ))


(* -- Tilde (Sampling notation) --------------------------------------------- *)

(* -- Break ----------------------------------------------------------------- *)
(* Break and continue only occur in loops. *)
let semantic_check_break ~loc ~cf = Validate.(
    if cf.loop_depth = 0 then
      Semantic_error.break_outside_loop loc |> error
    else
      mk_typed_statement ~stmt:Break ~return_type:NoReturnType ~loc
      |> ok
)

(* -- Continue -------------------------------------------------------------- *)

let semantic_check_continue ~loc ~cf = Validate.(
      (* Break and continue only occur in loops. *)
      if cf.loop_depth = 0 then
        Semantic_error.continue_outside_loop loc |> error
      else
        mk_typed_statement ~stmt:Continue ~return_type:NoReturnType ~loc
        |> ok
)
        
(* -- Return ---------------------------------------------------------------- *)

(** No returns outside of function definitions 
    In case of void function, no return statements anywhere 
*)
let semantic_check_return ~loc ~cf e = Validate.(
  if not cf.in_returning_fun_def then
    Semantic_error.expression_return_outside_returning_fn loc
    |> error
  else
    semantic_check_expression cf e
    |> map 
      ~f:(fun ue ->
            mk_typed_statement ~stmt:(Return ue)
              ~return_type:(Complete (ReturnType ue.emeta.type_)) ~loc 
      )
  )


(* -- Return `void` --------------------------------------------------------- *)

let semantic_check_returnvoid ~loc ~cf  = Validate.(
    if (not cf.in_fun_def) || cf.in_returning_fun_def then
        Semantic_error.void_ouside_nonreturning_fn loc |> error
      else
        mk_typed_statement ~stmt:ReturnVoid ~return_type:(Complete Void) ~loc
        |> ok
)
  

(* -- Print ----------------------------------------------------------------- *)

let semantic_check_print ~loc ~cf ps = Validate.(
    ps
      |> List.map ~f:(semantic_check_printable cf)
      |> sequence
      |> map ~f:(fun ups ->
             mk_typed_statement ~stmt:(Print ups) ~return_type:NoReturnType
               ~loc )
)

(* -- Reject ---------------------------------------------------------------- *)

let semantic_check_reject ~loc ~cf ps = Validate.(
  ps
  |> List.map ~f:(semantic_check_printable cf)
  |> sequence
  |> map ~f:(fun ups ->
          mk_typed_statement ~stmt:(Reject ups) ~return_type:AnyReturnType
            ~loc )
)

(* -- Skip ------------------------------------------------------------------ *)

let semantic_check_skip ~loc = 
  mk_typed_statement ~stmt:Skip ~return_type:NoReturnType ~loc
  |> Validate.ok 

(* -- If-Then-Else ---------------------------------------------------------- *)

let try_compute_ifthenelse_statement_returntype loc srt1 srt2 =
  match (srt1, srt2) with
  | Complete rt1, Complete rt2 ->
      lub_rt loc rt1 rt2 |> Validate.map ~f:(fun t -> Complete t)
  | Incomplete rt1, Incomplete rt2
   |Complete rt1, Incomplete rt2
   |Incomplete rt1, Complete rt2 ->
      lub_rt loc rt1 rt2 |> Validate.map ~f:(fun t -> Incomplete t)
  | AnyReturnType, NoReturnType
   |NoReturnType, AnyReturnType
   |NoReturnType, NoReturnType ->
      Validate.ok NoReturnType
  | AnyReturnType, Incomplete rt
   |Incomplete rt, AnyReturnType
   |Complete rt, NoReturnType
   |NoReturnType, Complete rt
   |NoReturnType, Incomplete rt
   |Incomplete rt, NoReturnType ->
      Validate.ok @@ Incomplete rt
  | Complete rt, AnyReturnType | AnyReturnType, Complete rt ->
      Validate.ok @@ Complete rt
  | AnyReturnType, AnyReturnType -> Validate.ok AnyReturnType



(* -- Blocks ---------------------------------------------------------------- *)

let stmt_is_escape {stmt; _} =
  match stmt with
  | Break | Continue | Reject _ | Return _ | ReturnVoid -> true
  | _ -> false

let list_until_escape xs =
  let rec aux accu = function
    | next :: _ when stmt_is_escape next -> List.rev @@ (next :: accu)
    | next :: rest -> aux (next :: accu) rest
    | _ -> List.rev accu
  in
  aux xs

(* -- Top-level Statements -------------------------------------------------- *)

let rec semantic_check_statement cf (s : Ast.untyped_statement) :
    Ast.typed_statement Validate.t =
  let loc = s.smeta.loc in
  match s.stmt with
  | NRFunApp (_, id, es) -> semantic_check_nr_fn_app ~loc ~cf id es
  | Assignment
      { assign_identifier= id
      ; assign_indices= lindex
      ; assign_op= assop
      ; assign_rhs= e } ->
      
      let block =
          Symbol_table.look vm id.name
          |> Option.map ~f:fst 
          |> Option.value 
              ~default:(
                if is_stan_math_function_name id.name then MathLibrary
                else fatal_error ()
              )
      and lhs =
        Ast.mk_untyped_expression ~loc
          ~expr:
            (Indexed
               ( Ast.mk_untyped_expression ~expr:(Variable id) ~loc:id.id_loc
               , lindex ))
        |> semantic_check_expression cf
      and assop = semantic_check_assignmentoperator assop
      and rhs = semantic_check_expression cf e

      in
      Validate.(
        liftA3 tuple3 lhs assop rhs
        >>= 
          fun (lhs,assop,rhs) ->
            semantic_check_assignment_operator ~loc assop lhs rhs
            |> apply_const (semantic_check_assignment_global ~loc ~cf ~block id)
            |> apply_const (semantic_check_assignment_read_only ~loc id)
      )
     
  | TargetPE e -> semantic_check_target_pe ~loc ~cf e
  | IncrementLogProb e -> semantic_check_incr_logprob ~loc ~cf e 
  | Tilde {arg= e; distribution= id; args= es; truncation= t} ->
      let ue = semantic_check_expression cf e in
      let uid = semantic_check_identifier id in
      let _ =
        if
          String.is_suffix uid.name ~suffix:"_lpdf"
          || String.is_suffix uid.name ~suffix:"_lpmf"
        then
          semantic_error ~loc:uid.id_loc
            "~-statement expects a distribution name without '_lpdf' or \
             '_lpmf' suffix."
      in
      let ues = List.map ~f:(semantic_check_expression cf) es in
      let ut = semantic_check_truncation cf t in
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      let _ =
        if not (cf.in_lp_fun_def || cf.current_block = Model) then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      let _ =
        if
          String.is_suffix uid.name ~suffix:"_cdf"
          || String.is_suffix uid.name ~suffix:"_ccdf"
        then
          semantic_error ~loc
            ( "CDF and CCDF functions may not be used with sampling notation. \
               Use increment_log_prob(" ^ uid.name ^ "_log(...)) instead." )
      in
      (* Check typing of ~ and target += *)
      let distribution_name_is_defined name arguments =
        let argumenttypes = get_arg_types arguments in
        stan_math_returntype (name ^ "_lpdf") argumenttypes
        = Some (ReturnType UReal)
        || stan_math_returntype (name ^ "_lpmf") argumenttypes
           = Some (ReturnType UReal)
        || stan_math_returntype (name ^ "_log") argumenttypes
           = Some (ReturnType UReal)
           && name <> "binomial_coefficient"
           && name <> "multiply"
        || ( match Symbol_table.look vm (name ^ "_lpdf") with
           | Some (Functions, UFun (listedtypes, ReturnType UReal)) ->
               check_compatible_arguments_mod_conv name listedtypes
                 argumenttypes
           | _ -> false )
        || ( match Symbol_table.look vm (name ^ "_lpmf") with
           | Some (Functions, UFun (listedtypes, ReturnType UReal)) ->
               check_compatible_arguments_mod_conv name listedtypes
                 argumenttypes
           | _ -> false )
        ||
        match Symbol_table.look vm (name ^ "_log") with
        | Some (Functions, UFun (listedtypes, ReturnType UReal)) ->
            check_compatible_arguments_mod_conv name listedtypes argumenttypes
        | _ -> false
      in
      let _ =
        if distribution_name_is_defined uid.name (ue :: ues) then ()
        else
          semantic_error ~loc
            ( "Ill-typed arguments to '~' statement. No distribution "
            ^ ("'" ^ uid.name ^ "'")
            ^ " was found with the correct signature." )
      in
      let cumulative_density_is_defined name arguments =
        let argumenttypes = get_arg_types arguments in
        ( stan_math_returntype (name ^ "_lcdf") argumenttypes
          = Some (ReturnType UReal)
        ||
        match Symbol_table.look vm (name ^ "_lcdf") with
        | Some (Functions, UFun (listedtypes, ReturnType UReal)) ->
            check_compatible_arguments_mod_conv name listedtypes argumenttypes
        | _ -> (
            false
            || stan_math_returntype (name ^ "_cdf_log") argumenttypes
               = Some (ReturnType UReal)
            ||
            match Symbol_table.look vm (name ^ "_cdf_log") with
            | Some (Functions, UFun (listedtypes, ReturnType UReal)) ->
                check_compatible_arguments_mod_conv name listedtypes
                  argumenttypes
            | _ -> false ) )
        && ( stan_math_returntype (name ^ "_lccdf") argumenttypes
             = Some (ReturnType UReal)
           ||
           match Symbol_table.look vm (name ^ "_lccdf") with
           | Some (Functions, UFun (listedtypes, ReturnType UReal)) ->
               check_compatible_arguments_mod_conv name listedtypes
                 argumenttypes
           | _ -> (
               false
               || stan_math_returntype (name ^ "_ccdf_log") argumenttypes
                  = Some (ReturnType UReal)
               ||
               match Symbol_table.look vm (name ^ "_ccdf_log") with
               | Some (Functions, UFun (listedtypes, ReturnType UReal)) ->
                   check_compatible_arguments_mod_conv name listedtypes
                     argumenttypes
               | _ -> false ) )
      in
      let _ =
        if
          match ut with
          | NoTruncate -> true
          | TruncateUpFrom ue ->
              cumulative_density_is_defined uid.name (ue :: ues)
          | TruncateDownFrom ue ->
              cumulative_density_is_defined uid.name (ue :: ues)
          | TruncateBetween (ue1, ue2) ->
              cumulative_density_is_defined uid.name (ue1 :: ues)
              && cumulative_density_is_defined uid.name (ue2 :: ues)
        then ()
        else
          semantic_error ~loc
            "Truncation is only defined if distribution has _lcdf and _lccdf \
             functions implemented with appropriate signature."
      in
      mk_typed_statement
        ~stmt:(Tilde {arg= ue; distribution= uid; args= ues; truncation= ut})
        ~return_type:NoReturnType ~loc

  | Break -> semantic_check_break ~loc ~cf
      
  | Continue -> semantic_check_continue ~loc ~cf
      
  | Return e -> semantic_check_return ~loc ~cf e
      
  | ReturnVoid -> semantic_check_returnvoid ~loc ~cf
      
  | Print ps -> semantic_check_print ~loc ~cf ps
      
  | Reject ps -> semantic_check_reject ~loc ~cf ps
      
  | Skip -> semantic_check_skip ~loc
      
  | IfThenElse (e, s1, os2) ->
      (* For, while, for each, if constructs take expressions of valid type *)
      let us1 = semantic_check_statement cf s1
      and uos2 =
        os2 
        |> Option.map ~f:(fun s -> semantic_check_statement cf s |> Validate.map ~f:Option.some)
        |> Option.value ~default:(Validate.ok None)
      and ue =
        semantic_check_expression_of_int_or_real_type cf e
          "Condition in conditional"
      in
      Validate.(
        liftA3 tuple3 ue us1 uos2
        >>= fun (ue, us1, uos2) ->
        let stmt = IfThenElse (ue, us1, uos2)
        and srt1 = us1.smeta.return_type
        and srt2 =
          uos2
          |> Option.map ~f:(fun s -> s.smeta.return_type)
          |> Option.value ~default:NoReturnType
        in
        try_compute_ifthenelse_statement_returntype loc srt1 srt2
        |> map ~f:(fun return_type ->
               mk_typed_statement ~stmt ~return_type ~loc ))
  | While (e, s) ->
      (* For, while, for each, if constructs take expressions of valid type *)
      let us =
        semantic_check_statement {cf with loop_depth= cf.loop_depth + 1} s
      and ue =
        semantic_check_expression_of_int_or_real_type cf e
          "Condition in while-loop"
      in
      Validate.liftA2
        (fun ue us ->
          mk_typed_statement
            ~stmt:(While (ue, us))
            ~return_type:us.smeta.return_type ~loc )
        ue us
  | For {loop_variable= id; lower_bound= e1; upper_bound= e2; loop_body= s} ->
      let uid = semantic_check_identifier id in
      let ue1 =
        semantic_check_expression_of_int_type cf e1 "Lower bound of for-loop"
      in
      let ue2 =
        semantic_check_expression_of_int_type cf e2 "Upper bound of for-loop"
      in
      (* For, while, for each, if constructs take expressions of valid type *)
      let _ = Symbol_table.begin_scope vm in
      let _ = check_fresh_variable uid false in
      let oindexblock = cf.current_block in
      let _ = Symbol_table.enter vm uid.name (oindexblock, UInt) in
      (* Check that function args and loop identifiers are not modified in function. (passed by const ref)*)
      let _ = Symbol_table.set_read_only vm uid.name in
      let us =
        semantic_check_statement {cf with loop_depth= cf.loop_depth + 1} s
      in
      let _ = Symbol_table.end_scope vm in
      mk_typed_statement
        ~stmt:
          (For
             { loop_variable= uid
             ; lower_bound= ue1
             ; upper_bound= ue2
             ; loop_body= us })
        ~return_type:us.smeta.return_type ~loc
        
  | ForEach (id, e, s) ->
      let uid = semantic_check_identifier id in
      let ue = semantic_check_expression cf e in
      (* For, while, for each, if constructs take expressions of valid type *)
      let loop_identifier_unsizedtype =
        match ue.emeta.type_ with
        | UArray ut -> ut
        | UVector | URowVector | UMatrix -> UReal
        | _ ->
            semantic_error ~loc:ue.emeta.loc
              ( "Foreach-loop must be over array, vector, row_vector or \
                 matrix. Instead found expression of type "
              ^ pretty_print_unsizedtype ue.emeta.type_
              ^ "." )
      in
      let _ = Symbol_table.begin_scope vm in
      let _ = check_fresh_variable uid false in
      let oindexblock = cf.current_block in
      let _ =
        Symbol_table.enter vm uid.name
          (oindexblock, loop_identifier_unsizedtype)
      in
      (* Check that function args and loop identifiers are not modified in function. (passed by const ref)*)
      let _ = Symbol_table.set_read_only vm uid.name in
      let us =
        semantic_check_statement {cf with loop_depth= cf.loop_depth + 1} s
      in
      let _ = Symbol_table.end_scope vm in
      mk_typed_statement
        ~stmt:(ForEach (uid, ue, us))
        ~return_type:us.smeta.return_type ~loc
  | Block vdsl ->
      Symbol_table.begin_scope vm ;
      let uvdsl =
        List.map ~f:(semantic_check_statement cf) vdsl |> Validate.sequence
      in
      Symbol_table.end_scope vm ;
      (* Any statements after a break or continue or return or reject do not count for the return
       type. *)
      Validate.(
        uvdsl
        >>= fun xs ->
        xs
        |> List.map ~f:(fun s -> s.smeta.return_type)
        |> List.fold ~init:(ok NoReturnType) ~f:(fun accu x ->
               accu >>= fun y -> try_compute_block_statement_returntype loc y x
           )
        |> map ~f:(fun return_type ->
               mk_typed_statement ~stmt:(Block xs) ~return_type ~loc ))
  | VarDecl
      { sizedtype= st
      ; transformation= trans
      ; identifier= id
      ; initial_value= init
      ; is_global= glob } ->
      let ust = semantic_check_sizedtype cf st
      and not_ptq e f =
        match e.emeta.ad_level with AutoDiffable -> false | _ -> f ()
      in
      let rec check_sizes_data_only = function
        | Middle.SVector ue -> not_ptq ue (fun () -> true)
        | SRowVector ue -> not_ptq ue (fun () -> true)
        | SMatrix (ue1, ue2) ->
            not_ptq ue1 (fun () ->
                match ue2.emeta.ad_level with
                | AutoDiffable -> false
                | _ -> true )
        | SArray (ust2, ue) -> not_ptq ue (fun () -> check_sizes_data_only ust2)
        | _ -> true
      in
      (* Sizes must be of level at most data. *)
      let _ =
        if glob && not (check_sizes_data_only ust) then
          semantic_error ~loc
            "Non-data variables are not allowed in top level size declarations."
      in
      let utrans = semantic_check_transformation cf trans in
      let uid = semantic_check_identifier id in
      let ut = unsizedtype_of_sizedtype ust in
      let _ = check_fresh_variable uid false in
      let ob = cf.current_block in
      let _ = Symbol_table.enter vm id.name (ob, ut) in
      let _ =
        if
          glob && ust = SInt
          &&
          match utrans with
          | Lower ue1 -> (
            match ue1.emeta.type_ with UReal -> true | _ -> false )
          | Upper ue1 -> (
            match ue1.emeta.type_ with UReal -> true | _ -> false )
          | LowerUpper (ue1, ue2) -> (
            match ue1.emeta.type_ with
            | UReal -> true
            | _ -> ( match ue2.emeta.type_ with UReal -> true | _ -> false ) )
          | _ -> false
        then
          semantic_error ~loc
            "Bounds of integer variable must be of type int. Found type real."
      in
      (* Parameters and transformed parameters are not int(array)  *)
      let _ =
        if
          glob
          && (cf.current_block = Param || cf.current_block = TParam)
          && unsizedtype_contains_int ut
        then semantic_error ~loc "(Transformed) Parameters cannot be integers."
      in
      let uinit =
        match init with
        | None -> None
        | Some e -> (
            let ts : Ast.typed_statement =
              semantic_check_statement cf
                (mk_untyped_statement ~loc
                   ~stmt:
                     (Assignment
                        { assign_identifier= id
                        ; assign_indices= []
                        ; assign_op= Assign
                        ; assign_rhs= e }))
            in
            match (ts.stmt, ts.smeta.return_type) with
            | Assignment {assign_rhs= ue; _}, NoReturnType -> Some ue
            | _ -> fatal_error () )
      in
      mk_typed_statement
        ~stmt:
          (VarDecl
             { sizedtype= ust
             ; transformation= utrans
             ; identifier= uid
             ; initial_value= uinit
             ; is_global= glob })
        ~loc ~return_type:NoReturnType
  | FunDef {returntype= rt; funname= id; arguments= args; body= b} ->
      let urt = semantic_check_returntype rt in
      let uid = semantic_check_identifier id in
      let uargs =
        List.map
          ~f:(function
            | at, ut, id ->
                ( semantic_check_autodifftype at
                , semantic_check_unsizedtype ut
                , semantic_check_identifier id ))
          args
      in
      let uarg_types = List.map ~f:(function w, y, _ -> (w, y)) uargs in
      (* User defined functions cannot be overloaded *)
      let _ =
        if Symbol_table.check_is_unassigned vm uid.name then (
          if
            Symbol_table.look vm uid.name
            <> Some (Functions, UFun (uarg_types, urt))
          then
            semantic_error ~loc
              ( "Function "
              ^ ("'" ^ uid.name ^ "'")
              ^ " has already been declared to have type "
              ^ Option.value_map ~default:"unknown"
                  ~f:(fun (_, t) -> pretty_print_unsizedtype t)
                  (Symbol_table.look vm uid.name) ) )
        else check_fresh_variable uid (List.length uarg_types = 0)
      in
      let _ =
        match b with
        | {stmt= Skip; _} ->
            if Symbol_table.check_is_unassigned vm uid.name then
              semantic_error ~loc
                ( "Function "
                ^ ("'" ^ uid.name ^ "'")
                ^ " has already been declared. A definition is expected." )
            else Symbol_table.set_is_unassigned vm uid.name
        | _ -> Symbol_table.set_is_assigned vm uid.name
      in
      let _ =
        Symbol_table.enter vm uid.name (Functions, UFun (uarg_types, urt))
      in
      let uarg_identifiers = List.map ~f:(function _, _, z -> z) uargs in
      let uarg_names = List.map ~f:(fun x -> x.name) uarg_identifiers in
      (* Check that function args and loop identifiers are not modified in function. (passed by const ref)*)
      let _ = List.map ~f:(Symbol_table.set_read_only vm) uarg_names in
      let open String in
      let open Pervasives in
      let _ =
        if
          urt <> ReturnType UReal
          && List.exists
               ~f:(fun x -> is_suffix uid.name ~suffix:x)
               ["_log"; "_lpdf"; "_lpmf"; "_lcdf"; "_lccdf"]
        then
          semantic_error ~loc
            "Real return type required for probability functions ending in \
             _log, _lpdf, _lpmf, _lcdf, or _lccdf."
      in
      let _ =
        if is_suffix uid.name ~suffix:"_lpdf" then
          let error_string =
            "Probability density functions require real variates (first \
             argument)."
          in
          match Option.map ~f:snd (List.hd uarg_types) with
          | None -> semantic_error ~loc error_string
          | Some Middle.UReal
           |Some UVector
           |Some URowVector
           |Some UMatrix
           |Some (UArray UReal)
           |Some (UArray UVector)
           |Some (UArray URowVector)
           |Some (UArray UMatrix) ->
              ()
          | Some x ->
              semantic_error ~loc
                ( error_string ^ " Instead found type "
                ^ pretty_print_unsizedtype x ^ "." )
      in
      let _ =
        if is_suffix uid.name ~suffix:"_lpmf" then
          let error_string =
            "Probability mass functions require integer variates (first \
             argument)."
          in
          match Option.map ~f:snd (List.hd uarg_types) with
          | None -> semantic_error ~loc error_string
          | Some Middle.UInt | Some (UArray UInt) -> ()
          | Some x ->
              semantic_error ~loc
                ( error_string ^ " Instead found type "
                ^ pretty_print_unsizedtype x ^ "." )
      in
      let _ = Symbol_table.begin_scope vm in
      (* All function arguments are distinct *)
      let _ =
        if dup_exists uarg_names then
          semantic_error ~loc
            "All function arguments must be distinct identifiers."
      in
      let _ =
        List.map ~f:(fun x -> check_fresh_variable x false) uarg_identifiers
      in
      (* TODO: Bob was suggesting that function arguments must be allowed to shadow user defined functions but not library functions. Should we allow for that? *)
      (* We treat DataOnly arguments as if they are data and AutoDiffable arguments
       as if they are parameters, for the purposes of type checking. *)
      let _ =
        List.map2 ~f:(Symbol_table.enter vm) uarg_names
          (List.map
             ~f:(function
               | Middle.DataOnly, ut -> (Data, ut)
               | AutoDiffable, ut -> (Param, ut))
             uarg_types)
      in
      let ub =
        semantic_check_statement
          { cf with
            in_fun_def= true
          ; in_rng_fun_def= is_suffix uid.name ~suffix:"_rng"
          ; in_lp_fun_def= is_suffix uid.name ~suffix:"_lp"
          ; in_returning_fun_def= urt <> Void }
          b
      in
      (* Check that every trace through function body contains return statement of right type *)
      let _ =
        if
          Symbol_table.check_is_unassigned vm uid.name
          || check_of_compatible_return_type urt ub.smeta.return_type
        then ()
        else
          semantic_error ~loc
            "Function bodies must contain a return statement of correct type \
             in every branch."
      in
      let _ = Symbol_table.end_scope vm in
      mk_typed_statement ~return_type:NoReturnType ~loc
        ~stmt:
          (FunDef {returntype= urt; funname= uid; arguments= uargs; body= ub})

(* The actual semantic checks for all AST nodes! *)
let semantic_check_program
    { functionblock= fb
    ; datablock= db
    ; transformeddatablock= tdb
    ; parametersblock= pb
    ; transformedparametersblock= tpb
    ; modelblock= mb
    ; generatedquantitiesblock= gb } =
  (* NB: We always want to make sure we start with an empty symbol table, in
     case we are processing multiple files in one run. *)
  let _ = unsafe_clear_symbol_table vm in
  let semantic_check_ostatements_in_block cf b =
    Option.map
      ~f:(List.map ~f:(semantic_check_statement {cf with current_block= b}))
  in
  let cf =
    { current_block= Functions
    ; in_fun_def= false
    ; in_returning_fun_def= false
    ; in_rng_fun_def= false
    ; in_lp_fun_def= false
    ; loop_depth= 0 }
  in
  let ufb = semantic_check_ostatements_in_block cf Functions fb in
  (* Check that all declared functions have a definition *)
  let _ =
    if
      Symbol_table.check_some_id_is_unassigned vm
      && !check_that_all_functions_have_definition
    then
      semantic_error ~loc:(List.hd_exn (Option.value_exn ufb)).smeta.loc
        "Some function is declared without specifying a definition."
    (* TODO: insert better location in the error above *)
  in
  let udb = semantic_check_ostatements_in_block cf Data db in
  let utdb = semantic_check_ostatements_in_block cf TData tdb in
  let upb = semantic_check_ostatements_in_block cf Param pb in
  let utpb = semantic_check_ostatements_in_block cf TParam tpb in
  (* Model top level variables only assigned and read in model  *)
  let _ = Symbol_table.begin_scope vm in
  let umb = semantic_check_ostatements_in_block cf Model mb in
  let _ = Symbol_table.end_scope vm in
  let ugb = semantic_check_ostatements_in_block cf GQuant gb in
  { functionblock= ufb
  ; datablock= udb
  ; transformeddatablock= utdb
  ; parametersblock= upb
  ; transformedparametersblock= utpb
  ; modelblock= umb
  ; generatedquantitiesblock= ugb }
