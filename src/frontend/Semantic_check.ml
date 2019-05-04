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

(* Top level function semantic_check_program declares the AST while operating
   on (1) a global symbol table vm, and (2) structure of type context_flags_record
   to communicate information down the AST. *)

let ternary_if = "TernaryIf__"

let%test "bad op name" = phys_equal (Mir.operator_of_string "Pluss__") None
let%test "good op name" = Mir.operator_of_string "Plus__" = Some Plus

(** A hash table to hold some name conversions between the AST nodes and the
    Stan Math name of the operator *)
let string_of_operators =
  Map.Poly.of_alist_multi
    [ (Mir.string_of_operator Mir.Plus, "add")
    ; (Mir.string_of_operator PPlus, "plus")
    ; (Mir.string_of_operator Minus, "subtract")
    ; (Mir.string_of_operator PMinus, "minus")
    ; (Mir.string_of_operator Times, "multiply")
    ; (Mir.string_of_operator Divide, "mdivide_right")
    ; (Mir.string_of_operator Divide, "divide")
    ; (Mir.string_of_operator Modulo, "modulus")
    ; (Mir.string_of_operator LDivide, "mdivide_left")
    ; (Mir.string_of_operator EltTimes, "elt_multiply")
    ; (Mir.string_of_operator EltDivide, "elt_divide")
    ; (Mir.string_of_operator Pow, "pow")
    ; (Mir.string_of_operator Or, "logical_or")
    ; (Mir.string_of_operator And, "logical_and")
    ; (Mir.string_of_operator Equals, "logical_eq")
    ; (Mir.string_of_operator NEquals, "logical_neq")
    ; (Mir.string_of_operator Less, "logical_lt")
    ; (Mir.string_of_operator Leq, "logical_lte")
    ; (Mir.string_of_operator Greater, "logical_gt")
    ; (Mir.string_of_operator Geq, "logical_gte")
    ; (Mir.string_of_operator PNot, "logical_negation")
    ; (Mir.string_of_operator Transpose, "transpose")
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
let operator_return_type_from_string op_name (args : Ast.typed_expression list)
    =
  if op_name = "Assign" || op_name = "ArrowAssign" then
    match args with
    | [{emeta= meta1; _}; {emeta= meta2; _}]
      when Type_conversion.check_of_same_type_mod_array_conv "" meta1.type_
             meta2.type_ ->
        Some Mir.Void
    | _ -> None
  else
    Map.Poly.find_multi string_of_operators op_name
    |> List.find_map ~f:(fun name ->
           Stan_math_signatures.stan_math_returntype name args )

let operator_return_type op =
  operator_return_type_from_string (Mir.string_of_operator op)

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
  | Mir.UInt -> true
  | UArray ut -> unsizedtype_contains_int ut
  | _ -> false

let rec unsizedtype_of_sizedtype = function
  | Mir.SInt -> Mir.UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix (_, _) -> UMatrix
  | SArray (st, _) -> UArray (unsizedtype_of_sizedtype st)

let rec lub_ad_type = function
  | [] -> Mir.DataOnly
  | x :: xs ->
      let y = lub_ad_type xs in
      if Mir.compare_autodifftype x y < 0 then y else x

let calculate_autodifftype at ut =
  match at with
  | (Param | TParam | Model) when not (unsizedtype_contains_int ut) ->
      Mir.AutoDiffable
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
  | Mir.ReturnType UReal, Mir.ReturnType UInt
   |ReturnType UInt, ReturnType UReal ->
      Mir.ReturnType UReal
  | rt1, rt2 when rt1 = rt2 -> rt2
  | _ ->
      semantic_error ~loc
        ( "Branches of function definition need to have the same return type. \
           Instead, found return types "
        ^ pretty_print_returntype rt1
        ^ " and "
        ^ pretty_print_returntype rt2
        ^ "." )

let try_compute_ifthenelse_statement_returntype loc srt1 srt2 =
  match (srt1, srt2) with
  | Complete rt1, Complete rt2 -> Complete (lub_rt loc rt1 rt2)
  | Incomplete rt1, Incomplete rt2
   |Complete rt1, Incomplete rt2
   |Incomplete rt1, Complete rt2 ->
      Incomplete (lub_rt loc rt1 rt2)
  | AnyReturnType, NoReturnType
   |NoReturnType, AnyReturnType
   |NoReturnType, NoReturnType ->
      NoReturnType
  | AnyReturnType, Incomplete rt
   |Incomplete rt, AnyReturnType
   |Complete rt, NoReturnType
   |NoReturnType, Complete rt
   |NoReturnType, Incomplete rt
   |Incomplete rt, NoReturnType ->
      Incomplete rt
  | Complete rt, AnyReturnType | AnyReturnType, Complete rt -> Complete rt
  | AnyReturnType, AnyReturnType -> AnyReturnType

let try_compute_block_statement_returntype loc srt1 srt2 =
  match (srt1, srt2) with
  | Complete rt1, Complete rt2 | Incomplete rt1, Complete rt2 ->
      Complete (lub_rt loc rt1 rt2)
  | Incomplete rt1, Incomplete rt2 | Complete rt1, Incomplete rt2 ->
      Incomplete (lub_rt loc rt1 rt2)
  | NoReturnType, NoReturnType -> NoReturnType
  | AnyReturnType, Incomplete rt
   |Complete rt, NoReturnType
   |NoReturnType, Incomplete rt
   |Incomplete rt, NoReturnType ->
      Incomplete rt
  | NoReturnType, Complete rt
   |Complete rt, AnyReturnType
   |Incomplete rt, AnyReturnType
   |AnyReturnType, Complete rt ->
      Complete rt
  | AnyReturnType, NoReturnType
   |NoReturnType, AnyReturnType
   |AnyReturnType, AnyReturnType ->
      AnyReturnType

let check_fresh_variable_basic id is_nullary_function =
  (* No shadowing! *)
  (* For some strange reason, Stan allows user declared identifiers that are
     not of nullary function types to clash with nullary library functions.
     No other name clashes are tolerated. Here's the logic to
     achieve that. *)
  let _ =
    if
      is_stan_math_function_name id.name
      && (is_nullary_function || stan_math_returntype id.name [] = None)
    then
      let error_msg =
        String.concat ~sep:" "
          [ "Identifier"
          ; "'" ^ id.name ^ "'"
          ; "clashes with Stan Math library function." ]
      in
      semantic_error ~loc:id.id_loc error_msg
  in
  match Symbol_table.look vm id.name with
  | Some _ ->
      let error_msg =
        String.concat ~sep:" "
          ["Identifier"; "'" ^ id.name ^ "'"; "is already in use."]
      in
      semantic_error ~loc:id.id_loc error_msg
  | None -> ()

let check_fresh_variable id is_nullary_function =
  List.iter
    ~f:(fun name -> check_fresh_variable_basic name is_nullary_function)
    (probability_distribution_name_variants id)

(** Least upper bound of expression autodiff types *)
let lub_ad_e exprs =
  exprs |> List.map ~f:(fun x -> x.emeta.ad_level) |> lub_ad_type

let rec inferred_unsizedtype_of_indexed loc ut typed_indexl =
  let recurse = inferred_unsizedtype_of_indexed loc in
  match (ut, typed_indexl) with
  (* Here, we need some special logic to deal with row and column vectors
     properly. *)
  | Mir.UMatrix, [(All, _); (Single _, Mir.UInt)]
   |UMatrix, [(Upfrom _, _); (Single _, UInt)]
   |UMatrix, [(Downfrom _, _); (Single _, UInt)]
   |UMatrix, [(Between _, _); (Single _, UInt)]
   |UMatrix, [(Single _, UArray UInt); (Single _, UInt)] ->
      Mir.UVector
  | ut, [] -> ut
  | ut, typed_index :: typed_indices -> (
      let reduce_type =
        match typed_index with Single _, UInt -> true | _ -> false
      in
      match ut with
      | UArray ut' ->
          if reduce_type then recurse ut' typed_indices
            (* TODO: this can easily be made tail recursive if needs be *)
          else UArray (recurse ut' typed_indices)
      | UVector ->
          if reduce_type then recurse UReal typed_indices
          else recurse UVector typed_indices
      | URowVector ->
          if reduce_type then recurse UReal typed_indices
          else recurse URowVector typed_indices
      | UMatrix ->
          if reduce_type then recurse URowVector typed_indices
          else recurse UMatrix typed_indices
      (* Check that expressions take valid number of indices (based on their matrix/array dimensions) *)
      | _ ->
          semantic_error ~loc
            ( "Only expressions of array, matrix, row_vector and vector type \
               may be indexed. Instead, found type "
            ^ pretty_print_unsizedtype ut
            ^ "." ) )

(* == SEMANTIC CHECK OF PROGRAM ELEMENTS ==================================== *)

(* Probably nothing to do here *)
let semantic_check_assignmentoperator op = op

(* Probably nothing to do here *)
let semantic_check_autodifftype at = at

(* Probably nothing to do here *)
let rec semantic_check_unsizedtype = function
  | Mir.UArray ut -> Mir.UArray (semantic_check_unsizedtype ut)
  | UFun (l, rt) ->
      UFun
        ( List.map
            ~f:(fun (at, ut) ->
              (semantic_check_autodifftype at, semantic_check_unsizedtype ut)
              )
            l
        , semantic_check_returntype rt )
  | ut -> ut

and semantic_check_returntype = function
  | Mir.Void -> Mir.Void
  | ReturnType ut -> ReturnType (semantic_check_unsizedtype ut)

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
    Errors.semantic_error ~loc:id.id_loc
      ("Identifier " ^ ("'" ^ id.name ^ "'") ^ " clashes with model name.") ;
  if
    String.is_suffix id.name ~suffix:"__"
    || List.exists ~f:(fun str -> str = id.name) reserved_keywords
  then
    semantic_error ~loc:id.id_loc
      ( "Identifier "
      ^ ("'" ^ id.name ^ "'")
      ^ " clashes with reserved keyword." ) ;
  id

(* -- Operators ------------------------------------------------------------- *)
let semantic_check_operator i = i

(* -- Expressions ----------------------------------------------------------- *)

(* Function application validation checks *)
let semantic_check_fn_map_rect ~loc id es =
  match (id.name, es) with
  | "map_rect", {expr= Variable arg1; _} :: _
    when String.(
           is_suffix arg1.name ~suffix:"_lp"
           || is_suffix arg1.name ~suffix:"_rng") ->
      semantic_error ~loc
        (Format.sprintf
           "Mapped function cannot be an _rng or _lp function, found function \
            name: %s"
           arg1.name)
  | _ -> ()

let semantic_check_fn_conditioning ~loc id =
  if
    List.exists ["_lpdf"; "_lpmf"; "_lcdf"; "_lccdf"] ~f:(fun x ->
        String.is_suffix id.name ~suffix:x )
  then
    semantic_error ~loc
      "Probabilty functions with suffixes _lpdf, _lpmf, _lcdf, and _lccdf, \
       require a vertical bar (|) between the first two arguments."
  else ()

(** `Target+=` can only be used in model and functions
    with right suffix (same for tilde etc)
*)
let semantic_check_fn_target_plus_equals cf ~loc id =
  if
    String.is_suffix id.name ~suffix:"_lp"
    && not (cf.in_lp_fun_def || cf.current_block = Model)
  then
    semantic_error ~loc
      "Target can only be accessed in the model block or in definitions of \
       functions with the suffix _lp."
  else ()

(** Rng functions cannot be used in Tp or Model and only
    in funciton defs with the right suffix
*)
let semantic_check_fn_rng cf ~loc id =
  if
    String.is_suffix id.name ~suffix:"_rng"
    && ( (cf.in_fun_def && not cf.in_rng_fun_def)
       || cf.current_block = TParam || cf.current_block = Model )
  then
    semantic_error ~loc
      "Random number generators are only allowed in transformed data block, \
       generated quantities block or user-defined functions with names ending \
       in _rng."
  else ()

(* Regular function application *)
let semantic_check_fn_normal ~loc id es =
  match Symbol_table.look vm id.name with
  | Some (_, Mir.UFun (_, Void)) ->
      semantic_error ~loc
        (Format.sprintf
           "A returning function was expected but a non-returning function \
            '%s' was supplied."
           id.name)
  | Some (_, UFun (listed_tys, rt))
    when not (check_compatible_arguments_mod_conv id.name listed_tys es) ->
      semantic_error ~loc
        (Format.sprintf
           "Ill-typed arguments supplied to function '%s'. Available \
            signatures:\n\
            %s\n\
            Instead supplied arguments of incompatible type: %s."
           id.name
           (pretty_print_unsizedtype (UFun (listed_tys, rt)))
           (List.map es ~f:type_of_expr_typed |> pretty_print_unsizedtypes))
  | Some (_, UFun (_, ReturnType ut)) ->
      mk_typed_expression
        ~expr:(FunApp (UserDefined, id, es))
        ~ad_level:(lub_ad_e es) ~type_:ut ~loc
  | Some _ ->
      (* Check that Funaps are actually functions *)
      semantic_error ~loc
        (Format.sprintf
           "A returning function was expected but a non-returning function \
            '%s' was supplied."
           id.name)
  | None ->
      semantic_error ~loc
        (Format.sprintf
           "A returning function was expected but an undeclared identifier \
            '%s' was supplied."
           id.name)

(** Stan-Math function application
*)
let semantic_check_fn_stan_math ~loc id es =
  match stan_math_returntype id.name es with
  | Some Void ->
      semantic_error ~loc
        (Format.sprintf
           "A returning function was expected but a non-returning function \
            '%s' was supplied."
           id.name)
  | Some (ReturnType ut) ->
      mk_typed_expression
        ~expr:(FunApp (StanLib, id, es))
        ~ad_level:(lub_ad_e es) ~type_:ut ~loc
  | _ ->
      semantic_error ~loc
        (Format.sprintf
           "Ill-typed arguments supplied to function '%s'. Available \
            signatures: %s\n\
            Instead supplied arguments of incompatible type: %s."
           id.name
           (Stan_math_signatures.pretty_print_all_math_lib_fn_sigs id.name)
           (List.map es ~f:type_of_expr_typed |> pretty_print_unsizedtypes))

let fn_kind_from_identifier id =
  if is_stan_math_function_name id.name then StanLib else UserDefined

(** Determines the function kind based on the identifier and performs the
    corresponding semantic check
*)
let semantic_check_fn ~loc id es =
  match fn_kind_from_identifier id with
  | StanLib -> semantic_check_fn_stan_math ~loc id es
  | UserDefined -> semantic_check_fn_normal ~loc id es

let rec semantic_check_expression cf ({emeta; expr} : Ast.untyped_expression) :
    Ast.typed_expression =
  match expr with
  | TernaryIf (e1, e2, e3) -> (
      let ue1 = semantic_check_expression cf e1 in
      let ue2 = semantic_check_expression cf e2 in
      let ue3 = semantic_check_expression cf e3 in
      match operator_return_type_from_string ternary_if [ue1; ue2; ue3] with
      | Some (Mir.ReturnType ut) ->
          mk_typed_expression
            ~expr:(TernaryIf (ue1, ue2, ue3))
            ~ad_level:(lub_ad_e [ue1; ue2; ue3])
            ~type_:ut ~loc:emeta.loc
      | Some Mir.Void | None ->
          semantic_error ~loc:emeta.loc
            ( "Ill-typed arguments supplied to ? : operator. Available \
               signatures: "
            ^ pretty_print_all_operator_signatures ternary_if
            ^ "\nInstead supplied arguments of incompatible type: "
            ^ pretty_print_unsizedtype ue1.emeta.type_
            ^ ", "
            ^ pretty_print_unsizedtype ue2.emeta.type_
            ^ ", "
            ^ pretty_print_unsizedtype ue3.emeta.type_
            ^ "." ) )
  | BinOp (e1, op, e2) -> (
      let ue1 = semantic_check_expression cf e1
      and uop = semantic_check_operator op
      and ue2 = semantic_check_expression cf e2 in
      match operator_return_type uop [ue1; ue2] with
      | Some (Mir.ReturnType ut) ->
          mk_typed_expression
            ~expr:(BinOp (ue1, uop, ue2))
            ~ad_level:(lub_ad_e [ue1; ue2])
            ~type_:ut ~loc:emeta.loc
      | Some Mir.Void | None ->
          semantic_error ~loc:emeta.loc
            ( "Ill-typed arguments supplied to infix operator "
            ^ pretty_print_operator uop ^ ". Available signatures: "
            ^ pretty_print_all_operator_signatures (Mir.string_of_operator uop)
            ^ "\nInstead supplied arguments of incompatible type: "
            ^ pretty_print_unsizedtype ue1.emeta.type_
            ^ ", "
            ^ pretty_print_unsizedtype ue2.emeta.type_
            ^ "." ) )
  | PrefixOp (op, e) -> (
      let uop = semantic_check_operator op
      and ue = semantic_check_expression cf e in
      match operator_return_type uop [ue] with
      | Some (Mir.ReturnType ut) ->
          mk_typed_expression
            ~expr:(PrefixOp (uop, ue))
            ~ad_level:(lub_ad_e [ue])
            ~type_:ut ~loc:emeta.loc
      | Some Mir.Void | None ->
          semantic_error ~loc:emeta.loc
            ( "Ill-typed arguments supplied to prefix operator "
            ^ pretty_print_operator uop ^ ". Available signatures: "
            ^ pretty_print_all_operator_signatures (Mir.string_of_operator uop)
            ^ "\nInstead supplied argument of incompatible type: "
            ^ pretty_print_unsizedtype ue.emeta.type_
            ^ "." ) )
  | PostfixOp (e, op) -> (
      let ue = semantic_check_expression cf e in
      let uop = semantic_check_operator op in
      match operator_return_type op [ue] with
      | Some (Mir.ReturnType ut) ->
          mk_typed_expression
            ~expr:(PostfixOp (ue, uop))
            ~ad_level:(lub_ad_e [ue])
            ~type_:ut ~loc:emeta.loc
      | Some Mir.Void | None ->
          semantic_error ~loc:emeta.loc
            ( "Ill-typed arguments supplied to postfix operator "
            ^ pretty_print_operator uop ^ ". Available signatures: "
            ^ pretty_print_all_operator_signatures (Mir.string_of_operator op)
            ^ "\nInstead supplied argument of incompatible type: "
            ^ pretty_print_unsizedtype ue.emeta.type_
            ^ "." ) )
  | Variable id ->
      let uid = semantic_check_identifier id in
      let ut = Symbol_table.look vm id.name in
      (* Check that variable in scope if used  *)
      let _ =
        if ut = None && not (is_stan_math_function_name uid.name) then
          semantic_error ~loc:emeta.loc
            ("Identifier " ^ ("'" ^ uid.name ^ "'") ^ " not in scope.")
      and originblock, type_ =
        Option.value ~default:(MathLibrary, Mir.UMathLibraryFunction) ut
      in
      mk_typed_expression ~expr:(Variable uid)
        ~ad_level:(calculate_autodifftype originblock type_)
        ~type_ ~loc:emeta.loc
  | IntNumeral s ->
      mk_typed_expression ~expr:(IntNumeral s) ~ad_level:DataOnly ~type_:UInt
        ~loc:emeta.loc
  | RealNumeral s ->
      mk_typed_expression ~expr:(RealNumeral s) ~ad_level:DataOnly ~type_:UReal
        ~loc:emeta.loc
  | FunApp (_, id, es) ->
      let uid = semantic_check_identifier id
      and ues = List.map ~f:(semantic_check_expression cf) es in
      semantic_check_fn_map_rect ~loc:emeta.loc uid ues ;
      semantic_check_fn_conditioning ~loc:emeta.loc uid ;
      semantic_check_fn_target_plus_equals cf ~loc:emeta.loc uid ;
      semantic_check_fn_rng cf ~loc:emeta.loc uid ;
      semantic_check_fn ~loc:emeta.loc uid ues
  | CondDistApp (id, es) -> (
      let uid = semantic_check_identifier id in
      let open String in
      let open Pervasives in
      let _ =
        if
          not
            (List.exists
               ~f:(fun x -> is_suffix uid.name ~suffix:x)
               ["_lpdf"; "_lpmf"; "_lcdf"; "_lccdf"])
        then
          semantic_error ~loc:emeta.loc
            "Only functions with names ending in _lpdf, _lpmf, _lcdf, _lccdf \
             can make use of conditional notation."
      in
      let ues = List.map ~f:(semantic_check_expression cf) es in
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      let _ =
        if
          is_suffix uid.name ~suffix:"_lp"
          && not (cf.in_lp_fun_def || cf.current_block = Model)
        then
          semantic_error ~loc:emeta.loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      let returnblock = lub_ad_e ues in
      match stan_math_returntype uid.name ues with
      | Some Void ->
          semantic_error ~loc:emeta.loc
            ( "A returning function was expected but a non-returning function "
            ^ ("'" ^ uid.name ^ "'")
            ^ " was supplied." )
      | Some (ReturnType ut) ->
          mk_typed_expression
            ~expr:(CondDistApp (uid, ues))
            ~ad_level:returnblock ~type_:ut ~loc:emeta.loc
      (* Check that function arguments match signature  *)
      (* Also check whether function arguments meet data requirement. *)
      | None -> (
          let _ =
            if is_stan_math_function_name uid.name then
              semantic_error ~loc:emeta.loc
                ( "Ill-typed arguments supplied to function "
                ^ ("'" ^ uid.name ^ "'")
                ^ ". Available signatures: "
                ^ pretty_print_all_math_lib_fn_sigs uid.name
                ^ "\nInstead supplied arguments of incompatible type: "
                ^ pretty_print_unsizedtypes
                    (List.map ~f:type_of_expr_typed ues)
                ^ "." )
          in
          match Symbol_table.look vm uid.name with
          | Some (_, UFun (_, Void)) ->
              semantic_error ~loc:emeta.loc
                ( "A returning function was expected but a non-returning \
                   function "
                ^ ("'" ^ uid.name ^ "'")
                ^ " was supplied." )
          | Some (_, UFun (listedtypes, ReturnType ut)) ->
              let _ =
                if
                  not
                    (check_compatible_arguments_mod_conv uid.name listedtypes
                       ues)
                then
                  semantic_error ~loc:emeta.loc
                    ( "Ill-typed arguments supplied to function "
                    ^ ("'" ^ uid.name ^ "'")
                    ^ ". Available signatures:\n"
                    ^ pretty_print_unsizedtype
                        (UFun (listedtypes, ReturnType ut))
                    ^ "\nInstead supplied arguments of incompatible type: "
                    ^ pretty_print_unsizedtypes
                        (List.map ~f:type_of_expr_typed ues)
                    ^ "." )
              in
              mk_typed_expression
                ~expr:(CondDistApp (uid, ues))
                ~ad_level:returnblock ~type_:ut ~loc:emeta.loc
          | Some _ ->
              (* Check that Funaps are actually functions *)
              semantic_error ~loc:emeta.loc
                ( "A returning function was expected but a non-function value "
                ^ ("'" ^ uid.name ^ "'")
                ^ " was supplied." )
          | None ->
              semantic_error ~loc:emeta.loc
                ( "A returning function was expected but an undeclared \
                   identifier "
                ^ ("'" ^ uid.name ^ "'")
                ^ " was supplied." ) ) )
  | GetLP ->
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      let _ =
        if
          not
            ( cf.in_lp_fun_def || cf.current_block = Model
            || cf.current_block = TParam )
        then
          semantic_error ~loc:emeta.loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      mk_typed_expression ~expr:GetLP
        ~ad_level:(calculate_autodifftype cf.current_block UReal)
        ~type_:UReal ~loc:emeta.loc
  | GetTarget ->
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      let _ =
        if
          not
            ( cf.in_lp_fun_def || cf.current_block = Model
            || cf.current_block = TParam )
        then
          semantic_error ~loc:emeta.loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      mk_typed_expression ~expr:GetTarget
        ~ad_level:(calculate_autodifftype cf.current_block UReal)
        ~type_:UReal ~loc:emeta.loc
  | ArrayExpr es ->
      let ues = List.map ~f:(semantic_check_expression cf) es in
      let elementtypes = List.map ~f:(fun y -> y.emeta.type_) ues in
      (* Array expressions must be of uniform type. (Or mix of int and real) *)
      let _ =
        if
          List.exists
            ~f:(fun x ->
              not
                ( check_of_same_type_mod_array_conv "" x.emeta.type_
                    (List.hd_exn ues).emeta.type_
                || check_of_same_type_mod_array_conv ""
                     (List.hd_exn ues).emeta.type_ x.emeta.type_ ) )
            ues
        then
          semantic_error ~loc:emeta.loc
            "Array expression must have entries of consistent type."
      in
      let array_type =
        if List.exists ~f:(fun x -> List.hd_exn elementtypes <> x) elementtypes
        then Mir.UArray UReal
        else UArray (List.hd_exn elementtypes)
      in
      let returnblock = lub_ad_e ues in
      mk_typed_expression ~expr:(ArrayExpr ues) ~ad_level:returnblock
        ~type_:array_type ~loc:emeta.loc
  | RowVectorExpr es ->
      let ues = List.map ~f:(semantic_check_expression cf) es in
      let elementtypes = List.map ~f:(fun y -> y.emeta.type_) ues in
      let ut =
        if List.for_all ~f:(fun x -> x = UReal || x = UInt) elementtypes then
          Mir.URowVector
        else if List.for_all ~f:(fun x -> x = URowVector) elementtypes then
          UMatrix
        else
          semantic_error ~loc:emeta.loc
            "Row_vector expression must have all int and real entries or all \
             row_vector entries."
      in
      let returnblock = lub_ad_e ues in
      mk_typed_expression ~expr:(RowVectorExpr ues) ~ad_level:returnblock
        ~type_:ut ~loc:emeta.loc
  | Paren e ->
      let ue = semantic_check_expression cf e in
      mk_typed_expression ~expr:(Paren ue) ~ad_level:ue.emeta.ad_level
        ~type_:ue.emeta.type_ ~loc:emeta.loc
  | Indexed (e, indices) ->
      let ue = semantic_check_expression cf e in
      let uindices = List.map ~f:(semantic_check_index cf) indices in
      let uindices_with_types =
        List.map
          ~f:(function
            | Single e as i -> (i, e.emeta.type_) | i -> (i, Mir.UInt) )
          uindices
      in
      let inferred_ad_type_of_indexed at uindices =
        lub_ad_type
          ( at
          :: List.map
               ~f:(function
                 | All -> Mir.DataOnly
                 | Single ue1 | Upfrom ue1 | Downfrom ue1 ->
                     lub_ad_type [at; ue1.emeta.ad_level]
                 | Between (ue1, ue2) ->
                     lub_ad_type [at; ue1.emeta.ad_level; ue2.emeta.ad_level]
                 )
               uindices )
      in
      let at = inferred_ad_type_of_indexed ue.emeta.ad_level uindices
      and ut =
        inferred_unsizedtype_of_indexed emeta.loc ue.emeta.type_
          uindices_with_types
      in
      mk_typed_expression
        ~expr:(Indexed (ue, uindices))
        ~ad_level:at ~type_:ut ~loc:emeta.loc

and semantic_check_expression_of_int_type cf e name =
  let ue = semantic_check_expression cf e in
  let _ =
    if not (has_int_type ue) then
      semantic_error_e ue
        ( name ^ " must be of type int. Instead found type "
        ^ pretty_print_unsizedtype ue.emeta.type_
        ^ "." )
  in
  ue

and semantic_check_expression_of_int_or_real_type cf e name =
  let ue = semantic_check_expression cf e in
  let _ =
    if not (has_int_or_real_type ue) then
      semantic_error_e ue
        ( name ^ " must be of type int or real. Instead found type "
        ^ pretty_print_unsizedtype ue.emeta.type_
        ^ "." )
  in
  ue

(* -- Indices --------------------------------------------------------------- *)
and semantic_check_index cf = function
  | All -> All
  (* Check that indexes have int (container) type *)
  | Single e ->
      let ue = semantic_check_expression cf e in
      let loc = ue.emeta.loc in
      if has_int_type ue || has_int_array_type ue then Single ue
      else
        semantic_error ~loc
          ( "Index must be of type int or int[] or must be a range. Instead \
             found type "
          ^ pretty_print_unsizedtype ue.emeta.type_
          ^ "." )
  | Upfrom e ->
      let ue = semantic_check_expression_of_int_type cf e "Range bound" in
      Upfrom ue
  | Downfrom e ->
      let ue = semantic_check_expression_of_int_type cf e "Range bound" in
      Downfrom ue
  | Between (e1, e2) ->
      let ue1 = semantic_check_expression_of_int_type cf e1 "Range bound" in
      let ue2 = semantic_check_expression_of_int_type cf e2 "Range bound" in
      Between (ue1, ue2)

(* -- Sized Types ----------------------------------------------------------- *)
let rec semantic_check_sizedtype cf = function
  | Mir.SInt -> Mir.SInt
  | SReal -> SReal
  | SVector e ->
      let ue = semantic_check_expression_of_int_type cf e "Vector sizes" in
      SVector ue
  | SRowVector e ->
      let ue = semantic_check_expression_of_int_type cf e "Row vector sizes" in
      SRowVector ue
  | SMatrix (e1, e2) ->
      let ue1 = semantic_check_expression_of_int_type cf e1 "Matrix sizes"
      and ue2 = semantic_check_expression_of_int_type cf e2 "Matrix sizes" in
      SMatrix (ue1, ue2)
  | SArray (st, e) ->
      let ust = semantic_check_sizedtype cf st in
      let ue = semantic_check_expression_of_int_type cf e "Array sizes" in
      SArray (ust, ue)

(* -- Transformations ------------------------------------------------------- *)
let semantic_check_transformation cf = function
  | Identity -> Identity
  | Lower e ->
      let ue =
        semantic_check_expression_of_int_or_real_type cf e "Lower bound"
      in
      Lower ue
  | Upper e ->
      let ue =
        semantic_check_expression_of_int_or_real_type cf e "Upper bound"
      in
      Upper ue
  | LowerUpper (e1, e2) ->
      let ue1 =
        semantic_check_expression_of_int_or_real_type cf e1 "Lower bound"
      in
      let ue2 =
        semantic_check_expression_of_int_or_real_type cf e2 "Upper bound"
      in
      LowerUpper (ue1, ue2)
  | Offset e ->
      let ue = semantic_check_expression_of_int_or_real_type cf e "Offset" in
      Offset ue
  | Multiplier e ->
      let ue =
        semantic_check_expression_of_int_or_real_type cf e "Multiplier"
      in
      Multiplier ue
  | OffsetMultiplier (e1, e2) ->
      let ue1 = semantic_check_expression_of_int_or_real_type cf e1 "Offset" in
      let ue2 =
        semantic_check_expression_of_int_or_real_type cf e2 "Multiplier"
      in
      OffsetMultiplier (ue1, ue2)
  | Ordered -> Ordered
  | PositiveOrdered -> PositiveOrdered
  | Simplex -> Simplex
  | UnitVector -> UnitVector
  | CholeskyCorr -> CholeskyCorr
  | CholeskyCov -> CholeskyCov
  | Correlation -> Correlation
  | Covariance -> Covariance

(* -- Printables ------------------------------------------------------------ *)

let semantic_check_printable cf = function
  | PString s -> PString s
  (* Print/reject expressions cannot be of function type. *)
  | PExpr e -> (
      let ue = semantic_check_expression cf e in
      match ue.emeta.type_ with
      | UFun _ | UMathLibraryFunction ->
          semantic_error ~loc:ue.emeta.loc "Functions cannot be printed."
      | _ -> PExpr ue )

(* -- Truncations ----------------------------------------------------------- *)

let semantic_check_truncation cf = function
  | NoTruncate -> NoTruncate
  | TruncateUpFrom e ->
      let ue =
        semantic_check_expression_of_int_or_real_type cf e "Truncation bound"
      in
      TruncateUpFrom ue
  | TruncateDownFrom e ->
      let ue =
        semantic_check_expression_of_int_or_real_type cf e "Truncation bound"
      in
      TruncateDownFrom ue
  | TruncateBetween (e1, e2) ->
      let ue1 =
        semantic_check_expression_of_int_or_real_type cf e1 "Truncation bound"
      in
      let ue2 =
        semantic_check_expression_of_int_or_real_type cf e2 "Truncation bound"
      in
      TruncateBetween (ue1, ue2)

(* Probably nothing to do here *)

(* -- Statements ------------------------------------------------------------ *)

let semantic_check_nrfn_target ~loc ~cf id =
  if
    String.is_suffix id.name ~suffix:"_lp"
    && not (cf.in_lp_fun_def || cf.current_block = Model)
  then
    semantic_error ~loc
      "Target can only be accessed in the model block or in definitions of \
       functions with the suffix _lp."
  else ()

let semantic_check_nrfn_normal ~loc id es =
  match Symbol_table.look vm id.name with
  | Some (_, UFun (listedtypes, Void))
    when not (check_compatible_arguments_mod_conv id.name listedtypes es) ->
      semantic_error ~loc
        (Format.sprintf
           "Ill-typed arguments supplied to function '%s'. Available \
            signatures:\n\
            %s\n\
            Instead supplied arguments of incompatible type: %s."
           id.name
           (pretty_print_unsizedtype (UFun (listedtypes, Void)))
           (List.map ~f:type_of_expr_typed es |> pretty_print_unsizedtypes))
  | Some (_, UFun (_, Void)) ->
      mk_typed_statement
        ~stmt:(NRFunApp (UserDefined, id, es))
        ~return_type:NoReturnType ~loc
  | Some (_, UFun (_, ReturnType _)) ->
      semantic_error ~loc
        (Format.sprintf
           "A non-returning function was expected but a returning function \
            '%s' was supplied."
           id.name)
  | Some _ ->
      semantic_error ~loc
        (Format.sprintf
           "A returning function was expected but a non-function value '%s' \
            was supplied."
           id.name)
  | None ->
      semantic_error ~loc
        (Format.sprintf
           "A non-returning function was expected but an undeclared \
            identifier '%s' was supplied."
           id.name)

let semantic_check_nrfn_stan_math ~loc id es =
  match stan_math_returntype id.name es with
  | Some Void ->
      mk_typed_statement
        ~stmt:(NRFunApp (StanLib, id, es))
        ~return_type:NoReturnType ~loc
  | Some (ReturnType _) ->
      semantic_error ~loc
        (Format.sprintf
           "A non-returning function was expected but a returning function \
            '%s' was supplied."
           id.name)
  | None ->
      semantic_error ~loc
        (Format.sprintf
           "Ill-typed arguments supplied to function '%s'. Available \
            signatures: %s\n\
            Instead supplied arguments of incompatible type: %s."
           id.name
           (Stan_math_signatures.pretty_print_all_math_lib_fn_sigs id.name)
           (List.map ~f:type_of_expr_typed es |> pretty_print_unsizedtypes))

let semantic_check_nrfn ~loc id es =
  match fn_kind_from_identifier id with
  | StanLib -> semantic_check_nrfn_stan_math ~loc id es
  | UserDefined -> semantic_check_nrfn_normal ~loc id es

let rec semantic_check_statement cf (s : Ast.untyped_statement) :
    Ast.typed_statement =
  let loc = s.smeta.loc in
  match s.stmt with
  | Assignment
      { assign_identifier= id
      ; assign_indices= lindex
      ; assign_op= assop
      ; assign_rhs= e } -> (
      let ute =
        Ast.mk_untyped_expression
          ~expr:
            (Indexed
               ( Ast.mk_untyped_expression ~expr:(Variable id) ~loc:id.id_loc
               , lindex ))
          ~loc
      in
      let ue2 = semantic_check_expression cf ute in
      let uid, ulindex =
        match ue2 with
        | {expr= Indexed ({expr= Variable uid; _}, ulindex); _} ->
            (uid, ulindex)
        | _ -> fatal_error ()
      in
      let uassop = semantic_check_assignmentoperator assop in
      let ue = semantic_check_expression cf e in
      let uidoblock =
        match Option.map ~f:fst (Symbol_table.look vm uid.name) with
        | Some b -> b
        | None ->
            if is_stan_math_function_name uid.name then MathLibrary
            else fatal_error ()
      in
      let _ =
        if Symbol_table.get_read_only vm uid.name then
          semantic_error ~loc
            ( "Cannot assign to function argument or loop identifier "
            ^ ("'" ^ uid.name ^ "'")
            ^ "." )
      in
      (* Variables from previous blocks are read-only. In particular, data and parameters never assigned to *)
      let _ =
        if
          (not (Symbol_table.is_global vm uid.name))
          || uidoblock = cf.current_block
        then ()
        else
          semantic_error ~loc
            ( "Cannot assign to global variable "
            ^ ("'" ^ uid.name ^ "'")
            ^ " declared in previous blocks." )
      in
      let opname = Sexp.to_string (sexp_of_assignmentoperator uassop) in
      match operator_return_type_from_string opname [ue2; ue] with
      | Some Void ->
          mk_typed_statement ~return_type:NoReturnType ~loc
            ~stmt:
              (Assignment
                 { assign_identifier= uid
                 ; assign_indices= ulindex
                 ; assign_op= uassop
                 ; assign_rhs= ue })
      (* Check that assignments are type consistent *)
      | None | Some (ReturnType _) ->
          let lhs_type = pretty_print_unsizedtype ue2.emeta.type_
          and rhs_type = pretty_print_unsizedtype ue.emeta.type_ in
          semantic_error ~loc
            ( "Ill-typed arguments supplied to assignment operator "
            ^ pretty_print_assignmentoperator uassop
            ^ ": lhs has type " ^ lhs_type ^ " and rhs has type " ^ rhs_type
            ^
            if uassop <> Assign && uassop <> ArrowAssign then
              ". Available signatures:"
              ^ pretty_print_all_operator_signatures opname
            else "" ) )
  | NRFunApp (_, id, es) ->
      let uid = semantic_check_identifier id in
      let ues = List.map ~f:(semantic_check_expression cf) es in
      semantic_check_nrfn_target ~loc ~cf uid ;
      semantic_check_nrfn ~loc uid ues
  | TargetPE e ->
      let ue = semantic_check_expression cf e in
      (* Check typing of ~ and target += *)
      let _ =
        match ue.emeta.type_ with
        | UFun _ | UMathLibraryFunction ->
            semantic_error ~loc
              "A (container of) reals or ints needs to be supplied to \
               increment target."
        | _ -> ()
      in
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      let _ =
        if not (cf.in_lp_fun_def || cf.current_block = Model) then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      mk_typed_statement ~stmt:(TargetPE ue) ~return_type:NoReturnType ~loc
  | IncrementLogProb e ->
      let ue = semantic_check_expression cf e in
      let _ =
        match ue.emeta.type_ with
        | UFun _ | UMathLibraryFunction ->
            semantic_error ~loc
              "A (container of) reals or ints needs to be supplied to \
               increment target."
        | _ -> ()
      in
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      let _ =
        if not (cf.in_lp_fun_def || cf.current_block = Model) then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      mk_typed_statement ~stmt:(IncrementLogProb ue) ~return_type:NoReturnType
        ~loc
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
      let distribution_name_is_defined name argumenttypes =
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
      let cumulative_density_is_defined name argumenttypes =
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
  | Break ->
      (* Break and continue only occur in loops. *)
      let _ =
        if cf.loop_depth = 0 then
          semantic_error ~loc "Break statements may only be used in loops."
      in
      mk_typed_statement ~stmt:Break ~return_type:NoReturnType ~loc
  | Continue ->
      (* Break and continue only occur in loops. *)
      let _ =
        if cf.loop_depth = 0 then
          semantic_error ~loc "Continue statements may only be used in loops."
      in
      mk_typed_statement ~stmt:Continue ~return_type:NoReturnType ~loc
  | Return e ->
      (* No returns outside of function definitions *)
      (* In case of void function, no return statements anywhere *)
      let _ =
        if not cf.in_returning_fun_def then
          semantic_error ~loc
            "Expression return statements may only be used inside returning \
             function definitions."
      in
      let ue = semantic_check_expression cf e in
      mk_typed_statement ~stmt:(Return ue)
        ~return_type:(Complete (ReturnType ue.emeta.type_)) ~loc
  | ReturnVoid ->
      let _ =
        if (not cf.in_fun_def) || cf.in_returning_fun_def then
          semantic_error ~loc
            "Void return statements may only be used inside non-returning \
             function definitions."
      in
      mk_typed_statement ~stmt:ReturnVoid ~return_type:(Complete Void) ~loc
  | Print ps ->
      let ups = List.map ~f:(semantic_check_printable cf) ps in
      mk_typed_statement ~stmt:(Print ups) ~return_type:NoReturnType ~loc
  | Reject ps ->
      let ups = List.map ~f:(semantic_check_printable cf) ps in
      mk_typed_statement ~stmt:(Reject ups) ~return_type:AnyReturnType ~loc
  | Skip -> mk_typed_statement ~stmt:Skip ~return_type:NoReturnType ~loc
  | IfThenElse (e, s1, os2) ->
      let ue =
        semantic_check_expression_of_int_or_real_type cf e
          "Condition in conditional"
      in
      (* For, while, for each, if constructs take expressions of valid type *)
      let us1 = semantic_check_statement cf s1 in
      let uos2 = Option.map ~f:(semantic_check_statement cf) os2 in
      let srt1 = us1.smeta.return_type in
      let srt2 =
        match uos2 with
        | None -> NoReturnType
        | Some us2 -> us2.smeta.return_type
      in
      let srt = try_compute_ifthenelse_statement_returntype loc srt1 srt2 in
      mk_typed_statement
        ~stmt:(IfThenElse (ue, us1, uos2))
        ~return_type:srt ~loc
  | While (e, s) ->
      let ue =
        semantic_check_expression_of_int_or_real_type cf e
          "Condition in while-loop"
      in
      (* For, while, for each, if constructs take expressions of valid type *)
      let us =
        semantic_check_statement {cf with loop_depth= cf.loop_depth + 1} s
      in
      mk_typed_statement
        ~stmt:(While (ue, us))
        ~return_type:us.smeta.return_type ~loc
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
      let _ = Symbol_table.begin_scope vm in
      let uvdsl = List.map ~f:(semantic_check_statement cf) vdsl in
      let _ = Symbol_table.end_scope vm in
      (* Any statements after a break or continue or return or reject do not count for the return
       type. *)
      let rec list_until_escape = function
        | x1 :: ({stmt; _} as r) :: tl -> (
          match stmt with
          | Break | Continue | Reject _ | Return _ | ReturnVoid -> [x1; r]
          | _ -> x1 :: list_until_escape (r :: tl) )
        | x -> x
      in
      let return_type =
        List.fold_left
          ~f:(try_compute_block_statement_returntype loc)
          ~init:NoReturnType
          (List.map ~f:(fun x -> x.smeta.return_type) (list_until_escape uvdsl))
      in
      mk_typed_statement ~stmt:(Block uvdsl) ~return_type ~loc
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
        | Mir.SVector ue -> not_ptq ue (fun () -> true)
        | SRowVector ue -> not_ptq ue (fun () -> true)
        | SMatrix (ue1, ue2) ->
            not_ptq ue1 (fun () ->
                match ue2.emeta.ad_level with
                | AutoDiffable -> false
                | _ -> true )
        | SArray (ust2, ue) ->
            not_ptq ue (fun () -> check_sizes_data_only ust2)
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
                , semantic_check_identifier id ) )
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
          | Some Mir.UReal
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
          | Some Mir.UInt | Some (UArray UInt) -> ()
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
               | Mir.DataOnly, ut -> (Data, ut)
               | AutoDiffable, ut -> (Param, ut) )
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
