(** Semantic validation of AST*)

(* Idea: check many of things related to identifiers that are hard to check
during parsing and are in fact irrelevant for building up the parse tree *)

open Core_kernel
open Symbol_table
open Ast
open Stan_math_signatures
open Operators
open Errors
open Type_conversion
open Pretty_printing

(* There is a semantic checking function for each AST node that calls
   the checking functions for its children left to right. *)

(* Top level function semantic_check_program declares the AST while operating
   on (1) a global symbol table vm, and (2) structure of type context_flags_record
   to communicate information down the AST. *)

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

let type_of_expr_typed ue = ue.expr_typed_type

let rec unsizedtype_contains_int ut =
  match ut with
  | UInt -> true
  | UArray ut -> unsizedtype_contains_int ut
  | _ -> false

let rec unsizedtype_of_sizedtype = function
  | SInt -> UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix (_, _) -> UMatrix
  | SArray (st, _) -> UArray (unsizedtype_of_sizedtype st)

let rec lub_ad_type = function
  | [] -> DataOnly
  | x :: xs ->
      let y = lub_ad_type xs in
      if compare_autodifftype x y < 0 then y else x

let calculate_autodifftype at ut =
  match at with
  | (Param | TParam | Model) when not (unsizedtype_contains_int ut) ->
      AutoDiffable
  | _ -> DataOnly

let has_int_type ue = ue.expr_typed_type = UInt
let has_int_array_type ue = ue.expr_typed_type = UArray UInt

let has_int_or_real_type ue =
  match ue.expr_typed_type with UInt | UReal -> true | _ -> false

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
  | ReturnType UReal, ReturnType UInt | ReturnType UInt, ReturnType UReal ->
      ReturnType UReal
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

(* The actual semantic checks for all AST nodes! *)
let rec semantic_check_program
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
      semantic_error ~loc:(List.hd_exn (Option.value_exn ufb)).stmt_typed_loc
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

and semantic_check_identifier id =
  let _ =
    if id.name = !model_name then
      Errors.semantic_error ~loc:id.id_loc
        ("Identifier " ^ ("'" ^ id.name ^ "'") ^ " clashes with model name.")
  in
  let _ =
    if
      String.is_suffix id.name ~suffix:"__"
      || List.exists
           ~f:(fun str -> str = id.name)
           [ "true"; "false"; "repeat"; "until"; "then"; "var"; "fvar"
           ; "STAN_MAJOR"; "STAN_MINOR"; "STAN_PATCH"; "STAN_MATH_MAJOR"
           ; "STAN_MATH_MINOR"; "STAN_MATH_PATCH"; "alignas"; "alignof"; "and"
           ; "and_eq"; "asm"; "auto"; "bitand"; "bitor"; "bool"; "break"
           ; "case"; "catch"; "char"; "char16_t"; "char32_t"; "class"; "compl"
           ; "const"; "constexpr"; "const_cast"; "continue"; "decltype"
           ; "default"; "delete"; "do"; "double"; "dynamic_cast"; "else"
           ; "enum"; "explicit"; "export"; "extern"; "false"; "float"; "for"
           ; "friend"; "goto"; "if"; "inline"; "int"; "long"; "mutable"
           ; "namespace"; "new"; "noexcept"; "not"; "not_eq"; "nullptr"
           ; "operator"; "or"; "or_eq"; "private"; "protected"; "public"
           ; "register"; "reinterpret_cast"; "return"; "short"; "signed"
           ; "sizeof"; "static"; "static_assert"; "static_cast"; "struct"
           ; "switch"; "template"; "this"; "thread_local"; "throw"; "true"
           ; "try"; "typedef"; "typeid"; "typename"; "union"; "unsigned"
           ; "using"; "virtual"; "void"; "volatile"; "wchar_t"; "while"; "xor"
           ; "xor_eq" ]
    then
      semantic_error ~loc:id.id_loc
        ( "Identifier "
        ^ ("'" ^ id.name ^ "'")
        ^ " clashes with reserved keyword." )
  in
  id

(* Probably nothing to do here *)
and semantic_check_autodifftype at = at

(* Probably nothing to do here *)
and semantic_check_returntype = function
  | Void -> Void
  | ReturnType ut -> ReturnType (semantic_check_unsizedtype ut)

(* Probably nothing to do here *)
and semantic_check_unsizedtype = function
  | UArray ut -> UArray (semantic_check_unsizedtype ut)
  | UFun (l, rt) ->
      UFun
        ( List.map
            ~f:(fun (at, ut) ->
              (semantic_check_autodifftype at, semantic_check_unsizedtype ut)
              )
            l
        , semantic_check_returntype rt )
  | ut -> ut

and semantic_error_e {expr_typed_loc; _} msg =
  semantic_error ~loc:expr_typed_loc msg

and semantic_check_sizedtype cf = function
  | SInt -> SInt
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

and semantic_check_transformation cf = function
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

and lub_ad_e exprs =
  lub_ad_type (List.map ~f:(fun x -> x.expr_typed_ad_level) exprs)

and semantic_check_expression_of_int_or_real_type cf e name =
  let ue = semantic_check_expression cf e in
  let _ =
    if not (has_int_or_real_type ue) then
      semantic_error_e ue
        ( name ^ " must be of type int or real. Instead found type "
        ^ pretty_print_unsizedtype ue.expr_typed_type
        ^ "." )
  in
  ue

and semantic_check_expression_of_int_type cf e name =
  let ue = semantic_check_expression cf e in
  let _ =
    if not (has_int_type ue) then
      semantic_error_e ue
        ( name ^ " must be of type int. Instead found type "
        ^ pretty_print_unsizedtype ue.expr_typed_type
        ^ "." )
  in
  ue

and inferred_unsizedtype_of_indexed loc ut typed_indexl =
  let recurse = inferred_unsizedtype_of_indexed loc in
  match (ut, typed_indexl) with
  (* Here, we need some special logic to deal with row and column vectors
     properly. *)
  | UMatrix, [(All, _); (Single _, UInt)]
   |UMatrix, [(Upfrom _, _); (Single _, UInt)]
   |UMatrix, [(Downfrom _, _); (Single _, UInt)]
   |UMatrix, [(Between _, _); (Single _, UInt)]
   |UMatrix, [(Single _, UArray UInt); (Single _, UInt)] ->
      UVector
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

and semantic_check_expression cf {expr_untyped_loc= loc; expr_untyped} =
  match expr_untyped with
  | TernaryIf (e1, e2, e3) -> (
      let ue1 = semantic_check_expression cf e1 in
      let ue2 = semantic_check_expression cf e2 in
      let ue3 = semantic_check_expression cf e3 in
      match operator_return_type_from_string ternary_if [ue1; ue2; ue3] with
      | Some (ReturnType ut) ->
          { expr_typed= TernaryIf (ue1, ue2, ue3)
          ; expr_typed_ad_level= lub_ad_e [ue1; ue2; ue3]
          ; expr_typed_type= ut
          ; expr_typed_loc= loc }
      | Some Void | None ->
          semantic_error ~loc
            ( "Ill-typed arguments supplied to ? : operator. Available \
               signatures: "
            ^ pretty_print_all_operator_signatures ternary_if
            ^ "\nInstead supplied arguments of incompatible type: "
            ^ pretty_print_unsizedtype ue1.expr_typed_type
            ^ ", "
            ^ pretty_print_unsizedtype ue2.expr_typed_type
            ^ ", "
            ^ pretty_print_unsizedtype ue3.expr_typed_type
            ^ "." ) )
  | BinOp (e1, op, e2) -> (
      let ue1 = semantic_check_expression cf e1
      and uop = semantic_check_operator op
      and ue2 = semantic_check_expression cf e2 in
      match operator_return_type uop [ue1; ue2] with
      | Some (ReturnType ut) ->
          { expr_typed= BinOp (ue1, uop, ue2)
          ; expr_typed_ad_level= lub_ad_e [ue1; ue2]
          ; expr_typed_type= ut
          ; expr_typed_loc= loc }
      | Some Void | None ->
          semantic_error ~loc
            ( "Ill-typed arguments supplied to infix operator "
            ^ pretty_print_operator uop ^ ". Available signatures: "
            ^ pretty_print_all_operator_signatures (operator_name uop)
            ^ "\nInstead supplied arguments of incompatible type: "
            ^ pretty_print_unsizedtype ue1.expr_typed_type
            ^ ", "
            ^ pretty_print_unsizedtype ue2.expr_typed_type
            ^ "." ) )
  | PrefixOp (op, e) -> (
      let uop = semantic_check_operator op
      and ue = semantic_check_expression cf e in
      match operator_return_type uop [ue] with
      | Some (ReturnType ut) ->
          { expr_typed= PrefixOp (uop, ue)
          ; expr_typed_ad_level= lub_ad_e [ue]
          ; expr_typed_type= ut
          ; expr_typed_loc= loc }
      | Some Void | None ->
          semantic_error ~loc
            ( "Ill-typed arguments supplied to prefix operator "
            ^ pretty_print_operator uop ^ ". Available signatures: "
            ^ pretty_print_all_operator_signatures (operator_name uop)
            ^ "\nInstead supplied argument of incompatible type: "
            ^ pretty_print_unsizedtype ue.expr_typed_type
            ^ "." ) )
  | PostfixOp (e, op) -> (
      let ue = semantic_check_expression cf e in
      let uop = semantic_check_operator op in
      match operator_return_type op [ue] with
      | Some (ReturnType ut) ->
          { expr_typed= PostfixOp (ue, uop)
          ; expr_typed_ad_level= lub_ad_e [ue]
          ; expr_typed_type= ut
          ; expr_typed_loc= loc }
      | Some Void | None ->
          semantic_error ~loc
            ( "Ill-typed arguments supplied to postfix operator "
            ^ pretty_print_operator uop ^ ". Available signatures: "
            ^ pretty_print_all_operator_signatures (operator_name op)
            ^ "\nInstead supplied argument of incompatible type: "
            ^ pretty_print_unsizedtype ue.expr_typed_type
            ^ "." ) )
  | Variable id ->
      let uid = semantic_check_identifier id in
      let ut = Symbol_table.look vm id.name in
      (* Check that variable in scope if used  *)
      let _ =
        if ut = None && not (is_stan_math_function_name uid.name) then
          semantic_error ~loc
            ("Identifier " ^ ("'" ^ uid.name ^ "'") ^ " not in scope.")
      and originblock, type_ =
        Option.value ~default:(MathLibrary, UMathLibraryFunction) ut
      in
      { expr_typed= Variable uid
      ; expr_typed_ad_level= calculate_autodifftype originblock type_
      ; expr_typed_type= type_
      ; expr_typed_loc= loc }
  | IntNumeral s ->
      { expr_typed= IntNumeral s
      ; expr_typed_ad_level= DataOnly
      ; expr_typed_type= UInt
      ; expr_typed_loc= loc }
  | RealNumeral s ->
      { expr_typed= RealNumeral s
      ; expr_typed_ad_level= DataOnly
      ; expr_typed_type= UReal
      ; expr_typed_loc= loc }
  | FunApp (id, es) -> (
      let uid = semantic_check_identifier id
      and ues = List.map ~f:(semantic_check_expression cf) es in
      let _ =
        if uid.name = "map_rect" then
          match ues with
          | {expr_typed= Variable arg1_name; _} :: _ ->
              if
                String.is_suffix arg1_name.name ~suffix:"_lp"
                || String.is_suffix arg1_name.name ~suffix:"_rng"
              then
                semantic_error ~loc
                  ( "Mapped function cannot be an _rng or _lp function, found \
                     function name: " ^ arg1_name.name )
          | _ -> ()
      in
      let open String in
      let open Pervasives in
      let _ =
        if
          List.exists
            ~f:(fun x -> is_suffix uid.name ~suffix:x)
            ["_lpdf"; "_lpmf"; "_lcdf"; "_lccdf"]
        then
          semantic_error ~loc
            "Probabilty functions with suffixes _lpdf, _lpmf, _lcdf, and \
             _lccdf, require a vertical bar (|) between the first two \
             arguments."
      in
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      let _ =
        if
          is_suffix uid.name ~suffix:"_lp"
          && not (cf.in_lp_fun_def || cf.current_block = Model)
        then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      (* Rng functions cannot be used in Tp or Model and only in funciton defs with the right suffix *)
      let _ =
        if
          is_suffix uid.name ~suffix:"_rng"
          && ( (cf.in_fun_def && not cf.in_rng_fun_def)
             || cf.current_block = TParam || cf.current_block = Model )
        then
          semantic_error ~loc
            "Random number generators are only allowed in transformed data \
             block, generated quantities block or user-defined functions with \
             names ending in _rng."
      in
      let returnblock = lub_ad_e ues in
      (* Function applications are returning functions *)
      match stan_math_returntype uid.name ues with
      | Some Void ->
          semantic_error ~loc
            ( "A returning function was expected but a non-returning function "
            ^ ("'" ^ uid.name ^ "'")
            ^ " was supplied." )
      | Some (ReturnType ut) ->
          { expr_typed= FunApp (uid, ues)
          ; expr_typed_ad_level= returnblock
          ; expr_typed_type= ut
          ; expr_typed_loc= loc }
      (* Check that function arguments match signature  *)
      (* Also check whether function arguments meet data requirement. *)
      | None -> (
          let _ =
            if is_stan_math_function_name uid.name then
              semantic_error ~loc
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
              semantic_error ~loc
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
                  semantic_error ~loc
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
              { expr_typed= FunApp (uid, ues)
              ; expr_typed_ad_level= returnblock
              ; expr_typed_type= ut
              ; expr_typed_loc= loc }
          | Some _ ->
              (* Check that Funaps are actually functions *)
              semantic_error ~loc
                ( "A returning function was expected but a non-function value "
                ^ ("'" ^ uid.name ^ "'")
                ^ " was supplied." )
          | None ->
              semantic_error ~loc
                ( "A returning function was expected but an undeclared \
                   identifier "
                ^ ("'" ^ uid.name ^ "'")
                ^ " was supplied." ) ) )
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
          semantic_error ~loc
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
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      let returnblock = lub_ad_e ues in
      match stan_math_returntype uid.name ues with
      | Some Void ->
          semantic_error ~loc
            ( "A returning function was expected but a non-returning function "
            ^ ("'" ^ uid.name ^ "'")
            ^ " was supplied." )
      | Some (ReturnType ut) ->
          { expr_typed= CondDistApp (uid, ues)
          ; expr_typed_ad_level= returnblock
          ; expr_typed_type= ut
          ; expr_typed_loc= loc }
      (* Check that function arguments match signature  *)
      (* Also check whether function arguments meet data requirement. *)
      | None -> (
          let _ =
            if is_stan_math_function_name uid.name then
              semantic_error ~loc
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
              semantic_error ~loc
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
                  semantic_error ~loc
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
              { expr_typed= CondDistApp (uid, ues)
              ; expr_typed_ad_level= returnblock
              ; expr_typed_type= ut
              ; expr_typed_loc= loc }
          | Some _ ->
              (* Check that Funaps are actually functions *)
              semantic_error ~loc
                ( "A returning function was expected but a non-function value "
                ^ ("'" ^ uid.name ^ "'")
                ^ " was supplied." )
          | None ->
              semantic_error ~loc
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
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      { expr_typed= GetLP
      ; expr_typed_ad_level= calculate_autodifftype cf.current_block UReal
      ; expr_typed_type= UReal
      ; expr_typed_loc= loc }
  | GetTarget ->
      (* Target+= can only be used in model and functions with right suffix (same for tilde etc) *)
      let _ =
        if
          not
            ( cf.in_lp_fun_def || cf.current_block = Model
            || cf.current_block = TParam )
        then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      { expr_typed= GetTarget
      ; expr_typed_ad_level= calculate_autodifftype cf.current_block UReal
      ; expr_typed_type= UReal
      ; expr_typed_loc= loc }
  | ArrayExpr es ->
      let ues = List.map ~f:(semantic_check_expression cf) es in
      let elementtypes = List.map ~f:(fun y -> y.expr_typed_type) ues in
      (* Array expressions must be of uniform type. (Or mix of int and real) *)
      let _ =
        if
          List.exists
            ~f:(fun x ->
              not
                ( check_of_same_type_mod_array_conv "" x.expr_typed_type
                    (List.hd_exn ues).expr_typed_type
                || check_of_same_type_mod_array_conv ""
                     (List.hd_exn ues).expr_typed_type x.expr_typed_type ) )
            ues
        then
          semantic_error ~loc
            "Array expression must have entries of consistent type."
      in
      let array_type =
        if List.exists ~f:(fun x -> List.hd_exn elementtypes <> x) elementtypes
        then UArray UReal
        else UArray (List.hd_exn elementtypes)
      in
      let returnblock = lub_ad_e ues in
      { expr_typed= ArrayExpr ues
      ; expr_typed_ad_level= returnblock
      ; expr_typed_type= array_type
      ; expr_typed_loc= loc }
  | RowVectorExpr es ->
      let ues = List.map ~f:(semantic_check_expression cf) es in
      let elementtypes = List.map ~f:(fun y -> y.expr_typed_type) ues in
      let ut =
        if List.for_all ~f:(fun x -> x = UReal || x = UInt) elementtypes then
          URowVector
        else if List.for_all ~f:(fun x -> x = URowVector) elementtypes then
          UMatrix
        else
          semantic_error ~loc
            "Row_vector expression must have all int and real entries or all \
             row_vector entries."
      in
      let returnblock = lub_ad_e ues in
      { expr_typed= RowVectorExpr ues
      ; expr_typed_ad_level= returnblock
      ; expr_typed_type= ut
      ; expr_typed_loc= loc }
  | Paren e ->
      let ue = semantic_check_expression cf e in
      { expr_typed= Paren ue
      ; expr_typed_ad_level= ue.expr_typed_ad_level
      ; expr_typed_type= ue.expr_typed_type
      ; expr_typed_loc= loc }
  | Indexed (e, indices) ->
      let ue = semantic_check_expression cf e in
      let uindices = List.map ~f:(semantic_check_index cf) indices in
      let uindices_with_types =
        List.map
          ~f:(function
            | Single e as i -> (i, e.expr_typed_type) | i -> (i, UInt))
          uindices
      in
      let inferred_ad_type_of_indexed at uindices =
        lub_ad_type
          ( at
          :: List.map
               ~f:(function
                 | All -> DataOnly
                 | Single ue1 | Upfrom ue1 | Downfrom ue1 ->
                     lub_ad_type [at; ue1.expr_typed_ad_level]
                 | Between (ue1, ue2) ->
                     lub_ad_type
                       [at; ue1.expr_typed_ad_level; ue2.expr_typed_ad_level])
               uindices )
      in
      let at = inferred_ad_type_of_indexed ue.expr_typed_ad_level uindices
      and ut =
        inferred_unsizedtype_of_indexed loc ue.expr_typed_type
          uindices_with_types
      in
      { expr_typed= Indexed (ue, uindices)
      ; expr_typed_ad_level= at
      ; expr_typed_type= ut
      ; expr_typed_loc= loc }

(* Probably nothing to do here *)
and semantic_check_operator i = i

and semantic_check_printable cf = function
  | PString s -> PString s
  (* Print/reject expressions cannot be of function type. *)
  | PExpr e -> (
      let ue = semantic_check_expression cf e in
      match ue.expr_typed_type with
      | UFun _ | UMathLibraryFunction ->
          semantic_error ~loc:ue.expr_typed_loc "Functions cannot be printed."
      | _ -> PExpr ue )

and semantic_check_statement cf s =
  let loc = s.stmt_untyped_loc in
  match s.stmt_untyped with
  | Assignment
      { assign_identifier= id
      ; assign_indices= lindex
      ; assign_op= assop
      ; assign_rhs= e } -> (
      let ue2 =
        semantic_check_expression cf
          { expr_untyped=
              Indexed
                ( {expr_untyped= Variable id; expr_untyped_loc= id.id_loc}
                , lindex )
          ; expr_untyped_loc= loc }
      in
      let uid, ulindex =
        match ue2 with
        | {expr_typed= Indexed ({expr_typed= Variable uid; _}, ulindex); _} ->
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
          { stmt_typed=
              Assignment
                { assign_identifier= uid
                ; assign_indices= ulindex
                ; assign_op= uassop
                ; assign_rhs= ue }
          ; stmt_typed_returntype= NoReturnType
          ; stmt_typed_loc= loc }
      (* Check that assignments are type consistent *)
      | None | Some (ReturnType _) ->
          let lhs_type = pretty_print_unsizedtype ue2.expr_typed_type
          and rhs_type = pretty_print_unsizedtype ue.expr_typed_type in
          semantic_error ~loc
            ( "Ill-typed arguments supplied to assignment operator "
            ^ pretty_print_assignmentoperator uassop
            ^ ": lhs has type " ^ lhs_type ^ " and rhs has type " ^ rhs_type
            ^
            if uassop <> Assign && uassop <> ArrowAssign then
              ". Available signatures:"
              ^ pretty_print_all_operator_signatures opname
            else "" ) )
  | NRFunApp (id, es) -> (
      let uid = semantic_check_identifier id in
      let ues = List.map ~f:(semantic_check_expression cf) es in
      let _ =
        if
          String.is_suffix uid.name ~suffix:"_lp"
          && not (cf.in_lp_fun_def || cf.current_block = Model)
        then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      match stan_math_returntype uid.name ues with
      | Some Void ->
          { stmt_typed= NRFunApp (uid, ues)
          ; stmt_typed_returntype= NoReturnType
          ; stmt_typed_loc= loc }
      (* Check that NRFunction applications are non-returning functions *)
      | Some (ReturnType _) ->
          semantic_error ~loc
            ( "A non-returning function was expected but a returning function "
            ^ ("'" ^ uid.name ^ "'")
            ^ " was supplied." )
      | None -> (
          let _ =
            if is_stan_math_function_name uid.name then
              semantic_error ~loc
                {| "Ill-typed arguments supplied to function "
                    ("'" ^ uid.name ^ "'")
                    ". Available signatures: "
                    pretty_print_all_math_lib_fn_sigs uid.name
                    "\nInstead supplied arguments of incompatible type: "
                    pretty_print_unsizedtypes (List.map ~f: type_of_expr_typed ues)
                    "."  |}
          in
          match Symbol_table.look vm uid.name with
          | Some (_, UFun (listedtypes, Void)) ->
              let _ =
                if
                  not
                    (check_compatible_arguments_mod_conv uid.name listedtypes
                       ues)
                then
                  semantic_error ~loc
                    ( "Ill-typed arguments supplied to function "
                    ^ ("'" ^ uid.name ^ "'")
                    ^ ". Available signatures:\n"
                    ^ pretty_print_unsizedtype (UFun (listedtypes, Void))
                    ^ "\nInstead supplied arguments of incompatible type: "
                    ^ pretty_print_unsizedtypes
                        (List.map ~f:type_of_expr_typed ues)
                    ^ "." )
              in
              { stmt_typed= NRFunApp (uid, ues)
              ; stmt_typed_returntype= NoReturnType
              ; stmt_typed_loc= loc }
          | Some (_, UFun (_, ReturnType _)) ->
              semantic_error ~loc
                ( "A non-returning function was expected but a returning \
                   function "
                ^ ("'" ^ uid.name ^ "'")
                ^ " was supplied." )
          | Some _ ->
              semantic_error ~loc
                ( "A non-returning function was expected but a non-function \
                   value "
                ^ ("'" ^ uid.name ^ "'")
                ^ " was supplied." )
          | None ->
              semantic_error ~loc
                ( "A non-returning function was expected but an undeclared \
                   identifier "
                ^ ("'" ^ uid.name ^ "'")
                ^ " was supplied." ) ) )
  | TargetPE e ->
      let ue = semantic_check_expression cf e in
      (* Check typing of ~ and target += *)
      let _ =
        match ue.expr_typed_type with
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
      { stmt_typed= TargetPE ue
      ; stmt_typed_returntype= NoReturnType
      ; stmt_typed_loc= loc }
  | IncrementLogProb e ->
      let ue = semantic_check_expression cf e in
      let _ =
        match ue.expr_typed_type with
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
      { stmt_typed= IncrementLogProb ue
      ; stmt_typed_returntype= NoReturnType
      ; stmt_typed_loc= loc }
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
      { stmt_typed=
          Tilde {arg= ue; distribution= uid; args= ues; truncation= ut}
      ; stmt_typed_returntype= NoReturnType
      ; stmt_typed_loc= loc }
  | Break ->
      (* Break and continue only occur in loops. *)
      let _ =
        if cf.loop_depth = 0 then
          semantic_error ~loc "Break statements may only be used in loops."
      in
      { stmt_typed= Break
      ; stmt_typed_returntype= NoReturnType
      ; stmt_typed_loc= loc }
  | Continue ->
      (* Break and continue only occur in loops. *)
      let _ =
        if cf.loop_depth = 0 then
          semantic_error ~loc "Continue statements may only be used in loops."
      in
      { stmt_typed= Continue
      ; stmt_typed_returntype= NoReturnType
      ; stmt_typed_loc= loc }
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
      { stmt_typed= Return ue
      ; stmt_typed_returntype= Complete (ReturnType ue.expr_typed_type)
      ; stmt_typed_loc= loc }
  | ReturnVoid ->
      let _ =
        if (not cf.in_fun_def) || cf.in_returning_fun_def then
          semantic_error ~loc
            "Void return statements may only be used inside non-returning \
             function definitions."
      in
      { stmt_typed= ReturnVoid
      ; stmt_typed_returntype= Complete Void
      ; stmt_typed_loc= loc }
  | Print ps ->
      let ups = List.map ~f:(semantic_check_printable cf) ps in
      { stmt_typed= Print ups
      ; stmt_typed_returntype= NoReturnType
      ; stmt_typed_loc= loc }
  | Reject ps ->
      let ups = List.map ~f:(semantic_check_printable cf) ps in
      { stmt_typed= Reject ups
      ; stmt_typed_returntype= AnyReturnType
      ; stmt_typed_loc= loc }
  | Skip ->
      { stmt_typed= Skip
      ; stmt_typed_returntype= NoReturnType
      ; stmt_typed_loc= loc }
  | IfThenElse (e, s1, os2) ->
      let ue =
        semantic_check_expression_of_int_or_real_type cf e
          "Condition in conditional"
      in
      (* For, while, for each, if constructs take expressions of valid type *)
      let us1 = semantic_check_statement cf s1 in
      let uos2 = Option.map ~f:(semantic_check_statement cf) os2 in
      let srt1 = us1.stmt_typed_returntype in
      let srt2 =
        match uos2 with
        | None -> NoReturnType
        | Some us2 -> us2.stmt_typed_returntype
      in
      let srt = try_compute_ifthenelse_statement_returntype loc srt1 srt2 in
      { stmt_typed= IfThenElse (ue, us1, uos2)
      ; stmt_typed_loc= loc
      ; stmt_typed_returntype= srt }
  | While (e, s) ->
      let ue =
        semantic_check_expression_of_int_or_real_type cf e
          "Condition in while-loop"
      in
      (* For, while, for each, if constructs take expressions of valid type *)
      let us =
        semantic_check_statement {cf with loop_depth= cf.loop_depth + 1} s
      in
      { stmt_typed= While (ue, us)
      ; stmt_typed_returntype= us.stmt_typed_returntype
      ; stmt_typed_loc= loc }
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
      { stmt_typed=
          For
            { loop_variable= uid
            ; lower_bound= ue1
            ; upper_bound= ue2
            ; loop_body= us }
      ; stmt_typed_returntype= us.stmt_typed_returntype
      ; stmt_typed_loc= loc }
  | ForEach (id, e, s) ->
      let uid = semantic_check_identifier id in
      let ue = semantic_check_expression cf e in
      (* For, while, for each, if constructs take expressions of valid type *)
      let loop_identifier_unsizedtype =
        match ue.expr_typed_type with
        | UArray ut -> ut
        | UVector | URowVector | UMatrix -> UReal
        | _ ->
            semantic_error ~loc:ue.expr_typed_loc
              ( "Foreach-loop must be over array, vector, row_vector or \
                 matrix. Instead found expression of type "
              ^ pretty_print_unsizedtype ue.expr_typed_type
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
      { stmt_typed= ForEach (uid, ue, us)
      ; stmt_typed_returntype= us.stmt_typed_returntype
      ; stmt_typed_loc= loc }
  | Block vdsl ->
      let _ = Symbol_table.begin_scope vm in
      let uvdsl = List.map ~f:(semantic_check_statement cf) vdsl in
      let _ = Symbol_table.end_scope vm in
      (* Any statements after a break or continue or return or reject do not count for the return
      type. *)
      let rec list_until_escape = function
        | x1 :: ({stmt_typed; _} as r) :: tl -> (
          match stmt_typed with
          | Break | Continue | Reject _ | Return _ | ReturnVoid -> [x1; r]
          | _ -> x1 :: list_until_escape (r :: tl) )
        | x -> x
      in
      { stmt_typed= Block uvdsl
      ; stmt_typed_returntype=
          List.fold_left
            ~f:(try_compute_block_statement_returntype loc)
            ~init:NoReturnType
            (List.map
               ~f:(fun x -> x.stmt_typed_returntype)
               (list_until_escape uvdsl))
      ; stmt_typed_loc= loc }
  | VarDecl
      { sizedtype= st
      ; transformation= trans
      ; identifier= id
      ; initial_value= init
      ; is_global= glob } ->
      let ust = semantic_check_sizedtype cf st
      and not_ptq e f =
        match e.expr_typed_ad_level with AutoDiffable -> false | _ -> f ()
      in
      let rec check_sizes_data_only = function
        | SVector ue -> not_ptq ue (fun () -> true)
        | SRowVector ue -> not_ptq ue (fun () -> true)
        | SMatrix (ue1, ue2) ->
            not_ptq ue1 (fun () ->
                match ue2.expr_typed_ad_level with
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
            match ue1.expr_typed_type with UReal -> true | _ -> false )
          | Upper ue1 -> (
            match ue1.expr_typed_type with UReal -> true | _ -> false )
          | LowerUpper (ue1, ue2) -> (
            match ue1.expr_typed_type with
            | UReal -> true
            | _ -> (
              match ue2.expr_typed_type with UReal -> true | _ -> false ) )
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
          match
            semantic_check_statement cf
              { stmt_untyped=
                  Assignment
                    { assign_identifier= id
                    ; assign_indices= []
                    ; assign_op= Assign
                    ; assign_rhs= e }
              ; stmt_untyped_loc= loc }
          with
          | { stmt_typed=
                Assignment
                  { assign_identifier= _
                  ; assign_indices= _
                  ; assign_op= Assign
                  ; assign_rhs= ue }
            ; stmt_typed_returntype= NoReturnType; _ } ->
              Some ue
          | _ -> fatal_error () )
      in
      { stmt_typed=
          VarDecl
            { sizedtype= ust
            ; transformation= utrans
            ; identifier= uid
            ; initial_value= uinit
            ; is_global= glob }
      ; stmt_typed_loc= loc
      ; stmt_typed_returntype= NoReturnType }
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
        | {stmt_untyped= Skip; _} ->
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
          | Some UReal
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
          | Some UInt | Some (UArray UInt) -> ()
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
               | DataOnly, ut -> (Data, ut) | AutoDiffable, ut -> (Param, ut))
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
          || check_of_compatible_return_type urt ub.stmt_typed_returntype
        then ()
        else
          semantic_error ~loc
            "Function bodies must contain a return statement of correct type \
             in every branch."
      in
      let _ = Symbol_table.end_scope vm in
      { stmt_typed=
          FunDef {returntype= urt; funname= uid; arguments= uargs; body= ub}
      ; stmt_typed_returntype= NoReturnType
      ; stmt_typed_loc= loc }

and semantic_check_truncation cf = function
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

and semantic_check_index cf = function
  | All -> All
  (* Check that indexes have int (container) type *)
  | Single e ->
      let ue = semantic_check_expression cf e in
      let loc = ue.expr_typed_loc in
      if has_int_type ue || has_int_array_type ue then Single ue
      else
        semantic_error ~loc
          ( "Index must be of type int or int[] or must be a range. Instead \
             found type "
          ^ pretty_print_unsizedtype ue.expr_typed_type
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

(* Probably nothing to do here *)
and semantic_check_assignmentoperator op = op
