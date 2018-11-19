(** Semantic validation of AST*)

(* Idea: check many of things related to identifiers that are hard to check
during parsing and are in fact irrelevant for building up the parse tree *)

(* To check:
Variable in scope if used 
Variable assigned to before dereferenced 
Target+= can only be used in model and functions with right suffix (same for tilde etc)
Data and parameters never assigned to
Transformed data only assigned in td block
Tp only assigned in Tp block
Parameters and transformed parameters are not int(array) (silly restriction if you ask me)
Model top level variables only assigned and read in model 
Function arguments match signature 
Assignments are type consistent
Indexes have int type 
Expressions take valid number of indices (based on their matrix/array dimensions)
Funaps are actually functions
For, while, for each, if constructs take expressions of valid type
No shadowing!
Check variable not already declared during declaration
Check that function args and loop identifiers are not modified in function. (passed by const ref) 
No returns outside of function definitions?
Rng functions cannot be used in Tp or Model and only in funciton defs with the right suffix
Print/reject expressions cannot be of type void.
Break and continue only occur in loops.
Array expressions must be of uniform type. (Or mix of int and real)
Typing of ~ and target +=
Also check whether function arguments meet data requirement.
Maybe should also infer bounds for every indexing that happens so we know what code to generate for bound checking?
Every trace through function body contains return statement of right type
In case of void function, no return statements anywhere
All function arguments are distinct
Function applications are returning functions
NRFunction applications are non-returning functions
Primitives cannot be printed
Functions cannot be printed
User defined functions cannot be overloaded
Function ending in _lp only where target is available
TODO: Test that user defined functions with probability suffixes have right type.
(Mutual) recursive functions have a definition
Make sure return types of statements involving continue and break are correct.
Make sure data only arguments to functions are checked properly.
Sizes should be of level at most data.
*)

(* TODO: encapsulate some of the state in this file so people don't hurt themselves *)

open Symbol_table
open Ast
open Stan_math_signatures
open Errors

(* Idea: we have a semantic checking function for each AST node.
   Each such calls the corresponding checking functions for its children
   left-to-right. *)

(* NB DANGER: this file specifies an imperative tree algorithm which
   decorates the AST while operating on two bits of state:
   1) a global symbol table vm
   2) some context flags context_flags, to communicate information down
      the AST   *)

let vm = Symbol_table.initialize ()

(* Some imperative context flags that mainly serve to throw semantic errors in a more
   informative location than would be natural if we treated these functionally. *)
type context_flags_record =
  { mutable current_block: originblock
  ; mutable in_fun_def: bool
  ; mutable in_returning_fun_def: bool
  ; mutable in_rng_fun_def: bool
  ; mutable in_lp_fun_def: bool
  ; mutable in_loop: bool }

let context_flags =
  { current_block= Functions
  ; in_fun_def= false
  ; in_returning_fun_def= false
  ; in_rng_fun_def= false
  ; in_lp_fun_def= false
  ; in_loop= false }

(* Some helper functions *)
let dup_exists l =
  let rec dup_consecutive = function
    | [] | [_] -> false
    | x1 :: x2 :: tl -> x1 = x2 || dup_consecutive (x2 :: tl)
  in
  dup_consecutive (List.sort String.compare l)

let typed_expression_unroll = function TypedExpr x -> x

let untyped_expression_unroll = function UntypedExpr x -> x

let typed_statement_unroll = function TypedStmt x -> x

let untyped_statement_unroll = function UntypedStmt x -> x

let rec unsizedtype_contains_int ut =
  match ut with
  | Int -> true
  | Array ut -> unsizedtype_contains_int ut
  | _ -> false

let rec unsizedtype_of_sizedtype = function
  | SInt -> Int
  | SReal -> Real
  | SVector _ -> Vector
  | SRowVector _ -> RowVector
  | SMatrix (_, _) -> Matrix
  | SArray (st, _) -> Array (unsizedtype_of_sizedtype st)

let rec lub_originblock = function
  | [] -> Primitives
  | x :: xs ->
      let y = lub_originblock xs in
      if compare_originblock x y < 0 then y else x

let check_of_int_type ue =
  match (snd (typed_expression_unroll ue)).expr_typed_meta_origin_type with
  | _, Int -> true
  | _ -> false

let check_of_int_array_type ue =
  match (snd (typed_expression_unroll ue)).expr_typed_meta_origin_type with
  | _, Array Int -> true
  | _ -> false

let check_of_int_or_real_type ue =
  match (snd (typed_expression_unroll ue)).expr_typed_meta_origin_type with
  | _, Int -> true
  | _, Real -> true
  | _ -> false

(* TODO: write function that pretty prints all signatures for defined function
   and call that in the appropriate error messages. *)

(* TODO: write function that pretty prints the location of the error messages
   in the way the existing parser does it: print a phrase surrounding the error
   together with a tick to indicate the precise location. *)

let check_fresh_variable id is_nullary_function =
  (* For some strange reason, Stan allows user declared identifiers that are
   not of nullary function types to clash with nullary library functions.
   No other name clashes are tolerated. Here's the logic to
   achieve that. *)
  let _ =
    if
      is_primitive_name id.name
      && ( is_nullary_function
         || try_get_primitive_return_type id.name [] = None )
    then
      let error_msg =
        String.concat " " ["Identifier "; id.name; " clashes with primitive."]
      in
      semantic_error ~loc:id.id_loc error_msg
  in
  match Symbol_table.look vm id.name with
  | Some _ ->
      let error_msg =
        String.concat " " ["Identifier "; id.name; " is already in use."]
      in
      semantic_error ~loc:id.id_loc error_msg
  | None -> ()

(* TODO: the following is very ugly, but we seem to need something like it to
   reproduce the (strange) behaviour in the current Stan that local variables
   have a block level that is determined by what has been assigned to them
   rather than by where they were declared. I'm not sure that behaviour makes
   sense unless we use static analysis as well to make sure these assignments
   actually get evaluated in that phase. *)
let update_originblock name ob =
  match Symbol_table.look vm name with
  | Some (old_ob, ut) ->
      let new_ob = lub_originblock [ob; old_ob] in
      Symbol_table.unsafe_replace vm name (new_ob, ut)
  | _ ->
      semantic_error
        "This should never happen. Please file a bug. Error code 18."

(* The actual semantic checks for all AST nodes! *)
let rec semantic_check_program p =
  match p
  with
  | { functionblock= bf
    ; datablock= bd
    ; transformeddatablock= btd
    ; parametersblock= bp
    ; transformedparametersblock= btp
    ; modelblock= bm
    ; generatedquantitiesblock= bgq }
  ->
    (* We always want to make sure we start with an empty symbol table, in case
       we are processing multiple files in one run. *)
    let _ = unsafe_clear_symbol_table vm in
    let _ = context_flags.current_block <- Functions in
    let ubf =
      Core_kernel.Option.map ~f:(List.map semantic_check_statement) bf
    in
    let _ =
      if Symbol_table.some_fun_is_missing_def vm then
        semantic_error
          ~loc:
            (snd
               (typed_statement_unroll
                  (List.hd
                     ((function
                        | Some x -> x
                        | _ ->
                            semantic_error
                              "This should never happen. Please file a bug. \
                               Eror code 24.")
                        ubf))))
              .stmt_typed_meta_loc
          "Some function is declared without specifying a definition."
    in
    let _ = context_flags.current_block <- Data in
    let ubd =
      Core_kernel.Option.map ~f:(List.map semantic_check_statement) bd
    in
    let _ = context_flags.current_block <- TData in
    let ubtd =
      Core_kernel.Option.map ~f:(List.map semantic_check_statement) btd
    in
    let _ = context_flags.current_block <- Param in
    let ubp =
      Core_kernel.Option.map ~f:(List.map semantic_check_statement) bp
    in
    let _ = context_flags.current_block <- TParam in
    let ubtp =
      Core_kernel.Option.map ~f:(List.map semantic_check_statement) btp
    in
    let _ = context_flags.current_block <- Model in
    let _ = Symbol_table.begin_scope vm in
    let ubm =
      Core_kernel.Option.map ~f:(List.map semantic_check_statement) bm
    in
    let _ = Symbol_table.end_scope vm in
    let _ = context_flags.current_block <- GQuant in
    let ubgq =
      Core_kernel.Option.map ~f:(List.map semantic_check_statement) bgq
    in
    { functionblock= ubf
    ; datablock= ubd
    ; transformeddatablock= ubtd
    ; parametersblock= ubp
    ; transformedparametersblock= ubtp
    ; modelblock= ubm
    ; generatedquantitiesblock= ubgq }

(* This could also be dealt with during lexing. That would probably be more efficient. *)
and semantic_check_identifier id =
  let _ =
    match id.id_loc with
    | Nowhere ->
        semantic_error
          "This should never happen. Please file a bug. Error code 25."
    | Errors.Location (startpos, _) ->
        let modelname =
          List.hd
            (List.rev (Core_kernel.String.split startpos.pos_fname ~on:'/'))
        in
        if
          Core_kernel.String.is_suffix id.name ~suffix:"_model"
          && Core_kernel.String.drop_suffix id.name 6 ^ ".stan" = modelname
        then
          Errors.semantic_error ~loc:id.id_loc
            ("Identifier " ^ id.name ^ " clashes with model name.")
  in
  let _ =
    if
      Core_kernel.String.is_suffix id.name ~suffix:"__"
      || List.exists
           (fun str -> str = id.name)
           [ "true"
           ; "false"
           ; "repeat"
           ; "until"
           ; "then"
           ; "var"
           ; "fvar"
           ; "STAN_MAJOR"
           ; "STAN_MINOR"
           ; "STAN_PATCH"
           ; "STAN_MATH_MAJOR"
           ; "STAN_MATH_MINOR"
           ; "STAN_MATH_PATCH"
           ; "alignas"
           ; "alignof"
           ; "and"
           ; "and_eq"
           ; "asm"
           ; "auto"
           ; "bitand"
           ; "bitor"
           ; "bool"
           ; "break"
           ; "case"
           ; "catch"
           ; "char"
           ; "char16_t"
           ; "char32_t"
           ; "class"
           ; "compl"
           ; "const"
           ; "constexpr"
           ; "const_cast"
           ; "continue"
           ; "decltype"
           ; "default"
           ; "delete"
           ; "do"
           ; "double"
           ; "dynamic_cast"
           ; "else"
           ; "enum"
           ; "explicit"
           ; "export"
           ; "extern"
           ; "false"
           ; "float"
           ; "for"
           ; "friend"
           ; "goto"
           ; "if"
           ; "inline"
           ; "int"
           ; "long"
           ; "mutable"
           ; "namespace"
           ; "new"
           ; "noexcept"
           ; "not"
           ; "not_eq"
           ; "nullptr"
           ; "operator"
           ; "or"
           ; "or_eq"
           ; "private"
           ; "protected"
           ; "public"
           ; "register"
           ; "reinterpret_cast"
           ; "return"
           ; "short"
           ; "signed"
           ; "sizeof"
           ; "static"
           ; "static_assert"
           ; "static_cast"
           ; "struct"
           ; "switch"
           ; "template"
           ; "this"
           ; "thread_local"
           ; "throw"
           ; "true"
           ; "try"
           ; "typedef"
           ; "typeid"
           ; "typename"
           ; "union"
           ; "unsigned"
           ; "using"
           ; "virtual"
           ; "void"
           ; "volatile"
           ; "wchar_t"
           ; "while"
           ; "xor"
           ; "xor_eq" ]
    then
      semantic_error ~loc:id.id_loc
        ("Identifier " ^ id.name ^ " clashes with reserved keyword.")
  in
  id

(* Probably nothing to do here *)
and semantic_check_originblock ob = ob

(* Probably nothing to do here *)
and semantic_check_returntype = function
  | Void -> Void
  | ReturnType ut -> ReturnType (semantic_check_unsizedtype ut)

(* Probably nothing to do here *)
and semantic_check_unsizedtype = function
  | Array ut -> Array (semantic_check_unsizedtype ut)
  | Fun (l, rt) ->
      Fun
        ( List.map
            (function
              | ob, ut ->
                  (semantic_check_originblock ob, semantic_check_unsizedtype ut))
            l
        , semantic_check_returntype rt )
  | ut -> ut

and semantic_check_sizedtype = function
  | SInt -> SInt
  | SReal -> SReal
  | SVector e ->
      let ue = semantic_check_expression e in
      let loc = (snd (typed_expression_unroll ue)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_type ue) then
          semantic_error ~loc "Vector sizes should be of type int."
      in
      SVector ue
  | SRowVector e ->
      let ue = semantic_check_expression e in
      let loc = (snd (typed_expression_unroll ue)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_type ue) then
          semantic_error ~loc "Row vector sizes should be of type int."
      in
      SRowVector ue
  | SMatrix (e1, e2) ->
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let loc1 = (snd (typed_expression_unroll ue1)).expr_typed_meta_loc in
      let loc2 = (snd (typed_expression_unroll ue2)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_type ue1) then
          semantic_error ~loc:loc1 "Matrix sizes should be of type int."
      in
      let _ =
        if not (check_of_int_type ue2) then
          semantic_error ~loc:loc2 "Matrix sizes should be of type int."
      in
      SMatrix (ue1, ue2)
  | SArray (st, e) ->
      let ust = semantic_check_sizedtype st in
      let ue = semantic_check_expression e in
      let loc = (snd (typed_expression_unroll ue)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_type ue) then
          semantic_error ~loc "Array sizes should be of type int."
      in
      SArray (ust, ue)

and semantic_check_transformation = function
  | Identity -> Identity
  | Lower e ->
      let ue = semantic_check_expression e in
      let loc = (snd (typed_expression_unroll ue)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error ~loc "Lower bound should be of int or real type."
      in
      Lower ue
  | Upper e ->
      let ue = semantic_check_expression e in
      let loc = (snd (typed_expression_unroll ue)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error ~loc "Upper bound should be of int or real type."
      in
      Upper ue
  | LowerUpper (e1, e2) ->
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let loc1 = (snd (typed_expression_unroll ue1)).expr_typed_meta_loc in
      let loc2 = (snd (typed_expression_unroll ue2)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_or_real_type ue1) then
          semantic_error ~loc:loc1
            "Lower and upper bound should be of int or real type."
      in
      let _ =
        if not (check_of_int_or_real_type ue2) then
          semantic_error ~loc:loc2
            "Lower and upper bound should be of int or real type."
      in
      LowerUpper (ue1, ue2)
  | LocationScale (e1, e2) ->
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let loc1 = (snd (typed_expression_unroll ue1)).expr_typed_meta_loc in
      let loc2 = (snd (typed_expression_unroll ue2)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_or_real_type ue1) then
          semantic_error ~loc:loc1
            "Location and scale should be of int or real type."
      in
      let _ =
        if not (check_of_int_or_real_type ue2) then
          semantic_error ~loc:loc2
            "Location and scale should be of int or real type."
      in
      LocationScale (ue1, ue2)
  | Ordered -> Ordered
  | PositiveOrdered -> PositiveOrdered
  | Simplex -> Simplex
  | UnitVector -> UnitVector
  | CholeskyCorr -> CholeskyCorr
  | CholeskyCov -> CholeskyCov
  | Correlation -> Correlation
  | Covariance -> Covariance

and semantic_check_expression x =
  let loc = (snd (untyped_expression_unroll x)).expr_untyped_meta_loc in
  match fst (untyped_expression_unroll x) with
  | Conditional (e1, e2, e3) -> (
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let ue3 = semantic_check_expression e3 in
      let returnblock =
        lub_originblock
          (List.map fst
             (List.map
                (fun z ->
                  (snd (typed_expression_unroll z)).expr_typed_meta_origin_type
                  )
                [ue1; ue2; ue3]))
      in
      match
        try_get_operator_return_type "Conditional"
          (List.map
             (fun z ->
               (snd (typed_expression_unroll z)).expr_typed_meta_origin_type )
             [ue1; ue2; ue3])
      with
      | Some (ReturnType ut) ->
          TypedExpr
            ( Conditional (ue1, ue2, ue3)
            , { expr_typed_meta_origin_type= (returnblock, ut)
              ; expr_typed_meta_loc= loc } )
      | _ ->
          semantic_error ~loc
            "Ill-typed arguments supplied to Conditional operator." )
  | InfixOp (e1, op, e2) -> (
      let ue1 = semantic_check_expression e1 in
      let uop = semantic_check_infixop op in
      let ue2 = semantic_check_expression e2 in
      let returnblock =
        lub_originblock
          (List.map fst
             (List.map
                (fun z ->
                  (snd (typed_expression_unroll z)).expr_typed_meta_origin_type
                  )
                [ue1; ue2]))
      in
      let opname = Core_kernel.Sexp.to_string (sexp_of_infixop uop) in
      match
        try_get_operator_return_type opname
          (List.map
             (fun z ->
               (snd (typed_expression_unroll z)).expr_typed_meta_origin_type )
             [ue1; ue2])
      with
      | Some (ReturnType ut) ->
          TypedExpr
            ( InfixOp (ue1, uop, ue2)
            , { expr_typed_meta_origin_type= (returnblock, ut)
              ; expr_typed_meta_loc= loc } )
      | _ ->
          semantic_error ~loc
            ("Ill-typed arguments supplied to " ^ opname ^ " operator.") )
  | PrefixOp (op, e) -> (
      let uop = semantic_check_prefixop op in
      let ue = semantic_check_expression e in
      let returnblock =
        lub_originblock
          (List.map fst
             [(snd (typed_expression_unroll ue)).expr_typed_meta_origin_type])
      in
      let opname = Core_kernel.Sexp.to_string (sexp_of_prefixop uop) in
      match
        try_get_operator_return_type opname
          [(snd (typed_expression_unroll ue)).expr_typed_meta_origin_type]
      with
      | Some (ReturnType ut) ->
          TypedExpr
            ( PrefixOp (uop, ue)
            , { expr_typed_meta_origin_type= (returnblock, ut)
              ; expr_typed_meta_loc= loc } )
      | _ ->
          semantic_error ~loc
            ("Ill-typed arguments supplied to " ^ opname ^ " operator.") )
  | PostfixOp (e, op) -> (
      let ue = semantic_check_expression e in
      let returnblock =
        lub_originblock
          (List.map fst
             [(snd (typed_expression_unroll ue)).expr_typed_meta_origin_type])
      in
      let uop = semantic_check_postfixop op in
      let opname = Core_kernel.Sexp.to_string (sexp_of_postfixop uop) in
      match
        try_get_operator_return_type opname
          [(snd (typed_expression_unroll ue)).expr_typed_meta_origin_type]
      with
      | Some (ReturnType ut) ->
          TypedExpr
            ( PostfixOp (ue, uop)
            , { expr_typed_meta_origin_type= (returnblock, ut)
              ; expr_typed_meta_loc= loc } )
      | _ ->
          semantic_error ~loc
            ("Ill-typed arguments supplied to " ^ opname ^ " operator.") )
  | Variable id ->
      let uid = semantic_check_identifier id in
      let ort = Symbol_table.look vm id.name in
      let _ =
        if ort = None && not (is_primitive_name uid.name) then
          semantic_error ~loc "Identifier not in scope."
      in
      TypedExpr
        ( Variable uid
        , { expr_typed_meta_origin_type=
              (function
                | None -> (Primitives, PrimitiveFunction) | Some x -> x)
                ort
          ; expr_typed_meta_loc= loc } )
  | IntNumeral s ->
      TypedExpr
        ( IntNumeral s
        , {expr_typed_meta_origin_type= (Data, Int); expr_typed_meta_loc= loc}
        )
  | RealNumeral s ->
      TypedExpr
        ( RealNumeral s
        , {expr_typed_meta_origin_type= (Data, Real); expr_typed_meta_loc= loc}
        )
  | FunApp (id, es) -> (
      let uid = semantic_check_identifier id in
      let ues = List.map semantic_check_expression es in
      let argumenttypes =
        List.map
          (fun z ->
            (snd (typed_expression_unroll z)).expr_typed_meta_origin_type )
          ues
      in
      let _ =
        if uid.name = "map_rect" then
          match ues with
          | TypedExpr (Indexed (TypedExpr (Variable arg1_name, _), []), _) :: _
            ->
              if
                Core_kernel.String.is_suffix arg1_name.name ~suffix:"_lp"
                || Core_kernel.String.is_suffix arg1_name.name ~suffix:"_rng"
              then
                semantic_error ~loc
                  ( "Mapped function cannot be an _rng or _lp function, found \
                     function name: " ^ arg1_name.name )
          | _ -> ()
      in
      let _ =
        if
          Core_kernel.String.is_suffix uid.name ~suffix:"_lpdf"
          || Core_kernel.String.is_suffix uid.name ~suffix:"_lpdf"
          || Core_kernel.String.is_suffix uid.name ~suffix:"_lpmf"
          || Core_kernel.String.is_suffix uid.name ~suffix:"_lcdf"
          || Core_kernel.String.is_suffix uid.name ~suffix:"_lccdf"
        then
          semantic_error ~loc
            "Probabilty functions with suffixes _lpdf, _lpmf, _lcdf, and \
             _lccdf, require a vertical bar (|) between the first two \
             arguments."
      in
      let _ =
        if
          Core_kernel.String.is_suffix uid.name ~suffix:"_lp"
          && not
               ( context_flags.in_lp_fun_def
               || context_flags.current_block = Model )
        then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      let _ =
        if
          Core_kernel.String.is_suffix uid.name ~suffix:"_rng"
          && ( (context_flags.in_fun_def && not context_flags.in_rng_fun_def)
             || context_flags.current_block = TParam
             || context_flags.current_block = Model )
        then
          semantic_error ~loc
            "Random number generators only allowed in transformed data block, \
             generated quantities block or user-defined functions with names \
             ending in _rng."
      in
      let returnblock = lub_originblock (List.map fst argumenttypes) in
      match try_get_primitive_return_type uid.name argumenttypes with
      | Some Void ->
          semantic_error ~loc
            "A returning function was expected but a non-returning function \
             was supplied."
      | Some (ReturnType ut) ->
          TypedExpr
            ( FunApp (uid, ues)
            , { expr_typed_meta_origin_type= (returnblock, ut)
              ; expr_typed_meta_loc= loc } )
      | _ -> (
        match Symbol_table.look vm uid.name with
        | Some (_, Fun (_, Void)) ->
            semantic_error ~loc
              "A returning function was expected but a non-returning function \
               was supplied."
        | Some (_, Fun (listedtypes, ReturnType ut)) ->
            let _ =
              if
                not
                  (check_compatible_arguments_mod_conv uid.name listedtypes
                     argumenttypes)
              then
                semantic_error ~loc
                  ("Ill-typed arguments supplied to function " ^ uid.name)
            in
            TypedExpr
              ( FunApp (uid, ues)
              , { expr_typed_meta_origin_type= (returnblock, ut)
                ; expr_typed_meta_loc= loc } )
        | Some _ ->
            semantic_error ~loc
              "A returning function was expected but a ground type value was \
               supplied."
        | None ->
            semantic_error ~loc
              ( "A returning function was expected but an undeclared identifier "
              ^ uid.name ^ " was supplied." ) )
      (* TODO: Insert informative error message in case identifier is found but not with appropriate type. *)
      )
  | CondFunApp (id, es) -> (
      let uid = semantic_check_identifier id in
      let _ =
        if
          not
            ( Core_kernel.String.is_suffix uid.name ~suffix:"_lpdf"
            || Core_kernel.String.is_suffix uid.name ~suffix:"_lcdf"
            || Core_kernel.String.is_suffix uid.name ~suffix:"_lpmf"
            || Core_kernel.String.is_suffix uid.name ~suffix:"_lccdf" )
        then
          semantic_error ~loc
            "Only functions with names ending in _lpdf, _lpmf, _lcdf, _lccdf \
             can make use of conditional notation."
      in
      let ues = List.map semantic_check_expression es in
      let argumenttypes =
        List.map
          (fun z ->
            (snd (typed_expression_unroll z)).expr_typed_meta_origin_type )
          ues
      in
      let _ =
        if
          Core_kernel.String.is_suffix uid.name ~suffix:"_lp"
          && not
               ( context_flags.in_lp_fun_def
               || context_flags.current_block = Model )
        then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      let returnblock = lub_originblock (List.map fst argumenttypes) in
      match try_get_primitive_return_type uid.name argumenttypes with
      | Some Void ->
          semantic_error ~loc
            "A returning function was expected but a non-returning function \
             was supplied."
      | Some (ReturnType ut) ->
          TypedExpr
            ( CondFunApp (uid, ues)
            , { expr_typed_meta_origin_type= (returnblock, ut)
              ; expr_typed_meta_loc= loc } )
      | _ -> (
        match Symbol_table.look vm uid.name with
        | Some (_, Fun (_, Void)) ->
            semantic_error ~loc
              "A returning function was expected but a non-returning function \
               was supplied."
        | Some (_, Fun (listedtypes, ReturnType ut)) ->
            let _ =
              if
                not
                  (check_compatible_arguments_mod_conv uid.name listedtypes
                     argumenttypes)
              then
                semantic_error ~loc
                  ("Ill-typed arguments supplied to function " ^ uid.name)
            in
            TypedExpr
              ( CondFunApp (uid, ues)
              , { expr_typed_meta_origin_type= (returnblock, ut)
                ; expr_typed_meta_loc= loc } )
        | Some _ ->
            semantic_error ~loc
              "A returning function was expected but a ground type value was \
               supplied."
        | None ->
            semantic_error ~loc
              ( "A returning function was expected but an undeclared identifier "
              ^ uid.name ^ " was supplied." ) )
      (* TODO: Insert informative error message in case identifier is found but not with appropriate type. *)
      )
  | GetLP ->
      let _ =
        if
          not
            ( context_flags.in_lp_fun_def
            || context_flags.current_block = Model
            || context_flags.current_block = TParam )
        then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      TypedExpr
        ( GetLP
        , { expr_typed_meta_origin_type= (context_flags.current_block, Real)
          ; expr_typed_meta_loc= loc } )
  | GetTarget ->
      let _ =
        if
          not
            ( context_flags.in_lp_fun_def
            || context_flags.current_block = Model
            || context_flags.current_block = TParam )
        then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      TypedExpr
        ( GetTarget
        , { expr_typed_meta_origin_type= (context_flags.current_block, Real)
          ; expr_typed_meta_loc= loc } )
  | ArrayExpr es ->
      let ues = List.map semantic_check_expression es in
      let elementtypes =
        List.map
          (fun y ->
            snd (snd (typed_expression_unroll y)).expr_typed_meta_origin_type
            )
          ues
      in
      let _ =
        if
          List.exists
            (fun x ->
              not
                ( check_of_same_type_mod_array_conv "" x (List.hd elementtypes)
                || check_of_same_type_mod_array_conv "" (List.hd elementtypes)
                     x ) )
            elementtypes
        then
          semantic_error ~loc
            "Array expression should have entries of consistent type."
      in
      let array_type =
        if List.exists (fun x -> List.hd elementtypes <> x) elementtypes then
          Array Real
        else Array (List.hd elementtypes)
      in
      let returnblock =
        lub_originblock
          (List.map fst
             (List.map
                (fun z ->
                  (snd (typed_expression_unroll z)).expr_typed_meta_origin_type
                  )
                ues))
      in
      TypedExpr
        ( ArrayExpr ues
        , { expr_typed_meta_origin_type= (returnblock, array_type)
          ; expr_typed_meta_loc= loc } )
  | RowVectorExpr es ->
      let ues = List.map semantic_check_expression es in
      let elementtypes =
        List.map
          (fun y ->
            snd (snd (typed_expression_unroll y)).expr_typed_meta_origin_type
            )
          ues
      in
      let ut =
        if List.for_all (fun x -> x = Real || x = Int) elementtypes then
          RowVector
        else if List.for_all (fun x -> x = RowVector) elementtypes then Matrix
        else
          semantic_error ~loc
            "Row_vector expression should have all int and real entries or \
             all row_vector entries."
      in
      let returnblock =
        lub_originblock
          (List.map fst
             (List.map
                (fun z ->
                  (snd (typed_expression_unroll z)).expr_typed_meta_origin_type
                  )
                ues))
      in
      TypedExpr
        ( RowVectorExpr ues
        , { expr_typed_meta_origin_type= (returnblock, ut)
          ; expr_typed_meta_loc= loc } )
  | Paren e ->
      let ue = semantic_check_expression e in
      TypedExpr (Paren ue, snd (typed_expression_unroll ue))
  | Indexed (e, indices) ->
      let ue = semantic_check_expression e in
      let uindices = List.map semantic_check_index indices in
      let inferred_originblock_of_indexed ob uindices =
        lub_originblock
          ( ob
          :: List.map
               (function
                 | All -> Primitives
                 | Single ue1 | Upfrom ue1 | Downfrom ue1 | Multiple ue1 ->
                     lub_originblock
                       [ ob
                       ; fst
                           (snd (typed_expression_unroll ue1))
                             .expr_typed_meta_origin_type ]
                 | Between (ue1, ue2) ->
                     lub_originblock
                       [ ob
                       ; fst
                           (snd (typed_expression_unroll ue1))
                             .expr_typed_meta_origin_type
                       ; fst
                           (snd (typed_expression_unroll ue2))
                             .expr_typed_meta_origin_type ])
               uindices )
      in
      let rec inferred_unsizedtype_of_indexed ut indexl =
        match (ut, indexl) with
        (* Here, we need some special logic to deal with row and column vectors
           properly. *)
        | Matrix, [All; Single _]
         |Matrix, [Upfrom _; Single _]
         |Matrix, [Downfrom _; Single _]
         |Matrix, [Between _; Single _]
         |Matrix, [Multiple _; Single _] ->
            Vector
        | ut, [] -> ut
        | ut, index :: indices -> (
            let reduce_type =
              match index with
              | Single _ -> true
              | All | Upfrom _ | Downfrom _ | Between _ | Multiple _ -> false
            in
            match ut with
            | Array ut' ->
                if reduce_type then inferred_unsizedtype_of_indexed ut' indices
                  (* TODO: this can easily be made tail recursive if needs be *)
                else Array (inferred_unsizedtype_of_indexed ut' indices)
            | Vector ->
                if reduce_type then
                  inferred_unsizedtype_of_indexed Real indices
                else inferred_unsizedtype_of_indexed Vector indices
            | RowVector ->
                if reduce_type then
                  inferred_unsizedtype_of_indexed Real indices
                else inferred_unsizedtype_of_indexed RowVector indices
            | Matrix ->
                if reduce_type then
                  inferred_unsizedtype_of_indexed RowVector indices
                else inferred_unsizedtype_of_indexed Matrix indices
            | _ ->
                semantic_error ~loc
                  "Only expressions of array, matrix, row_vector and vector \
                   type may be indexed." )
      in
      TypedExpr
        ( Indexed (ue, uindices)
        , { expr_typed_meta_origin_type=
              ( match
                    (snd (typed_expression_unroll ue))
                      .expr_typed_meta_origin_type
                with ob, ut ->
                  ( inferred_originblock_of_indexed ob uindices
                  , inferred_unsizedtype_of_indexed ut uindices ) )
          ; expr_typed_meta_loc= loc } )

(* Probably nothing to do here *)
and semantic_check_infixop i = i

(* Probably nothing to do here *)
and semantic_check_prefixop p = p

(* Probably nothing to do here *)
and semantic_check_postfixop p = p

and semantic_check_printable = function
  | PString s -> PString s
  | PExpr e -> (
      let ue = semantic_check_expression e in
      let loc = (snd (typed_expression_unroll ue)).expr_typed_meta_loc in
      match (snd (typed_expression_unroll ue)).expr_typed_meta_origin_type with
      | _, Fun _ | _, PrimitiveFunction ->
          semantic_error ~loc "Functions cannot be printed."
      | _ -> PExpr ue )

and semantic_check_statement s =
  let loc = (snd (untyped_statement_unroll s)).stmt_untyped_meta_loc in
  match fst (untyped_statement_unroll s) with
  | Assignment
      { assign_identifier= id
      ; assign_indices= lindex
      ; assign_op= assop
      ; assign_rhs= e } -> (
      let ue2 =
        semantic_check_expression
          (UntypedExpr
             ( Indexed
                 ( UntypedExpr (Variable id, {expr_untyped_meta_loc= id.id_loc})
                 , lindex )
             , {expr_untyped_meta_loc= loc} ))
      in
      let uid, ulindex =
        match ue2 with
        | TypedExpr (Indexed (TypedExpr (Variable uid, _), ulindex), _) ->
            (uid, ulindex)
        | _ ->
            semantic_error ~loc
              "This should never happen. Please file a bug. Error code 8."
      in
      let uassop = semantic_check_assignmentoperator assop in
      let ue = semantic_check_expression e in
      let uidoblock =
        (function
          | Some ob1, Some ob2 -> Some (lub_originblock [ob1; ob2])
          | Some ob1, _ -> Some ob1
          | _, Some ob2 -> Some ob2
          | _ -> None)
          ( (if is_primitive_name uid.name then Some Primitives else None)
          , Core_kernel.Option.map ~f:fst (Symbol_table.look vm uid.name) )
      in
      let _ =
        if Symbol_table.get_read_only vm uid.name then
          semantic_error ~loc
            "Cannot assign to function argument or loop identifier."
      in
      let _ =
        match uidoblock with
        | Some b ->
            if
              (not (Symbol_table.is_global vm uid.name))
              || b = context_flags.current_block
            then ()
            else
              semantic_error ~loc
                "Cannot assign to global variable declared in previous blocks."
        | _ ->
            semantic_error ~loc
              "This should never happen. Please file a bug. Error code 5."
      in
      (* TODO: the following is very ugly, but we seem to need something like it to
   reproduce the (strange) behaviour in the current Stan that local variables
   have a block level that is determined by what has been assigned to them
   rather than by where they were declared. I'm not sure that behaviour makes
   sense unless we use static analysis as well to make sure these assignments
   actually get evaluated in that phase. *)
      let _ =
        match (snd (typed_expression_unroll ue)).expr_typed_meta_origin_type
        with rhs_ob, _ -> update_originblock uid.name rhs_ob
      in
      let opname =
        Core_kernel.Sexp.to_string (sexp_of_assignmentoperator uassop)
      in
      match
        try_get_operator_return_type opname
          (List.map
             (fun z ->
               (snd (typed_expression_unroll z)).expr_typed_meta_origin_type )
             [ue2; ue])
      with
      | Some Void ->
          TypedStmt
            ( Assignment
                { assign_identifier= uid
                ; assign_indices= ulindex
                ; assign_op= uassop
                ; assign_rhs= ue }
            , {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc} )
      | _ ->
          let lhs_type =
            string_of_expressiontype
              (snd (typed_expression_unroll ue2)).expr_typed_meta_origin_type
          in
          let rhs_type =
            string_of_expressiontype
              (snd (typed_expression_unroll ue)).expr_typed_meta_origin_type
          in
          semantic_error ~loc
            ( "Ill-typed arguments supplied to assignment operator: lhs has\n\
              \          type " ^ lhs_type ^ " and rhs has type " ^ rhs_type
            ^ "." ) )
  | NRFunApp (id, es) -> (
      let uid = semantic_check_identifier id in
      let ues = List.map semantic_check_expression es in
      let argumenttypes =
        List.map
          (fun z ->
            (snd (typed_expression_unroll z)).expr_typed_meta_origin_type )
          ues
      in
      let _ =
        if
          Core_kernel.String.is_suffix uid.name ~suffix:"_lp"
          && not
               ( context_flags.in_lp_fun_def
               || context_flags.current_block = Model )
        then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      match try_get_primitive_return_type uid.name argumenttypes with
      | Some Void ->
          TypedStmt
            ( NRFunApp (uid, ues)
            , {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc} )
      | Some (ReturnType _) ->
          semantic_error ~loc
            "A non-returning function was expected but a returning function \
             was supplied."
      | _ -> (
        match Symbol_table.look vm uid.name with
        | Some (_, Fun (listedtypes, Void)) ->
            let _ =
              if
                not
                  (check_compatible_arguments_mod_conv uid.name listedtypes
                     argumenttypes)
              then
                semantic_error ~loc
                  ( "Ill-typed arguments supplied to non-returning function "
                  ^ uid.name )
            in
            TypedStmt
              ( NRFunApp (uid, ues)
              , {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc} )
        | Some (_, Fun (_, ReturnType _)) ->
            semantic_error ~loc
              "A non-returning function was expected but a returning function \
               was supplied."
        | Some _ ->
            semantic_error ~loc
              "A non-returning function was expected but a ground type value \
               was supplied."
        | None ->
            semantic_error ~loc
              ( "A returning function was expected but an undeclared identifier "
              ^ uid.name ^ " was supplied." ) )
      (* TODO: Insert informative error message in case identifier is found but not with appropriate type. *)
      )
  | TargetPE e ->
      let ue = semantic_check_expression e in
      let _ =
        match
          (snd (typed_expression_unroll ue)).expr_typed_meta_origin_type
        with
        | _, Fun _ | _, PrimitiveFunction ->
            semantic_error ~loc
              "A (container of) reals or ints needs to be supplied to \
               increment target."
        | _ -> ()
      in
      let _ =
        if
          not
            (context_flags.in_lp_fun_def || context_flags.current_block = Model)
        then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      TypedStmt
        (TargetPE ue, {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc})
  | IncrementLogProb e ->
      let ue = semantic_check_expression e in
      let _ =
        match
          (snd (typed_expression_unroll ue)).expr_typed_meta_origin_type
        with
        | _, Fun _ | _, PrimitiveFunction ->
            semantic_error ~loc
              "A (container of) reals or ints needs to be supplied to \
               increment target."
        | _ -> ()
      in
      let _ =
        if
          not
            (context_flags.in_lp_fun_def || context_flags.current_block = Model)
        then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      TypedStmt
        ( IncrementLogProb ue
        , {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc} )
  | Tilde {arg= e; distribution= id; args= es; truncation= t} ->
      let ue = semantic_check_expression e in
      let uid = semantic_check_identifier id in
      let ues = List.map semantic_check_expression es in
      let ut = semantic_check_truncation t in
      let argumenttypes =
        List.map
          (fun z ->
            (snd (typed_expression_unroll z)).expr_typed_meta_origin_type )
          (ue :: ues)
      in
      let _ =
        if
          not
            (context_flags.in_lp_fun_def || context_flags.current_block = Model)
        then
          semantic_error ~loc
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      let _ =
        if
          Core_kernel.String.is_suffix uid.name ~suffix:"_cdf"
          || Core_kernel.String.is_suffix uid.name ~suffix:"_ccdf"
        then
          semantic_error ~loc
            ( "CDF and CCDF functions may not be used with sampling notation. \
               Use increment_log_prob(" ^ uid.name ^ "_log(...)) instead." )
      in
      let _ =
        if
          try_get_primitive_return_type (uid.name ^ "_lpdf") argumenttypes
          = Some (ReturnType Real)
          || try_get_primitive_return_type (uid.name ^ "_lpmf") argumenttypes
             = Some (ReturnType Real)
          || try_get_primitive_return_type (uid.name ^ "_log") argumenttypes
             = Some (ReturnType Real)
             && uid.name <> "binomial_coefficient"
             && uid.name <> "multiply"
          || ( match Symbol_table.look vm (uid.name ^ "_lpdf") with
             | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
                 check_compatible_arguments_mod_conv uid.name listedtypes
                   argumenttypes
             | _ -> false )
          || ( match Symbol_table.look vm (uid.name ^ "_lpmf") with
             | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
                 check_compatible_arguments_mod_conv uid.name listedtypes
                   argumenttypes
             | _ -> false )
          ||
          match Symbol_table.look vm (uid.name ^ "_log") with
          | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
              check_compatible_arguments_mod_conv uid.name listedtypes
                argumenttypes
          | _ -> false
        then ()
        else semantic_error ~loc "Ill-typed arguments to '~' statement."
      in
      let _ =
        if
          ut = NoTruncate
          || ( try_get_primitive_return_type (uid.name ^ "_lcdf") argumenttypes
               = Some (ReturnType Real)
             ||
             match Symbol_table.look vm (uid.name ^ "_lcdf") with
             | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
                 check_compatible_arguments_mod_conv uid.name listedtypes
                   argumenttypes
             | _ -> (
                 false
                 || try_get_primitive_return_type (uid.name ^ "_cdf_log")
                      argumenttypes
                    = Some (ReturnType Real)
                 ||
                 match Symbol_table.look vm (uid.name ^ "_cdf_log") with
                 | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
                     check_compatible_arguments_mod_conv uid.name listedtypes
                       argumenttypes
                 | _ -> false ) )
             && ( try_get_primitive_return_type (uid.name ^ "_lccdf")
                    argumenttypes
                  = Some (ReturnType Real)
                ||
                match Symbol_table.look vm (uid.name ^ "_lccdf") with
                | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
                    check_compatible_arguments_mod_conv uid.name listedtypes
                      argumenttypes
                | _ -> (
                    false
                    || try_get_primitive_return_type (uid.name ^ "_ccdf_log")
                         argumenttypes
                       = Some (ReturnType Real)
                    ||
                    match Symbol_table.look vm (uid.name ^ "_ccdf_log") with
                    | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
                        check_compatible_arguments_mod_conv uid.name
                          listedtypes argumenttypes
                    | _ -> false ) )
        then ()
        else
          semantic_error ~loc
            "Truncation is only defined if distribution has _lcdf and _lccdf \
             functions implemented."
      in
      TypedStmt
        ( Tilde {arg= ue; distribution= uid; args= ues; truncation= ut}
        , {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc} )
  | Break ->
      let _ =
        if not context_flags.in_loop then
          semantic_error ~loc "Break statements may only be used in loops."
      in
      TypedStmt (Break, {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc})
  | Continue ->
      let _ =
        if not context_flags.in_loop then
          semantic_error ~loc "Continue statements may only be used in loops."
      in
      TypedStmt
        (Continue, {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc})
  | Return e ->
      let _ =
        if not context_flags.in_returning_fun_def then
          semantic_error ~loc
            "Expression return statements may only be used inside returning \
             function definitions."
      in
      let ue = semantic_check_expression e in
      TypedStmt
        ( Return ue
        , { stmt_typed_meta_type=
              ReturnType
                (snd
                   (snd (typed_expression_unroll ue))
                     .expr_typed_meta_origin_type)
          ; stmt_typed_meta_loc= loc } )
  | ReturnVoid ->
      let _ =
        if (not context_flags.in_fun_def) || context_flags.in_returning_fun_def
        then
          semantic_error ~loc
            "Void return statements may only be used inside non-returning \
             function definitions."
      in
      TypedStmt
        (ReturnVoid, {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc})
  | Print ps ->
      let ups = List.map semantic_check_printable ps in
      TypedStmt
        (Print ups, {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc})
  | Reject ps ->
      let ups = List.map semantic_check_printable ps in
      TypedStmt
        (Reject ups, {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc})
  | Skip ->
      TypedStmt (Skip, {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc})
  | IfThen (e, s) ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error ~loc
            "Condition in conditional needs to be of type int or real."
      in
      let us = semantic_check_statement s in
      TypedStmt (IfThen (ue, us), snd (typed_statement_unroll us))
  | IfThenElse (e, s1, s2) ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error ~loc
            "Condition in conditional needs to be of type int or real."
      in
      let us1 = semantic_check_statement s1 in
      let us2 = semantic_check_statement s2 in
      let t1 = (snd (typed_statement_unroll us1)).stmt_typed_meta_type in
      let t2 = (snd (typed_statement_unroll us2)).stmt_typed_meta_type in
      let _ =
        if t1 <> t2 then
          semantic_error ~loc
            ( "Branches of conditional need to have the same return type. \
               Instead, found return types " ^ string_of_returntype t1
            ^ " and " ^ string_of_returntype t2 ^ "." )
      in
      TypedStmt (IfThenElse (ue, us1, us2), snd (typed_statement_unroll us1))
  | While (e, s) ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error ~loc
            "Condition in while loop needs to be of type int or real."
      in
      let _ = context_flags.in_loop <- true in
      let us = semantic_check_statement s in
      let _ = context_flags.in_loop <- false in
      TypedStmt (While (ue, us), snd (typed_statement_unroll us))
  | For {loop_variable= id; lower_bound= e1; upper_bound= e2; loop_body= s} ->
      let uid = semantic_check_identifier id in
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let _ =
        if not (check_of_int_type ue1) then
          semantic_error ~loc
            "Lower bound of for-loop needs to be of type int."
      in
      let _ =
        if not (check_of_int_type ue2) then
          semantic_error ~loc
            "Upper bound of for-loop needs to be of type int."
      in
      let _ = Symbol_table.begin_scope vm in
      let _ = check_fresh_variable uid false in
      let oindexblock =
        lub_originblock
          (List.map fst
             [ (snd (typed_expression_unroll ue1)).expr_typed_meta_origin_type
             ; (snd (typed_expression_unroll ue2)).expr_typed_meta_origin_type
             ])
      in
      let _ = Symbol_table.enter vm uid.name (oindexblock, Int) in
      let _ = Symbol_table.set_read_only vm uid.name in
      let _ = context_flags.in_loop <- true in
      let us = semantic_check_statement s in
      let _ = context_flags.in_loop <- false in
      let _ = Symbol_table.end_scope vm in
      TypedStmt
        ( For
            { loop_variable= uid
            ; lower_bound= ue1
            ; upper_bound= ue2
            ; loop_body= us }
        , snd (typed_statement_unroll us) )
  | ForEach (id, e, s) ->
      let uid = semantic_check_identifier id in
      let ue = semantic_check_expression e in
      let loop_identifier_unsizedtype =
        match
          (snd (typed_expression_unroll ue)).expr_typed_meta_origin_type
        with
        | _, Array ut -> ut
        | _, Vector | _, RowVector | _, Matrix -> Real
        | _ ->
            semantic_error ~loc
              "Foreach loop must be over array, vector, row_vector or matrix"
      in
      let _ = Symbol_table.begin_scope vm in
      let _ = check_fresh_variable uid false in
      let oindexblock =
        fst (snd (typed_expression_unroll ue)).expr_typed_meta_origin_type
      in
      let _ =
        Symbol_table.enter vm uid.name
          (oindexblock, loop_identifier_unsizedtype)
      in
      let _ = Symbol_table.set_read_only vm uid.name in
      let _ = context_flags.in_loop <- true in
      let us = semantic_check_statement s in
      let _ = context_flags.in_loop <- false in
      let _ = Symbol_table.end_scope vm in
      TypedStmt (ForEach (uid, ue, us), snd (typed_statement_unroll us))
  | Block vdsl ->
      let _ = Symbol_table.begin_scope vm in
      let uvdsl = List.map semantic_check_statement vdsl in
      let _ = Symbol_table.end_scope vm in
      (* Any statements after a break or continue do not count for the return
      type. *)
      let rec list_until_breakcontinue = function
        | [] -> []
        | [x] -> [x]
        | x1 :: (Break, _) :: _ -> [x1]
        | x1 :: (Continue, _) :: _ -> [x1]
        | x1 :: (ReturnVoid, _) :: _ -> [x1]
        | x1 :: x2 :: xs -> x1 :: list_until_breakcontinue (x2 :: xs)
      in
      (* We make sure that for an if-then statement, everything after the then
      block has the same return type as the then block. This could probably
      be done in a prettier way. *)
      let compute_rt_and_check_if_then_branches_agree vdsl2 =
        let rec helper = function
          | [] -> Void
          | x :: xs -> (
            match x with
            | false, ReturnType x -> ReturnType x
            | true, y ->
                if helper xs = y then y
                else
                  semantic_error ~loc
                    "Branches of conditional need to have the same return type."
            | false, Void -> helper xs )
        in
        helper
          (List.map
             (function
               | IfThen _, {stmt_typed_meta_type= rt; _} -> (true, rt)
               | _, {stmt_typed_meta_type= rt; _} -> (false, rt))
             vdsl2)
      in
      TypedStmt
        ( Block uvdsl
        , { stmt_typed_meta_type=
              compute_rt_and_check_if_then_branches_agree
                (list_until_breakcontinue
                   (List.map typed_statement_unroll uvdsl))
          ; stmt_typed_meta_loc= loc } )
  | VDecl (st, id) ->
      let ust = semantic_check_sizedtype st in
      let uid = semantic_check_identifier id in
      let ut = unsizedtype_of_sizedtype st in
      let _ = check_fresh_variable uid false in
      (* Note: this origin block here is a bit of a curiosity to get Stan
         to treat the level of local variables in the right way. It will get
         modified (can be elevated) based on assignments.*)
      let _ = Symbol_table.enter vm id.name (Functions, ut) in
      TypedStmt
        ( VDecl (ust, uid)
        , {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc} )
  | VDeclAss {sizedtype= st; identifier= id; value= e} -> (
    match
      ( semantic_check_statement
          (UntypedStmt (VDecl (st, id), {stmt_untyped_meta_loc= loc}))
      , semantic_check_statement
          (UntypedStmt
             ( Assignment
                 { assign_identifier= id
                 ; assign_indices= []
                 ; assign_op= Assign
                 ; assign_rhs= e }
             , {stmt_untyped_meta_loc= loc} )) )
    with
    | ( TypedStmt (VDecl (ust, uid), _)
      , TypedStmt
          ( Assignment
              { assign_identifier= _
              ; assign_indices= []
              ; assign_op= Assign
              ; assign_rhs= ue }
          , {stmt_typed_meta_type= Void; _} ) ) ->
        TypedStmt
          ( VDeclAss {sizedtype= ust; identifier= uid; value= ue}
          , {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc} )
    | _ ->
        semantic_error ~loc
          "This should never happen. Please file a bug. Error code 2." )
  | TVDecl (st, trans, id) ->
      let ust = semantic_check_sizedtype st in
      let rec check_sizes_below_param_level = function
        | SVector ue -> (
          match
            (snd (typed_expression_unroll ue)).expr_typed_meta_origin_type
          with
          | Param, _ | TParam, _ | GQuant, _ -> false
          | _ -> true )
        | SRowVector ue -> (
          match
            (snd (typed_expression_unroll ue)).expr_typed_meta_origin_type
          with
          | Param, _ | TParam, _ | GQuant, _ -> false
          | _ -> true )
        | SMatrix (ue1, ue2) -> (
          match
            (snd (typed_expression_unroll ue1)).expr_typed_meta_origin_type
          with
          | Param, _ | TParam, _ | GQuant, _ -> false
          | _ -> (
            match
              (snd (typed_expression_unroll ue2)).expr_typed_meta_origin_type
            with
            | Param, _ | TParam, _ | GQuant, _ -> false
            | _ -> true ) )
        | SArray (ust2, ue) -> (
          match
            (snd (typed_expression_unroll ue)).expr_typed_meta_origin_type
          with
          | Param, _ | TParam, _ | GQuant, _ -> false
          | _ -> check_sizes_below_param_level ust2 )
        | _ -> true
      in
      let _ =
        if not (check_sizes_below_param_level ust) then
          semantic_error ~loc
            "Non-data variables are not allowed in top level size declarations."
      in
      let utrans = semantic_check_transformation trans in
      let uid = semantic_check_identifier id in
      let ut = unsizedtype_of_sizedtype st in
      let _ = check_fresh_variable uid false in
      let _ =
        Symbol_table.enter vm uid.name (context_flags.current_block, ut)
      in
      let _ =
        if
          ust = SInt
          &&
          match utrans with
          | Lower ue1 -> (
            match
              (snd (typed_expression_unroll ue1)).expr_typed_meta_origin_type
            with
            | _, Real -> true
            | _ -> false )
          | Upper ue1 -> (
            match
              (snd (typed_expression_unroll ue1)).expr_typed_meta_origin_type
            with
            | _, Real -> true
            | _ -> false )
          | LowerUpper (ue1, ue2) -> (
            match
              (snd (typed_expression_unroll ue1)).expr_typed_meta_origin_type
            with
            | _, Real -> true
            | _ -> (
              match
                (snd (typed_expression_unroll ue2)).expr_typed_meta_origin_type
              with
              | _, Real -> true
              | _ -> false ) )
          | _ -> false
        then
          semantic_error ~loc
            "Bounds of integer variable should be of type int. Found type real."
      in
      let _ =
        if
          ( context_flags.current_block = Param
          || context_flags.current_block = TParam )
          && unsizedtype_contains_int ut
        then semantic_error ~loc "(Transformed) Parameters cannot be integers."
      in
      TypedStmt
        ( TVDecl (ust, utrans, uid)
        , {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc} )
  | TVDeclAss
      {tsizedtype= st; transformation= trans; tidentifier= id; tvalue= e} -> (
    match
      ( semantic_check_statement
          (UntypedStmt (TVDecl (st, trans, id), {stmt_untyped_meta_loc= loc}))
      , semantic_check_statement
          (UntypedStmt
             ( Assignment
                 { assign_identifier= id
                 ; assign_indices= []
                 ; assign_op= Assign
                 ; assign_rhs= e }
             , {stmt_untyped_meta_loc= loc} )) )
    with
    | ( TypedStmt (TVDecl (ust, utrans, uid), _)
      , TypedStmt
          ( Assignment
              { assign_identifier= _
              ; assign_indices= []
              ; assign_op= Assign
              ; assign_rhs= ue }
          , {stmt_typed_meta_type= Void; _} ) ) ->
        TypedStmt
          ( TVDeclAss
              { tsizedtype= ust
              ; transformation= utrans
              ; tidentifier= uid
              ; tvalue= ue }
          , {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc} )
    | _ ->
        semantic_error ~loc
          "This should never happen. Please file a bug. Error code 1." )
  | FunDef {returntype= rt; funname= id; arguments= args; body= b} ->
      let urt = semantic_check_returntype rt in
      let uid = semantic_check_identifier id in
      let uargs =
        List.map
          (function
            | ob, ut, id ->
                ( semantic_check_originblock ob
                , semantic_check_unsizedtype ut
                , semantic_check_identifier id ))
          args
      in
      let uarg_types = List.map (function w, y, _ -> (w, y)) uargs in
      let _ =
        if Symbol_table.is_missing_fun_def vm uid.name then (
          if
            Symbol_table.look vm uid.name
            <> Some (Functions, Fun (uarg_types, urt))
          then
            semantic_error ~loc
              ( "Function " ^ uid.name
              ^ " has already been declared to have type "
              ^ string_of_opt_expressiontype (Symbol_table.look vm uid.name) ) )
        else check_fresh_variable uid (List.length uarg_types = 0)
      in
      let _ =
        match b with
        | UntypedStmt (Skip, _) ->
            if Symbol_table.is_missing_fun_def vm uid.name then
              semantic_error ~loc
                ( "Function " ^ uid.name
                ^ " has already been declared. A definition is expected." )
            else Symbol_table.add_is_missing_fun_def vm uid.name
        | _ -> Symbol_table.remove_is_missing_fun_def vm uid.name
      in
      let _ =
        Symbol_table.enter vm uid.name (Functions, Fun (uarg_types, urt))
      in
      let uarg_identifiers = List.map (function _, _, z -> z) uargs in
      let uarg_names = List.map (fun x -> x.name) uarg_identifiers in
      let _ = List.map (Symbol_table.set_read_only vm) uarg_names in
      let _ =
        if
          urt <> ReturnType Real
          && ( Core_kernel.String.is_suffix uid.name ~suffix:"_log"
             || Core_kernel.String.is_suffix uid.name ~suffix:"_lpdf"
             || Core_kernel.String.is_suffix uid.name ~suffix:"_lpmf"
             || Core_kernel.String.is_suffix uid.name ~suffix:"_lcdf"
             || Core_kernel.String.is_suffix uid.name ~suffix:"_lccdf" )
        then
          semantic_error ~loc
            "Real return type required for probability functions ending in \
             _log, _lpdf, _lpmf, _lcdf, or _lccdf."
      in
      let _ =
        if
          Core_kernel.String.is_suffix uid.name ~suffix:"_lpdf"
          && (List.length uarg_types = 0 || snd (List.hd uarg_types) <> Real)
        then
          semantic_error ~loc
            "Probability density functions require real variates (first \
             argument)."
      in
      let _ =
        if
          Core_kernel.String.is_suffix uid.name ~suffix:"_lpmf"
          && (List.length uarg_types = 0 || snd (List.hd uarg_types) <> Int)
        then
          semantic_error ~loc
            "Probability mass functions require integer variates (first \
             argument)."
      in
      let _ = context_flags.in_fun_def <- true in
      let _ =
        if Core_kernel.String.is_suffix uid.name ~suffix:"_rng" then
          context_flags.in_rng_fun_def <- true
      in
      let _ =
        if Core_kernel.String.is_suffix uid.name ~suffix:"_lp" then
          context_flags.in_lp_fun_def <- true
      in
      let _ = if urt <> Void then context_flags.in_returning_fun_def <- true in
      let _ = Symbol_table.begin_scope vm in
      let _ =
        if dup_exists uarg_names then
          semantic_error ~loc
            "All function arguments should be distinct identifiers."
      in
      let _ =
        List.map (fun x -> check_fresh_variable x false) uarg_identifiers
      in
      let _ = List.map2 (Symbol_table.enter vm) uarg_names uarg_types in
      let ub = semantic_check_statement b in
      let _ =
        if
          Symbol_table.is_missing_fun_def vm uid.name
          || check_of_compatible_return_type urt
               (snd (typed_statement_unroll ub)).stmt_typed_meta_type
        then ()
        else
          semantic_error ~loc
            "Function bodies must contain a return statement of correct type \
             in every branch."
      in
      let _ = Symbol_table.end_scope vm in
      let _ = context_flags.in_fun_def <- false in
      let _ = context_flags.in_returning_fun_def <- false in
      let _ = context_flags.in_lp_fun_def <- false in
      let _ = context_flags.in_rng_fun_def <- false in
      TypedStmt
        ( FunDef {returntype= urt; funname= uid; arguments= uargs; body= ub}
        , {stmt_typed_meta_type= Void; stmt_typed_meta_loc= loc} )

and semantic_check_truncation = function
  | NoTruncate -> NoTruncate
  | TruncateUpFrom e ->
      let ue = semantic_check_expression e in
      let loc = (snd (typed_expression_unroll ue)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error ~loc "Truncation bound should be of type int or real."
      in
      TruncateUpFrom ue
  | TruncateDownFrom e ->
      let ue = semantic_check_expression e in
      let loc = (snd (typed_expression_unroll ue)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error ~loc "Truncation bound should be of type int or real."
      in
      TruncateDownFrom ue
  | TruncateBetween (e1, e2) ->
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let loc1 = (snd (typed_expression_unroll ue1)).expr_typed_meta_loc in
      let loc2 = (snd (typed_expression_unroll ue2)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_or_real_type ue1) then
          semantic_error ~loc:loc1
            "Truncation bound should be of type int or real."
      in
      let _ =
        if not (check_of_int_or_real_type ue2) then
          semantic_error ~loc:loc2
            "Truncation bound should be of type int or real."
      in
      TruncateBetween (ue1, ue2)

and semantic_check_index = function
  | All -> All
  | Single e ->
      let ue = semantic_check_expression e in
      let loc = (snd (typed_expression_unroll ue)).expr_typed_meta_loc in
      if check_of_int_type ue then Single ue
      else if check_of_int_array_type ue then Multiple ue
      else
        semantic_error ~loc
          "Index should be of type int or int[] or should be a range."
  | Upfrom e ->
      let ue = semantic_check_expression e in
      let loc = (snd (typed_expression_unroll ue)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_type ue) then
          semantic_error ~loc "Range bound should be of type int."
      in
      Upfrom ue
  | Downfrom e ->
      let ue = semantic_check_expression e in
      let loc = (snd (typed_expression_unroll ue)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_type ue) then
          semantic_error ~loc "Range bound should be of type int."
      in
      Downfrom ue
  | Between (e1, e2) ->
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let loc1 = (snd (typed_expression_unroll ue1)).expr_typed_meta_loc in
      let loc2 = (snd (typed_expression_unroll ue2)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_type ue1) then
          semantic_error ~loc:loc1 "Range bound should be of type int."
      in
      let _ =
        if not (check_of_int_type ue2) then
          semantic_error ~loc:loc2 "Range bound should be of type int."
      in
      Between (ue1, ue2)
  | Multiple e ->
      let ue = semantic_check_expression e in
      let loc = (snd (typed_expression_unroll ue)).expr_typed_meta_loc in
      let _ =
        if not (check_of_int_array_type ue) then
          semantic_error ~loc "Multiple index should be of type int[]."
      in
      Multiple ue

(* Probably nothing to do here *)
and semantic_check_assignmentoperator op = op
