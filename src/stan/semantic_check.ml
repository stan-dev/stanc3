(* Semantic validation of AST*)

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
TODO: Allow math library functions to clash with variable names as long as signatures/types differ. I.e. users can overload library functions.
*)

(* TODO: encapsulate some of the state in this file so people don't hurt themselves *)

open Symbol_table
open Syntax
open Primitives

(* Idea: we have a semantic checking function for each AST node.
   Each such calls the corresponding checking functions for its children
   left-to-right. *)

(* Invariant: after an expression has been checked, it has a well-defined type *)

(* Invariant: after a statement has been checked, it has a well-defined return type *)

(* e.g. compare Data Model = -1  and compare GQuant Functions = 1 *)

(* NB DANGER: this file specifies an imperative tree algorithm which
   decorates the AST while operating on two bits of state:
   1) a global symbol table vm
   2) some context flags context_flags, to communicate information down
      the AST   *)

let vm = Symbol.initialize ()

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
  | SVector e -> Vector
  | SRowVector e -> RowVector
  | SMatrix (e1, e2) -> Matrix
  | SArray (st, e) -> Array (unsizedtype_of_sizedtype st)

let rec lub_originblock = function
  | [] -> Primitives
  | x :: xs ->
      let y = lub_originblock xs in
      if compare_originblock x y < 0 then y else x

let lub_op_originblock l =
  lub_originblock (List.map (function None -> Primitives | Some b -> b) l)

let check_of_int_type ue =
  match (snd (typed_expression_unroll ue)).expr_meta_origintype with
  | Some (_, Int) -> true
  | _ -> false

let check_of_int_array_type ue =
  match (snd (typed_expression_unroll ue)).expr_meta_origintype with
  | Some (_, Array Int) -> true
  | _ -> false

let check_of_int_or_real_type ue =
  match (snd (typed_expression_unroll ue)).expr_meta_origintype with
  | Some (_, Int) -> true
  | Some (_, Real) -> true
  | _ -> false

(* TODO: insert positions into semantic errors! *)

let check_fresh_variable id is_nullary_function =
  (* For some strange reason, Stan allows user declared identifiers that are
   not of nullary function types to clash with nullary library functions.
   No other name clashes are tolerated. Here's the logic to
   achieve that. *)
  let _ =
    if
      is_primitive_name id
      && (is_nullary_function || try_get_primitive_return_type id [] = None)
    then
      let error_msg =
        String.concat " " ["Identifier "; id; " clashes with primitive."]
      in
      semantic_error error_msg
  in
  match Symbol.look vm id with
  | Some x ->
      let error_msg =
        String.concat " " ["Identifier "; id; " is already in use."]
      in
      semantic_error error_msg
  | None -> ()

(* TODO: the following is very ugly, but we seem to need something like it to
   reproduce the (strange) behaviour in the current Stan that local variables
   have a block level that is determined by what has been assigned to them
   rather than by where they were declared. I'm not sure that behaviour makes
   sense unless we use static analysis as well to make sure these assignments
   actually get evaluated in that phase. *)
let update_originblock name ob =
  match Symbol.look vm name with
  | Some (old_ob, ut) ->
      let new_ob = lub_originblock [ob; old_ob] in
      Symbol.unsafe_remove vm name ;
      Symbol.unsafe_add vm name (new_ob, ut)
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
    let _ = context_flags.current_block <- Functions in
    let ubf = Core_kernel.Option.map bf (List.map semantic_check_statement) in
    let _ =
      if Symbol.some_fun_is_missing_def vm then
        semantic_error
          "Some function is declared without specifying a definition."
    in
    let _ = context_flags.current_block <- Data in
    let ubd = Core_kernel.Option.map bd (List.map semantic_check_statement) in
    let _ = context_flags.current_block <- TData in
    let ubtd =
      Core_kernel.Option.map btd (List.map semantic_check_statement)
    in
    let _ = context_flags.current_block <- Param in
    let ubp = Core_kernel.Option.map bp (List.map semantic_check_statement) in
    let _ = context_flags.current_block <- TParam in
    let ubtp =
      Core_kernel.Option.map btp (List.map semantic_check_statement)
    in
    let _ = context_flags.current_block <- Model in
    let _ = Symbol.begin_scope vm in
    let ubm = Core_kernel.Option.map bm (List.map semantic_check_statement) in
    let _ = Symbol.end_scope vm in
    let _ = context_flags.current_block <- GQuant in
    let ubgq =
      Core_kernel.Option.map bgq (List.map semantic_check_statement)
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
    if
      Filename.check_suffix id "__"
      || List.exists
           (fun str -> str = id)
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
    then semantic_error ("Identifier " ^ id ^ " clashes with reserved keyword.")
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
      let _ =
        if not (check_of_int_type ue) then
          semantic_error "Vector sizes should be of type int."
      in
      SVector ue
  | SRowVector e ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_type ue) then
          semantic_error "Row vector sizes should be of type int."
      in
      SRowVector ue
  | SMatrix (e1, e2) ->
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let _ =
        if not (check_of_int_type ue1 && check_of_int_type ue2) then
          semantic_error "Matrix sizes should be of type int."
      in
      SMatrix (ue1, ue2)
  | SArray (st, e) ->
      let ust = semantic_check_sizedtype st in
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_type ue) then
          semantic_error "Array sizes should be of type int."
      in
      SArray (ust, ue)

and semantic_check_transformation = function
  | Identity -> Identity
  | Lower e ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error "Lower bound should be of int or real type."
      in
      Lower ue
  | Upper e ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error "Upper bound should be of int or real type."
      in
      Upper ue
  | LowerUpper (e1, e2) ->
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let _ =
        if not (check_of_int_or_real_type ue1 && check_of_int_or_real_type ue2)
        then
          semantic_error "Lower and upper bound should be of int or real type."
      in
      LowerUpper (ue1, ue2)
  | LocationScale (e1, e2) ->
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let _ =
        if not (check_of_int_or_real_type ue1 && check_of_int_or_real_type ue2)
        then semantic_error "Location and scale should be of int or real type."
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
  match fst (untyped_expression_unroll x) with
  | Conditional (e1, e2, e3) -> (
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let ue3 = semantic_check_expression e3 in
      let returnblock =
        lub_op_originblock
          (List.map
             (fun y -> Core_kernel.Option.map y fst)
             (List.map
                (fun z ->
                  (snd (typed_expression_unroll z)).expr_meta_origintype )
                [ue1; ue2; ue3]))
      in
      match
        try_get_operator_return_type "Conditional"
          (List.map
             (fun z -> (snd (typed_expression_unroll z)).expr_meta_origintype)
             [ue1; ue2; ue3])
      with
      | Some (ReturnType ut) ->
          TypedExpr
            ( Conditional (ue1, ue2, ue3)
            , {expr_meta_origintype= Some (returnblock, ut)} )
      | _ ->
          semantic_error
            "Ill-typed arguments supplied to Conditional operator." )
  | InfixOp (e1, op, e2) -> (
      let ue1 = semantic_check_expression e1 in
      let uop = semantic_check_infixop op in
      let ue2 = semantic_check_expression e2 in
      let returnblock =
        lub_op_originblock
          (List.map
             (fun y -> Core_kernel.Option.map y fst)
             (List.map
                (fun z ->
                  (snd (typed_expression_unroll z)).expr_meta_origintype )
                [ue1; ue2]))
      in
      let opname = Core_kernel.Sexp.to_string (sexp_of_infixop uop) in
      match
        try_get_operator_return_type opname
          (List.map
             (fun z -> (snd (typed_expression_unroll z)).expr_meta_origintype)
             [ue1; ue2])
      with
      | Some (ReturnType ut) ->
          TypedExpr
            ( InfixOp (ue1, uop, ue2)
            , {expr_meta_origintype= Some (returnblock, ut)} )
      | _ ->
          semantic_error
            ("Ill-typed arguments supplied to " ^ opname ^ " operator.") )
  | PrefixOp (op, e) -> (
      let uop = semantic_check_prefixop op in
      let ue = semantic_check_expression e in
      let returnblock =
        lub_op_originblock
          (List.map
             (fun y -> Core_kernel.Option.map y fst)
             [(snd (typed_expression_unroll ue)).expr_meta_origintype])
      in
      let opname = Core_kernel.Sexp.to_string (sexp_of_prefixop uop) in
      match
        try_get_operator_return_type opname
          [(snd (typed_expression_unroll ue)).expr_meta_origintype]
      with
      | Some (ReturnType ut) ->
          TypedExpr
            (PrefixOp (uop, ue), {expr_meta_origintype= Some (returnblock, ut)})
      | _ ->
          semantic_error
            ("Ill-typed arguments supplied to " ^ opname ^ " operator.") )
  | PostfixOp (e, op) -> (
      let ue = semantic_check_expression e in
      let returnblock =
        lub_op_originblock
          (List.map
             (fun y -> Core_kernel.Option.map y fst)
             [(snd (typed_expression_unroll ue)).expr_meta_origintype])
      in
      let uop = semantic_check_postfixop op in
      let opname = Core_kernel.Sexp.to_string (sexp_of_postfixop uop) in
      match
        try_get_operator_return_type opname
          [(snd (typed_expression_unroll ue)).expr_meta_origintype]
      with
      | Some (ReturnType ut) ->
          TypedExpr
            ( PostfixOp (ue, uop)
            , {expr_meta_origintype= Some (returnblock, ut)} )
      | _ ->
          semantic_error
            ("Ill-typed arguments supplied to " ^ opname ^ " operator.") )
  | Variable id ->
      let uid = semantic_check_identifier id in
      let ort = Symbol.look vm id in
      let _ =
        if ort = None && not (is_primitive_name uid) then
          semantic_error "Identifier not in scope."
      in
      TypedExpr
        ( Variable uid
        , { expr_meta_origintype=
              (function
                | None -> Some (Primitives, PrimitiveFunction)
                | Some x -> Some x)
                ort } )
  | IntNumeral s ->
      TypedExpr (IntNumeral s, {expr_meta_origintype= Some (Data, Int)})
  | RealNumeral s ->
      TypedExpr (RealNumeral s, {expr_meta_origintype= Some (Data, Real)})
  | FunApp (id, es) -> (
      let uid = semantic_check_identifier id in
      let ues = List.map semantic_check_expression es in
      let optargumenttypes =
        List.map
          (fun z -> (snd (typed_expression_unroll z)).expr_meta_origintype)
          ues
      in
      let _ =
        if uid = "map_rect" then
          match ues with
          | TypedExpr (Indexed (TypedExpr (Variable arg1_name, _), []), _) :: _
            ->
              if
                Filename.check_suffix arg1_name "_lp"
                || Filename.check_suffix arg1_name "_rng"
              then
                semantic_error
                  ( "Mapped function cannot be an _rng or _lp function, found \
                     function name: " ^ arg1_name )
          | _ -> ()
      in
      let _ =
        if
          Filename.check_suffix uid "_lpdf"
          || Filename.check_suffix uid "_lpdf"
          || Filename.check_suffix uid "_lpmf"
          || Filename.check_suffix uid "_lcdf"
          || Filename.check_suffix uid "_lccdf"
        then
          semantic_error
            "Probabilty functions with suffixes _lpdf, _lpmf, _lcdf, and \
             _lccdf, require a vertical bar (|) between the first two \
             arguments."
      in
      let _ =
        if
          Filename.check_suffix uid "_lp"
          && not
               ( context_flags.in_lp_fun_def
               || context_flags.current_block = Model )
        then
          semantic_error
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      let _ =
        if
          Filename.check_suffix uid "_rng"
          && ( (context_flags.in_fun_def && not context_flags.in_rng_fun_def)
             || context_flags.current_block = TParam
             || context_flags.current_block = Model )
        then
          semantic_error
            "Random number generators only allowed in transformed data block, \
             generated quantities block or user-defined functions with names \
             ending in _rng."
      in
      let returnblock =
        lub_op_originblock
          (List.map (fun x -> Core_kernel.Option.map x fst) optargumenttypes)
      in
      match try_get_primitive_return_type uid optargumenttypes with
      | Some Void ->
          semantic_error
            "A returning function was expected but a non-returning function \
             was supplied."
      | Some (ReturnType ut) ->
          TypedExpr
            (FunApp (uid, ues), {expr_meta_origintype= Some (returnblock, ut)})
      | _ -> (
        match Symbol.look vm uid with
        | Some (_, Fun (_, Void)) ->
            semantic_error
              "A returning function was expected but a non-returning function \
               was supplied."
        | Some (_, Fun (listedtypes, ReturnType ut)) ->
            let _ =
              if
                not
                  (check_compatible_arguments_mod_conv uid listedtypes
                     optargumenttypes)
              then
                semantic_error
                  ("Ill-typed arguments supplied to function " ^ uid)
            in
            TypedExpr
              ( FunApp (uid, ues)
              , {expr_meta_origintype= Some (returnblock, ut)} )
        | Some _ ->
            semantic_error
              "A returning function was expected but a ground type value was \
               supplied."
        | None ->
            semantic_error
              ( "A returning function was expected but an undeclared identifier "
              ^ uid ^ " was supplied." ) ) )
  | CondFunApp (id, es) -> (
      let uid = semantic_check_identifier id in
      let _ =
        if
          not
            ( Filename.check_suffix uid "_lpdf"
            || Filename.check_suffix uid "_lcdf"
            || Filename.check_suffix uid "_lpmf"
            || Filename.check_suffix uid "_lccdf" )
        then
          semantic_error
            "Only functions with names ending in _lpdf, _lpmf, _lcdf, _lccdf \
             can make use of conditional notation."
      in
      let ues = List.map semantic_check_expression es in
      let optargumenttypes =
        List.map
          (fun z -> (snd (typed_expression_unroll z)).expr_meta_origintype)
          ues
      in
      let _ =
        if
          Filename.check_suffix uid "_lp"
          && not
               ( context_flags.in_lp_fun_def
               || context_flags.current_block = Model )
        then
          semantic_error
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      let returnblock =
        lub_op_originblock
          (List.map (fun x -> Core_kernel.Option.map x fst) optargumenttypes)
      in
      match try_get_primitive_return_type uid optargumenttypes with
      | Some Void ->
          semantic_error
            "A returning function was expected but a non-returning function \
             was supplied."
      | Some (ReturnType ut) ->
          TypedExpr
            ( CondFunApp (uid, ues)
            , {expr_meta_origintype= Some (returnblock, ut)} )
      | _ -> (
        match Symbol.look vm uid with
        | Some (_, Fun (_, Void)) ->
            semantic_error
              "A returning function was expected but a non-returning function \
               was supplied."
        | Some (_, Fun (listedtypes, ReturnType ut)) ->
            let _ =
              if
                not
                  (check_compatible_arguments_mod_conv uid listedtypes
                     optargumenttypes)
              then
                semantic_error
                  ("Ill-typed arguments supplied to function " ^ uid)
            in
            TypedExpr
              ( CondFunApp (uid, ues)
              , {expr_meta_origintype= Some (returnblock, ut)} )
        | Some _ ->
            semantic_error
              "A returning function was expected but a ground type value was \
               supplied."
        | None ->
            semantic_error
              ( "A returning function was expected but an undeclared identifier "
              ^ uid ^ " was supplied." ) ) )
  | GetLP ->
      let _ =
        if
          not
            ( context_flags.in_lp_fun_def
            || context_flags.current_block = Model
            || context_flags.current_block = TParam )
        then
          semantic_error
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      TypedExpr
        ( GetLP
        , {expr_meta_origintype= Some (context_flags.current_block, Real)} )
  | GetTarget ->
      let _ =
        if
          not
            ( context_flags.in_lp_fun_def
            || context_flags.current_block = Model
            || context_flags.current_block = TParam )
        then
          semantic_error
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      TypedExpr
        ( GetTarget
        , {expr_meta_origintype= Some (context_flags.current_block, Real)} )
  | ArrayExpr es ->
      let ues = List.map semantic_check_expression es in
      let elementtypes =
        List.map
          (fun y ->
            match snd (typed_expression_unroll y) with
            | {expr_meta_origintype= None} ->
                semantic_error
                  "This should never happen. Please file a bug. Error code 3."
            | {expr_meta_origintype= Some x} -> snd x )
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
          semantic_error
            "Array expression should have entries of consistent type."
      in
      let returnblock =
        lub_op_originblock
          (List.map
             (fun x -> Core_kernel.Option.map x fst)
             (List.map
                (fun z ->
                  (snd (typed_expression_unroll z)).expr_meta_origintype )
                ues))
      in
      TypedExpr
        ( ArrayExpr ues
        , { expr_meta_origintype=
              Some (returnblock, Array (List.hd elementtypes)) } )
  | RowVectorExpr es ->
      let ues = List.map semantic_check_expression es in
      let elementtypes =
        List.map
          (fun y ->
            match snd (typed_expression_unroll y) with
            | {expr_meta_origintype= None} ->
                semantic_error
                  "This should never happen. Please file a bug. Error code 4."
            | {expr_meta_origintype= Some x} -> snd x )
          ues
      in
      let ut =
        if List.for_all (fun x -> x = Real || x = Int) elementtypes then
          RowVector
        else if List.for_all (fun x -> x = RowVector) elementtypes then Matrix
        else
          semantic_error
            "Row_vector expression should have all int and real entries or \
             all row_vector entries."
      in
      let returnblock =
        lub_op_originblock
          (List.map
             (fun x -> Core_kernel.Option.map x fst)
             (List.map
                (fun z ->
                  (snd (typed_expression_unroll z)).expr_meta_origintype )
                ues))
      in
      TypedExpr
        (RowVectorExpr ues, {expr_meta_origintype= Some (returnblock, ut)})
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
                     lub_op_originblock
                       [ Some ob
                       ; Core_kernel.Option.map
                           (snd (typed_expression_unroll ue1))
                             .expr_meta_origintype fst ]
                 | Between (ue1, ue2) ->
                     lub_op_originblock
                       [ Some ob
                       ; Core_kernel.Option.map
                           (snd (typed_expression_unroll ue1))
                             .expr_meta_origintype fst
                       ; Core_kernel.Option.map
                           (snd (typed_expression_unroll ue2))
                             .expr_meta_origintype fst ])
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
                semantic_error
                  "Only expressions of array, matrix, row_vector and vector \
                   type may be indexed." )
      in
      TypedExpr
        ( Indexed (ue, uindices)
        , { expr_meta_origintype=
              ( match
                  (snd (typed_expression_unroll ue)).expr_meta_origintype
                with
              | None ->
                  semantic_error
                    "This should never happen. Please file a bug. Error code \
                     21."
              | Some (ob, ut) ->
                  Some
                    ( inferred_originblock_of_indexed ob uindices
                    , inferred_unsizedtype_of_indexed ut uindices ) ) } )

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
      match (snd (typed_expression_unroll ue)).expr_meta_origintype with
      | Some (_, Fun _) | Some (_, PrimitiveFunction) ->
          semantic_error "Functions cannot be printed."
      | None ->
          semantic_error
            "This should never happen. Please file a bug. Error code 18."
      | _ -> PExpr ue )

and semantic_check_statement s =
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
                 (UntypedExpr (Variable id, {expr_meta_none= None}), lindex)
             , {expr_meta_none= None} ))
      in
      let uid, ulindex =
        match ue2 with
        | TypedExpr (Indexed (TypedExpr (Variable uid, _), ulindex), _) ->
            (uid, ulindex)
        | _ ->
            semantic_error
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
          ( (if is_primitive_name uid then Some Primitives else None)
          , Core_kernel.Option.map (Symbol.look vm uid) fst )
      in
      let _ =
        if Symbol.get_read_only vm uid then
          semantic_error
            "Cannot assign to function argument or loop identifier."
      in
      let _ =
        match uidoblock with
        | Some b ->
            if
              (not (Symbol.get_global vm uid))
              || b = context_flags.current_block
            then ()
            else
              semantic_error
                "Cannot assign to global variable declared in previous blocks."
        | _ ->
            semantic_error
              "This should never happen. Please file a bug. Error code 5."
      in
      (* TODO: the following is very ugly, but we seem to need something like it to
   reproduce the (strange) behaviour in the current Stan that local variables
   have a block level that is determined by what has been assigned to them
   rather than by where they were declared. I'm not sure that behaviour makes
   sense unless we use static analysis as well to make sure these assignments
   actually get evaluated in that phase. *)
      let _ =
        match (snd (typed_expression_unroll ue)).expr_meta_origintype with
        | Some (rhs_ob, _) -> update_originblock uid rhs_ob
        | _ ->
            semantic_error
              "Right hand side of assignment operator references undeclared \
               variable."
      in
      let opname =
        Core_kernel.Sexp.to_string (sexp_of_assignmentoperator uassop)
      in
      match
        try_get_operator_return_type opname
          (List.map
             (fun z -> (snd (typed_expression_unroll z)).expr_meta_origintype)
             [ue2; ue])
      with
      | Some Void ->
          TypedStmt
            ( Assignment
                { assign_identifier= uid
                ; assign_indices= ulindex
                ; assign_op= uassop
                ; assign_rhs= ue }
            , {stmt_meta_type= Some Void} )
      | _ ->
          let lhs_type =
            string_of_expressiontype
              (snd (typed_expression_unroll ue2)).expr_meta_origintype
          in
          let rhs_type =
            string_of_expressiontype
              (snd (typed_expression_unroll ue)).expr_meta_origintype
          in
          semantic_error
            ( "Ill-typed arguments supplied to assignment operator: lhs has\n\
              \          type " ^ lhs_type ^ " and rhs has type " ^ rhs_type
            ^ "." ) )
  | NRFunApp (id, es) -> (
      let uid = semantic_check_identifier id in
      let ues = List.map semantic_check_expression es in
      let optargumenttypes =
        List.map
          (fun z -> (snd (typed_expression_unroll z)).expr_meta_origintype)
          ues
      in
      let _ =
        if
          Filename.check_suffix uid "_lp"
          && not
               ( context_flags.in_lp_fun_def
               || context_flags.current_block = Model )
        then
          semantic_error
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      match try_get_primitive_return_type uid optargumenttypes with
      | Some Void ->
          TypedStmt (NRFunApp (uid, ues), {stmt_meta_type= Some Void})
      | Some (ReturnType _) ->
          semantic_error
            "A non-returning function was expected but a returning function \
             was supplied."
      | _ -> (
        match Symbol.look vm uid with
        | Some (_, Fun (listedtypes, Void)) ->
            let _ =
              if
                not
                  (check_compatible_arguments_mod_conv uid listedtypes
                     optargumenttypes)
              then
                semantic_error
                  ( "Ill-typed arguments supplied to non-returning function "
                  ^ uid )
            in
            TypedStmt (NRFunApp (uid, ues), {stmt_meta_type= Some Void})
        | Some (_, Fun (_, ReturnType _)) ->
            semantic_error
              "A non-returning function was expected but a returning function \
               was supplied."
        | Some _ ->
            semantic_error
              "A non-returning function was expected but a ground type value \
               was supplied."
        | None ->
            semantic_error
              ( "A returning function was expected but an undeclared identifier "
              ^ uid ^ " was supplied." ) ) )
  | TargetPE e ->
      let ue = semantic_check_expression e in
      let _ =
        match (snd (typed_expression_unroll ue)).expr_meta_origintype with
        | Some (_, Fun _) | Some (_, PrimitiveFunction) ->
            semantic_error
              "A (container of) reals or ints needs to be supplied to \
               increment target."
        | None ->
            semantic_error
              "This should never happen. Please file a bug. Error code 19."
        | _ -> ()
      in
      let _ =
        if
          not
            (context_flags.in_lp_fun_def || context_flags.current_block = Model)
        then
          semantic_error
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      TypedStmt (TargetPE ue, {stmt_meta_type= Some Void})
  | IncrementLogProb e ->
      let ue = semantic_check_expression e in
      let _ =
        match (snd (typed_expression_unroll ue)).expr_meta_origintype with
        | Some (_, Fun _) | Some (_, PrimitiveFunction) ->
            semantic_error
              "A (container of) reals or ints needs to be supplied to \
               increment target."
        | None ->
            semantic_error
              "This should never happen. Please file a bug. Error code 20."
        | _ -> ()
      in
      let _ =
        if
          not
            (context_flags.in_lp_fun_def || context_flags.current_block = Model)
        then
          semantic_error
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      TypedStmt (IncrementLogProb ue, {stmt_meta_type= Some Void})
  | Tilde {arg= e; distribution= id; args= es; truncation= t} ->
      let ue = semantic_check_expression e in
      let uid = semantic_check_identifier id in
      let ues = List.map semantic_check_expression es in
      let ut = semantic_check_truncation t in
      let optargumenttypes =
        List.map
          (fun z -> (snd (typed_expression_unroll z)).expr_meta_origintype)
          (ue :: ues)
      in
      let argumenttypes =
        List.map
          (function
            | Some x -> x
            | _ ->
                semantic_error
                  "This should never happen. Please report a bug. Error code 6.")
          optargumenttypes
      in
      let _ =
        if
          not
            (context_flags.in_lp_fun_def || context_flags.current_block = Model)
        then
          semantic_error
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      let _ =
        if
          Filename.check_suffix uid "_cdf" || Filename.check_suffix uid "_ccdf"
        then
          semantic_error
            ( "CDF and CCDF functions may not be used with sampling \
               notation.Use increment_log_prob(" ^ uid ^ "_log(...)) instead."
            )
      in
      let _ =
        if
          try_get_primitive_return_type (uid ^ "_lpdf") optargumenttypes
          = Some (ReturnType Real)
          || try_get_primitive_return_type (uid ^ "_lpmf") optargumenttypes
             = Some (ReturnType Real)
          || try_get_primitive_return_type (uid ^ "_log") optargumenttypes
             = Some (ReturnType Real)
             && uid <> "binomial_coefficient"
             && uid <> "multiply"
          || ( match Symbol.look vm (uid ^ "_lpdf") with
             | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
                 check_compatible_arguments_mod_conv uid listedtypes
                   optargumenttypes
             | _ -> false )
          || ( match Symbol.look vm (uid ^ "_lpmf") with
             | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
                 check_compatible_arguments_mod_conv uid listedtypes
                   optargumenttypes
             | _ -> false )
          ||
          match Symbol.look vm (uid ^ "_log") with
          | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
              check_compatible_arguments_mod_conv uid listedtypes
                optargumenttypes
          | _ -> false
        then ()
        else semantic_error "Ill-typed arguments to '~' statement."
      in
      let _ =
        if
          ut = NoTruncate
          || ( try_get_primitive_return_type (uid ^ "_lcdf") optargumenttypes
               = Some (ReturnType Real)
             ||
             match Symbol.look vm (uid ^ "_lcdf") with
             | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
                 check_compatible_arguments_mod_conv uid listedtypes
                   optargumenttypes
             | _ -> (
                 false
                 || try_get_primitive_return_type (uid ^ "_cdf_log")
                      optargumenttypes
                    = Some (ReturnType Real)
                 ||
                 match Symbol.look vm (uid ^ "_cdf_log") with
                 | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
                     check_compatible_arguments_mod_conv uid listedtypes
                       optargumenttypes
                 | _ -> false ) )
             && ( try_get_primitive_return_type (uid ^ "_lccdf")
                    optargumenttypes
                  = Some (ReturnType Real)
                ||
                match Symbol.look vm (uid ^ "_lccdf") with
                | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
                    check_compatible_arguments_mod_conv uid listedtypes
                      optargumenttypes
                | _ -> (
                    false
                    || try_get_primitive_return_type (uid ^ "_ccdf_log")
                         optargumenttypes
                       = Some (ReturnType Real)
                    ||
                    match Symbol.look vm (uid ^ "_ccdf_log") with
                    | Some (Functions, Fun (listedtypes, ReturnType Real)) ->
                        check_compatible_arguments_mod_conv uid listedtypes
                          optargumenttypes
                    | _ -> false ) )
        then ()
        else
          semantic_error
            "Truncation is only defined if distribution has _lcdf and _lccdf \
             functions implemented."
      in
      TypedStmt
        ( Tilde {arg= ue; distribution= uid; args= ues; truncation= ut}
        , {stmt_meta_type= Some Void} )
  | Break ->
      let _ =
        if not context_flags.in_loop then
          semantic_error "Break statements may only be used in loops."
      in
      TypedStmt (Break, {stmt_meta_type= Some Void})
  | Continue ->
      let _ =
        if not context_flags.in_loop then
          semantic_error "Continue statements may only be used in loops."
      in
      TypedStmt (Continue, {stmt_meta_type= Some Void})
  | Return e ->
      let _ =
        if not context_flags.in_returning_fun_def then
          semantic_error
            "Return statements may only be used inside returning function \
             definitions."
      in
      let ue = semantic_check_expression e in
      TypedStmt
        ( Return ue
        , { stmt_meta_type=
              Core_kernel.Option.map
                (snd (typed_expression_unroll ue)).expr_meta_origintype
                (fun x -> ReturnType (snd x) ) } )
  | Print ps ->
      let ups = List.map semantic_check_printable ps in
      TypedStmt (Print ups, {stmt_meta_type= Some Void})
  | Reject ps ->
      let ups = List.map semantic_check_printable ps in
      TypedStmt (Reject ups, {stmt_meta_type= Some Void})
  | Skip -> TypedStmt (Skip, {stmt_meta_type= Some Void})
  | IfThen (e, s) ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error
            "Condition in conditional needs to be of type int or real."
      in
      let us = semantic_check_statement s in
      TypedStmt (IfThen (ue, us), snd (typed_statement_unroll us))
  | IfThenElse (e, s1, s2) ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error
            "Condition in conditional needs to be of type int or real."
      in
      let us1 = semantic_check_statement s1 in
      let us2 = semantic_check_statement s2 in
      let _ =
        if
          not
            ( snd (typed_statement_unroll us1)
            = snd (typed_statement_unroll us2) )
        then
          semantic_error
            "Branches of conditional need to have the same return type."
      in
      TypedStmt (IfThenElse (ue, us1, us2), snd (typed_statement_unroll us1))
  | While (e, s) ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error
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
          semantic_error "Lower bound of for-loop needs to be of type int."
      in
      let _ =
        if not (check_of_int_type ue2) then
          semantic_error "Upper bound of for-loop needs to be of type int."
      in
      let _ = Symbol.begin_scope vm in
      let _ = check_fresh_variable uid false in
      let oindexblock =
        lub_op_originblock
          (List.map
             (fun x -> Core_kernel.Option.map x fst)
             [ (snd (typed_expression_unroll ue1)).expr_meta_origintype
             ; (snd (typed_expression_unroll ue2)).expr_meta_origintype ])
      in
      let _ = Symbol.enter vm uid (oindexblock, Int) in
      let _ = Symbol.set_read_only vm uid in
      let _ = context_flags.in_loop <- true in
      let us = semantic_check_statement s in
      let _ = context_flags.in_loop <- false in
      let _ = Symbol.end_scope vm in
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
        match (snd (typed_expression_unroll ue)).expr_meta_origintype with
        | Some (_, Array ut) -> ut
        | Some (_, Vector) | Some (_, RowVector) | Some (_, Matrix) -> Real
        | _ ->
            semantic_error
              "Foreach loop must be over array, vector, row_vector or matrix"
      in
      let _ = Symbol.begin_scope vm in
      let _ = check_fresh_variable uid false in
      let oindexblock =
        (function
          | None ->
              semantic_error
                "This should never happen. Please file a bug. Error code 23."
          | Some x -> fst x)
          (snd (typed_expression_unroll ue)).expr_meta_origintype
      in
      let _ = Symbol.enter vm uid (oindexblock, loop_identifier_unsizedtype) in
      let _ = Symbol.set_read_only vm uid in
      let _ = context_flags.in_loop <- true in
      let us = semantic_check_statement s in
      let _ = context_flags.in_loop <- false in
      let _ = Symbol.end_scope vm in
      TypedStmt (ForEach (uid, ue, us), snd (typed_statement_unroll us))
  | Block vdsl ->
      let _ = Symbol.begin_scope vm in
      let uvdsl = List.map semantic_check_statement vdsl in
      let _ = Symbol.end_scope vm in
      (* Any statements after a break or continue do not count for the return
      type. *)
      let rec list_until_breakcontinue = function
        | [] -> []
        | [x] -> [x]
        | x1 :: (Break, _) :: xs -> [x1]
        | x1 :: (Continue, _) :: xs -> [x1]
        | x1 :: x2 :: xs -> x1 :: list_until_breakcontinue (x2 :: xs)
      in
      (* We make sure that for an if-then statement, everything after the then
      block has the same return type as the then block. This could probably
      be done in a prettier way. *)
      let compute_ort_and_check_if_then_branches_agree vdsl2 =
        let rec helper = function
          | [] -> Some Void
          | x :: xs -> (
            match x with
            | false, Some (ReturnType x) -> Some (ReturnType x)
            | true, y ->
                if helper xs = y then y
                else
                  semantic_error
                    "Branches of conditional need to have the same return type."
            | false, Some Void -> helper xs
            | _ ->
                semantic_error
                  "This should never happen. Please report a bug. Error code 7."
            )
        in
        helper
          (List.map
             (function
               | IfThen _, {stmt_meta_type= ort} -> (true, ort)
               | _, {stmt_meta_type= ort} -> (false, ort))
             vdsl2)
      in
      TypedStmt
        ( Block uvdsl
        , { stmt_meta_type=
              compute_ort_and_check_if_then_branches_agree
                (list_until_breakcontinue
                   (List.map typed_statement_unroll uvdsl)) } )
  | VDecl (st, id) ->
      let ust = semantic_check_sizedtype st in
      let uid = semantic_check_identifier id in
      let ut = unsizedtype_of_sizedtype st in
      let _ = check_fresh_variable uid false in
      let _ = Symbol.enter vm id (Functions, ut) in
      TypedStmt (VDecl (ust, uid), {stmt_meta_type= Some Void})
  | VDeclAss {sizedtype= st; identifier= id; value= e} -> (
    match
      ( semantic_check_statement
          (UntypedStmt (VDecl (st, id), {stmt_meta_none= None}))
      , semantic_check_statement
          (UntypedStmt
             ( Assignment
                 { assign_identifier= id
                 ; assign_indices= []
                 ; assign_op= Assign
                 ; assign_rhs= e }
             , {stmt_meta_none= None} )) )
    with
    | ( TypedStmt (VDecl (ust, uid), _)
      , TypedStmt
          ( Assignment
              { assign_identifier= id
              ; assign_indices= []
              ; assign_op= Assign
              ; assign_rhs= ue }
          , {stmt_meta_type= Some Void} ) ) ->
        TypedStmt
          ( VDeclAss {sizedtype= ust; identifier= uid; value= ue}
          , {stmt_meta_type= Some Void} )
    | _ ->
        semantic_error
          "This should never happen. Please file a bug. Error code 2." )
  | TVDecl (st, trans, id) ->
      let ust = semantic_check_sizedtype st in
      let rec check_sizes_below_param_level = function
        | SVector ue -> (
          match (snd (typed_expression_unroll ue)).expr_meta_origintype with
          | Some (Param, _) | Some (TParam, _) | Some (GQuant, _) -> false
          | _ -> true )
        | SRowVector ue -> (
          match (snd (typed_expression_unroll ue)).expr_meta_origintype with
          | Some (Param, _) | Some (TParam, _) | Some (GQuant, _) -> false
          | _ -> true )
        | SMatrix (ue1, ue2) -> (
          match (snd (typed_expression_unroll ue1)).expr_meta_origintype with
          | Some (Param, _) | Some (TParam, _) | Some (GQuant, _) -> false
          | _ -> (
            match (snd (typed_expression_unroll ue2)).expr_meta_origintype with
            | Some (Param, _) | Some (TParam, _) | Some (GQuant, _) -> false
            | _ -> true ) )
        | SArray (ust2, ue) -> (
          match (snd (typed_expression_unroll ue)).expr_meta_origintype with
          | Some (Param, _) | Some (TParam, _) | Some (GQuant, _) -> false
          | _ -> check_sizes_below_param_level ust2 )
        | _ -> true
      in
      let _ =
        if not (check_sizes_below_param_level ust) then
          semantic_error
            "Non-data variables are not allowed in top level size declarations."
      in
      let utrans = semantic_check_transformation trans in
      let uid = semantic_check_identifier id in
      let ut = unsizedtype_of_sizedtype st in
      let _ = check_fresh_variable uid false in
      let _ = Symbol.enter vm uid (context_flags.current_block, ut) in
      let _ = Symbol.set_global vm uid in
      let _ =
        if
          ust = SInt
          &&
          match utrans with
          | Lower ue1 -> (
            match (snd (typed_expression_unroll ue1)).expr_meta_origintype with
            | Some (_, Real) -> true
            | _ -> false )
          | Upper ue1 -> (
            match (snd (typed_expression_unroll ue1)).expr_meta_origintype with
            | Some (_, Real) -> true
            | _ -> false )
          | LowerUpper (ue1, ue2) -> (
            match (snd (typed_expression_unroll ue1)).expr_meta_origintype with
            | Some (_, Real) -> true
            | _ -> (
              match
                (snd (typed_expression_unroll ue2)).expr_meta_origintype
              with
              | Some (_, Real) -> true
              | _ -> false ) )
          | _ -> false
        then
          semantic_error
            "Bounds of integer variable should be of type int. Found type real."
      in
      let _ =
        if
          ( context_flags.current_block = Param
          || context_flags.current_block = TParam )
          && unsizedtype_contains_int ut
        then semantic_error "(Transformed) Parameters cannot be integers."
      in
      TypedStmt (TVDecl (ust, utrans, uid), {stmt_meta_type= Some Void})
  | TVDeclAss
      {tsizedtype= st; transformation= trans; tidentifier= id; tvalue= e} -> (
    match
      ( semantic_check_statement
          (UntypedStmt (TVDecl (st, trans, id), {stmt_meta_none= None}))
      , semantic_check_statement
          (UntypedStmt
             ( Assignment
                 { assign_identifier= id
                 ; assign_indices= []
                 ; assign_op= Assign
                 ; assign_rhs= e }
             , {stmt_meta_none= None} )) )
    with
    | ( TypedStmt (TVDecl (ust, utrans, uid), _)
      , TypedStmt
          ( Assignment
              { assign_identifier= id
              ; assign_indices= []
              ; assign_op= Assign
              ; assign_rhs= ue }
          , {stmt_meta_type= Some Void} ) ) ->
        TypedStmt
          ( TVDeclAss
              { tsizedtype= ust
              ; transformation= utrans
              ; tidentifier= uid
              ; tvalue= ue }
          , {stmt_meta_type= Some Void} )
    | _ ->
        semantic_error
          "This should never happen. Please file a bug. Error code 1." )
  | FunDef {returntype= rt; name= id; arguments= args; body= b} ->
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
      let uarg_types = List.map (function w, y, z -> (w, y)) uargs in
      let _ =
        if Symbol.is_missing_fun_def vm uid then (
          if Symbol.look vm uid <> Some (Functions, Fun (uarg_types, urt)) then
            semantic_error
              ( "Function " ^ uid ^ " has already been declared to have type "
              ^ string_of_expressiontype (Symbol.look vm uid) ) )
        else check_fresh_variable uid (List.length uarg_types = 0)
      in
      let _ =
        match b with
        | UntypedStmt (Skip, _) ->
            if Symbol.is_missing_fun_def vm uid then
              semantic_error
                ( "Function " ^ uid
                ^ " has already been declared. A definition is expected." )
            else Symbol.add_is_missing_fun_def vm uid
        | _ -> Symbol.remove_is_missing_fun_def vm uid
      in
      let _ = Symbol.enter vm uid (Functions, Fun (uarg_types, urt)) in
      let uarg_names = List.map (function w, y, z -> z) uargs in
      let _ = List.map (Symbol.set_read_only vm) uarg_names in
      let _ =
        if
          urt <> ReturnType Real
          && ( Filename.check_suffix uid "_log"
             || Filename.check_suffix uid "_lpdf"
             || Filename.check_suffix uid "_lpmf"
             || Filename.check_suffix uid "_lcdf"
             || Filename.check_suffix uid "_lccdf" )
        then
          semantic_error
            "Real return type required for probability functions ending in \
             _log, _lpdf, _lpmf, _lcdf, or _lccdf."
      in
      let _ =
        if
          Filename.check_suffix uid "_lpdf"
          && (List.length uarg_types = 0 || snd (List.hd uarg_types) <> Real)
        then
          semantic_error
            "Probability density functions require real variates (first \
             argument)."
      in
      let _ =
        if
          Filename.check_suffix uid "_lpmf"
          && (List.length uarg_types = 0 || snd (List.hd uarg_types) <> Int)
        then
          semantic_error
            "Probability mass functions require integer variates (first \
             argument)."
      in
      let _ = context_flags.in_fun_def <- true in
      let _ =
        if Filename.check_suffix uid "_rng" then
          context_flags.in_rng_fun_def <- true
      in
      let _ =
        if Filename.check_suffix uid "_lp" then
          context_flags.in_lp_fun_def <- true
      in
      let _ = if urt <> Void then context_flags.in_returning_fun_def <- true in
      let _ = Symbol.begin_scope vm in
      let _ =
        if dup_exists uarg_names then
          semantic_error
            "All function arguments should be distinct identifiers."
      in
      let _ = List.map (fun x -> check_fresh_variable x false) uarg_names in
      let _ = List.map2 (Symbol.enter vm) uarg_names uarg_types in
      let ub = semantic_check_statement b in
      let _ =
        if
          Symbol.is_missing_fun_def vm uid
          || check_of_compatible_return_type (Some urt)
               (snd (typed_statement_unroll ub)).stmt_meta_type
        then ()
        else
          semantic_error
            "Function bodies must contain a return statement of correct type \
             in every branch."
      in
      let _ = Symbol.end_scope vm in
      let _ = context_flags.in_fun_def <- false in
      let _ = context_flags.in_returning_fun_def <- false in
      let _ = context_flags.in_lp_fun_def <- false in
      let _ = context_flags.in_rng_fun_def <- false in
      TypedStmt
        ( FunDef {returntype= urt; name= uid; arguments= uargs; body= ub}
        , {stmt_meta_type= Some Void} )

and semantic_check_truncation = function
  | NoTruncate -> NoTruncate
  | TruncateUpFrom e ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error "Truncation bound should be of type int or real."
      in
      TruncateUpFrom ue
  | TruncateDownFrom e ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error "Truncation bound should be of type int or real."
      in
      TruncateDownFrom ue
  | TruncateBetween (e1, e2) ->
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let _ =
        if not (check_of_int_or_real_type ue1 && check_of_int_or_real_type ue2)
        then semantic_error "Truncation bound should be of type int or real."
      in
      TruncateBetween (ue1, ue2)

and semantic_check_index = function
  | All -> All
  | Single e ->
      let ue = semantic_check_expression e in
      if check_of_int_type ue then Single ue
      else if check_of_int_array_type ue then Multiple ue
      else
        semantic_error
          "Index should be of type int or int[] or should be a range."
  | Upfrom e ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_type ue) then
          semantic_error "Range bound should be of type int."
      in
      Upfrom ue
  | Downfrom e ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_type ue) then
          semantic_error "Range bound should be of type int."
      in
      Downfrom ue
  | Between (e1, e2) ->
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let _ =
        if not (check_of_int_type ue1 && check_of_int_type ue2) then
          semantic_error "Range bound should be of type int."
      in
      Between (ue1, ue2)
  | Multiple e ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_array_type ue) then
          semantic_error
            "This should never happen. Please file a bug. Error code 17."
      in
      Multiple ue

(* Probably nothing to do here *)
and semantic_check_assignmentoperator op = op
