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
TODO: we cannot assign to function arguments.
TODO: Allow math library functions to clash with variable names as long as signatures/types differ. I.e. users can overload library functions.
*)

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
  | [] -> Functions
  | x :: xs ->
      let y = lub_originblock xs in
      if compare_originblock x y < 0 then y else x

let lub_op_originblock l =
  lub_originblock (List.map (function None -> Functions | Some b -> b) l)

let check_of_int_type e = match snd e with Some (_, Int) -> true | _ -> false

let check_of_int_or_real_type e =
  match snd e with
  | Some (_, Int) -> true
  | Some (_, Real) -> true
  | _ -> false

(* TODO: insert positions into semantic errors! *)

let check_fresh_variable id =
  let _ =
    if is_primitive_name id then
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
    let ubf = semantic_check_functionblock bf in
    let _ = context_flags.current_block <- Data in
    let ubd = semantic_check_datablock bd in
    let _ = context_flags.current_block <- TData in
    let ubtd = semantic_check_transformeddatablock btd in
    let _ = context_flags.current_block <- Param in
    let ubp = semantic_check_parametersblock bp in
    let _ = context_flags.current_block <- TParam in
    let ubtp = semantic_check_transformedparametersblock btp in
    let _ = context_flags.current_block <- Model in
    let _ = Symbol.begin_scope vm in
    let ubm = semantic_check_modelblock bm in
    let _ = Symbol.end_scope vm in
    let _ = context_flags.current_block <- GQuant in
    let ubgq = semantic_check_generatedquantitiesblock bgq in
    { functionblock= ubf
    ; datablock= ubd
    ; transformeddatablock= ubtd
    ; parametersblock= ubp
    ; transformedparametersblock= ubtp
    ; modelblock= ubm
    ; generatedquantitiesblock= ubgq }

(* Probably nothing to do here *)
and semantic_check_functionblock bf =
  Core_kernel.Option.map bf (List.map semantic_check_fundef)

(* Probably nothing to do here *)
and semantic_check_datablock bd =
  Core_kernel.Option.map bd (List.map semantic_check_topvardecl)

(* Probably nothing to do here *)
and semantic_check_transformeddatablock btd =
  Core_kernel.Option.map btd (List.map semantic_check_topvardecl_or_statement)

(* Probably nothing to do here *)
and semantic_check_parametersblock bp =
  Core_kernel.Option.map bp (List.map semantic_check_topvardecl)

(* Probably nothing to do here *)
and semantic_check_transformedparametersblock btp =
  Core_kernel.Option.map btp (List.map semantic_check_topvardecl_or_statement)

(* Probably nothing to do here *)
and semantic_check_modelblock bm =
  Core_kernel.Option.map bm (List.map semantic_check_vardecl_or_statement)

(* Probably nothing to do here *)
and semantic_check_generatedquantitiesblock bgq =
  Core_kernel.Option.map bgq (List.map semantic_check_topvardecl_or_statement)

(* TODO: deal properly with recursive functions here. *)
and semantic_check_fundef = function
  | {returntype= rt; name= id; arguments= args; body= b} ->
      let urt = semantic_check_returntype rt in
      let uid = semantic_check_identifier id in
      let _ = check_fresh_variable uid in
      let uargs = List.map semantic_check_argdecl args in
      let uarg_types = List.map (function w, y, z -> (w, y)) uargs in
      let _ =
        Symbol.enter vm uid (context_flags.current_block, Fun (uarg_types, urt))
      in
      let uarg_names = List.map (function w, y, z -> z) uargs in
      let _ =
        if dup_exists uarg_names then
          semantic_error
            "All function arguments should be distinct identifiers."
      in
      let _ = context_flags.in_fun_def <- true in
      let _ =
        if Filename.check_suffix id "_rng" then
          context_flags.in_rng_fun_def <- true
      in
      let _ =
        if Filename.check_suffix id "_lp" then
          context_flags.in_lp_fun_def <- true
      in
      let _ = if urt <> Void then context_flags.in_returning_fun_def <- true in
      let _ = Symbol.begin_scope vm in
      (* This is a bit of a hack to make sure that function arguments cannot be assigned to. *)
      let _ =
        List.map2 (Symbol.enter vm) uarg_names
          (List.map (function w, y -> (Functions, y)) uarg_types)
      in
      let ub = semantic_check_statement b in
      let _ =
        if snd ub <> Some urt then
          semantic_error
            "Function bodies must contain a return statement of correct type \
             in every branch."
      in
      let _ = Symbol.end_scope vm in
      let _ = context_flags.in_fun_def <- false in
      let _ = context_flags.in_returning_fun_def <- false in
      let _ = context_flags.in_lp_fun_def <- false in
      let _ = context_flags.in_rng_fun_def <- false in
      {returntype= urt; name= uid; arguments= uargs; body= ub}

(* Probably nothing to do here *)
and semantic_check_identifier id = id

(* TODO: This could be one place where we check for reserved variable names. Though it would be nicer to just do it in the lexer. *)

(* Probably nothing to do here *)
and semantic_check_originblock ob = ob

(* Probably nothing to do here *)
and semantic_check_argdecl = function
  | ob, ut, id ->
      ( semantic_check_originblock ob
      , semantic_check_unsizedtype ut
      , semantic_check_identifier id )

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

and semantic_check_topvardecl = function
  | st, trans, id ->
      let ust = semantic_check_sizedtype st in
      let utrans = semantic_check_transformation trans in
      let uid = semantic_check_identifier id in
      let ut = unsizedtype_of_sizedtype st in
      let _ = check_fresh_variable uid in
      let _ = Symbol.enter vm id (context_flags.current_block, ut) in
      let _ =
        if
          ( context_flags.current_block = Param
          || context_flags.current_block = TParam )
          && unsizedtype_contains_int ut
        then semantic_error "(Transformed) Parameters cannot be integers."
      in
      (ust, utrans, uid)

(* Probably nothing to check here. *)
and semantic_check_compound_topvardecl_assign = function
  | {sizedtype= st; transformation= trans; identifier= id; value= e} -> (
      let ust, utrans, uid = semantic_check_topvardecl (st, trans, id) in
      match
        semantic_check_statement (Assignment ((uid, []), Assign, e), None)
      with
      | Assignment ((uid, []), Assign, ue), Some Void ->
          {sizedtype= ust; transformation= utrans; identifier= uid; value= ue}
      | _ ->
          semantic_error
            "This should never happen. Please file a bug. Error code 1." )

and semantic_check_vardecl = function
  | st, id ->
      let ust = semantic_check_sizedtype st in
      let uid = semantic_check_identifier id in
      let ut = unsizedtype_of_sizedtype st in
      let _ = check_fresh_variable uid in
      let _ = Symbol.enter vm id (context_flags.current_block, ut) in
      (ust, uid)

(* Probably nothing to check here. *)
and semantic_check_compound_vardecl_assign = function
  | {sizedtype= st; identifier= id; value= e} -> (
      let ust, uid = semantic_check_vardecl (st, id) in
      match
        semantic_check_statement (Assignment ((uid, []), Assign, e), None)
      with
      | Assignment ((uid, []), Assign, ue), Some Void ->
          {sizedtype= ust; identifier= uid; value= ue}
      | _ ->
          semantic_error
            "This should never happen. Please file a bug. Error code 2." )

(* Probably nothing to do here *)
and semantic_check_topvardecl_or_statement tvds =
  match tvds with
  | TVDecl tvd -> TVDecl (semantic_check_topvardecl tvd)
  | TStmt s -> TStmt (semantic_check_statement s)
  | TVDeclAss tvda ->
      TVDeclAss (semantic_check_compound_topvardecl_assign tvda)

(* Probably nothing to do here *)
and semantic_check_vardecl_or_statement vds =
  match vds with
  | VDecl vd -> VDecl (semantic_check_vardecl vd)
  | Stmt s -> Stmt (semantic_check_statement s)
  | VDeclAss vda -> VDeclAss (semantic_check_compound_vardecl_assign vda)

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
  match fst x with
  | Conditional (e1, e2, e3) -> (
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let ue3 = semantic_check_expression e3 in
      let returnblock =
        lub_op_originblock
          (List.map
             (fun y -> Core_kernel.Option.map y fst)
             (List.map snd [ue1; ue2; ue3]))
      in
      match
        try_get_operator_return_type "Conditional" [snd ue1; snd ue2; snd ue3]
      with
      | Some (ReturnType ut) ->
          (Conditional (ue1, ue2, ue3), Some (returnblock, ut))
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
             (List.map snd [ue1; ue2]))
      in
      let opname = Core_kernel.string_of_sexp (sexp_of_infixop uop) in
      match try_get_operator_return_type opname [snd ue1; snd ue2] with
      | Some (ReturnType ut) ->
          (InfixOp (ue1, uop, ue2), Some (returnblock, ut))
      | _ ->
          semantic_error
            ("Ill-typed arguments supplied to " ^ opname ^ " operator.") )
  | PrefixOp (op, e) -> (
      let uop = semantic_check_prefixop op in
      let ue = semantic_check_expression e in
      let returnblock =
        lub_op_originblock
          (List.map (fun y -> Core_kernel.Option.map y fst) (List.map snd [ue]))
      in
      let opname = Core_kernel.string_of_sexp (sexp_of_prefixop uop) in
      match try_get_operator_return_type opname [snd ue] with
      | Some (ReturnType ut) -> (PrefixOp (uop, ue), Some (returnblock, ut))
      | _ ->
          semantic_error
            ("Ill-typed arguments supplied to " ^ opname ^ " operator.") )
  | PostfixOp (e, op) -> (
      let ue = semantic_check_expression e in
      let returnblock =
        lub_op_originblock
          (List.map (fun y -> Core_kernel.Option.map y fst) (List.map snd [ue]))
      in
      let uop = semantic_check_postfixop op in
      let opname = Core_kernel.string_of_sexp (sexp_of_postfixop uop) in
      match try_get_operator_return_type opname [snd ue] with
      | Some (ReturnType ut) -> (PostfixOp (ue, uop), Some (returnblock, ut))
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
      (Variable uid, ort)
  | IntNumeral s -> (IntNumeral s, Some (Data, Int))
  | RealNumeral s -> (RealNumeral s, Some (Data, Real))
  | FunApp (id, es) -> (
      let uid = semantic_check_identifier id in
      let ues = List.map semantic_check_expression es in
      let optargumenttypes = List.map snd ues in
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
          (List.map (fun x -> Core_kernel.Option.map x fst) (List.map snd ues))
      in
      match try_get_primitive_return_type uid optargumenttypes with
      | Some Void ->
          semantic_error
            "A returning function was expected but a non-returning function \
             was supplied."
      | Some (ReturnType ut) -> (FunApp (uid, ues), Some (returnblock, ut))
      | _ -> (
        match Symbol.look vm uid with
        | Some (_, Fun (_, Void)) ->
            semantic_error
              "A returning function was expected but a non-returning function \
               was supplied."
        | Some (_, Fun (_, ReturnType ut)) ->
            (FunApp (uid, ues), Some (returnblock, ut))
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
          Filename.check_suffix uid "_lpdf"
          || Filename.check_suffix uid "_lcdf"
          || Filename.check_suffix uid "_lpmf"
          || Filename.check_suffix uid "_lccdf"
        then
          semantic_error
            "Only functions with names ending in _lpdf, _lpmf, _lcdf, _lccdf \
             can make use of conditional notation."
      in
      let ues = List.map semantic_check_expression es in
      let optargumenttypes = List.map snd ues in
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
          (List.map (fun x -> Core_kernel.Option.map x fst) (List.map snd ues))
      in
      match try_get_primitive_return_type uid optargumenttypes with
      | Some Void ->
          semantic_error
            "A returning function was expected but a non-returning function \
             was supplied."
      | Some (ReturnType ut) -> (CondFunApp (uid, ues), Some (returnblock, ut))
      | _ -> (
        match Symbol.look vm uid with
        | Some (_, Fun (_, Void)) ->
            semantic_error
              "A returning function was expected but a non-returning function \
               was supplied."
        | Some (_, Fun (_, ReturnType ut)) ->
            (CondFunApp (uid, ues), Some (returnblock, ut))
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
            (context_flags.in_lp_fun_def || context_flags.current_block = Model)
        then
          semantic_error
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      (GetLP, Some (context_flags.current_block, Real))
  | GetTarget ->
      let _ =
        if
          not
            (context_flags.in_lp_fun_def || context_flags.current_block = Model)
        then
          semantic_error
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      (GetTarget, Some (context_flags.current_block, Real))
  | ArrayExpr es ->
      let ues = List.map semantic_check_expression es in
      let elementtypes =
        List.map
          (fun y ->
            match snd y with
            | None ->
                semantic_error
                  "This should never happen. Please file a bug. Error code 3."
            | Some x -> snd x )
          ues
      in
      let _ =
        if List.exists (fun x -> x <> List.hd elementtypes) elementtypes then
          semantic_error
            "Array expression should have entries of consistent type."
      in
      let returnblock =
        lub_op_originblock
          (List.map (fun x -> Core_kernel.Option.map x fst) (List.map snd ues))
      in
      (ArrayExpr ues, Some (returnblock, Array (List.hd elementtypes)))
  | RowVectorExpr es ->
      let ues = List.map semantic_check_expression es in
      let elementtypes =
        List.map
          (fun y ->
            match snd y with
            | None ->
                semantic_error
                  "This should never happen. Please file a bug. Error code 4."
            | Some x -> snd x )
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
          (List.map (fun x -> Core_kernel.Option.map x fst) (List.map snd ues))
      in
      (RowVectorExpr ues, Some (returnblock, ut))
  | Paren e ->
      let ue = semantic_check_expression e in
      (Paren ue, snd ue)
  | Indexed (e, indices) ->
      let ue = semantic_check_expression e in
      let uindices = List.map semantic_check_index indices in
      let infer_type_of_indexed etype index =
        let originaltype, reducedtype =
          match etype with
          | Some (_, Array ut) -> (Array ut, ut)
          | Some (_, Vector) -> (Vector, Real)
          | Some (_, RowVector) -> (RowVector, Real)
          | Some (_, Matrix) -> (Matrix, RowVector)
          | _ ->
              semantic_error
                "Only expressions of array, matrix, row_vector and vector \
                 type may be indexed."
        in
        match index with
        | All -> etype
        | Single e1 ->
            let e1type = snd e1 in
            let originblock =
              lub_op_originblock
                (List.map
                   (fun x -> Core_kernel.Option.map x fst)
                   [etype; e1type])
            in
            Some (originblock, reducedtype)
        | Upfrom e1 | Downfrom e1 ->
            let e1type = snd e1 in
            let originblock =
              lub_op_originblock
                (List.map
                   (fun x -> Core_kernel.Option.map x fst)
                   [etype; e1type])
            in
            Some (originblock, originaltype)
        | Between (e1, e2) ->
            let e1type = snd e1 in
            let e2type = snd e2 in
            let originblock =
              lub_op_originblock
                (List.map
                   (fun x -> Core_kernel.Option.map x fst)
                   [etype; e1type; e2type])
            in
            Some (originblock, originaltype)
      in
      ( Indexed (ue, uindices)
      , List.fold_left infer_type_of_indexed (snd ue) uindices )

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
      match snd ue with
      | Some (_, Fun _) -> semantic_error "Functions cannot be printed."
      | None -> semantic_error "Primitives cannot be printed."
      | _ -> PExpr ue )

and semantic_check_statement s =
  match fst s with
  | Assignment ((id, lindex), assop, e) -> (
      (* TODO: The lhs is effectively checked twice here, because I am lazy. *)
      let uid, ulindex = semantic_check_lhs (id, lindex) in
      let uassop = semantic_check_assignmentoperator assop in
      let ue = semantic_check_expression e in
      let ue2 =
        semantic_check_expression
          (Indexed ((Variable uid, None), ulindex), None)
      in
      let uidoblock =
        if is_primitive_name uid then Some Functions
        else Core_kernel.Option.map (Symbol.look vm uid) fst
      in
      let _ =
        match uidoblock with
        | Some Functions -> semantic_error "Cannot assign to this identifier."
        | Some Data -> semantic_error "Cannot assign to data."
        | Some Param -> semantic_error "Cannot assign to parameter."
        | Some b ->
            if b = context_flags.current_block then ()
            else
              semantic_error
                "Cannot assign to variable declared in previous blocks."
        | _ ->
            semantic_error
              "This should never happen. Please file a bug. Error code 5."
      in
      let opname =
        Core_kernel.string_of_sexp (sexp_of_assignmentoperator uassop)
      in
      match try_get_operator_return_type opname [snd ue2; snd ue] with
      | Some Void -> (Assignment ((uid, ulindex), uassop, ue), Some Void)
      | _ ->
          semantic_error "Ill-typed arguments supplied to assignment operator."
      )
  | NRFunApp (id, es) -> (
      let uid = semantic_check_identifier id in
      let ues = List.map semantic_check_expression es in
      let optargumenttypes = List.map snd ues in
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
      | Some Void -> (NRFunApp (uid, ues), Some Void)
      | Some (ReturnType _) ->
          semantic_error
            "A non-returning function was expected but a returning function \
             was supplied."
      | _ -> (
        match Symbol.look vm uid with
        | Some (_, Fun (_, Void)) -> (NRFunApp (uid, ues), Some Void)
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
        if not (check_of_int_or_real_type ue) then
          semantic_error
            "A real or int needs to be supplied to increment target."
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
      (TargetPE ue, Some Void)
  | IncrementLogProb e ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error
            "A real or int needs to be supplied to increment LogProb."
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
      (IncrementLogProb ue, Some Void)
  | Tilde {arg= e; distribution= id; args= es; truncation= t} ->
      let ue = semantic_check_expression e in
      let uid = semantic_check_identifier id in
      let ues = List.map semantic_check_expression es in
      let ut = semantic_check_truncation t in
      let optargumenttypes = snd ue :: List.map snd ues in
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
          try_get_primitive_return_type (uid ^ "_lpdf") optargumenttypes
          = Some (ReturnType Real)
          || try_get_primitive_return_type (uid ^ "_lpmf") optargumenttypes
             = Some (ReturnType Real)
          || Symbol.look vm (uid ^ "_lpdf")
             = Some (Functions, Fun (argumenttypes, ReturnType Real))
          || Symbol.look vm (uid ^ "_lpmf")
             = Some (Functions, Fun (argumenttypes, ReturnType Real))
        then ()
        else semantic_error "Ill-typed arguments to '~' statement."
      in
      (Tilde {arg= ue; distribution= uid; args= ues; truncation= ut}, Some Void)
  | Break ->
      let _ =
        if not context_flags.in_loop then
          semantic_error "Break statements may only be used in loops."
      in
      (Break, Some Void)
  | Continue ->
      let _ =
        if not context_flags.in_loop then
          semantic_error "Continue statements may only be used in loops."
      in
      (Continue, Some Void)
  | Return e ->
      let _ =
        if not context_flags.in_returning_fun_def then
          semantic_error
            "Return statements may only be used inside returning function \
             definitions."
      in
      let ue = semantic_check_expression e in
      (Return ue, Core_kernel.Option.map (snd ue) (fun x -> ReturnType (snd x)))
  | Print ps ->
      let ups = List.map semantic_check_printable ps in
      (Print ups, Some Void)
  | Reject ps ->
      let ups = List.map semantic_check_printable ps in
      (Reject ups, Some Void)
  | Skip -> (Skip, Some Void)
  | IfThen (e, s) ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_or_real_type ue) then
          semantic_error
            "Condition in conditional needs to be of type int or real."
      in
      let us = semantic_check_statement s in
      (IfThen (ue, us), snd us)
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
        if not (snd us1 = snd us2) then
          semantic_error
            "Branches of conditional need to have the same return type."
      in
      (IfThenElse (ue, us1, us2), snd us1)
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
      (While (ue, us), snd us)
      (* OK until here *)
  | For {loop_variable= id; lower_bound= e1; upper_bound= e2; loop_body= s} ->
      let uid = semantic_check_identifier id in
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let _ =
        if not (check_of_int_or_real_type ue1) then
          semantic_error "Lower bound of for-loop needs to be of type int."
      in
      let _ =
        if not (check_of_int_or_real_type ue2) then
          semantic_error "Upper bound of for-loop needs to be of type int."
      in
      let _ = Symbol.begin_scope vm in
      let _ = check_fresh_variable uid in
      (* This is a bit of a hack to ensure that loop identifiers cannot get assigned to. *)
      let _ = Symbol.enter vm uid (Functions, Int) in
      let _ = context_flags.in_loop <- true in
      let us = semantic_check_statement s in
      let _ = context_flags.in_loop <- false in
      let _ = Symbol.end_scope vm in
      ( For
          { loop_variable= uid
          ; lower_bound= ue1
          ; upper_bound= ue2
          ; loop_body= us }
      , snd us )
  | ForEach (id, e, s) ->
      let uid = semantic_check_identifier id in
      let ue = semantic_check_expression e in
      let loop_identifier_unsizedtype =
        match snd ue with
        | Some (_, Array ut) -> ut
        | Some (_, Vector) | Some (_, RowVector) | Some (_, Matrix) -> Real
        | _ ->
            semantic_error
              "Foreach loop must be over array, vector, row_vector or matrix"
      in
      let _ = Symbol.begin_scope vm in
      let _ = check_fresh_variable uid in
      (* This is a bit of a hack to ensure that loop identifiers cannot get assigned to. *)
      let _ = Symbol.enter vm uid (Functions, loop_identifier_unsizedtype) in
      let _ = context_flags.in_loop <- true in
      let us = semantic_check_statement s in
      let _ = context_flags.in_loop <- false in
      let _ = Symbol.end_scope vm in
      (While (ue, us), snd us)
  | Block vdsl ->
      let _ = Symbol.begin_scope vm in
      let uvdsl = List.map semantic_check_vardecl_or_statement vdsl in
      let _ = Symbol.end_scope vm in
      let rec compute_ort = function
        | [] -> Some Void
        | x :: xs -> (
          match x with
          | false, Some (ReturnType x) -> Some (ReturnType x)
          | true, y ->
              if compute_ort xs = y then y
              else
                semantic_error
                  "Branches of conditional need to have the same return type."
          | false, Some Void -> compute_ort xs
          | _ ->
              semantic_error
                "This should never happen. Please report a bug. Error code 7."
          )
      in
      let temp =
        List.map
          (function
            | VDecl x -> (false, Some Void)
            | VDeclAss x -> (false, Some Void)
            | Stmt (IfThen _, ort) -> (true, ort)
            | Stmt (_, ort) -> (false, ort))
          uvdsl
      in
      (Block uvdsl, compute_ort temp)

(* TODO: this is still not doing the correct thing in case a statement
         has dead code due to break or continue statements      *)
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

and semantic_check_lhs (id, lindex) =
  match
    semantic_check_expression (Indexed ((Variable id, None), lindex), None)
  with
  | Indexed ((Variable uid, _), ulindex), _ -> (uid, ulindex)
  | _ ->
      semantic_error
        "This should never happen. Please file a bug. Error code 8."

and semantic_check_index = function
  | All -> All
  | Single e ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_type ue) then
          semantic_error "Index should be of type int."
      in
      Single ue
  | Upfrom e ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_type ue) then
          semantic_error "Index should be of type int."
      in
      Upfrom ue
  | Downfrom e ->
      let ue = semantic_check_expression e in
      let _ =
        if not (check_of_int_type ue) then
          semantic_error "Index should be of type int."
      in
      Downfrom ue
  | Between (e1, e2) ->
      let ue1 = semantic_check_expression e1 in
      let ue2 = semantic_check_expression e2 in
      let _ =
        if not (check_of_int_type ue1 && check_of_int_type ue2) then
          semantic_error "Index should be of type int."
      in
      Between (ue1, ue2)

(* Probably nothing to do here *)
and semantic_check_assignmentoperator op = op

(* OK up until here except expressions and statements *)
