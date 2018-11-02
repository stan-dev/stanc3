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
*)

open Symbol_table
open Syntax

(* Idea: we have a semantic checking function for each AST node.
   Each such calls the corresponding checking functions for its children
   left-to-right. *)

(* Invariant: after an expression has been checked, it has a well-defined type *)

(* Invariant: after a statement has been checked, it has a well-defined return type *)

(* A semantic error reported by the toplevel *)
let semantic_error ?loc msg =
  Zoo.error ~kind:"Semantic error" ?loc (Scanf.format_from_string msg "")

(* e.g. compare Data Model = -1  and compare GQuant Functions = 1 *)

(* NB DANGER: this file specifies an imperative tree algorithm which
   decorates the AST while operating on two bits of state:
   1) a global symbol table vm
   2) some context flags context_flags, to communicate information down
      the AST   *)

(* This is ugly. An ideal treatment of function overloading works by carrying around*
   a LAZY set of types for each expression. However, that's awkward in OCaml.
   Perhaps an argument for Haskell after all?
   OCaml does have lazy lists. Perhaps those could be used for this purpose?
   Or implement our own lazy sets?
   
*)
(* TODO: first load whole math library into try_get_primitive_return_type -- we are using a predicate here because the functions are overloaded so heavily  *)
let try_get_primitive_return_type name argtypes = None

(* TODO *)
let is_primitive_name name = false

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

let look_block id = Core_kernel.Option.map (Symbol.look vm id) snd

let rec lub_originblock = function
  | [] -> Functions
  | x :: xs ->
      let y = lub_originblock xs in
      if compare_originblock x y < 0 then y else x

let check_of_int_type e = match snd e with Some (_, Int) -> true | _ -> false

let check_of_real_type e =
  match snd e with Some (_, Real) -> true | _ -> false

let check_of_int_or_real_type e =
  match snd e with
  | Some (_, Int) -> true
  | Some (_, Real) -> true
  | _ -> false

(* TODO!!! *)
let check_compatible_indices e lindex =
  semantic_error "not implemented" ;
  true

let check_of_same_type_mod_conv e1 e2 =
  match snd e1 with
  | Some (_, t1) -> (
    match snd e2 with
    | Some (_, t2) ->
        t1 = t2 || (t1 = Real && t1 = Int) || (t1 = Int && t1 = Real)
    | _ -> false )
  | _ -> false

(* TODO: insert positions into semantic errors! *)

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

and semantic_check_functionblock bf =
  Core_kernel.Option.map bf (List.map semantic_check_fundef)

and semantic_check_datablock bd =
  Core_kernel.Option.map bd (List.map semantic_check_topvardecl)

and semantic_check_transformeddatablock btd =
  Core_kernel.Option.map btd (List.map semantic_check_topvardecl_or_statement)

and semantic_check_parametersblock bp =
  Core_kernel.Option.map bp (List.map semantic_check_topvardecl)

and semantic_check_transformedparametersblock btp =
  Core_kernel.Option.map btp (List.map semantic_check_topvardecl_or_statement)

and semantic_check_modelblock bm =
  Core_kernel.Option.map bm (List.map semantic_check_vardecl_or_statement)

and semantic_check_generatedquantitiesblock bgq =
  Core_kernel.Option.map bgq (List.map semantic_check_topvardecl_or_statement)

and semantic_check_fundef = function
  | {returntype= rt; name= id; arguments= args; body= b} ->
      let urt = semantic_check_returntype rt in
      let uid = semantic_check_identifier id in
      let _ =
        if is_primitive_name uid then
          let error_msg =
            String.concat " " ["Identifier "; id; " clashes with primitive."]
          in
          semantic_error error_msg
      in
      let _ =
        match Symbol.look vm uid with
        | Some x ->
            let error_msg =
              String.concat " " ["Identifier "; id; " is already in use."]
            in
            semantic_error error_msg
        | None -> ()
      in
      let _ =
        Symbol.enter vm uid
          ( context_flags.current_block
          , Fun (List.map (function w, y, z -> (w, y)) args, urt) )
      in
      let arg_names = List.map (function w, y, z -> z) args in
      let _ =
        if dup_exists arg_names then
          semantic_error
            "All function arguments should be distinct identifiers."
      in
      let uargs = List.map semantic_check_argdecl args in
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
and semantic_check_argblock isdata = isdata

(* Probably nothing to do here *)
and semantic_check_argdecl = function
  | isdata, ut, id ->
      ( semantic_check_argblock isdata
      , semantic_check_unsizedtype ut
      , semantic_check_identifier id )

(* Probably nothing to do here *)
and semantic_check_returntype = function
  | Void -> Void
  | ReturnType ut -> ReturnType (semantic_check_unsizedtype ut)

(* Probably nothing to do here *)
and semantic_check_unsizedtype = function
  | Array ut -> semantic_check_unsizedtype ut
  | Fun (l, rt) ->
      Fun
        ( List.map
            (function
              | ab, ut ->
                  (semantic_check_argblock ab, semantic_check_unsizedtype ut))
            l
        , semantic_check_returntype rt )
  | ut -> ut

and semantic_check_topvardecl = function
  | st, trans, id ->
      let ust = semantic_check_sizedtype st in
      let utrans = semantic_check_transformation trans in
      let uid = semantic_check_identifier id in
      let vt = unsizedtype_of_sizedtype st in
      let _ =
        if is_primitive_name uid then
          let error_msg =
            String.concat " " ["Identifier "; id; " clashes with primitive."]
          in
          semantic_error error_msg
      in
      let _ =
        match Symbol.look vm uid with
        | Some x ->
            let error_msg =
              String.concat " " ["Identifier "; uid; " is already in use."]
            in
            semantic_error error_msg
        | None -> ()
      in
      let _ = Symbol.enter vm id (context_flags.current_block, vt) in
      let _ =
        if
          ( context_flags.current_block = Param
          || context_flags.current_block = TParam )
          && unsizedtype_contains_int vt
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
      | Assignment ((uid, _), Assign, ue), Some Void ->
          TVDeclAss
            {sizedtype= ust; transformation= utrans; identifier= uid; value= ue}
      | _ -> semantic_error "This should never happen. Please file a bug." )

and semantic_check_vardecl vd =
  let st = fst vd in
  let id = snd vd in
  let ust = semantic_check_sizedtype st in
  let uid = semantic_check_identifier id in
  let vt = unsizedtype_of_sizedtype st in
  let _ =
    if is_primitive_name uid then
      let error_msg =
        String.concat " " ["Identifier "; id; " clashes with primitive."]
      in
      semantic_error error_msg
  in
  let _ =
    match Symbol.look vm id with
    | Some x ->
        let error_msg =
          String.concat " " ["Identifier "; id; " is already in use."]
        in
        semantic_error error_msg
    | None -> ()
  in
  let _ = Symbol.enter vm id (context_flags.current_block, vt) in
  (ust, uid)

(* Probably nothing to check here. *)
and semantic_check_compound_vardecl_assign = function
  | {sizedtype= st; identifier= id; value= e} -> (
      let ust, uid = semantic_check_vardecl (st, id) in
      match
        semantic_check_statement (Assignment ((uid, []), Assign, e), None)
      with
      | Assignment ((uid, _), Assign, ue), Some Void ->
          VDeclAss {sizedtype= ust; identifier= uid; value= ue}
      | _ -> semantic_error "This should never happen. Please file a bug." )

(* Probably nothing to do here *)
and semantic_check_topvardecl_or_statement tvds =
  match tvds with
  | TVDecl tvd -> TVDecl (semantic_check_topvardecl tvd)
  | TStmt s -> TStmt (semantic_check_statement s)
  | TVDeclAss tvda -> semantic_check_compound_topvardecl_assign tvda

(* Probably nothing to do here *)
and semantic_check_vardecl_or_statement vds =
  match vds with
  | VDecl vd -> VDecl (semantic_check_vardecl vd)
  | Stmt s -> Stmt (semantic_check_statement s)
  | VDeclAss vda -> semantic_check_compound_vardecl_assign vda

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
      match
        try_get_primitive_return_type "-operator-Conditional"
          [snd ue1; snd ue2; snd ue3]
      with
      | Some rt -> (Conditional (ue1, ue2, ue3), Some rt)
      | _ ->
          semantic_error
            "Ill-typed arguments supplied to Conditional operator." )
  | InfixOp (e1, op, e2) -> (
      let ue1 = semantic_check_expression e1 in
      let uop = semantic_check_infixop op in
      let ue2 = semantic_check_expression e2 in
      let opname = Core_kernel.string_of_sexp (sexp_of_infixop uop) in
      match
        try_get_primitive_return_type ("-operator-" ^ opname) [snd ue1; snd ue2]
      with
      | Some rt -> (InfixOp (ue1, uop, ue2), Some rt)
      | _ ->
          semantic_error
            ("Ill-typed arguments supplied to" ^ opname ^ "operator.") )
  | PrefixOp (op, e) -> (
      let uop = semantic_check_prefixop op in
      let ue = semantic_check_expression e in
      let opname = Core_kernel.string_of_sexp (sexp_of_prefixop uop) in
      match try_get_primitive_return_type ("-operator-" ^ opname) [snd ue] with
      | Some rt -> (PrefixOp (uop, ue), Some rt)
      | _ ->
          semantic_error
            ("Ill-typed arguments supplied to" ^ opname ^ "operator.") )
  | PostfixOp (e, op) -> (
      let ue = semantic_check_expression e in
      let uop = semantic_check_postfixop op in
      let opname = Core_kernel.string_of_sexp (sexp_of_postfixop uop) in
      match try_get_primitive_return_type ("-operator-" ^ opname) [snd ue] with
      | Some rt -> (PostfixOp (ue, uop), Some rt)
      | _ ->
          semantic_error
            ("Ill-typed arguments supplied to" ^ opname ^ "operator.") )
  | Variable id ->
      let uid = semantic_check_identifier id in
      let ort = Symbol.look vm id in
      let _ =
        if ort = None && not (is_primitive_name uid) then
          semantic_error "Identifier not in scope."
      in
      (Variable uid, ort)
  | IntNumeral s -> semantic_error "not implemented"
  | RealNumeral s -> semantic_error "not implemented"
  | FunApp (id, es) -> semantic_error "not implemented"
  | CondFunApp (id, es) -> semantic_error "not implemented"
  | GetLP -> semantic_error "not implemented"
  | GetTarget -> semantic_error "not implemented"
  | ArrayExpr es -> semantic_error "not implemented"
  | RowVectorExpr es -> semantic_error "not implemented"
  | Paren e -> semantic_error "not implemented"
  | Indexed (e, indices) -> semantic_error "not implemented"

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
  | Assignment ((id, lindex), assop, e) ->
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
        | Some Functions -> semantic_error "Cannot assign to function."
        | Some Data -> semantic_error "Cannot assign to data."
        | Some Param -> semantic_error "Cannot assign to parameter."
        | _ -> ()
      in
      let _ =
        if not (check_of_same_type_mod_conv ue ue2) then
          semantic_error "Type mismatch in assignment"
      in
      (Assignment ((uid, ulindex), uassop, ue), Some Void)
  | NRFunApp (id, es) -> (
      let uid = semantic_check_identifier id in
      let ues = List.map semantic_check_expression es in
      let argumenttypes = List.map snd ues in
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
      match try_get_primitive_return_type uid argumenttypes with
      | Some Void -> (NRFunApp (uid, ues), Some Void)
      | Some (ReturnType _) ->
          semantic_error
            "A non-returning function was expected but a returning function \
             was supplied."
      | _ -> (
        match Symbol.look vm uid with
        | Some (_, Fun (argumenttypes, Void)) ->
            (NRFunApp (uid, ues), Some Void)
        | Some (_, Fun (argumenttypes, ReturnType _)) ->
            semantic_error
              "A non-returning function was expected but a returning function \
               was supplied."
        | _ ->
            semantic_error
              "A non-returning function was expected but a ground type value \
               was supplied." ) )
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
      (TargetPE e, Some Void)
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
      (IncrementLogProb e, Some Void)
  | Tilde {arg= e; distribution= id; args= es; truncation= t} ->
      let ue = semantic_check_expression e in
      let uid = semantic_check_identifier id in
      let ues = List.map semantic_check_expression es in
      let ut = semantic_check_truncation t in
      let argumenttypes = snd ue :: List.map snd ues in
      let _ =
        if
          not
            (context_flags.in_lp_fun_def || context_flags.current_block = Model)
        then
          semantic_error
            "Target can only be accessed in the model block or in definitions \
             of functions with the suffix _lp."
      in
      if
        try_get_primitive_return_type (uid ^ "_lpdf") argumenttypes = Some Real
        || try_get_primitive_return_type (uid ^ "_lpmf") argumenttypes
           = Some Real
        || Symbol.look vm uid
           = Some
               ( Functions
               , Fun
                   ( List.map
                       (function
                         | Some x -> x
                         | _ ->
                             semantic_error
                               "This should never happen. Please report a bug.")
                       argumenttypes
                   , ReturnType Real ) )
      then
        ( Tilde {arg= ue; distribution= uid; args= ues; truncation= ut}
        , Some Void )
      else semantic_error "Ill-typed arguments to '~' statement."
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
      (Print ps, Some Void)
  | Reject ps ->
      let ups = List.map semantic_check_printable ps in
      (Reject ps, Some Void)
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
      let _ = context_flags.in_loop <- true in
      let us = semantic_check_statement s in
      let _ = context_flags.in_loop <- false in
      ( For
          { loop_variable= uid
          ; lower_bound= ue1
          ; upper_bound= ue2
          ; loop_body= us }
      , snd us )
  | ForEach (id, e, s) ->
      let ue = semantic_check_expression e in
      let _ =
        match snd ue with
        | Some (_, Int) ->
            semantic_error
              "Foreach loop must be over array, vector, row_vector or matrix"
        | Some (_, Real) ->
            semantic_error
              "Foreach loop must be over array, vector, row_vector or matrix"
        | Some (_, Fun _) ->
            semantic_error
              "Foreach loop must be over array, vector, row_vector or matrix"
        | _ -> ()
      in
      let _ = context_flags.in_loop <- true in
      let us = semantic_check_statement s in
      let _ = context_flags.in_loop <- false in
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
              semantic_error "This should never happen. Please report a bug." )
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
  | _ -> semantic_error "This should never happen. Please file a bug."

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
