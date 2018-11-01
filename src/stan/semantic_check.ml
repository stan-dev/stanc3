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
*)

(*
To store:
Function sigs from math library 
Variables in scope at each point in program
Type of every variable at point in program
Block of every variable in program 
For code generation, important to remember type of various expressions to know how to instantiate overloaded functions 
*)

(* Other ideas:
Annotate identifiers (function apps) with signature during sem checking. That's all we need for code generation 
Add extra optional field of type sig to every function app. Leave blank during parsing. Fill in during sem checking. Return decorated parse tree at end of recursive algorithm. Build up var map imperatively as you go
Same with for each loop bounds. 
Don't really seem to need function sigs after all for code generation as the names are the same in C++.
Build up hash table (and break down again) with variables in scope (as strings) mapping to type and block, while we traverse the tree.
Implement function signatures as partial functions on types. Also implement operators that way. That lets us quickly compute the type of an expression.
Perhaps use Appel's imperative symbol table?
*)

(* semantic_check : program -> program | recursive function, side effecting on var_map *)
(* var_map (imperative) stores type and block for each variable in scope + flags: in function body, in loop, in lpdf/model, in rng, current block; has operations new, enter, look, beginscope, endscope *)
(* infer_expression_type : expression -> (type * block) option (recursively implemented calling var_map.look) *)
(* use var_map.infer_expression_type to make sure all types are OK for operations we perform and to decorate AST with extra type information as we proceed *)
(* use var_map enter to enter math library functions into var map at start of program *)
(* use var_map enter to enter functions from function block and whenever we encounter a variable *)
(* use begin scope and end scope to deal with block structure *)
(* use new once to initialise var_map *)
(* use var_map flags for checking other constraints *)
(* specialised commonly used definitions like check_int, check_data *)

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

(* A type to keep track of the origin of variables *)
type var_origin = Functions | Data | TData | Param | TParam | Model | GQuant

(* NB DANGER: this file specifies an imperative tree algorithm which
   decorates the AST while operating on two bits of state:
   1) a global symbol table vm
   2) some context flags context_flags, to communicate information down
      the AST   *)
let vm = Symbol.initialize ()

(* TODO: first load whole math library into the *)

type context_flags_record =
  { mutable current_block: var_origin
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

let vartype_contains_int vt =
  match vt with
  | rt -> (
    match rt with ReturnType ut -> unsizedtype_contains_int ut | _ -> false )
  | _ -> false

let rec unsizedtype_of_sizedtype = function
  | SInt -> Int
  | SReal -> Real
  | SVector e -> Vector
  | SRowVector e -> RowVector
  | SMatrix (e1, e2) -> Matrix
  | SArray (st, e) -> Array (unsizedtype_of_sizedtype st)

let vartype_of_sizedtype st = ReturnType (unsizedtype_of_sizedtype st)

let look_block id = Core_kernel.Option.map (Symbol.look vm id) snd

(* TODO: generalize this to arbitrary expressions? *)
(* TODO: should throw if Funapp of non-returning function *)
let infer_expression_type e = Some Int

(* Now, derive optional return type of statement as we semantically check it: fill in.
If two brances of else return different, then throw.
Return type of list is the first return type encountered.
At return here, check that it matches the specified type. *)
(* TODO: should throw if NRFunapp of returning function *)
let infer_statement_return_type s = Some Void

(* TODO!!!! Implement this *)

let check_of_int_type e =
  match (snd e) with Some Int -> true | _ -> false

let check_of_real_type e =
  match (snd e) with Some Real -> true | _ -> false

let check_of_int_or_real_type e =
  match (snd e) with
  | Some Int -> true
  | Some Real -> true
  | _ -> false

(* TODO!!! *)
let check_compatible_indices e lindex = true

let check_of_same_type_mod_conv e1 e2 =
  match (snd e1) with
  | Some t1 -> (
    match (snd e2) with
    | Some t2 -> t1 = t2 || (t1 = Real && t1 = Int) || (t1 = Int && t1 = Real)
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
          , ReturnType
              (Fun (List.map (function w, y, z -> (w, y)) args, urt)) )
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
  | Fun (l, rt) -> Fun (List.map (function | (ab, ut) ->
  (semantic_check_argblock ab, semantic_check_unsizedtype ut)) l, semantic_check_returntype rt)
  | ut -> ut

(* OK up to here *)

(* Probably nothing to do here *)
and semantic_check_topvardecl tvd =
  match tvd with st, trans, id ->
    let uid = semantic_check_identifier id in
    let ust = semantic_check_sizedtype st in
    let utrans = semantic_check_transformation trans in
    let vt = vartype_of_sizedtype st in
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
    let _ =
      if
        ( context_flags.current_block = Param
        || context_flags.current_block = TParam )
        && vartype_contains_int vt
      then semantic_error "(Transformed) Parameters cannot be integers."
    in
    (ust, utrans, uid)

and semantic_check_vardecl vd =
  let id = snd vd in
  let st = fst vd in
  let uid = semantic_check_identifier id in
  let ust = semantic_check_sizedtype st in
  let vt = vartype_of_sizedtype st in
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

and semantic_check_topvardecl_or_statement tvds =
  match tvds with
  | TVDecl tvd -> TVDecl (semantic_check_topvardecl tvd)
  | TStmt s -> TStmt (semantic_check_statement s)
  | TVDeclAss {sizedtype= st; transformation= trans; identifier= id; value= e}
    ->
      let ust, utrans, uid = semantic_check_topvardecl (st, trans, id) in
      let uid, uassop, ue = semantic_check_assign (uid, Assign, e) in
      TVDeclAss
        {sizedtype= ust; transformation= utrans; identifier= uid; value= ue}

(* Probably nothing to do here *)
and semantic_check_vardecl_or_statement vds =
  match vds with
  | VDecl vd -> VDecl (semantic_check_vardecl vd)
  | Stmt s -> Stmt (semantic_check_statement s)
  | VDeclAss {sizedtype= st; identifier= id; value= e} ->
      let ust, uid = semantic_check_vardecl (st, id) in
      let uid, uassop, ue = semantic_check_assign (uid, Assign, e) in
      VDeclAss {sizedtype= ust; identifier= uid; value= ue}

(* TODO: here, we only check_of_int_type after checking semantic check expression.
We could also make these check of int types include the semantic check expression.
Probably, that's the way to do it.*)
(* Probably nothing to do here *)
and semantic_check_sizedtype = function
  | SInt -> SInt
  | SReal -> SReal
  | SVector e ->
      if check_of_int_type e then SVector (semantic_check_expression e)
      else semantic_error "Vector sizes should be of type int."
  | SRowVector e ->
      if check_of_int_type e then SRowVector (semantic_check_expression e)
      else semantic_error "Row vector sizes should be of type int."
  | SMatrix (e1, e2) ->
      if check_of_int_type e1 && check_of_int_type e2 then
        SMatrix (semantic_check_expression e1, semantic_check_expression e2)
      else semantic_error "Matrix sizes should be of type int."
  | SArray (st, e) ->
      if check_of_int_type e then
        SArray (semantic_check_sizedtype st, semantic_check_expression e)
      else semantic_error "Array sizes should be of type int."

and semantic_check_transformation = function
  | Identity -> Identity
  | Lower e ->
      if check_of_int_or_real_type e then Lower (semantic_check_expression e)
      else semantic_error "Lower bound should be of int or real type."
  | Upper e ->
      if check_of_int_or_real_type e then Upper (semantic_check_expression e)
      else semantic_error "Upper bound should be of int or real type."
  | LowerUpper (e1, e2) ->
      if check_of_int_or_real_type e1 && check_of_int_or_real_type e2 then
        LowerUpper (semantic_check_expression e1, semantic_check_expression e2)
      else
        semantic_error "Lower and upper bound should be of int or real type."
  | LocationScale (e1, e2) ->
      if check_of_int_or_real_type e1 && check_of_int_or_real_type e2 then
        LocationScale
          (semantic_check_expression e1, semantic_check_expression e2)
      else semantic_error "Location and scale should be of int or real type."
  | Ordered -> Ordered
  | PositiveOrdered -> PositiveOrdered
  | Simplex -> Simplex
  | UnitVector -> UnitVector
  | CholeskyCorr -> CholeskyCorr
  | CholeskyCov -> CholeskyCov
  | Correlation -> Correlation
  | Covariance -> Covariance

and semantic_check_expression e = (fst e, infer_expression_type e)

(*(function
  | Conditional (e1, e2, e3) ->
      if check_of_int_type e1 then
        if check_of_same_type_mod_conv e2 e3 then
          Conditional
            ( semantic_check_expression e1
            , semantic_check_expression e2
            , semantic_check_expression e3 )
        else
          semantic_error
            "Both branches of a conditional operator need to have the same \
             type."
      else semantic_error "Condition in conditional should be of type int."
  | _ -> GetTarget *)

(* TODO!!! *)
and semantic_check_infixop i = i

(* Probably nothing to do here *)
and semantic_check_prefixop p = p

(* Probably nothing to do here *)
and semantic_check_postfixop p = p

(* Probably nothing to do here *)
and semantic_check_printable = function
  | PString s -> PString s
  | PExpr e -> PExpr (semantic_check_expression e)

(* TODO: do we even want to check this? *)
and (* TODO: get rid of some of this error checking *)
    semantic_check_statement s =
  (* function
  | Assignment (lhs, assop, e) ->
      if
        check_of_same_type_mod_conv
          (Indexed (Variable (fst lhs), snd lhs))
          e
        (* TODO: This is probably too simplified. Go over all compound assignment operators to check their signature. *)
      then
        if look_block (fst lhs) = look_block "1currentblock" then
          if look_block (fst lhs) = Some Data then
            if look_block (fst lhs) = Some Param then
              Assignment
                ( semantic_check_lhs lhs
                , semantic_check_assignmentoperator assop
                , semantic_check_expression e )
            else semantic_error "Parameters cannot be assigned to."
          else semantic_error "Data variables cannot be assigned to."
        else
          semantic_error
            "Variables from previous blocks cannot be assigned to."
      else semantic_error "Assignment is ill-typed." 
  | _ ->*)
  (fst s, infer_statement_return_type s)

and semantic_check_assign ass_s = ass_s

(* TODO; also for other statements *)

(* TODO!!! Probably should separate out these clauses; same for types of expressions. *)
and semantic_check_truncation = function
  | NoTruncate -> NoTruncate
  | TruncateUpFrom e ->
      if check_of_int_or_real_type e then
        TruncateUpFrom (semantic_check_expression e)
      else semantic_error "Truncation bound should be of type int or real."
  | TruncateDownFrom e ->
      if check_of_int_or_real_type e then
        TruncateDownFrom (semantic_check_expression e)
      else semantic_error "Truncation bound should be of type int or real."
  | TruncateBetween (e1, e2) ->
      if check_of_int_or_real_type e1 && check_of_int_or_real_type e2 then
        TruncateBetween
          (semantic_check_expression e1, semantic_check_expression e2)
      else semantic_error "Truncation bound should be of type int or real."

and semantic_check_lhs lhs =
  let id = fst lhs in
  let lindex = snd lhs in
  let uid = semantic_check_identifier id in
  let ulindex = List.map semantic_check_index lindex in
  let _ = check_compatible_indices (Variable uid) ulindex in
  (uid, ulindex)

and semantic_check_index = function
  | All -> All
  | Single e ->
      if check_of_int_type e then Single (semantic_check_expression e)
      else semantic_error "Index should be of type int."
  | Upfrom e ->
      if check_of_int_type e then Upfrom (semantic_check_expression e)
      else semantic_error "Index should be of type int."
  | Downfrom e ->
      if check_of_int_type e then Downfrom (semantic_check_expression e)
      else semantic_error "Index should be of type int."
  | Between (e1, e2) ->
      if check_of_int_type e1 && check_of_int_type e2 then
        Between (semantic_check_expression e1, semantic_check_expression e2)
      else semantic_error "Index should be of type int."

and semantic_check_assignmentoperator op = op

(* Probably nothing to do here *)
