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
Every trace through function body contains return statement
In case of void function, no return statements anywhere
All function arguments are distinct
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
(* infer_type : expression -> (type * block) option (recursively implemented calling var_map.look) *)
(* use var_map.infer_type to make sure all types are OK for operations we perform and to decorate AST with extra type information as we proceed *)
(* use var_map enter to enter math library functions into var map at start of program *)
(* use var_map enter to enter functions from function block and whenever we encounter a variable *)
(* use begin scope and end scope to deal with block structure *)
(* use new once to initialise var_map *)
(* use var_map flags for checking other constraints *)
(* specialised commonly used definitions like check_int, check_data *)


(* We will use the following non-identifier (protected) strings in the var map as flags for properties of our position in the program:
1currentblock           - holds value of current block we are in
1for                    - in for/foreach loop
1while                  - in while loop
1if                     - in if condition
1then                   - in then body
1else                   - in else body
1fundef                 - in fundef
1lp                     - in lp fun def
1rng                    - in rng fun def
1allreturn              - have seen return statement in every path so far
1noreturn               - have seen return statement in no path so far
1voidfun                - in non-returning function
 *)

open Symbol_table
open Syntax

type var_origin =
  | Functions
  | Data
  | TData
  | Param
  | TParam
  | Model
  | GQuant
  | Meta

type var_type =
  | Basic of returntype
  | Fun of (var_type list) * var_type
  | True (* for use with Meta *)
  | False (* for use with Meta *)

let var_type_of_argdecl ad = match ad with DataArg (ut, id) -> Basic (ReturnType ut) | Arg (ut, id) -> Basic (ReturnType ut)

let identifier_of_argdecl ad = match ad with DataArg (ut, id) -> id | Arg (ut, id) -> id

(** A semantic error reported by the toplevel *)
let semantic_error ?loc msg = Zoo.error ~kind:"Semantic error" ?loc (Scanf.format_from_string msg "") (* TODO: this is not very pretty *)

let rec remove_dups lst= match lst with
                             | [] -> []
                             | h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))

let duplicate_arg_names args = let lst = (List.map identifier_of_argdecl args) in
                               if List.length (remove_dups lst) < List.length lst then true else false

let rec unsizedtype_contains_int ut = match ut with Int -> true | Array ut -> unsizedtype_contains_int ut | _ -> false

let vartype_contains_int vt = match vt with Basic rt -> (match rt with ReturnType ut -> unsizedtype_contains_int ut | _ -> false)
                                          | _ -> false

let rec unsizedtype_of_sizedtype = function
                                      SInt -> Int
                                    | SReal -> Real
                                    | SVector e -> Vector
                                    | SRowVector e -> RowVector
                                    | SMatrix (e1, e2) -> Matrix
                                    | SArray (st, e) -> Array (unsizedtype_of_sizedtype st)

let vartype_of_sizedtype st = Basic (ReturnType (unsizedtype_of_sizedtype st))

let check_of_int_type vm e = true (* TODO *)

(* TODO: insert positions into semantic errors! *)

(* TODO: return decorated AST instead of plain one *)
                                        
let rec semantic_check_program vm p = match p with Program (bf, bd, btd, bp, btp, bm, bgq) -> (* TODO: first load whole math library into the vm *)
                                      let _ = Symbol.enter vm "1currentblock" (Functions, True) in
                                      let ubf = semantic_check_functionblock vm bf in
                                      let _ = Symbol.enter vm "1currentblock" (Data, True) in
                                      let ubd = semantic_check_datablock vm bd in
                                      let _ = Symbol.enter vm "1currentblock" (TData, True) in
                                      let ubtd = semantic_check_transformeddatablock vm btd in
                                      let _ = Symbol.enter vm "1currentblock" (Param, True) in
                                      let ubp = semantic_check_parametersblock vm bp in
                                      let _ = Symbol.enter vm "1currentblock" (TParam, True) in
                                      let ubtp = semantic_check_transformedparametersblock vm btp in
                                      let _ = Symbol.enter vm "1currentblock" (Model, True) in
                                      let _ = Symbol.begin_scope vm in
                                      let ubm = semantic_check_modelblock vm bm in
                                      let _ = Symbol.end_scope vm in
                                      let _ = Symbol.enter vm "1currentblock" (GQuant, True) in
                                      let ubgq = semantic_check_generatedquantitiesblock vm bgq in
                                      Program (ubf, ubd, ubtd, ubp, ubtp, ubm, ubgq)
                                      

and

semantic_check_functionblock vm bf = match bf with FunBlock lfd -> FunBlock (List.map (semantic_check_fundef vm) lfd)
                                                 | _ -> bf

and

semantic_check_datablock vm bd = match bd with DataBlock ltvd -> DataBlock (List.map (semantic_check_topvardecl vm) ltvd)
                                             | _ -> bd

and

semantic_check_transformeddatablock vm btd = match btd with TDataBlock ltvds -> TDataBlock (List.map (semantic_check_topvardecl_or_statement vm) ltvds)
                                                          | _ -> btd

and

semantic_check_parametersblock vm bp = match bp with ParamBlock ltvd -> ParamBlock (List.map (semantic_check_topvardecl vm) ltvd)
                                                   | _ -> bp

and

semantic_check_transformedparametersblock vm btp = match btp with TParamBlock ltvds -> TParamBlock (List.map (semantic_check_topvardecl_or_statement vm) ltvds)
                                                                | _ -> btp

and

semantic_check_modelblock vm bm = match bm with ModelBlock lvds -> ModelBlock (List.map (semantic_check_vardecl_or_statement vm) lvds)
                                              | _ -> bm

and

semantic_check_generatedquantitiesblock vm bgq = match bgq with GQBlock ltvds -> GQBlock (List.map (semantic_check_topvardecl_or_statement vm) ltvds)
                                                              | _ -> bgq

and

semantic_check_fundef vm fd = match fd with FunDef (rt, id, args, b) ->
                              let urt = semantic_check_returntype vm rt in
                              let uid = semantic_check_identifier vm id in
                              match Symbol.look vm id with Some x -> let error_msg = String.concat " " ["Identifier "; id; " is already in use."] in semantic_error error_msg
                                                         | None ->
                              let _ = Symbol.enter vm id (Functions, Fun (List.map var_type_of_argdecl args, Basic rt)) in
                              let _ = if duplicate_arg_names args then semantic_error "All function arguments should be distinct identifiers." in 
                              let uargs = List.map (semantic_check_argdecl vm) args in
                              let _ = Symbol.enter vm "1noreturn" (Meta, True) in
                              let _ = Symbol.enter vm "1allreturn" (Meta, True) in
                              let _ = Symbol.enter vm "1fundef" (Meta, True) in
                              let _ = if Filename.check_suffix id "_rng" then Symbol.enter vm "1rng" (Meta, True) in
                              let _ = if Filename.check_suffix id "_lp" then Symbol.enter vm "1rng" (Meta, True) in
                              let ub = semantic_check_statement vm b in
                              let _ = Symbol.enter vm "1fundef" (Meta, False) in
                              let _ = Symbol.enter vm "1rng" (Meta, False) in
                              let _ = Symbol.enter vm "1lp" (Meta, False) in
                              let _ = match rt with Void -> (match Symbol.look vm "1noreturn" with Some (Meta, False) -> semantic_error "Void function bodies cannot contain return statements." | _ -> ())
                                                   | _   -> (match Symbol.look vm "1allreturn" with Some (Meta, False) -> semantic_error "Non-void function bodies must contain a return statement in every branch." | _ -> ()) in
                              FunDef (urt, uid, uargs, ub)
and

semantic_check_identifier vm id = id (* TODO: This could be one place where we check for reserved variable names. Though it would be nicer to just do it in the lexer. *)

and

semantic_check_real vm r = r (* Probably nothing to do here *)

and

semantic_check_size vm s = s (* Probably nothing to do here *)

and

semantic_check_argdecl vm ad = ad (* Probably nothing to do here *)

and

semantic_check_returntype vm rt = rt (* Probably nothing to do here *)

and

semantic_check_unsizedtype vm ut = ut (* Probably nothing to do here *)

and

semantic_check_topvardecl vm tvd = let id = snd tvd in
                                   let tvt = fst tvd in
                                   let uid = semantic_check_identifier vm id in
                                   let utvt = semantic_check_topvartype vm tvt in
                                   let vt = vartype_of_sizedtype (fst tvt) in
                                   let currentblock = match Symbol.look vm "1currentblock" with Some p -> (fst p) | _ -> Meta in
                                   let _ = match Symbol.look vm id with Some x -> (let error_msg = String.concat " " ["Identifier "; id; " is already in use."] in semantic_error error_msg)
                                                                      | None -> () in
                                   let _ = Symbol.enter vm id (currentblock, vt) in
                                   let _ = if ((currentblock = Param) || (currentblock = TParam)) && (vartype_contains_int vt) then semantic_error "(Transformed) Parameters cannot be integers." in
                                   (utvt, uid)

and

semantic_check_vardecl vm vd = let id = snd vd in
                               let st = fst vd in
                               let uid = semantic_check_identifier vm id in
                               let ust = semantic_check_sizedtype vm st in
                               let vt = vartype_of_sizedtype st in
                               let currentblock = match Symbol.look vm "1currentblock" with Some p -> (fst p) | _ -> Meta in
                               let _ = match Symbol.look vm id with Some x -> (let error_msg = String.concat " " ["Identifier "; id; " is already in use."] in semantic_error error_msg)
                                                                  | None -> () in
                               let _ = Symbol.enter vm id (currentblock, vt) in
                               (ust, uid)

and

semantic_check_topvardecl_or_statement vm tvds = match tvds with TVDecl tvd -> TVDecl (semantic_check_topvardecl vm tvd)
                                                               | TStmt s -> TStmt (semantic_check_statement vm s)

and

semantic_check_vardecl_or_statement vm vds = match vds with VDecl vd -> VDecl (semantic_check_vardecl vm vd)
                                                          | Stmt s -> Stmt (semantic_check_statement vm s)

and

semantic_check_topvartype vm tvt = let st = fst tvt in
                                   let trans = snd tvt in
                                   let ust = semantic_check_sizedtype vm st in
                                   let utrans = semantic_check_transformation vm trans in
                                   (ust, utrans)

and

semantic_check_sizedtype vm = function
                                SInt -> SInt
                              | SReal -> SReal
                              | SVector e -> if check_of_int_type vm e then SVector (semantic_check_expression vm e) else semantic_error "Vector sizes should be of type int."
                              | SRowVector e -> if check_of_int_type vm e then SRowVector (semantic_check_expression vm e) else semantic_error "Row vector sizes should be of type int."
                              | SMatrix (e1, e2) -> if (check_of_int_type vm e1) && (check_of_int_type vm e2) then SMatrix (semantic_check_expression vm e1, semantic_check_expression vm e2) else semantic_error "Matrix sizes should be of type int."
                              | SArray (st, e) -> if check_of_int_type vm e then SArray (semantic_check_sizedtype vm st, semantic_check_expression vm e) else semantic_error "Array sizes should be of type int."

and

semantic_check_transformation vm t = t

and

semantic_check_expression vm e = e

and

semantic_check_infixop vm i = i

and

semantic_check_prefixop vm p = p

and

semantic_check_postfixop vm p = p

and

semantic_check_printable vm p = p

and

semantic_check_statement vm s = s

and

semantic_check_truncation vm t = t

and

semantic_check_lhs vm l = l

and

semantic_check_index vm i = i