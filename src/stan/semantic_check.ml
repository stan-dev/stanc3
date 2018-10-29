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
Check that function args and loop identifiers are not modified in function. (passed by cost ref) 
No returns outside of function definitions?
Rng functions cannot be used in Tp or Model and only in funciton defs with the right suffix
Print/reject expressions cannot be of type void.
Break and continue only occur in loops.
Array expressions must be of uniform type. (Or mix of int and real)
Typing of ~ and target +=
Also check whether function arguments meet data requirement.
Maybe should also infer bounds for every indexing that happens so we know what code to generate for bound checking?
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


(* idea!! : use data keywords in var map to store that we are in a data block;
similarly for other blocks;
and language constructs;
may need to add remove function to var map (as it's not obvious we can always use scope to exit blocks again like that) *)


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

type var_type =
  | Void
  | Int
  | Real
  | Vector
  | RowVector
  | Matrix
  | Array of var_type
  | Fun of (var_type list) * var_type

                                                    
let rec semantic_check_program vm p = match p with Program (bf, bd, btd, bp, btp, bm, bgq) ->
                                      let ubf = semantic_check_functionblock vm bf in
                                      let ubd = semantic_check_datablock vm bd in
                                      let ubtd = semantic_check_transformeddatablock vm btd in
                                      let ubp = semantic_check_parametersblock vm bp in
                                      let ubtp = semantic_check_transformedparametersblock vm btp in
                                      let _ = Symbol.begin_scope vm in
                                      let ubm = semantic_check_modelblock vm bm in
                                      let _ = Symbol.end_scope vm in
                                      let ubgq = semantic_check_generatedquantitiesblock vm bgq in
                                      Program (ubf, ubd, ubtd, ubp, ubtp, ubm, ubgq)
                                      

and

semantic_check_functionblock vm bf = bf

and

semantic_check_datablock vm bd = bd

and

semantic_check_transformeddatablock vm btd = btd

and

semantic_check_parametersblock vm bp = bp

and

semantic_check_transformedparametersblock vm btp = btp

and

semantic_check_modelblock vm bm = bm

and

semantic_check_generatedquantitiesblock vm bgq = bgq

and

semantic_check_fundef vm fd = fd

and

semantic_check_identifier vm id = id

and

semantic_check_real vm r = r

and

semantic_check_size vm s = s

and

semantic_check_argdecl vm ad = ad

and

semantic_check_returntype vm rt = rt

and

semantic_check_unsizedtype vm ut = ut

and

semantic_check_topvardecl vm tvd = tvd

and

semantic_check_vardecl vm vd = vd

and

semantic_check_topvardecl_or_statement vm tvds = tvds

and

semantic_check_vardecl_or_statement vm vds = vds

and

semantic_check_topvartype vm tvt = tvt

and

semantic_check_sizedtyped vm st = st

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