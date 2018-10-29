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