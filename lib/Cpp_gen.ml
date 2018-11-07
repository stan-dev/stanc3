open Mir

let rec emit_stantype = function
  | SInt -> ["int"]
  | SReal -> ["double"]
  | SArray(t, size) ->
    let my_type = emit_stantype t in
    begin match size with
      | Some(e) -> my_type @ emit_index e
      | None -> my_type @ ["[]"]
    end
  | SMatrix(_) -> []
  | SRowVector(_) -> []
  | SVector(_) -> []

and emit_index e = ["["] @ emit_expr e @ ["]"]

and emit_cond_op c = [match c with
  | Equals -> "=="
  | NEquals -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  ]


and emit_expr = function
  | Var(s) -> [s]
  | Lit(Str, s) -> ["\""; s; "\""]
  | Lit(_, s) -> [s]
  | FnApp(fname, args) ->
    [fname; "("] @ List.(flatten (map emit_expr args)) @ [")"]
  | Cond(e1, op, e2) ->
    emit_expr e1 @ emit_cond_op op @ emit_expr e2
  | ArrayExpr(es) ->
    ["{"] @ List.(flatten (map emit_expr es)) @ ["}"]
  | Indexed(e, idcs) -> (* totally guessing here*)
    emit_expr e @ List.(flatten (map emit_index idcs))

and emit_vardecl (st, name) = emit_stantype st @ [" "; name]

and emit_assign_op op = [" "; begin match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Modulo -> "%"
  | LDivide -> "\\"
  | EltTimes -> ".*"
  | EltDivide -> "./"
  | Exp -> "^"
  | Or -> "|"
  | And -> "&" end;
   "= "]

and emit_statement s = begin match s with
  | Assignment({assignee; indices; op; rhs}) ->
    [assignee]
    @ List.(flatten (map emit_index indices))
    @ emit_assign_op op
    @ emit_expr rhs
  | NRFunApp(fname, args) ->
    fname :: List.(flatten (map emit_expr args))
  | Break -> ["break"]
  | Continue -> ["continue"]
  | Return(e) -> "return " :: emit_expr e
  | Skip -> [""]
  | IfElse(cond, ifbranch, elsebranch) ->
    "if (" :: emit_expr cond
      @ [") {\n"]
  | While of expr * statement
  | For of
      { loop_variable: string
      ; lower_bound: expr
      ; upper_bound: expr
      ; loop_body: statement }
  | ForEach of string * expr * statement
  | Block of statement list
  | Decl of vardecl * (expr option)
  end @ [";"]

and emit_fndef {returntype; name; arguments; body} =
  let rt = match returntype with
    | None -> ["void"]
    | Some(st) -> emit_stantype st in
  let args = List.(flatten (map emit_vardecl arguments)) in
  rt @ [" "; name] @ args
  @ List.(flatten (map emit_statement body))

and indent level s =
  indent (level - 1) "  " ^ s

let emit_class name super fields methods =
  ["class "; name; " : "; super; " {\nprivate:"]
  @ List.(flatten (map emit_vardecl fields))
  @ List.(flatten (map emit_fndef methods)
