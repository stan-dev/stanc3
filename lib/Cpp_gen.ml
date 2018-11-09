open Mir
open Format

let emit_str ppf s = fprintf ppf "%s" s

let emit_option emitter ppf = function
  | Some(x) -> fprintf ppf "%a" emitter x
  | None -> ()

let rec emit_list emitter sep ppf coll = match coll with
  | [] -> ()
  | hd::[] -> fprintf ppf "%a" emitter hd
  | hd::tl -> fprintf ppf "%a%s%a" emitter hd sep (emit_list emitter sep) tl

let%expect_test "emitlist" =
  fprintf str_formatter "%a" (emit_list emit_str "; ") ["hi"; "there"];
  flush_str_formatter () |> print_endline;
  [%expect {| hi; there |}]

(* XXX Make the above test style much cleaner! *)

let emit_cond_op ppf c = emit_str ppf begin match c with
    | Equals -> "=="
    | NEquals -> "!="
    | Less -> "<"
    | Leq -> "<="
    | Greater -> ">"
    | Geq -> ">="
  end

let emit_assign_op ppf op = fprintf ppf " %s= " begin match op with
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
    | And -> "&"
  end

let rec emit_stantype ppf = function
  | SInt -> emit_str ppf "int"
  | SReal -> emit_str ppf "double"
  | SArray(t, size) ->
    emit_stantype ppf t;
    begin match size with
      | Some(e) -> emit_index ppf e
      | None -> emit_str ppf "[]"
    end
  | SMatrix(_) -> ()
  | SRowVector(_) -> ()
  | SVector(_) -> ()

and emit_index ppf e = fprintf ppf "[%a]" emit_expr e

and emit_expr ppf s = match s with
  | Var(s) -> emit_str ppf s
  | Lit(Str, s) -> fprintf ppf "\"%s\"" s
  | Lit(_, s) -> emit_str ppf s
  | FnApp(fname, args) ->
    fprintf ppf "%s(%a)" fname (emit_list emit_expr ", ") args
  | Cond(e1, op, e2) ->
    emit_expr ppf e1;
    emit_cond_op ppf op;
    emit_expr ppf e2
  | ArrayExpr(es) ->
    fprintf ppf "{%a}" (emit_list emit_expr ", ") es
  | Indexed(e, idcs) -> (* totally guessing here*)
    fprintf ppf "%a%a" emit_expr e (emit_list emit_index ", ") idcs

let%expect_test "expr" =
  FnApp("sassy", [ArrayExpr([Lit(Int, "4"); Lit(Int, "2")]);
                 Lit(Real, "27.0")])
  |> emit_expr str_formatter;
  flush_str_formatter () |> print_endline;
  [%expect {| sassy({4, 2}, 27.0) |}]


let rec emit_statement ppf s = match s with
  | Assignment({assignee; indices; op; rhs}) ->
    fprintf ppf "%s%a%a%a" assignee (emit_list emit_index ", ") indices
      emit_assign_op op emit_expr rhs
  | NRFunApp(fname, args) ->
    fprintf ppf "%s(%a)" fname (emit_list emit_expr ", ") args
  | Break -> emit_str ppf "break"
  | Continue -> emit_str ppf "continue"
  | Return(e) -> fprintf ppf "%s%a" "return " emit_expr e
  | Skip -> ()
  | IfElse(cond, ifbranch, elsebranch) ->
    let emit_else ppf x = fprintf ppf " else {\n %a\n}" emit_statement x in
    fprintf ppf "if (%a){\n %a\n}%a\n" emit_expr cond emit_statement ifbranch
      (emit_option emit_else) elsebranch
  | While(cond, body) ->
    fprintf ppf "while (%a) {\n  %a\n}\n" emit_expr cond emit_statement body
  | For({init; cond; step; body}) ->
    fprintf ppf "for (%a; %a; %a) {\n  %a\n}\n" emit_statement init
      emit_expr cond emit_statement step emit_statement body
  | Block(s) ->
    emit_list emit_statement ";\n" ppf s
  | Decl((st, ident), rhs) ->
    let emit_assignment ppf rhs = fprintf ppf " = %a" emit_expr rhs in
    fprintf ppf "%a %s%a" emit_stantype st ident (emit_option emit_assignment) rhs

let%expect_test "decl" =
  Decl((SInt, "i"), Some(Lit(Int, "0"))) |> emit_statement str_formatter;
  flush_str_formatter () |> print_endline;
  [%expect {| int i = 0 |}]

let%expect_test "statement" =
  For({init = Decl((SInt, "i"), Some(Lit(Int, "0")));
       cond = Cond(Var "i", Geq, Lit(Int, "10"));
       step = Assignment({assignee = "i"; op = Plus; rhs = Lit(Int, "1");
                          indices = []});
       body = NRFunApp("print", [Var "i"])})
  |> emit_statement str_formatter;
  flush_str_formatter () |> print_endline;
  [%expect {|
    for (int i = 0; i>=10; i += 1) {
      print(i)
    } |}]

let emit_vardecl ppf (st, name) = fprintf ppf "%a %s" emit_stantype st name

let emit_fndef ppf {returntype; name; arguments; body} =
  match returntype with
    | None -> emit_str ppf "void"
    | Some(st) -> emit_stantype ppf st;
      fprintf ppf " %s(%a) {\n  %a\n}" name (emit_list emit_vardecl ", ") arguments
        emit_statement body

let emit_class ppf name super fields methods =
  fprintf ppf "class %s : %s {\n private:\n %a\n public:\n %a}" name super
    (emit_list emit_vardecl ";\n") fields
    (emit_list emit_fndef "\n") methods

(*
let%expect_test "class" =
  emit_class str_formatter "bernoulli_model" "log_prob"
    [(Real, "x"); (SMatrix "y")]
*)
