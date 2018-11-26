open Base_prog_tree
open Cpp_gen_tree
open Format

let comma ppf () = fprintf ppf ", "
let semi_new ppf () = fprintf ppf ";@ "
let newline ppf () = fprintf ppf "@ "
let emit_str ppf s = fprintf ppf "%s" s

let emit_option ?default:(d="") emitter ppf opt = match opt with
  | Some(x) -> fprintf ppf "%a" emitter x
  | None -> emit_str ppf d

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

let rec emit_stantype ad ppf = function
  | SInt | SReal | SSize_t | SString -> emit_str ppf ad
  | SArray(t) -> fprintf ppf "std::vector<%a>" (emit_stantype ad) t
  | SMatrix -> fprintf ppf "Eigen::Matrix<%s, -1, -1>" ad
  | SRowVector -> fprintf ppf "Eigen::Matrix<%s, 1, -1>" ad
  | SVector -> fprintf ppf "Eigen::Matrix<%s, -1, 1>" ad
  | SOther(s) -> fprintf ppf "%s" s

and emit_index ppf e = fprintf ppf "[%a]" emit_expr e

and emit_expr ppf s = match s with
  | Var(s) -> emit_str ppf s
  | Lit(Str, s) -> fprintf ppf "\"%s\"" s
  | Lit(_, s) -> emit_str ppf s
  | FnApp(fname, args) ->
    fprintf ppf "%s(%a)" fname (pp_print_list ~pp_sep:comma emit_expr) args
  | Cond(e1, op, e2) ->
    emit_expr ppf e1;
    emit_cond_op ppf op;
    emit_expr ppf e2
  | ArrayExpr(es) ->
    fprintf ppf "{%a}" (pp_print_list ~pp_sep:comma emit_expr) es
  | Indexed(e, idcs) -> (* totally guessing here*)
    fprintf ppf "%a%a" emit_expr e (pp_print_list ~pp_sep:comma emit_index) idcs

let%expect_test "expr" =
  FnApp("sassy", [ArrayExpr([Lit(Int, "4"); Lit(Int, "2")]);
                 Lit(Real, "27.0")])
  |> emit_expr str_formatter;
  flush_str_formatter () |> print_endline;
  [%expect {| sassy({4, 2}, 27.0) |}]

let emit_vanilla_stantype ppf st =
  let rec ad_str = function
    | SInt -> "int"
    | SSize_t -> "size_t"
    | SString -> "std::string"
    | SArray(t) -> ad_str t
    | _ -> "double"
  in
  emit_stantype (ad_str st) ppf st

let rec emit_statement ppf s = match s with
  | Assignment({assignee; indices; op; rhs}) ->
    fprintf ppf "%s%a%a%a" assignee (pp_print_list ~pp_sep:comma emit_index) indices
      emit_assign_op op emit_expr rhs
  | NRFnApp(fname, args) ->
    fprintf ppf "%s(%a)" fname (pp_print_list ~pp_sep:comma emit_expr) args
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
  | Block(s) -> pp_print_list ~pp_sep:semi_new emit_statement ppf s
  | Decl((st, ident), rhs) ->
    let emit_assignment ppf rhs = fprintf ppf " = %a" emit_expr rhs in
    fprintf ppf "%a %s%a" emit_vanilla_stantype st ident
      (emit_option emit_assignment) rhs

let%expect_test "decl" =
  Decl((SInt, "i"), Some(Lit(Int, "0"))) |> emit_statement str_formatter;
  flush_str_formatter () |> print_endline;
  [%expect {| int i = 0 |}]

let%expect_test "statement" =
  For({init = Decl((SInt, "i"), Some(Lit(Int, "0")));
       cond = Cond(Var "i", Geq, Lit(Int, "10"));
       step = Assignment({assignee = "i"; op = Plus; rhs = Lit(Int, "1");
                          indices = []});
       body = NRFnApp("print", [Var "i"])})
  |> emit_statement str_formatter;
  flush_str_formatter () |> print_endline;
  [%expect {|
    for (int i = 0; i>=10; i += 1) {
      print(i)
    } |}]

let emit_ad_stantype ppf (ad, st) = match ad with
  | AVar -> emit_stantype "T__" ppf st
  | AData -> emit_vanilla_stantype ppf st

(* XXX this pattern below is annoying... *)
let emit_vardecl ppf (st, name) = fprintf ppf "%a %s" emit_vanilla_stantype st name

let emit_mut ppf (ad, mut, st) =
  fprintf ppf "%s%a%s"
    (match mut with | Immutable -> "const " | _ -> "")
    emit_ad_stantype (ad, st)
    (match mut with | Immutable | Mutable -> "&" | _ -> "")

let emit_ad_vardecl ppf (ad, mut, st, name) = fprintf ppf "%a %s"
    emit_mut (ad, mut, st) name

let emit_templates ppf templates = if List.length templates > 0 then
    let emit_template ppf t = fprintf ppf "typename %s" t in
    fprintf ppf "template <%a>@ "
      (pp_print_list ~pp_sep:comma emit_template) templates

let emit_fndef ppf {returntype; name; arguments; body; templates} =
  let templated =
    List.exists (fun (ad, _, _, _) -> match ad with | AData -> false | AVar -> true)
      arguments in
  let templates = if templated then "T__" :: templates else templates in
  fprintf ppf "@[<v>%a%a %s(%a) {@ @[<v 2>  %a;@]@ }@]"
    emit_templates templates
    (emit_option ~default:"void" emit_ad_stantype) returntype
    name
    (pp_print_list ~pp_sep:comma emit_ad_vardecl) arguments
    emit_statement body

let emit_ctor classname ppf (args, body) =
  fprintf ppf "%s(%a) {@ @[<v 2>%a@]}" classname
    (pp_print_list ~pp_sep:comma emit_vardecl) args
    emit_statement body

let emit_class ppf {classname; super; fields; methods; ctor} =
  fprintf ppf "@[<v 1>class %s : %s {@ private:@ @[<v 1> %a;@]@ @ public:@ @[<v 1> %a@ %a@]@ }@."
    classname super
    (pp_print_list ~pp_sep:semi_new emit_vardecl) fields
    (pp_print_list ~pp_sep:newline emit_fndef) methods
    (emit_ctor classname) ctor

let%expect_test "class" =
  emit_class str_formatter
    {classname = "bernoulli_model"; super="prob_grad";
     fields=[(SMatrix, "x"); (SVector, "y")];
     ctor = [], Skip;
     methods=[{returntype = Some (AVar, SReal); name = "log_prob";
               arguments = [AVar, Immutable, SVector, "params"]; templates = [];
               body = Block [
                   Assignment ({assignee = "target"; op = Plus; indices = [];
                                rhs = FnApp("normal",
                                            [FnApp("multiply", [Var "x"; Var "params"]);
                                             Lit(Real, "1.0")])})]};
              {returntype = None; name = "get_param_names"; arguments = [AData, Mutable, SString, "names"]; templates = []; body = Block [Assignment ({assignee = "target"; op = Plus; indices = []; rhs = FnApp("normal", [FnApp("multiply", [Var "x"; Var "params"]); Lit(Real, "1.0")])})]}]} ;
  flush_str_formatter () |> print_endline;
  [%expect {|
    class bernoulli_model : prob_grad {
     private:
      Eigen::Matrix<double, -1, -1> x;
      Eigen::Matrix<double, -1, 1> y;

     public:
      template <typename T__>
      T__ log_prob(const Eigen::Matrix<T__, -1, 1>& params) {
        target += normal(multiply(x, params), 1.0);
      }
      void get_param_names(std::string& names) {
        target += normal(multiply(x, params), 1.0);
      }
      bernoulli_model() {
      }
     } |}]

let emit_using ppf u = fprintf ppf "using %s;" u
let emit_include ppf i = fprintf ppf "include <%s>" i

let rec emit_namespaced cppclass functions usings ppf = function
  | [] -> fprintf ppf "%a@ %a@ %a"
            (pp_print_list ~pp_sep:newline emit_using) usings
            (pp_print_list ~pp_sep:newline emit_fndef) functions
            emit_class cppclass
  | ns :: tl -> fprintf ppf "namespace %s {@ %a@ }" ns
                  (emit_namespaced cppclass functions usings) tl

let version = "// Code generated by Stan version 2.18.0"

let emit_prog ppf {cppclass; functions; includes; namespace; usings} =
  fprintf ppf "@[<v>%s@ %a@ %a@ @]}"
    version
    (pp_print_list ~pp_sep:newline emit_include) includes
    (emit_namespaced cppclass functions usings) namespace
