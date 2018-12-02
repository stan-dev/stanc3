open Mir
open Core_kernel
open Format

let comma ppf () = fprintf ppf ", "

let semi_new ppf () = fprintf ppf ";@ "

let newline ppf () = fprintf ppf "@ "

let emit_str ppf s = fprintf ppf "%s" s

let emit_option ?default:(d = "") emitter ppf opt =
  match opt with
  | Some x -> fprintf ppf "%a" emitter x
  | None -> emit_str ppf d

let emit_cond_op ppf c =
  emit_str ppf
    ( match c with
    | Equals -> "=="
    | NEquals -> "!="
    | Less -> "<"
    | Leq -> "<="
    | Greater -> ">"
    | Geq -> ">=" )

let rec emit_stantype ad ppf = function
  | SInt | SReal -> emit_str ppf ad
  | SArray t -> fprintf ppf "std::vector<%a>" (emit_stantype ad) t
  | SMatrix -> fprintf ppf "Eigen::Matrix<%s, -1, -1>" ad
  | SRowVector -> fprintf ppf "Eigen::Matrix<%s, 1, -1>" ad
  | SVector -> fprintf ppf "Eigen::Matrix<%s, -1, 1>" ad

and emit_index ppf e = fprintf ppf "[%a]" emit_expr e

and emit_expr ppf s =
  match s with
  | Var s -> emit_str ppf s
  | Lit (Str, s) -> fprintf ppf "\"%s\"" s
  | Lit (_, s) -> emit_str ppf s
  | FnApp (fname, args) ->
      fprintf ppf "%s(%a)" fname (pp_print_list ~pp_sep:comma emit_expr) args
  | Cond (e1, op, e2) ->
      emit_expr ppf e1 ; emit_cond_op ppf op ; emit_expr ppf e2
  | ArrayExpr es ->
      fprintf ppf "{%a}" (pp_print_list ~pp_sep:comma emit_expr) es
  | Indexed (e, idcs) ->
      (* totally guessing here*)
      fprintf ppf "%a%a" emit_expr e
        (pp_print_list ~pp_sep:comma emit_index)
        idcs

let%expect_test "expr" =
  FnApp
    ("sassy", [ArrayExpr [Lit (Int, "4"); Lit (Int, "2")]; Lit (Real, "27.0")])
  |> emit_expr str_formatter ;
  flush_str_formatter () |> print_endline ;
  [%expect {| sassy({4, 2}, 27.0) |}]

let emit_vanilla_stantype ppf st =
  let rec ad_str = function
    | SInt -> "int"
    | SArray t -> ad_str t
    | _ -> "double"
  in
  emit_stantype (ad_str st) ppf st

let rec emit_statement ppf s =
  match s with
  | Assignment {assignee; indices; rhs} ->
      fprintf ppf "%s%a = %a" assignee
        (pp_print_list ~pp_sep:comma emit_index)
        indices emit_expr rhs
  | NRFnApp (fname, args) ->
      fprintf ppf "%s(%a)" fname (pp_print_list ~pp_sep:comma emit_expr) args
  | Break -> emit_str ppf "break"
  | Continue -> emit_str ppf "continue"
  | Return e -> fprintf ppf "%s%a" "return " emit_expr e
  | Skip -> ()
  | IfElse (cond, ifbranch, elsebranch) ->
      let emit_else ppf x = fprintf ppf " else {\n %a\n}" emit_statement x in
      fprintf ppf "if (%a){\n %a\n}%a\n" emit_expr cond emit_statement ifbranch
        (emit_option emit_else) elsebranch
  | While (cond, body) ->
      fprintf ppf "while (%a) {\n  %a\n}\n" emit_expr cond emit_statement body
  | For {init; cond; step; body} ->
      fprintf ppf "for (%a; %a; %a) {\n  %a\n}\n" emit_statement init emit_expr
        cond emit_statement step emit_statement body
  | Block s -> pp_print_list ~pp_sep:semi_new emit_statement ppf s
  | Decl ((st, ident), rhs) ->
      let emit_assignment ppf rhs = fprintf ppf " = %a" emit_expr rhs in
      fprintf ppf "%a %s%a" emit_vanilla_stantype ident st
        (emit_option emit_assignment)
        rhs

let%expect_test "decl" =
  Decl (("i", SInt), Some (Lit (Int, "0"))) |> emit_statement str_formatter ;
  flush_str_formatter () |> print_endline ;
  [%expect {| int i = 0 |}]

let%expect_test "statement" =
  For
    { init= Decl (("i", SInt), Some (Lit (Int, "1")))
    ; cond= Cond (Var "i", Geq, Lit (Int, "10"))
    ; step=
        Assignment {assignee= "i"; rhs= (FnApp("+", [Var "i"; Lit (Int, "0")])); indices= []}
    ; body= NRFnApp ("print", [Var "i"]) }
  |> emit_statement str_formatter ;
  flush_str_formatter () |> print_endline ;
  [%expect {|
    for (int i = 1; i>=10; i = +(i, 0)) {
      print(i)
    } |}]
(*

let emit_fndef ppf {returntype; name; arguments; body; templates} =
  let templated =
    List.exists
      (fun (ad, _, _, _) -> match ad with AData -> false | AVar -> true)
      arguments
  in
  let templates = if templated then "T__" :: templates else templates in
  fprintf ppf "@[<v>%a%a %s(%a) {@ @[<v 2>  %a;@]@ }@]" emit_templates
    templates
    (emit_option ~default:"void" emit_ad_stantype)
    returntype name
    (pp_print_list ~pp_sep:comma emit_ad_vardecl)
    arguments emit_statement body

let emit_ctor classname ppf (args, body) =
  fprintf ppf "%s(%a) {@ @[<v 2>%a@]}" classname
    (pp_print_list ~pp_sep:comma emit_vardecl)
    args emit_statement body

let emit_class ppf {classname; super; fields; methods; ctor} =
  fprintf ppf
    {|
@[<v 1>
class %s : %s {
@  private:@
  @[<v 1> %a;@]
@ @  public:
@ @[<v 1> %a
@ %a@]
@ }@.|}
    classname super
    (pp_print_list ~pp_sep:semi_new emit_vardecl)
    fields
    (pp_print_list ~pp_sep:newline emit_fndef)
    methods (emit_ctor classname) ctor

let%expect_test "class" =
  emit_class str_formatter
    { classname= "bernoulli_model"
    ; super= "prob_grad"
    ; fields= [(SMatrix, "x"); (SVector, "y")]
    ; ctor= ([], Skip)
    ; methods=
        [ { returntype= Some (AVar, SReal)
          ; name= "log_prob"
          ; arguments= [(AVar, Immutable, SVector, "params")]
          ; templates= []
          ; body=
              Block
                [ Assignment
                    { assignee= "target"
                    ; op= Plus
                    ; indices= []
                    ; rhs=
                        FnApp
                          ( "normal"
                          , [ FnApp ("multiply", [Var "x"; Var "params"])
                            ; Lit (Real, "1.0") ] ) } ] }
        ; { returntype= None
          ; name= "get_param_names"
          ; arguments= [(AData, Mutable, SString, "names")]
          ; templates= []
          ; body=
              Block
                [ Assignment
                    { assignee= "target"
                    ; op= Plus
                    ; indices= []
                    ; rhs=
                        FnApp
                          ( "normal"
                          , [ FnApp ("multiply", [Var "x"; Var "params"])
                            ; Lit (Real, "1.0") ] ) } ] } ] } ;
  flush_str_formatter () |> print_endline ;
  [%expect
    {|
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

let emit_prog ppf {cppclass; functions; includes; namespace; usings} =
  fprintf ppf "@[<v>%s@ %a@ %a@ @]}" version
    (pp_print_list ~pp_sep:newline emit_include)
    includes
    (emit_namespaced cppclass functions usings)
    namespace
*)

let stan_math_map =
  String.Map.of_alist_exn
    [("+", "add"); ("-", "minus"); ("/", "divide"); ("*", "multiply")]

let emit_version ppf () = fprintf ppf "// Code generated by Stan version 2.18.0"
let emit_includes ppf () = fprintf ppf "#include <stan/model/model_header.hpp>"

let emit_udf ppf {returntype; name; arguments; body}=
  fprintf ppf "%s" (string_of_sexp [%sexp ({returntype; name; arguments; body} : Mir.udf_defn)])

let emit_model ppf p =
  fprintf ppf {|
class %s_model : public prob_grad {
@ @[<v 1>
@ private:
@ // Fields here from the data block of the model
@ @<v 1>[%a@]@

@ public:
@ @<v 1>[
@ // Constructor takes in data fields and transforms them
@ %a
@ @ // transform_inits takes in constrained init values for parameters and unconstrains them
@ %a
@ @ ~%s_model() { }
@ @ // The log_prob function transforms the parameters from the unconstrained space
@ // back to the constrained space and runs the model block on them.
@ template <bool propto__, bool jacobian__, typename T__>
@ T__ log_prob(@[<v>std::vector<T__>& params_r__,
@ std::ostream* pstream__ = 0) const { @]
@[<v 2>
@ %a
@ } // log_prob()
@ @ void get_param_names(std::vector<std::string>& names__) const {
@ @[<v 2>

@ @ typedef T__ local_scalar_t__;

@ @ local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
@ (void) DUMMY_VAR__;  // dummy to suppress unused var warning
@ names__.resize(0);
%a
@ @]
@ } // get_param_names()
@ @ void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
@ @[<v 2>
@ dimss__.resize(0);
@ %a
@ @]
@ } // get_dims()
@ template <typename RNG>
@ void write_array(RNG& base_rng__,
@                  std::vector<double>& params_r__,
@                  std::vector<double>& vars__,
@                  bool include_tparams__ = true,
@                  bool include_gqs__ = true,
@                  std::ostream* pstream__ = 0) const {
@ static const char* function__ = "bernoulli_model_namespace::write_array";
@ (void) function__;  // dummy to suppress unused var warning
@ vars__.resize(0);
@ %a
]@
@ } // write_array()

    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }

    static std::string model_name() {
        return "bernoulli_model";
    }


    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "theta";
        param_names__.push_back(param_name_stream__.str());

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
        }

        if (!include_gqs__) return;
    }


    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "theta";
        param_names__.push_back(param_name_stream__.str());

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
        }

        if (!include_gqs__) return;
    }

}; // model
|}  p.prog_name

let emit_namespaced ppf (p: Mir.prog) =
  fprintf ppf "@[<v>@ %a@ %a@ namespace %s_model_namespace {@ %a @ %a@ }@ @]"
    emit_version () emit_includes () p.prog_name
    (pp_print_list emit_udf) p.functions
    emit_model p
