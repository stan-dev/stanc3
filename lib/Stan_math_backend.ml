open Mir
open Core_kernel
open Format

let comma ppf () = fprintf ppf ", "
let semi_new ppf () = fprintf ppf ";@;"
let newline ppf () = fprintf ppf "@;"
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
  | SArray (_, t) -> fprintf ppf "std::vector<%a>" (emit_stantype ad) t
  | SMatrix _ -> fprintf ppf "Eigen::Matrix<%s, -1, -1>" ad
  | SRowVector _ -> fprintf ppf "Eigen::Matrix<%s, 1, -1>" ad
  | SVector _ -> fprintf ppf "Eigen::Matrix<%s, -1, 1>" ad

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
  | ArrayLiteral es ->
      fprintf ppf "{%a}" (pp_print_list ~pp_sep:comma emit_expr) es
  | Indexed (e, idcs) ->
      fprintf ppf "%a%a" emit_expr e
        (pp_print_list ~pp_sep:comma emit_index)
        idcs
  (* TODO: Emit something like:
stan::model::rvalue(
    eigmat, stan::model::cons_list(
      stan::model::index_multi(y),
      stan::model::cons_list(
        stan::model::index_multi(y),
        stan::model::nil_index_list())),
    "eigmat"));
 *)
  | MultiIndexed (ident, indices) -> ignore (ident, indices)

let%expect_test "expr" =
  FnApp
    ( "sassy"
    , [ArrayLiteral [Lit (Int, "4"); Lit (Int, "2")]; Lit (Real, "27.0")] )
  |> emit_expr str_formatter ;
  flush_str_formatter () |> print_endline ;
  [%expect {| sassy({4, 2}, 27.0) |}]

let emit_prim_stantype ppf st =
  let rec ad_str = function
    | SInt -> "int"
    | SArray (_, t) -> ad_str t
    | _ -> "double"
  in
  emit_stantype (ad_str st) ppf st

let emit_block ppf (emit_body, body) =
  fprintf ppf "{@;<0 4>@[<v>%a@]@,}" emit_body body

let emit_for_loop ppf (loopvar, lower, upper, emit_body, body) =
  fprintf ppf "for (size_t %s = %a; %s < %a; %s++) %a" loopvar emit_expr lower
    loopvar emit_expr upper loopvar emit_block (emit_body, body)

let rec emit_run_code_per_el ?depth:(d = 0) emit_code_per_element ppf (name, st)
    =
  let mkloopvar d = sprintf "i_%d__" d in
  let loopvar = mkloopvar d in
  let zero = Lit (Int, "0") in
  match st with
  | SInt | SReal -> fprintf ppf "%a" emit_code_per_element name
  | SVector (Some dim) | SRowVector (Some dim) ->
      emit_for_loop ppf
        ( mkloopvar d
        , zero
        , dim
        , emit_code_per_element
        , sprintf "%s[%s]" name loopvar )
  | SMatrix (Some (dim1, dim2)) ->
      let loopvar2 = mkloopvar (d + 1) in
      emit_for_loop ppf
        ( loopvar
        , zero
        , dim1
        , emit_for_loop
        , ( loopvar2
          , zero
          , dim2
          , emit_code_per_element
          , sprintf "%s(%s, %s)" name loopvar loopvar2 ) )
  | SArray (Some dim, st) ->
      emit_for_loop ppf
        ( loopvar
        , zero
        , dim
        , emit_run_code_per_el ~depth:(d + 1) emit_code_per_element
        , (sprintf "%s[%s]" name loopvar, st) )
  | SMatrix None | SVector None | SRowVector None | SArray (None, _) ->
      raise_s [%message "Type should have dimensions" (st : stantype)]

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
      (* XXX Refactor these to share with other code gen *)
      fprintf ppf "while (%a) {\n  %a\n}\n" emit_expr cond emit_statement body
  | For {loopvar; lower; upper; body} ->
      let lv =
        fprintf str_formatter "%a" emit_expr loopvar ;
        flush_str_formatter ()
      in
      emit_for_loop ppf (lv, lower, upper, emit_statement, body)
  | Block s -> pp_print_list ~pp_sep:semi_new emit_statement ppf s
  | Decl ({vident; st; trans; loc}, rhs) ->
      ignore (trans, loc) ;
      let emit_assignment ppf rhs = fprintf ppf " = %a" emit_expr rhs in
      fprintf ppf "%a %s%a" emit_prim_stantype st vident
        (emit_option emit_assignment)
        rhs

let%expect_test "run code per element" =
  let assign ppf x =
    emit_statement ppf
      (Block
         [ Assignment
             { assignee= x
             ; indices= []
             ; rhs= Indexed (Var "vals_r__", [Var "pos__++"]) }
         ; NRFnApp ("print", [Var x]) ]) ;
    fprintf ppf ";"
    (* XXX when to print semis? *)
  in
  fprintf str_formatter "@[<v>%a@]"
    (emit_run_code_per_el assign)
    ( "dubvec"
    , SArray
        ( Some (Var "X")
        , SArray (Some (Var "Y"), SMatrix (Some (Var "Z", Var "W"))) ) ) ;
  flush_str_formatter () |> print_endline ;
  [%expect
    {|
    for (size_t i_0__ = 0; i_0__ < X; i_0__++) {
        for (size_t i_1__ = 0; i_1__ < Y; i_1__++) {
            for (size_t i_2__ = 0; i_2__ < Z; i_2__++) {
                for (size_t i_3__ = 0; i_3__ < W; i_3__++) {
                    dubvec[i_0__][i_1__](i_2__, i_3__) = vals_r__[pos__++];
                    print(dubvec[i_0__][i_1__](i_2__, i_3__));
                }
            }
        }
    } |}]

let%expect_test "decl" =
  Decl
    ( {vident= "i"; st= SInt; loc= "line num"; trans= NoTransformation}
    , Some (Lit (Int, "0")) )
  |> emit_statement str_formatter ;
  flush_str_formatter () |> print_endline ;
  [%expect {| int i = 0 |}]

let emit_vardecl ppf (name, stype) =
  fprintf ppf "%a %s;" emit_prim_stantype stype name

let stan_math_map =
  String.Map.of_alist_exn
    [("+", "add"); ("-", "minus"); ("/", "divide"); ("*", "multiply")]

let version = "// Code generated by Stan version 2.18.0"
let includes = "#include <stan/model/model_header.hpp>"

let emit_error_wrapper ppf (emit_err, err_arg, emit_contents, contents_arg) =
  fprintf ppf
    {|
@ try {
  @ @[<v 4> %a;
@] } catch (const std::exception& e) {
  @ @[<v 4> stan::lang::rethrow_located(std::runtime_error(std::string("%a")
  + e.what()), current_statement_begin__, prog_reader__());
  @ // Next line prevents compiler griping about no return
  @ throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
@]@ }
|}
    emit_contents contents_arg emit_err err_arg

let rec integer_el_type = function
  | SReal | SVector _ | SMatrix _ | SRowVector _ -> false
  | SInt -> true
  | SArray (_, st) -> integer_el_type st

let emit_args ppf (name, st) =
  fprintf ppf "const %a& %s" (emit_stantype "T0__") st name

let emit_udf ppf {returntype; name; arguments; body} =
  fprintf ppf
    {|
template <typename T0__>
%a
%s(%a, std::ostream* pstream__) {
    typedef typename boost::math::tools::promote_args<T0__>::type local_scalar_t__;
    typedef local_scalar_t__ fun_return_scalar_t__;
    const static bool propto__ = true;
    (void) propto__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
    int current_statement_begin__ = -1;
    %a
    }
} |}
    (emit_option ~default:"void"
       (emit_stantype "typename boost::math::tools::promote_args<T0__>::type"))
    returntype name
    (pp_print_list ~pp_sep:comma emit_args)
    arguments emit_error_wrapper
    (emit_str, "", emit_statement, body)

let%expect_test "udf" =
  fprintf str_formatter "@[<v>%a" emit_udf
    { returntype= None
    ; name= "sars"
    ; arguments= [("x", SMatrix None)]
    ; body= Return (FnApp ("add", [Var "x"; Lit (Int, "1")])) } ;
  flush_str_formatter () |> print_endline ;
  [%expect
    {|
    template <typename T0__>
    void
    sars(const Eigen::Matrix<T0__, -1, -1>& x, std::ostream* pstream__) {
        typedef typename boost::math::tools::promote_args<T0__>::type local_scalar_t__;
        typedef local_scalar_t__ fun_return_scalar_t__;
        const static bool propto__ = true;
        (void) propto__;
            local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
            (void) DUMMY_VAR__;  // suppress unused var warning
        int current_statement_begin__ = -1;


    try {

     return add(x, 1);
     } catch (const std::exception& e) {

     stan::lang::rethrow_located(std::runtime_error(std::string("")
      + e.what()), current_statement_begin__, prog_reader__());

        // Next line prevents compiler griping about no return

        throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");

    }

        }
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
*)

(*
let emit_read_field ppf (name, st, loc) idx =
  let vals_suffix = context_suffix st in
  fprintf ppf {|
@ current_statement_begin__ = %s;
@ context__.validate_dims("data initialization", "%s", "%s",
@                         context__.to_vec());
@ %s = %a;
@ vals_%s__ = context.__.vals_%s("%s");
@ pos__ = %d;
@ %s = vals_%s__[pos_++];
@ // Check constraints
@ %a
|} loc name emit_zero_val st idx name vals_suffix

let emit_constructor ppf p = fprintf ppf {|

@ void %s(stan::io::var_context& context__,
@ @[<v>   unsigned int random_seed__ = 0,
@         std::ostream* pstream__ = NULL) : prob_grad(0) {
@] @ @[<v 4>
@       typedef double local_scalar_t__;

@       boost::ecuyer1988 base_rng__ =
@         stan::services::util::create_rng(random_seed__, 0);
@       (void) base_rng__;  // suppress unused var warning

@       current_statement_begin__ = -1;

@       static const char* function__ = "bernoulli_model_namespace::bernoulli_model";
@       (void) function__;  // dummy to suppress unused var warning
@       size_t pos__;
@       (void) pos__;  // dummy to suppress unused var warning
@       std::vector<int> vals_i__;
@       std::vector<double> vals_r__;
@       local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
@       (void) DUMMY_VAR__;  // suppress unused var warning
@ %a
@ %a
@]@ }
|} emit_error_wrapper (emit_str, "Error while reading in data: ", p.data)
    emit_error_wrapper (emit_str, "Error during transformed data block: ")


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
@ @ ~%s_model() { }
@ @ static std::string model_name() { return "%s"; }
@ @ // transform_inits takes in constrained init values for parameters and unconstrains them
@ %a
@ @ // The log_prob function transforms the parameters from the unconstrained space
@ // back to the constrained space and runs the model block on them.
@ %a
@ @ %a
@ @ %a
@]
@]
}; // model
|} p.prog_name
    (pp_print_list ~pp_sep:semi_new emit_vardecl) p.data
    emit_constructor p
    p.prog_name p.prog_name
(* transform inits
   log_prob
   get_param_names
   get_dims
   write_array
*)


let globals = "static int current_statement_begin__;"
let usings = {|
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
|}


let emit_prog ppf (p: Mir.prog) =
  fprintf ppf "@[<v>@ %s@ %s@ namespace %s_model_namespace {@ %s@ %s@ %a@ %a@ }@ @]"
    version includes p.prog_name
    globals
    usings
    (pp_print_list emit_udf) p.functions
    emit_model p

let%expect_test "prog" =

  { functions= []
  ; params= ["theta", SReal, "line 7"]
  ; data: ["y", SReal, "line 8"]
  ; model: NRFnApp("print", Lit(Str, "hello world"))
  ; gq: Skip ; tdata: Skip ; tparam: Skip
  ; prog_name: "testmodel"
  ; prog_path: "/testmodel.stan"}
  |> emit_prog str_formatter ;
  flush_str_formatter () |> print_endline ;
  [%expect {| sassy({4, 2}, 27.0) |}]
*)
