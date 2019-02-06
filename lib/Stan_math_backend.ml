(** Generate C++ from the MIR *)

open Core_kernel
open Mir
open Format

let comma ppf () = fprintf ppf ",@ "
let newline ppf () = fprintf ppf "@;"
let semi_new ppf () = fprintf ppf "@;"
let emit_str ppf s = fprintf ppf "%s" s
let zero = Lit (Int, "0")

let emit_option ppf (emitter, opt, default) =
  match opt with
  | Some x -> fprintf ppf "%a" emitter x
  | None -> emit_str ppf default

let rec emit_unsizedtype ad ppf = function
  | Ast.UInt | UReal -> emit_str ppf ad
  | UArray t -> fprintf ppf "std::vector<%a>" (emit_unsizedtype ad) t
  | UMatrix -> fprintf ppf "Eigen::Matrix<%s, -1, -1>" ad
  | URowVector -> fprintf ppf "Eigen::Matrix<%s, 1, -1>" ad
  | UVector -> fprintf ppf "Eigen::Matrix<%s, -1, 1>" ad
  | x -> raise_s [%message (x : unsizedtype) "not implemented yet"]

let%expect_test "emit function type raises" =
  ( match emit_unsizedtype "sars" str_formatter Ast.UMathLibraryFunction with
  | () -> print_endline (flush_str_formatter ())
  | exception e -> print_s [%sexp (e : exn)] ) ;
  [%expect {| ((x UMathLibraryFunction) "not implemented yet") |}]

let emit_call ppf (name, emit_args, args) =
  fprintf ppf "%s(@[<hov>%a@])" name emit_args args

let rec emit_expr ppf s =
  match s with
  | Var s -> emit_str ppf s
  | Lit (Str, s) -> fprintf ppf "\"%s\"" s
  | Lit (_, s) -> emit_str ppf s
  | FnApp (fname, args) ->
      emit_call ppf (fname, pp_print_list ~pp_sep:comma emit_expr, args)
  | BinOp (e1, op, e2) ->
      fprintf ppf "%a %s %a" emit_expr e1
        (Operators.operator_name op)
        emit_expr e2
  | TernaryIf (cond, ifb, elseb) ->
      fprintf ppf "(%a) ? (%a) : (%a)" emit_expr cond emit_expr ifb emit_expr
        elseb
  | Indexed (e, idcs) ->
      fprintf ppf "@[<hov 4>stan::model::rvalue(%a,@,@[<hov>%a,@]@ \"%a\");@]"
        emit_expr e emit_indices idcs emit_expr e

and emit_index ppf idx =
  let idx_phrase fmt idtype =
    fprintf ppf fmt ("stan::model::index_" ^ idtype)
  in
  match idx with
  | All -> idx_phrase "%s" "omni()"
  | Single e -> idx_phrase "%s(%a)" "uni" emit_expr e
  | Upfrom e -> idx_phrase "%s(%a)" "min" emit_expr e
  | Downfrom e -> idx_phrase "%s(%a)" "max" emit_expr e
  | Between (e1, e2) ->
      idx_phrase "%s(%a, %a)" "min_max" emit_expr e1 emit_expr e2
  | MultiIndex e -> idx_phrase "%s(%a)" "multi" emit_expr e

and emit_indices ppf = function
  | [] -> fprintf ppf "stan::model::nil_index_list()"
  | hd :: tail ->
      fprintf ppf "stan::model::cons_list(%a,@ %a)" emit_index hd emit_indices
        tail

let%expect_test "multi index" =
  Indexed (Var "vec", [MultiIndex (Var "intarr1"); MultiIndex (Var "intarr2")])
  |> fprintf str_formatter "%a" emit_expr ;
  flush_str_formatter () |> print_endline ;
  [%expect
    {|
    stan::model::rvalue(vec,
        stan::model::cons_list(stan::model::index_multi(intarr1),
        stan::model::cons_list(stan::model::index_multi(intarr2),
        stan::model::nil_index_list())), "vec"); |}]

let rec stantype_prim_str = function
  | Ast.UInt -> "int"
  | UArray t -> stantype_prim_str t
  | _ -> "double"

let emit_prim_stantype ppf st = emit_unsizedtype (stantype_prim_str st) ppf st

let emit_block ppf (emit_body, body) =
  fprintf ppf "{@;<1 4>@[<v>%a@]@,}" emit_body body

let emit_for_loop ppf (loopvar, lower, upper, emit_body, body) =
  fprintf ppf "@[<hov>for (@[<hov>size_t %s = %a;@ %s < %a;@ %s++@])" loopvar
    emit_expr lower loopvar emit_expr upper loopvar ;
  fprintf ppf "@;<0 4>@[<v>%a@]@]" emit_body body

(* XXX This should probably recursively build up a statement For loop instead...*)
let rec emit_run_code_per_el ?depth:(d = 0) emit_code_per_element ppf (name, st)
    =
  let mkloopvar d = sprintf "i_%d__" d in
  let loopvar = mkloopvar d in
  (*let for_loop = {loopvar; lower= zero; upper=dim; body}
*)
  match st with
  | Ast.SInt | SReal -> fprintf ppf "%a" emit_code_per_element name
  | SVector dim | SRowVector dim ->
      emit_for_loop ppf
        ( loopvar
        , zero
        , dim
        , emit_code_per_element
        , sprintf "%s[%s]" name loopvar )
  | SMatrix (dim1, dim2) ->
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
  | SArray (st, dim) ->
      emit_for_loop ppf
        ( loopvar
        , zero
        , dim
        , emit_run_code_per_el ~depth:(d + 1) emit_code_per_element
        , (sprintf "%s[%s]" name loopvar, st) )

let rec integer_el_type = function
  | Ast.SReal | SVector _ | SMatrix _ | SRowVector _ -> false
  | SInt -> true
  | SArray (st, _) -> integer_el_type st

let emit_arg ppf ((adtype, name, st), idx) =
  let adstr =
    match adtype with
    | Ast.DataOnly -> stantype_prim_str st
    | Ast.AutoDiffable -> sprintf "T%d__" idx
  in
  fprintf ppf "const %a& %s" (emit_unsizedtype adstr) st name

let emit_template_decls ppf ts =
  fprintf ppf "@[<hov>template <%a>@]@ "
    (pp_print_list ~pp_sep:comma (fun ppf t -> fprintf ppf "typename %s" t))
    ts

let with_idx lst = List.(zip_exn lst (range 0 (length lst)))

let emit_error_wrapper ppf (emit_err, err_arg, emit_contents, contents_arg) =
  emit_str ppf "try " ;
  emit_block ppf (emit_contents, contents_arg) ;
  emit_str ppf " catch const std::exception& e) " ;
  emit_block ppf (emit_err, err_arg)

let emit_located_msg ppf msg =
  fprintf ppf
    {|stan::lang::rethrow_located(
    std::runtime_error(std::string(%s) + e.what(), current_statement__);
// Next line prevents compiler griping about no return
throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
|}
  @@ Option.value ~default:"e" msg

let emit_located_error ppf (emit_contents, contents_arg, msg) =
  emit_error_wrapper ppf (emit_contents, contents_arg, emit_located_msg, msg)

let emit_statement emit_statement_with_meta ppf s =
  let emit_stmt_list =
    pp_print_list ~pp_sep:newline emit_statement_with_meta
  in
  match s with
  | Assignment (assignee, rhs) ->
      (* XXX completely wrong *)
      fprintf ppf "%a = %a;" emit_expr assignee emit_expr rhs
  | NRFnApp (fname, args) ->
      fprintf ppf "%s(%a);" fname (pp_print_list ~pp_sep:comma emit_expr) args
  | Break -> emit_str ppf "break;"
  | Continue -> emit_str ppf "continue;"
  | Return e -> fprintf ppf "return %a;" emit_option (emit_expr, e, "")
  | Skip -> ()
  | MarkLocation _ -> () (* XXX *)
  | Check _ -> () (* XXX *)
  | IfElse (cond, ifbranch, elsebranch) ->
      let emit_else ppf x =
        fprintf ppf " else {\n %a;\n}" emit_statement_with_meta x
      in
      fprintf ppf "if (%a) %a %a\n" emit_expr cond emit_block
        (emit_statement_with_meta, ifbranch)
        emit_option
        (emit_else, elsebranch, "")
  | While (cond, body) ->
      fprintf ppf "while (%a) %a" emit_expr cond emit_block
        (emit_statement_with_meta, body)
  | For {loopvar; lower; upper; body} ->
      let lv =
        fprintf str_formatter "%a" emit_expr loopvar ;
        flush_str_formatter ()
      in
      emit_for_loop ppf (lv, lower, upper, emit_statement_with_meta, body)
  | Block ls -> emit_block ppf (emit_stmt_list, ls)
  | SList ls -> emit_stmt_list ppf ls
  | Decl {adtype; vident; st} ->
      ignore adtype ;
      fprintf ppf "%a %s;" emit_prim_stantype (Ast.remove_size st) vident
  | FunDef {returntype; name; arguments; body} ->
      let argtypetemplates =
        List.mapi ~f:(fun i _ -> sprintf "T%d__" i) arguments
      in
      emit_template_decls ppf argtypetemplates ;
      fprintf ppf "%a@ " emit_option
        ( emit_unsizedtype "typename boost::math::tools::promote_args<>::type"
        , returntype
        , "void" ) ;
      (* XXX this is all so ugly: *)
      emit_call ppf
        ( name
        , (fun ppf (args, extra_arg) ->
            (pp_print_list ~pp_sep:comma emit_arg) ppf (with_idx args) ;
            fprintf ppf ",@ %s" extra_arg )
        , (arguments, "std::ostream* pstream__") ) ;
      fprintf ppf " " ;
      emit_block ppf
        ( (fun ppf body ->
            let text = fprintf ppf "%s@;" in
            fprintf ppf
              "@[<hv 8>typedef typename \
               boost::math::tools::promote_args<%a>::type \
               local_scalar_t__;@]@ "
              (pp_print_list ~pp_sep:comma emit_str)
              argtypetemplates ;
            text "typedef local_scalar_t__ fun_return_scalar_t__;" ;
            text "const static bool propto__ = true;" ;
            text "(void) propto__;" ;
            text
              "local_scalar_t__ \
               DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());" ;
            text "(void) DUMMY_VAR__;  // suppress unused var warning" ;
            text "int current_statement_begin__ = -1;" ;
            emit_located_error ppf (emit_statement_with_meta, body, None) )
        , body )

let rec emit_statement_loc ppf {sloc; stmt} =
  (*
  Sexp.to_string_hum [%sexp (stmt : stmt_loc statement)] |> print_endline ;
*)
  fprintf ppf "current_statement_loc__ = \"%s\";@;" sloc ;
  emit_statement emit_statement_loc ppf stmt

let%expect_test "location propagates" =
  {sloc= "hi"; stmt= Block [{stmt= Break; sloc= "lo"}]}
  |> fprintf str_formatter "@[<v>%a@]" emit_statement_loc ;
  flush_str_formatter () |> print_endline ;
  [%expect
    {|
    current_statement_loc__ = "hi";
    {
        current_statement_loc__ = "lo";
        break;
    } |}]

type stmt_plain = {splain: stmt_plain statement}

let rec emit_statement_plain ppf {splain} =
  emit_statement emit_statement_plain ppf splain

let%expect_test "if" =
  {splain= IfElse (Var "true", {splain= NRFnApp ("print", [Var "x"])}, None)}
  |> fprintf str_formatter "@[<v>%a@]" emit_statement_plain ;
  flush_str_formatter () |> print_endline ;
  [%expect {|
    if (true) {
        print(x);
    } |}]

let%expect_test "run code per element" =
  let assign ppf x =
    emit_statement_plain ppf
      { splain=
          Block
            [ { splain=
                  Assignment
                    (Var x, Indexed (Var "vals_r__", [Single (Var "pos__++")]))
              }
            ; {splain= NRFnApp ("print", [Var x])} ] }
  in
  fprintf str_formatter "@[<v>%a@]"
    (emit_run_code_per_el assign)
    ("dubvec", SArray (SArray (SMatrix (Var "Y", Var "Z"), Var "X"), Var "W")) ;
  flush_str_formatter () |> print_endline ;
  [%expect
    {|
    for (size_t i_0__ = 0; i_0__ < W; i_0__++)
        for (size_t i_1__ = 0; i_1__ < X; i_1__++)
            for (size_t i_2__ = 0; i_2__ < Y; i_2__++)
                for (size_t i_3__ = 0; i_3__ < Z; i_3__++)
                    {
                        dubvec[i_0__][i_1__](i_2__, i_3__) = stan::model::rvalue(vals_r__,
                                                                 stan::model::cons_list(stan::model::index_uni(pos__++),
                                                                 stan::model::nil_index_list()),
                                                                 "vals_r__");;
                        print(dubvec[i_0__][i_1__](i_2__, i_3__));
                    } |}]

let%expect_test "decl" =
  {splain= Decl {adtype= AutoDiffable; vident= "i"; st= SInt}}
  |> emit_statement_plain str_formatter ;
  flush_str_formatter () |> print_endline ;
  [%expect {| int i; |}]

let version = "// Code generated by Stan version 2.18.0"
let includes = "#include <stan/model/model_header.hpp>"

let%expect_test "udf" =
  fprintf str_formatter "@[<v>%a"
    (emit_statement emit_statement_plain)
    (FunDef
       { returntype= None
       ; name= "sars"
       ; arguments=
           [(Ast.DataOnly, "x", UMatrix); (Ast.AutoDiffable, "y", URowVector)]
       ; body=
           {splain= Return (Some (FnApp ("add", [Var "x"; Lit (Int, "1")])))}
       }) ;
  flush_str_formatter () |> print_endline ;
  [%expect
    {|
    template <typename T0__, typename T1__>
    void
    sars(const Eigen::Matrix<double, -1, -1>& x,
         const Eigen::Matrix<T1__, 1, -1>& y, std::ostream* pstream__) {
        typedef typename boost::math::tools::promote_args<T0__,
                T1__>::type local_scalar_t__;
        typedef local_scalar_t__ fun_return_scalar_t__;
        const static bool propto__ = true;
        (void) propto__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        int current_statement_begin__ = -1;
        try {
            stan::lang::rethrow_located(
        std::runtime_error(std::string(e) + e.what(), current_statement__);
    // Next line prevents compiler griping about no return
    throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");

        } catch const std::exception& e) {
            return add(x, 1);
        }
    } |}]

let rec basetype = function
  | Ast.UInt -> "int"
  | UReal -> "double"
  | UArray t -> basetype t
  | UMatrix -> "matrix_d"
  | URowVector -> "row_vector_d"
  | UVector -> "vector_d"
  | x -> raise_s [%message "basetype not defined for " (x : unsizedtype)]

let rec emit_zeroing_ctor_call ppf st =
  match st with
  | Ast.SInt | SReal -> emit_str ppf "0"
  | SArray (t, dim) ->
      fprintf ppf "%a, %a" emit_expr dim emit_zeroing_ctor_call t
  | SRowVector dim | SVector dim ->
      ignore dim ;
      fprintf ppf "%s" "XXX todo"
  | SMatrix (dim1, dim2) -> ignore (dim1, dim2)

(* XXX *)

let emit_zero ppf (name, st) =
  let ut = Ast.remove_size st in
  fprintf ppf "%s = %a(%a);" name
    (emit_unsizedtype (stantype_prim_str ut))
    ut emit_zeroing_ctor_call st

let var_context_container st =
  match basetype (Ast.remove_size st) with "int" -> "vals_i" | _ -> "vals_r"

(* Read in data steps:
   1. context__.validate_dims() (what is this?)
   2. Set data field to 0
   3. find vals_%s__ from context__.vals_%s(vident)
   4. keep track of pos__
   5. run checks on resulting vident
*)
let emit_read_data ppf (vident, st) =
  fprintf ppf "%a@ " emit_zero (vident, st) ;
  let vals = var_context_container st in
  let emit_read ppf loopvar = fprintf ppf "%s = %s;@ " vident loopvar in
  fprintf ppf "%s__ = context__.%s(\"%s\");@ " vals vals vident ;
  emit_run_code_per_el emit_read ppf (vals, st)

let%expect_test "read int N" =
  fprintf str_formatter "@[<v>%a@]" emit_read_data ("N", Ast.SInt) ;
  flush_str_formatter () |> print_endline ;
  [%expect
    {|
    N = int(0);
    vals_i__ = context__.vals_i("N");
    N = vals_i; |}]

(*

let emit_constructor ppf p = fprintf ppf {|
@ %s(stan::io::var_context& context__,
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
|}
    p.prog_name
    emit_located_error ((pp_print_list ~pp_sep:newline emit_read_field),
                        p.datab, Some "Error while reading in data: ")
    emit_located_error ((pp_print_list ~pp_sep:newline emit_transform),
                        p.datab, Some "Error during transformed data block: ")

let emit_model ppf (p: stmt_loc prog) =
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
    emit_statement_loc p.datab
    emit_constructor p
    p.prog_name p.prog_name
(* transform inits
   log_prob
   get_param_names
   get_dims
   write_array
*)

*)

(*
let%expect_test "prog" =
  { functionsb= Skip
  ; params= Decl (AutoDiffable, "theta", SReal)
  ; data: ["y", SReal, "line 8"]
  ; model: NRFnApp("print", Lit(Str, "hello world"))
  ; gq: Skip ; tdata: Skip ; tparam: Skip
  ; prog_name: "testmodel"
  ; prog_path: "/testmodel.stan"}
  |> emit_prog str_formatter ;
  flush_str_formatter () |> print_endline ;
  [%expect {| sassy({4, 2}, 27.0) |}]
*)

(* XXX Adds declarations.
   How will this mix with all the passes matching on declarations and adding
   new statements for each one, e.g. data reading and checking in the data block?*)
let rec array_to_for sloc (ident, st) bodyfn =
  match st with
  | Ast.SArray (t, dim) ->
      let loopvar = Mir.gensym () in
      let decl = Decl {adtype= Ast.DataOnly; vident= loopvar; st} in
      let body = array_to_for sloc (loopvar, t) bodyfn in
      let sfor = For {loopvar= Var loopvar; lower= zero; upper= dim; body} in
      let stmts = List.map ~f:(fun stmt -> {stmt; sloc}) [decl; sfor] in
      {stmt= SList stmts; sloc}
  | _ -> {stmt= bodyfn ident; sloc}

let trans_checks s =
  match s.stmt with
  | Check {cfname; cvarname; ctype; cargs} ->
      (* XXX The function__ is weird... maybe find a better way to handle it *)
      let ckfn ident =
        NRFnApp ("check_" ^ cfname, Var "function__" :: Var ident :: cargs)
      in
      array_to_for s.sloc (cvarname, ctype) ckfn
  | _ -> s

let%expect_test "trans check" =
  let cfname = "greater_or_equal" and cargs = [zero] and ctype = Ast.SInt in
  let ck = {stmt= Check {cfname; cvarname= "N"; ctype; cargs}; sloc= ""} in
  print_s [%sexp ((trans_checks ck).stmt : stmt_loc statement)] ;
  [%expect
    {| (NRFnApp check_greater_or_equal ((Var function__) (Var N) (Lit Int 0))) |}]

let add_data_read_mir s = match s.stmt with Decl _ -> s | _ -> s

(* XXX *)
let emit_model ppf p = ignore (ppf, p)
let globals = "static int current_statement_begin__;"

let usings =
  {|
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
|}

let emit_prog ppf (p : stmt_loc prog) =
  let tvtable, datab = p.datab in
  let datab = add_data_read_mir datab in
  let datab = (Mir.map_toplevel_stmts trans_checks) datab in
  let datab = (tvtable, datab) in
  fprintf ppf
    "@[<v>@ %s@ %s@ namespace %s_model_namespace {@ %s@ %s@ %a@ %a@ }@ @]"
    version includes p.prog_name globals usings emit_statement_loc p.functionsb
    emit_model {p with datab}
