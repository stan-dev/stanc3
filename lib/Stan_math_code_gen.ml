(** Generate C++ from the MIR *)

open Core_kernel
open Mir
open Fmt

let zero = Lit (Int, "0")

let rec pp_unsizedtype ad ppf = function
  | Ast.UInt | UReal -> string ppf ad
  | UArray t -> pf ppf "std::vector<%a>" (pp_unsizedtype ad) t
  | UMatrix -> pf ppf "Eigen::Matrix<%s, -1, -1>" ad
  | URowVector -> pf ppf "Eigen::Matrix<%s, 1, -1>" ad
  | UVector -> pf ppf "Eigen::Matrix<%s, -1, 1>" ad
  | x -> raise_s [%message (x : unsizedtype) "not implemented yet"]

let%expect_test "emit function type raises" =
  ( match pp_unsizedtype "sars" stdout Ast.UMathLibraryFunction with
  | () -> ()
  | exception e -> print_s [%sexp (e : exn)] ) ;
  [%expect {| ((x UMathLibraryFunction) "not implemented yet") |}]

let pp_call ppf (name, pp_args, args) =
  pf ppf "%s(@[<hov>%a@])" name pp_args args

let rec pp_expr ppf s =
  match s with
  | Var s -> string ppf s
  | Lit (Str, s) -> pf ppf "\"%s\"" s
  | Lit (_, s) -> string ppf s
  | FunApp (fname, args) -> pp_call ppf (fname, list ~sep:comma pp_expr, args)
  | BinOp (e1, op, e2) ->
      pf ppf "%a %s %a" pp_expr e1 (Operators.operator_name op) pp_expr e2
  | TernaryIf (cond, ifb, elseb) ->
      pf ppf "(%a) ? (%a) : (%a)" pp_expr cond pp_expr ifb pp_expr elseb
  | Indexed (e, idcs) ->
      pf ppf "@[<hov 4>stan::model::rvalue(%a,@,@[<hov>%a,@]@ \"%a\");@]"
        pp_expr e pp_indices idcs pp_expr e

and pp_index ppf idx =
  let idx_phrase fmt idtype = pf ppf fmt ("stan::model::index_" ^ idtype) in
  match idx with
  | All -> idx_phrase "%s" "omni()"
  | Single e -> idx_phrase "%s(%a)" "uni" pp_expr e
  | Upfrom e -> idx_phrase "%s(%a)" "min" pp_expr e
  | Downfrom e -> idx_phrase "%s(%a)" "max" pp_expr e
  | Between (e1, e2) -> idx_phrase "%s(%a, %a)" "min_max" pp_expr e1 pp_expr e2
  | MultiIndex e -> idx_phrase "%s(%a)" "multi" pp_expr e

and pp_indices ppf = function
  | [] -> pf ppf "stan::model::nil_index_list()"
  | hd :: tail ->
      pf ppf "stan::model::cons_list(%a,@ %a)" pp_index hd pp_indices tail

let%expect_test "multi index" =
  Indexed (Var "vec", [MultiIndex (Var "intarr1"); MultiIndex (Var "intarr2")])
  |> strf "%a" pp_expr |> print_endline ;
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

let pp_prim_stantype ppf st = pp_unsizedtype (stantype_prim_str st) ppf st
let pp_block ppf (pp_body, body) = pf ppf "{@;<1 2>@[<v>%a@]@,}" pp_body body

let pp_for_loop ppf (loopvar, lower, upper, pp_body, body) =
  pf ppf "@[<hov>for (@[<hov>size_t %s = %a;@ %s < %a;@ %s++@])" loopvar
    pp_expr lower loopvar pp_expr upper loopvar ;
  pf ppf "@ @;<0 2>@[<v>%a@]@]" pp_body body

let rec pp_run_code_per_el ?depth:(d = 0) pp_code_per_element ppf (name, st) =
  let mkloopvar d = sprintf "i_%d__" d in
  let loopvar = mkloopvar d in
  match st with
  | Ast.SInt | SReal -> pf ppf "%a" pp_code_per_element name
  | SVector dim | SRowVector dim ->
      pp_for_loop ppf
        (loopvar, zero, dim, pp_code_per_element, sprintf "%s[%s]" name loopvar)
  | SMatrix (dim1, dim2) ->
      let loopvar2 = mkloopvar (d + 1) in
      pp_for_loop ppf
        ( loopvar
        , zero
        , dim1
        , pp_for_loop
        , ( loopvar2
          , zero
          , dim2
          , pp_code_per_element
          , sprintf "%s(%s, %s)" name loopvar loopvar2 ) )
  | SArray (st, dim) ->
      pp_for_loop ppf
        ( loopvar
        , zero
        , dim
        , pp_run_code_per_el ~depth:(d + 1) pp_code_per_element
        , (sprintf "%s[%s]" name loopvar, st) )

let rec integer_el_type = function
  | Ast.SReal | SVector _ | SMatrix _ | SRowVector _ -> false
  | SInt -> true
  | SArray (st, _) -> integer_el_type st

let pp_arg ppf ((adtype, name, st), idx) =
  let adstr =
    match adtype with
    | Ast.DataOnly -> stantype_prim_str st
    | Ast.AutoDiffable -> sprintf "T%d__" idx
  in
  pf ppf "const %a& %s" (pp_unsizedtype adstr) st name

let pp_template_decls ppf =
  pf ppf "@[<hov>template <%a>@]@ " (list ~sep:comma (fmt "typename %s"))

let with_idx lst = List.(zip_exn lst (range 0 (length lst)))

let%expect_test "with idx" =
  print_s [%sexp (with_idx (List.range 10 15) : (int * int) list)] ;
  [%expect {| ((10 0) (11 1) (12 2) (13 3) (14 4)) |}]

let pp_decl ppf (vident, st) =
  pf ppf "%a %s;" pp_prim_stantype (Ast.remove_size st) vident

let with_no_loc stmt = {stmt; sloc= ""}

let pp_located_msg ppf msg =
  pf ppf
    {|stan::lang::rethrow_located(
    std::runtime_error(std::string(%s) + e.what(), current_statement__);
// Next line prevents compiler griping about no return
throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
|}
  @@ Option.value ~default:"e" msg

(** [pp_located_error ppf (body_block, err_msg)] surrounds [body_block]
    with a C++ try-catch that will rethrow the error with the proper source location
    from the [body_block] (required to be a [stmt_loc Block] variant).
*)
let rec pp_located_error ppf (body_block, err_msg) =
  pf ppf "try %a" pp_statement body_block ;
  string ppf " catch const std::exception& e) " ;
  pp_block ppf (pp_located_msg, err_msg)

and pp_statement ppf {stmt; sloc} =
  ( match stmt with
  | Block _ | SList _ | FunDef _ -> ()
  | _ -> pf ppf "current_statement_loc__ = \"%s\";@;" sloc ) ;
  let pp_stmt_list = list ~sep:cut pp_statement in
  match stmt with
  | Assignment (assignee, rhs) ->
      (* XXX completely wrong *)
      pf ppf "%a = %a;" pp_expr assignee pp_expr rhs
  | NRFunApp (fname, args) ->
      pf ppf "%s(%a);" fname (list ~sep:comma pp_expr) args
  | Break -> string ppf "break;"
  | Continue -> string ppf "continue;"
  | Return e -> pf ppf "return %a;" (option pp_expr) e
  | Skip -> ()
  | MarkLocation _ -> () (* XXX *)
  | Check _ -> () (* XXX *)
  | IfElse (cond, ifbranch, elsebranch) ->
      let pp_else ppf x = pf ppf "else %a" pp_statement x in
      pf ppf "if (%a) %a %a" pp_expr cond pp_block (pp_statement, ifbranch)
        (option pp_else) elsebranch
  | While (cond, body) ->
      pf ppf "while (%a) %a" pp_expr cond pp_block (pp_statement, body)
  | For {loopvar; lower; upper; body} ->
      let lv = strf "%a" pp_expr loopvar in
      pp_for_loop ppf (lv, lower, upper, pp_statement, body)
  | Block ls -> pp_block ppf (pp_stmt_list, ls)
  | SList ls -> pp_stmt_list ppf ls
  | Decl {decl_adtype; decl_id; decl_type} ->
      ignore decl_adtype ;
      pp_decl ppf (decl_id, decl_type)
  | FunDef {fdrt; fdname; fdargs; fdbody} -> (
      let argtypetemplates =
        List.mapi ~f:(fun i _ -> sprintf "T%d__" i) fdargs
      in
      pp_template_decls ppf argtypetemplates ;
      pf ppf "%a@ "
        (option ~none:(const string "void")
           (pp_unsizedtype "typename boost::math::tools::promote_args<>::type"))
        fdrt ;
      (* XXX this is all so ugly: *)
      pp_call ppf
        ( fdname
        , (fun ppf (args, extra_arg) ->
            (list ~sep:comma pp_arg) ppf (with_idx args) ;
            pf ppf ",@ %s" extra_arg )
        , (fdargs, "std::ostream* pstream__") ) ;
      pf ppf " " ;
      match fdbody.stmt with
      | Skip -> pf ppf ";@ "
      | _ ->
          pp_block ppf
            ( (fun ppf fdbody ->
                let text = pf ppf "%s@;" in
                pf ppf
                  "@[<hv 8>typedef typename \
                   boost::math::tools::promote_args<%a>::type \
                   local_scalar_t__;@]@ "
                  (list ~sep:comma string) argtypetemplates ;
                text "typedef local_scalar_t__ fun_return_scalar_t__;" ;
                text "const static bool propto__ = true;" ;
                text "(void) propto__;" ;
                text
                  "local_scalar_t__ \
                   DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());" ;
                text "(void) DUMMY_VAR__;  // suppress unused var warning" ;
                text "int current_statement_begin__ = -1;" ;
                pp_located_error ppf (fdbody, None) )
            , fdbody ) )

let%expect_test "location propagates" =
  {sloc= "hi"; stmt= Block [{stmt= Break; sloc= "lo"}]}
  |> strf "@[<v>%a@]" pp_statement
  |> print_endline ;
  [%expect
    {|
      {
        current_statement_loc__ = "lo";
        break;
      } |}]

let%expect_test "if" =
  with_no_loc
    (IfElse
       ( Var "true"
       , with_no_loc (NRFunApp ("print", [Var "x"]))
       , Some
           (with_no_loc (Block [with_no_loc (NRFunApp ("print", [Var "y"]))]))
       ))
  |> strf "@[<v>%a@]" pp_statement
  |> print_endline ;
  [%expect
    {|
    current_statement_loc__ = "";
    if (true) {
      current_statement_loc__ = "";
      print(x);
    } else {
      current_statement_loc__ = "";
      print(y);
    } |}]

let%expect_test "run code per element" =
  let assign ppf x =
    [ Assignment (Var x, Indexed (Var "vals_r__", [Single (Var "pos__++")]))
    ; NRFunApp ("print", [Var x]) ]
    |> List.map ~f:with_no_loc |> Block |> with_no_loc |> pp_statement ppf
  in
  strf "@[<v>%a@]"
    (pp_run_code_per_el assign)
    ("dubvec", SArray (SArray (SMatrix (Var "Y", Var "Z"), Var "X"), Var "W"))
  |> print_endline ;
  [%expect
    {|
    for (size_t i_0__ = 0; i_0__ < W; i_0__++)
      for (size_t i_1__ = 0; i_1__ < X; i_1__++)
        for (size_t i_2__ = 0; i_2__ < Y; i_2__++)
          for (size_t i_3__ = 0; i_3__ < Z; i_3__++)
            {
              current_statement_loc__ = "";
              dubvec[i_0__][i_1__](i_2__, i_3__) = stan::model::rvalue(vals_r__,
                                                       stan::model::cons_list(stan::model::index_uni(pos__++),
                                                       stan::model::nil_index_list()),
                                                       "vals_r__");;
              current_statement_loc__ = "";
              print(dubvec[i_0__][i_1__](i_2__, i_3__));
            } |}]

let%expect_test "decl" =
  Decl {decl_adtype= AutoDiffable; decl_id= "i"; decl_type= SInt}
  |> with_no_loc |> strf "%a" pp_statement |> print_endline ;
  [%expect {|
    current_statement_loc__ = "";
    int i; |}]

let version = "// Code generated by Stan version 2.18.0"
let includes = "#include <stan/model/model_header.hpp>"

let%expect_test "udf" =
  FunDef
    { fdrt= None
    ; fdname= "sars"
    ; fdargs=
        [(Ast.DataOnly, "x", UMatrix); (Ast.AutoDiffable, "y", URowVector)]
    ; fdbody=
        Return (Some (FunApp ("add", [Var "x"; Lit (Int, "1")])))
        |> with_no_loc |> List.return |> Block |> with_no_loc }
  |> with_no_loc
  |> strf "@[<v>%a" pp_statement
  |> print_endline ;
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
        current_statement_loc__ = "";
        return add(x, 1);
      } catch const std::exception& e) {
        stan::lang::rethrow_located(
        std::runtime_error(std::string(e) + e.what(), current_statement__);
    // Next line prevents compiler griping about no return
    throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");

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

let rec pp_zeroing_ctor_call ppf st =
  match st with
  | Ast.SInt | SReal -> string ppf "0"
  | SArray (t, dim) -> pf ppf "%a, %a" pp_expr dim pp_zeroing_ctor_call t
  | SRowVector dim | SVector dim -> ignore dim ; pf ppf "%s" "XXX todo"
  | SMatrix (dim1, dim2) -> ignore (dim1, dim2)

let var_context_container st =
  match basetype (Ast.remove_size st) with "int" -> "vals_i" | _ -> "vals_r"

(* Read in data steps:
   1. context__.validate_dims() (what is this?)
   1. find vals_%s__ from context__.vals_%s(vident)
   1. keep track of pos__
   1. run checks on resulting vident
*)
let pp_read_data ppf (decl_id, st) =
  let vals = var_context_container st in
  let pp_read ppf loopvar = pf ppf "%s = %s;@ " decl_id loopvar in
  pf ppf "%s__ = context__.%s(\"%s\");@ " vals vals decl_id ;
  pp_run_code_per_el pp_read ppf (vals, st)

let%expect_test "read int[N] y" =
  strf "@[<v>%a@]" pp_read_data ("y", Ast.SArray (Ast.SInt, Var "N"))
  |> print_endline ;
  [%expect
    {|
    vals_i__ = context__.vals_i("y");
    for (size_t i_0__ = 0; i_0__ < N; i_0__++) y = vals_i[i_0__]; |}]

(*

let pp_constructor ppf p = pf ppf {|
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
    pp_located_error ((list ~sep:newline pp_read_field),
                        p.datab, Some "Error while reading in data: ")
    pp_located_error ((list ~sep:newline pp_transform),
                        p.datab, Some "Error during transformed data block: ")

let pp_model ppf (p: stmt_loc prog) =
  pf ppf {|
class %s_model : public prob_grad {
@ @[<v 1>
@ private:
@ // Fields here from the data block of the model
@ @[<v 1>%a@]@

@ public:
@ @[<v 1>
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
    pp_statement_loc p.datab
    pp_constructor p
    p.prog_name p.prog_name
(* transform inits
   log_prob
   get_param_names
   get_dims
   write_array
*)

*)

(* XXX Adds declarations.
   How will this mix with all the passes matching on declarations and adding
   new statements for each one, e.g. data reading and checking in the data block?*)
let rec array_to_for sloc (ident, decl_type) bodyfn =
  match decl_type with
  | Ast.SArray (t, dim) ->
      let loopvar = Mir.gensym () in
      let decl =
        Decl {decl_adtype= Ast.DataOnly; decl_id= loopvar; decl_type}
      in
      let body = array_to_for sloc (loopvar, t) bodyfn in
      let sfor = For {loopvar= Var loopvar; lower= zero; upper= dim; body} in
      let stmts = List.map ~f:(fun stmt -> {stmt; sloc}) [decl; sfor] in
      {stmt= SList stmts; sloc}
  | _ -> {stmt= bodyfn ident; sloc}

let trans_checks s =
  match s.stmt with
  | Check {ccfunname; ccvid; cctype; ccargs} ->
      (* XXX The function__ is weird... maybe find a better way to handle it *)
      let ckfn ident =
        NRFunApp ("check_" ^ ccfunname, Var "function__" :: Var ident :: ccargs)
      in
      array_to_for s.sloc (ccvid, cctype) ckfn
  | _ -> s

let%expect_test "trans check" =
  let ck =
    Check
      { ccfunname= "greater_or_equal"
      ; ccvid= "N"
      ; cctype= Ast.SInt
      ; ccargs= [zero] }
  in
  print_s [%sexp ((trans_checks (with_no_loc ck)).stmt : stmt_loc statement)] ;
  [%expect
    {| (NRFunApp check_greater_or_equal ((Var function__) (Var N) (Lit Int 0))) |}]

let decls_of_p {datavars; _} =
  Map.Poly.data datavars |> List.map ~f:tvdecl_to_decl

let pp_ctor ppf p =
  pf ppf
    {|
%s(@[<v 0>stan::io::var_context& context__,
@ unsigned int random_seed__ = 0;
@ std::ostream* pstream__ = nullptr) : prob_grad(0) {@]
@ @[<v 2>
@ typedef double local_scalar_t__;
@ boost::ecuyer1988 base_rng__ =
@     stan::services::util::create_rng(random_seed__, 0);
@ (void) base_rng__;  // suppress unused var warning
@ static const char* function__ = "%s_model_namespace::%s";
@ (void) function__;  // dummy to suppress unused var warning |}
    p.prog_name p.prog_name p.prog_name ;
  pf ppf
    {|
@ @ // Read in data variables
@ %a
@ @ // Transform data variables?
@]
|}
    (list ~sep:cut pp_read_data)
    (decls_of_p p)

let pp_model_private ppf p =
  pf ppf "@ %a" (list ~sep:cut pp_decl) (decls_of_p p)

let pp_model_public ppf p = pf ppf "@ %a" pp_ctor p

(* XXX *)
let pp_model ppf p =
  pf ppf
    {|
  class %s_model : public prob_grad {
@ @[<v 1>
@ private:
@ @[<v 1>%a@]
|}
    p.prog_name pp_model_public p ;
  pf ppf "@ public: @ @[<v 1>@ @ ~%s_model() { }" p.prog_name ;
  pf ppf "@ @ static std::string model_name() { return \"%s\"; }" p.prog_name ;
  pf ppf "%a" pp_model_public p

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

let pp_prog ppf (p : stmt_loc prog) =
  pf ppf "@[<v>@ %s@ %s@ namespace %s_model_namespace {@ %s@ %s@ %a@ %a@ }@ @]"
    version includes p.prog_name globals usings pp_statement p.functionsb
    pp_model p
