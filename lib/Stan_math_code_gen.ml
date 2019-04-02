(** Generate C++ from the MIR.

    This module makes extensive use of the Format[0] module via the Fmt[1] API.
    As such, you'll need to understand the "%a" and "@" notation from [0], especially
    the section headed "Formatted pretty-printing." Then, we use functions like
    [pf] and [strf] from the Fmt library[1]. On top of that, the "@" pretty-printing
    specifiers all actually correspond 1-to-1 with commands like [open_box] from
    the Format library[0]. The boxing system is best described in this explainer
    pdf [2], particularly Section 3 ("Format basics"). It's worth noting that someone
    was able to make a decent-looking pretty-printer for a subset of Javascript[3] that
    might serve as a good reference. Good luck!

    [0] Format module doc: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
    [1] Fmt module doc: https://erratique.ch/software/fmt/doc/Fmt.html
    [2] Format Unraveled: https://hal.archives-ouvertes.fr/hal-01503081/file/format-unraveled.pdf
    [3] Javascript pretty-printer https://github.com/Virum/compiler/blob/28e807b842bab5dcf11460c8193dd5b16674951f/JavaScript.ml#L112
*)

open Core_kernel
open Mir
open Fmt
open Expression_gen

let pp_call ppf (name, pp_arg, args) =
  pf ppf "%s(@[<hov>%a@])" name (list ~sep:comma pp_arg) args

let pp_call_str ppf (name, args) = pp_call ppf (name, string, args)
let pp_block ppf (pp_body, body) = pf ppf "{@;<1 2>@[<v>%a@]@,}" pp_body body

let pp_set_size ppf (decl_id, st, adtype) =
  (* TODO: generate optimal adtypes for expressions and declarations *)
  ignore adtype ;
  let rec pp_size_ctor ppf st =
    let pp_st ppf st =
      pf ppf "%a" pp_unsizedtype_local (adtype, Ast.remove_size st)
    in
    match st with
    | Ast.SInt | SReal -> pf ppf "0"
    | SVector d | SRowVector d -> pf ppf "%a(%a)" pp_st st pp_expr d
    | SMatrix (d1, d2) -> pf ppf "%a(%a, %a)" pp_st st pp_expr d1 pp_expr d2
    | SArray (t, d) -> pf ppf "%a(%a, %a)" pp_st st pp_expr d pp_size_ctor t
  in
  match st with
  | Ast.SInt | SReal -> ()
  | st -> pf ppf "%s = %a;@," decl_id pp_size_ctor st

let%expect_test "set size mat array" =
  let int i = {internal_expr with texpr= Lit (Int, string_of_int i)} in
  strf "@[<v>%a@]" pp_set_size
    ("d", SArray (SArray (SMatrix (int 2, int 3), int 4), int 5), DataOnly)
  |> print_endline ;
  [%expect
    {| d = std::vector<std::vector<Eigen::Matrix<double, -1, -1>>>(5, std::vector<Eigen::Matrix<double, -1, -1>>(4, Eigen::Matrix<double, -1, -1>(2, 3))); |}]

(** [pp_for_loop ppf (loopvar, lower, upper, pp_body, body)] tries to
    pretty print a for-loop from lower to upper given some loopvar.*)
let pp_for_loop ppf (loopvar, lower, upper, pp_body, body) =
  pf ppf "@[<hov>for (@[<hov>size_t %s = %a;@ %s < %a;@ %s++@])" loopvar
    pp_expr lower loopvar pp_expr upper loopvar ;
  pf ppf "@,@;<1 2>@[<v>%a@]@]" pp_body body

(* XXX this is so bad, someone please rethink these concepts for us! I suspect
   the entire function is premised on a bad level of abstraction.
*)
let rec pp_run_code_per_el ?depth:(d = 0) pp_code_per_element ppf (name, st) =
  let size =
    { texpr= FunApp (fn_length, [{internal_expr with texpr= Var name}])
    ; texpr_loc= no_span
    ; texpr_type= UInt
    ; texpr_adlevel= DataOnly }
  in
  let loopvar = sprintf "i_%d__" d in
  let loop_0_to_size per_ele new_vident =
    pp_for_loop ppf (loopvar, zero, size, per_ele, new_vident)
  in
  match st with
  | Ast.SInt | SReal -> pf ppf "%a" pp_code_per_element name
  | SVector _ | SRowVector _ | SMatrix _ ->
      loop_0_to_size pp_code_per_element (strf "%s(%s)" name loopvar)
  | SArray (st, _) ->
      loop_0_to_size
        (pp_run_code_per_el ~depth:(d + 1) pp_code_per_element)
        (strf "%s[%s]" name loopvar, st)

let rec integer_el_type = function
  | Ast.SReal | SVector _ | SMatrix _ | SRowVector _ -> false
  | SInt -> true
  | SArray (st, _) -> integer_el_type st

let pp_decl ppf (vident, ut, adtype) =
  pf ppf "%a %s;" pp_unsizedtype_local (adtype, ut) vident

let pp_sized_decl ppf (vident, st, adtype) =
  pf ppf "%a@,%a" pp_decl
    (vident, Ast.remove_size st, adtype)
    pp_set_size (vident, st, adtype)

let with_no_loc stmt = {stmt; sloc= no_span}

let pp_located_msg ppf msg =
  pf ppf
    {|stan::lang::rethrow_located(
          std::runtime_error(std::string(%S) + ": " + e.what(), current_statement__));
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); |}
    msg

let maybe_templated_arg_types (args : fun_arg_decl) =
  let is_autodiff (adtype, _, _) =
    match adtype with Ast.AutoDiffable -> true | _ -> false
  in
  List.filter ~f:is_autodiff args |> List.mapi ~f:(fun i _ -> sprintf "T%d__" i)

let%expect_test "arg types templated correctly" =
  [(Ast.AutoDiffable, "xreal", Ast.UReal); (Ast.DataOnly, "yint", Ast.UInt)]
  |> maybe_templated_arg_types |> String.concat ~sep:"," |> print_endline ;
  [%expect {| T0__ |}]

(** Pretty-prints a function's return-type, taking into account templated argument
    promotion.*)
let pp_returntype ppf arg_types rt =
  let scalar =
    strf "typename boost::math::tools::promote_args<@[<-35>%a@]>::type"
      (list ~sep:comma string)
      (maybe_templated_arg_types arg_types)
  in
  match rt with
  | Some ut -> pf ppf "%a@," pp_unsizedtype_custom_scalar (scalar, ut)
  | None -> pf ppf "void@,"

let pp_location ppf loc =
  pf ppf "current_statement__ = %S;@;" (Errors.string_of_location_span loc)

(** [pp_located_error ppf (pp_body_block, body_block, err_msg)] surrounds [body_block]
    with a C++ try-catch that will rethrow the error with the proper source location
    from the [body_block] (required to be a [stmt_loc Block] variant).*)
let pp_located_error ppf (pp_body_block, body, err_msg) =
  pf ppf "@ try %a" pp_body_block body ;
  (* XXX Figure out a good way to refactor this so it doesn't require a body block. *)
  string ppf " catch (const std::exception& e) " ;
  pp_block ppf (pp_located_msg, err_msg)

let math_fn_translations =
  Map.Poly.of_alist_exn
    [ (fn_print, ("stan_print", [{internal_expr with texpr= Var "pstream__"}]))
    ; (fn_length, ("length", [])) ]

let trans_math_fn fname =
  match Map.Poly.find math_fn_translations fname with
  | Some x -> x
  | None -> (fname, [])

let pp_arg ppf (custom_scalar, (_, name, ut)) =
  pf ppf "const %a& %s" pp_unsizedtype_custom_scalar (custom_scalar, ut) name

let rec pp_statement ppf {stmt; sloc} =
  ( match stmt with
  | Block _ | FunDef _ | Break | Continue | Skip -> ()
  | _ -> pp_location ppf sloc ) ;
  let pp_stmt_list = list ~sep:cut pp_statement in
  match stmt with
  | Assignment (assignee, rhs) ->
      (* XXX completely wrong *)
      pf ppf "@[<hov 4>%a =@;%a;@]" pp_expr assignee pp_expr rhs
  | TargetPE e -> pf ppf "lp_accum__.add(%a)" pp_expr e
  | NRFunApp (fname, {texpr= Lit (Str, check_name); _} :: args)
    when fname = fn_check ->
      let args = {internal_expr with texpr= Var "function__"} :: args in
      pp_statement ppf {stmt= NRFunApp ("check_" ^ check_name, args); sloc}
  | NRFunApp (fname, args) ->
      let fname, extra_args = trans_math_fn fname in
      pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr)
        (extra_args @ args)
  | Break -> string ppf "break;"
  | Continue -> string ppf "continue;"
  | Return e -> pf ppf "return %a;" (option pp_expr) e
  | Skip -> ()
  | IfElse (cond, ifbranch, elsebranch) ->
      let pp_else ppf x = pf ppf "else %a" pp_statement x in
      pf ppf "if (@[<hov>%a@]) %a %a" pp_expr cond pp_block
        (pp_statement, ifbranch) (option pp_else) elsebranch
  | While (cond, body) ->
      pf ppf "while (@[<hov>%a@]) %a" pp_expr cond pp_block (pp_statement, body)
  | For {loopvar; lower; upper; body} ->
      pp_for_loop ppf (loopvar, lower, upper, pp_statement, body)
  | Block ls -> pp_block ppf (pp_stmt_list, ls)
  | SList ls -> pp_stmt_list ppf ls
  | Decl {decl_adtype; decl_id; decl_type} ->
      pp_sized_decl ppf (decl_id, decl_type, decl_adtype)
  | FunDef {fdrt; fdname; fdargs; fdbody} -> (
      let argtypetemplates =
        List.mapi ~f:(fun i _ -> sprintf "T%d__" i) fdargs
      in
      pf ppf "@[<hov>template <%a>@]@ "
        (list ~sep:comma (fmt "typename %s"))
        argtypetemplates ;
      pp_returntype ppf fdargs fdrt ;
      pf ppf "%s(@[<hov>%a" fdname (list ~sep:comma pp_arg)
        (List.zip_exn argtypetemplates fdargs) ;
      pf ppf ", std::ostream* pstream__@]) " ;
      match fdbody.stmt with
      | Skip -> pf ppf ";@ "
      | _ ->
          pp_block ppf
            ( (fun ppf fdbody ->
                let text = pf ppf "%s@;" in
                pf ppf
                  "@[<hv 8>typedef typename \
                   boost::math::tools::promote_args<%a>::type \
                   local_scalar_t__;@]@,"
                  (list ~sep:comma string) argtypetemplates ;
                text "typedef local_scalar_t__ fun_return_scalar_t__;" ;
                text "const static bool propto__ = true;" ;
                text "(void) propto__;" ;
                text
                  "local_scalar_t__ \
                   DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());" ;
                text "(void) DUMMY_VAR__;  // suppress unused var warning" ;
                pp_located_error ppf
                  (pp_statement, fdbody, "inside UDF " ^ fdname) )
            , fdbody ) ;
          pf ppf "@ " )

let%expect_test "location propagates" =
  let loc1 = {no_span with begin_loc= {no_loc with filename= "HI"}} in
  let loc2 = {no_span with begin_loc= {no_loc with filename= "LO"}} in
  {sloc= loc1; stmt= Block [{stmt= NRFunApp (fn_print, []); sloc= loc2}]}
  |> strf "@[<v>%a@]" pp_statement
  |> print_endline ;
  [%expect
    {|
    {
      current_statement__ = "file LO, line 0, column 0 to file , line 0, column 0";
      stan_print(pstream__);
    } |}]

let pp_statements = list ~sep:cut pp_statement
let version = "// Code generated by Stan version 2.18.0"
let includes = "#include <stan/model/model_header.hpp>"

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
let pp_read_data ppf (decl_id, st, loc) =
  (* XXX:
     * Add stuff like
       context__.validate_dims("data initialization", "J", "int", context__.to_vec());
     * Add validate_non_negative_index("sigma", "J", J);
  *)
  pp_location ppf loc ;
  let vals = var_context_container st ^ "__" in
  let pp_read ppf decl_id = pf ppf "%s = %s;" decl_id vals in
  pf ppf "%s = context__.%s(\"%s\");@;" vals vals decl_id ;
  pp_set_size ppf (decl_id, st, DataOnly) ;
  pp_run_code_per_el pp_read ppf (decl_id, st) ;
  pf ppf "@;"

let%expect_test "read int[N] y" =
  strf "@[<v>%a@]" pp_read_data
    ("y", Ast.SArray (Ast.SInt, {internal_expr with texpr= Var "N"}), no_span)
  |> print_endline ;
  [%expect
    {|
    current_statement__ = "file , line 0, column 0";
    vals_i__ = context__.vals_i__("y");
    y = std::vector<int>(N, 0);
    for (size_t i_0__ = 0; i_0__ < length(y); i_0__++) y[i_0__] = vals_i__; |}]

let pp_ctor ppf p =
  (* XXX:
     1. Set num_params_r__
  *)
  let params =
    [ "stan::io::var_context& context__"; "unsigned int random_seed__ = 0"
    ; "std::ostream* pstream__ = nullptr" ]
  in
  pf ppf "%s(@[<hov 0>%a) : prob_grad(0) @]" p.prog_name
    (list ~sep:comma string) params ;
  pp_block ppf
    ( (fun ppf p ->
        pf ppf "typedef double local_scalar_t__;" ;
        pf ppf "@ boost::ecuyer1988 base_rng__ = " ;
        pf ppf "@     stan::services::util::create_rng(random_seed__, 0);" ;
        pf ppf "@ (void) base_rng__;  // suppress unused var warning" ;
        pf ppf "@ static const char* function__ = \"%s_namespace::%s\";"
          p.prog_name p.prog_name ;
        pf ppf "@ (void) function__;  // dummy to suppress unused var warning" ;
        pp_located_error ppf (pp_statements, p.prepare_data, "inside ctor") )
    , p )

let pp_model_private ppf p =
  let is_data decl_id = List.Assoc.mem ~equal:( = ) p.input_vars decl_id in
  let return_decl = function
    | {stmt= Decl {decl_type; decl_id; _}; _} when is_data decl_id ->
        Some (decl_id, Ast.remove_size decl_type, Ast.DataOnly)
    | _ -> None
  in
  let data_decls = List.filter_map ~f:return_decl p.prepare_data in
  pf ppf "%a" (list ~sep:cut pp_decl) data_decls

let pp_get_param_names ppf p =
  let add_param = fmt "names.push_back(%S);" in
  pf ppf "@[<v 2>void %a const {@,%a@]@,}" pp_call_str
    ("get_param_names", ["std::vector<std::string>& names"])
    (list ~sep:cut add_param)
    (List.map ~f:fst p.output_vars)

let rec get_dims = function
  | Ast.SInt | Ast.SReal -> []
  | Ast.SVector d | Ast.SRowVector d -> [d]
  | Ast.SMatrix (dim1, dim2) -> [dim1; dim2]
  | Ast.SArray (t, dim) -> dim :: get_dims t

let%expect_test "dims" =
  let v s = {internal_expr with texpr= Var s} in
  strf "@[%a@]" (list ~sep:comma pp_expr)
    (get_dims (Ast.SArray (Ast.SMatrix (v "x", v "y"), v "z")))
  |> print_endline ;
  [%expect {| z, x, y |}]

let pp_get_dims ppf p =
  let pp_dim ppf dim = pf ppf "dims__.push_back(%a);@," pp_expr dim in
  let pp_dim_sep ppf () =
    pf ppf "dimss__.push_back(dims__);@,dims__.resize(0);@,"
  in
  let params = ["std::vector<std::vector<size_t>>& dimss__"] in
  pf ppf "@[<v 2>void %a const " pp_call_str ("get_dims", params) ;
  pf ppf "{@,dimss__.resize(0);@,std::vector<size_t> dims__;@,%a%a@]@,}"
    (list ~sep:pp_dim_sep (list ~sep:cut pp_dim))
    List.(map ~f:get_dims (map ~f:(fun (_, (st, _)) -> st) p.output_vars))
    pp_dim_sep ()

let pp_write_array ppf p =
  ignore p ;
  string ppf "//TODO write_array"

let pp_constrained_param_names ppf p =
  ignore p ;
  string ppf "//TODO constrained_param_names"

let pp_unconstrained_param_names ppf p =
  ignore p ;
  string ppf "//TODO unconstrained_param_names"

let pp_transform_inits ppf params =
  ignore params ;
  string ppf "//TODO transform_inits"

let pp_fndef_sig ppf (rt, fname, params) =
  pf ppf "%s %s(@[<hov>%a@])" rt fname (list ~sep:comma string) params

let pp_log_prob ppf p =
  let text = pf ppf "%s@," in
  text "template <bool propto__, bool jacobian__, typename T__>" ;
  let params =
    [ "std::vector<T__>& params_r__"; "std::vector<int>& params_i__"
    ; "std::ostream* pstream__ = 0" ]
  in
  pf ppf "T__ %a" pp_call_str ("log_prob", params) ;
  pf ppf " {@,@[<v 2>" ;
  text "typedef T__ local_scalar_t__;" ;
  text
    "local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());" ;
  text "(void) DUMMY_VAR__;  // dummy to suppress unused var warning" ;
  text "T__ lp__(0.0);" ;
  text "stan::math::accumulator<T__> lp_accum__;" ;
  text "stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);" ;
  pp_located_error ppf
    (pp_statement, {stmt= Block p.log_prob; sloc= no_span}, "inside log_prob") ;
  pf ppf "@]@,}@,"

let pp_model_public ppf p =
  pf ppf "@ %a" pp_ctor p ;
  pf ppf "@ %a" pp_log_prob p ;
  pf ppf "@ %a" pp_get_param_names p ;
  pf ppf "@ %a" pp_get_dims p ;
  pf ppf "@ %a" pp_write_array p ;
  pf ppf "@ %a" pp_constrained_param_names p ;
  pf ppf "@ %a" pp_unconstrained_param_names p ;
  pf ppf "@ %a" pp_transform_inits p

let pp_model ppf (p : typed_prog) =
  pf ppf "class %s : public prob_grad {" p.prog_name ;
  pf ppf "@ @[<v 1>@ private:@ @[<v 1> %a@]@ " pp_model_private p ;
  pf ppf "@ public:@ @[<v 1> ~%s() { }" p.prog_name ;
  pf ppf "@ @ static std::string model_name() { return \"%s\"; }" p.prog_name ;
  pf ppf "@ %a@]@]@ }" pp_model_public p

let globals = "static char* current_statement__;"

let usings =
  {|
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math; |}

let pp_prog ppf (p : (expr_typed_located, stmt_loc) prog) =
  pf ppf "@[<v>@ %s@ %s@ namespace %s_namespace {@ %s@ %s@ %a@ %a@ }@ @]"
    version includes p.prog_name usings globals
    (list ~sep:cut pp_statement)
    p.functions_block pp_model p ;
  pf ppf "@,typedef %snamespace::%s stan_model;@," p.prog_name p.prog_name

(* XXX arg templating is broken - needs T0, T1 etc in arg decl*)
let%expect_test "udf" =
  let w e = {internal_expr with texpr= e} in
  FunDef
    { fdrt= None
    ; fdname= "sars"
    ; fdargs=
        [(Ast.DataOnly, "x", UMatrix); (Ast.AutoDiffable, "y", URowVector)]
    ; fdbody=
        Return
          (Some (w @@ FunApp ("add", [w @@ Var "x"; w @@ Lit (Int, "1")])))
        |> with_no_loc |> List.return |> Block |> with_no_loc }
  |> with_no_loc
  |> strf "@[<v>%a" pp_statement
  |> print_endline ;
  [%expect
    {|
    template <typename T0__, typename T1__>
    void
    sars(const Eigen::Matrix<T0__, -1, -1>& x,
         const Eigen::Matrix<T1__, 1, -1>& y, std::ostream* pstream__) {
      typedef typename boost::math::tools::promote_args<T0__,
              T1__>::type local_scalar_t__;
      typedef local_scalar_t__ fun_return_scalar_t__;
      const static bool propto__ = true;
      (void) propto__;
      local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
      (void) DUMMY_VAR__;  // suppress unused var warning

      try {
        current_statement__ = "file , line 0, column 0";
        return add(x, 1);
      } catch (const std::exception& e) {
        stan::lang::rethrow_located(
              std::runtime_error(std::string("inside UDF sars") + ": " + e.what(), current_statement__));
          // Next line prevents compiler griping about no return
          throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
      }
    } |}]
