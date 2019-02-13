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
  | Lit (Str, s) -> pf ppf "%S" s
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

(** [pp_for_loop ppf (loopvar, lower, upper, pp_body, body)] tries to
    pretty print a for-loop from lower to upper given some loopvar.*)
let pp_for_loop ppf (loopvar, lower, upper, pp_body, body) =
  pf ppf "@[<hov>for (@[<hov>size_t %s = %a;@ %s < %a;@ %s++@])" loopvar
    pp_expr lower loopvar pp_expr upper loopvar ;
  pf ppf "@,@;<1 2>@[<v>%a@]@]" pp_body body

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
          std::runtime_error(std::string(%s) + e.what(), current_statement__));
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
|}
  @@ Option.value ~default:"e" msg

let maybe_templated_arg_types (args : formal_params) =
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
  pf ppf "%a@ "
    (option ~none:(const string "void")
       (pp_unsizedtype
          (strf "typename boost::math::tools::promote_args<@[<-35>%a@]>::type"
             (list ~sep:comma string)
             (maybe_templated_arg_types arg_types))))
    rt

let pp_location ppf = pf ppf "current_statement__ = %S;@;"

(** [pp_located_error ppf (body_block, err_msg)] surrounds [body_block]
    with a C++ try-catch that will rethrow the error with the proper source location
    from the [body_block] (required to be a [stmt_loc Block] variant).
*)
let pp_located_error ppf (pp_body_block, body, err_msg) =
  pf ppf "@ try %a" pp_body_block body ;
  string ppf " catch (const std::exception& e) " ;
  pp_block ppf (pp_located_msg, err_msg)

let rec pp_for_each_in_array ppf (cctype, pp_body, ident) =
  match cctype with
  | Ast.SArray (t, dim) ->
      let loopvar, reset_sym = Mir.gensym_enter () in
      let new_ident = strf "%s[%s]" ident loopvar in
      let new_pp_body ppf ident =
        pp_for_loop ppf (loopvar, zero, dim, pp_body, ident)
      in
      pp_for_each_in_array ppf (t, new_pp_body, new_ident) ;
      reset_sym ()
  | _ -> pp_body ppf ident

let%expect_test "single pp_for_each_in_array" =
  let ppbody ppf id = pf ppf "%s++" id in
  strf "%a" pp_for_each_in_array (Ast.SReal, ppbody, "x") |> print_endline ;
  [%expect {| x++ |}]

let%expect_test "for each in array" =
  let ppbody ppf id = pf ppf "check_whatever(%s);" id in
  strf "%a" pp_for_each_in_array
    (Ast.SArray (Ast.SArray (Ast.SReal, Var "z"), Var "y"), ppbody, "alpha")
  |> print_endline ;
  [%expect
    {|
    for (size_t sym2 = 0; sym2 < z; sym2++)
      for (size_t sym1 = 0; sym1 < y; sym1++) check_whatever(alpha[sym1][sym2]); |}]

let trans_math_fn fname =
  match fname with "print" -> ("stan_print", [Var "pstream__"]) | x -> (x, [])

let rec pp_statement ppf {stmt; sloc} =
  ( match stmt with
  | Block _ | SList _ | FunDef _ | Break | Continue | Skip -> ()
  | _ -> pp_location ppf sloc ) ;
  let pp_stmt_list = list ~sep:cut pp_statement in
  match stmt with
  | Assignment (assignee, rhs) ->
      (* XXX completely wrong *)
      pf ppf "%a = %a;" pp_expr assignee pp_expr rhs
  | NRFunApp (fname, args) ->
      let fname, extra_args = trans_math_fn fname in
      pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr)
        (extra_args @ args)
  | Break -> string ppf "break;"
  | Continue -> string ppf "continue;"
  | Return e -> pf ppf "return %a;" (option pp_expr) e
  | Skip -> ()
  | Check {ccfunname; ccvid; ccargs; cctype} ->
      let pp_ckfn ppf ident =
        pf ppf "check_%s(@[<hov>%a@]);" ccfunname (list ~sep:comma pp_expr)
          (Var "function__" :: Var ident :: ccargs)
      in
      pp_for_each_in_array ppf (cctype, pp_ckfn, ccvid)
  | IfElse (cond, ifbranch, elsebranch) ->
      let pp_else ppf x = pf ppf "else %a" pp_statement x in
      pf ppf "if (@[<hov>%a@]) %a %a" pp_expr cond pp_block
        (pp_statement, ifbranch) (option pp_else) elsebranch
  | While (cond, body) ->
      pf ppf "while (@[<hov>%a@]) %a" pp_expr cond pp_block (pp_statement, body)
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
      (* Print template line*)
      pf ppf "@[<hov>template <%a>@]@ "
        (list ~sep:comma (fmt "typename %s"))
        argtypetemplates ;
      (* print return type *)
      pp_returntype ppf fdargs fdrt ;
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
                   local_scalar_t__;@]@,"
                  (list ~sep:comma string) argtypetemplates ;
                text "typedef local_scalar_t__ fun_return_scalar_t__;" ;
                text "const static bool propto__ = true;" ;
                text "(void) propto__;" ;
                text
                  "local_scalar_t__ \
                   DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());" ;
                text "(void) DUMMY_VAR__;  // suppress unused var warning" ;
                pp_located_error ppf (pp_statement, fdbody, None) )
            , fdbody ) ;
          pf ppf "@ " )

let%expect_test "location propagates" =
  {sloc= "hi"; stmt= Block [{stmt= NRFunApp ("print", []); sloc= "lo"}]}
  |> strf "@[<v>%a@]" pp_statement
  |> print_endline ;
  [%expect
    {|
      {
        current_statement__ = "lo";
        stan_print(pstream__);
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
    current_statement__ = "";
    if (true) {
      current_statement__ = "";
      stan_print(pstream__, x);
    } else {
      current_statement__ = "";
      stan_print(pstream__, y);
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
              current_statement__ = "";
              dubvec[i_0__][i_1__](i_2__, i_3__) = stan::model::rvalue(vals_r__,
                                                       stan::model::cons_list(stan::model::index_uni(pos__++),
                                                       stan::model::nil_index_list()),
                                                       "vals_r__");;
              current_statement__ = "";
              stan_print(pstream__, dubvec[i_0__][i_1__](i_2__, i_3__));
            } |}]

let%expect_test "decl" =
  Decl {decl_adtype= AutoDiffable; decl_id= "i"; decl_type= SInt}
  |> with_no_loc |> strf "%a" pp_statement |> print_endline ;
  [%expect {|
    current_statement__ = "";
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

      try {
        current_statement__ = "";
        return add(x, 1);
      } catch (const std::exception& e) {
        stan::lang::rethrow_located(
              std::runtime_error(std::string(e) + e.what(), current_statement__));
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
let pp_read_data ppf (decl_id, st, loc) =
  pp_location ppf loc ;
  let vals = var_context_container st ^ "__" in
  let pp_read ppf loopvar = pf ppf "%s = %s;" decl_id loopvar in
  pf ppf "%s = context__.%s(\"%s\");@;" vals vals decl_id ;
  pp_run_code_per_el pp_read ppf (vals, st) ;
  pf ppf "@;"

let%expect_test "read int[N] y" =
  strf "@[<v>%a@]" pp_read_data ("y", Ast.SArray (Ast.SInt, Var "N"), "")
  |> print_endline ;
  [%expect
    {|
    current_statement__ = "";
    vals_i__ = context__.vals_i__("y");
    for (size_t i_0__ = 0; i_0__ < N; i_0__++) y = vals_i__[i_0__]; |}]

let decls_of_p {datavars; _} =
  Map.Poly.data datavars |> List.map ~f:tvdecl_to_decl

let pp_read_and_check_decls ppf p =
  list ~sep:cut pp_read_data ppf (decls_of_p p) ;
  pp_statement ppf (snd p.tdatab)

let pp_ctor ppf p =
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
        pf ppf "@ static const char* function__ = \"%s_model_namespace::%s\";"
          p.prog_name p.prog_name ;
        pf ppf "@ (void) function__;  // dummy to suppress unused var warning" ;
        pp_located_error ppf
          ((fun ppf p -> pp_block ppf (pp_read_and_check_decls, p)), p, None)
        )
    , p )

let pp_model_private ppf p =
  pf ppf "%a" (list ~sep:cut pp_decl)
    (List.map ~f:(fun (x, y, _) -> (x, y)) (decls_of_p p))

(* XXX *)
let pp_model_public ppf p = pf ppf "@ %a" pp_ctor p

let pp_model ppf p =
  pf ppf "class %s_model : public prob_grad {" p.prog_name ;
  pf ppf "@ @[<v 1>@ private:@ @[<v 1> %a@]@ " pp_model_private p ;
  pf ppf "@ public:@ @[<v 1> ~%s_model() { }" p.prog_name ;
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
using namespace stan::math;
|}

let pp_prog ppf (p : stmt_loc prog) =
  pf ppf "@[<v>@ %s@ %s@ namespace %s_namespace {@ %s@ %s@ %a@ %a@ }@ @]"
    version includes p.prog_name globals usings pp_statement p.functionsb
    pp_model p
