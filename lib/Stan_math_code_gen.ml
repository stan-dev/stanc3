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

let pp_operator = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Modulo -> "%"
  | Or -> "||"
  | And -> "&&"
  | Equals -> "=="
  | NEquals -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

let zero = Lit (Int, "0")

let rec pp_unsizedtype scalar_type ppf = function
  | Ast.UInt | UReal -> string ppf scalar_type
  | UArray t -> pf ppf "std::vector<%a>" (pp_unsizedtype scalar_type) t
  | UMatrix -> pf ppf "Eigen::Matrix<%s, -1, -1>" scalar_type
  | URowVector -> pf ppf "Eigen::Matrix<%s, 1, -1>" scalar_type
  | UVector -> pf ppf "Eigen::Matrix<%s, -1, 1>" scalar_type
  | x -> raise_s [%message (x : unsizedtype) "not implemented yet"]

let%expect_test "emit function type raises" =
  ( match pp_unsizedtype "sars" stdout Ast.UMathLibraryFunction with
  | () -> ()
  | exception e -> print_s [%sexp (e : exn)] ) ;
  [%expect {| ((x UMathLibraryFunction) "not implemented yet") |}]

let pp_call ppf (name, pp_arg, args) =
  pf ppf "%s(@[<hov>%a@])" name (list ~sep:comma pp_arg) args

let pp_call_str ppf (name, args) = pp_call ppf (name, string, args)

let rec pp_expr ppf s =
  match s with
  | Var s -> string ppf s
  | Lit (Str, s) -> pf ppf "%S" s
  | Lit (_, s) -> string ppf s
  | FunApp (fname, args) -> pp_call ppf (fname, pp_expr, args)
  | BinOp (e1, op, e2) ->
      pf ppf "%a %s %a" pp_expr e1 (pp_operator op) pp_expr e2
  | TernaryIf (cond, ifb, elseb) ->
      pf ppf "(%a) ? (%a) : (%a)" pp_expr cond pp_expr ifb pp_expr elseb
  | Indexed (e, idcs) ->
      pf ppf "stan::model::rvalue(@[<hov>%a,@,%a,@]@ \"%a\")" pp_expr e
        pp_indices idcs pp_expr e

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
                        stan::model::nil_index_list())),
    "vec") |}]

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

(* XXX this is so bad, someone please rethink these concepts for us! I suspect
   the entire function is premised on a bad level of abstraction.
*)
let rec pp_run_code_per_el ?depth:(d = 0) pp_code_per_element ppf (name, st) =
  let size = FunApp (name ^ ".size", []) in
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
  pf ppf "%a@ " (option ~none:(const string "void") (pp_unsizedtype scalar)) rt

let pp_location = fmt "current_statement__ = %S;@;"

(** [pp_located_error ppf (pp_body_block, body_block, err_msg)] surrounds [body_block]
    with a C++ try-catch that will rethrow the error with the proper source location
    from the [body_block] (required to be a [stmt_loc Block] variant).*)
let pp_located_error ppf (pp_body_block, body, err_msg) =
  pf ppf "@ try %a" pp_body_block body ;
  (* XXX Figure out a good way to refactor this so it doesn't require a body block. *)
  string ppf " catch (const std::exception& e) " ;
  pp_block ppf (pp_located_msg, err_msg)

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
      pf ppf "@[<hov 4>%a =@;%a;@]" pp_expr assignee pp_expr rhs
  | TargetPE e -> pf ppf "lp_accum__.add(%a)" pp_expr e
  | NRFunApp (fname, args) ->
      let fname, extra_args = trans_math_fn fname in
      pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr)
        (extra_args @ args)
  | Check (fname, args) ->
      pp_statement ppf {stmt= NRFunApp (fname, Var "function__" :: args); sloc}
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
      pf ppf "%s(@[<hov>%a" fdname (list ~sep:comma pp_arg) (with_idx fdargs) ;
      pf ppf ", std::ostream* pstream__" ;
      pf ppf "@]) " ;
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
                (* XXX Need to figure out how to pass in appropriate template arguments
                   to pp_statement or something such that local variable declarations get the right
                   scalar type w.r.t. autodiff/template (var vs double scalar type).
                *)
                pp_located_error ppf
                  (pp_statement, fdbody, "inside UDF " ^ fdname) )
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
              std::runtime_error(std::string("inside UDF sars") + ": " + e.what(), current_statement__));
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

let pp_set_size ppf decl_id st =
  let rec pp_size_ctor ppf st =
    let pp_st ppf st = pf ppf "%a" pp_prim_stantype (Ast.remove_size st) in
    match st with
    | Ast.SInt | SReal -> pf ppf "0"
    | SVector d | SRowVector d -> pf ppf "%a(%a)" pp_st st pp_expr d
    | SMatrix (d1, d2) -> pf ppf "%a(%a, %a)" pp_st st pp_expr d1 pp_expr d2
    | SArray (t, d) -> pf ppf "%a(%a, %a)" pp_st st pp_expr d pp_size_ctor t
  in
  match st with
  | Ast.SInt | SReal -> ()
  | st -> pf ppf "%s = %a;@," decl_id pp_size_ctor st

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
  pp_set_size ppf decl_id st ;
  pp_run_code_per_el pp_read ppf (decl_id, st) ;
  pf ppf "@;"

let%expect_test "read int[N] y" =
  strf "@[<v>%a@]" pp_read_data ("y", Ast.SArray (Ast.SInt, Var "N"), "")
  |> print_endline ;
  [%expect
    {|
    current_statement__ = "";
    vals_i__ = context__.vals_i__("y");
    y = std::vector<int>(N, 0);
    for (size_t i_0__ = 0; i_0__ < y.size(); i_0__++) y[i_0__] = vals_i__; |}]

let data_decls_of_p {data_vars; _} =
  Map.Poly.data data_vars |> List.map ~f:tvdecl_to_decl

let pp_read_and_check_decls ppf p =
  pf ppf "//Read data variables in@," ;
  list ~sep:cut pp_read_data ppf (data_decls_of_p p) ;
  list ~sep:cut pp_statement ppf p.prepare_data

let block_of_list s =
  {s with stmt= (match s.stmt with SList ls -> Block ls | x -> x)}

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
        pp_located_error ppf
          ( (fun ppf p -> pp_block ppf (pp_read_and_check_decls, p))
          , p
          , "inside ctor" ) )
    , p )

let pp_model_private ppf p =
  pf ppf "%a" (list ~sep:cut pp_decl)
    (List.map ~f:(fun (x, y, _) -> (x, y)) (data_decls_of_p p))

let pp_get_param_names ppf p =
  let param_names =
    List.map ~f:Map.Poly.keys [p.params; p.tparams]
    |> List.concat
    |> List.sort ~compare:String.compare
  in
  let add_param = fmt "names.push_back(%S);" in
  pf ppf "@[<v 2>void %a const {@,%a@]@,}" pp_call_str
    ("get_param_names", ["std::vector<std::string>& names"])
    (list ~sep:cut add_param) param_names

let rec get_dims = function
  | Ast.SInt | Ast.SReal -> []
  | Ast.SVector d | Ast.SRowVector d -> [d]
  | Ast.SMatrix (dim1, dim2) -> [dim1; dim2]
  | Ast.SArray (t, dim) -> dim :: get_dims t

let%expect_test "dims" =
  strf "@[%a@]" (list ~sep:comma pp_expr)
    (get_dims (Ast.SArray (Ast.SMatrix (Var "x", Var "y"), Var "z")))
  |> print_endline ;
  [%expect {| z, x, y |}]

let get_all_params {params; tparams; _} =
  List.(concat (map ~f:Map.Poly.data [params; tparams]))

let get_all_param_types p =
  get_all_params p |> List.map ~f:tvdecl_to_decl
  |> List.map ~f:(fun (_, t, _) -> t)

let pp_get_dims ppf p =
  let pp_dim ppf dim = pf ppf "dims__.push_back(%a);@," pp_expr dim in
  let pp_dim_sep ppf () =
    pf ppf "dimss__.push_back(dims__);@,dims__.resize(0);@,"
  in
  let params = ["std::vector<std::vector<size_t>>& dimss__"] in
  pf ppf "@[<v 2>void %a const " pp_call_str ("get_dims", params) ;
  pf ppf "{@,dimss__.resize(0);@,std::vector<size_t> dims__;@,%a%a@]@,}"
    (list ~sep:pp_dim_sep (list ~sep:cut pp_dim))
    (List.map ~f:get_dims (get_all_param_types p))
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

let pp_transformed_params ppf tparams = ignore tparams ; string ppf "//TODO"

let pp_transformed_param_checks ppf params =
  ignore params ; string ppf "//TODO"

let pp_transform_inits ppf params =
  ignore params ;
  string ppf "//TODO transform_inits"

let pp_fndef_sig ppf (rt, fname, params) =
  pf ppf "%s %s(@[<hov>%a@])" rt fname (list ~sep:comma string) params

(* constraining parameters for before log prob and write_array
  match tvtrans with
  | Ast.Identity -> None
  | Lower lb -> constrain for_scalar "lb_constrain" [lb]
  | Upper ub -> constrain for_scalar "ub_constrain" [ub]
  | LowerUpper (lb, ub) -> constrain for_scalar "lub_constrain" [lb; ub]
  | Offset o -> constrain for_scalar "offset_multiplier" [o; Lit (Int, "1")]
  | Multiplier m -> constrain for_scalar "offset_multiplier" [Lit (Int, "0"); m]
  | OffsetMultiplier (o, m) -> constrain for_scalar "offset_multiplier" [o; m]
  | Ordered -> constrain for_eigen "ordered" []
  | PositiveOrdered | Simplex | UnitVector -> constrain for_eigen "ordered" []
  |CholeskyCorr -> constrain for_eigen "cholesky_factor_corr" []
  | CholeskyCov | Correlation | Covariance ->
      None

*)

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
  pf ppf "//TODO: Unpack parameters with reader and constrain@," ;
  (* XXX Eventually we can create a separate prepare_params method? *)
  pp_located_error ppf
    ( pp_statement
    , {stmt= Block p.prepare_params; sloc= ""}
    , "inside prepare_params" ) ;
  pf ppf "//Transformed parameters@,%a@," pp_transformed_params p.tparams ;
  pp_located_error ppf
    (pp_statement, {stmt= Block p.log_prob; sloc= ""}, "inside log_prob") ;
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

let pp_model ppf p =
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

let pp_prog ppf (p : stmt_loc prog) =
  pf ppf "@[<v>@ %s@ %s@ namespace %s_namespace {@ %s@ %s@ %a@ %a@ }@ @]"
    version includes p.prog_name usings globals
    (list ~sep:cut pp_statement)
    p.functions_block pp_model p ;
  pf ppf "@,typedef %snamespace::%s stan_model;@," p.prog_name p.prog_name
