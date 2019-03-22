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
open Stanc_mir.Mir


let rec stantype_prim_str = function
  | UInt -> "int"
  | UArray t -> stantype_prim_str t
  | _ -> "double"

let rec pp_unsizedtype_custom_scalar scalar ppf ut =
  match ut with
  | UInt
  | UReal ->
      Fmt.string ppf scalar

  | UArray t ->
      Fmt.pf ppf "std::vector<%a>"
        (pp_unsizedtype_custom_scalar scalar) t

  | UMatrix ->
      Fmt.pf ppf "Eigen::Matrix<%s, -1, -1>"
        scalar

  | URowVector ->
      Fmt.pf ppf "Eigen::Matrix<%s, 1, -1>"
        scalar

  | UVector ->
      Fmt.pf ppf "Eigen::Matrix<%s, -1, 1>" scalar

  | x ->
      raise_s [%message (x : unsizedtype) "not implemented yet"]


let pp_unsizedtype adtype ppf ut =
  let scalar_type =
    match adtype with
    | DataOnly -> stantype_prim_str ut
    | AutoDiffable -> "T__"
    (* XXX Bob - do we ever need different autodiff types per var / fn arg / etc?*)
  in
  pp_unsizedtype_custom_scalar scalar_type ppf ut


let%expect_test "emit function type raises" =
  ( match pp_unsizedtype DataOnly Fmt.stdout UMathLibraryFunction with
  | () -> ()
  | exception e -> print_s [%sexp (e : exn)] ) ;
  [%expect {| ((x UMathLibraryFunction) "not implemented yet") |}]

let pp_call pp_arg ppf (name, args) =
  Fmt.pf ppf "%s(@[<hov>%a@])" 
    name 
    Fmt.(list ~sep:comma pp_arg) args

let pp_call_str  =
  pp_call Fmt.string
  

let rec pp_expr ppf {texpr; _} =
  match texpr with
  | Var s ->
      Fmt.string ppf s

  | Lit (Str, s) ->
      Fmt.pf ppf "%S" s

  | Lit (_, s) ->
      Fmt.string ppf s

  | FunApp (name,args)->
      pp_call pp_expr ppf (name,args)
        

  | TernaryIf (cond, ifb, elseb) ->
      Fmt.pf ppf "(%a) ? (%a) : (%a)"
          pp_expr cond
          pp_expr ifb
          pp_expr elseb
  
  | Indexed (e, idcs) ->
      Fmt.pf ppf "stan::model::rvalue(@[<hov>%a,@,%a,@]@ \"%a\")"
          pp_expr e
          pp_indices idcs
          pp_expr e

and pp_index ppf idx =
  let idx_phrase fmt idtype =
      Fmt.pf ppf fmt ("stan::model::index_" ^ idtype)
  in
  match idx with
  | All ->
      idx_phrase "%s" "omni()"

  | Single e ->
      idx_phrase "%s(%a)" "uni" pp_expr e

  | Upfrom e ->
      idx_phrase "%s(%a)" "min" pp_expr e

  | Downfrom e ->
      idx_phrase "%s(%a)" "max" pp_expr e

  | Between (e1, e2) ->
      idx_phrase "%s(%a, %a)" "min_max" pp_expr e1 pp_expr e2

  | MultiIndex e ->
      idx_phrase "%s(%a)" "multi" pp_expr e


and pp_indices ppf = function
  | [] ->
      Fmt.string ppf "stan::model::nil_index_list()"

  | hd :: tail ->
      Fmt.pf ppf "stan::model::cons_list(%a,@ %a)"
          pp_index hd
          pp_indices tail


let pp_prim_stantype ppf st =
    pp_unsizedtype ppf st


let pp_block ppf (pp_body, body) =
    Fmt.pf ppf "{@;<1 2>@[<v>%a@]@,}" pp_body body

(** [pp_for_loop ppf (loopvar, lower, upper, pp_body, body)] tries to
    pretty print a for-loop from lower to upper given some loopvar.*)
let pp_for_loop ppf (loopvar, lower, upper, pp_body, body) =
  Fmt.pf ppf "@[<hov>for (@[<hov>size_t %s = %a;@ %s < %a;@ %s++@])" 
    loopvar
    pp_expr lower 
    loopvar 
    pp_expr upper 
    loopvar;
  Fmt.pf ppf "@,@;<1 2>@[<v>%a@]@]" 
    pp_body body

(* XXX this is so bad, someone please rethink these concepts for us! I suspect
   the entire function is premised on a bad level of abstraction.
*)
let rec pp_run_code_per_el ?depth:(d = 0) pp_code_per_element ppf (name, st) =
  let size =
    { texpr= FunApp (name ^ ".size", [])
    ; texpr_loc= no_span
    ; texpr_type= UInt
    ; texpr_adlevel= DataOnly }

  and loopvar = sprintf "i_%d__" d in

  let loop_0_to_size per_ele new_vident =
    pp_for_loop ppf (loopvar, zero, size, per_ele, new_vident)

  in
  match st with
  | SInt
  | SReal ->
      Fmt.pf ppf "%a" 
        pp_code_per_element name

  | SVector _
  | SRowVector _
  | SMatrix _ ->
      loop_0_to_size pp_code_per_element (Fmt.strf "%s(%s)" name loopvar)

  | SArray (st, _) ->
      loop_0_to_size
        (pp_run_code_per_el ~depth:(d + 1) pp_code_per_element)
        (Fmt.strf "%s[%s]" name loopvar, st)

let rec integer_el_type = function
  | SReal
  | SVector _
  | SMatrix _
  | SRowVector _ ->
      false

  | SInt ->
      true

  | SArray (st, _) ->
      integer_el_type st

let pp_arg ppf (adtype, name, st) =
  Fmt.pf ppf "const %a& %s"
    (pp_unsizedtype adtype) st
    name

let pp_decl ppf (vident, st, adtype) =
  Fmt.pf ppf "%a %s;"
    (pp_unsizedtype adtype) st
    vident

let with_no_loc stmt =
  { stmt
  ; sloc = no_span
  }

let pp_located_msg ppf msg =
  Fmt.pf ppf
    {|stan::lang::rethrow_located(
          std::runtime_error(std::string(%S) + ": " + e.what(), current_statement__));
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); |}
    msg

let maybe_templated_arg_types (args : fun_arg_decl) =
  let is_autodiff (adtype, _, _) =
    match adtype with
    | AutoDiffable -> true
    | _ -> false

  in
  args
  |> List.filter ~f:is_autodiff
  |> List.mapi ~f:(fun i _ -> sprintf "T%d__" i)

let%expect_test "arg types templated correctly" =
  [(AutoDiffable, "xreal", UReal); (DataOnly, "yint", UInt)]
  |> maybe_templated_arg_types |> String.concat ~sep:"," |> print_endline ;
  [%expect {| T0__ |}]

(** Pretty-prints a function's return-type, taking into account templated
    argument promotion.
*)
let pp_returntype ppf arg_types rt =
  let scalar =
    Fmt.strf "typename boost::math::tools::promote_args<@[<-35>%a@]>::type"
      Fmt.(list ~sep:comma string)
      (maybe_templated_arg_types arg_types)
  in
  Fmt.pf ppf "%a@ "
    Fmt.(option ~none:(const string "void") 
          (pp_unsizedtype_custom_scalar scalar)
        )
    rt



(* XXX Duplicated from `Errors` for refactoring *)
let rec string_of_location loc =
  let open Format in
  let included_from_str =
    match loc.included_from with
    | None -> ""
    | Some loc2 -> sprintf ", included from\n%s" (string_of_location loc2)
  in
  sprintf "file %s, line %d, column %d%s" loc.filename loc.line_num loc.col_num
    included_from_str

(* XXX Duplicated from `Errors` for refactoring *)
let string_of_location_span loc_sp =
  match loc_sp with {begin_loc; end_loc} ->
    let bf = begin_loc.filename in
    let ef = end_loc.filename in
    let bl = begin_loc.line_num in
    let el = end_loc.line_num in
    let bc = begin_loc.col_num in
    let ec = end_loc.col_num in
    let open Format in
    let file_line_col_string =
      if bf = ef then
        sprintf "file %s, %s" bf
          ( if bl = el then
            sprintf "line %d, %s" bl
              ( if bc = ec then sprintf "column %d" bc
              else sprintf "columns %d-%d" bc ec )
          else sprintf "line %d, column %d to line %d, column %d" bl bc el ec
          )
      else
        sprintf "file %s, line %d, column %d to file %s, line %d, column %d" bf
          bl bc ef el ec
    in
    let included_from_str =
      match begin_loc.included_from with
      | None -> ""
      | Some loc -> sprintf ", included from\n%s" (string_of_location loc)
    in
    sprintf "%s%s" file_line_col_string included_from_str

let pp_location ppf loc =
  Fmt.pf ppf "current_statement__ = %S;@;"
    (string_of_location_span loc)

(** [pp_located_error ppf (pp_body_block, body_block, err_msg)] surrounds [body_block]
    with a C++ try-catch that will rethrow the error with the proper source location
    from the [body_block] (required to be a [stmt_loc Block] variant).*)
let pp_located_error ppf (pp_body_block, body, err_msg) =
  Fmt.pf ppf "@ try %a" pp_body_block body;
  (* XXX Figure out a good way to refactor this so it doesn't require a body block. *)
  Fmt.string ppf " catch (const std::exception& e) ";
  pp_block ppf (pp_located_msg, err_msg)

let trans_math_fn fname =
  match fname with
  | "print" -> ("stan_print", [{internal_expr with texpr= Var "pstream__"}])
  | x -> (x, [])

let rec pp_statement ppf {stmt; sloc} =
  ( match stmt with
  | Block _ | SList _ | FunDef _ | Break | Continue | Skip -> ()
  | _ -> pp_location ppf sloc
  );
  let pp_stmt_list = Fmt.(list ~sep:cut pp_statement) in
  match stmt with
  | Assignment (assignee, rhs) ->
      (* XXX completely wrong *)
      Fmt.pf ppf "@[<hov 4>%a =@;%a;@]"
        pp_expr assignee
        pp_expr rhs

  | TargetPE e ->
      Fmt.pf ppf "lp_accum__.add(%a)"
        pp_expr e

  | NRFunApp (fname, args) ->
      let fname, extra_args = trans_math_fn fname
      in
      Fmt.pf ppf "%s(@[<hov>%a@]);"
        fname
        Fmt.(list ~sep:comma pp_expr)
        (extra_args @ args)

  | Check (fname, args) ->
      let args = {internal_expr with texpr= Var "function__"} :: args
      in
      pp_statement ppf {stmt= NRFunApp (fname, args); sloc}

  | Break ->
      Fmt.string ppf "break;"

  | Continue ->
      Fmt.string ppf "continue;"

  | Return e ->
      Fmt.pf ppf "return %a;"
        (Fmt.option pp_expr) e

  | Skip ->
      ()

  | IfElse (cond, ifbranch, elsebranch) ->
      let pp_else ppf x =
        Fmt.pf ppf "else %a"
          pp_statement x
      in
      Fmt.pf ppf "if (@[<hov>%a@]) %a %a"
        pp_expr cond
        pp_block (pp_statement, ifbranch)
        (Fmt.option pp_else) elsebranch

  | While (cond, body) ->
      Fmt.pf ppf "while (@[<hov>%a@]) %a"
        pp_expr cond
        pp_block (pp_statement, body)

  | For {loopvar; lower; upper; body} ->
      pp_for_loop ppf (loopvar, lower, upper, pp_statement, body)

  | Block ls ->
    pp_block ppf (pp_stmt_list, ls)

  | SList ls ->
    pp_stmt_list ppf ls

  | Decl {decl_adtype; decl_id; decl_type} ->
      pp_decl ppf (decl_id, decl_type, decl_adtype)

  | FunDef {fdrt; fdname; fdargs; fdbody} -> (
      let argtypetemplates =
        List.mapi ~f:(fun i _ -> sprintf "T%d__" i) fdargs
      in
      (* Print template line*)
      Fmt.pf ppf "@[<hov>template <%a>@]@ "
        (Fmt.list ~sep:Fmt.comma (Fmt.fmt "typename %s"))
        argtypetemplates;
      (* print return type *)
      pp_returntype ppf fdargs fdrt;
      (* XXX this is all so ugly: *)
      Fmt.pf ppf "%s(@[<hov>%a"
        fdname
        Fmt.(list ~sep:comma pp_arg) fdargs;
      Fmt.string ppf ", std::ostream* pstream__";
      Fmt.pf ppf "@]) ";
      match fdbody.stmt with
      | Skip ->
          Fmt.char ppf ';';
          Fmt.sp ppf ();

      | _ ->
          pp_block ppf
            ( (fun ppf fdbody ->
                let text = Fmt.pf ppf "%s@;" in

                Fmt.pf ppf
                  "@[<hv 8>typedef typename \
                   boost::math::tools::promote_args<%a>::type \
                   local_scalar_t__;@]@,"
                  Fmt.(list ~sep:comma string) argtypetemplates;

                text "typedef local_scalar_t__ fun_return_scalar_t__;";

                text "const static bool propto__ = true;";

                text "(void) propto__;";

                text
                  "local_scalar_t__ \
                   DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());";

                text "(void) DUMMY_VAR__;  // suppress unused var warning";

                pp_located_error ppf
                  (pp_statement, fdbody, "inside UDF " ^ fdname) )
            , fdbody
            );

          Fmt.pf ppf "@ "
    )

let%expect_test "location propagates" =
  let loc1 = {no_span with begin_loc= {no_loc with filename= "HI"}}
  and loc2 = {no_span with begin_loc= {no_loc with filename= "LO"}} in
  {sloc= loc1; stmt= Block [{stmt= NRFunApp ("print", []); sloc= loc2}]}
  |> Fmt.strf "@[<v>%a@]" pp_statement
  |> print_endline;
  [%expect
    {|
    {
      current_statement__ = "file LO, line 0, column 0 to file , line 0, column 0";
      stan_print(pstream__);
    } |}]
let version = "// Code generated by Stan version 2.18.0"
let includes = "#include <stan/model/model_header.hpp>"

let rec basetype = function
  | UInt -> "int"
  | UReal -> "double"
  | UArray t -> basetype t
  | UMatrix -> "matrix_d"
  | URowVector -> "row_vector_d"
  | UVector -> "vector_d"
  | x -> raise_s [%message "basetype not defined for " (x : unsizedtype)]

let rec pp_zeroing_ctor_call ppf st =
  match st with
  | SInt
  | SReal ->
      Fmt.string ppf "0"

  | SArray (t, dim) ->
      Fmt.pf ppf "%a, %a"
        pp_expr dim
        pp_zeroing_ctor_call t

  | SRowVector dim
  | SVector dim ->
      ignore dim;
      Fmt.pf ppf "%s" "XXX todo"

  | SMatrix (dim1, dim2) ->
      ignore (dim1, dim2)

let var_context_container st =
  if basetype (remove_size st) = "int" then
    "vals_i"
  else
    "vals_r"

let pp_set_size ppf decl_id st =
  let rec pp_size_ctor ppf st =
    let pp_st ppf st =
      Fmt.pf ppf "%a" 
        (pp_unsizedtype DataOnly) (remove_size st)
    in
    match st with
    | SInt
    | SReal ->
        Fmt.string ppf "0"

    | SVector d
    | SRowVector d ->
        Fmt.pf ppf "%a(%a)"
          pp_st st
          pp_expr d

    | SMatrix (d1, d2) ->
        Fmt.pf ppf "%a(%a, %a)"
          pp_st st
          pp_expr d1
          pp_expr d2

    | SArray (t, d) ->
        Fmt.pf ppf "%a(%a, %a)"
          pp_st st
          pp_expr d
          pp_size_ctor t
  in
  match st with
  | SInt
  | SReal -> ()
  | st ->
      Fmt.pf ppf "%s = %a;@,"
        decl_id
        pp_size_ctor st

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
  pp_location ppf loc;
  let vals = var_context_container st ^ "__" in
  let pp_read ppf decl_id = Fmt.pf ppf "%s = %s;" decl_id vals in
  Fmt.pf ppf "%s = context__.%s(\"%s\");@;" vals vals decl_id;
  pp_set_size ppf decl_id st;
  pp_run_code_per_el pp_read ppf (decl_id, st);
  Fmt.pf ppf "@;"

let%expect_test "read int[N] y" =
  Fmt.strf "@[<v>%a@]" pp_read_data
    ("y", SArray (SInt, {internal_expr with texpr= Var "N"}), no_span)
  |> print_endline;
  [%expect
    {|
    current_statement__ = "file , line 0, column 0";
    vals_i__ = context__.vals_i__("y");
    y = std::vector<int>(N, 0);
    for (size_t i_0__ = 0; i_0__ < y.size(); i_0__++) y[i_0__] = vals_i__; |}]

let data_decls_of_p {data_vars; _} =
  data_vars
  |> Map.Poly.data
  |> List.map ~f:tvdecl_to_decl

let pp_read_and_check_decls ppf p =
  Fmt.pf ppf "//Read data variables in@,";
  Fmt.(list ~sep:cut pp_read_data ppf (data_decls_of_p p));
  Fmt.(list ~sep:cut pp_statement ppf p.prepare_data)

let block_of_list s =
  match s.stmt with
  | SList ls ->
    { s with stmt = Block ls }

  | _ -> s

let pp_ctor ppf p =
  (* XXX:
     1. Set num_params_r__
  *)
  let params =
    [ "stan::io::var_context& context__"
    ; "unsigned int random_seed__ = 0"
    ; "std::ostream* pstream__ = nullptr"
    ]
  in
  Fmt.pf ppf "%s(@[<hov 0>%a) : prob_grad(0) @]"
    p.prog_name
    Fmt.(list ~sep:comma string) params;

  pp_block ppf
    ( (fun ppf p ->
        Fmt.pf ppf "typedef double local_scalar_t__;";
        Fmt.pf ppf "@ boost::ecuyer1988 base_rng__ = ";
        Fmt.pf ppf "@     stan::services::util::create_rng(random_seed__, 0);";
        Fmt.pf ppf "@ (void) base_rng__;  // suppress unused var warning";
        Fmt.pf ppf "@ static const char* function__ = \"%s_namespace::%s\";"
          p.prog_name p.prog_name;
        Fmt.pf ppf "@ (void) function__;  // dummy to suppress unused var warning";
        pp_located_error ppf
          ( (fun ppf p -> pp_block ppf (pp_read_and_check_decls, p))
          , p
          , "inside ctor" ) )
    , p )

let pp_model_private ppf p =
  data_decls_of_p p
  |> List.map
    ~f:(fun (x,y,_) ->
        (x, remove_size y, DataOnly)
      )
  |> Fmt.pf ppf "%a"
      Fmt.(list ~sep:cut pp_decl)
  (* pf ppf "%a" (list ~sep:cut pp_decl)
    (List.map
       ~f:(fun (x, y, _) -> (x, remove_size y, DataOnly))
       (data_decls_of_p p)) *)

let pp_get_param_names ppf p =
  let param_names =
    List.concat_map ~f:Map.Poly.keys [p.params; p.tparams]
    |> List.sort ~compare:String.compare
  in
  let add_param = Fmt.fmt "names.push_back(%S);"
  in
  Fmt.pf ppf "@[<v 2>void %a const {@,%a@]@,}"
    pp_call_str ("get_param_names", ["std::vector<std::string>& names"])
    Fmt.(list ~sep:cut add_param) param_names

let rec get_dims = function
  | SInt
  | SReal -> []
  | SVector d
  | SRowVector d -> [d]
  | SMatrix (dim1, dim2) -> [dim1; dim2]
  | SArray (t, dim) -> dim :: get_dims t

let%expect_test "dims" =
  let v s = {internal_expr with texpr= Var s} in
  Fmt.(strf "@[%a@]" (list ~sep:comma pp_expr))
    (get_dims (SArray (SMatrix (v "x", v "y"), v "z")))
  |> print_endline;
  [%expect {| z, x, y |}]

let get_all_params {params; tparams; _} =
  List.concat_map ~f:Map.Poly.data [params; tparams]

let (>>) f g x = g (f x)

let get_all_param_types p =
  get_all_params p
  |> List.map ~f:(tvdecl_to_decl >> fun (_, t, _) -> t)

let pp_get_dims ppf p =
  let pp_dim ppf dim =
    Fmt.pf ppf "dims__.push_back(%a);@," 
      pp_expr dim
  and pp_dim_sep ppf () =
    Fmt.pf ppf "dimss__.push_back(dims__);@,dims__.resize(0);@,"
  and params =
    ["std::vector<std::vector<size_t>>& dimss__"]
  in
  Fmt.pf ppf "@[<v 2>void %a const " pp_call_str ("get_dims", params);
  Fmt.pf ppf "{@,dimss__.resize(0);@,std::vector<size_t> dims__;@,%a%a@]@,}"
    Fmt.(list ~sep:pp_dim_sep (list ~sep:cut pp_dim))
    (List.map ~f:get_dims (get_all_param_types p))
    pp_dim_sep ()

let pp_write_array ppf p =
  ignore p;
  Fmt.string ppf "//TODO write_array"

let pp_constrained_param_names ppf p =
  ignore p;
  Fmt.string ppf "//TODO constrained_param_names"

let pp_unconstrained_param_names ppf p =
  ignore p;
  Fmt.string ppf "//TODO unconstrained_param_names"

let pp_transformed_params ppf tparams =
  ignore tparams;
  Fmt.string ppf "//TODO"

let pp_transformed_param_checks ppf params =
  ignore params;
  Fmt.string ppf "//TODO"

let pp_transform_inits ppf params =
  ignore params;
  Fmt.string ppf "//TODO transform_inits"

let pp_fndef_sig ppf (rt, fname, params) =
  Fmt.pf ppf "%s %s(@[<hov>%a@])" 
    rt 
    fname 
    Fmt.(list ~sep:comma string) params

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
  let text = Fmt.pf ppf "%s@,"
  and params =
    [ "std::vector<T__>& params_r__"; "std::vector<int>& params_i__"
    ; "std::ostream* pstream__ = 0" ]
  in
  text "template <bool propto__, bool jacobian__, typename T__>";
  Fmt.pf ppf "T__ %a" pp_call_str ("log_prob", params);
  Fmt.pf ppf " {@,@[<v 2>";
  text "typedef T__ local_scalar_t__;";
  text "local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());";
  text "(void) DUMMY_VAR__;  // dummy to suppress unused var warning";
  text "T__ lp__(0.0);";
  text "stan::math::accumulator<T__> lp_accum__;";
  text "stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);";
  Fmt.pf ppf "//TODO: Unpack parameters with reader and constrain@,";
  (* XXX Eventually we can create a separate prepare_params method? *)
  pp_located_error ppf
    ( pp_statement
    , {stmt= Block p.prepare_params; sloc= no_span}
    , "inside prepare_params"
    );
  Fmt.pf ppf "//Transformed parameters@,%a@," pp_transformed_params p.tparams;
  pp_located_error ppf
    (pp_statement, {stmt= Block p.log_prob; sloc= no_span}, "inside log_prob");
  Fmt.pf ppf "@]@,}@,"

let pp_model_public ppf p =
  Fmt.pf ppf "@ %a" pp_ctor p ;
  Fmt.pf ppf "@ %a" pp_log_prob p ;
  Fmt.pf ppf "@ %a" pp_get_param_names p ;
  Fmt.pf ppf "@ %a" pp_get_dims p ;
  Fmt.pf ppf "@ %a" pp_write_array p ;
  Fmt.pf ppf "@ %a" pp_constrained_param_names p ;
  Fmt.pf ppf "@ %a" pp_unconstrained_param_names p ;
  Fmt.pf ppf "@ %a" pp_transform_inits p

let pp_model ppf p =
  Fmt.pf ppf "class %s : public prob_grad {" p.prog_name ;
  Fmt.pf ppf "@ @[<v 1>@ private:@ @[<v 1> %a@]@ " pp_model_private p ;
  Fmt.pf ppf "@ public:@ @[<v 1> ~%s() { }" p.prog_name ;
  Fmt.pf ppf "@ @ static std::string model_name() { return \"%s\"; }" p.prog_name ;
  Fmt.pf ppf "@ %a@]@]@ }" pp_model_public p

let globals =
  "static char* current_statement__;"

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
  Fmt.pf ppf "@[<v>@ %s@ %s@ namespace %s_namespace {@ %s@ %s@ %a@ %a@ }@ @]"
    version includes p.prog_name usings globals
    Fmt.(list ~sep:cut pp_statement)
    p.functions_block pp_model p ;
  Fmt.pf ppf "@,typedef %snamespace::%s stan_model;@," p.prog_name p.prog_name

let%expect_test "udf" =
  let w e = {internal_expr with texpr= e} in
  FunDef
    { fdrt= None
    ; fdname= "sars"
    ; fdargs=
        [(DataOnly, "x", UMatrix); (AutoDiffable, "y", URowVector)]
    ; fdbody=
        Return
          (Some (w @@ FunApp ("add", [w @@ Var "x"; w @@ Lit (Int, "1")])))
        |> with_no_loc |> List.return |> Block |> with_no_loc }
  |> with_no_loc
  |> Fmt.strf "@[<v>%a" pp_statement
  |> print_endline ;
  [%expect
    {|
    template <typename T0__, typename T1__>
    void
    sars(const Eigen::Matrix<double, -1, -1>& x,
         const Eigen::Matrix<T__, 1, -1>& y, std::ostream* pstream__) {
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
