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
open Middle
open Fmt
open Expression_gen
open Statement_gen

let pp_unused = fmt "(void) %s;  // suppress unused var warning@ "

let pp_function__ ppf (prog_name, fname) =
  pf ppf "static const char* function__ = %S;@ "
    (strf "%s_namespace::%s" prog_name fname) ;
  pp_unused ppf "function__"

let pp_located_msg ppf msg =
  pf ppf
    {|stan::lang::rethrow_located(
          std::runtime_error(std::string(%S) + ": " + e.what()), locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); |}
    msg

let maybe_templated_arg_types (args : fun_arg_decl) =
  let is_autodiff (adtype, _, _) =
    match adtype with AutoDiffable -> true | _ -> false
  in
  List.filter ~f:is_autodiff args |> List.mapi ~f:(fun i _ -> sprintf "T%d__" i)

let%expect_test "arg types templated correctly" =
  [(AutoDiffable, "xreal", UReal); (DataOnly, "yint", UInt)]
  |> maybe_templated_arg_types |> String.concat ~sep:"," |> print_endline ;
  [%expect {| T0__ |}]

let promoted_arg_type ppf argtypetemplates =
  match argtypetemplates with
  | [] -> pf ppf "double"
  | [a] -> pf ppf "%s" a
  | a ->
      let rec promote_args_chunked ppf args =
        let go ppf tl =
          match tl with
          | _ :: _ -> pf ppf ", %a" promote_args_chunked tl
          | _ -> ()
        in
        pf ppf "typename boost::math::tools::promote_args<%a%a>::type"
          (list ~sep:comma string) (List.hd_exn args) go (List.tl_exn args)
      in
      promote_args_chunked ppf (List.chunks_of ~length:5 a)

(** Pretty-prints a function's return-type, taking into account templated argument
    promotion.*)
let pp_returntype ppf arg_types rt =
  let scalar =
    strf "%a" promoted_arg_type (maybe_templated_arg_types arg_types)
  in
  match rt with
  | Some ut -> pf ppf "%a@," pp_unsizedtype_custom_scalar (scalar, ut)
  | None -> pf ppf "void@,"

(** [pp_located_error ppf (pp_body_block, body_block, err_msg)] surrounds [body_block]
    with a C++ try-catch that will rethrow the error with the proper source location
    from the [body_block] (required to be a [stmt_loc Block] variant).*)
let pp_located_error ppf (pp_body_block, body, err_msg) =
  pf ppf "@ try %a" pp_body_block body ;
  string ppf " catch (const std::exception& e) " ;
  pp_block ppf (pp_located_msg, err_msg)

let pp_arg ppf (custom_scalar, (_, name, ut)) =
  pf ppf "const %a& %s" pp_unsizedtype_custom_scalar (custom_scalar, ut) name

(** [pp_located_error_b] automatically adds a Block wrapper *)
let pp_located_error_b ppf (body_stmts, err_msg) =
  pp_located_error ppf
    ( pp_statement
    , {stmt= Block body_stmts; smeta= Locations.no_span_num}
    , err_msg )

(* XXX refactor this please - one idea might be to have different functions for
   printing user defined distributions vs rngs vs regular functions.
*)
let pp_fun_def ppf {fdrt; fdname; fdargs; fdbody; _} =
  let is_dist = is_user_dist fdname in
  let is_rng = String.is_suffix fdname ~suffix:"_rng" in
  let extra = if is_dist then ["lp__"; "lp_accum__"] else [] in
  let prefix_extra_templates, prefix_extra_args =
    if is_rng then (["RNG"], ["base_rng__"]) else ([], [])
  in
  let argtypetemplates =
    (* TODO: If one contains ints, we don't need to template it *)
    List.mapi ~f:(fun i _ -> sprintf "T%d__" i) fdargs
  in
  let extra_templates = List.map ~f:(( ^ ) "T_") extra in
  let mk_extra_args templates args =
    List.map ~f:(fun (t, v) -> t ^ "& " ^ v) (List.zip_exn templates args)
  in
  let pp_body ppf fdbody =
    let text = pf ppf "%s@;" in
    pf ppf "@[<hv 8>using local_scalar_t__ = %a;@]@," promoted_arg_type
      argtypetemplates ;
    text "typedef local_scalar_t__ fun_return_scalar_t__;" ;
    (* needed?*)
    if not is_dist then (
      text "const static bool propto__ = true;" ;
      text "(void) propto__;" ) ;
    text
      "local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());" ;
    pp_unused ppf "DUMMY_VAR__" ;
    pp_located_error ppf (pp_statement, fdbody, "inside UDF " ^ fdname) ;
    pf ppf "@ "
  in
  let templates =
    (if is_dist then ["bool propto__"] else [])
    @ List.map ~f:(( ^ ) "typename ")
        (prefix_extra_templates @ argtypetemplates @ extra_templates)
  in
  let pp_sig ppf name =
    ( match templates with
    | [] -> ()
    | a -> pf ppf "@[<hov>template <%a>@]@ " (list ~sep:comma string) a ) ;
    pp_returntype ppf fdargs fdrt ;
    let arg_strs =
      mk_extra_args prefix_extra_templates prefix_extra_args
      @ List.map
          ~f:(fun a -> strf "%a" pp_arg a)
          (List.zip_exn argtypetemplates fdargs)
      @ mk_extra_args extra_templates extra
      @ ["std::ostream* pstream__"]
    in
    pf ppf "%s(@[<hov>%a@]) " name (list ~sep:comma string) arg_strs
  in
  pp_sig ppf fdname ;
  match fdbody.stmt with
  | Skip -> pf ppf ";@ "
  | _ ->
      pp_block ppf (pp_body, fdbody) ;
      pf ppf "@,@,struct %s_functor__ {@,%a const @,{@,return %a;@,}@,};@,"
        fdname pp_sig "operator()" pp_call_str
        ( fdname
        , prefix_extra_args
          @ List.map ~f:(fun (_, name, _) -> name) fdargs
          @ extra @ ["pstream__"] )

let version = "// Code generated by %%NAME%% %%VERSION%%"
let includes = "#include <stan/model/model_header.hpp>"

let rec get_dims = function
  | SInt | SReal -> []
  | SVector d | SRowVector d -> [d]
  | SMatrix (dim1, dim2) -> [dim1; dim2]
  | SArray (t, dim) -> dim :: get_dims t

let%expect_test "dims" =
  let v s = {expr= Var s; emeta= internal_meta} in
  strf "@[%a@]" (list ~sep:comma pp_expr)
    (get_dims (SArray (SMatrix (v "x", v "y"), v "z")))
  |> print_endline ;
  [%expect {| z, x, y |}]

(* Read in data steps:
   1. context__.validate_dims() (what is this?)
   1. find vals_%s__ from context__.vals_%s(vident)
   1. keep track of pos__
   1. run checks on resulting vident
*)

let pp_ctor ppf (p : Locations.typed_prog_num) =
  let params =
    [ "stan::io::var_context& context__"; "unsigned int random_seed__ = 0"
    ; "std::ostream* pstream__ = nullptr" ]
  in
  pf ppf "%s(@[<hov 0>%a) : model_base_crtp(0) @]" p.prog_name
    (list ~sep:comma string) params ;
  let pp_mul ppf () = pf ppf " * " in
  let pp_num_param ppf dims =
    pf ppf "num_params_r__ += %a;" (list ~sep:pp_mul pp_expr) dims
  in
  let get_param_st = function
    | _, {out_block= Parameters; out_unconstrained_st= st; _} -> (
      match get_dims st with [] -> Some [loop_bottom] | ls -> Some ls )
    | _ -> None
  in
  let pp_stmt_topdecl_size_only ppf s =
    match s.stmt with
    | Decl {decl_id; decl_type; _} -> (
      match decl_type with
      | Sized st -> pp_set_size ppf (decl_id, st, DataOnly)
      | Unsized _ -> () )
    | _ -> pp_statement ppf s
  in
  pp_block ppf
    ( (fun ppf p ->
        pf ppf "typedef double local_scalar_t__;@ " ;
        pf ppf "boost::ecuyer1988 base_rng__ = @ " ;
        pf ppf "    stan::services::util::create_rng(random_seed__, 0);@ " ;
        pp_unused ppf "base_rng__" ;
        pp_function__ ppf (p.prog_name, p.prog_name) ;
        pp_located_error ppf
          ( pp_block
          , (list ~sep:cut pp_stmt_topdecl_size_only, p.prepare_data)
          , "inside ctor" ) ;
        cut ppf () ;
        pf ppf "num_params_r__ = 0U;@ " ;
        pf ppf "%a@ "
          (list ~sep:cut pp_num_param)
          (List.filter_map ~f:get_param_st p.output_vars) )
    , p )

let pp_model_private ppf p =
  let decl = function
    | {stmt= Decl d; _} ->
        Some (d.decl_id, remove_possible_size d.decl_type, DataOnly)
    | _ -> None
  in
  let data_decls = List.filter_map ~f:decl p.prepare_data in
  pf ppf "%a" (list ~sep:cut pp_decl) data_decls

let pp_method ppf rt name params intro ?(outro = []) ppbody =
  pf ppf "@[<v 2>%s %a const " rt pp_call_str (name, params) ;
  pf ppf "{@,%a" (list ~sep:cut string) intro ;
  pf ppf "@ " ;
  ppbody ppf ;
  if not (List.is_empty outro) then pf ppf "@ %a" (list ~sep:cut string) outro ;
  pf ppf "@,} // %s() @,@]" name

let pp_get_param_names ppf p =
  let add_param = fmt "names__.push_back(%S);" in
  pp_method ppf "void" "get_param_names" ["std::vector<std::string>& names__"]
    [] (fun ppf ->
      pf ppf "names__.resize(0);@ " ;
      (list ~sep:cut add_param) ppf (List.map ~f:fst p.output_vars) )

let pp_get_dims ppf p =
  let pp_dim ppf dim = pf ppf "dims__.push_back(%a);@," pp_expr dim in
  let pp_dim_sep ppf () =
    pf ppf "dimss__.push_back(dims__);@,dims__.resize(0);@,"
  in
  let pp_output_var ppf =
    (list ~sep:pp_dim_sep (list ~sep:cut pp_dim))
      ppf
      List.(
        map ~f:get_dims
          (map ~f:(fun (_, {out_constrained_st= st; _}) -> st) p.output_vars))
  in
  let params = ["std::vector<std::vector<size_t>>& dimss__"] in
  pp_method ppf "void" "get_dims" params
    ["dimss__.resize(0);"; "std::vector<size_t> dims__;"] (fun ppf ->
      pp_output_var ppf ; pp_dim_sep ppf () )

let pp_method_b ppf rt name params intro ?(outro = []) body =
  pp_method ppf rt name params intro
    (fun ppf -> pp_located_error_b ppf (body, "inside " ^ name))
    ~outro

let pp_write_array ppf p =
  pf ppf "template <typename RNG>@ " ;
  let params =
    [ "RNG& base_rng__"; "std::vector<double>& params_r__"
    ; "std::vector<int>& params_i__"; "std::vector<double>& vars__"
    ; "bool emit_transformed_parameters__ = true"
    ; "bool emit_generated_quantities__ = true"; "std::ostream* pstream__ = 0"
    ]
  in
  let intro =
    [ "typedef double local_scalar_t__;"; "vars__.resize(0);"
    ; "stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);"
    ; strf "%a" pp_function__ (p.prog_name, "write_array")
    ; strf "%a" pp_unused "function__"
    ; "double lp__ = 0.0;"
    ; "(void) lp__;  // dummy to suppress unused var warning"
    ; "stan::math::accumulator<double> lp_accum__;" ]
  in
  pp_method_b ppf "void" "write_array" params intro p.generate_quantities

let rec pp_for_loop_iteratee ?(index_ids = []) ppf (iteratee, dims, pp_body) =
  let iter d pp_body =
    let loopvar, gensym_exit = gensym_enter () in
    pp_for_loop ppf
      ( loopvar
      , loop_bottom
      , d
      , pp_block
      , (pp_body, (iteratee, loopvar :: index_ids)) ) ;
    gensym_exit ()
  in
  match dims with
  | [] -> pp_body ppf (iteratee, index_ids)
  | dim :: dims ->
      iter dim (fun ppf (i, idcs) ->
          pf ppf "%a" pp_block
            (pp_for_loop_iteratee ~index_ids:idcs, (i, dims, pp_body)) )

let pp_constrained_param_names ppf p =
  let params =
    [ "std::vector<std::string>& param_names__"
    ; "bool emit_transformed_parameters__ = true"
    ; "bool emit_generated_quantities__ = true" ]
  in
  let paramvars, tparamvars, gqvars =
    List.partition3_map
      ~f:(function
        | id, {out_block= Parameters; out_constrained_st= st; _} ->
            `Fst (id, st)
        | id, {out_block= TransformedParameters; out_constrained_st= st; _} ->
            `Snd (id, st)
        | id, {out_block= GeneratedQuantities; out_constrained_st= st; _} ->
            `Trd (id, st))
      p.output_vars
  in
  let emit_name ppf (name, idcs) =
    let to_string = fmt "std::to_string(%s)" in
    pf ppf "param_names__.push_back(std::string() + %a);"
      (list ~sep:(fun ppf () -> pf ppf " + '.' + ") string)
      (strf "%S" name :: List.map ~f:(strf "%a" to_string) idcs)
  in
  let pp_param_names ppf (decl_id, st) =
    let dims = List.rev (get_dims st) in
    pp_for_loop_iteratee ppf (decl_id, dims, emit_name)
  in
  pp_method ppf "void" "constrained_param_names" params [] (fun ppf ->
      (list ~sep:cut pp_param_names) ppf paramvars ;
      pf ppf "@,if (emit_transformed_parameters__) %a@," pp_block
        (list ~sep:cut pp_param_names, tparamvars) ;
      pf ppf "@,if (emit_generated_quantities__) %a@," pp_block
        (list ~sep:cut pp_param_names, gqvars) )

(* XXX This is just a copy of constrained, I need to figure out which one is wrong
   and fix it eventually. From Bob,

   Off the top of my head, I think the four that change sizes
   from constrained to unconstrained are:

   simplex:  K -> (K - 1)
   covar_matrix:  K^2 -> (K choose 2) + K
   corr_matrix:  K^2 -> (K choose 2)
   cholesky_corr: (K choose 2) + K -> (K choose 2)

   cholesky_cov does not change size (it's (K choose 2) + K).
   Now that our unit vector uses the normal thing, that also doesn't
   change size.  The ordered types and constrained types don't
   change sizes either.
*)
let pp_unconstrained_param_names ppf p =
  let params =
    [ "std::vector<std::string>& param_names__"
    ; "bool emit_transformed_parameters__ = true"
    ; "bool emit_generated_quantities__ = true" ]
  in
  let paramvars, tparamvars, gqvars =
    List.partition3_map
      ~f:(function
        | id, {out_block= Parameters; out_unconstrained_st= st; _} ->
            `Fst (id, st)
        | id, {out_block= TransformedParameters; out_unconstrained_st= st; _}
          ->
            `Snd (id, st)
        | id, {out_block= GeneratedQuantities; out_unconstrained_st= st; _} ->
            `Trd (id, st))
      p.output_vars
  in
  let emit_name ppf (name, idcs) =
    let to_string = fmt "std::to_string(%s)" in
    pf ppf "param_names__.push_back(std::string() + %a);"
      (list ~sep:(fun ppf () -> pf ppf " + '.' + ") string)
      (strf "%S" name :: List.map ~f:(strf "%a" to_string) idcs)
  in
  let pp_param_names ppf (decl_id, st) =
    let dims = List.rev (get_dims st) in
    pp_for_loop_iteratee ppf (decl_id, dims, emit_name)
  in
  pp_method ppf "void" "unconstrained_param_names" params [] (fun ppf ->
      (list ~sep:cut pp_param_names) ppf paramvars ;
      pf ppf "@,if (emit_transformed_parameters__) %a@," pp_block
        (list ~sep:cut pp_param_names, tparamvars) ;
      pf ppf "@,if (emit_generated_quantities__) %a@," pp_block
        (list ~sep:cut pp_param_names, gqvars) )

let pp_transform_inits ppf p =
  let params =
    [ "const stan::io::var_context& context__"; "std::vector<int>& params_i__"
    ; "std::vector<double>& vars__"; "std::ostream* pstream__" ]
  in
  let intro =
    [ "typedef double local_scalar_t__;"; "vars__.resize(0);"
    ; "vars__.reserve(num_params_r__);" ]
  in
  pp_method_b ppf "void" "transform_inits" params intro p.transform_inits

let pp_log_prob ppf p =
  pf ppf "template <bool propto__, bool jacobian__, typename T__>@ " ;
  let params =
    [ "std::vector<T__>& params_r__"; "std::vector<int>& params_i__"
    ; "std::ostream* pstream__ = 0" ]
  in
  let intro =
    [ "typedef T__ local_scalar_t__;"
    ; "local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());"
    ; strf "%a" pp_unused "DUMMY_VAR__"
    ; "T__ lp__(0.0);"; "stan::math::accumulator<T__> lp_accum__;"
    ; strf "%a" pp_function__ (p.prog_name, "log_prob")
    ; "stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);" ]
  in
  let outro = ["lp_accum__.add(lp__);"; "return lp_accum__.sum();"] in
  pp_method_b ppf "T__" "log_prob" params intro p.log_prob ~outro

let pp_outvar_metadata ppf (method_name, outvars) =
  let intro = ["stringstream s__;"] in
  let outro = ["return s__.str();"] in
  let json_str = Cpp_Json.out_var_interpolated_json_str outvars in
  let ppbody ppf = pf ppf "s__ << %s;" json_str in
  pp_method ppf "std::string" method_name [] intro ~outro ppbody

let pp_unconstrained_types ppf {output_vars; _} =
  let grab_unconstrained (name, {out_unconstrained_st; out_block; _}) =
    (name, out_unconstrained_st, out_block)
  in
  let outvars = List.map ~f:grab_unconstrained output_vars in
  pp_outvar_metadata ppf ("get_unconstrained_sizedtypes", outvars)

let pp_constrained_types ppf {output_vars; _} =
  let grab_constrained (name, {out_constrained_st; out_block; _}) =
    (name, out_constrained_st, out_block)
  in
  let outvars = List.map ~f:grab_constrained output_vars in
  pp_outvar_metadata ppf ("get_constrained_sizedtypes", outvars)

let pp_overloads ppf () =
  pf ppf
    {|
    // Begin method overload boilerplate
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool emit_transformed_parameters__ = true,
                     bool emit_generated_quantities__ = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng__, params_r_vec, params_i_vec, vars_vec,
          emit_transformed_parameters__, emit_generated_quantities__, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }

    template <bool propto__, bool jacobian__, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto__,jacobian__,T_>(vec_params_r, vec_params_i, pstream);
    }

    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }
|}

let pp_model_public ppf p =
  pf ppf "@ %a" pp_ctor p ;
  pf ppf "@ %a" pp_log_prob p ;
  pf ppf "@ %a" pp_write_array p ;
  pf ppf "@ %a" pp_transform_inits p ;
  (* Begin metadata methods *)
  pf ppf "@ %a" pp_get_param_names p ;
  (* Post-data metadata methods *)
  pf ppf "@ %a" pp_get_dims p ;
  pf ppf "@ %a" pp_constrained_param_names p ;
  pf ppf "@ %a" pp_unconstrained_param_names p ;
  pf ppf "@ %a" pp_constrained_types p ;
  pf ppf "@ %a" pp_unconstrained_types p ;
  (* Boilerplate *)
  pf ppf "@ %a" pp_overloads ()

let pp_model ppf (p : Locations.typed_prog_num) =
  pf ppf "class %s : public model_base_crtp<%s> {" p.prog_name p.prog_name ;
  pf ppf "@ @[<v 1>@ private:@ @[<v 1> %a@]@ " pp_model_private p ;
  pf ppf "@ public:@ @[<v 1> ~%s() { }" p.prog_name ;
  pf ppf "@ @ std::string model_name() const { return \"%s\"; }" p.prog_name ;
  pf ppf "@ %a@]@]@ };" pp_model_public p

let usings =
  {|
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::model_base_crtp;
using stan::model::rvalue;
using stan::model::cons_list;
using stan::model::index_uni;
using stan::model::index_max;
using stan::model::index_min;
using stan::model::index_min_max;
using stan::model::index_multi;
using stan::model::index_omni;
using stan::model::nil_index_list;
using namespace stan::math; |}

(* XXX probably move these to the Stan repo when these repos are joined. *)
let custom_functions =
  {|
template <typename T, typename S>
std::vector<T> resize_to_match__(std::vector<T>& dst, const std::vector<S>& src) {
  dst.resize(src.size());
  return dst;
}

template <typename T>
Eigen::Matrix<T, -1, -1>
resize_to_match__(Eigen::Matrix<T, -1, -1>& dst, const Eigen::Matrix<T, -1, -1>& src) {
  dst.resize(src.rows(), src.cols());
  return dst;
}

template <typename T>
Eigen::Matrix<T, 1, -1>
resize_to_match__(Eigen::Matrix<T, 1, -1>& dst, const Eigen::Matrix<T, 1, -1>& src) {
  dst.resize(src.size());
  return dst;
}

template <typename T>
Eigen::Matrix<T, -1, 1>
resize_to_match__(Eigen::Matrix<T, -1, 1>& dst, const Eigen::Matrix<T, -1, 1>& src) {
  dst.resize(src.size());
  return dst;
}
std::vector<double> to_doubles__(std::initializer_list<double> x) {
  return x;
}

std::vector<stan::math::var> to_vars__(std::initializer_list<stan::math::var> x) {
  return x;
}
|}

let pp_prog ppf (p : typed_prog) =
  (* First, do some transformations on the MIR itself before we begin printing it.*)
  let p, s = Locations.prepare_prog p in
  pf ppf "@[<v>@ %s@ %s@ namespace %s_namespace {@ %s@ %s@ %a@ %a@ %a@ }@ @]"
    version includes p.prog_name custom_functions usings Locations.pp_globals s
    (list ~sep:cut pp_fun_def) p.functions_block pp_model p ;
  pf ppf "@,typedef %s_namespace::%s stan_model;@," p.prog_name p.prog_name ;
  pf ppf
    {|
// Boilerplate
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
|}
