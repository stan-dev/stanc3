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

let pp_located ppf _ =
  pf ppf
    {|stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); |}

let rec contains_int = function
  | UnsizedType.UInt -> true
  | UArray t -> contains_int t
  | _ -> false

let arg_needs_template = function
  | UnsizedType.DataOnly, _, _ -> false
  | _, _, t when contains_int t -> false
  | _ -> true

let maybe_templated_arg_types (args : Program.fun_arg_decl) =
  List.mapi args ~f:(fun i a ->
      match arg_needs_template a with
      | true -> Some (sprintf "T%d__" i)
      | false -> None )

let%expect_test "arg types templated correctly" =
  [(AutoDiffable, "xreal", UReal); (DataOnly, "yint", UInt)]
  |> maybe_templated_arg_types |> List.filter_opt |> String.concat ~sep:","
  |> print_endline ;
  [%expect {| T0__ |}]

let pp_promoted_scalar ppf args =
  match args with
  | [] -> pf ppf "double"
  | _ ->
      let rec promote_args_chunked ppf args =
        let go ppf tl =
          match tl with [] -> () | _ -> pf ppf ", %a" promote_args_chunked tl
        in
        match args with
        | [] -> pf ppf "double"
        | hd :: tl ->
            pf ppf "typename boost::math::tools::promote_args<%a%a>::type"
              (list ~sep:comma string) hd go tl
      in
      promote_args_chunked ppf
        List.(
          chunks_of ~length:5 (filter_opt (maybe_templated_arg_types args)))

(** Pretty-prints a function's return-type, taking into account templated argument
    promotion.*)
let pp_returntype ppf arg_types rt =
  let scalar = strf "%a" pp_promoted_scalar arg_types in
  match rt with
  | Some ut when contains_int ut ->
      pf ppf "%a@," pp_unsizedtype_custom_scalar ("int", ut)
  | Some ut -> pf ppf "%a@," pp_unsizedtype_custom_scalar (scalar, ut)
  | None -> pf ppf "void@,"

(** [pp_located_error ppf (pp_body_block, body_block, err_msg)] surrounds [body_block]
    with a C++ try-catch that will rethrow the error with the proper source location
    from the [body_block] (required to be a [stmt_loc Block] variant).*)
let pp_located_error ppf (pp_body_block, body) =
  pf ppf "@ try %a" pp_body_block body ;
  string ppf " catch (const std::exception& e) " ;
  pp_block ppf (pp_located, ())

let pp_arg ppf (custom_scalar_opt, (_, name, ut)) =
  let scalar =
    match custom_scalar_opt with
    | Some scalar -> scalar
    | None -> stantype_prim_str ut
  in
  pf ppf "const %a& %s" pp_unsizedtype_custom_scalar (scalar, ut) name

(** [pp_located_error_b] automatically adds a Block wrapper *)
let pp_located_error_b ppf body_stmts =
  pp_located_error ppf
    ( pp_statement
    , Stmt.Fixed.{pattern= Block body_stmts; meta= Locations.no_span_num} )

let typename = ( ^ ) "typename "

let get_templates_and_args fdargs =
  let argtypetemplates = maybe_templated_arg_types fdargs in
  ( List.filter_opt argtypetemplates
  , List.map
      ~f:(fun a -> strf "%a" pp_arg a)
      (List.zip_exn argtypetemplates fdargs) )

let pp_template_decorator ppf = function
  | [] -> ()
  | templates ->
      pf ppf "@[<hov>template <%a>@]@ " (list ~sep:comma string) templates

(* XXX refactor this please - one idea might be to have different functions for
   printing user defined distributions vs rngs vs regular functions.
*)
let pp_fun_def ppf Program.({fdrt; fdname; fdargs; fdbody; _}) =
  let is_lp = is_user_lp fdname in
  let is_dist = is_user_dist fdname in
  let is_rng = String.is_suffix fdname ~suffix:"_rng" in
  let extra, extra_templates =
    if is_lp then (["lp__"; "lp_accum__"], ["T_lp__"; "T_lp_accum__"])
    else if is_rng then (["base_rng__"], ["RNG"])
    else ([], [])
  in
  let mk_extra_args templates args =
    List.map ~f:(fun (t, v) -> t ^ "& " ^ v) (List.zip_exn templates args)
  in
  let argtypetemplates, args = get_templates_and_args fdargs in
  let pp_body ppf (Stmt.Fixed.({pattern; _}) as fdbody) =
    let text = pf ppf "%s@;" in
    pf ppf "@[<hv 8>using local_scalar_t__ = %a;@]@," pp_promoted_scalar fdargs ;
    if not (is_dist || is_lp) then (
      text "const static bool propto__ = true;" ;
      text "(void) propto__;" ) ;
    let blocked_fdbody =
      match pattern with
      | SList stmts -> {fdbody with pattern= Block stmts}
      | Block _ -> fdbody
      | _ -> {fdbody with pattern= Block [fdbody]}
    in
    pp_located_error ppf (pp_statement, blocked_fdbody) ;
    pf ppf "@ "
  in
  let templates =
    (if is_dist || is_lp then ["bool propto__"] else [])
    @ List.(map ~f:typename (argtypetemplates @ extra_templates))
  in
  let pp_sig ppf name =
    pp_template_decorator ppf templates ;
    pp_returntype ppf fdargs fdrt ;
    let arg_strs =
      args @ mk_extra_args extra_templates extra @ ["std::ostream* pstream__"]
    in
    pf ppf "%s(@[<hov>%a@]) " name (list ~sep:comma string) arg_strs
  in
  pp_sig ppf fdname ;
  match Stmt.Fixed.(fdbody.pattern) with
  | Skip -> pf ppf ";@ "
  | _ ->
      pp_block ppf (pp_body, fdbody) ;
      pf ppf "@,@,struct %s%s {@,%a const @,{@,return %a;@,}@,};@," fdname
        functor_suffix pp_sig "operator()" pp_call_str
        ( fdname
        , List.map ~f:(fun (_, name, _) -> name) fdargs @ extra @ ["pstream__"]
        )

let version = "// Code generated by %%NAME%% %%VERSION%%"
let includes = "#include <stan/model/model_header.hpp>"

let pp_validate_data ppf (name, st) =
  if String.is_suffix ~suffix:"__" name then ()
  else
    pf ppf "@[<hov 4>context__.validate_dims(@,%S,@,%S,@,%S,@,%a);@]@ "
      "data initialization" name
      (stantype_prim_str (SizedType.to_unsized st))
      pp_call
      ("context__.to_vec", pp_expr, SizedType.get_dims st)

(* Read in data steps:
   1. context__.validate_dims() (what is this?)
   1. find vals_%s__ from context__.vals_%s(vident)
   1. keep track of pos__
   1. run checks on resulting vident
*)
let pp_ctor ppf p =
  let params =
    [ "stan::io::var_context& context__"; "unsigned int random_seed__ = 0"
    ; "std::ostream* pstream__ = nullptr" ]
  in
  pf ppf "%s(@[<hov 0>%a) : model_base_crtp(0) @]" p.Program.prog_name
    (list ~sep:comma string) params ;
  let pp_mul ppf () = pf ppf " * " in
  let pp_num_param ppf (checks, dims) =
    if List.length checks > 0 then (
      list ~sep:cut pp_statement ppf checks ;
      cut ppf () ) ;
    pf ppf "num_params_r__ += %a;" (list ~sep:pp_mul pp_expr) dims
  in
  let get_param_st = function
    | ( decl_id
      , { Program.out_block= Parameters
        ; out_unconstrained_st= st
        ; out_constrained_st= cst
        ; out_trans= tr } ) -> (
        let meta =
          p.log_prob
          |> List.find ~f:(function
               | {Stmt.Fixed.pattern= Decl {decl_id= id; _}; _}
                 when id = decl_id ->
                   true
               | _ -> false )
          |> Option.map ~f:(fun x -> x.meta)
          |> Option.value ~default:Stmt.Numbered.Meta.empty
        in
        let dims_check = Transform_Mir.validate_sized decl_id meta (Some tr) in
        match SizedType.get_dims st with
        | [] -> Some (dims_check cst, [Expr.Helpers.loop_bottom])
        | ls -> Some (dims_check cst, ls) )
    | _ -> None
  in
  let data_idents = List.map ~f:fst p.input_vars |> String.Set.of_list in
  let pp_stmt_topdecl_size_only ppf (Stmt.Fixed.({pattern; _}) as s) =
    match pattern with
    | Decl {decl_id; decl_type; _} -> (
      match decl_type with
      | Sized st ->
          if Set.mem data_idents decl_id then pp_validate_data ppf (decl_id, st) ;
          pp_set_size ppf (decl_id, st, DataOnly)
      | Unsized _ -> () )
    | _ -> pp_statement ppf s
  in
  pp_block ppf
    ( (fun ppf {Program.prog_name; prepare_data; output_vars; _} ->
        pf ppf "typedef double local_scalar_t__;@ " ;
        pf ppf "boost::ecuyer1988 base_rng__ = @ " ;
        pf ppf "    stan::services::util::create_rng(random_seed__, 0);@ " ;
        pp_unused ppf "base_rng__" ;
        pp_function__ ppf (prog_name, prog_name) ;
        pp_located_error ppf
          (pp_block, (list ~sep:cut pp_stmt_topdecl_size_only, prepare_data)) ;
        cut ppf () ;
        pf ppf "num_params_r__ = 0U;@ " ;
        pp_located_error ppf
          ( pp_block
          , ( list ~sep:cut pp_num_param
            , List.filter_map ~f:get_param_st output_vars ) ) )
    , p )

let pp_model_private ppf {Program.prepare_data; _} =
  let decl Stmt.Fixed.({pattern; _}) =
    match pattern with
    | Decl d ->
        Some (d.decl_id, Type.to_unsized d.decl_type, UnsizedType.DataOnly)
    | _ -> None
  in
  let data_decls = List.filter_map ~f:decl prepare_data in
  pf ppf "%a" (list ~sep:cut pp_decl) data_decls

let pp_method ppf rt name params intro ?(outro = []) ppbody =
  pf ppf "@[<v 2>%s %s(@[<hov>@,%a@]) const " rt name (list ~sep:comma string)
    params ;
  pf ppf "{@,%a" (list ~sep:cut string) intro ;
  pf ppf "@ " ;
  ppbody ppf ;
  if not (List.is_empty outro) then pf ppf "@ %a" (list ~sep:cut string) outro ;
  pf ppf "@,} // %s() @,@]" name

let pp_get_param_names ppf {Program.output_vars; _} =
  let add_param = fmt "names__.push_back(%S);" in
  pp_method ppf "void" "get_param_names" ["std::vector<std::string>& names__"]
    [] (fun ppf ->
      pf ppf "names__.resize(0);@ " ;
      (list ~sep:cut add_param) ppf (List.map ~f:fst output_vars) )

let pp_get_dims ppf {Program.output_vars; _} =
  let pp_dim ppf dim = pf ppf "dims__.push_back(%a);@," pp_expr dim in
  let pp_dim_sep ppf () =
    pf ppf "dimss__.push_back(dims__);@,dims__.resize(0);@,"
  in
  let pp_output_var ppf =
    (list ~sep:pp_dim_sep (list ~sep:cut pp_dim))
      ppf
      List.(
        map ~f:SizedType.get_dims
          (map
             ~f:(fun (_, {Program.out_constrained_st= st; _}) -> st)
             output_vars))
  in
  let params = ["std::vector<std::vector<size_t>>& dimss__"] in
  pp_method ppf "void" "get_dims" params
    ["dimss__.resize(0);"; "std::vector<size_t> dims__;"] (fun ppf ->
      pp_output_var ppf ; pp_dim_sep ppf () )

let pp_method_b ppf rt name params intro ?(outro = []) body =
  pp_method ppf rt name params intro
    (fun ppf -> pp_located_error_b ppf body)
    ~outro

let pp_write_array ppf {Program.prog_name; generate_quantities; _} =
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
    ; strf "%a" pp_function__ (prog_name, "write_array")
    ; strf "%a" pp_unused "function__"
    ; "double lp__ = 0.0;"
    ; "(void) lp__;  // dummy to suppress unused var warning"
    ; "stan::math::accumulator<double> lp_accum__;" ]
  in
  pp_method_b ppf "void" "write_array" params intro generate_quantities

let rec pp_for_loop_iteratee ?(index_ids = []) ppf (iteratee, dims, pp_body) =
  let iter d pp_body =
    let loopvar, gensym_exit = Common.Gensym.enter () in
    pp_for_loop ppf
      ( loopvar
      , Expr.Helpers.loop_bottom
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

let pp_constrained_param_names ppf {Program.output_vars; _} =
  let params =
    [ "std::vector<std::string>& param_names__"
    ; "bool emit_transformed_parameters__ = true"
    ; "bool emit_generated_quantities__ = true" ]
  in
  let paramvars, tparamvars, gqvars =
    List.partition3_map
      ~f:(function
        | id, {Program.out_block= Parameters; out_constrained_st= st; _} ->
            `Fst (id, st)
        | id, {out_block= TransformedParameters; out_constrained_st= st; _} ->
            `Snd (id, st)
        | id, {out_block= GeneratedQuantities; out_constrained_st= st; _} ->
            `Trd (id, st))
      output_vars
  in
  let emit_name ppf (name, idcs) =
    let to_string = fmt "std::to_string(%s)" in
    pf ppf "param_names__.push_back(std::string() + %a);"
      (list ~sep:(fun ppf () -> pf ppf " + '.' + ") string)
      (strf "%S" name :: List.map ~f:(strf "%a" to_string) idcs)
  in
  let pp_param_names ppf (decl_id, st) =
    let dims = List.rev (SizedType.get_dims st) in
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
let pp_unconstrained_param_names ppf {Program.output_vars; _} =
  let params =
    [ "std::vector<std::string>& param_names__"
    ; "bool emit_transformed_parameters__ = true"
    ; "bool emit_generated_quantities__ = true" ]
  in
  let paramvars, tparamvars, gqvars =
    List.partition3_map
      ~f:(function
        | id, {Program.out_block= Parameters; out_unconstrained_st= st; _} ->
            `Fst (id, st)
        | id, {out_block= TransformedParameters; out_unconstrained_st= st; _}
          ->
            `Snd (id, st)
        | id, {out_block= GeneratedQuantities; out_unconstrained_st= st; _} ->
            `Trd (id, st))
      output_vars
  in
  let emit_name ppf (name, idcs) =
    let to_string = fmt "std::to_string(%s)" in
    pf ppf "param_names__.push_back(std::string() + %a);"
      (list ~sep:(fun ppf () -> pf ppf " + '.' + ") string)
      (strf "%S" name :: List.map ~f:(strf "%a" to_string) idcs)
  in
  let pp_param_names ppf (decl_id, st) =
    let dims = List.rev (SizedType.get_dims st) in
    pp_for_loop_iteratee ppf (decl_id, dims, emit_name)
  in
  pp_method ppf "void" "unconstrained_param_names" params [] (fun ppf ->
      (list ~sep:cut pp_param_names) ppf paramvars ;
      pf ppf "@,if (emit_transformed_parameters__) %a@," pp_block
        (list ~sep:cut pp_param_names, tparamvars) ;
      pf ppf "@,if (emit_generated_quantities__) %a@," pp_block
        (list ~sep:cut pp_param_names, gqvars) )

let pp_transform_inits ppf {Program.transform_inits; _} =
  let params =
    [ "const stan::io::var_context& context__"; "std::vector<int>& params_i__"
    ; "std::vector<double>& vars__"; "std::ostream* pstream__" ]
  in
  let intro =
    [ "typedef double local_scalar_t__;"; "vars__.resize(0);"
    ; "vars__.reserve(num_params_r__);" ]
  in
  pp_method_b ppf "void" "transform_inits" params intro transform_inits

let pp_log_prob ppf Program.({prog_name; log_prob; _}) =
  pf ppf "template <bool propto__, bool jacobian__, typename T__>@ " ;
  let params =
    [ "std::vector<T__>& params_r__"; "std::vector<int>& params_i__"
    ; "std::ostream* pstream__ = 0" ]
  in
  let intro =
    [ "typedef T__ local_scalar_t__;"; "T__ lp__(0.0);"
    ; "stan::math::accumulator<T__> lp_accum__;"
    ; strf "%a" pp_function__ (prog_name, "log_prob")
    ; "stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);" ]
  in
  let outro = ["lp_accum__.add(lp__);"; "return lp_accum__.sum();"] in
  pp_method_b ppf "T__" "log_prob" params intro log_prob ~outro

let pp_outvar_metadata ppf (method_name, outvars) =
  let intro = ["stringstream s__;"] in
  let outro = ["return s__.str();"] in
  let json_str = Cpp_Json.out_var_interpolated_json_str outvars in
  let ppbody ppf = pf ppf "s__ << %s;" json_str in
  pp_method ppf "std::string" method_name [] intro ~outro ppbody

let pp_unconstrained_types ppf {Program.output_vars; _} =
  let grab_unconstrained (name, {Program.out_unconstrained_st; out_block; _}) =
    (name, out_unconstrained_st, out_block)
  in
  let outvars = List.map ~f:grab_unconstrained output_vars in
  pp_outvar_metadata ppf ("get_unconstrained_sizedtypes", outvars)

let pp_constrained_types ppf {Program.output_vars; _} =
  let grab_constrained (name, {Program.out_constrained_st; out_block; _}) =
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

let pp_model ppf ({Program.prog_name; _} as p) =
  pf ppf "class %s : public model_base_crtp<%s> {" prog_name prog_name ;
  pf ppf "@ @[<v 1>@ private:@ @[<v 1> %a@]@ " pp_model_private p ;
  pf ppf "@ public:@ @[<v 1> ~%s() { }" p.prog_name ;
  pf ppf "@ @ std::string model_name() const { return \"%s\"; }" prog_name ;
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

inline void validate_positive_index(const char* var_name, const char* expr,
                                    int val) {
  if (val < 1) {
    std::stringstream msg;
    msg << "Found dimension size less than one in simplex declaration"
        << "; variable=" << var_name << "; dimension size expression=" << expr
        << "; expression value=" << val;
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}

inline void validate_unit_vector_index(const char* var_name, const char* expr,
                                       int val) {
  if (val <= 1) {
    std::stringstream msg;
    if (val == 1) {
      msg << "Found dimension size one in unit vector declaration."
          << " One-dimensional unit vector is discrete"
          << " but the target distribution must be continuous."
          << " variable=" << var_name << "; dimension size expression=" << expr;
    } else {
      msg << "Found dimension size less than one in unit vector declaration"
          << "; variable=" << var_name << "; dimension size expression=" << expr
          << "; expression value=" << val;
    }
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}
|}

let namespace Program.({prog_name; _}) = prog_name ^ "_namespace"

let pp_register_map_rect_functors ppf p =
  let find_functors_expr accum Expr.Fixed.({pattern; _}) =
    match pattern with
    | FunApp (StanLib, "map_rect", {pattern= Var f; _} :: _) -> f :: accum
    | _ -> accum
  in
  let rec find_functors_stmt accum stmt =
    Stmt.Fixed.(
      Pattern.fold find_functors_expr find_functors_stmt accum stmt.pattern)
  in
  let functors = Program.fold find_functors_expr find_functors_stmt [] p in
  let pp_register_functor ppf (i, f) =
    pf ppf "STAN_REGISTER_MAP_RECT(%d, %s::%s%s)" i (namespace p) f
      functor_suffix
  in
  pf ppf "@ %a"
    (list ~sep:cut pp_register_functor)
    (List.mapi ~f:(fun i f -> (i + 1, f)) functors)

let pp_prog ppf (p : Program.Typed.t) =
  (* First, do some transformations on the MIR itself before we begin printing it.*)
  let p, s = Locations.prepare_prog p in
  pf ppf "@[<v>@ %s@ %s@ namespace %s {@ %s@ %s@ %a@ %a@ %a@ }@ @]" version
    includes (namespace p) custom_functions usings Locations.pp_globals s
    (list ~sep:cut pp_fun_def) p.functions_block pp_model p ;
  pf ppf "@,typedef %s_namespace::%s stan_model;@," p.prog_name p.prog_name ;
  pf ppf
    {|
#ifndef USING_R

// Boilerplate
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}

#endif
|} ;
  pf ppf "@[<v>%a@]" pp_register_map_rect_functors p
