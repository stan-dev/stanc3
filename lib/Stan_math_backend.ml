
open Base_prog_tree
open Cpp_gen_tree
open Core_kernel

let stan_math_map =
  String.Map.of_alist_exn
    ["+", "add"; "-", "minus"; "/", "divide"; "*", "multiply"]

(* XXX Ask OCaml person to code review this duplication below *)
let rec translate_fn_names_expr = function
  | FnApp(f, args) -> FnApp(
      String.Map.find stan_math_map f |> Option.value ~default:f,
      List.map ~f:translate_fn_names_expr args)
  | e -> map_expr translate_fn_names_expr e

let id x = x
let rec translate_fn_names_statement = function
  | NRFnApp(f, args) -> NRFnApp(
      String.Map.find stan_math_map f |> Option.value ~default:f,
      List.map ~f:translate_fn_names_expr args)
  | s -> map_statement id translate_fn_names_statement id s

let rec translate_stantype = function
  | Mir.SInt -> SInt
  | Mir.SReal -> SReal
  | Mir.SVector -> SVector
  | Mir.SRowVector -> SRowVector
  | Mir.SMatrix -> SMatrix
  | Mir.SArray st -> SArray (translate_stantype st)

let mir_to_cpp_decl : Mir.vardecl -> vardecl =
  fun (name, st) -> (translate_stantype st), name

let rec translate_statement (s: Mir.statement) : statement =
  Base_prog_tree.map_statement mir_to_cpp_decl translate_statement id s

let translate_udf {Mir.returntype = rt; name; arguments; body} =
  {returntype = begin match rt with
      | None -> None
      | Some st -> Some(AVar, (translate_stantype st)) end;
   name = name; templates = []; body = translate_statement body;
   arguments = List.map
       ~f:(fun (name, st) -> AVar, Immutable, (translate_stantype st), name)
       arguments}

let fncall name args =
  let is_num s = not (Str.string_match (Str.regexp "[a-zA-Z]") s 0) in
  let is_floating s = String.contains s '.' in
  let to_arg x = if is_num x then
      if is_floating x then Lit (Real, x) else Lit (Int, x)
    else
      Lit (Str, x) in
  NRFnApp(name, List.map ~f:to_arg args)

let prog_reader_fn path =
  let prog_reader_type = SOther "stan::io::program_reader" in
  {
    returntype = Some (AData, prog_reader_type);
    name = "prog_reader__";
    arguments = [];
    body = Block [
        Decl((prog_reader_type,"reader"), None);
        fncall "reader.add_event" ["0"; "0"; "start"; path];
        (* XXX Fix the end numbers*)
        fncall "reader.add_event" ["0"; "0"; "end"; path];
        Return(Var "reader")];
    templates = [];
  }

let convert p =
  let p = Mir.map_prog translate_fn_names_statement translate_fn_names_expr p in
  {usings = ["std::istream"; "std::string"; "std::stringstream";
             "std::vector"; "stan::io::dump"; "stan::math::lgamma";
             "stan::model::prob_grad"; "namespace stan::math"];
   namespace = [p.prog_name ^ "_model_namespace"];
   includes = ["stan/model/model_header.hpp"];
   functions = prog_reader_fn p.prog_path :: List.map ~f:translate_udf p.functions;
   cppclass = {
     classname = p.prog_name ^ "_model"; super = "prob_grad";
     fields = List.map ~f:mir_to_cpp_decl p.data;
     (* These will be all of the special methods like getting the param names and shit*)
     methods = [
       {returntype = Some(AData, SString); arguments = []; templates = [];
        name = "model_name"; body = Return(Lit(Str, p.prog_name))
       };
       (* void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "theta";
        param_names__.push_back(param_name_stream__.str());

        if (!include_gqs__ && !include_tparams__) return;

        if (include_tparams__) {
        }

        if (!include_gqs__) return;} *)
       {name = "constrained_param_names"; returntype = None; templates = [];
        arguments = [AData, Mutable, SArray SString, "param_names__"];
        body = Block [
            Decl((SOther "std::stringstream", "param_name_stream__"), None);
            NRFnApp("param_name_stream__.str", [FnApp("std::string", [])]);
            (* XXX need to figure out strm << "" *)
            NRFnApp("param_names__.push_back",
                    [FnApp("param_name_stream__.str", [])]);
            (* XXX Is any of this a good idea?
               here i would like to put a blank line...*)
            (* XXX And here I would like to use && *)
          ];
       };

       (*
    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;

        current_statement_begin__ = 11;
        if (!(context__.contains_r("theta")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable theta missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("theta");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "theta", "double", context__.to_vec());
        double theta(0);
        theta = vals_r__[pos__++];
        try {
            writer__.scalar_lub_unconstrain(0, 1, theta);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable theta: ") + e.what()), current_statement_begin__, prog_reader__());
        }

        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
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


    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {

        typedef T__ local_scalar_t__;

        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning

        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);

            // model parameters
            current_statement_begin__ = 11;
            local_scalar_t__ theta;
            (void) theta;  // dummy to suppress unused var warning
            if (jacobian__)
                theta = in__.scalar_lub_constrain(0, 1, lp__);
            else
                theta = in__.scalar_lub_constrain(0, 1);

            // model body

            current_statement_begin__ = 14;
            lp_accum__.add(beta_log<propto__>(theta, sars(0, pstream__), 1));
            current_statement_begin__ = 15;
            for (int n = 1; n <= N; ++n) {
                current_statement_begin__ = 16;
                lp_accum__.add(bernoulli_log<propto__>(get_base1(y,n,"y",1), theta));
            }

        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        lp_accum__.add(lp__);
        return lp_accum__.sum();

    } // log_prob()

    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }


    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("theta");
    }


    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dimss__.push_back(dims__);
    }

    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;

        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "bernoulli_model_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning

        // read-transform, write parameters
        double theta = in__.scalar_lub_constrain(0, 1);
        vars__.push_back(theta);

        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;

        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        if (!include_tparams__ && !include_gqs__) return;

        try {
            if (!include_gqs__ && !include_tparams__) return;
            if (!include_gqs__) return;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }

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
        *)

     ];
     ctor = [
       (*
              bernoulli_model(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, 0, pstream__);
    }

    bernoulli_model(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        ctor_body(context__, random_seed__, pstream__);
    }

    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;

        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning

        current_statement_begin__ = -1;

        static const char* function__ = "bernoulli_model_namespace::bernoulli_model";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        try {
            // initialize data block variables from context__
            current_statement_begin__ = 7;
            context__.validate_dims("data initialization", "N", "int", context__.to_vec());
            N = int(0);
            vals_i__ = context__.vals_i("N");
            pos__ = 0;
            N = vals_i__[pos__++];
            check_greater_or_equal(function__, "N", N, 0);

            current_statement_begin__ = 8;
            validate_non_negative_index("y", "N", N);
            context__.validate_dims("data initialization", "y", "int", context__.to_vec(N));
            y = std::vector<int>(N, int(0));
            vals_i__ = context__.vals_i("y");
            pos__ = 0;
            size_t y_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < y_k_0_max__; ++k_0__) {
                y[k_0__] = vals_i__[pos__++];
            }
            size_t y_i_0_max__ = N;
            for (size_t i_0__ = 0; i_0__ < y_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "y[i_0__]", y[i_0__], 0);
                check_less_or_equal(function__, "y[i_0__]", y[i_0__], 1);
            }


            // initialize transformed data variables
            // execute transformed data statements

            // validate transformed data

            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 11;
            num_params_r__ += 1;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }


        *)

     ], Skip;}}

(*
let emit_prelude ppf = fprintf ppf {|
static int current_statement_begin__;

|} (* XXX fix end position*)
*)

(*

   Okay. when outputting strings, the true unit of shit is strings to add together.
   We can parameterize these using the emit_* ppf pattern.
   Then if we want to emit a program, we take in a bunch of other emitters
  (and their arguments?) and use them appropriately? So like "%a" emit_functions funcs
   And we want to basically hardcode ~6 functions or something. So those will each
   depend on some aspect of the program (param
   names or whatever). We essentially want to procedurally generate only a
   few functions
   (ctor with transformed data, transformed inits for transformed parameters,
    generated quantities)
   using something like the Cpp_gen interface, and then the rest we want to construct
   in some custom way using little bits of information we have about the program.
   For the non-procedural ones, we'd like to do custom formatting and generally
   have easier facility to output straight C++ instead of translating by hand a bunch
   of mostly hardcoded shenanigans. So what is the right layer to mingle these two? We
   need to know about all of the functions for e.g. a class at once. There's some kind
   of monad implied by the format ppf thingies, I'm sure of it.

*)
