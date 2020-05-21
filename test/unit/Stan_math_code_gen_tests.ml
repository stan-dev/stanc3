open Middle
open Stan_math_backend
open Core_kernel
open Fmt
open Stan_math_code_gen

let%expect_test "udf" =
  let with_no_loc stmt =
    Stmt.Fixed.{pattern= stmt; meta= Locations.no_span_num}
  in
  let w e = Expr.{Fixed.pattern= e; meta= Typed.Meta.empty} in
  let pp_fun_def_w_rs a b = pp_fun_def a b String.Set.empty in
  { fdrt= None
  ; fdname= "sars"
  ; fdargs= [(DataOnly, "x", UMatrix); (AutoDiffable, "y", URowVector)]
  ; fdbody=
      Stmt.Fixed.Pattern.Return
        (Some
           (w @@ FunApp (StanLib, "add", [w @@ Var "x"; w @@ Lit (Int, "1")])))
      |> with_no_loc |> List.return |> Stmt.Fixed.Pattern.Block |> with_no_loc
  ; fdloc= Location_span.empty }
  |> strf "@[<v>%a" pp_fun_def_w_rs
  |> print_endline ;
  [%expect
    {|
    template <typename T1__>
    void
    sars(const Eigen::Matrix<double, -1, -1>& x,
         const Eigen::Matrix<T1__, 1, -1>& y, std::ostream* pstream__) {
      using local_scalar_t__ = typename boost::math::tools::promote_args<T1__>::type;
      const static bool propto__ = true;
      (void) propto__;
      local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
      (void) DUMMY_VAR__;  // suppress unused var warning

      try {
        return add(x, 1);
      } catch (const std::exception& e) {
        stan::lang::rethrow_located(e, locations_array__[current_statement__]);
          // Next line prevents compiler griping about no return
          throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
      }

    }

    struct sars_functor__ {
    template <typename T1__>
    void
    operator()(const Eigen::Matrix<double, -1, -1>& x,
               const Eigen::Matrix<T1__, 1, -1>& y, std::ostream* pstream__)  const
    {
    return sars(x, y, pstream__);
    }
    }; |}]
