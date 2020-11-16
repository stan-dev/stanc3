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
  let pp_fun_def ppf b =
    pp_forward_decl String.Set.empty ppf b ;
    cut ppf () ;
    match get_impl b with Some b -> pp_function_body ppf b | None -> ()
  in
  { fdrt= None
  ; fdname= "sars"
  ; fdcaptures= None
  ; fdargs= [(DataOnly, "x", UMatrix); (AutoDiffable, "y", URowVector)]
  ; fdbody=
      Stmt.Fixed.Pattern.Return
        (Some
           (w @@ FunApp (StanLib, "add", [w @@ Var "x"; w @@ Lit (Int, "1")])))
      |> with_no_loc |> List.return |> Stmt.Fixed.Pattern.Block |> with_no_loc
      |> Some
  ; fdloc= Location_span.empty }
  |> strf "@[<v>%a" pp_fun_def |> print_endline ;
  [%expect
    {|
    template <typename T1__>
    void
    sars(const Eigen::Matrix<double, -1, -1>& x,
         const Eigen::Matrix<T1__, 1, -1>& y, std::ostream* pstream__) ;

    struct sars_functor__ {
    template <typename T1__>
    void
    operator()(const Eigen::Matrix<double, -1, -1>& x,
               const Eigen::Matrix<T1__, 1, -1>& y, std::ostream* pstream__)  const
    {
    return sars(x, y, pstream__);
    }
    };

    template <typename T1__>
    void
    sars(const Eigen::Matrix<double, -1, -1>& x,
         const Eigen::Matrix<T1__, 1, -1>& y, std::ostream* pstream__) {
      using local_scalar_t__ = stan::promote_args_t<T1__>;
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

    } |}]
