open Middle
open Stan_math_backend
open Core_kernel
open Fmt
open Stan_math_code_gen

let pp_fun_def_w_rs a b =
  pp_fun_def a
    ( b
    , String.Table.create ()
    , String.Set.empty
    , String.Set.empty
    , String.Set.empty )

let%expect_test "udf" =
  let with_no_loc stmt =
    Stmt.Fixed.{pattern= stmt; meta= Locations.no_span_num} in
  let w e = Expr.{Fixed.pattern= e; meta= Typed.Meta.empty} in
  { fdrt= None
  ; fdname= "sars"
  ; fdsuffix= FnPlain
  ; fdargs= [(DataOnly, "x", UMatrix); (AutoDiffable, "y", URowVector)]
  ; fdbody=
      Stmt.Fixed.Pattern.Return
        (Some
           ( w
           @@ FunApp
                ( StanLib ("add", FnPlain, AoS)
                , [w @@ Var "x"; w @@ Lit (Int, "1")] ) ) )
      |> with_no_loc |> List.return |> Stmt.Fixed.Pattern.Block |> with_no_loc
      |> Some
  ; fdloc= Location_span.empty }
  |> str "@[<v>%a" pp_fun_def_w_rs
  |> print_endline ;
  [%expect
    {|
    template <typename T0__, typename T1__,
              stan::require_eigen_matrix_dynamic_t<T0__>* = nullptr,
              stan::require_row_vector_t<T1__>* = nullptr>
    void
    sars(const T0__& x_arg__, const T1__& y_arg__, std::ostream* pstream__) {
      using local_scalar_t__ =
              stan::promote_args_t<stan::value_type_t<T0__>,
                                   stan::value_type_t<T1__>>;
      int current_statement__ = 0;
      const auto& x = stan::math::to_ref(x_arg__);
      const auto& y = stan::math::to_ref(y_arg__);
      static constexpr bool propto__ = true;
      (void) propto__;
      local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
      (void) DUMMY_VAR__;  // suppress unused var warning
      try {
        return stan::math::add(x, 1);
      } catch (const std::exception& e) {
        stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      }

    } |}]

let%expect_test "udf-expressions" =
  let with_no_loc stmt =
    Stmt.Fixed.{pattern= stmt; meta= Locations.no_span_num} in
  let w e = Expr.{Fixed.pattern= e; meta= Typed.Meta.empty} in
  { fdrt= Some UMatrix
  ; fdname= "sars"
  ; fdsuffix= FnPlain
  ; fdargs=
      [ (DataOnly, "x", UMatrix); (AutoDiffable, "y", URowVector)
      ; (AutoDiffable, "z", URowVector); (AutoDiffable, "w", UArray UMatrix) ]
  ; fdbody=
      Stmt.Fixed.Pattern.Return
        (Some
           ( w
           @@ FunApp
                ( StanLib ("add", FnPlain, AoS)
                , [w @@ Var "x"; w @@ Lit (Int, "1")] ) ) )
      |> with_no_loc |> List.return |> Stmt.Fixed.Pattern.Block |> with_no_loc
      |> Some
  ; fdloc= Location_span.empty }
  |> str "@[<v>%a" pp_fun_def_w_rs
  |> print_endline ;
  [%expect
    {|
    template <typename T0__, typename T1__, typename T2__, typename T3__,
              stan::require_eigen_matrix_dynamic_t<T0__>* = nullptr,
              stan::require_row_vector_t<T1__>* = nullptr,
              stan::require_row_vector_t<T2__>* = nullptr,
              stan::require_stan_scalar_t<T3__>* = nullptr>
    Eigen::Matrix<stan::promote_args_t<stan::value_type_t<T0__>, stan::value_type_t<T1__>,
                         stan::value_type_t<T2__>, T3__>, -1, -1>
    sars(const T0__& x_arg__, const T1__& y_arg__, const T2__& z_arg__,
         const std::vector<Eigen::Matrix<T3__, -1, -1>>& w,
         std::ostream* pstream__) {
      using local_scalar_t__ =
              stan::promote_args_t<stan::value_type_t<T0__>,
                                   stan::value_type_t<T1__>,
                                   stan::value_type_t<T2__>, T3__>;
      int current_statement__ = 0;
      const auto& x = stan::math::to_ref(x_arg__);
      const auto& y = stan::math::to_ref(y_arg__);
      const auto& z = stan::math::to_ref(z_arg__);
      static constexpr bool propto__ = true;
      (void) propto__;
      local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
      (void) DUMMY_VAR__;  // suppress unused var warning
      try {
        return stan::math::add(x, 1);
      } catch (const std::exception& e) {
        stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      }

    } |}]
