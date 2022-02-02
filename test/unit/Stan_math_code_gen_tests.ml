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
    template <typename Tx__,
              typename Ty__, stan::require_all_t<stan::is_eigen_matrix_dynamic<Tx__>,
              stan::is_row_vector<Ty__>>* = nullptr>
    inline void
    sars(const Tx__& x_arg__, const Ty__& y_arg__, std::ostream* pstream__) {
      using local_scalar_t__ = stan::return_type_t<Tx__, Ty__>;
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
    template <typename Tx__, typename Ty__, typename Tz__,
              typename Tw__, stan::require_all_t<stan::is_eigen_matrix_dynamic<Tx__>,
              stan::is_row_vector<Ty__>, stan::is_row_vector<Tz__>,
              stan::is_std_vector<Tw__>, stan::is_eigen_matrix_dynamic<stan::value_type_t<Tw__>>>* = nullptr>
    inline Eigen::Matrix<stan::return_type_t<Tx__, Ty__, Tz__, Tw__>, -1, -1>
    sars(const Tx__& x_arg__, const Ty__& y_arg__, const Tz__& z_arg__,
         const Tw__& w, std::ostream* pstream__) {
      using local_scalar_t__ = stan::return_type_t<Tx__, Ty__, Tz__, Tw__>;
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
