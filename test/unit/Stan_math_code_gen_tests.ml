open Middle
open Stan_math_backend
open Core_kernel
open Fmt
open Stan_math_code_gen

let pp_fun_def ppf b =
  pp_forward_decl String.Set.empty ppf b ;
  cut ppf () ;
  match get_impl b with Some b -> pp_function_body ppf b | None -> ()

let%expect_test "udf" =
  let with_no_loc stmt =
    Stmt.Fixed.{pattern= stmt; meta= Locations.no_span_num}
  in
  let w e = Expr.{Fixed.pattern= e; meta= Typed.Meta.empty} in
  { fdrt= None
  ; fdname= "sars"
  ; fdsuffix= FnPlain
  ; fdcaptures= None
  ; fdargs= [(DataOnly, "x", UMatrix); (AutoDiffable, "y", URowVector)]
  ; fdbody=
      Stmt.Fixed.Pattern.Return
        (Some
           ( w
           @@ FunApp
                (StanLib ("add", FnPlain), [w @@ Var "x"; w @@ Lit (Int, "1")])
           ))
      |> with_no_loc |> List.return |> Stmt.Fixed.Pattern.Block |> with_no_loc
      |> Some
  ; fdloc= Location_span.empty }
  |> strf "@[<v>%a" pp_fun_def |> print_endline ;
  [%expect
    {|
template <typename T0__, typename T1__>
void
sars(const T0__& x, const T1__& y, std::ostream* pstream__) ;

struct sars_functor__ {
template <typename T0__, typename T1__>
void
operator()(const T0__& x, const T1__& y, std::ostream* pstream__)  const
{
return sars(x, y, pstream__);
}
};

template <typename T0__, typename T1__>
void
sars(const T0__& x_arg__, const T1__& y_arg__, std::ostream* pstream__) {
  using local_scalar_t__ = stan::promote_args_t<stan::value_type_t<T0__>,
          stan::value_type_t<T1__>>;
  int current_statement__ = 0;
  const auto& x = to_ref(x_arg__);
  const auto& y = to_ref(y_arg__);
  static constexpr bool propto__ = true;
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

let%expect_test "udf-expressions" =
  let with_no_loc stmt =
    Stmt.Fixed.{pattern= stmt; meta= Locations.no_span_num}
  in
  let w e = Expr.{Fixed.pattern= e; meta= Typed.Meta.empty} in
  { fdrt= Some UMatrix
  ; fdname= "sars"
  ; fdsuffix= FnPlain
  ; fdcaptures= None
  ; fdargs=
      [ (DataOnly, "x", UMatrix)
      ; (AutoDiffable, "y", URowVector)
      ; (AutoDiffable, "z", URowVector)
      ; (AutoDiffable, "w", UArray UMatrix) ]
  ; fdbody=
      Stmt.Fixed.Pattern.Return
        (Some
           ( w
           @@ FunApp
                (StanLib ("add", FnPlain), [w @@ Var "x"; w @@ Lit (Int, "1")])
           ))
      |> with_no_loc |> List.return |> Stmt.Fixed.Pattern.Block |> with_no_loc
      |> Some
  ; fdloc= Location_span.empty }
  |> strf "@[<v>%a" pp_fun_def |> print_endline ;
  [%expect
    {|
template <typename T0__, typename T1__, typename T2__, typename T3__>
Eigen::Matrix<stan::promote_args_t<stan::value_type_t<T0__>, stan::value_type_t<T1__>,
stan::value_type_t<T2__>,
T3__>, -1, -1>
sars(const T0__& x, const T1__& y, const T2__& z,
     const std::vector<Eigen::Matrix<T3__, -1, -1>>& w,
     std::ostream* pstream__) ;

struct sars_functor__ {
template <typename T0__, typename T1__, typename T2__, typename T3__>
Eigen::Matrix<stan::promote_args_t<stan::value_type_t<T0__>, stan::value_type_t<T1__>,
stan::value_type_t<T2__>,
T3__>, -1, -1>
operator()(const T0__& x, const T1__& y, const T2__& z,
           const std::vector<Eigen::Matrix<T3__, -1, -1>>& w,
           std::ostream* pstream__)  const
{
return sars(x, y, z, w, pstream__);
}
};

template <typename T0__, typename T1__, typename T2__, typename T3__>
Eigen::Matrix<stan::promote_args_t<stan::value_type_t<T0__>, stan::value_type_t<T1__>,
stan::value_type_t<T2__>,
T3__>, -1, -1>
sars(const T0__& x_arg__, const T1__& y_arg__, const T2__& z_arg__,
     const std::vector<Eigen::Matrix<T3__, -1, -1>>& w,
     std::ostream* pstream__) {
  using local_scalar_t__ = stan::promote_args_t<stan::value_type_t<T0__>,
          stan::value_type_t<T1__>,
          stan::value_type_t<T2__>,
          T3__>;
  int current_statement__ = 0;
  const auto& x = to_ref(x_arg__);
  const auto& y = to_ref(y_arg__);
  const auto& z = to_ref(z_arg__);
  static constexpr bool propto__ = true;
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

let%expect_test "closure" =
  let with_no_loc stmt =
    Stmt.Fixed.{pattern= stmt; meta= Locations.no_span_num}
  in
  let w e = Expr.{Fixed.pattern= e; meta= Typed.Meta.empty} in
  { fdrt= None
  ; fdname= "sars"
  ; fdsuffix= FnPlain
  ; fdcaptures=
      Some
        [(Ref, DataOnly, "x", UMatrix); (Copy, AutoDiffable, "y", URowVector)]
  ; fdargs= []
  ; fdbody=
      Stmt.Fixed.Pattern.Return
        (Some
           ( w
           @@ FunApp
                (StanLib ("add", FnPlain), [w @@ Var "x"; w @@ Lit (Int, "1")])
           ))
      |> with_no_loc |> List.return |> Stmt.Fixed.Pattern.Block |> with_no_loc
      |> Some
  ; fdloc= Location_span.empty }
  |> strf "@[<v>%a" pp_fun_def |> print_endline ;
  [%expect
    {|
template <typename T0__, typename T1__>
void
sars_impl__(const T0__& x, const T1__& y, std::ostream* pstream__)  ;

template<bool ref__, typename F1__>
class sars_cfunctor__ {
  const Eigen::Matrix<double, -1, -1>& x;
  stan::capture_type_t<Eigen::Matrix<F1__, 1, -1>, ref__> y;
  public:
  const size_t vars_count__;
  sars_cfunctor__(const sars_cfunctor__<ref__, F1__>&) = default ;
  sars_cfunctor__(sars_cfunctor__<ref__, F1__>&&) = default ;
  sars_cfunctor__(const Eigen::Matrix<double, -1, -1>& x__,
                  const Eigen::Matrix<F1__, 1, -1>& y__)
  : x(x__), y(y__), vars_count__(count_vars(x__, y__)) {}
  void
  operator()(std::ostream* pstream__)  const
  {
  return sars_impl__(x, y, pstream__);
  }

  using captured_scalar_t__ = stan::return_type_t<F1__>;
  using ValueOf__ = sars_cfunctor__<false, double>;
  using CopyOf__ = sars_cfunctor__<false, stan::capture_type_t<F1__, false>>;
  size_t count_vars__() const {
  return vars_count__;
  }
  auto value_of__() const {
  return ValueOf__(x, eval(value_of(y)));
  }
  auto deep_copy_vars__() const {
  return CopyOf__(x, eval(deep_copy_vars(y)));
  }
  void zero_adjoints__() {
  stan::math::zero_adjoints(x);
  stan::math::zero_adjoints(y);
  }
  double* accumulate_adjoints__(double *dest) const {
  return stan::math::accumulate_adjoints(dest, x, y);
  }
  stan::math::vari** save_varis__(stan::math::vari **dest) const {
  return stan::math::save_varis(dest, x, y);
  }


  };
template<typename F1__>
auto sars_make__(const Eigen::Matrix<double, -1, -1>& x,
                 const Eigen::Matrix<F1__, 1, -1>& y) {
return sars_cfunctor__<false,
F1__>(x, y);
}

template <typename T0__, typename T1__>
void
sars_impl__(const T0__& x_arg__, const T1__& y_arg__, std::ostream* pstream__) {
  using local_scalar_t__ = stan::promote_args_t<stan::value_type_t<T0__>,
          stan::value_type_t<T1__>>;
  int current_statement__ = 0;
  const auto& x = to_ref(x_arg__);
  const auto& y = to_ref(y_arg__);
  static constexpr bool propto__ = true;
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
