open Common
open Middle

let inline source =
  Gensym.reset_danger_use_cautiously ();
  Test_utils.mir_of_string source
  |> Analysis_and_optimization.Optimize.function_inlining

let print_statements statements =
  Fmt.pr "@[<v>%a@]@." Fmt.(list ~sep:cut Stmt.Located.pp) statements

let%expect_test "evaluate a repeated scalar actual once" =
  let mir =
    inline
      {|
      functions {
        real f(real x) { return x * x; }
        real single_use(real x) { return x; }
      }
      parameters { real theta; }
      model {
        target += f(exp(theta));
        target += f(theta);
        target += f(2.0);
        target += single_use(log(theta));
      }
      |}
  in
  print_statements mir.log_prob;
  [%expect
    {|
    real theta;
    {
      real inline_f_x_arg_sym1__;
      real inline_f_return_sym2__;
      inline_f_x_arg_sym1__ = exp(theta);
      {
        inline_f_return_sym2__ = (inline_f_x_arg_sym1__ * inline_f_x_arg_sym1__);
      }
      target += inline_f_return_sym2__;
      real inline_f_return_sym4__;
      {
        inline_f_return_sym4__ = (theta * theta);
      }
      target += inline_f_return_sym4__;
      real inline_f_return_sym6__;
      {
        inline_f_return_sym6__ = (2.0 * 2.0);
      }
      target += inline_f_return_sym6__;
      real inline_single_use_return_sym8__;
      {
        inline_single_use_return_sym8__ = log(theta);
      }
      target += inline_single_use_return_sym8__;
    }
    |}]

let%expect_test "evaluate repeated integer and complex actuals once" =
  let mir =
    inline
      {|
      functions {
        int twice_int(int x) { return x + x; }
        complex square_complex(complex z) { return z * z; }
      }
      data { int i; }
      parameters { complex z; }
      model {
        target += twice_int(abs(i));
        target += get_real(square_complex(exp(z)));
      }
      generated quantities {
        int draw = twice_int(poisson_rng(2));
        complex value = square_complex(exp(to_complex(1, 1)));
      }
      |}
  in
  print_statements mir.log_prob;
  print_statements mir.generate_quantities;
  [%expect
    {|
    complex z;
    {
      data int inline_twice_int_x_arg_sym7__;
      int inline_twice_int_return_sym8__;
      inline_twice_int_x_arg_sym7__ = abs(i);
      {
        inline_twice_int_return_sym8__ = (inline_twice_int_x_arg_sym7__ + inline_twice_int_x_arg_sym7__);
      }
      target += inline_twice_int_return_sym8__;
      complex inline_square_complex_z_arg_sym10__;
      complex inline_square_complex_return_sym11__;
      inline_square_complex_z_arg_sym10__ = exp(z);
      {
        inline_square_complex_return_sym11__ = (inline_square_complex_z_arg_sym10__ * inline_square_complex_z_arg_sym10__);
      }
      target += get_real(inline_square_complex_return_sym11__);
    }
    data complex z;
    if(emit_transformed_parameters__) ; else {

    }
    if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
    if(PNot__(emit_generated_quantities__)) return;
    data int draw;
    data int inline_twice_int_x_arg_sym1__;
    data int inline_twice_int_return_sym2__;
    inline_twice_int_x_arg_sym1__ = poisson_rng(2);
    {
      inline_twice_int_return_sym2__ = (inline_twice_int_x_arg_sym1__ + inline_twice_int_x_arg_sym1__);
    }
    draw = inline_twice_int_return_sym2__;
    data complex value;
    data complex inline_square_complex_z_arg_sym4__;
    data complex inline_square_complex_return_sym5__;
    inline_square_complex_z_arg_sym4__ = exp(to_complex(promote(1, real, data),
                                                        promote(1, real, data)));
    {
      inline_square_complex_return_sym5__ = (inline_square_complex_z_arg_sym4__ * inline_square_complex_z_arg_sym4__);
    }
    value = inline_square_complex_return_sym5__;
    |}]

let%expect_test "bind scalar actuals in argument evaluation order" =
  let mir =
    inline
      {|
      functions {
        real twice(real x, real y) { return x * x + y * y; }
        real announce(real x) { print(x); return x; }
      }
      generated quantities {
        real a = twice(normal_rng(0, 1), announce(2));
        real b = twice(announce(3), normal_rng(4, 1));
      }
      |}
  in
  print_statements mir.generate_quantities;
  [%expect
    {|
    if(emit_transformed_parameters__) ; else {

    }
    if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
    if(PNot__(emit_generated_quantities__)) return;
    data real a;
    data real inline_announce_x_arg_sym2__;
    data real inline_announce_return_sym3__;
    data real inline_twice_x_arg_sym1__;
    data real inline_twice_return_sym5__;
    inline_announce_x_arg_sym2__ = promote(2, real, data);
    {
      FnPrint__(inline_announce_x_arg_sym2__);
      inline_announce_return_sym3__ = inline_announce_x_arg_sym2__;
    }
    inline_twice_x_arg_sym1__ = normal_rng(0, 1);
    {
      inline_twice_return_sym5__ = ((inline_twice_x_arg_sym1__ * inline_twice_x_arg_sym1__) + (inline_announce_return_sym3__ * inline_announce_return_sym3__));
    }
    a = inline_twice_return_sym5__;
    data real b;
    data real inline_twice_y_arg_sym10__;
    data real inline_announce_x_arg_sym7__;
    data real inline_announce_return_sym8__;
    data real inline_twice_return_sym11__;
    inline_twice_y_arg_sym10__ = normal_rng(4, 1);
    inline_announce_x_arg_sym7__ = promote(3, real, data);
    {
      FnPrint__(inline_announce_x_arg_sym7__);
      inline_announce_return_sym8__ = inline_announce_x_arg_sym7__;
    }
    {
      inline_twice_return_sym11__ = ((inline_announce_return_sym8__ * inline_announce_return_sym8__) + (inline_twice_y_arg_sym10__ * inline_twice_y_arg_sym10__));
    }
    b = inline_twice_return_sym11__;
    |}]

let%expect_test "bind repeated scalar actuals for void functions" =
  let mir =
    inline
      {|
      functions { void twice(real x) { print(x, x); } }
      generated quantities { twice(normal_rng(0, 1)); }
      |}
  in
  print_statements mir.generate_quantities;
  [%expect
    {|
    if(emit_transformed_parameters__) ; else {

    }
    if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
    if(PNot__(emit_generated_quantities__)) return;
    data real inline_twice_x_arg_sym1__;
    inline_twice_x_arg_sym1__ = normal_rng(0, 1);
    {
      FnPrint__(inline_twice_x_arg_sym1__, inline_twice_x_arg_sym1__);
    }
    |}]
