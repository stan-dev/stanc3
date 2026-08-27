open Std
open Std.Sexp_conv
open Analysis_and_optimization.Optimize
open Middle
open Common
open Analysis_and_optimization.Mir_utils
open Analysis_and_optimization.Dataflow_types

let reset_and_mir_of_string s =
  Gensym.reset_danger_use_cautiously ();
  Test_utils.mir_of_string s

let%expect_test "map_rec_stmt_loc" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        print(24);
        if (13) {
          print(244);
          if (24) {
            print(24);
          }
        }
      }
      |}
  in
  let f = function
    | Stmt.Pattern.NRFunApp (CompilerInternal FnPrint, [s]) ->
        Stmt.Pattern.NRFunApp (CompilerInternal FnPrint, [s; s])
    | x -> x in
  let mir = Program.map Fun.id (map_rec_stmt_loc f) Fun.id mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          FnPrint__(24, 24);
          if(13) {
            FnPrint__(244, 244);
            if(24) {
              FnPrint__(24, 24);
            }
          }
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "map_rec_state_stmt_loc" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        print(24);
        if (13) {
          print(244);
          if (24) {
            print(24);
          }
        }
      }
      |}
  in
  let f i = function
    | Stmt.Pattern.NRFunApp (CompilerInternal FnPrint, [s]) ->
        Stmt.Pattern.(NRFunApp (CompilerInternal FnPrint, [s; s]), i + 1)
    | x -> (x, i) in
  let mir_stmt, num =
    (map_rec_state_stmt_loc f 0)
      Stmt.{pattern= SList mir.log_prob; meta= Location_span.empty} in
  let mir = {mir with log_prob= [mir_stmt]} in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  print_endline (Int.to_string num);
  [%expect
    {|
      log_prob {
        {
          FnPrint__(24, 24);
          if(13) {
            FnPrint__(244, 244);
            if(24) {
              FnPrint__(24, 24);
            }
          }
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      }



      3 |}]

let%expect_test "inline functions" =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        void f(int x, matrix y) {
          print(x);
          print(y);
        }
        real g(int z) {
          return z^2;
        }
      }
      model {
        f(3, [[3,2],[4,6]]);
        reject(g(53));
      }
      |}
  in
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      functions {
        void f(int x, matrix y) {
          {
            FnPrint__(x);
            FnPrint__(y);
          }
        }
        real g(int z) {
          {
            return promote((z ^ 2), real, var);
          }
        }
      }



      log_prob {
        {
          {
            FnPrint__(3);
            FnPrint__(FnMakeRowVec__(FnMakeRowVec__(promote(3, real, data),
                                                    promote(2, real, data)),
                                     FnMakeRowVec__(promote(4, real, data),
                                                    promote(6, real, data))));
          }
          real inline_g_return_sym2__;
          {
            inline_g_return_sym2__ = promote((53 ^ 2), real, var);
          }
          FnReject__(inline_g_return_sym2__);
        }
      }


      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "inline functions 2" =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        void f() {
        }
        void g() {
          f();
        }
      }
      generated quantities {
        g();
      }
      |}
  in
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      functions {
        void f() {
          {

          }
        }
        void g() {
          {
            f();
          }
        }
      }





      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
        {
          {

          }
        }
      } |}]

let%expect_test "list collapsing" =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        void f(int x, matrix y) {
          print(x);
          print(y);
        }
        real g(int z) {
          return z^2;
        }
      }
      model {
        f(3, [[3,2],[4,6]]);
        reject(g(53));
      }
      |}
  in
  let mir = function_inlining mir in
  let mir = list_collapsing mir in
  print_s [%sexp (mir : Middle.Program.Typed.t)];
  [%expect
    {|
((functions_block
  (((fdrt Void) (fdname f) (fdsuffix FnPlain)
    (fdargs ((AutoDiffable x UInt) (AutoDiffable y UMatrix)))
    (fdbody
     (((pattern
        (Block
         (((pattern
            (NRFunApp (CompilerInternal FnPrint)
             (((pattern (Var x))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
           (meta <opaque>))
          ((pattern
            (NRFunApp (CompilerInternal FnPrint)
             (((pattern (Var y))
               (meta ((type_ UMatrix) (loc <opaque>) (adlevel AutoDiffable)))))))
           (meta <opaque>)))))
       (meta <opaque>))))
    (fdloc <opaque>))
   ((fdrt (ReturnType UReal)) (fdname g) (fdsuffix FnPlain)
    (fdargs ((AutoDiffable z UInt)))
    (fdbody
     (((pattern
        (Block
         (((pattern
            (Return
             (((pattern
                (Promotion
                 ((pattern
                   (FunApp (StanLib Pow__ FnPlain AoS)
                    (((pattern (Var z))
                      (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                     ((pattern (Lit Int 2))
                      (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
                  (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))
                 UReal AutoDiffable))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
           (meta <opaque>)))))
       (meta <opaque>))))
    (fdloc <opaque>))))
 (input_vars ()) (prepare_data ())
 (log_prob
  (((pattern
     (Block
      (((pattern
         (Block
          (((pattern
             (NRFunApp (CompilerInternal FnPrint)
              (((pattern (Lit Int 3))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
            (meta <opaque>))
           ((pattern
             (NRFunApp (CompilerInternal FnPrint)
              (((pattern
                 (FunApp (CompilerInternal FnMakeRowVec)
                  (((pattern
                     (FunApp (CompilerInternal FnMakeRowVec)
                      (((pattern
                         (Promotion
                          ((pattern (Lit Int 3))
                           (meta
                            ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                          UReal DataOnly))
                        (meta
                         ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))
                       ((pattern
                         (Promotion
                          ((pattern (Lit Int 2))
                           (meta
                            ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                          UReal DataOnly))
                        (meta
                         ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
                    (meta
                     ((type_ URowVector) (loc <opaque>) (adlevel DataOnly))))
                   ((pattern
                     (FunApp (CompilerInternal FnMakeRowVec)
                      (((pattern
                         (Promotion
                          ((pattern (Lit Int 4))
                           (meta
                            ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                          UReal DataOnly))
                        (meta
                         ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))
                       ((pattern
                         (Promotion
                          ((pattern (Lit Int 6))
                           (meta
                            ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                          UReal DataOnly))
                        (meta
                         ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
                    (meta
                     ((type_ URowVector) (loc <opaque>) (adlevel DataOnly)))))))
                (meta ((type_ UMatrix) (loc <opaque>) (adlevel DataOnly)))))))
            (meta <opaque>)))))
        (meta <opaque>))
       ((pattern
         (Decl (decl_adtype AutoDiffable) (decl_id inline_g_return_sym2__)
          (decl_type (Sized SReal)) (initialize Uninit)))
        (meta <opaque>))
       ((pattern
         (Block
          (((pattern
             (Assignment ((LVariable inline_g_return_sym2__) ()) UReal
              ((pattern
                (Promotion
                 ((pattern
                   (FunApp (StanLib Pow__ FnPlain AoS)
                    (((pattern (Lit Int 53))
                      (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                     ((pattern (Lit Int 2))
                      (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
                  (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))
                 UReal AutoDiffable))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))))
            (meta <opaque>)))))
        (meta <opaque>))
       ((pattern
         (NRFunApp (CompilerInternal FnReject)
          (((pattern (Var inline_g_return_sym2__))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
        (meta <opaque>)))))
    (meta <opaque>))))
 (reverse_mode_log_prob ())
 (generate_quantities
  (((pattern
     (IfElse
      ((pattern (Var emit_transformed_parameters__))
       (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
      ((pattern Skip) (meta <opaque>))
      (((pattern (Block ())) (meta <opaque>)))))
    (meta <opaque>))
   ((pattern
     (IfElse
      ((pattern
        (FunApp (StanLib PNot__ FnPlain AoS)
         (((pattern
            (EOr
             ((pattern (Var emit_transformed_parameters__))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
             ((pattern (Var emit_generated_quantities__))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
       (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
      ((pattern (Return ())) (meta <opaque>)) ()))
    (meta <opaque>))
   ((pattern
     (IfElse
      ((pattern
        (FunApp (StanLib PNot__ FnPlain AoS)
         (((pattern (Var emit_generated_quantities__))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
       (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
      ((pattern (Return ())) (meta <opaque>)) ()))
    (meta <opaque>))))
 (transform_inits ()) (unconstrain_array ()) (output_vars ()) (prog_name "")
 (prog_path ""))
    |}]

let%expect_test "recursive functions" =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        int fib(int n) {
          if (n == 0 || n == 1) {
            return n;
          }
          return fib(n - 1) + fib(n - 2);
        }
      }
      model {
        reject(fib(5));
      }
      |}
  in
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      functions {
        int fib(int n) {
          {
            if((n == 0) || (n == 1)) {
              return n;
            }
            return (fib((n - 1)) + fib((n - 2)));
          }
        }
      }



      log_prob {
        {
          int inline_fib_return_sym1__;
          data int inline_fib_early_ret_check_sym2__;
          inline_fib_early_ret_check_sym2__ = 0;
          for(inline_fib_iterator_sym3__ in 1:1) {
            if((5 == 0)) ; else {

            }
            if((5 == 0) || (5 == 1)) {
              inline_fib_early_ret_check_sym2__ = 1;
              inline_fib_return_sym1__ = 5;
              break;
            }
            inline_fib_early_ret_check_sym2__ = 1;
            inline_fib_return_sym1__ = (fib((5 - 1)) + fib((5 - 2)));
            break;
          }
          FnReject__(inline_fib_return_sym1__);
        }
      }


      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "do not try to inline extern functions" =
  let mir =
    reset_and_mir_of_string
      {|
            functions {
              int fib(int n);
            }
            model {
              reject(fib(5));
            }
            |}
  in
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
            functions {
              extern int fib(int n);
            }



            log_prob {
              {
                FnReject__(fib(5));
              }
            }


            generate_quantities {
              if(emit_transformed_parameters__) ; else {

              }
              if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
              if(PNot__(emit_generated_quantities__)) return;
            } |}]

let%expect_test "inline function in for loop" =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        int f(int z) {
          print("f");
          return 42;
        }
        int g(int z) {
          print("g");
          return z + 24;
        }
      }
      model {
        for (i in f(2) : g(3)) print("body");
      }
      |}
  in
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      functions {
        int f(int z) {
          {
            FnPrint__("f");
            return 42;
          }
        }
        int g(int z) {
          {
            FnPrint__("g");
            return (z + 24);
          }
        }
      }



      log_prob {
        {
          int inline_f_return_sym1__;
          int inline_g_return_sym3__;
          {
            FnPrint__("f");
            inline_f_return_sym1__ = 42;
          }
          {
            FnPrint__("g");
            inline_g_return_sym3__ = (3 + 24);
          }
          for(i in inline_f_return_sym1__:inline_g_return_sym3__) {
            {
              FnPrint__("body");
            }
            {
              FnPrint__("g");
              inline_g_return_sym3__ = (3 + 24);
            }
          }
        }
      }


      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

(* TODO: check test results from here *)

let%expect_test "inline function in for loop 2" =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        int f(int z) {
          print("f");
          return 42;
        }
        int g(int z) {
          print("g");
          return f(z) + 24;
        }
      }
      model {
        for (i in f(2) : g(3)) print("body");
      }
      |}
  in
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      functions {
        int f(int z) {
          {
            FnPrint__("f");
            return 42;
          }
        }
        int g(int z) {
          {
            FnPrint__("g");
            return (f(z) + 24);
          }
        }
      }



      log_prob {
        {
          int inline_f_return_sym5__;
          int inline_g_return_sym7__;
          {
            FnPrint__("f");
            inline_f_return_sym5__ = 42;
          }
          {
            FnPrint__("g");
            int inline_g_inline_f_return_sym3___sym8__;
            {
              FnPrint__("f");
              inline_g_inline_f_return_sym3___sym8__ = 42;
            }
            inline_g_return_sym7__ = (inline_g_inline_f_return_sym3___sym8__ + 24);
          }
          for(i in inline_f_return_sym5__:inline_g_return_sym7__) {
            {
              FnPrint__("body");
            }
            {
              FnPrint__("g");
              int inline_g_inline_f_return_sym3___sym8__;
              {
                FnPrint__("f");
                inline_g_inline_f_return_sym3___sym8__ = 42;
              }
              inline_g_return_sym7__ = (inline_g_inline_f_return_sym3___sym8__ + 24);
            }
          }
        }
      }


      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "inline function in while loop" =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        int f(int z) {
          print("f");
          return 42;
        }
        int g(int z) {
          print("g");
          return z + 24;
        }
      }
      model {
        while (g(3)) print("body");
      }
      |}
  in
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      functions {
        int f(int z) {
          {
            FnPrint__("f");
            return 42;
          }
        }
        int g(int z) {
          {
            FnPrint__("g");
            return (z + 24);
          }
        }
      }



      log_prob {
        {
          int inline_g_return_sym1__;
          {
            FnPrint__("g");
            inline_g_return_sym1__ = (3 + 24);
          }
          while(inline_g_return_sym1__) {
            FnPrint__("body");
            {
              FnPrint__("g");
              inline_g_return_sym1__ = (3 + 24);
            }
          }
        }
      }


      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "inline function in if then else" =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        int f(int z) {
          print("f");
          return 42;
        }
        int g(int z) {
          print("g");
          return z + 24;
        }
      }
      model {
        if (g(3)) print("body");
      }
      |}
  in
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      functions {
        int f(int z) {
          {
            FnPrint__("f");
            return 42;
          }
        }
        int g(int z) {
          {
            FnPrint__("g");
            return (z + 24);
          }
        }
      }



      log_prob {
        {
          int inline_g_return_sym1__;
          {
            FnPrint__("g");
            inline_g_return_sym1__ = (3 + 24);
          }
          if(inline_g_return_sym1__) FnPrint__("body");
        }
      }


      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      }

    |}]

let%expect_test "inline function in ternary if " =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        int f(int z) {
          print("f");
          return 42;
        }
        int g(int z) {
          print("g");
          return z + 24;
        }
        int h(int z) {
          print("h");
          return z + 4;
        }
      }
      model {
        print(f(2) ? g(3) : h(4));
      }
      |}
  in
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      functions {
        int f(int z) {
          {
            FnPrint__("f");
            return 42;
          }
        }
        int g(int z) {
          {
            FnPrint__("g");
            return (z + 24);
          }
        }
        int h(int z) {
          {
            FnPrint__("h");
            return (z + 4);
          }
        }
      }



      log_prob {
        {
          int inline_f_return_sym1__;
          int inline_g_return_sym3__;
          int inline_h_return_sym5__;
          {
            FnPrint__("f");
            inline_f_return_sym1__ = 42;
          }
          if(inline_f_return_sym1__) {
            {
              FnPrint__("g");
              inline_g_return_sym3__ = (3 + 24);
            }
          } else {
            {
              FnPrint__("h");
              inline_h_return_sym5__ = (4 + 4);
            }
          }
          FnPrint__((inline_f_return_sym1__ ? inline_g_return_sym3__ :
                     inline_h_return_sym5__));
        }
      }


      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "inline function multiple returns " =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        int f(int z) {
          if (2) {
            print("f");
            return 42;
          }
          return 6;
        }
      }
      model {
        print(f(2));
      }
      |}
  in
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      functions {
        int f(int z) {
          {
            if(2) {
              FnPrint__("f");
              return 42;
            }
            return 6;
          }
        }
      }



      log_prob {
        {
          int inline_f_return_sym1__;
          data int inline_f_early_ret_check_sym2__;
          inline_f_early_ret_check_sym2__ = 0;
          for(inline_f_iterator_sym3__ in 1:1) {
            if(2) {
              FnPrint__("f");
              inline_f_early_ret_check_sym2__ = 1;
              inline_f_return_sym1__ = 42;
              break;
            }
            inline_f_early_ret_check_sym2__ = 1;
            inline_f_return_sym1__ = 6;
            break;
          }
          FnPrint__(inline_f_return_sym1__);
        }
      }


      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "inline function indices " =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        int f(int z) {
          print(z);
          return 42;
        }
      }
      model {
        array[2, 2] int a;
        print(a[f(1), f(2)]);
      }
      |}
  in
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      functions {
        int f(int z) {
          {
            FnPrint__(z);
            return 42;
          }
        }
      }



      log_prob {
        {
          array[array[int, 2], 2] a;
          int inline_f_return_sym3__;
          int inline_f_return_sym1__;
          {
            FnPrint__(2);
            inline_f_return_sym3__ = 42;
          }
          {
            FnPrint__(1);
            inline_f_return_sym1__ = 42;
          }
          FnPrint__(a[inline_f_return_sym1__, inline_f_return_sym3__]);
        }
      }


      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "inline function and " =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        int f(int z) {
          print(z);
          return 42;
        }
      }
      model {
        print(f(1) && f(2));
      }
      |}
  in
  (* TODO: these declarations are still in the wrong place *)
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      functions {
        int f(int z) {
          {
            FnPrint__(z);
            return 42;
          }
        }
      }



      log_prob {
        {
          int inline_f_return_sym1__;
          int inline_f_return_sym3__;
          {
            FnPrint__(1);
            inline_f_return_sym1__ = 42;
          }
          if(inline_f_return_sym1__) {
            {
              FnPrint__(2);
              inline_f_return_sym3__ = 42;
            }
          }
          FnPrint__(inline_f_return_sym1__ && inline_f_return_sym3__);
        }
      }


      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "inline function or " =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        int f(int z) {
          print(z);
          return 42;
        }
      }
      model {
        print(f(1) || f(2));
      }
      |}
  in
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      functions {
        int f(int z) {
          {
            FnPrint__(z);
            return 42;
          }
        }
      }



      log_prob {
        {
          int inline_f_return_sym1__;
          int inline_f_return_sym3__;
          {
            FnPrint__(1);
            inline_f_return_sym1__ = 42;
          }
          if(inline_f_return_sym1__) ; else {
            {
              FnPrint__(2);
              inline_f_return_sym3__ = 42;
            }
          }
          FnPrint__(inline_f_return_sym1__ || inline_f_return_sym3__);
        }
      }


      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "unroll nested loop" =
  let mir =
    reset_and_mir_of_string
      {|      model {
                for (i in 1:2)
                  for (j in 3:4)
                    print(i, j);
                   }
      |}
  in
  let mir = static_loop_unrolling mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          {
            {
              FnPrint__(1, 3);
            }
            {
              FnPrint__(1, 4);
            }
          }
          {
            {
              FnPrint__(2, 3);
            }
            {
              FnPrint__(2, 4);
            }
          }
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "unroll nested loop 2" =
  let mir =
    reset_and_mir_of_string
      {|      model {
                for (i in 1:2)
                  for (j in i:4)
                    for (k in j:9)
                       print(i, j, k);
                   }
      |}
  in
  let mir = static_loop_unrolling mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          {
            {
              {
                FnPrint__(1, 1, 1);
              }
              {
                FnPrint__(1, 1, 2);
              }
              {
                FnPrint__(1, 1, 3);
              }
              {
                FnPrint__(1, 1, 4);
              }
              {
                FnPrint__(1, 1, 5);
              }
              {
                FnPrint__(1, 1, 6);
              }
              {
                FnPrint__(1, 1, 7);
              }
              {
                FnPrint__(1, 1, 8);
              }
              {
                FnPrint__(1, 1, 9);
              }
            }
            {
              {
                FnPrint__(1, 2, 2);
              }
              {
                FnPrint__(1, 2, 3);
              }
              {
                FnPrint__(1, 2, 4);
              }
              {
                FnPrint__(1, 2, 5);
              }
              {
                FnPrint__(1, 2, 6);
              }
              {
                FnPrint__(1, 2, 7);
              }
              {
                FnPrint__(1, 2, 8);
              }
              {
                FnPrint__(1, 2, 9);
              }
            }
            {
              {
                FnPrint__(1, 3, 3);
              }
              {
                FnPrint__(1, 3, 4);
              }
              {
                FnPrint__(1, 3, 5);
              }
              {
                FnPrint__(1, 3, 6);
              }
              {
                FnPrint__(1, 3, 7);
              }
              {
                FnPrint__(1, 3, 8);
              }
              {
                FnPrint__(1, 3, 9);
              }
            }
            {
              {
                FnPrint__(1, 4, 4);
              }
              {
                FnPrint__(1, 4, 5);
              }
              {
                FnPrint__(1, 4, 6);
              }
              {
                FnPrint__(1, 4, 7);
              }
              {
                FnPrint__(1, 4, 8);
              }
              {
                FnPrint__(1, 4, 9);
              }
            }
          }
          {
            {
              {
                FnPrint__(2, 2, 2);
              }
              {
                FnPrint__(2, 2, 3);
              }
              {
                FnPrint__(2, 2, 4);
              }
              {
                FnPrint__(2, 2, 5);
              }
              {
                FnPrint__(2, 2, 6);
              }
              {
                FnPrint__(2, 2, 7);
              }
              {
                FnPrint__(2, 2, 8);
              }
              {
                FnPrint__(2, 2, 9);
              }
            }
            {
              {
                FnPrint__(2, 3, 3);
              }
              {
                FnPrint__(2, 3, 4);
              }
              {
                FnPrint__(2, 3, 5);
              }
              {
                FnPrint__(2, 3, 6);
              }
              {
                FnPrint__(2, 3, 7);
              }
              {
                FnPrint__(2, 3, 8);
              }
              {
                FnPrint__(2, 3, 9);
              }
            }
            {
              {
                FnPrint__(2, 4, 4);
              }
              {
                FnPrint__(2, 4, 5);
              }
              {
                FnPrint__(2, 4, 6);
              }
              {
                FnPrint__(2, 4, 7);
              }
              {
                FnPrint__(2, 4, 8);
              }
              {
                FnPrint__(2, 4, 9);
              }
            }
          }
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "unroll nested loop 3" =
  let mir =
    reset_and_mir_of_string
      {|      model {
                for (i in 1:2)
                  for (j in i:4)
                    for (k in j:i+j)
                       print(i, j, k);
                   }
      |}
  in
  let mir = static_loop_unrolling mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          {
            {
              {
                FnPrint__(1, 1, 1);
              }
              {
                FnPrint__(1, 1, 2);
              }
            }
            {
              {
                FnPrint__(1, 2, 2);
              }
              {
                FnPrint__(1, 2, 3);
              }
            }
            {
              {
                FnPrint__(1, 3, 3);
              }
              {
                FnPrint__(1, 3, 4);
              }
            }
            {
              {
                FnPrint__(1, 4, 4);
              }
              {
                FnPrint__(1, 4, 5);
              }
            }
          }
          {
            {
              {
                FnPrint__(2, 2, 2);
              }
              {
                FnPrint__(2, 2, 3);
              }
              {
                FnPrint__(2, 2, 4);
              }
            }
            {
              {
                FnPrint__(2, 3, 3);
              }
              {
                FnPrint__(2, 3, 4);
              }
              {
                FnPrint__(2, 3, 5);
              }
            }
            {
              {
                FnPrint__(2, 4, 4);
              }
              {
                FnPrint__(2, 4, 5);
              }
              {
                FnPrint__(2, 4, 6);
              }
            }
          }
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "unroll nested loop with break" =
  let mir =
    reset_and_mir_of_string
      {|      model {
                for (i in 1:2)
                  for (j in 3:4) {
                    print(i);
                    break;
                  }
              }
      |}
  in
  let mir = static_loop_unrolling mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          {
            for(j in 3:4) {
              FnPrint__(1);
              break;
            }
          }
          {
            for(j in 3:4) {
              FnPrint__(2);
              break;
            }
          }
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "constant propagation" =
  let mir =
    reset_and_mir_of_string
      {|
      transformed data {
        int i;
        i = 42;
        int j;
        j = 2 + i;
      }
      model {
        for (x in 1:i) {
          print(i + j);
        }
      }
      |}
  in
  let mir = constant_propagation mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
    prepare_data {
      data int i;
      i = 42;
      data int j;
      j = (2 + 42);
    }

    log_prob {
      {
        for(x in 1:42) {
          FnPrint__((42 + 44));
        }
      }
    }


    generate_quantities {
      if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
      if(PNot__(emit_generated_quantities__)) return;
    } |}]

let%expect_test "constant propagation, local scope" =
  let mir =
    reset_and_mir_of_string
      {|
      transformed data {
        int i;
        i = 42;
        {
          int j;
          j = 2;
        }
      }
      model {
        int j;
        for (x in 1:i) {
          print(i + j);
        }
      }
      |}
  in
  let mir = constant_propagation mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
    prepare_data {
      data int i;
      i = 42;
      {
        data int j;
        j = 2;
      }
    }

    log_prob {
      {
        int j;
        for(x in 1:42) {
          FnPrint__((42 + j));
        }
      }
    }


    generate_quantities {
      if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
      if(PNot__(emit_generated_quantities__)) return;
    } |}]

let%expect_test "constant propagation, model block local scope" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int i;
        i = 42;
        int j;
        j = 2;
      }
      generated quantities {
        int i;
        int j;
        for (x in 1:i) {
          print(i + j);
        }
      }
      |}
  in
  let mir = constant_propagation mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
    log_prob {
      {
        int i;
        i = 42;
        int j;
        j = 2;
      }
    }


    generate_quantities {
      if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
      if(PNot__(emit_generated_quantities__)) return;
      data int i;
      data int j;
      for(x in 1:i) {
        FnPrint__((i + j));
      }
    }


    output_vars {
      generated_quantities int i; //int
      generated_quantities int j; //int
    } |}]

let%expect_test "expression propagation" =
  let mir =
    reset_and_mir_of_string
      {|
      transformed data {
        int i;
        int j;
        j = 2 + i;
      }
      model {
        for (x in 1:i) {
          print(i + j);
        }
      }
      |}
  in
  let mir = expression_propagation mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      prepare_data {
        data int i;
        data int j;
        j = (2 + i);
      }

      log_prob {
        {
          for(x in 1:i) {
            FnPrint__((i + (2 + i)));
          }
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "copy propagation" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int i;
        int j;
        j = i;
        int k;
        k = 2 * j;
        for (x in 1:i) {
          print(i + j + k);
        }
      }
      |}
  in
  let mir = copy_propagation mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          int i;
          int j;
          j = i;
          int k;
          k = (2 * i);
          for(x in 1:i) {
            FnPrint__(((i + i) + k));
          }
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "dead code elimination" =
  let mir =
    reset_and_mir_of_string
      {|
      transformed data {
        array[2] int i;
        i[1] = 2;
        i = {3, 2};
        array[2] int j;
        j = {3, 2};
        j[1] = 2;
      }
      model {
        print(i);
        print(j);
      }
      |}
  in
  let mir = dead_code_elimination mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      prepare_data {
        data array[int, 2] i;
        i[1] = 2;
        i = FnMakeArray__(3, 2);
        data array[int, 2] j;
        j = FnMakeArray__(3, 2);
        j[1] = 2;
      }

      log_prob {
        {
          FnPrint__(i);
          FnPrint__(j);
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "dead code elimination decl" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int i;
        i = 4;
      }
      generated quantities {
        {
          int i;
          print(i);
        }
      }
      |}
  in
  let mir = dead_code_elimination mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          int i;
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
        {
          data int i;
          FnPrint__(i);
        }
      } |}]

let%expect_test "dead code elimination, for loop" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int i;
        print(i);
        for (j in 3:5);
      }
      |}
  in
  let mir = dead_code_elimination mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          int i;
          FnPrint__(i);
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "dead code elimination, while loop" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int i;
        print(i);
        while (0) {
          print(13);
        };
        while (1) {
        }
      }
      |}
  in
  let mir = dead_code_elimination mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          int i;
          FnPrint__(i);
          while(1) ;
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "dead code elimination, if then" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int i;
        print(i);
        if (1) {
          print("hello");
        } else {
          print("goodbye");
        }
        if (0) {
          print("hello");
        } else {
          print("goodbye");
        }
        if (i) {

        } else {

        }
      }
      |}
  in
  let mir = dead_code_elimination mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          int i;
          FnPrint__(i);
          {
            FnPrint__("hello");
          }
          {
            FnPrint__("goodbye");
          }
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "dead code elimination, nested" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int i;
        print(i);
        for (j in 3:5) {
          for (k in 34:2);
        }
      }
      |}
  in
  let mir = dead_code_elimination mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          int i;
          FnPrint__(i);
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "dead code elimination, real zero if (direct MIR)" =
  (* Construct MIR directly because Stan's type checker rejects real in
     if-conditions, but optimization passes can produce Lit(Real, "0.") there.
     Expr.Helpers.float 0.0 produces Lit(Real, "0.") via Float.to_string. *)
  let mir = reset_and_mir_of_string {|
      model {}
      |} in
  let real_zero = Expr.Helpers.float 0.0 in
  let print_hello =
    Stmt.
      { pattern= NRFunApp (CompilerInternal FnPrint, [Expr.Helpers.str "hello"])
      ; meta= Location_span.empty } in
  let print_goodbye =
    Stmt.
      { pattern=
          NRFunApp (CompilerInternal FnPrint, [Expr.Helpers.str "goodbye"])
      ; meta= Location_span.empty } in
  let mir =
    { mir with
      Middle.Program.log_prob=
        [ Stmt.
            { pattern= IfElse (real_zero, print_hello, Some print_goodbye)
            ; meta= Location_span.empty } ] } in
  let mir = dead_code_elimination mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        FnPrint__("goodbye");
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "dead code elimination, real zero while (direct MIR)" =
  let mir = reset_and_mir_of_string {|
      model {}
      |} in
  let real_zero = Expr.Helpers.float 0.0 in
  let print_hello =
    Stmt.
      { pattern= NRFunApp (CompilerInternal FnPrint, [Expr.Helpers.str "hello"])
      ; meta= Location_span.empty } in
  let mir =
    { mir with
      Middle.Program.log_prob=
        [ Stmt.
            {pattern= While (real_zero, print_hello); meta= Location_span.empty}
        ] } in
  let mir = dead_code_elimination mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "dead code elimination, real zero if no else (direct MIR)" =
  (* Test Lit(Real, "0.") in if with no else branch *)
  let mir = reset_and_mir_of_string {|
      model {}
      |} in
  let real_zero = Expr.Helpers.float 0.0 in
  let print_hello =
    Stmt.
      { pattern= NRFunApp (CompilerInternal FnPrint, [Expr.Helpers.str "hello"])
      ; meta= Location_span.empty } in
  let mir =
    { mir with
      Middle.Program.log_prob=
        [ Stmt.
            { pattern= IfElse (real_zero, print_hello, None)
            ; meta= Location_span.empty } ] } in
  let mir = dead_code_elimination mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "partial evaluation" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        if (1 > 2) {
          int i;
          print(1+2);
          print(i + (1+2));
          print(log(1-i));
        }
      }
      |}
  in
  let mir = partial_evaluation mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          if(0) {
            int i;
            FnPrint__(3);
            FnPrint__((i + 3));
            FnPrint__(log1m(i));
          }
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "partial evaluate reject" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int x = 5 %/% 0;
      }
      |} in
  let mir = partial_evaluation mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          int x;
          FnReject__("Integer division by zero");
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "try partially evaluate" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        real x;
        real y;
        vector[2] a;
        vector[2] b;
        print(log(exp(x)-exp(y)));
        print(log(exp(a)-exp(b)));
      }
      |}
  in
  let mir = partial_evaluation mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          real x;
          real y;
          vector[2] a;
          vector[2] b;
          FnPrint__(log_diff_exp(x, y));
          FnPrint__(log_diff_exp(a, b));
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "partially evaluate with equality check" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        vector[2] x;
        vector[2] y;
        print(dot_product(x, x));
        print(dot_product(x, y));
      }
      |}
  in
  let mir = partial_evaluation mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        {
          vector[2] x;
          vector[2] y;
          FnPrint__(dot_self(x));
          FnPrint__(dot_product(x, y));
        }
      }


      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "partially evaluate functions" =
  let mir =
    reset_and_mir_of_string
      {|
parameters {
    matrix[3, 2] x_matrix;
    matrix[2, 4] y_matrix;
    matrix[4, 2] z_matrix;
    vector[2] x_vector;
    vector[3] y_vector;
    cov_matrix[2] x_cov;
    real theta_u;
    real phi_u;
}
model {
    real theta = 34.;
    real phi = 5.;
    real x;
    int i = 23;
    int j = 32;
    array[3] int y_arr = {32, 2, 35};
    target += +i;
    target += -i;
    target += !i;
    target += +theta;
    target += -theta;
    target += i+j;
    target += i-j;
    target += i*j;
    target += i%/%j;
    target += i==j;
    target += i!=j;
    target += i<j;
    target += i<=j;
    target += i>j;
    target += i>=j;
    target += i && j;
    target += i || j;
    target += theta + phi;
    target += theta - phi;
    target += theta * phi;
    target += theta / phi;
    target += theta == phi;
    target += theta != phi;
    target += theta <= phi;
    target += theta < phi;
    target += theta > phi;
    target += theta >= phi;
    target += theta != 0 && phi != 0;
    target += theta != 0 || phi != 0 ;
    target += bernoulli_lpmf(y_arr| inv_logit(theta + x_matrix * x_vector));
    target += bernoulli_lpmf(y_arr| inv_logit(x_matrix * x_vector + theta));
    target += bernoulli_lpmf(y_arr| inv_logit(x_matrix * x_vector));
    target += bernoulli_lupmf(y_arr| inv_logit(theta + x_matrix * x_vector));
    target += bernoulli_lupmf(y_arr| inv_logit(x_matrix * x_vector + theta));
    target += bernoulli_lupmf(y_arr| inv_logit(x_matrix * x_vector));
    target += bernoulli_logit_lpmf(y_arr| (theta + x_matrix * x_vector));
    target += bernoulli_logit_lpmf(y_arr| (x_matrix * x_vector + theta));
    target += bernoulli_logit_lpmf(y_arr| (x_matrix * x_vector));
    target += bernoulli_logit_lupmf(y_arr| (theta + x_matrix * x_vector));
    target += bernoulli_logit_lupmf(y_arr| (x_matrix * x_vector + theta));
    target += bernoulli_logit_lupmf(y_arr| (x_matrix * x_vector));
    target += bernoulli_lpmf(y_arr| inv_logit(x_vector));
    target += bernoulli_lupmf(y_arr| inv_logit(x_vector));
    target += binomial_lpmf(y_arr| j, inv_logit(x_vector));
    target += binomial_lupmf(y_arr| j, inv_logit(x_vector));
    target += categorical_lpmf(y_arr| inv_logit(x_vector));
    target += categorical_lupmf(y_arr| inv_logit(x_vector));
    target += columns_dot_product(x_matrix, x_matrix);
    target += dot_product(x_vector, x_vector);
    target += inv(sqrt(x_vector));
    target += inv(square(x_vector));
    target += log(1 - exp(x_vector));
    target += log(1 - inv_logit(x_vector));
    target += log(1 - x_matrix);
    target += log(1. - exp(x_vector));
    target += log(1. - inv_logit(x_vector));
    target += log(1. - x_matrix);
    target += log(1 + exp(x_vector));
    target += log(1 + x_matrix);
    target += log(abs(determinant(x_matrix)));
    target += log(exp(theta) - exp(theta));
    target += log(falling_factorial(phi, i));
    target += log(rising_factorial(phi, i));
    target += log(inv_logit(theta));
    target += log(softmax(x_vector));
    target += log(sum(exp(x_vector)));
    target += log(exp(theta_u) + exp(phi_u));
    target += multi_normal_lpdf(x_vector| x_vector, inverse(x_cov));
    target += multi_normal_lupdf(x_vector| x_vector, inverse(x_cov));
    target += neg_binomial_2_lpmf(y_arr| exp(theta + x_matrix * x_vector), phi);
    target += neg_binomial_2_lpmf(y_arr| exp(x_matrix * x_vector + theta), phi);
    target += neg_binomial_2_lpmf(y_arr| exp(x_matrix * x_vector), phi);
    target += neg_binomial_2_lupmf(y_arr| exp(theta + x_matrix * x_vector), phi);
    target += neg_binomial_2_lupmf(y_arr| exp(x_matrix * x_vector + theta), phi);
    target += neg_binomial_2_lupmf(y_arr| exp(x_matrix * x_vector), phi);
    target += neg_binomial_2_log_lpmf(y_arr| (theta + x_matrix * x_vector), phi);
    target += neg_binomial_2_log_lpmf(y_arr| (x_matrix * x_vector + theta), phi);
    target += neg_binomial_2_log_lpmf(y_arr| (x_matrix * x_vector), phi);
    target += neg_binomial_2_log_lupmf(y_arr| (theta + x_matrix * x_vector), phi);
    target += neg_binomial_2_log_lupmf(y_arr| (x_matrix * x_vector + theta), phi);
    target += neg_binomial_2_log_lupmf(y_arr| (x_matrix * x_vector), phi);
    target += neg_binomial_2_lpmf(y_arr| exp(theta), phi);
    target += neg_binomial_2_lupmf(y_arr| exp(theta), phi);
    target += normal_lpdf(y_vector| theta + x_matrix * x_vector, phi);
    target += normal_lpdf(y_vector| x_matrix * x_vector + theta, phi);
    target += normal_lpdf(y_vector| x_matrix * x_vector, phi);
    target += normal_lupdf(y_vector| theta + x_matrix * x_vector, phi);
    target += normal_lupdf(y_vector| x_matrix * x_vector + theta, phi);
    target += normal_lupdf(y_vector| x_matrix * x_vector, phi);
    target += poisson_lpmf(y_arr| exp(theta + x_matrix * x_vector));
    target += poisson_lpmf(y_arr| exp(x_matrix * x_vector + theta));
    target += poisson_lpmf(y_arr| exp(x_matrix * x_vector));
    target += poisson_lupmf(y_arr| exp(theta + x_matrix * x_vector));
    target += poisson_lupmf(y_arr| exp(x_matrix * x_vector + theta));
    target += poisson_lupmf(y_arr| exp(x_matrix * x_vector));
    target += poisson_log_lpmf(y_arr| (theta + x_matrix * x_vector));
    target += poisson_log_lpmf(y_arr| (x_matrix * x_vector + theta));
    target += poisson_log_lpmf(y_arr| (x_matrix * x_vector));
    target += poisson_log_lupmf(y_arr| (theta + x_matrix * x_vector));
    target += poisson_log_lupmf(y_arr| (x_matrix * x_vector + theta));
    target += poisson_log_lupmf(y_arr| (x_matrix * x_vector));
    target += poisson_lpmf(y_arr| exp(x_vector));
    target += poisson_lupmf(y_arr| exp(x_vector));
    target += pow(2, theta);
    target += pow(theta, 2);
    target += pow(theta, 0.5);
    target += pow(theta, 1./2.);
    target += pow(theta, 1/2.);
    target += pow(theta, 1./2);
    target += pow(theta, 1/2);
    target += square(sd(x_vector));
    target += sqrt(2);
    target += sum(square(x_vector - y_vector));
    target += sum(diagonal(x_matrix));
    target += trace(x_matrix * transpose(y_matrix) * z_matrix * y_matrix);
    target += trace(quad_form(y_matrix, z_matrix));
    target += 1 - erf(x_vector);
    target += 1. - erf(x_vector);
    target += 1 - erfc(x_vector);
    target += 1. - erfc(x_vector);
    target += exp(x_vector) - 1;
    target += exp(x_vector) - 1.;
    target += 1 - gamma_p(theta, phi);
    target += 1. - gamma_p(theta, phi);
    target += 1 - gamma_q(theta, phi);
    target += 1. - gamma_q(theta, phi);
    target += matrix_exp(theta * x_matrix) * y_matrix;
    target += matrix_exp(x_matrix * theta) * y_matrix;
    target += matrix_exp(x_matrix) * y_matrix;
    target += phi * log(theta);
    target += log(theta) * phi;
    target += diag_matrix(x_vector) * x_cov * diag_matrix(x_vector);
    target += diag_matrix(x_vector) * (x_cov * diag_matrix(x_vector));
    target += transpose(x_vector) * x_cov * x_vector;
    target += transpose(x_vector) * (x_cov * x_vector);
    target += diag_matrix(x_vector) * x_cov;
    target += x_cov * diag_matrix(x_vector);
    target += 0 ? x_vector : y_vector;
    target += 7 ? x_vector : y_vector;
    }
      |}
  in
  let mir = constant_propagation mir in
  let mir = partial_evaluation mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        matrix[3, 2] x_matrix;
        matrix[2, 4] y_matrix;
        matrix[4, 2] z_matrix;
        vector[2] x_vector;
        vector[3] y_vector;
        matrix[2, 2] x_cov;
        real theta_u;
        real phi_u;
        {
          real theta;
          theta = 34.;
          real phi;
          phi = 5.;
          real x;
          int i;
          i = 23;
          int j;
          j = 32;
          array[int, 3] y_arr;
          y_arr = FnMakeArray__(32, 2, 35);
          target += 23;
          target += -23;
          target += 0;
          target += 34.;
          target += -34.;
          target += 55;
          target += -9;
          target += 736;
          target += 0;
          target += 0;
          target += 1;
          target += 1;
          target += 1;
          target += 0;
          target += 0;
          target += 1;
          target += 1;
          target += 39.;
          target += 29.;
          target += 170.;
          target += 6.8;
          target += 0;
          target += 1;
          target += 0;
          target += 0;
          target += 1;
          target += 1;
          target += 1;
          target += 1;
          target += bernoulli_logit_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += bernoulli_logit_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += bernoulli_logit_glm_lpmf(y_arr, x_matrix, 0, x_vector);
          target += bernoulli_logit_glm_lupmf(y_arr, x_matrix, 34., x_vector);
          target += bernoulli_logit_glm_lupmf(y_arr, x_matrix, 34., x_vector);
          target += bernoulli_logit_glm_lupmf(y_arr, x_matrix, 0, x_vector);
          target += bernoulli_logit_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += bernoulli_logit_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += bernoulli_logit_glm_lpmf(y_arr, x_matrix, 0, x_vector);
          target += bernoulli_logit_glm_lupmf(y_arr, x_matrix, 34., x_vector);
          target += bernoulli_logit_glm_lupmf(y_arr, x_matrix, 34., x_vector);
          target += bernoulli_logit_glm_lupmf(y_arr, x_matrix, 0, x_vector);
          target += bernoulli_logit_lpmf(y_arr, x_vector);
          target += bernoulli_logit_lupmf(y_arr, x_vector);
          target += binomial_logit_lpmf(y_arr, 32, x_vector);
          target += binomial_logit_lupmf(y_arr, 32, x_vector);
          target += categorical_logit_lpmf(y_arr, x_vector);
          target += categorical_logit_lupmf(y_arr, x_vector);
          target += columns_dot_self(x_matrix);
          target += dot_self(x_vector);
          target += inv_sqrt(x_vector);
          target += inv_square(x_vector);
          target += log1m_exp(x_vector);
          target += log1m_inv_logit(x_vector);
          target += log1m(x_matrix);
          target += log1m_exp(x_vector);
          target += log1m_inv_logit(x_vector);
          target += log1m(x_matrix);
          target += log1p_exp(x_vector);
          target += log1p(x_matrix);
          target += log_determinant(x_matrix);
          target += log_diff_exp(34., 34.);
          target += log_falling_factorial(5., 23);
          target += log_rising_factorial(5., 23);
          target += log_inv_logit(34.);
          target += log_softmax(x_vector);
          target += log_sum_exp(x_vector);
          target += log_sum_exp(theta_u, phi_u);
          target += multi_normal_prec_lpdf(x_vector, x_vector, x_cov);
          target += multi_normal_prec_lupdf(x_vector, x_vector, x_cov);
          target += neg_binomial_2_log_glm_lpmf(y_arr, x_matrix, 34., x_vector, 5.);
          target += neg_binomial_2_log_glm_lpmf(y_arr, x_matrix, 34., x_vector, 5.);
          target += neg_binomial_2_log_glm_lpmf(y_arr, x_matrix, 0, x_vector, 5.);
          target += neg_binomial_2_log_glm_lupmf(y_arr, x_matrix, 34., x_vector, 5.);
          target += neg_binomial_2_log_glm_lupmf(y_arr, x_matrix, 34., x_vector, 5.);
          target += neg_binomial_2_log_glm_lupmf(y_arr, x_matrix, 0, x_vector, 5.);
          target += neg_binomial_2_log_glm_lpmf(y_arr, x_matrix, 34., x_vector, 5.);
          target += neg_binomial_2_log_glm_lpmf(y_arr, x_matrix, 34., x_vector, 5.);
          target += neg_binomial_2_log_glm_lpmf(y_arr, x_matrix, 0, x_vector, 5.);
          target += neg_binomial_2_log_glm_lupmf(y_arr, x_matrix, 34., x_vector, 5.);
          target += neg_binomial_2_log_glm_lupmf(y_arr, x_matrix, 34., x_vector, 5.);
          target += neg_binomial_2_log_glm_lupmf(y_arr, x_matrix, 0, x_vector, 5.);
          target += neg_binomial_2_log_lpmf(y_arr, 34., 5.);
          target += neg_binomial_2_log_lupmf(y_arr, 34., 5.);
          target += normal_id_glm_lpdf(y_vector, x_matrix, 34., x_vector, 5.);
          target += normal_id_glm_lpdf(y_vector, x_matrix, 34., x_vector, 5.);
          target += normal_id_glm_lpdf(y_vector, x_matrix, 0, x_vector, 5.);
          target += normal_id_glm_lupdf(y_vector, x_matrix, 34., x_vector, 5.);
          target += normal_id_glm_lupdf(y_vector, x_matrix, 34., x_vector, 5.);
          target += normal_id_glm_lupdf(y_vector, x_matrix, 0, x_vector, 5.);
          target += poisson_log_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += poisson_log_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += poisson_log_glm_lpmf(y_arr, x_matrix, 0, x_vector);
          target += poisson_log_glm_lupmf(y_arr, x_matrix, 34., x_vector);
          target += poisson_log_glm_lupmf(y_arr, x_matrix, 34., x_vector);
          target += poisson_log_glm_lupmf(y_arr, x_matrix, 0, x_vector);
          target += poisson_log_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += poisson_log_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += poisson_log_glm_lpmf(y_arr, x_matrix, 0, x_vector);
          target += poisson_log_glm_lupmf(y_arr, x_matrix, 34., x_vector);
          target += poisson_log_glm_lupmf(y_arr, x_matrix, 34., x_vector);
          target += poisson_log_glm_lupmf(y_arr, x_matrix, 0, x_vector);
          target += poisson_log_lpmf(y_arr, x_vector);
          target += poisson_log_lupmf(y_arr, x_vector);
          target += exp2(34.);
          target += square(34.);
          target += sqrt(34.);
          target += sqrt(34.);
          target += sqrt(34.);
          target += sqrt(34.);
          target += pow(34., 0);
          target += variance(x_vector);
          target += sqrt2();
          target += squared_distance(x_vector, y_vector);
          target += trace(x_matrix);
          target += trace_gen_quad_form(x_matrix, z_matrix, y_matrix);
          target += trace_quad_form(y_matrix, z_matrix);
          target += erfc(x_vector);
          target += erfc(x_vector);
          target += erf(x_vector);
          target += erf(x_vector);
          target += expm1(x_vector);
          target += expm1(x_vector);
          target += gamma_q(34., 5.);
          target += gamma_q(34., 5.);
          target += gamma_p(34., 5.);
          target += gamma_p(34., 5.);
          target += scale_matrix_exp_multiply(34., x_matrix, y_matrix);
          target += scale_matrix_exp_multiply(34., x_matrix, y_matrix);
          target += matrix_exp_multiply(x_matrix, y_matrix);
          target += lmultiply(5., 34.);
          target += lmultiply(5., 34.);
          target += quad_form_diag(x_cov, x_vector);
          target += quad_form_diag(x_cov, x_vector);
          target += quad_form(x_cov, x_vector);
          target += quad_form(x_cov, x_vector);
          target += diag_pre_multiply(x_vector, x_cov);
          target += diag_post_multiply(x_cov, x_vector);
          target += y_vector;
          target += x_vector;
        }
      }


      generate_quantities {
        data matrix[3, 2] x_matrix;
        data matrix[2, 4] y_matrix;
        data matrix[4, 2] z_matrix;
        data vector[2] x_vector;
        data vector[3] y_vector;
        data matrix[2, 2] x_cov;
        data real theta_u;
        data real phi_u;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      }


      output_vars {
        parameters matrix[3, 2] x_matrix; //matrix[3, 2]
        parameters matrix[2, 4] y_matrix; //matrix[2, 4]
        parameters matrix[4, 2] z_matrix; //matrix[4, 2]
        parameters vector[2] x_vector; //vector[2]
        parameters vector[3] y_vector; //vector[3]
        parameters matrix[2, 2] x_cov; //vector[3]
        parameters real theta_u; //real
        parameters real phi_u; //real
      } |}]

let%expect_test "lazy code motion" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        print({3.0});
        print({3.0});
        print({3.0});
      }
      |}
  in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
    log_prob {
      data array[] real lcm_sym3__;
      {
        lcm_sym3__ = FnMakeArray__(3.0);
        FnPrint__(lcm_sym3__);
        FnPrint__(lcm_sym3__);
        FnPrint__(lcm_sym3__);
      }
    }


    generate_quantities {
      data int lcm_sym2__;
      data int lcm_sym1__;
      if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
      if(PNot__(emit_generated_quantities__)) return;
    } |}]

let%expect_test "lazy code motion, 2" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        for (i in 1:2)
          print(3 + 4);
      }
      |}
  in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        data int lcm_sym3__;
        {
          for(i in 1:2) {
            FnPrint__((3 + 4));
          }
        }
      }


      generate_quantities {
        data int lcm_sym2__;
        data int lcm_sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "lazy code motion, 3" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        print(3);
        print(3 + 5);
        print((3 + 5) + 7);
      }
      |}
  in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        data int lcm_sym4__;
        data int lcm_sym3__;
        {
          FnPrint__(3);
          lcm_sym3__ = (3 + 5);
          FnPrint__(lcm_sym3__);
          FnPrint__((lcm_sym3__ + 7));
        }
      }


      generate_quantities {
        data int lcm_sym2__;
        data int lcm_sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "lazy code motion, 4" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int b;
        int c;
        int x;
        int y;
        b = 1;
        if (1) {
          ;
          ;
          ;
        } else {
          x = b + c;
          ;
        }
        y = b + c;
      }
      |}
  in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  (* TODO: make sure that these temporaries do not get assigned level DataOnly
     unless appropriate *)
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        data int lcm_sym3__;
        {
          int b;
          int c;
          int x;
          int y;
          b = 1;
          if(1) {
            {
              ;
              ;
              ;
            }
            lcm_sym3__ = (b + c);
            ;
          } else {
            {
              lcm_sym3__ = (b + c);
              x = lcm_sym3__;
              ;
            }
            ;
          }
          y = lcm_sym3__;
        }
      }


      generate_quantities {
        data int lcm_sym2__;
        data int lcm_sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "lazy code motion, 5" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int b;
        int c;
        int x;
        int y;
        b = 1;
        if (1) {
          ;
          ;
          ;
        } else {
          if (2) x = b + c;
          ;
        }
        y = b + c;
      }
      |}
  in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        data int lcm_sym3__;
        {
          int b;
          int c;
          int x;
          int y;
          b = 1;
          if(1) {
            {
              ;
              ;
              ;
            }
            lcm_sym3__ = (b + c);
            ;
          } else {
            {
              if(2) {
                lcm_sym3__ = (b + c);
                x = lcm_sym3__;
                ;
              } else lcm_sym3__ = (b + c);
                     ;
              ;
            }
            ;
          }
          y = lcm_sym3__;
        }
      }


      generate_quantities {
        data int lcm_sym2__;
        data int lcm_sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "lazy code motion, 6" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int x;
        int y;
        if (2)
          x = 1 + 2;
        y = 4 + 3;
      }
      |}
  in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        data int lcm_sym4__;
        data int lcm_sym3__;
        {
          int x;
          int y;
          if(2) x = (1 + 2);
          y = (4 + 3);
        }
      }


      generate_quantities {
        data int lcm_sym2__;
        data int lcm_sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "lazy code motion, 7" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int a;
        int b;
        int c;
        int x;
        int y;
        int z;
        if (1) {
          a = c;
          x = a + b;
        } else ;
        if (2) {
          if (3) {
            ;
            while (4) y = a + b;
            ;
          } else {
              ;
              while (5) ;
              y = a + b;
            }
            z = a + b;
          } else ;
          ;
        }
      |}
  in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        data int lcm_sym3__;
        {
          int a;
          int b;
          int c;
          int x;
          int y;
          int z;
          if(1) {
            a = c;
            x = (a + b);
          } else ;
          if(2) {
            if(3) {
              lcm_sym3__ = (a + b);
              ;
              while(4) y = lcm_sym3__;
              ;
            } else {
              ;
              while(5) ;
              lcm_sym3__ = (a + b);
              y = lcm_sym3__;
            }
            z = lcm_sym3__;
          } else ;
          ;
        }
      }


      generate_quantities {
        data int lcm_sym2__;
        data int lcm_sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "lazy code motion, 8, _lp functions not optimized" =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        int foo_lp(int x) { target += 1; return 24; }
        int foo(int x) { return 24; }
      }
      model {
        print(foo(foo_lp(1)));
        print(foo(foo_lp(1)));
        print(foo(foo(1)));
        print(foo(foo(1)));
      }
      |}
  in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      functions {
        int foo_lp(int x) {
          {
            target += 1;
            return 24;
          }
        }
        int foo(int x) {
          {
            return 24;
          }
        }
      }



      log_prob {
        data int lcm_sym3__;
        {
          FnPrint__(foo(foo_lp(1)));
          FnPrint__(foo(foo_lp(1)));
          lcm_sym3__ = foo(foo(1));
          FnPrint__(lcm_sym3__);
          FnPrint__(lcm_sym3__);
        }
      }


      generate_quantities {
        data int lcm_sym2__;
        data int lcm_sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "lazy code motion, 9" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int x;
        while (x * 2) print("hello") ;
      }
      |}
  in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        data int lcm_sym3__;
        {
          int x;
          while((x * 2)) FnPrint__("hello");
        }
      }


      generate_quantities {
        data int lcm_sym2__;
        data int lcm_sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "lazy code motion, 10" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int x;
        x = 3;
        print(x * 2);
        x = 2;
        print(x * 2);
      }
      |}
  in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        data int lcm_sym3__;
        {
          int x;
          x = 3;
          FnPrint__((x * 2));
          x = 2;
          FnPrint__((x * 2));
        }
      }


      generate_quantities {
        data int lcm_sym2__;
        data int lcm_sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "lazy code motion, 11" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        {
          int x;
          print(x * 2);
        }
        {
          int x;
          print(x * 2);
        }
      }
      |}
  in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        data int lcm_sym3__;
        {
          {
            int x;
            FnPrint__((x * 2));
          }
          {
            int x;
            FnPrint__((x * 2));
          }
        }
      }


      generate_quantities {
        data int lcm_sym2__;
        data int lcm_sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "lazy code motion, 12" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        int x;
        for (i in 1:6) {
          print(x + 42);
          x = 3;
        }
      }
      |}
  in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        data int lcm_sym3__;
        {
          int x;
          for(i in 1:6) {
            FnPrint__((x + 42));
            x = 3;
          }
        }
      }


      generate_quantities {
        data int lcm_sym2__;
        data int lcm_sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "lazy code motion, 13" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        real temp;
        if (2 > 3)
          temp = 2 * 2;
        else
          print("hello");
        temp =  2 * 2;
        real temp2;
        for (i in 2 : 3) {
            temp2 = 2 * 3;
            target += temp;
            target += temp2;
        }
      }
      |}
  in
  let mir = one_step_loop_unrolling mir in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        real lcm_sym7__;
        real lcm_sym6__;
        data int lcm_sym5__;
        data int lcm_sym4__;
        data int lcm_sym3__;
        {
          real temp;
          if((2 > 3)) {
            lcm_sym6__ = promote((2 * 2), real, var);
            temp = lcm_sym6__;
            ;
          } else {
            FnPrint__("hello");
            lcm_sym6__ = promote((2 * 2), real, var);
            ;
          }
          temp = lcm_sym6__;
          real temp2;
          if((3 >= 2)) {
            lcm_sym7__ = promote((2 * 3), real, var);
            temp2 = lcm_sym7__;
            target += temp;
            lcm_sym5__ = (2 + 1);
            target += temp2;
            for(i in lcm_sym5__:3) {
              temp2 = lcm_sym7__;
              target += temp;
              target += temp2;
            }
          }
        }
      }


      generate_quantities {
        data int lcm_sym2__;
        data int lcm_sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test
    "cool example: expression propagation + partial evaluation + lazy code \
     motion + dead code elimination" =
  let mir =
    reset_and_mir_of_string
      {|
      model {
        real x;
        int y;
        real theta;
        for (i in 1:100000) {
          theta = inv_logit(x);
          target += bernoulli_lpmf(y| theta);
        }
      }
      |}
  in
  let mir = expression_propagation mir in
  let mir = partial_evaluation mir in
  let mir = one_step_loop_unrolling mir in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  let mir = dead_code_elimination mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        real lcm_sym6__;
        real lcm_sym5__;
        data int lcm_sym4__;
        data int lcm_sym3__;
        {
          real x;
          int y;
          real theta;
          if((100000 >= 1)) {
            lcm_sym4__ = (1 + 1);
            lcm_sym5__ = bernoulli_logit_lpmf(y, x);
            target += lcm_sym5__;
            for(i in lcm_sym4__:100000) {
              target += lcm_sym5__;
            }
          }
        }
      }


      generate_quantities {
        data int lcm_sym2__;
        data int lcm_sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "block fixing" =
  let mir = reset_and_mir_of_string {|
      model {
      }
      |} in
  let mir =
    { mir with
      Middle.Program.log_prob=
        [ Stmt.
            { pattern=
                IfElse
                  ( Expr.Helpers.zero
                  , { pattern=
                        While
                          ( Expr.Helpers.zero
                          , {pattern= SList []; meta= Location_span.empty} )
                    ; meta= Location_span.empty }
                  , None )
            ; meta= Location_span.empty } ] } in
  let mir = block_fixing mir in
  print_s [%sexp (mir : Program.Typed.t)];
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((pattern
           (IfElse
            ((pattern (Lit Int 0))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
            ((pattern
              (While
               ((pattern (Lit Int 0))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
               ((pattern (Block ())) (meta <opaque>))))
             (meta <opaque>))
            ()))
          (meta <opaque>))))
       (reverse_mode_log_prob ())
       (generate_quantities
        (((pattern
           (IfElse
            ((pattern
              (FunApp (StanLib PNot__ FnPlain AoS)
               (((pattern
                  (EOr
                   ((pattern (Var emit_transformed_parameters__))
                    (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                   ((pattern (Var emit_generated_quantities__))
                    (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
            ((pattern (Return ())) (meta <opaque>)) ()))
          (meta <opaque>))
         ((pattern
           (IfElse
            ((pattern
              (FunApp (StanLib PNot__ FnPlain AoS)
               (((pattern (Var emit_generated_quantities__))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
            ((pattern (Return ())) (meta <opaque>)) ()))
          (meta <opaque>))))
       (transform_inits ()) (unconstrain_array ()) (output_vars ()) (prog_name "")
       (prog_path "")) |}]

let%expect_test "one-step loop unrolling" =
  let mir =
    reset_and_mir_of_string
      {|
      transformed data {
        int x;
        for (i in x:6) print("hello");
        while (1<2) print("goodbye");
        for (i in 1:1) for (j in 2:2) print("nested");
      }
      |}
  in
  let mir = one_step_loop_unrolling mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      prepare_data {
        data int x;
        if((6 >= x)) {
          FnPrint__("hello");
          for(i in (x + 1):6) {
            FnPrint__("hello");
          }
        }
        if((1 < 2)) {
          FnPrint__("goodbye");
          while((1 < 2)) FnPrint__("goodbye");
        }
        if((1 >= 1)) {
          if((2 >= 2)) {
            FnPrint__("nested");
            for(j in (2 + 1):2) {
              FnPrint__("nested");
            }
          }
          for(i in (1 + 1):1) {
            if((2 >= 2)) {
              FnPrint__("nested");
              for(j in (2 + 1):2) {
                FnPrint__("nested");
              }
            }
          }
        }
      }



      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "adlevel_optimization" =
  let mir =
    reset_and_mir_of_string
      {|
      parameters {
        real w;
      }
      transformed parameters {
        {
          int x;
          real y;
          real z;
          real z_data;
          if (1 > 2)
            y = y + x;
          else
            y = y + w;
          if (2 > 1)
            z = y;
          if (3 > 1)
            z_data = x;
          print(z);
          print(z_data);
        }
      }
      |}
  in
  let mir = optimize_ad_levels mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        real w;
        {
          data int x;
          real y;
          real z;
          data real z_data;
          if((1 > 2)) y = (y + promote(x, real, data)); else y = (y + w);
          if((2 > 1)) z = y;
          if((3 > 1)) z_data = promote(x, real, var);
          FnPrint__(z);
          FnPrint__(z_data);
        }
      }


      generate_quantities {
        data real w;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        {
          data int x;
          data real y;
          data real z;
          data real z_data;
          if((1 > 2)) y = (y + promote(x, real, data)); else y = (y + w);
          if((2 > 1)) z = y;
          if((3 > 1)) z_data = promote(x, real, var);
          FnPrint__(z);
          FnPrint__(z_data);
        }
        if(PNot__(emit_generated_quantities__)) return;
      }


      output_vars {
        parameters real w; //real
      } |}]

let%expect_test "adlevel_optimization expressions" =
  let mir =
    reset_and_mir_of_string
      {|
      parameters {
        real w;
      }
      transformed parameters {
        {
          int x;
          real y;
          real z;
          real z_data;
          if (1 > 2)
            y = y + x;
          else
            y = y + w;
          if (2 > 1)
            z = y;
          if (3 > 1)
            z_data = x;
          print(z);
          print(z_data);
        }
      }
      |}
  in
  let mir = optimize_ad_levels mir in
  print_s [%sexp (mir.log_prob : Stmt.Located.t list)];
  [%expect
    {|
      (((pattern
         (Decl (decl_adtype AutoDiffable) (decl_id w) (decl_type (Sized SReal))
          (initialize Default)))
        (meta <opaque>))
       ((pattern
         (Block
          (((pattern
             (Decl (decl_adtype DataOnly) (decl_id x) (decl_type (Sized SInt))
              (initialize Default)))
            (meta <opaque>))
           ((pattern
             (Decl (decl_adtype AutoDiffable) (decl_id y) (decl_type (Sized SReal))
              (initialize Default)))
            (meta <opaque>))
           ((pattern
             (Decl (decl_adtype AutoDiffable) (decl_id z) (decl_type (Sized SReal))
              (initialize Default)))
            (meta <opaque>))
           ((pattern
             (Decl (decl_adtype DataOnly) (decl_id z_data)
              (decl_type (Sized SReal)) (initialize Default)))
            (meta <opaque>))
           ((pattern
             (IfElse
              ((pattern
                (FunApp (StanLib Greater__ FnPlain AoS)
                 (((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  ((pattern (Lit Int 2))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
              ((pattern
                (Assignment ((LVariable y) ()) UReal
                 ((pattern
                   (FunApp (StanLib Plus__ FnPlain AoS)
                    (((pattern (Var y))
                      (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
                     ((pattern
                       (Promotion
                        ((pattern (Var x))
                         (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                        UReal DataOnly))
                      (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
                  (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))))
               (meta <opaque>))
              (((pattern
                 (Assignment ((LVariable y) ()) UReal
                  ((pattern
                    (FunApp (StanLib Plus__ FnPlain AoS)
                     (((pattern (Var y))
                       (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
                      ((pattern (Var w))
                       (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
                   (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))))
                (meta <opaque>)))))
            (meta <opaque>))
           ((pattern
             (IfElse
              ((pattern
                (FunApp (StanLib Greater__ FnPlain AoS)
                 (((pattern (Lit Int 2))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  ((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
              ((pattern
                (Assignment ((LVariable z) ()) UReal
                 ((pattern (Var y))
                  (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))))
               (meta <opaque>))
              ()))
            (meta <opaque>))
           ((pattern
             (IfElse
              ((pattern
                (FunApp (StanLib Greater__ FnPlain AoS)
                 (((pattern (Lit Int 3))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  ((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
              ((pattern
                (Assignment ((LVariable z_data) ()) UReal
                 ((pattern
                   (Promotion
                    ((pattern (Var x))
                     (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                    UReal AutoDiffable))
                  (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))))
               (meta <opaque>))
              ()))
            (meta <opaque>))
           ((pattern
             (NRFunApp (CompilerInternal FnPrint)
              (((pattern (Var z))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
            (meta <opaque>))
           ((pattern
             (NRFunApp (CompilerInternal FnPrint)
              (((pattern (Var z_data))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
            (meta <opaque>)))))
        (meta <opaque>))) |}]

let%expect_test "adlevel_optimization 2" =
  let mir =
    reset_and_mir_of_string
      {|
      parameters {
        real w;
      }
      transformed parameters {
        real w_trans = 1;
        {
          int x;
          array[2] real y;
          real z;
          real z_data;
          if (1 > 2)
            y[1] = y[1] + x;
          else
            y[2] = y[2] + w;
          if (2 > 1)
            z = y[1];
          if (3 > 1)
            z_data = x;
          print(z);
          print(z_data);
        }
      }
      |}
  in
  let mir = optimize_ad_levels mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline;
  [%expect
    {|
      log_prob {
        real w;
        data real w_trans;
        w_trans = promote(1, real, var);
        {
          data int x;
          array[real, 2] y;
          real z;
          data real z_data;
          if((1 > 2)) y[1] = (y[1] + promote(x, real, data)); else y[2] = (y[2] + w);
          if((2 > 1)) z = y[1];
          if((3 > 1)) z_data = promote(x, real, var);
          FnPrint__(z);
          FnPrint__(z_data);
        }
      }


      generate_quantities {
        data real w;
        data real w_trans;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        w_trans = promote(1, real, var);
        {
          data int x;
          data array[real, 2] y;
          data real z;
          data real z_data;
          if((1 > 2)) y[1] = (y[1] + promote(x, real, data)); else y[2] = (y[2] + w);
          if((2 > 1)) z = y[1];
          if((3 > 1)) z_data = promote(x, real, var);
          FnPrint__(z);
          FnPrint__(z_data);
        }
        if(PNot__(emit_generated_quantities__)) return;
      }


      output_vars {
        parameters real w; //real
        transformed_parameters real w_trans; //real
      } |}]

let%expect_test "Mapping acts recursively" =
  let from = Expr.Helpers.variable "x" in
  let into = Expr.Helpers.variable "y" in
  let unpattern p = {Stmt.pattern= p; meta= Location_span.empty} in
  let s =
    Stmt.Pattern.NRFunApp
      ( CompilerInternal (FnWriteParam {var= from; unconstrain_opt= None})
      , [from] ) in
  let m = ExprMap.of_list [(from, into)] in
  let s' = expr_subst_stmt_base m s in
  Fmt.str "@[<v>%a@]" Stmt.Located.pp (unpattern s') |> print_endline;
  [%expect {| (FnWriteParam(unconstrain_opt())(var y))__(y); |}]

(* ---- loop vectorization ---- *)

let print_vectorized s =
  let mir = vectorize_loops (reset_and_mir_of_string s) in
  List.iter mir.functions_block ~f:(fun fd ->
      Fmt.str "@[<v>%a@]" (Program.pp_fun_def Stmt.Located.pp) fd
      |> print_endline);
  Fmt.str "@[<v>%a@]" (Fmt.list ~sep:Fmt.cut Stmt.Located.pp) mir.log_prob
  |> print_endline

let%expect_test "vectorize: full-range density loop becomes one density" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        vector[N] x;
        vector[N] y;
      }
      parameters {
        real alpha;
        real beta;
        real<lower=0> sigma;
      }
      model {
        vector[N] mu = alpha + beta * x;
        for (n in 1 : N) {
          target += normal_lpdf(y[n] | mu[n], sigma);
        }
      }
      |};
  [%expect
    {|
    real alpha;
    real beta;
    real sigma;
    {
      FnValidateSize__("mu", "N", N);
      vector[N] mu;
      mu = (alpha + (beta * x));
      target += normal_lpdf(y, mu, sigma);
    }
    |}]

let%expect_test "vectorize: tilde keeps its propto suffix" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        vector[N] y;
      }
      parameters {
        vector[N] mu;
        real<lower=0> sigma;
      }
      model {
        for (n in 1 : N) {
          y[n] ~ normal(mu[n], sigma);
        }
      }
      |};
  [%expect
    {|
    vector[N] mu;
    real sigma;
    {
      target += normal_lupdf(y, mu, sigma);
    }
    |}]

let%expect_test "vectorize: partial range slices instead of bailing" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        vector[N] y;
      }
      parameters {
        real mu;
        real<lower=0> sigma;
      }
      model {
        for (n in 3 : N) {
          target += normal_lpdf(y[n] | mu, sigma);
        }
      }
      |};
  [%expect
    {|
    real mu;
    real sigma;
    {
      target += normal_lpdf(y[3:N], mu, sigma);
    }
    |}]

let%expect_test "vectorize: lpmf over an int outcome array" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        array[N] int<lower=0, upper=1> y;
      }
      parameters {
        vector[N] eta;
      }
      model {
        for (n in 1 : N) {
          y[n] ~ bernoulli_logit(eta[n]);
        }
      }
      |};
  [%expect
    {|
    vector[N] eta;
    {
      target += bernoulli_logit_lupmf(y, eta);
    }
    |}]

let%expect_test "vectorize bail: no argument varies with the loop variable" =
  (* Collapsing this loop would add one lp term to the target instead of N. *)
  print_vectorized
    {|
      data {
        int<lower=0> N;
      }
      parameters {
        real mu;
        real<lower=0> sigma;
      }
      model {
        for (n in 1 : N) {
          target += normal_lpdf(0.5 | mu, sigma);
        }
      }
      |};
  [%expect
    {|
    real mu;
    real sigma;
    {
      for(n in 1:N) {
        target += normal_lpdf(0.5, mu, sigma);
      }
    }
    |}]

let%expect_test "vectorize bail: loop variable used as a value" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        vector[N] y;
      }
      parameters {
        real<lower=0> sigma;
      }
      model {
        for (n in 1 : N) {
          target += normal_lpdf(y[n] | n, sigma);
        }
      }
      |};
  [%expect
    {|
    real sigma;
    {
      for(n in 1:N) {
        target += normal_lpdf(y[n], promote(n, real, data), sigma);
      }
    }
    |}]

let%expect_test "vectorize bail: truncation lowers to a multi-statement body" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        vector[N] y;
      }
      parameters {
        real mu;
        real<lower=0> sigma;
      }
      model {
        for (n in 1 : N) {
          y[n] ~ normal(mu, sigma) T[0, ];
        }
      }
      |};
  [%expect
    {|
    real mu;
    real sigma;
    {
      for(n in 1:N) {
        target += normal_lupdf(y[n], mu, sigma);
        if((y[n] < 0)) target += FnNegInf__(); else target += PMinus__(normal_lccdf(
                                                                       promote(
                                                                       0, real,
                                                                       data), mu,
                                                                       sigma));
      }
    }
    |}]

let%expect_test "vectorize bail: user-defined densities are not vectorized" =
  print_vectorized
    {|
      functions {
        real foo_lpdf(real y, real mu) {
          return -square(y - mu);
        }
      }
      data {
        int<lower=0> N;
        vector[N] y;
      }
      parameters {
        real mu;
      }
      model {
        for (n in 1 : N) {
          y[n] ~ foo(mu);
        }
      }
      |};
  [%expect
    {|
    real foo_lpdf(real y, real mu) {
      {
        return PMinus__(square((y - mu)));
      }
    }
    real mu;
    {
      for(n in 1:N) {
        target += foo_lupdf(y[n], mu);
      }
    }
    |}]

let%expect_test "vectorize: indirect indexing becomes a multi-index gather" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        int<lower=1> J;
        array[N] int<lower=1, upper=J> county;
        vector[N] y;
      }
      parameters {
        vector[J] alpha;
        real<lower=0> sigma;
      }
      model {
        for (n in 1 : N) {
          target += normal_lpdf(y[n] | alpha[county[n]], sigma);
        }
      }
      |};
  [%expect
    {|
    vector[J] alpha;
    real sigma;
    {
      target += normal_lpdf(y, alpha[county], sigma);
    }
    |}]

let%expect_test "vectorize: partial-range gather slices the index array" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        int<lower=1> J;
        array[N] int<lower=1, upper=J> county;
        vector[N] y;
      }
      parameters {
        vector[J] alpha;
        real<lower=0> sigma;
      }
      model {
        for (n in 2 : N) {
          target += normal_lpdf(y[n] | alpha[county[n]], sigma);
        }
      }
      |};
  [%expect
    {|
    vector[J] alpha;
    real sigma;
    {
      target += normal_lpdf(y[2:N], alpha[county[2:N]], sigma);
    }
    |}]

let%expect_test "vectorize: assignment loop with a gather (radon_county)" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        int<lower=1> J;
        array[N] int<lower=1, upper=J> county;
        vector[N] y;
      }
      parameters {
        vector[J] a;
        real<lower=0> sigma;
      }
      model {
        vector[N] y_hat;
        for (i in 1 : N) {
          y_hat[i] = a[county[i]];
        }
        y ~ normal(y_hat, sigma);
      }
      |};
  [%expect
    {|
    vector[J] a;
    real sigma;
    {
      FnValidateSize__("y_hat", "N", N);
      vector[N] y_hat;
      y_hat[:] = a[county];
      target += normal_lupdf(y, y_hat, sigma);
    }
    |}]

let%expect_test "vectorize: assignment loop widens arithmetic" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        vector[N] x;
        vector[N] w;
      }
      parameters {
        real alpha;
        real beta;
      }
      model {
        vector[N] mu;
        vector[N] v;
        for (n in 1 : N) {
          mu[n] = alpha + beta * x[n];
        }
        for (n in 1 : N) {
          v[n] = x[n] * w[n] / mu[n];
        }
        target += normal_lpdf(v | mu, 1);
      }
      |};
  [%expect
    {|
    real alpha;
    real beta;
    {
      FnValidateSize__("mu", "N", N);
      vector[N] mu;
      FnValidateSize__("v", "N", N);
      vector[N] v;
      mu[:] = (alpha + (beta * x));
      v[:] = ((x .* w) ./ mu);
      target += normal_lpdf(v, mu, promote(1, real, data));
    }
    |}]

let%expect_test "vectorize: assignment loop widens vectorized functions" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        vector[N] x;
      }
      parameters {
        vector[N] u;
      }
      model {
        vector[N] v;
        for (n in 1 : N) {
          v[n] = exp(x[n]) + sqrt(square(u[n]));
        }
        target += normal_lpdf(v | 0, 1);
      }
      |};
  [%expect
    {|
    vector[N] u;
    {
      FnValidateSize__("v", "N", N);
      vector[N] v;
      v[:] = (exp(x) + sqrt(square(u)));
      target += normal_lpdf(v, promote(0, real, data), promote(1, real, data));
    }
    |}]

let%expect_test "vectorize bail: assignment reading the written vector" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
      }
      parameters {
        vector[N] u;
      }
      model {
        vector[N] v;
        v[1] = 0.5;
        for (n in 2 : N) {
          v[n] = v[n - 1] + u[n];
        }
        target += normal_lpdf(v | 0, 1);
      }
      |};
  [%expect
    {|
    vector[N] u;
    {
      FnValidateSize__("v", "N", N);
      vector[N] v;
      v[1] = 0.5;
      for(n in 2:N) {
        v[n] = (v[(n - 1)] + u[n]);
      }
      target += normal_lpdf(v, promote(0, real, data), promote(1, real, data));
    }
    |}]

let%expect_test "vectorize bail: density value assigned, not summed" =
  (* normal_lpdf used as a value. Widening it would compute one summed lp where
     the loop stored per-element lps. *)
  print_vectorized
    {|
      data {
        int<lower=0> N;
        vector[N] y;
      }
      parameters {
        real mu;
      }
      model {
        vector[N] v;
        for (n in 1 : N) {
          v[n] = normal_lpdf(y[n] | mu, 1);
        }
        target += sum(v);
      }
      |};
  [%expect
    {|
    real mu;
    {
      FnValidateSize__("v", "N", N);
      vector[N] v;
      for(n in 1:N) {
        v[n] = normal_lpdf(y[n], mu, promote(1, real, data));
      }
      target += sum(v);
    }
    |}]

let%expect_test "vectorize bail: offset index and invariant rhs" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        vector[N] x;
      }
      parameters {
        real alpha;
      }
      model {
        vector[N] v;
        vector[N] w;
        for (n in 1 : N) {
          v[n] = alpha;
        }
        for (n in 2 : N) {
          w[n] = x[n - 1];
        }
        target += normal_lpdf(v | w, 1);
      }
      |};
  [%expect
    {|
    real alpha;
    {
      FnValidateSize__("v", "N", N);
      vector[N] v;
      FnValidateSize__("w", "N", N);
      vector[N] w;
      for(n in 1:N) {
        v[n] = alpha;
      }
      for(n in 2:N) {
        w[n] = x[(n - 1)];
      }
      target += normal_lpdf(v, w, promote(1, real, data));
    }
    |}]

let%expect_test "vectorize bail: mixed lane containers under an operator" =
  (* x widens to an array and u to a vector. Neither Times nor EltTimes takes
     that pair, so the loop stays. *)
  print_vectorized
    {|
      data {
        int<lower=0> N;
        array[N] real x;
      }
      parameters {
        vector[N] u;
      }
      model {
        vector[N] v;
        for (n in 1 : N) {
          v[n] = x[n] * u[n];
        }
        target += normal_lpdf(v | 0, 1);
      }
      |};
  [%expect
    {|
    vector[N] u;
    {
      FnValidateSize__("v", "N", N);
      vector[N] v;
      for(n in 1:N) {
        v[n] = (x[n] * u[n]);
      }
      target += normal_lpdf(v, promote(0, real, data), promote(1, real, data));
    }
    |}]

let%expect_test "vectorize bail: doubly indirect indexing" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        int<lower=1> J;
        array[N] int<lower=1, upper=N> site;
        array[N] int<lower=1, upper=J> county;
        vector[N] y;
      }
      parameters {
        vector[J] alpha;
        real<lower=0> sigma;
      }
      model {
        for (n in 1 : N) {
          target += normal_lpdf(y[n] | alpha[county[site[n]]], sigma);
        }
      }
      |};
  [%expect
    {|
    vector[J] alpha;
    real sigma;
    {
      for(n in 1:N) {
        target += normal_lpdf(y[n], alpha[county[site[n]]], sigma);
      }
    }
    |}]

let%expect_test "vectorize bail: invariant container argument" =
  (* Each iteration sums over all of mu. The vectorized call would zip mu
     against y elementwise. Both typecheck, so the classifier must refuse
     container invariants. *)
  print_vectorized
    {|
      data {
        int<lower=0> N;
        vector[N] y;
      }
      parameters {
        vector[N] mu;
        real<lower=0> sigma;
      }
      model {
        for (n in 1 : N) {
          y[n] ~ normal(mu, sigma);
        }
      }
      |};
  [%expect
    {|
    vector[N] mu;
    real sigma;
    {
      for(n in 1:N) {
        target += normal_lupdf(y[n], mu, sigma);
      }
    }
    |}]

let%expect_test "vectorize: array target takes an array right-hand side" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        array[N] real x;
      }
      parameters {
        real<lower=0> sigma;
      }
      model {
        array[N] real v;
        for (n in 1 : N) {
          v[n] = exp(x[n]);
        }
        target += normal_lpdf(v | 0, sigma);
      }
      |};
  [%expect
    {|
    real sigma;
    {
      FnValidateSize__("v", "N", N);
      array[real, N] v;
      v[:] = exp(x);
      target += normal_lpdf(v, promote(0, real, data), sigma);
    }
    |}]

let%expect_test "vectorize bail: array target with a vector right-hand side" =
  (* The loop assigned real into real, but the widened rhs is a vector and the
     target an array, so the sliced assignment would not typecheck.
     GLM_Binomial_model's generated quantities have this shape. *)
  print_vectorized
    {|
      data {
        int<lower=0> N;
      }
      parameters {
        vector[N] logit_p;
      }
      model {
        array[N] real p;
        for (n in 1 : N) {
          p[n] = inv_logit(logit_p[n]);
        }
        target += normal_lpdf(p | 0, 1);
      }
      |};
  [%expect
    {|
    vector[N] logit_p;
    {
      FnValidateSize__("p", "N", N);
      array[real, N] p;
      for(n in 1:N) {
        p[n] = inv_logit(logit_p[n]);
      }
      target += normal_lpdf(p, promote(0, real, data), promote(1, real, data));
    }
    |}]

let%expect_test "vectorize bail: side-effecting invariant argument" =
  (* The rewrite would evaluate the invariant argument once instead of N times,
     and a _lp call changes the target each time. *)
  print_vectorized
    {|
      functions {
        real bump_lp(real x) {
          target += x;
          return x;
        }
      }
      data {
        int<lower=0> N;
        vector[N] y;
      }
      parameters {
        real<lower=0> sigma;
      }
      model {
        for (n in 1 : N) {
          target += normal_lpdf(y[n] | bump_lp(sigma), sigma);
        }
      }
      |};
  [%expect
    {|
    real bump_lp(real x) {
      {
        target += x;
        return x;
      }
    }
    real sigma;
    {
      for(n in 1:N) {
        target += normal_lpdf(y[n], bump_lp(sigma), sigma);
      }
    }
    |}]

let%expect_test "vectorize: transformed data sizes are trusted" =
  (* Declared sizes never change at runtime, so a transformed data vector
     spanning the range appears bare. *)
  print_vectorized
    {|
      data {
        int<lower=0> N;
      }
      transformed data {
        vector[N] w;
        w = rep_vector(1.5, N);
      }
      parameters {
        real mu;
        real<lower=0> sigma;
      }
      model {
        for (n in 1 : N) {
          target += normal_lpdf(w[n] | mu, sigma);
        }
      }
      |};
  [%expect
    {|
    real mu;
    real sigma;
    {
      target += normal_lpdf(w, mu, sigma);
    }
    |}]

let%expect_test "vectorize bail: no vectorized signature (matrix rows)" =
  (* Each iteration is a valid scalar statement over a row_vector. The sliced
     argument is a matrix and normal_lpdf has no matrix signature, so the final
     re-typecheck rejects the rewrite. *)
  print_vectorized
    {|
      data {
        int<lower=0> N;
        matrix[N, 2] m;
      }
      parameters {
        real mu;
        real<lower=0> sigma;
      }
      model {
        for (n in 1 : N) {
          target += normal_lpdf(m[n] | mu, sigma);
        }
      }
      |};
  [%expect
    {|
    real mu;
    real sigma;
    {
      for(n in 1:N) {
        target += normal_lpdf(m[n], mu, sigma);
      }
    }
    |}]

let%expect_test "vectorize: loops inside function bodies" =
  (* Function bodies see only their own arguments, so the slice form is emitted,
     never a bare name. *)
  print_vectorized
    {|
      functions {
        real total_lpdf(vector y, real mu, real sigma) {
          real lp = 0;
          for (n in 1 : num_elements(y)) {
            lp += normal_lpdf(y[n] | mu, sigma);
          }
          return lp;
        }
      }
      data {
        int<lower=0> N;
        vector[N] y;
      }
      parameters {
        real mu;
        real<lower=0> sigma;
      }
      model {
        y ~ total(mu, sigma);
      }
      |};
  [%expect
    {|
    real total_lpdf(vector y, real mu, real sigma) {
      {
        real lp;
        lp = promote(0, real, var);
        for(n in 1:num_elements(y)) {
          lp = (lp + normal_lpdf(y[n], mu, sigma));
        }
        return lp;
      }
    }
    real mu;
    real sigma;
    {
      target += total_lupdf(y, mu, sigma);
    }
    |}]

let%expect_test "vectorize: generated quantity names are not trusted sizes" =
  (* A generated quantity is not in scope in the model block, so a model local
     may reuse its name at another size. The local x has three elements and the
     loop covers two, so the slice form is required. *)
  print_vectorized
    {|
      model {
        vector[3] x = rep_vector(1.0, 3);
        for (i in 1 : 2) {
          x[i] ~ std_normal();
        }
      }
      generated quantities {
        vector[2] x;
      }
      |};
  [%expect
    {|
    {
      vector[3] x;
      x = rep_vector(1.0, 3);
      target += std_normal_lupdf(x[1:2]);
    }
    |}]

let%expect_test "vectorize: elementwise power" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        vector[N] x;
        vector[N] w;
      }
      parameters {
        real<lower=0> sigma;
      }
      model {
        vector[N] v;
        for (n in 1 : N) {
          v[n] = x[n] ^ w[n];
        }
        target += normal_lpdf(v | 0, sigma);
      }
      |};
  [%expect
    {|
    real sigma;
    {
      FnValidateSize__("v", "N", N);
      vector[N] v;
      v[:] = (x ^ w);
      target += normal_lpdf(v, promote(0, real, data), sigma);
    }
    |}]

let%expect_test "vectorize: row_vector lanes" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        row_vector[N] r;
      }
      parameters {
        real<lower=0> sigma;
      }
      model {
        row_vector[N] v;
        for (n in 1 : N) {
          v[n] = r[n] * 2;
        }
        target += normal_lpdf(v | 0, sigma);
      }
      |};
  [%expect
    {|
    real sigma;
    {
      FnValidateSize__("v", "N", N);
      row_vector[N] v;
      v[:] = (r * promote(2, real, data));
      target += normal_lpdf(v, promote(0, real, data), sigma);
    }
    |}]

let%expect_test "vectorize bail: side effect inside an assignment" =
  print_vectorized
    {|
      functions {
        real bump_lp(real x) {
          target += x;
          return x;
        }
      }
      data {
        int<lower=0> N;
        vector[N] x;
      }
      parameters {
        real<lower=0> sigma;
      }
      model {
        vector[N] v;
        for (n in 1 : N) {
          v[n] = x[n] + bump_lp(sigma);
        }
        target += normal_lpdf(v | 0, sigma);
      }
      |};
  [%expect
    {|
    real bump_lp(real x) {
      {
        target += x;
        return x;
      }
    }
    real sigma;
    {
      FnValidateSize__("v", "N", N);
      vector[N] v;
      for(n in 1:N) {
        v[n] = (x[n] + bump_lp(sigma));
      }
      target += normal_lpdf(v, promote(0, real, data), sigma);
    }
    |}]

let%expect_test "vectorize: reassigned size keeps the required rhs slice" =
  print_vectorized
    {|
      data {
        int<lower=1> N;
        vector[N] x;
      }
      model {
        int M = N;
        vector[M] y = x;
        M = N - 1;
        vector[M] z;
        for (i in 1 : M) {
          z[i] = y[i];
        }
        print(z);
      }
      |};
  [%expect
    {|
    {
      int M;
      M = N;
      FnValidateSize__("y", "M", M);
      vector[M] y;
      y = x;
      M = (N - 1);
      FnValidateSize__("z", "M", M);
      vector[M] z;
      z[:] = y[1:M];
      FnPrint__(z);
    }
    |}]

let%expect_test "vectorize bail: rng inside an assignment" =
  print_vectorized
    {|
      functions {
        real draw_sum_rng(vector x) {
          vector[num_elements(x)] draws;
          for (n in 1 : num_elements(x)) {
            draws[n] = x[n] + normal_rng(0, 1);
          }
          return sum(draws);
        }
      }
      model {}
      |};
  [%expect
    {|
    real draw_sum_rng(vector x) {
      {
        FnValidateSize__("draws", "num_elements(x)", num_elements(x));
        vector[num_elements(x)] draws;
        for(n in 1:num_elements(x)) {
          draws[n] = (x[n] + normal_rng(0, 1));
        }
        return sum(draws);
      }
    }
    |}]

let%expect_test "vectorize: independent statements in one loop" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        vector[N] x;
        vector[N] y;
      }
      parameters {
        real alpha;
      }
      model {
        vector[N] v;
        vector[N] w;
        for (n in 1 : N) {
          v[n] = x[n] + alpha;
          w[n] = y[n] * 2;
        }
        target += normal_lpdf(v | w, 1);
      }
      |};
  [%expect
    {|
    real alpha;
    {
      FnValidateSize__("v", "N", N);
      vector[N] v;
      FnValidateSize__("w", "N", N);
      vector[N] w;
      v[:] = (x + alpha);
      w[:] = (y * promote(2, real, data));
      target += normal_lpdf(v, w, promote(1, real, data));
    }
    |}]

let%expect_test "vectorize bail: statements in one loop interfere" =
  print_vectorized
    {|
      data {
        int<lower=0> N;
        vector[N] x;
      }
      parameters {
        real alpha;
      }
      model {
        vector[N] v;
        vector[N] w;
        for (n in 1 : N) {
          v[n] = x[n] + alpha;
          w[n] = v[n] * 2;
        }
        target += normal_lpdf(v | w, 1);
      }
      |};
  [%expect
    {|
    real alpha;
    {
      FnValidateSize__("v", "N", N);
      vector[N] v;
      FnValidateSize__("w", "N", N);
      vector[N] w;
      for(n in 1:N) {
        v[n] = (x[n] + alpha);
        w[n] = (v[n] * promote(2, real, data));
      }
      target += normal_lpdf(v, w, promote(1, real, data));
    }
    |}]
