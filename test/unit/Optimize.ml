open Core_kernel
open Frontend
open Analysis_and_optimization.Optimize
open Middle
open Common
open Analysis_and_optimization.Mir_utils

let reset_and_mir_of_string s =
  Gensym.reset_danger_use_cautiously () ;
  Frontend_utils.typed_ast_of_string_exn s |> Ast_to_Mir.trans_prog ""

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
    | Stmt.Fixed.Pattern.NRFunApp (CompilerInternal FnPrint, [s]) ->
        Stmt.Fixed.Pattern.NRFunApp (CompilerInternal FnPrint, [s; s])
    | x -> x in
  let mir = Program.map Fn.id (map_rec_stmt_loc f) mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
    | Stmt.Fixed.Pattern.NRFunApp (CompilerInternal FnPrint, [s]) ->
        Stmt.Fixed.Pattern.(NRFunApp (CompilerInternal FnPrint, [s; s]), i + 1)
    | x -> (x, i) in
  let mir_stmt, num =
    (map_rec_state_stmt_loc f 0)
      Stmt.Fixed.{pattern= SList mir.log_prob; meta= Location_span.empty} in
  let mir = {mir with log_prob= [mir_stmt]} in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  print_endline (string_of_int num) ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
            return (z ^ 2);
          }
        }
      }



      log_prob {
        {
          data int inline_sym1__;
          inline_sym1__ = 0;
          for(inline_sym2__ in 1:1) {
            FnPrint__(3);
            FnPrint__(FnMakeRowVec__(FnMakeRowVec__(promote(3, real),
                      promote(2, real)), FnMakeRowVec__(promote(4, real),
                      promote(6, real))));
          }
          real inline_sym3__;
          data int inline_sym4__;
          inline_sym4__ = 0;
          for(inline_sym5__ in 1:1) {
            inline_sym4__ = 1;
            inline_sym3__ = (53 ^ 2);
            break;
          }
          FnReject__(inline_sym3__);
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
        data int inline_sym7__;
        inline_sym7__ = 0;
        for(inline_sym8__ in 1:1) {
          data int inline_sym5__;
          inline_sym5__ = 0;
          for(inline_sym6__ in 1:1) {

          }
          if(inline_sym7__) break;
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
  print_s [%sexp (mir : Middle.Program.Typed.t)] ;
  [%expect
    {|
    ((functions_block
      (((fdrt ()) (fdname f) (fdsuffix FnPlain)
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
       ((fdrt (UReal)) (fdname g) (fdsuffix FnPlain)
        (fdargs ((AutoDiffable z UInt)))
        (fdbody
         (((pattern
            (Block
             (((pattern
                (Return
                 (((pattern
                    (FunApp (StanLib Pow__ FnPlain AoS)
                     (((pattern (Var z))
                       (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                      ((pattern (Lit Int 2))
                       (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
                   (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
               (meta <opaque>)))))
           (meta <opaque>))))
        (fdloc <opaque>))))
     (input_vars ()) (prepare_data ())
     (log_prob
      (((pattern
         (Block
          (((pattern
             (Decl (decl_adtype DataOnly) (decl_id inline_sym1__)
              (decl_type (Sized SInt)) (initialize true)))
            (meta <opaque>))
           ((pattern
             (Assignment (inline_sym1__ UInt ())
              ((pattern (Lit Int 0))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
            (meta <opaque>))
           ((pattern
             (For (loopvar inline_sym2__)
              (lower
               ((pattern (Lit Int 1))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
              (upper
               ((pattern (Lit Int 1))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
              (body
               ((pattern
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
                                    ((type_ UInt) (loc <opaque>)
                                     (adlevel DataOnly))))
                                  UReal DataOnly))
                                (meta
                                 ((type_ UReal) (loc <opaque>)
                                  (adlevel DataOnly))))
                               ((pattern
                                 (Promotion
                                  ((pattern (Lit Int 2))
                                   (meta
                                    ((type_ UInt) (loc <opaque>)
                                     (adlevel DataOnly))))
                                  UReal DataOnly))
                                (meta
                                 ((type_ UReal) (loc <opaque>)
                                  (adlevel DataOnly)))))))
                            (meta
                             ((type_ URowVector) (loc <opaque>)
                              (adlevel DataOnly))))
                           ((pattern
                             (FunApp (CompilerInternal FnMakeRowVec)
                              (((pattern
                                 (Promotion
                                  ((pattern (Lit Int 4))
                                   (meta
                                    ((type_ UInt) (loc <opaque>)
                                     (adlevel DataOnly))))
                                  UReal DataOnly))
                                (meta
                                 ((type_ UReal) (loc <opaque>)
                                  (adlevel DataOnly))))
                               ((pattern
                                 (Promotion
                                  ((pattern (Lit Int 6))
                                   (meta
                                    ((type_ UInt) (loc <opaque>)
                                     (adlevel DataOnly))))
                                  UReal DataOnly))
                                (meta
                                 ((type_ UReal) (loc <opaque>)
                                  (adlevel DataOnly)))))))
                            (meta
                             ((type_ URowVector) (loc <opaque>)
                              (adlevel DataOnly)))))))
                        (meta
                         ((type_ UMatrix) (loc <opaque>) (adlevel DataOnly)))))))
                    (meta <opaque>)))))
                (meta <opaque>)))))
            (meta <opaque>))
           ((pattern
             (Decl (decl_adtype AutoDiffable) (decl_id inline_sym3__)
              (decl_type (Unsized UReal)) (initialize true)))
            (meta <opaque>))
           ((pattern
             (Decl (decl_adtype DataOnly) (decl_id inline_sym4__)
              (decl_type (Sized SInt)) (initialize true)))
            (meta <opaque>))
           ((pattern
             (Assignment (inline_sym4__ UInt ())
              ((pattern (Lit Int 0))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
            (meta <opaque>))
           ((pattern
             (For (loopvar inline_sym5__)
              (lower
               ((pattern (Lit Int 1))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
              (upper
               ((pattern (Lit Int 1))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
              (body
               ((pattern
                 (Block
                  (((pattern
                     (Assignment (inline_sym4__ UInt ())
                      ((pattern (Lit Int 1))
                       (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
                    (meta <opaque>))
                   ((pattern
                     (Assignment (inline_sym3__ UReal ())
                      ((pattern
                        (FunApp (StanLib Pow__ FnPlain AoS)
                         (((pattern (Lit Int 53))
                           (meta
                            ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                          ((pattern (Lit Int 2))
                           (meta
                            ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
                       (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))))
                    (meta <opaque>))
                   ((pattern Break) (meta <opaque>)))))
                (meta <opaque>)))))
            (meta <opaque>))
           ((pattern
             (NRFunApp (CompilerInternal FnReject)
              (((pattern (Var inline_sym3__))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
            (meta <opaque>)))))
        (meta <opaque>))))
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
     (transform_inits ()) (output_vars ()) (prog_name "") (prog_path ""))
    |}]

let%expect_test "do not inline recursive functions" =
  let mir =
    reset_and_mir_of_string
      {|
      functions {
        real g(int z);
        real g(int z) {
          return z^2;
        }
      }
      model {
        reject(g(53));
      }
      |}
  in
  let mir = function_inlining mir in
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      functions {
        real g(int z) {
          ;
        }
        real g(int z) {
          {
            return (z ^ 2);
          }
        }
      }



      log_prob {
        {
          FnReject__(g(53));
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int inline_sym1__;
          int inline_sym4__;
          data int inline_sym2__;
          inline_sym2__ = 0;
          for(inline_sym3__ in 1:1) {
            FnPrint__("f");
            inline_sym2__ = 1;
            inline_sym1__ = 42;
            break;
          }
          data int inline_sym5__;
          inline_sym5__ = 0;
          for(inline_sym6__ in 1:1) {
            FnPrint__("g");
            inline_sym5__ = 1;
            inline_sym4__ = (3 + 24);
            break;
          }
          for(i in inline_sym1__:inline_sym4__) {
            {
              FnPrint__("body");
            }
            data int inline_sym5__;
            inline_sym5__ = 0;
            for(inline_sym6__ in 1:1) {
              FnPrint__("g");
              inline_sym5__ = 1;
              inline_sym4__ = (3 + 24);
              break;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int inline_sym7__;
          int inline_sym10__;
          data int inline_sym8__;
          inline_sym8__ = 0;
          for(inline_sym9__ in 1:1) {
            FnPrint__("f");
            inline_sym8__ = 1;
            inline_sym7__ = 42;
            break;
          }
          data int inline_sym14__;
          inline_sym14__ = 0;
          for(inline_sym15__ in 1:1) {
            FnPrint__("g");
            int inline_sym11__;
            data int inline_sym12__;
            inline_sym12__ = 0;
            for(inline_sym13__ in 1:1) {
              FnPrint__("f");
              inline_sym12__ = 1;
              inline_sym11__ = 42;
              break;
            }
            if(inline_sym14__) break;
            inline_sym14__ = 1;
            inline_sym10__ = (inline_sym11__ + 24);
            break;
          }
          for(i in inline_sym7__:inline_sym10__) {
            {
              FnPrint__("body");
            }
            data int inline_sym14__;
            inline_sym14__ = 0;
            for(inline_sym15__ in 1:1) {
              FnPrint__("g");
              int inline_sym11__;
              data int inline_sym12__;
              inline_sym12__ = 0;
              for(inline_sym13__ in 1:1) {
                FnPrint__("f");
                inline_sym12__ = 1;
                inline_sym11__ = 42;
                break;
              }
              if(inline_sym14__) break;
              inline_sym14__ = 1;
              inline_sym10__ = (inline_sym11__ + 24);
              break;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int inline_sym1__;
          data int inline_sym2__;
          inline_sym2__ = 0;
          for(inline_sym3__ in 1:1) {
            FnPrint__("g");
            inline_sym2__ = 1;
            inline_sym1__ = (3 + 24);
            break;
          }
          while(inline_sym1__) {
            FnPrint__("body");
            data int inline_sym2__;
            inline_sym2__ = 0;
            for(inline_sym3__ in 1:1) {
              FnPrint__("g");
              inline_sym2__ = 1;
              inline_sym1__ = (3 + 24);
              break;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int inline_sym1__;
          data int inline_sym2__;
          inline_sym2__ = 0;
          for(inline_sym3__ in 1:1) {
            FnPrint__("g");
            inline_sym2__ = 1;
            inline_sym1__ = (3 + 24);
            break;
          }
          if(inline_sym1__) FnPrint__("body");
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int inline_sym1__;
          int inline_sym4__;
          int inline_sym7__;
          data int inline_sym2__;
          inline_sym2__ = 0;
          for(inline_sym3__ in 1:1) {
            FnPrint__("f");
            inline_sym2__ = 1;
            inline_sym1__ = 42;
            break;
          }
          if(inline_sym1__) {
            data int inline_sym5__;
            inline_sym5__ = 0;
            for(inline_sym6__ in 1:1) {
              FnPrint__("g");
              inline_sym5__ = 1;
              inline_sym4__ = (3 + 24);
              break;
            }
          } else {
            data int inline_sym8__;
            inline_sym8__ = 0;
            for(inline_sym9__ in 1:1) {
              FnPrint__("h");
              inline_sym8__ = 1;
              inline_sym7__ = (4 + 4);
              break;
            }
          }
          FnPrint__(inline_sym1__ ?inline_sym4__: inline_sym7__);
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int inline_sym1__;
          data int inline_sym2__;
          inline_sym2__ = 0;
          for(inline_sym3__ in 1:1) {
            if(2) {
              FnPrint__("f");
              inline_sym2__ = 1;
              inline_sym1__ = 42;
              break;
            }
            inline_sym2__ = 1;
            inline_sym1__ = 6;
            break;
          }
          FnPrint__(inline_sym1__);
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int inline_sym4__;
          int inline_sym1__;
          data int inline_sym5__;
          inline_sym5__ = 0;
          for(inline_sym6__ in 1:1) {
            FnPrint__(2);
            inline_sym5__ = 1;
            inline_sym4__ = 42;
            break;
          }
          data int inline_sym2__;
          inline_sym2__ = 0;
          for(inline_sym3__ in 1:1) {
            FnPrint__(1);
            inline_sym2__ = 1;
            inline_sym1__ = 42;
            break;
          }
          FnPrint__(a[inline_sym1__, inline_sym4__]);
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int inline_sym1__;
          int inline_sym4__;
          data int inline_sym2__;
          inline_sym2__ = 0;
          for(inline_sym3__ in 1:1) {
            FnPrint__(1);
            inline_sym2__ = 1;
            inline_sym1__ = 42;
            break;
          }
          if(inline_sym1__) {
            data int inline_sym5__;
            inline_sym5__ = 0;
            for(inline_sym6__ in 1:1) {
              FnPrint__(2);
              inline_sym5__ = 1;
              inline_sym4__ = 42;
              break;
            }
          }
          FnPrint__(inline_sym1__ && inline_sym4__);
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int inline_sym1__;
          int inline_sym4__;
          data int inline_sym2__;
          inline_sym2__ = 0;
          for(inline_sym3__ in 1:1) {
            FnPrint__(1);
            inline_sym2__ = 1;
            inline_sym1__ = 42;
            break;
          }
          if(inline_sym1__) ; else {
            data int inline_sym5__;
            inline_sym5__ = 0;
            for(inline_sym6__ in 1:1) {
              FnPrint__(2);
              inline_sym5__ = 1;
              inline_sym4__ = 42;
              break;
            }
          }
          FnPrint__(inline_sym1__ || inline_sym4__);
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
    target += theta && phi;
    target += theta || phi;
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
    target += log(fabs(determinant(x_matrix)));
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  (* TODO: make sure that these
     temporaries do not get assigned level DataOnly unless appropriate *)
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        data real lcm_sym7__;
        data real lcm_sym6__;
        data int lcm_sym5__;
        data int lcm_sym4__;
        data int lcm_sym3__;
        {
          real temp;
          if((2 > 3)) {
            lcm_sym6__ = promote((2 * 2), real);
            temp = lcm_sym6__;
            ;
          } else {
            FnPrint__("hello");
            lcm_sym6__ = promote((2 * 2), real);
            ;
          }
          temp = lcm_sym6__;
          real temp2;
          if((3 >= 2)) {
            lcm_sym7__ = promote((2 * 3), real);
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

let%expect_test "cool example: expression propagation + partial evaluation + \
                 lazy code motion + dead code elimination" =
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
        [ Stmt.Fixed.
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
  print_s [%sexp (mir : Program.Typed.t)] ;
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
       (transform_inits ()) (output_vars ()) (prog_name "") (prog_path "")) |}]

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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        real w;
        {
          data int x;
          real y;
          real z;
          data real z_data;
          if((1 > 2)) y = (y + promote(x, real)); else y = (y + w);
          if((2 > 1)) z = y;
          if((3 > 1)) z_data = promote(x, real);
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
          if((1 > 2)) y = (y + promote(x, real)); else y = (y + w);
          if((2 > 1)) z = y;
          if((3 > 1)) z_data = promote(x, real);
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
  print_s [%sexp (mir.log_prob : Stmt.Located.t list)] ;
  [%expect
    {|
      (((pattern
         (Decl (decl_adtype AutoDiffable) (decl_id w) (decl_type (Sized SReal))
          (initialize true)))
        (meta <opaque>))
       ((pattern
         (Block
          (((pattern
             (Decl (decl_adtype DataOnly) (decl_id x) (decl_type (Sized SInt))
              (initialize true)))
            (meta <opaque>))
           ((pattern
             (Decl (decl_adtype AutoDiffable) (decl_id y) (decl_type (Sized SReal))
              (initialize true)))
            (meta <opaque>))
           ((pattern
             (Decl (decl_adtype AutoDiffable) (decl_id z) (decl_type (Sized SReal))
              (initialize true)))
            (meta <opaque>))
           ((pattern
             (Decl (decl_adtype DataOnly) (decl_id z_data)
              (decl_type (Sized SReal)) (initialize true)))
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
                (Assignment (y UReal ())
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
                 (Assignment (y UReal ())
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
                (Assignment (z UReal ())
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
                (Assignment (z_data UReal ())
                 ((pattern
                   (Promotion
                    ((pattern (Var x))
                     (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                    UReal DataOnly))
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
  Fmt.str "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        real w;
        data real w_trans;
        w_trans = promote(1, real);
        {
          data int x;
          array[real, 2] y;
          real z;
          data real z_data;
          if((1 > 2)) y[1] = (y[1] + promote(x, real)); else y[2] = (y[2] + w);
          if((2 > 1)) z = y[1];
          if((3 > 1)) z_data = promote(x, real);
          FnPrint__(z);
          FnPrint__(z_data);
        }
      }

      generate_quantities {
        data real w;
        data real w_trans;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        w_trans = promote(1, real);
        {
          data int x;
          data array[real, 2] y;
          data real z;
          data real z_data;
          if((1 > 2)) y[1] = (y[1] + promote(x, real)); else y[2] = (y[2] + w);
          if((2 > 1)) z = y[1];
          if((3 > 1)) z_data = promote(x, real);
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
  let unpattern p = {Stmt.Fixed.pattern= p; meta= Location_span.empty} in
  let s =
    Stmt.Fixed.Pattern.NRFunApp
      ( CompilerInternal (FnWriteParam {var= from; unconstrain_opt= None})
      , [from] ) in
  let m = Expr.Typed.Map.of_alist_exn [(from, into)] in
  let s' = expr_subst_stmt_base m s in
  Fmt.str "@[<v>%a@]" Stmt.Located.pp (unpattern s') |> print_endline ;
  [%expect {| (FnWriteParam(unconstrain_opt())(var y))__(y); |}]
