open Core_kernel
open Frontend
open Analysis_and_optimization.Optimize
open Middle
open Common
open Analysis_and_optimization.Mir_utils

let semantic_check_program ast =
  Option.value_exn
    (Result.ok
       (Semantic_check.semantic_check_program
          (Option.value_exn (Result.ok ast))))

let%expect_test "map_rec_stmt_loc" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let f = function
    | Stmt.Fixed.Pattern.NRFunApp (CompilerInternal, "FnPrint__", [s]) ->
        Stmt.Fixed.Pattern.NRFunApp (CompilerInternal, "FnPrint__", [s; s])
    | x -> x
  in
  let mir = Program.map Fn.id (map_rec_stmt_loc f) mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let f i = function
    | Stmt.Fixed.Pattern.NRFunApp (CompilerInternal, "FnPrint__", [s]) ->
        Stmt.Fixed.Pattern.
          (NRFunApp (CompilerInternal, "FnPrint__", [s; s]), i + 1)
    | x -> (x, i)
  in
  let mir_stmt, num =
    (map_rec_state_stmt_loc f 0)
      Stmt.Fixed.{pattern= SList mir.log_prob; meta= Location_span.empty}
  in
  let mir = {mir with log_prob= [mir_stmt]} in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          for(sym1__ in 1:1) {
            FnPrint__(3);
            FnPrint__(FnMakeRowVec__(FnMakeRowVec__(3, 2), FnMakeRowVec__(4, 6)));
          }
          real sym4__;
          for(sym3__ in 1:1) {
            sym4__ = (53 ^ 2);
            break;
          }
          FnReject__(sym4__);
        }
      }

      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "inline functions 2" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
        for(sym3__ in 1:1) {
          for(sym1__ in 1:1) {

          }
        }
      } |}]

let%expect_test "list collapsing" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  let mir = list_collapsing mir in
  print_s [%sexp (mir : Middle.Program.Typed.t)] ;
  [%expect
    {|
    ((functions_block
      (((fdrt ()) (fdname f)
        (fdargs ((AutoDiffable x UInt) (AutoDiffable y UMatrix)))
        (fdbody
         ((pattern
           (Block
            (((pattern
               (NRFunApp CompilerInternal FnPrint__
                (((pattern (Var x))
                  (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
              (meta <opaque>))
             ((pattern
               (NRFunApp CompilerInternal FnPrint__
                (((pattern (Var y))
                  (meta ((type_ UMatrix) (loc <opaque>) (adlevel AutoDiffable)))))))
              (meta <opaque>)))))
          (meta <opaque>)))
        (fdloc <opaque>))
       ((fdrt (UReal)) (fdname g) (fdargs ((AutoDiffable z UInt)))
        (fdbody
         ((pattern
           (Block
            (((pattern
               (Return
                (((pattern
                   (FunApp StanLib Pow__
                    (((pattern (Var z))
                      (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                     ((pattern (Lit Int 2))
                      (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
                  (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
              (meta <opaque>)))))
          (meta <opaque>)))
        (fdloc <opaque>))))
     (input_vars ()) (prepare_data ())
     (log_prob
      (((pattern
         (Block
          (((pattern
             (For (loopvar sym1__)
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
                     (NRFunApp CompilerInternal FnPrint__
                      (((pattern (Lit Int 3))
                        (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
                    (meta <opaque>))
                   ((pattern
                     (NRFunApp CompilerInternal FnPrint__
                      (((pattern
                         (FunApp CompilerInternal FnMakeRowVec__
                          (((pattern
                             (FunApp CompilerInternal FnMakeRowVec__
                              (((pattern (Lit Int 3))
                                (meta
                                 ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                               ((pattern (Lit Int 2))
                                (meta
                                 ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
                            (meta
                             ((type_ URowVector) (loc <opaque>)
                              (adlevel DataOnly))))
                           ((pattern
                             (FunApp CompilerInternal FnMakeRowVec__
                              (((pattern (Lit Int 4))
                                (meta
                                 ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                               ((pattern (Lit Int 6))
                                (meta
                                 ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
                            (meta
                             ((type_ URowVector) (loc <opaque>)
                              (adlevel DataOnly)))))))
                        (meta
                         ((type_ UMatrix) (loc <opaque>) (adlevel DataOnly)))))))
                    (meta <opaque>)))))
                (meta <opaque>)))))
            (meta <opaque>))
           ((pattern
             (Decl (decl_adtype AutoDiffable) (decl_id sym4__)
              (decl_type (Unsized UReal))))
            (meta <opaque>))
           ((pattern
             (For (loopvar sym3__)
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
                     (Assignment (sym4__ UReal ())
                      ((pattern
                        (FunApp StanLib Pow__
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
             (NRFunApp CompilerInternal FnReject__
              (((pattern (Var sym4__))
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
            (FunApp StanLib PNot__
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
            (FunApp StanLib PNot__
             (((pattern (Var emit_generated_quantities__))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
          ((pattern (Return ())) (meta <opaque>)) ()))
        (meta <opaque>))))
     (transform_inits ()) (output_vars ()) (prog_name "") (prog_path ""))
    |}]

let%expect_test "do not inline recursive functions" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int sym3__;
          int sym6__;
          for(sym2__ in 1:1) {
            FnPrint__("f");
            sym3__ = 42;
            break;
          }
          for(sym5__ in 1:1) {
            FnPrint__("g");
            sym6__ = (3 + 24);
            break;
          }
          for(i in sym3__:sym6__) {
            {
              FnPrint__("body");
            }
            for(sym5__ in 1:1) {
              FnPrint__("g");
              sym6__ = (3 + 24);
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int sym9__;
          int sym12__;
          for(sym8__ in 1:1) {
            FnPrint__("f");
            sym9__ = 42;
            break;
          }
          for(sym11__ in 1:1) {
            FnPrint__("g");
            int sym13__;
            for(sym5__ in 1:1) {
              FnPrint__("f");
              sym13__ = 42;
              break;
            }
            sym12__ = (sym13__ + 24);
            break;
          }
          for(i in sym9__:sym12__) {
            {
              FnPrint__("body");
            }
            for(sym11__ in 1:1) {
              FnPrint__("g");
              int sym13__;
              for(sym5__ in 1:1) {
                FnPrint__("f");
                sym13__ = 42;
                break;
              }
              sym12__ = (sym13__ + 24);
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int sym3__;
          for(sym2__ in 1:1) {
            FnPrint__("g");
            sym3__ = (3 + 24);
            break;
          }
          while(sym3__) {
            FnPrint__("body");
            for(sym2__ in 1:1) {
              FnPrint__("g");
              sym3__ = (3 + 24);
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int sym3__;
          for(sym2__ in 1:1) {
            FnPrint__("g");
            sym3__ = (3 + 24);
            break;
          }
          if(sym3__) FnPrint__("body");
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int sym3__;
          int sym6__;
          int sym9__;
          for(sym2__ in 1:1) {
            FnPrint__("f");
            sym3__ = 42;
            break;
          }
          if(sym3__) {
            for(sym5__ in 1:1) {
              FnPrint__("g");
              sym6__ = (3 + 24);
              break;
            }
          } else {
            for(sym8__ in 1:1) {
              FnPrint__("h");
              sym9__ = (4 + 4);
              break;
            }
          }
          FnPrint__(sym3__ ?sym6__: sym9__);
        }
      }

      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "inline function multiple returns " =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int sym3__;
          for(sym2__ in 1:1) {
            if(2) {
              FnPrint__("f");
              sym3__ = 42;
              break;
            }
            sym3__ = 6;
            break;
          }
          FnPrint__(sym3__);
        }
      }

      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "inline function indices " =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      functions {
        int f(int z) {
          print(z);
          return 42;
        }
      }
      model {
        int a[2, 2];
        print(a[f(1), f(2)]);
      }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int sym6__;
          int sym3__;
          for(sym5__ in 1:1) {
            FnPrint__(2);
            sym6__ = 42;
            break;
          }
          for(sym2__ in 1:1) {
            FnPrint__(1);
            sym3__ = 42;
            break;
          }
          FnPrint__(a[sym3__, sym6__]);
        }
      }

      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "inline function and " =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int sym3__;
          int sym6__;
          for(sym2__ in 1:1) {
            FnPrint__(1);
            sym3__ = 42;
            break;
          }
          if(sym3__) {
            for(sym5__ in 1:1) {
              FnPrint__(2);
              sym6__ = 42;
              break;
            }
          }
          FnPrint__(sym3__ && sym6__);
        }
      }

      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "inline function or " =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = function_inlining mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
          int sym3__;
          int sym6__;
          for(sym2__ in 1:1) {
            FnPrint__(1);
            sym3__ = 42;
            break;
          }
          if(sym3__) ; else {
            for(sym5__ in 1:1) {
              FnPrint__(2);
              sym6__ = 42;
              break;
            }
          }
          FnPrint__(sym3__ || sym6__);
        }
      }

      generate_quantities {
        if(emit_transformed_parameters__) ; else {

        }
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "unroll nested loop" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|      model {
                for (i in 1:2)
                  for (j in 3:4)
                    print(i, j);
                   }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = static_loop_unrolling mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  let _ = Gensym.reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|      model {
                for (i in 1:2)
                  for (j in i:4)
                    for (k in j:9)
                       print(i, j, k);
                   }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = static_loop_unrolling mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  let _ = Gensym.reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|      model {
                for (i in 1:2)
                  for (j in i:4)
                    for (k in j:i+j)
                       print(i, j, k);
                   }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = static_loop_unrolling mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|      model {
                for (i in 1:2)
                  for (j in 3:4) {
                    print(i);
                    break;
                  }
              }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = static_loop_unrolling mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = constant_propagation mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = constant_propagation mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = constant_propagation mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = expression_propagation mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      transformed data {
        int i;
        int j;
        j = i;
        int k;
        k = 2 * j;
      }
      model {
        for (x in 1:i) {
          print(i + j + k);
        }
      }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = copy_propagation mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      prepare_data {
        data int i;
        data int j;
        j = i;
        data int k;
        k = (2 * i);
      }

      log_prob {
        {
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      transformed data {
        int i[2];
        i[1] = 2;
        i = {3, 2};
        int j[2];
        j = {3, 2};
        j[1] = 2;
      }
      model {
        print(i);
        print(j);
      }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      prepare_data {
        data array[int, 2] i;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int i;
        print(i);
        for (j in 3:5);
      }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = dead_code_elimination mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = partial_evaluation mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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

let%expect_test "try partially evaluate" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = partial_evaluation mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        {
          real x;
          real y;
          vector[2] a;
          vector[2] b;
          FnPrint__(log_diff_exp(x, y));
          FnPrint__(log((exp(a) - exp(b))));
        }
      }

      generate_quantities {
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      } |}]

let%expect_test "partially evaluate with equality check" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        vector[2] x;
        vector[2] y;
        print(dot_product(x, x));
        print(dot_product(x, y));
      }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = partial_evaluation mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
    int y_arr[3] = {32, 2, 35};
    target += +i;
    target += -i;
    target += !i;
    target += +theta;
    target += -theta;
    target += i+j;
    target += i-j;
    target += i*j;
    target += i/j;
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
    target += bernoulli_logit_lpmf(y_arr| (theta + x_matrix * x_vector));
    target += bernoulli_logit_lpmf(y_arr| (x_matrix * x_vector + theta));
    target += bernoulli_logit_lpmf(y_arr| (x_matrix * x_vector));
    target += bernoulli_lpmf(y_arr| inv_logit(x_vector));
    target += binomial_lpmf(y_arr| j, inv_logit(x_vector));
    target += categorical_lpmf(y_arr| inv_logit(x_vector));
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
    target += neg_binomial_2_lpmf(y_arr| exp(theta + x_matrix * x_vector), phi);
    target += neg_binomial_2_lpmf(y_arr| exp(x_matrix * x_vector + theta), phi);
    target += neg_binomial_2_lpmf(y_arr| exp(x_matrix * x_vector), phi);
    target += neg_binomial_2_log_lpmf(y_arr| (theta + x_matrix * x_vector), phi);
    target += neg_binomial_2_log_lpmf(y_arr| (x_matrix * x_vector + theta), phi);
    target += neg_binomial_2_log_lpmf(y_arr| (x_matrix * x_vector), phi);
    target += neg_binomial_2_lpmf(y_arr| exp(theta), phi);
    target += normal_lpdf(y_vector| theta + x_matrix * x_vector, phi);
    target += normal_lpdf(y_vector| x_matrix * x_vector + theta, phi);
    target += normal_lpdf(y_vector| x_matrix * x_vector, phi);
    target += poisson_lpmf(y_arr| exp(theta + x_matrix * x_vector));
    target += poisson_lpmf(y_arr| exp(x_matrix * x_vector + theta));
    target += poisson_lpmf(y_arr| exp(x_matrix * x_vector));
    target += poisson_log_lpmf(y_arr| (theta + x_matrix * x_vector));
    target += poisson_log_lpmf(y_arr| (x_matrix * x_vector + theta));
    target += poisson_log_lpmf(y_arr| (x_matrix * x_vector));
    target += poisson_lpmf(y_arr| exp(x_vector));
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = constant_propagation mir in
  let mir = partial_evaluation mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      Info: Found int division at 'string', line 27, column 14 to column 15:
        i / j
      Values will be rounded towards zero.



      log_prob {
        matrix[3, 2] x_matrix;
        matrix[2, 4] y_matrix;
        matrix[4, 2] z_matrix;
        vector[2] x_vector;
        vector[3] y_vector;
        matrix[2, 2] x_cov;
        x_cov = FnConstrain__(x_cov, "cov_matrix", 2);
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
          target += bernoulli_logit_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += bernoulli_logit_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += bernoulli_logit_glm_lpmf(y_arr, x_matrix, 0, x_vector);
          target += bernoulli_logit_lpmf(y_arr, x_vector);
          target += binomial_logit_lpmf(y_arr, 32, x_vector);
          target += categorical_logit_lpmf(y_arr, x_vector);
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
          target += neg_binomial_2_log_glm_lpmf(y_arr, x_matrix, 34., x_vector, 5.);
          target += neg_binomial_2_log_glm_lpmf(y_arr, x_matrix, 34., x_vector, 5.);
          target += neg_binomial_2_log_glm_lpmf(y_arr, x_matrix, 0, x_vector, 5.);
          target += neg_binomial_2_log_glm_lpmf(y_arr, x_matrix, 34., x_vector, 5.);
          target += neg_binomial_2_log_glm_lpmf(y_arr, x_matrix, 34., x_vector, 5.);
          target += neg_binomial_2_log_glm_lpmf(y_arr, x_matrix, 0, x_vector, 5.);
          target += neg_binomial_2_log_lpmf(y_arr, 34., 5.);
          target += normal_id_glm_lpdf(y_vector, x_matrix, 34., x_vector, 5.);
          target += normal_id_glm_lpdf(y_vector, x_matrix, 34., x_vector, 5.);
          target += normal_id_glm_lpdf(y_vector, x_matrix, 0, x_vector, 5.);
          target += poisson_log_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += poisson_log_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += poisson_log_glm_lpmf(y_arr, x_matrix, 0, x_vector);
          target += poisson_log_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += poisson_log_glm_lpmf(y_arr, x_matrix, 34., x_vector);
          target += poisson_log_glm_lpmf(y_arr, x_matrix, 0, x_vector);
          target += poisson_log_lpmf(y_arr, x_vector);
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
        x_cov = FnConstrain__(x_cov, "cov_matrix", 2);
        data real theta_u;
        data real phi_u;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        if(PNot__(emit_generated_quantities__)) return;
      }

      transform_inits {
        data matrix[3, 2] x_matrix;
        data matrix[2, 4] y_matrix;
        data matrix[4, 2] z_matrix;
        data vector[2] x_vector;
        data vector[3] y_vector;
        data matrix[2, 2] x_cov;
        data vector[3] x_cov_free__;
        x_cov_free__ = FnUnconstrain__(x_cov, "cov_matrix");
        data real theta_u;
        data real phi_u;
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        print({3.0});
        print({3.0});
        print({3.0});
      }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
    log_prob {
      data real[] sym3__;
      {
        sym3__ = FnMakeArray__(3.0);
        FnPrint__(sym3__);
        FnPrint__(sym3__);
        FnPrint__(sym3__);
      }
    }

    generate_quantities {
      data int sym2__;
      data int sym1__;
      if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
        return;
        ;
      } else ;
      if(PNot__(emit_generated_quantities__)) {
        return;
        ;
      } else ;
    } |}]

let%expect_test "lazy code motion, 2" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        for (i in 1:2)
          print(3 + 4);
      }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        data int sym4__;
        data int sym3__;
        {
          for(i in 1:2) {
            {
              FnPrint__((3 + 4));
            }
            ;
          }
        }
      }

      generate_quantities {
        data int sym2__;
        data int sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
          return;
          ;
        } else ;
        if(PNot__(emit_generated_quantities__)) {
          return;
          ;
        } else ;
      } |}]

let%expect_test "lazy code motion, 3" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        print(3);
        print(3 + 5);
        print((3 + 5) + 7);
      }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        data int sym4__;
        data int sym3__;
        {
          FnPrint__(3);
          sym3__ = (3 + 5);
          FnPrint__(sym3__);
          FnPrint__((sym3__ + 7));
        }
      }

      generate_quantities {
        data int sym2__;
        data int sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
          return;
          ;
        } else ;
        if(PNot__(emit_generated_quantities__)) {
          return;
          ;
        } else ;
      } |}]

let%expect_test "lazy code motion, 4" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  (* TODO: make sure that these
     temporaries do not get assigned level DataOnly unless appropriate *)
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        data int sym3__;
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
            sym3__ = (b + c);
            ;
          } else {
            {
              sym3__ = (b + c);
              x = sym3__;
              ;
            }
            ;
          }
          y = sym3__;
        }
      }

      generate_quantities {
        data int sym2__;
        data int sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
          return;
          ;
        } else ;
        if(PNot__(emit_generated_quantities__)) {
          return;
          ;
        } else ;
      } |}]

let%expect_test "lazy code motion, 5" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        data int sym3__;
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
            sym3__ = (b + c);
            ;
          } else {
            {
              if(2) {
                sym3__ = (b + c);
                x = sym3__;
                ;
              } else sym3__ = (b + c);
                     ;
              ;
            }
            ;
          }
          y = sym3__;
        }
      }

      generate_quantities {
        data int sym2__;
        data int sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
          return;
          ;
        } else ;
        if(PNot__(emit_generated_quantities__)) {
          return;
          ;
        } else ;
      } |}]

let%expect_test "lazy code motion, 6" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        data int sym4__;
        data int sym3__;
        {
          int x;
          int y;
          if(2) {
            x = (1 + 2);
            ;
          } else ;
          y = (4 + 3);
        }
      }

      generate_quantities {
        data int sym2__;
        data int sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
          return;
          ;
        } else ;
        if(PNot__(emit_generated_quantities__)) {
          return;
          ;
        } else ;
      } |}]

let%expect_test "lazy code motion, 7" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        data int sym4__;
        data int sym3__;
        {
          int a;
          int b;
          int c;
          int x;
          int y;
          int z;
          if(1) {
            {
              a = c;
              x = (a + b);
            }
            ;
          } else {
            ;
            ;
          }
          if(2) {
            {
              if(3) {
                {
                  sym4__ = (a + b);
                  ;
                  while(4) {
                    y = sym4__;
                    ;
                  }
                  ;
                }
                ;
              } else {
                {
                  ;
                  while(5) {
                    ;
                    ;
                  }
                  sym4__ = (a + b);
                  y = sym4__;
                }
                ;
              }
              z = sym4__;
            }
            ;
          } else {
            ;
            ;
          }
          ;
        }
      }

      generate_quantities {
        data int sym2__;
        data int sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
          return;
          ;
        } else ;
        if(PNot__(emit_generated_quantities__)) {
          return;
          ;
        } else ;
      } |}]

let%expect_test "lazy code motion, 8, _lp functions not optimized" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
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
        data int sym3__;
        {
          FnPrint__(foo(foo_lp(1)));
          FnPrint__(foo(foo_lp(1)));
          sym3__ = foo(foo(1));
          FnPrint__(sym3__);
          FnPrint__(sym3__);
        }
      }

      generate_quantities {
        data int sym2__;
        data int sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
          return;
          ;
        } else ;
        if(PNot__(emit_generated_quantities__)) {
          return;
          ;
        } else ;
      } |}]

let%expect_test "lazy code motion, 9" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        int x;
        while (x * 2) print("hello") ;
      }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        data int sym3__;
        {
          int x;
          while((x * 2)) {
            FnPrint__("hello");
            ;
          }
        }
      }

      generate_quantities {
        data int sym2__;
        data int sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
          return;
          ;
        } else ;
        if(PNot__(emit_generated_quantities__)) {
          return;
          ;
        } else ;
      } |}]

let%expect_test "lazy code motion, 10" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        data int sym3__;
        {
          int x;
          x = 3;
          FnPrint__((x * 2));
          x = 2;
          FnPrint__((x * 2));
        }
      }

      generate_quantities {
        data int sym2__;
        data int sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
          return;
          ;
        } else ;
        if(PNot__(emit_generated_quantities__)) {
          return;
          ;
        } else ;
      } |}]

let%expect_test "lazy code motion, 11" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        data int sym3__;
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
        data int sym2__;
        data int sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
          return;
          ;
        } else ;
        if(PNot__(emit_generated_quantities__)) {
          return;
          ;
        } else ;
      } |}]

let%expect_test "lazy code motion, 12" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        data int sym4__;
        data int sym3__;
        {
          int x;
          for(i in 1:6) {
            {
              FnPrint__((x + 42));
              x = 3;
            }
            ;
          }
        }
      }

      generate_quantities {
        data int sym2__;
        data int sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
          return;
          ;
        } else ;
        if(PNot__(emit_generated_quantities__)) {
          return;
          ;
        } else ;
      } |}]

let%expect_test "lazy code motion, 13" =
  let _ = Gensym.reset_danger_use_cautiously () in
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = one_step_loop_unrolling mir in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        data int sym10__;
        data int sym9__;
        data int sym8__;
        data int sym7__;
        data int sym6__;
        real sym5__;
        real sym4__;
        data int sym3__;
        {
          real temp;
          if((2 > 3)) {
            sym9__ = (2 * 2);
            temp = sym9__;
            ;
          } else {
            FnPrint__("hello");
            sym9__ = (2 * 2);
            ;
          }
          temp = sym9__;
          real temp2;
          if((2 <= 3)) {
            {
              {
                sym10__ = (2 * 3);
                temp2 = sym10__;
                sym4__ = temp;
                target += sym4__;
                sym8__ = (2 + 1);
                target += temp2;
              }
              for(i in sym8__:3) {
                {
                  temp2 = sym10__;
                  target += sym4__;
                  target += temp2;
                }
                ;
              }
            }
            ;
          } else ;
        }
      }

      generate_quantities {
        data int sym2__;
        data int sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
          return;
          ;
        } else ;
        if(PNot__(emit_generated_quantities__)) {
          return;
          ;
        } else ;
      } |}]

let%expect_test "cool example: expression propagation + partial evaluation + \
                 lazy code motion + dead code elimination" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = expression_propagation mir in
  let mir = partial_evaluation mir in
  let mir = one_step_loop_unrolling mir in
  let mir = lazy_code_motion mir in
  let mir = list_collapsing mir in
  let mir = dead_code_elimination mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        real sym7__;
        real sym6__;
        data int sym5__;
        data int sym4__;
        data int sym3__;
        {
          real x;
          int y;
          real theta;
          if((1 <= 100000)) {
            {
              {
                sym5__ = (1 + 1);
                sym6__ = bernoulli_logit_lpmf(y, x);
                target += sym6__;
              }
              for(i in sym5__:100000) {
                {
                  target += sym6__;
                }
              }
            }
          } else ;
        }
      }

      generate_quantities {
        data int sym2__;
        data int sym1__;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) {
          return;
        } else ;
        if(PNot__(emit_generated_quantities__)) {
          return;
        } else ;
      } |}]

let%expect_test "block fixing" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
      }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
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
            ; meta= Location_span.empty } ] }
  in
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
              (FunApp StanLib PNot__
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
              (FunApp StanLib PNot__
               (((pattern (Var emit_generated_quantities__))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
            ((pattern (Return ())) (meta <opaque>)) ()))
          (meta <opaque>))))
       (transform_inits ()) (output_vars ()) (prog_name "") (prog_path "")) |}]

let%expect_test "one-step loop unrolling" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      transformed data {
        int x;
        for (i in x:6) print("hello");
        while (1<2) print("goodbye");
        for (i in 1:1) for (j in 2:2) print("nested");
      }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = one_step_loop_unrolling mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      prepare_data {
        data int x;
        if((x <= 6)) {
          {
            FnPrint__("hello");
          }
          for(i in (x + 1):6) {
            FnPrint__("hello");
          }
        }
        if((1 < 2)) {
          FnPrint__("goodbye");
          while((1 < 2)) FnPrint__("goodbye");
        }
        if((1 <= 1)) {
          {
            if((2 <= 2)) {
              {
                FnPrint__("nested");
              }
              for(j in (2 + 1):2) {
                FnPrint__("nested");
              }
            }
          }
          for(i in (1 + 1):1) {
            if((2 <= 2)) {
              {
                FnPrint__("nested");
              }
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
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = optimize_ad_levels mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        real w;
        {
          data int x;
          real y;
          real z;
          data real z_data;
          if((1 > 2)) y = (y + x); else y = (y + w);
          if((2 > 1)) z = y;
          if((3 > 1)) z_data = x;
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
          if((1 > 2)) y = (y + x); else y = (y + w);
          if((2 > 1)) z = y;
          if((3 > 1)) z_data = x;
          FnPrint__(z);
          FnPrint__(z_data);
        }
        if(PNot__(emit_generated_quantities__)) return;
      }

      transform_inits {
        data real w;
      }

      output_vars {
        parameters real w; //real
      } |}]

let%expect_test "adlevel_optimization expressions" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = optimize_ad_levels mir in
  print_s [%sexp (mir.log_prob : Stmt.Located.t list)] ;
  [%expect
    {|
      (((pattern
         (Decl (decl_adtype AutoDiffable) (decl_id w) (decl_type (Sized SReal))))
        (meta <opaque>))
       ((pattern
         (Block
          (((pattern
             (Decl (decl_adtype DataOnly) (decl_id x) (decl_type (Sized SInt))))
            (meta <opaque>))
           ((pattern
             (Decl (decl_adtype AutoDiffable) (decl_id y)
              (decl_type (Sized SReal))))
            (meta <opaque>))
           ((pattern
             (Decl (decl_adtype AutoDiffable) (decl_id z)
              (decl_type (Sized SReal))))
            (meta <opaque>))
           ((pattern
             (Decl (decl_adtype DataOnly) (decl_id z_data)
              (decl_type (Sized SReal))))
            (meta <opaque>))
           ((pattern
             (IfElse
              ((pattern
                (FunApp StanLib Greater__
                 (((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  ((pattern (Lit Int 2))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
              ((pattern
                (Assignment (y UReal ())
                 ((pattern
                   (FunApp StanLib Plus__
                    (((pattern (Var y))
                      (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
                     ((pattern (Var x))
                      (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
                  (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))))
               (meta <opaque>))
              (((pattern
                 (Assignment (y UReal ())
                  ((pattern
                    (FunApp StanLib Plus__
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
                (FunApp StanLib Greater__
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
                (FunApp StanLib Greater__
                 (((pattern (Lit Int 3))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  ((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
              ((pattern
                (Assignment (z_data UReal ())
                 ((pattern (Var x))
                  (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
               (meta <opaque>))
              ()))
            (meta <opaque>))
           ((pattern
             (NRFunApp CompilerInternal FnPrint__
              (((pattern (Var z))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
            (meta <opaque>))
           ((pattern
             (NRFunApp CompilerInternal FnPrint__
              (((pattern (Var z_data))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
            (meta <opaque>)))))
        (meta <opaque>))) |}]

let%expect_test "adlevel_optimization 2" =
  Gensym.reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      parameters {
        real w;
      }
      transformed parameters {
        real w_trans = 1;
        {
          int x;
          real y[2];
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
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = optimize_ad_levels mir in
  Fmt.strf "@[<v>%a@]" Program.Typed.pp mir |> print_endline ;
  [%expect
    {|
      log_prob {
        real w;
        data real w_trans;
        w_trans = 1;
        {
          data int x;
          array[real, 2] y;
          real z;
          data real z_data;
          if((1 > 2)) y[1] = (y[1] + x); else y[2] = (y[2] + w);
          if((2 > 1)) z = y[1];
          if((3 > 1)) z_data = x;
          FnPrint__(z);
          FnPrint__(z_data);
        }
      }

      generate_quantities {
        data real w;
        data real w_trans;
        if(PNot__(emit_transformed_parameters__ || emit_generated_quantities__)) return;
        w_trans = 1;
        {
          data int x;
          data array[real, 2] y;
          data real z;
          data real z_data;
          if((1 > 2)) y[1] = (y[1] + x); else y[2] = (y[2] + w);
          if((2 > 1)) z = y[1];
          if((3 > 1)) z_data = x;
          FnPrint__(z);
          FnPrint__(z_data);
        }
        if(PNot__(emit_generated_quantities__)) return;
      }

      transform_inits {
        data real w;
      }

      output_vars {
        parameters real w; //real
        transformed_parameters real w_trans; //real
      } |}]
