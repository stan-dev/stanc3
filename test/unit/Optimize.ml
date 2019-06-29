open Core_kernel
open Frontend
open Analysis_and_optimization.Optimize
open Middle
open Middle.Pretty
open Analysis_and_optimization.Mir_utils

let semantic_check_program ast =
  Option.value_exn
    (Result.ok
       (Semantic_check.semantic_check_program
          (Option.value_exn (Result.ok ast))))

let%expect_test "map_rec_stmt_loc" =
  gensym_reset_danger_use_cautiously () ;
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
    | NRFunApp (CompilerInternal, "FnPrint__", [s]) ->
        NRFunApp (CompilerInternal, "FnPrint__", [s; s])
    | x -> x
  in
  let mir = map_prog (fun x -> x) (map_rec_stmt_loc f) mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "map_rec_state_stmt_loc" =
  gensym_reset_danger_use_cautiously () ;
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
    | NRFunApp (CompilerInternal, "FnPrint__", [s]) ->
        (NRFunApp (CompilerInternal, "FnPrint__", [s; s]), i + 1)
    | x -> (x, i)
  in
  let mir_stmt, num =
    (map_rec_state_stmt_loc f 0)
      {stmt= SList mir.log_prob; smeta= Middle.no_span}
  in
  let mir = {mir with log_prob= [mir_stmt]} in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  print_endline (string_of_int num) ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

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

      }

      transform_inits {

      }

      output_vars {

      }

      3 |}]

let%expect_test "inline functions" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
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

      input_vars {

      }

      prepare_data {

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline functions 2" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
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

      input_vars {

      }

      prepare_data {

      }

      log_prob {

      }

      generate_quantities {
        if(emit_generated_quantities__) {
          for(sym3__ in 1:1) {
            for(sym1__ in 1:1) {

            }
          }
        }
      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "list collapsing" =
  gensym_reset_danger_use_cautiously () ;
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
  print_s [%sexp (mir : Middle.typed_prog)] ;
  [%expect
    {|
    ((functions_block
      (((fdrt ()) (fdname f)
        (fdargs ((AutoDiffable x UInt) (AutoDiffable y UMatrix)))
        (fdbody
         ((stmt
           (Block
            (((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var x))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var y))
                  (emeta
                   ((mtype UMatrix) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>)))))
          (smeta <opaque>)))
        (fdloc <opaque>))
       ((fdrt (UReal)) (fdname g) (fdargs ((AutoDiffable z UInt)))
        (fdbody
         ((stmt
           (Block
            (((stmt
               (Return
                (((expr
                   (FunApp StanLib Pow__
                    (((expr (Var z))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                     ((expr (Lit Int 2))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>)))))
          (smeta <opaque>)))
        (fdloc <opaque>))))
     (input_vars ()) (prepare_data ())
     (log_prob
      (((stmt
         (Block
          (((stmt
             (For (loopvar sym1__)
              (lower
               ((expr (Lit Int 1))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
              (upper
               ((expr (Lit Int 1))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
              (body
               ((stmt
                 (Block
                  (((stmt
                     (NRFunApp CompilerInternal FnPrint__
                      (((expr (Lit Int 3))
                        (emeta
                         ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                    (smeta <opaque>))
                   ((stmt
                     (NRFunApp CompilerInternal FnPrint__
                      (((expr
                         (FunApp CompilerInternal FnMakeRowVec__
                          (((expr
                             (FunApp CompilerInternal FnMakeRowVec__
                              (((expr (Lit Int 3))
                                (emeta
                                 ((mtype UInt) (mloc <opaque>)
                                  (madlevel DataOnly))))
                               ((expr (Lit Int 2))
                                (emeta
                                 ((mtype UInt) (mloc <opaque>)
                                  (madlevel DataOnly)))))))
                            (emeta
                             ((mtype URowVector) (mloc <opaque>)
                              (madlevel DataOnly))))
                           ((expr
                             (FunApp CompilerInternal FnMakeRowVec__
                              (((expr (Lit Int 4))
                                (emeta
                                 ((mtype UInt) (mloc <opaque>)
                                  (madlevel DataOnly))))
                               ((expr (Lit Int 6))
                                (emeta
                                 ((mtype UInt) (mloc <opaque>)
                                  (madlevel DataOnly)))))))
                            (emeta
                             ((mtype URowVector) (mloc <opaque>)
                              (madlevel DataOnly)))))))
                        (emeta
                         ((mtype UMatrix) (mloc <opaque>) (madlevel DataOnly)))))))
                    (smeta <opaque>)))))
                (smeta <opaque>)))))
            (smeta <opaque>))
           ((stmt
             (Decl (decl_adtype AutoDiffable) (decl_id sym4__)
              (decl_type (Unsized UReal))))
            (smeta <opaque>))
           ((stmt
             (For (loopvar sym3__)
              (lower
               ((expr (Lit Int 1))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
              (upper
               ((expr (Lit Int 1))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
              (body
               ((stmt
                 (Block
                  (((stmt
                     (Assignment (sym4__ ())
                      ((expr
                        (FunApp StanLib Pow__
                         (((expr (Lit Int 53))
                           (emeta
                            ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                          ((expr (Lit Int 2))
                           (emeta
                            ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                       (emeta
                        ((mtype UReal) (mloc <opaque>) (madlevel DataOnly))))))
                    (smeta <opaque>))
                   ((stmt Break) (smeta <opaque>)))))
                (smeta <opaque>)))))
            (smeta <opaque>))
           ((stmt
             (NRFunApp CompilerInternal FnReject__
              (((expr (Var sym4__))
                (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
            (smeta <opaque>)))))
        (smeta <opaque>))))
     (generate_quantities ()) (transform_inits ()) (output_vars ())
     (prog_name "") (prog_path ""))
    |}]

let%expect_test "do not inline recursive functions" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
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

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
          FnReject__(g(53));
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function in for loop" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
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

      input_vars {

      }

      prepare_data {

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

(* TODO: check test results from here *)

let%expect_test "inline function in for loop 2" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
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

      input_vars {

      }

      prepare_data {

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function in while loop" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
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

      input_vars {

      }

      prepare_data {

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function in if then else" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
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

      input_vars {

      }

      prepare_data {

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

      }

      transform_inits {

      }

      output_vars {

      }

    |}]

let%expect_test "inline function in ternary if " =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
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

      input_vars {

      }

      prepare_data {

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function multiple returns " =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
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

      input_vars {

      }

      prepare_data {

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function indices " =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
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

      input_vars {

      }

      prepare_data {

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function and " =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
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

      input_vars {

      }

      prepare_data {

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "inline function or " =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
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

      input_vars {

      }

      prepare_data {

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "unroll nested loop" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "unroll nested loop with break" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "constant propagation" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
    functions {

    }

    input_vars {

    }

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

    }

    transform_inits {

    }

    output_vars {

    } |}]

let%expect_test "constant propagation, local scope" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
    functions {

    }

    input_vars {

    }

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

    }

    transform_inits {

    }

    output_vars {

    } |}]

let%expect_test "constant propagation, model block local scope" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
    functions {

    }

    input_vars {

    }

    prepare_data {

    }

    log_prob {
      {
        int i;
        i = 42;
        int j;
        j = 2;
      }
    }

    generate_quantities {
      data int i;
      data int j;
      if(emit_generated_quantities__) {
        for(x in 1:i) {
          FnPrint__((i + j));
        }
        FnWriteParam__(i);
        FnWriteParam__(j);
      }
    }

    transform_inits {

    }

    output_vars {
      generated_quantities int i; //int
      generated_quantities int j; //int
    } |}]

let%expect_test "expression propagation" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "copy propagation" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "dead code elimination" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "dead code elimination decl" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
          int i;
        }
      }

      generate_quantities {
        if(emit_generated_quantities__) {
          data int i;
          FnPrint__(i);
        }
      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "dead code elimination, for loop" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
          int i;
          FnPrint__(i);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "dead code elimination, while loop" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
          int i;
          FnPrint__(i);
          while(1) ;
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "dead code elimination, if then" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "dead code elimination, nested" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
          int i;
          FnPrint__(i);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "partial evaluation" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "try partially evaluate" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "partially evaluate with equality check" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
          vector[2] x;
          vector[2] y;
          FnPrint__(dot_self(x));
          FnPrint__(dot_product(x, y));
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "partially evaluate glm" =
  gensym_reset_danger_use_cautiously () ;
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        matrix[2,3] x;
        int y[2];
        vector[2] y_real;
        vector[3] beta;
        vector[2] alpha;
        real sigma;
        print(bernoulli_lpmf(y| inv_logit(alpha + x * beta)));
        print(bernoulli_logit_lpmf(y| alpha + x * beta));
        print(bernoulli_lpmf(y| inv_logit(x * beta + alpha)));
        print(bernoulli_logit_lpmf(y| x * beta + alpha));
        print(bernoulli_lpmf(y| inv_logit(x * beta)));
        print(bernoulli_logit_lpmf(y| x * beta));
        print(neg_binomial_2_lpmf(y| exp(alpha + x * beta), sigma));
        print(neg_binomial_2_log_lpmf(y| alpha + x * beta, sigma));
        print(neg_binomial_2_lpmf(y| exp(x * beta + alpha), sigma));
        print(neg_binomial_2_log_lpmf(y| x * beta + alpha, sigma));
        print(neg_binomial_2_lpmf(y| exp(x * beta), sigma));
        print(neg_binomial_2_log_lpmf(y| x * beta, sigma));
        print(normal_lpdf(y_real| alpha + x * beta, sigma));
        print(normal_lpdf(y_real| x * beta + alpha, sigma));
        print(normal_lpdf(y_real| x * beta, sigma));
        print(poisson_lpmf(y| exp(alpha + x * beta)));
        print(poisson_log_lpmf(y| alpha + x * beta));
        print(poisson_lpmf(y| exp(x * beta + alpha)));
        print(poisson_log_lpmf(y| x * beta + alpha));
        print(poisson_lpmf(y| exp(x * beta)));
        print(poisson_log_lpmf(y| x * beta));
      }
      |}
  in
  let ast = semantic_check_program ast in
  let mir = Ast_to_Mir.trans_prog "" ast in
  let mir = partial_evaluation mir in
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        {
          matrix[2, 3] x;
          array[int, 2] y;
          vector[2] y_real;
          vector[3] beta;
          vector[2] alpha;
          real sigma;
          FnPrint__(bernoulli_logit_glm_lpmf(y, x, alpha, beta));
          FnPrint__(bernoulli_logit_glm_lpmf(y, x, alpha, beta));
          FnPrint__(bernoulli_logit_glm_lpmf(y, x, alpha, beta));
          FnPrint__(bernoulli_logit_glm_lpmf(y, x, alpha, beta));
          FnPrint__(bernoulli_logit_glm_lpmf(y, x, 0, beta));
          FnPrint__(bernoulli_logit_glm_lpmf(y, x, 0, beta));
          FnPrint__(neg_binomial_2_log_glm_lpmf(y, x, alpha, beta, sigma));
          FnPrint__(neg_binomial_2_log_glm_lpmf(y, x, alpha, beta, sigma));
          FnPrint__(neg_binomial_2_log_glm_lpmf(y, x, alpha, beta, sigma));
          FnPrint__(neg_binomial_2_log_glm_lpmf(y, x, alpha, beta, sigma));
          FnPrint__(neg_binomial_2_log_glm_lpmf(y, x, 0, beta, sigma));
          FnPrint__(neg_binomial_2_log_glm_lpmf(y, x, 0, beta, sigma));
          FnPrint__(normal_id_glm_lpdf(y_real, x, alpha, beta, sigma));
          FnPrint__(normal_id_glm_lpdf(y_real, x, alpha, beta, sigma));
          FnPrint__(normal_id_glm_lpdf(y_real, x, 0, beta, sigma));
          FnPrint__(poisson_log_glm_lpmf(y, x, alpha, beta));
          FnPrint__(poisson_log_glm_lpmf(y, x, alpha, beta));
          FnPrint__(poisson_log_glm_lpmf(y, x, alpha, beta));
          FnPrint__(poisson_log_glm_lpmf(y, x, alpha, beta));
          FnPrint__(poisson_log_glm_lpmf(y, x, 0, beta));
          FnPrint__(poisson_log_glm_lpmf(y, x, 0, beta));
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
    functions {

    }

    input_vars {

    }

    prepare_data {

    }

    log_prob {
      data real[] sym1__;
      {
        sym1__ = FnMakeArray__(3.0);
        FnPrint__(sym1__);
        FnPrint__(sym1__);
        FnPrint__(sym1__);
      }
    }

    generate_quantities {

    }

    transform_inits {

    }

    output_vars {

    } |}]

let%expect_test "lazy code motion, 2" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym2__;
        data int sym1__;
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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 3" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym2__;
        data int sym1__;
        {
          FnPrint__(3);
          sym1__ = (3 + 5);
          FnPrint__(sym1__);
          FnPrint__((sym1__ + 7));
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 4" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym1__;
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
            sym1__ = (b + c);
            ;
          } else {
            {
              sym1__ = (b + c);
              x = sym1__;
              ;
            }
            ;
          }
          y = sym1__;
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 5" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym1__;
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
            sym1__ = (b + c);
            ;
          } else {
            {
              if(2) {
                sym1__ = (b + c);
                x = sym1__;
                ;
              } else sym1__ = (b + c);
                     ;
              ;
            }
            ;
          }
          y = sym1__;
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 6" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym2__;
        data int sym1__;
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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 7" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym2__;
        data int sym1__;
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
                  sym2__ = (a + b);
                  ;
                  while(4) {
                    y = sym2__;
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
                  sym2__ = (a + b);
                  y = sym2__;
                }
                ;
              }
              z = sym2__;
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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 8, _lp functions not optimized" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
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

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym1__;
        {
          FnPrint__(foo(foo_lp(1)));
          FnPrint__(foo(foo_lp(1)));
          sym1__ = foo(foo(1));
          FnPrint__(sym1__);
          FnPrint__(sym1__);
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 9" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym1__;
        {
          int x;
          while((x * 2)) {
            FnPrint__("hello");
            ;
          }
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 10" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym1__;
        {
          int x;
          x = 3;
          FnPrint__((x * 2));
          x = 2;
          FnPrint__((x * 2));
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 11" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym1__;
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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "lazy code motion, 12" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        data int sym2__;
        data int sym1__;
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

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "cool example: expression propagation + partial evaluation + \
                 lazy code motion + dead code elimination" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        real sym5__;
        real sym4__;
        data int sym3__;
        data int sym2__;
        data int sym1__;
        {
          real x;
          int y;
          real theta;
          if((1 <= 100000)) {
            {
              {
                sym3__ = (1 + 1);
                sym4__ = bernoulli_logit_lpmf(y, x);
                target += sym4__;
              }
              for(i in sym3__:100000) {
                {
                  target += sym4__;
                }
              }
            }
          } else ;
        }
      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "block fixing" =
  gensym_reset_danger_use_cautiously () ;
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
      Middle.log_prob=
        [ { stmt=
              IfElse
                ( zero
                , { stmt= While (zero, {stmt= SList []; smeta= no_span})
                  ; smeta= no_span }
                , None )
          ; smeta= no_span } ] }
  in
  let mir = block_fixing mir in
  print_s [%sexp (mir : Middle.typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (IfElse
            ((expr (Lit Int 0))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
            ((stmt
              (While
               ((expr (Lit Int 0))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
               ((stmt (Block ())) (smeta <opaque>))))
             (smeta <opaque>))
            ()))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let%expect_test "one-step loop unrolling" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

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

      log_prob {

      }

      generate_quantities {

      }

      transform_inits {

      }

      output_vars {

      } |}]

let%expect_test "adlevel_optimization" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        real w;
        w = FnReadParam__("w", "scalar");
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
        w = FnReadParam__("w", "scalar");
        FnWriteParam__(w);
        if(emit_transformed_parameters__) {
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
      }

      transform_inits {
        data real w;
        w = FnReadData__("w", "scalar");
        FnWriteParam__(w);
      }

      output_vars {
        parameters real w; //real
      } |}]

let%expect_test "adlevel_optimization expressions" =
  gensym_reset_danger_use_cautiously () ;
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
  print_s
    [%sexp
      ( mir.log_prob
        : ( mtype_loc_ad
          , (location_span sexp_opaque[@compare.ignore]) )
          stmt_with
          list )] ;
  [%expect
    {|
      (((stmt
         (Decl (decl_adtype AutoDiffable) (decl_id w) (decl_type (Sized SReal))))
        (smeta <opaque>))
       ((stmt
         (Assignment (w ())
          ((expr
            (FunApp CompilerInternal FnReadParam__
             (((expr (Lit Str w))
               (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly))))
              ((expr (Lit Str scalar))
               (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
           (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly))))))
        (smeta <opaque>))
       ((stmt
         (Block
          (((stmt
             (Decl (decl_adtype DataOnly) (decl_id x) (decl_type (Sized SInt))))
            (smeta <opaque>))
           ((stmt
             (Decl (decl_adtype AutoDiffable) (decl_id y)
              (decl_type (Sized SReal))))
            (smeta <opaque>))
           ((stmt
             (Decl (decl_adtype AutoDiffable) (decl_id z)
              (decl_type (Sized SReal))))
            (smeta <opaque>))
           ((stmt
             (Decl (decl_adtype DataOnly) (decl_id z_data)
              (decl_type (Sized SReal))))
            (smeta <opaque>))
           ((stmt
             (IfElse
              ((expr
                (FunApp StanLib Greater__
                 (((expr (Lit Int 1))
                   (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                  ((expr (Lit Int 2))
                   (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
               (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
              ((stmt
                (Assignment (y ())
                 ((expr
                   (FunApp StanLib Plus__
                    (((expr (Var y))
                      (emeta
                       ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable))))
                     ((expr (Var x))
                      (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable))))))
               (smeta <opaque>))
              (((stmt
                 (Assignment (y ())
                  ((expr
                    (FunApp StanLib Plus__
                     (((expr (Var y))
                       (emeta
                        ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable))))
                      ((expr (Var w))
                       (emeta
                        ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
                   (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable))))))
                (smeta <opaque>)))))
            (smeta <opaque>))
           ((stmt
             (IfElse
              ((expr
                (FunApp StanLib Greater__
                 (((expr (Lit Int 2))
                   (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                  ((expr (Lit Int 1))
                   (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
               (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
              ((stmt
                (Assignment (z ())
                 ((expr (Var y))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable))))))
               (smeta <opaque>))
              ()))
            (smeta <opaque>))
           ((stmt
             (IfElse
              ((expr
                (FunApp StanLib Greater__
                 (((expr (Lit Int 3))
                   (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                  ((expr (Lit Int 1))
                   (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
               (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
              ((stmt
                (Assignment (z_data ())
                 ((expr (Var x))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
               (smeta <opaque>))
              ()))
            (smeta <opaque>))
           ((stmt
             (NRFunApp CompilerInternal FnPrint__
              (((expr (Var z))
                (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
            (smeta <opaque>))
           ((stmt
             (NRFunApp CompilerInternal FnPrint__
              (((expr (Var z_data))
                (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
            (smeta <opaque>)))))
        (smeta <opaque>))) |}]

let%expect_test "adlevel_optimization 2" =
  gensym_reset_danger_use_cautiously () ;
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
  Fmt.strf "@[<v>%a@]" pp_typed_prog mir |> print_endline ;
  [%expect
    {|
      functions {

      }

      input_vars {

      }

      prepare_data {

      }

      log_prob {
        real w;
        w = FnReadParam__("w", "scalar");
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
        w = FnReadParam__("w", "scalar");
        FnWriteParam__(w);
        data real w_trans;
        if(emit_transformed_parameters__) {
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
          FnWriteParam__(w_trans);
        }
      }

      transform_inits {
        data real w;
        w = FnReadData__("w", "scalar");
        FnWriteParam__(w);
      }

      output_vars {
        parameters real w; //real
        transformed_parameters real w_trans; //real
      } |}]
