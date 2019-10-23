open Frontend
open Analysis_and_optimization.Factor_graph
open Core_kernel
open Analysis_and_optimization.Dataflow_types

let semantic_check_program ast =
  Option.value_exn
    (Result.ok
       (Semantic_check.semantic_check_program
          (Option.value_exn (Result.ok ast))))

let example1_program =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        model
        {                                // 1
          int i                          // 2: 3
              = 0;                       //    4
          if (i < 0)                     // 5
          {                              // 6
            print(i);                    // 7
          } else
          {                              // 8
            for (j in 1:10)              // 9
            {                            // 10
              if (j > 9)                 // 11
              {                          // 12
                break;                   // 13
              }
              if (j > 8 && i < -1)       // 14
              {                          // 15
                continue;                // 16
              }
              if (j > 5)                 // 17
              {                          // 18
                reject(0);               // 19
              } else
              {                          // 20
                target += 1 + 1;         // 21
              }
              print("Fin");              // 22
            }
          }
        }
      |}
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Variable dependency example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps = prog_factor_graph example1_program in
  print_s [%sexp (deps : (label * factor * vexpr Set.Poly.t) list)] ;
  [%expect
    {|
      ((19 Reject ((VVar i) (VVar j)))
       (21
        (TargetTerm
         ((pattern (Lit Int 1))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
        ((VVar i) (VVar j)))
       (21
        (TargetTerm
         ((pattern (Lit Int 1))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
        ((VVar i) (VVar j))))
    |}]
