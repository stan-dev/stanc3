open Core_kernel
open Middle
open Dataflow_types
open Mir_utils

(*open Dataflow_utils*)
open Dependence_analysis

type factor =
  | TargetTerm of expr_typed_located
  | Reject
  | LPFunction of (string * expr_typed_located list)
[@@deriving sexp, hash, compare]

let extract_factors_statement (stmt : (expr_typed_located, 's) statement) :
    factor list =
  match stmt with
  | Middle.TargetPE e -> List.map (summation_terms e) ~f:(fun x -> TargetTerm x)
  | Middle.NRFunApp (_, f, _) when internal_fn_of_string f = Some FnReject ->
      [Reject]
  | Middle.NRFunApp (_, s, args) when String.suffix s 3 = "_lp" ->
      [LPFunction (s, args)]
  | Middle.Assignment (_, _)
   |Middle.NRFunApp (_, _, _)
   |Middle.Break | Middle.Continue | Middle.Return _ | Middle.Skip
   |Middle.IfElse (_, _, _)
   |Middle.While (_, _)
   |Middle.For _ | Middle.Block _ | Middle.SList _
   |Middle.Decl {decl_id= _; _} ->
      []

let rec extract_factors
    (statement_map :
      (label, (expr_typed_located, label) statement * 'm) Map.Poly.t)
    (label : label) : (label * factor) list =
  let stmt, _ = Map.Poly.find_exn statement_map label in
  let this_stmt =
    List.map (extract_factors_statement stmt) ~f:(fun x -> (label, x))
  in
  fold_statement
    (fun s _ -> s)
    (fun state label -> List.append state (extract_factors statement_map label))
    this_stmt stmt

let factor_rhs (factor : factor) : vexpr Set.Poly.t =
  match factor with
  | TargetTerm e -> expr_var_set e
  | Reject -> Set.Poly.empty
  | LPFunction (_, es) -> Set.Poly.of_list (List.map es ~f:vexpr_of_expr_exn)

let factor_var_dependencies
    (statement_map :
      (label, (expr_typed_located, label) statement * node_dep_info) Map.Poly.t)
    ((label, factor) : label * factor) : vexpr Set.Poly.t =
  let rhs = factor_rhs factor in
  let dep_labels = node_vars_dependencies statement_map rhs label in
  let label_vars l =
    stmt_rhs_var_set (fst (Map.Poly.find_exn statement_map l))
  in
  let dep_vars = union_map dep_labels ~f:label_vars in
  Set.Poly.union dep_vars rhs

(* So far just extracts factors in log_prob, finds their dependencies *)
let prog_factor_graph (prog : typed_prog) :
    (label * factor * vexpr Set.Poly.t) list =
  let statement_map = log_prob_build_dep_info_map prog in
  let factors = extract_factors statement_map 1 in
  List.map factors ~f:(fun (fac, l) ->
      (fac, l, factor_var_dependencies statement_map (fac, l)) )

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
  Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)

let%expect_test "Variable dependency example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps = prog_factor_graph example1_program in
  print_s [%sexp (deps : (label * factor * vexpr Set.Poly.t) list)] ;
  [%expect
    {|
      ((19 Reject ((VVar i) (VVar j)))
       (21 (TargetTerm (Lit Int 1)) ((VVar i) (VVar j)))
       (21 (TargetTerm (Lit Int 1)) ((VVar i) (VVar j))))
    |}]
