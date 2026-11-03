open Std
open Std.Sexp_conv
open Analysis_and_optimization.Dependence_analysis
open Analysis_and_optimization.Dataflow_utils
open Middle
open Analysis_and_optimization.Dataflow_types

(* These tests check {!Analysis_and_optimization.Dependence_analysis}. The
   module finds the earlier statements that each statement depends on. Pedantic
   mode and the factor graph use these statement dependencies.

   Each test builds a map from labels to statements with
   [build_log_prob_dependence_info]. A label is the integer that identifies a
   statement. The map covers the log density, which contains the statements of
   the parameters, transformed parameters and model blocks. The // comments in
   the first program give the labels. Label 1 is the root statement of the map,
   which holds all the other statements.

   A statement has a data dependency on an earlier write when the write can
   supply a value that the statement reads. Such a write flows to the read. A
   statement has a control dependency on an enclosing if or loop, and on a jump
   that can skip the statement. A jump is a [break], [continue] or [return].
   These ifs, loops and jumps are the controls of the statement. An immediate
   dependency is a direct data or control dependency. A transitive dependency is
   a dependency of a dependency.

   A definition is a write, recorded as the access of the write and the label of
   the statement that writes the variable. A definition reaches a read when the
   program can run the write before the read, and no write between them replaces
   the same element. These definitions are the reaching definitions of the read.
   Each statement records the reaching definitions at entry, before the
   statement runs. A later write kills an earlier write when the later write
   always replaces the element of the earlier write. The later write is the
   killer. A killed write no longer reaches later reads. A query then drops each
   reaching write that selects a different element than the read, or that runs
   after the read ({!Analysis_and_optimization.Dependence}).

   A definition from outside the analyzed statement, for example data, has no
   label and prints as [W N None]. Dependency results contain statement labels
   only, so these definitions do not appear in them.

   Most later tests print [label: statement // labels]. The labels after // are
   the immediate dependencies, which are the writes that the statement reads and
   the statements that control the statement. *)

let example1_program =
  Test_utils.mir_of_string
    {|
        model
        {                                // 2
          int i                          // 3
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
                continue;                // 19
              } else
              {                          // 20
                print("Badger", i + j);  // 21
              }
              print("Fin");              // 22
            }
          }
        }
      |}

let%expect_test "Dependency graph example" =
  let deps =
    dependence_graph (build_log_prob_dependence_info example1_program) in
  print_s [%sexp (deps : label Set.Poly.t LabelMap.t)];
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 ()) (5 (4)) (6 (4 5)) (7 (4 5)) (8 (4 5))
       (9 (4 5 11 13)) (10 (4 5 9 11 13)) (11 (4 5 9 13)) (12 (4 5 9 11 13))
       (13 (4 5 9 11)) (14 (4 5 9 11 13)) (15 (4 5 9 11 13 14))
       (16 (4 5 9 11 13 14)) (17 (4 5 9 11 13 14 16)) (18 (4 5 9 11 13 14 16 17))
       (19 (4 5 9 11 13 14 16 17)) (20 (4 5 9 11 13 14 16 17))
       (21 (4 5 9 11 13 14 16 17)) (22 (4 5 9 11 13 14 16 17 19)))
    |}]

module Accesses = Analysis_and_optimization.Accesses

(** Prints a set of definitions as [W N None, W a 2, W s [7, 12]]: each entry is
    the access, then the labels of the definitions with that access. The outside
    definitions come first. *)
let pp_definitions (info : dependence_info) ppf
    (definitions : reaching_definition Set.Poly.t) =
  let pp_access =
    Test_utils.one_line
      (Accesses.pp_access ~loop_name:(Test_utils.loop_name info)) in
  let entries =
    List.map (Set.Poly.to_list definitions) ~f:(fun (access, label) ->
        (Fmt.str "%a" pp_access access, label)) in
  let outside =
    List.filter_map entries ~f:(function
      | text, None -> Some (text ^ " None")
      | _, Some _ -> None) in
  (* The distinct access texts with a label, in set order. *)
  let texts =
    List.rev
      (List.fold_left entries ~init:[] ~f:(fun texts (text, label) ->
           match label with
           | Some _ when not (List.mem text ~set:texts) -> text :: texts
           | Some _ | None -> texts)) in
  let inside =
    List.map texts ~f:(fun text ->
        match
          List.filter_map entries ~f:(function
            | entry_text, Some label when String.equal entry_text text ->
                Some label
            | _, Some _ | _, None -> None)
        with
        | [label] -> Fmt.str "%s %d" text label
        | labels -> Fmt.str "%s [%a]" text Fmt.(list ~sep:(any ", ") int) labels)
  in
  Fmt.(list ~sep:(any ", ") string) ppf (outside @ inside)

(* The loop variable [j] of the loop at label 9 is not a definition: no
   statement writes a loop variable. *)
let%expect_test "Reaching definition records at statement entry" =
  let info = build_log_prob_dependence_info example1_program in
  LabelMap.iter info.reaching_definitions ~f:(fun ~key ~data ->
      Fmt.pr "%d: [%a]@." key (pp_definitions info) data);
  [%expect
    {|
    1: []
    2: [W i 4]
    3: []
    4: [W i 3]
    5: [W i 4]
    6: [W i 4]
    7: [W i 4]
    8: [W i 4]
    9: [W i 4]
    10: [W i 4]
    11: [W i 4]
    12: [W i 4]
    13: [W i 4]
    14: [W i 4]
    15: [W i 4]
    16: [W i 4]
    17: [W i 4]
    18: [W i 4]
    19: [W i 4]
    20: [W i 4]
    21: [W i 4]
    22: [W i 4]
    |}]

let%expect_test "Variable dependency example" =
  let deps =
    var_dependence_ids
      (build_log_prob_dependence_info example1_program)
      (Set.Poly.singleton "j") 17 in
  print_s [%sexp (deps : label Set.Poly.t)];
  [%expect {|
      (4 5 9 11 13 14 16)
    |}]

let%expect_test "External definitions are distinct from statement label 1" =
  let write var : Accesses.access = {var; path= []; kind= Accesses.Write} in
  let definitions =
    Set.Poly.of_list
      [(write "x", None); (write "x", Some 1); (write "y", Some 2)] in
  print_s [%sexp (definition_ids definitions "x" : label option Set.Poly.t)];
  [%expect {| (() (1)) |}]

(** Prints the data labels, then the control labels, of a set of direct
    dependences, as [data 5 9  control 6]. A missing kind prints nothing. *)
let pp_dependences ppf (dependences : kind Set.Poly.t) =
  let labels_of kind_name labels =
    if Set.Poly.is_empty labels then None
    else Some (Fmt.str "%s %a" kind_name Test_utils.pp_labels labels) in
  let data_ids =
    Set.Poly.filter_map dependences ~f:(function
      | Data id -> Some id
      | Control _ -> None) in
  let control_ids =
    Set.Poly.filter_map dependences ~f:(function
      | Control id -> Some id
      | Data _ -> None) in
  Fmt.(list ~sep:(any "  ") string)
    ppf
    (List.filter_map ~f:Fun.id
       [labels_of "data" data_ids; labels_of "control" control_ids])

(* The data [N] reaches the condition as an outside definition, [()], but the
   dependencies of the condition hold statement labels only. *)
let%expect_test "External inputs reach a statement but are not dependencies" =
  let map =
    build_log_prob_dependence_info
      (Test_utils.mir_of_string
         {|
           data { int N; }
           parameters { real mu; }
           model { if (N > 0) target += mu; }
         |})
  in
  LabelMap.iter map.statements ~f:(fun ~key:statement_id ~data:statement ->
      match statement.pattern with
      | Stmt.Pattern.IfElse _ ->
          print_s
            [%sexp
              (definition_ids
                 (LabelMap.find statement_id map.reaching_definitions)
                 "N"
                : label option Set.Poly.t)];
          Fmt.pr "dependencies: %a@." pp_dependences
            (direct_dependence_ids map statement_id)
      | _ -> ());
  [%expect {|
    (())
    dependencies:
    |}]

(** Prints each statement and the immediate dependences of the statement. *)
let pp_immediate_dependencies ppf (map : dependence_info) =
  LabelMap.iter map.statements ~f:(fun ~key:statement_id ~data:statement ->
      Fmt.pf ppf "%d: %a  // %a@." statement_id Test_utils.pp_stmt
        statement.pattern pp_dependences
        (direct_dependence_ids map statement_id))

let print_immediate_dependencies prog =
  Fmt.pr "%a" pp_immediate_dependencies
    (build_log_prob_dependence_info (Test_utils.mir_of_string prog))

let%expect_test "Transitive dependencies of an if and of its condition" =
  let map =
    build_log_prob_dependence_info
      (Test_utils.mir_of_string
         {|
      parameters { real mu; real sigma; }
      model {
        real a = mu + 1;
        real b = a * 2;
        real c = sigma;
        if (b > 0)
          target += normal_lpdf(c | 0, 1);
      }
    |})
  in
  let if_id =
    LabelMap.fold map.statements ~init:0
      ~f:(fun ~key:statement_id ~data:statement found_id ->
        match statement.pattern with
        | Stmt.Pattern.IfElse _ -> statement_id
        | _ -> found_id) in
  Fmt.pr "%a" pp_immediate_dependencies map;
  Fmt.pr "transitive_dependence_ids %d: %a@." if_id Test_utils.pp_labels
    (transitive_dependence_ids map if_id);
  Fmt.pr "var_dependence_ids {b} %d: %a@." if_id Test_utils.pp_labels
    (var_dependence_ids map (Set.Poly.singleton "b") if_id);
  Fmt.pr "var_dependence_ids ~blockers:{b} {b} %d: %a@." if_id
    Test_utils.pp_labels
    (var_dependence_ids map ~blockers:(Set.Poly.singleton "b")
       (Set.Poly.singleton "b") if_id);
  Fmt.pr "var_dependence_ids ~blockers:{a} {b} %d: %a@." if_id
    Test_utils.pp_labels
    (var_dependence_ids map ~blockers:(Set.Poly.singleton "a")
       (Set.Poly.singleton "b") if_id);
  Fmt.pr "var_dependence_ids ~blockers:{mu} {b} %d: %a@." if_id
    Test_utils.pp_labels
    (var_dependence_ids map ~blockers:(Set.Poly.singleton "mu")
       (Set.Poly.singleton "b") if_id);
  (* The condition does not read c. Thus a query for c keeps each write of c
     that reaches the condition. *)
  Fmt.pr "var_dependence_ids {c} %d: %a@." if_id Test_utils.pp_labels
    (var_dependence_ids map (Set.Poly.singleton "c") if_id);
  [%expect
    {|
    1: 2 3 4  //
    2: real mu;  //
    3: real sigma;  //
    4: { 5 6 7 8 9 10 11 }  //
    5: real a;  //
    6: a = (mu + promote(1, real, data));  // data 2
    7: real b;  //
    8: b = (a * promote(2, real, data));  // data 6
    9: real c;  //
    10: c = sigma;  // data 3
    11: if((b > 0)) 12  // data 8
    12: target += normal_lpdf(c, promote(0, real, data), promote(1, real, data));  // data 10  control 11
    transitive_dependence_ids 11: 2 6 8 11
    var_dependence_ids {b} 11: 2 6 8
    var_dependence_ids ~blockers:{b} {b} 11:
    var_dependence_ids ~blockers:{a} {b} 11: 8
    var_dependence_ids ~blockers:{mu} {b} 11: 6 8
    var_dependence_ids {c} 11: 3 10
    |}]

let%expect_test
    "Reaching definitions: an _lp call in an assignment defines target" =
  let map =
    build_log_prob_dependence_info
      (Test_utils.mir_of_string
         {|
           functions { real foo_lp(real y) { target += y; return y; } }
           parameters { real mu; }
           model {
             real x;
             x = foo_lp(mu);
             if (target() > 0) target += 1;
           }
         |})
  in
  LabelMap.iter map.statements ~f:(fun ~key:statement_id ~data:statement ->
      match statement.pattern with
      | Stmt.Pattern.IfElse _ ->
          Fmt.pr "%d: %a@." statement_id pp_dependences
            (direct_dependence_ids map statement_id)
      | _ -> ());
  [%expect {| 6: data 5 |}]

(* The Graph tests use programs from test/unit/Dependence.ml. A write is not a
   dependency of a read if the write selects a different element, or if the
   write runs after the read. *)

let%expect_test "Graph: definitions that execute after the read never flow" =
  print_immediate_dependencies
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c; vector[N] d;
        for (n in 1:(N - 1)) {
          c[n] = b[n + 1];
          d[n] = b[n];
          b[n] = a;
        }
      }
    |};
  [%expect
    {|
    1: 2 3  //
    2: real a;  //
    3: { 4 5 6 7 8 9 10 }  //
    4: FnValidateSize__("b", "N", N);  //
    5: vector[N] b;  //
    6: FnValidateSize__("c", "N", N);  //
    7: vector[N] c;  //
    8: FnValidateSize__("d", "N", N);  //
    9: vector[N] d;  //
    10: for(n in 1:(N - 1)) 11  //
    11: { 12 13 14 }  // control 10
    12: c[n] = b[(n + 1)];  // data 5  control 10
    13: d[n] = b[n];  // data 5  control 10
    14: b[n] = a;  // data 2  control 10
    |}]

let%expect_test "Graph: a definition in a later outer iteration never flows" =
  print_immediate_dependencies
    {|
      data { int N; int M; }
      parameters { real a; }
      model {
        vector[N] b; matrix[N, M] c; matrix[N, M] d;
        for (n in 2:(N - 1)) {
          for (m in 1:M) {
            c[n, m] = b[n + 1];
            d[n, m] = b[n - 1];
            b[n] = a;
          }
        }
      }
    |};
  [%expect
    {|
    1: 2 3  //
    2: real a;  //
    3: { 4 5 6 7 8 9 10 11 12 }  //
    4: FnValidateSize__("b", "N", N);  //
    5: vector[N] b;  //
    6: FnValidateSize__("c", "N", N);  //
    7: FnValidateSize__("c", "M", M);  //
    8: matrix[N, M] c;  //
    9: FnValidateSize__("d", "N", N);  //
    10: FnValidateSize__("d", "M", M);  //
    11: matrix[N, M] d;  //
    12: for(n in 2:(N - 1)) 13  //
    13: { 14 }  // control 12
    14: for(m in 1:M) 15  // control 12
    15: { 16 17 18 }  // control 14
    16: c[n, m] = b[(n + 1)];  // data 5  control 14
    17: d[n, m] = b[(n - 1)];  // data 5 18  control 14
    18: b[n] = a;  // data 2  control 14
    |}]

let%expect_test "Graph: distinct literal subscripts are independent" =
  print_immediate_dependencies
    {|
      data { real y; real s; }
      parameters { real a; real b; }
      model {
        vector[2] theta;
        theta[1] = a;
        theta[2] = b;
        y ~ normal(theta[1], s);
        y ~ normal(theta[2], s);
      }
    |};
  [%expect
    {|
    1: 2 3 4  //
    2: real a;  //
    3: real b;  //
    4: { 5 6 7 8 9 }  //
    5: vector[2] theta;  //
    6: theta[1] = a;  // data 2
    7: theta[2] = b;  // data 3
    8: target += normal_lupdf(y, theta[1], s);  // data 5 6
    9: target += normal_lupdf(y, theta[2], s);  // data 5 7
    |}]

(* An increment of target does not use the value of an earlier increment. Thus
   the second increment does not depend on the first. *)
let%expect_test "Graph: increments of target do not depend on each other" =
  print_immediate_dependencies
    {|
      parameters { real a; real b; }
      model {
        target += a;
        target += b;
      }
    |};
  [%expect
    {|
    1: 2 3 4  //
    2: real a;  //
    3: real b;  //
    4: { 5 6 }  //
    5: target += a;  // data 2
    6: target += b;  // data 3
    |}]

let%expect_test "Variables read at a set of labels" =
  let map =
    build_log_prob_dependence_info
      (Test_utils.mir_of_string
         {|
      data { int N; int k; vector[N] x; }
      parameters { real mu; }
      model {
        vector[N] v;
        v[k] = x[k] + mu;
        for (n in 1:N) v[n] = x[n];
      }
    |})
  in
  LabelMap.iter map.statements ~f:(fun ~key:statement_id ~data:statement ->
      Fmt.pr "%d: %a  // %a@." statement_id Test_utils.pp_stmt statement.pattern
        Fmt.(list ~sep:(any " ") string)
        (Set.Poly.to_list (used_vars_at map (Set.Poly.singleton statement_id))));
  [%expect
    {|
    1: 2 3  //
    2: real mu;  //
    3: { 4 5 6 7 }  //
    4: FnValidateSize__("v", "N", N);  // N
    5: vector[N] v;  // N
    6: v[k] = (x[k] + mu);  // k mu x
    7: for(n in 1:N) 8  // N
    8: { 9 }  //
    9: v[n] = x[n];  // x
    |}]

(** Prints the controls of a statement as [loop 8 (n); if 13 then; jump 9]. *)
let pp_controls (info : dependence_info) ppf ({enclosing; jump_ids} : controls)
    =
  let pp_enclosing ppf = function
    | {id; side= Some Then} -> Fmt.pf ppf "if %d then" id
    | {id; side= Some Else} -> Fmt.pf ppf "if %d else" id
    | {id; side= None} ->
        Fmt.pf ppf "loop %d (%s)" id (Test_utils.loop_name info id) in
  Fmt.(list ~sep:(any "; ") string)
    ppf
    (List.map enclosing ~f:(Fmt.str "%a" pp_enclosing)
    @ List.map (Set.Poly.to_list jump_ids) ~f:(Fmt.str "jump %d"))

(** Prints rows of cells as a table. The first row is the header. Each column is
    padded to the widest cell of the column, except the last column, and two
    spaces separate the columns. *)
let pp_table ppf (rows : string list list) =
  let rec widen widths row =
    match (widths, row) with
    | width :: widths, cell :: cells ->
        max width (String.length cell) :: widen widths cells
    | [], cell :: cells -> String.length cell :: widen [] cells
    | widths, [] -> widths in
  let widths = List.fold_left rows ~init:[] ~f:widen in
  let last_column = List.length widths - 1 in
  (* An empty last cell would leave trailing spaces after the padding. *)
  let rec strip_trailing_spaces line =
    if String.ends_with ~suffix:" " line then
      strip_trailing_spaces
        (String.sub line ~pos:0 ~len:(String.length line - 1))
    else line in
  List.iter rows ~f:(fun row ->
      let cells =
        List.mapi row ~f:(fun column cell ->
            if column = last_column then cell
            else Printf.sprintf "%-*s" (List.nth widths column) cell) in
      Fmt.pf ppf "%s@." (strip_trailing_spaces (String.concat ~sep:"  " cells)))

(** The rows of a table, one per label of the statement map in ascending order,
    leaving out the labels for which [row] returns [None]. *)
let table_rows (info : dependence_info)
    ~(row : label -> stmt_info -> string list option) : string list list =
  List.rev
    (LabelMap.fold info.statements ~init:[] ~f:(fun ~key ~data rows ->
         match row key data with Some cells -> cells :: rows | None -> rows))

(* The running example of the Dependence_analysis interface. The tests below
   print the tables of the interface documentation, so the tables and the
   analysis cannot drift apart. The program starts at the first line of the
   string, so the Stan line of each statement is the line of the listing in the
   documentation. *)
let running_example =
  {|data {
  int N;
  vector[N] x;
}
parameters {
  real a;
}
model {
  matrix[N, 2] theta;
  real s = 0;
  for (n in 1:N) {
    theta[n, 1] = a;
    theta[n, 2] = x[n];
    s = s + theta[n, 2];
    if (s > 0) {
      target += 1;
    }
  }
}|}

let running_example_info () =
  build_log_prob_dependence_info (Test_utils.mir_of_string running_example)

(** Whether the statement at a label is the root or a block, which hold other
    statements and have no accesses of their own. *)
let is_block (statement : stmt_info) =
  match statement.pattern with
  | Stmt.Pattern.SList _ | Stmt.Pattern.Block _ -> true
  | _ -> false

(** Prints a pattern with [#] before each nested label, and the constructor name
    of a block: [Block { #10 #11 }], [SList { #2 #3 }], [if((s > 0)) #14]. *)
let pp_pattern ppf (pattern : (Expr.Typed.t, label) Stmt.Pattern.t) =
  let pp_label ppf label = Fmt.pf ppf "#%d" label in
  let pp_labels = Fmt.(list ~sep:(any " ") pp_label) in
  match pattern with
  | Stmt.Pattern.SList labels -> Fmt.pf ppf "SList { %a }" pp_labels labels
  | Stmt.Pattern.Block labels -> Fmt.pf ppf "Block { %a }" pp_labels labels
  | _ ->
      Test_utils.one_line (Stmt.Pattern.pp Expr.Typed.pp pp_label) ppf pattern

let%expect_test "Running example: the statement map" =
  let info = running_example_info () in
  let stan_lines (meta : Location_span.t) =
    let first = meta.begin_loc.line_num and last = meta.end_loc.line_num in
    if first < 0 then "-"
    else if first = last then Int.to_string first
    else Fmt.str "%d-%d" first last in
  let row statement_id (statement : stmt_info) =
    Some
      [ Int.to_string statement_id; stan_lines statement.meta
      ; Fmt.str "%a" pp_pattern statement.pattern
      ; Fmt.str "[%a]" (pp_controls info) statement.controls
      ; Fmt.str "%a"
          (Test_utils.one_line
             (Analysis_and_optimization.Accesses.pp
                ~loop_name:(Test_utils.loop_name info)))
          statement.accesses ] in
  Fmt.pr "%a" pp_table
    (["label"; "Stan lines"; "pattern"; "controls"; "accesses"]
    :: table_rows info ~row);
  [%expect
    {|
    label  Stan lines  pattern                             controls                  accesses
    1      -           SList { #2 #3 }                     []
    2      6           real a;                             []                        W a
    3      9           Block { #4 #5 #6 #7 #8 }            []
    4      9           FnValidateSize__("theta", "N", N);  []                        R N
    5      9           matrix[N, 2] theta;                 []                        R N, W theta
    6      10          real s;                             []                        W s
    7      10          s = promote(0, real, var);          []                        W s
    8      11-18       for(n in 1:N) #9                    []                        R N
    9      11-18       Block { #10 #11 #12 #13 }           [loop 8 (n)]
    10     12          theta[n, 1] = a;                    [loop 8 (n)]              R a, W theta[n, 1]
    11     13          theta[n, 2] = x[n];                 [loop 8 (n)]              R x[n], W theta[n, 2]
    12     14          s = (s + theta[n, 2]);              [loop 8 (n)]              R s, R theta[n, 2], W s
    13     15-17       if((s > 0)) #14                     [loop 8 (n)]              R s
    14     15-17       Block { #15 }                       [loop 8 (n); if 13 then]
    15     16          target += 1;                        [loop 8 (n); if 13 then]  += target
    |}]

let%expect_test "Running example: the reaching definitions" =
  let info = running_example_info () in
  let row statement_id (statement : stmt_info) =
    if is_block statement then None
    else
      Some
        [ Int.to_string statement_id; Fmt.str "%a" pp_pattern statement.pattern
        ; Fmt.str "%a" (pp_definitions info)
            (LabelMap.find statement_id info.reaching_definitions) ] in
  Fmt.pr "%a" pp_table
    (["label"; "pattern"; "reaching definitions"] :: table_rows info ~row);
  [%expect
    {|
    label  pattern                             reaching definitions
    2      real a;                             W N None, W a None, W x None
    4      FnValidateSize__("theta", "N", N);  W N None, W x None, W a 2
    5      matrix[N, 2] theta;                 W N None, W x None, W a 2
    6      real s;                             W N None, W x None, W a 2, W theta 5
    7      s = promote(0, real, var);          W N None, W x None, W a 2, W s 6, W theta 5
    8      for(n in 1:N) #9                    W N None, W x None, W a 2, W s [7, 12], += target 15, W theta 5, W theta[n, 1] 10, W theta[n, 2] 11
    10     theta[n, 1] = a;                    W N None, W x None, W a 2, W s [7, 12], += target 15, W theta 5, W theta[n, 1] 10, W theta[n, 2] 11
    11     theta[n, 2] = x[n];                 W N None, W x None, W a 2, W s [7, 12], += target 15, W theta 5, W theta[n, 1] 10, W theta[n, 2] 11
    12     s = (s + theta[n, 2]);              W N None, W x None, W a 2, W s [7, 12], += target 15, W theta 5, W theta[n, 1] 10, W theta[n, 2] 11
    13     if((s > 0)) #14                     W N None, W x None, W a 2, W s 12, += target 15, W theta 5, W theta[n, 1] 10, W theta[n, 2] 11
    15     target += 1;                        W N None, W x None, W a 2, W s 12, += target 15, W theta 5, W theta[n, 1] 10, W theta[n, 2] 11
    |}]

let%expect_test "Running example: the direct dependencies" =
  let info = running_example_info () in
  (* The Data labels and the Control labels of the set, as two columns. An empty
     column prints [-]. *)
  let labels_of_kind dependences ~f =
    let labels = Set.Poly.filter_map dependences ~f in
    if Set.Poly.is_empty labels then "-"
    else Fmt.str "%a" Test_utils.pp_labels labels in
  let row statement_id (statement : stmt_info) =
    let dependences = direct_dependence_ids info statement_id in
    if Set.Poly.is_empty dependences then None
    else
      Some
        [ Int.to_string statement_id; Fmt.str "%a" pp_pattern statement.pattern
        ; labels_of_kind dependences ~f:(function
            | Data id -> Some id
            | Control _ -> None)
        ; labels_of_kind dependences ~f:(function
            | Control id -> Some id
            | Data _ -> None) ] in
  Fmt.pr "%a" pp_table
    (["label"; "pattern"; "data dependencies"; "control dependencies"]
    :: table_rows info ~row);
  [%expect
    {|
    label  pattern                    data dependencies  control dependencies
    9      Block { #10 #11 #12 #13 }  -                  8
    10     theta[n, 1] = a;           2                  8
    11     theta[n, 2] = x[n];        -                  8
    12     s = (s + theta[n, 2]);     5 7 11 12          8
    13     if((s > 0)) #14            12                 8
    14     Block { #15 }              -                  13
    15     target += 1;               -                  13
    |}]

let%expect_test "Running example: the walk from target += 1" =
  let info = running_example_info () in
  let dependency_labels = transitive_dependence_ids info 15 in
  Fmt.pr "dependencies of 15: %a@.read: %a@." Test_utils.pp_labels
    dependency_labels
    Fmt.(list ~sep:(any " ") string)
    (Set.Poly.to_list (used_vars_at info dependency_labels));
  [%expect
    {|
    dependencies of 15: 5 7 8 11 12 13 15
    read: N s target theta x
    |}]

(* The Kills tests check that a killed write is not a dependency of a read after
   the later write. *)

let%expect_test "Kills: the same element in every iteration" =
  print_immediate_dependencies
    {|
      data { int N; int k; }
      parameters { real a; row_vector[N] r; }
      model {
        vector[N] theta; matrix[N, N] m; tuple(real, real) t; int j = k;
        theta[1] = a;
        theta[1] = 1;
        if (theta[1] > 0) target += 1;   // theta[1] = 1 removes theta[1] = a
        theta[k] = a;
        theta[k] = 1;
        if (theta[k] > 0) target += 1;   // k does not change, so theta[k] = a is removed
        theta[j] = a;
        j = 2;
        theta[j] = 1;
        if (theta[1] > 0) target += 1;   // j changes, so theta[j] = a remains
        t.1 = a;
        t.1 = 1;
        if (t.1 > 0) target += 1;        // the same field, so t.1 = a is removed
        m[1, 2] = a;
        m[1] = r;
        if (m[1, 2] > 0) target += 1;    // m[1] contains m[1, 2], so m[1, 2] = a is removed
        m[2] = r;
        m[2, 2] = 1;
        if (m[2, 3] > 0) target += 1;    // m[2, 2] is part of the row, so m[2] = r remains
      }
    |};
  [%expect
    {|
    1: 2 3 4  //
    2: real a;  //
    3: row_vector[N] r;  //
    4: { 5 6 7 8 9 10 11 12 13 14 15 17 18 19 21 22 23 24 26 27 28 30 31 32 34 35 36 }  //
    5: FnValidateSize__("theta", "N", N);  //
    6: vector[N] theta;  //
    7: FnValidateSize__("m", "N", N);  //
    8: FnValidateSize__("m", "N", N);  //
    9: matrix[N, N] m;  //
    10: tuple_ad(var, var)tuple(real, real) t;  //
    11: int j;  //
    12: j = k;  //
    13: theta[1] = a;  // data 2
    14: theta[1] = promote(1, real, var);  //
    15: if((theta[1] > 0)) 16  // data 6 14
    16: target += 1;  // control 15
    17: theta[k] = a;  // data 2
    18: theta[k] = promote(1, real, var);  //
    19: if((theta[k] > 0)) 20  // data 6 14 18
    20: target += 1;  // control 19
    21: theta[j] = a;  // data 2 12
    22: j = 2;  //
    23: theta[j] = promote(1, real, var);  // data 22
    24: if((theta[1] > 0)) 25  // data 6 14 18 21 23
    25: target += 1;  // control 24
    26: t.1 = a;  // data 2
    27: t.1 = promote(1, real, var);  //
    28: if((t.1 > 0)) 29  // data 10 27
    29: target += 1;  // control 28
    30: m[1, 2] = a;  // data 2
    31: m[1] = r;  // data 3
    32: if((m[1, 2] > 0)) 33  // data 9 31
    33: target += 1;  // control 32
    34: m[2] = r;  // data 3
    35: m[2, 2] = promote(1, real, var);  //
    36: if((m[2, 3] > 0)) 37  // data 9 31 34
    37: target += 1;  // control 36
    |}]

let%expect_test "Kills: control flow between the writes" =
  print_immediate_dependencies
    {|
      data { int N; int c; }
      parameters { real a; }
      model {
        vector[N] theta;
        theta[1] = a;
        if (c > 0) target += 1;
        theta[1] = 1;
        if (theta[1] > 0) target += 1;   // replaced after the if, so removed
        theta[2] = a;
        if (c > 0) theta[2] = 1; else theta[2] = 2;
        if (theta[2] > 0) target += 1;   // replaced in both branches, so removed
        theta[3] = a;
        if (c > 0) theta[3] = 1;
        if (theta[3] > 0) target += 1;   // replaced in one branch only, so theta[3] = a remains
        theta[1:2] = [a, a]';
        theta[1:2] = [1, 1]';
        if (theta[1] > 0) target += 1;   // a slice never removes a write, so theta[1:2] = [a, a]' remains
      }
    |};
  [%expect
    {|
    1: 2 3  //
    2: real a;  //
    3: { 4 5 6 7 9 10 12 13 16 18 19 21 23 24 25 }  //
    4: FnValidateSize__("theta", "N", N);  //
    5: vector[N] theta;  //
    6: theta[1] = a;  // data 2
    7: if((c > 0)) 8  //
    8: target += 1;  // control 7
    9: theta[1] = promote(1, real, var);  //
    10: if((theta[1] > 0)) 11  // data 5 9
    11: target += 1;  // control 10
    12: theta[2] = a;  // data 2
    13: if((c > 0)) 14 else 15  //
    14: theta[2] = promote(1, real, var);  // control 13
    15: theta[2] = promote(2, real, var);  // control 13
    16: if((theta[2] > 0)) 17  // data 5 14 15
    17: target += 1;  // control 16
    18: theta[3] = a;  // data 2
    19: if((c > 0)) 20  //
    20: theta[3] = promote(1, real, var);  // control 19
    21: if((theta[3] > 0)) 22  // data 5 18 20
    22: target += 1;  // control 21
    23: theta[1:2] = (FnMakeRowVec__(a, a) ');  // data 2
    24: theta[1:2] = (FnMakeRowVec__(promote(1, real, data), promote(1, real, data)) ');  //
    25: if((theta[1] > 0)) 26  // data 5 9 23 24
    26: target += 1;  // control 25
    |}]

let%expect_test "Kills: jumps and branches between the two writes" =
  print_immediate_dependencies
    {|
      data { int N; int c; }
      parameters { real a; }
      model {
        vector[N] theta;
        for (n in 2:N) {
          theta[n] = a;
          while (c > 0) break;
          theta[n] = 1;
          if (theta[n - 1] > 0) target += 1;  // the break leaves only the while, so removed
        }
        for (n in 2:N) {
          if (c > 0) theta[n] = a;
          theta[n] = 1;
          if (theta[n - 1] > 0) target += 1;  // replaced after the if, so removed
        }
        for (n in 2:N) {
          theta[n] = a;
          if (c > 0) theta[n] = 1; else theta[n] = 2;
          if (theta[n - 1] > 0) target += 1;  // a killer inside an if does not count, so theta[n] = a remains
        }
        for (n in 2:N) {
          theta[n] = a;
          if (c > 0) theta[n] = 1;
          if (theta[n - 1] > 0) target += 1;  // only one branch replaces theta[n], so theta[n] = a remains
        }
      }
    |};
  [%expect
    {|
    1: 2 3  //
    2: real a;  //
    3: { 4 5 6 14 21 29 }  //
    4: FnValidateSize__("theta", "N", N);  //
    5: vector[N] theta;  //
    6: for(n in 2:N) 7  //
    7: { 8 9 11 12 }  // control 6
    8: theta[n] = a;  // data 2  control 6
    9: while((c > 0)) 10  // control 6 10
    10: break;  // control 9
    11: theta[n] = promote(1, real, var);  // control 6
    12: if((theta[(n - 1)] > 0)) 13  // data 5 11  control 6
    13: target += 1;  // control 12
    14: for(n in 2:N) 15  //
    15: { 16 18 19 }  // control 14
    16: if((c > 0)) 17  // control 14
    17: theta[n] = a;  // data 2  control 16
    18: theta[n] = promote(1, real, var);  // control 14
    19: if((theta[(n - 1)] > 0)) 20  // data 5 11 18  control 14
    20: target += 1;  // control 19
    21: for(n in 2:N) 22  //
    22: { 23 24 27 }  // control 21
    23: theta[n] = a;  // data 2  control 21
    24: if((c > 0)) 25 else 26  // control 21
    25: theta[n] = promote(1, real, var);  // control 24
    26: theta[n] = promote(2, real, var);  // control 24
    27: if((theta[(n - 1)] > 0)) 28  // data 5 11 18 23 25 26  control 21
    28: target += 1;  // control 27
    29: for(n in 2:N) 30  //
    30: { 31 32 34 }  // control 29
    31: theta[n] = a;  // data 2  control 29
    32: if((c > 0)) 33  // control 29
    33: theta[n] = promote(1, real, var);  // control 32
    34: if((theta[(n - 1)] > 0)) 35  // data 5 11 18 23 25 26 31 33  control 29
    35: target += 1;  // control 34
    |}]

let%expect_test "Kills: loop-variable indices within one iteration" =
  print_immediate_dependencies
    {|
      data { int N; int c; }
      parameters { real a; }
      model {
        vector[N] theta; vector[N] y;
        for (n in 2:N) {
          theta[n] = a;
          theta[n] = 1;
          if (theta[n] > 0) target += 1;    // replaced in the same iteration, so removed
        }
        for (n in 2:N) {
          theta[n] = a;
          if (c > 0) y[n] = 1; else y[n] = 2;
          theta[n] = 1;
          if (theta[n] > 0) target += 1;    // replaced after the if, so removed
        }
        for (n in 2:N) {
          theta[n] = 1;
          theta[n] = a;
          y[n] = theta[n - 1];              // theta[n] = a of the previous iteration remains
        }
        for (n in 2:N) {
          theta[n] = a;
          if (c > 0) continue;
          theta[n] = 1;
          if (theta[n - 1] > 0) target += 1;  // continue can skip theta[n] = 1, so theta[n] = a remains
        }
        for (n in 2:N) {
          theta[n] = a;
          for (m in 1:N) theta[n] = 1;
          if (theta[n - 1] > 0) target += 1;  // the inner loop can run zero times, so theta[n] = a remains
        }
      }
    |};
  [%expect
    {|
    1: 2 3  //
    2: real a;  //
    3: { 4 5 6 7 8 14 23 28 36 }  //
    4: FnValidateSize__("theta", "N", N);  //
    5: vector[N] theta;  //
    6: FnValidateSize__("y", "N", N);  //
    7: vector[N] y;  //
    8: for(n in 2:N) 9  //
    9: { 10 11 12 }  // control 8
    10: theta[n] = a;  // data 2  control 8
    11: theta[n] = promote(1, real, var);  // control 8
    12: if((theta[n] > 0)) 13  // data 5 11  control 8
    13: target += 1;  // control 12
    14: for(n in 2:N) 15  //
    15: { 16 17 20 21 }  // control 14
    16: theta[n] = a;  // data 2  control 14
    17: if((c > 0)) 18 else 19  // control 14
    18: y[n] = promote(1, real, var);  // control 17
    19: y[n] = promote(2, real, var);  // control 17
    20: theta[n] = promote(1, real, var);  // control 14
    21: if((theta[n] > 0)) 22  // data 5 11 20  control 14
    22: target += 1;  // control 21
    23: for(n in 2:N) 24  //
    24: { 25 26 27 }  // control 23
    25: theta[n] = promote(1, real, var);  // control 23
    26: theta[n] = a;  // data 2  control 23
    27: y[n] = theta[(n - 1)];  // data 5 11 20 26  control 23
    28: for(n in 2:N) 29  //
    29: { 30 31 33 34 }  // control 28
    30: theta[n] = a;  // data 2  control 28
    31: if((c > 0)) 32  // control 28
    32: continue;  // control 31
    33: theta[n] = promote(1, real, var);  // control 28 32
    34: if((theta[(n - 1)] > 0)) 35  // data 5 11 20 26 30 33  control 28 32
    35: target += 1;  // control 32 34
    36: for(n in 2:N) 37  //
    37: { 38 39 42 }  // control 36
    38: theta[n] = a;  // data 2  control 36
    39: for(m in 1:N) 40  // control 36
    40: { 41 }  // control 39
    41: theta[n] = promote(1, real, var);  // control 39
    42: if((theta[(n - 1)] > 0)) 43  // data 5 11 20 26 30 33 38 41  control 36
    43: target += 1;  // control 42
    |}]

(* Each statement prints the controls, then the two views that queries use. The
   controls list the enclosing ifs and loops, outermost first, then the jumps.
   [control_parent_ids] keeps the nearest enclosing if or loop and the jumps.
   [loop_ids] keeps the loops, and a loop statement adds itself. *)
let%expect_test "Controls and the two views derived from them" =
  let map =
    build_log_prob_dependence_info
      (Test_utils.mir_of_string
         {|
      data { int N; int c; }
      parameters { real a; }
      model {
        vector[N] v;
        for (n in 1:N) {
          if (c > 0) continue;
          while (v[n] < 0) {
            if (c > 1) v[n] = a;
          }
        }
      }
    |})
  in
  let pp_loop = Fmt.int in
  LabelMap.iter map.statements ~f:(fun ~key:statement_id ~data:statement ->
      Fmt.pr "%d: %a@.   controls [%a]  dependencies %a  loops [%a]@."
        statement_id Test_utils.pp_stmt statement.pattern (pp_controls map)
        statement.controls Test_utils.pp_labels
        (control_parent_ids statement.controls)
        Fmt.(list ~sep:(any "; ") pp_loop)
        (loop_ids statement_id statement.pattern statement.controls));
  [%expect
    {|
    1: 2 3
       controls []  dependencies   loops []
    2: real a;
       controls []  dependencies   loops []
    3: { 4 5 6 }
       controls []  dependencies   loops []
    4: FnValidateSize__("v", "N", N);
       controls []  dependencies   loops []
    5: vector[N] v;
       controls []  dependencies   loops []
    6: for(n in 1:N) 7
       controls []  dependencies   loops [6]
    7: { 8 10 }
       controls [loop 6 (n)]  dependencies 6  loops [6]
    8: if((c > 0)) 9
       controls [loop 6 (n)]  dependencies 6  loops [6]
    9: continue;
       controls [loop 6 (n); if 8 then]  dependencies 8  loops [6]
    10: while((v[n] < 0)) 11
       controls [loop 6 (n); jump 9]  dependencies 6 9  loops [6; 10]
    11: { 12 }
       controls [loop 6 (n); loop 10 (while); jump 9]  dependencies 9 10  loops [6; 10]
    12: if((c > 1)) 13
       controls [loop 6 (n); loop 10 (while); jump 9]  dependencies 9 10  loops [6; 10]
    13: v[n] = a;
       controls [loop 6 (n); loop 10 (while); if 12 then; jump 9]  dependencies 9 12  loops [6; 10]
    |}]
