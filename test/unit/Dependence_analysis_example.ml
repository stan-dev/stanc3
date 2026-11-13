open Std
open Analysis_and_optimization.Dependence_analysis
open Analysis_and_optimization.Dataflow_utils
open Middle
open Analysis_and_optimization.Dataflow_types

(* These tests print the tables of the running example in the interface of
   {!Analysis_and_optimization.Dependence_analysis}, so the tables and the
   analysis cannot drift apart. When the analysis or the documentation changes,
   regenerate the tables in the interface from these tests. *)

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
let table_rows (statements : stmt_info LabelMap.t)
    ~(row : label -> stmt_info -> string list option) : string list list =
  List.rev
    (LabelMap.fold statements ~init:[] ~f:(fun ~key ~data rows ->
         match row key data with Some cells -> cells :: rows | None -> rows))

(* The program starts at the first line of the string, so the Stan line of each
   statement is the line of the listing in the documentation. *)
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

let running_example_statements () =
  build_log_prob_dependence_info (Test_utils.mir_of_string running_example)

(** Whether the statement at a label is the root or a block, which hold other
    statements and have no accesses of their own. *)
let is_block (statement : stmt_info) =
  match statement.pattern with
  | Stmt.Pattern.SList _ | Stmt.Pattern.Block _ -> true
  | _ -> false

let%expect_test "Running example: the statement map" =
  let statements = running_example_statements () in
  let loop_name = Test_utils.loop_name statements in
  let stan_lines (meta : Location_span.t) =
    let first = meta.begin_loc.line_num and last = meta.end_loc.line_num in
    if first < 0 then "-"
    else if first = last then Int.to_string first
    else Fmt.str "%d-%d" first last in
  let row statement_id (statement : stmt_info) =
    Some
      [ Int.to_string statement_id; stan_lines statement.meta
      ; Fmt.str "%a" pp_pattern statement.pattern
      ; Fmt.str "[%a]" (pp_controls ~loop_name) statement.controls
      ; Fmt.str "%a"
          (Test_utils.one_line
             (Analysis_and_optimization.Dependence.Accesses.pp ~loop_name))
          statement.accesses ] in
  Fmt.pr "%a" pp_table
    (["label"; "Stan lines"; "pattern"; "controls"; "accesses"]
    :: table_rows statements ~row);
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
    12     14          s = (s + theta[n, 2]);              [loop 8 (n)]              R theta[n, 2], RW s
    13     15-17       if((s > 0)) #14                     [loop 8 (n)]              R s
    14     15-17       Block { #15 }                       [loop 8 (n); if 13 then]
    15     16          target += 1;                        [loop 8 (n); if 13 then]  RW target
    |}]

let%expect_test "Running example: the reaching definitions" =
  let statements = running_example_statements () in
  let loop_name = Test_utils.loop_name statements in
  let row statement_id (statement : stmt_info) =
    if is_block statement then None
    else
      Some
        [ Int.to_string statement_id; Fmt.str "%a" pp_pattern statement.pattern
        ; Fmt.str "%a"
            (pp_definitions ~loop_name)
            statement.reaching_definitions ] in
  Fmt.pr "%a" pp_table
    (["label"; "pattern"; "reaching definitions"] :: table_rows statements ~row);
  [%expect
    {|
    label  pattern                             reaching definitions
    2      real a;                             None
    4      FnValidateSize__("theta", "N", N);  a 2
    5      matrix[N, 2] theta;                 a 2
    6      real s;                             a 2, theta 5
    7      s = promote(0, real, var);          a 2, s 6, theta 5
    8      for(n in 1:N) #9                    a 2, s [7, 12], target 15, theta 5, theta[n, 1] 10, theta[n, 2] 11
    10     theta[n, 1] = a;                    a 2, s [7, 12], target 15, theta 5, theta[n, 1] 10, theta[n, 2] 11
    11     theta[n, 2] = x[n];                 a 2, s [7, 12], target 15, theta 5, theta[n, 1] 10, theta[n, 2] 11
    12     s = (s + theta[n, 2]);              a 2, s [7, 12], target 15, theta 5, theta[n, 1] 10, theta[n, 2] 11
    13     if((s > 0)) #14                     a 2, s 12, target 15, theta 5, theta[n, 1] 10, theta[n, 2] 11
    15     target += 1;                        a 2, s 12, target 15, theta 5, theta[n, 1] 10, theta[n, 2] 11
    |}]

let%expect_test "Running example: the direct dependencies" =
  let statements = running_example_statements () in
  (* The writer labels and the control parents, as two columns. An empty column
     prints [-]. *)
  let column labels =
    if Set.Poly.is_empty labels then "-"
    else Fmt.str "%a" Test_utils.pp_labels labels in
  let row statement_id (statement : stmt_info) =
    let data_ids = Set.Poly.map statement.writers ~f:snd in
    let control_ids = control_parent_ids statement.controls in
    if Set.Poly.is_empty data_ids && Set.Poly.is_empty control_ids then None
    else
      Some
        [ Int.to_string statement_id; Fmt.str "%a" pp_pattern statement.pattern
        ; column data_ids; column control_ids ] in
  Fmt.pr "%a" pp_table
    (["label"; "pattern"; "data dependencies"; "control dependencies"]
    :: table_rows statements ~row);
  [%expect
    {|
    label  pattern                    data dependencies  control dependencies
    9      Block { #10 #11 #12 #13 }  -                  8
    10     theta[n, 1] = a;           2                  8
    11     theta[n, 2] = x[n];        -                  8
    12     s = (s + theta[n, 2]);     5 7 11 12          8
    13     if((s > 0)) #14            12                 8
    14     Block { #15 }              -                  13
    15     target += 1;               15                 13
    |}]

let%expect_test "Running example: the walk from target += 1" =
  let statements = running_example_statements () in
  let dependency_labels = transitive_dependence_ids statements 15 in
  Fmt.pr "dependencies of 15: %a@.read: %a@." Test_utils.pp_labels
    dependency_labels
    Fmt.(list ~sep:(any " ") string)
    (Set.Poly.to_list (used_vars_at statements dependency_labels));
  [%expect
    {|
    dependencies of 15: 5 7 8 11 12 13 15
    read: N s target theta x
    |}]
