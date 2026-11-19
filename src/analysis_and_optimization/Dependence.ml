open Std
open Middle

type access_step = Subscript of Expr.Typed.t Index.t | TupleField of int
type access_path = access_step list
type location = {variable: string; path: access_path}
type accesses = {reads: location list; writes: location list}

let pp_access_path pp_expr ppf path =
  let pp_indices reversed = Index.pp_indices pp_expr ppf (List.rev reversed) in
  let rec pp_steps indices = function
    | Subscript index :: rest -> pp_steps (index :: indices) rest
    | TupleField field :: rest ->
        pp_indices indices;
        Fmt.pf ppf ".%d" field;
        pp_steps [] rest
    | [] -> pp_indices indices in
  pp_steps [] path

module Location = struct
  let same_var (first : location) (second : location) =
    String.equal first.variable second.variable
end

let empty = {reads= []; writes= []}

let add_read (read_location : location) (collected_accesses : accesses) =
  {collected_accesses with reads= read_location :: collected_accesses.reads}

let add_write (write_location : location) (collected_accesses : accesses) =
  {collected_accesses with writes= write_location :: collected_accesses.writes}

let add_read_write (updated_location : location) (collected_accesses : accesses)
    =
  collected_accesses |> add_read updated_location |> add_write updated_location

let prepend_subscripts indices path =
  List.fold_right indices ~init:path ~f:(fun index rest ->
      Subscript index :: rest)

let rec location_of_lvalue ((base, indices) : Expr.Typed.t Stmt.Pattern.lvalue)
    (suffix : access_path) =
  let path = prepend_subscripts indices suffix in
  match base with
  | LVariable variable -> {variable; path}
  | LTupleProjection (inner, field) ->
      location_of_lvalue inner (TupleField field :: path)

let target = {variable= "target"; path= []}

let rec collect_expr_accesses (collected_accesses : accesses)
    (expr : Expr.Typed.t) =
  match expr.pattern with
  | Var variable -> add_read {variable; path= []} collected_accesses
  | Indexed _ | TupleProjection _ -> (
      let rec location_of_expr_opt (expr : Expr.Typed.t) (suffix : access_path)
          =
        match expr.pattern with
        | Var variable -> Some {variable; path= suffix}
        | Indexed (base, indices) ->
            if
              (not (List.is_empty suffix))
              && not (List.for_all indices ~f:Index.is_single)
            then
              None (* Indexing a slice cannot be flattened into a base path. *)
            else location_of_expr_opt base (prepend_subscripts indices suffix)
        | TupleProjection (base, field) ->
            location_of_expr_opt base (TupleField field :: suffix)
        | FunApp _ | TernaryIf _ | Lit _ | EAnd _ | EOr _ | Promotion _ -> None
      in
      match location_of_expr_opt expr [] with
      | Some read_location ->
          List.fold_left read_location.path
            ~init:(add_read read_location collected_accesses)
            ~f:(fun accesses -> function
            | Subscript index -> Index.fold collect_expr_accesses accesses index
            | TupleField _ -> accesses)
      | None ->
          Expr.Pattern.fold collect_expr_accesses collected_accesses
            expr.pattern)
  | FunApp (StanLib (_, FnTarget), []) -> add_read target collected_accesses
  | FunApp (kind, _) when Fun_kind.may_update_target kind ->
      Expr.Pattern.fold collect_expr_accesses collected_accesses expr.pattern
      |> add_read_write target
  | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ ->
      Expr.Pattern.fold collect_expr_accesses collected_accesses expr.pattern

let collect_accesses (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) =
  let expression_accesses =
    Stmt.Pattern.fold collect_expr_accesses Fun.const empty stmt in
  match stmt with
  | Assignment (lhs, _, _) ->
      add_write (location_of_lvalue lhs []) expression_accesses
  | Decl {decl_id; _} ->
      add_write {variable= decl_id; path= []} expression_accesses
  | For {loopvar; _} ->
      add_write {variable= loopvar; path= []} expression_accesses
  | TargetPE _ | JacobianPE _ -> add_read_write target expression_accesses
  | NRFunApp (kind, _) when Fun_kind.may_update_target kind ->
      add_read_write target expression_accesses
  | _ -> expression_accesses

(* v[2 * i] and v[2 * j + 1] cannot select the same element. The first index is
   always even (Some 0) and the second always odd (Some 1). Working modulo 2
   lets us check this without overflowing on intermediate sums or products. None
   means we couldn't determine whether the index is even or odd. *)
let rec integer_parity (expr : Expr.Typed.t) =
  match expr.pattern with
  | Lit (Int, _) ->
      Option.map (Expr.Helpers.int_literal_value expr) ~f:(fun value ->
          abs (value mod 2))
  | FunApp (Operator (PPlus | PMinus), [operand]) -> integer_parity operand
  | FunApp (Operator (Plus | Minus), [left; right]) -> (
      match (integer_parity left, integer_parity right) with
      | Some left_parity, Some right_parity ->
          Some ((left_parity + right_parity) mod 2)
      | None, _ | _, None -> None)
  | FunApp (Operator Times, [left; right]) -> (
      match (integer_parity left, integer_parity right) with
      | Some 0, _ | _, Some 0 -> Some 0
      | Some left_parity, Some right_parity -> Some (left_parity * right_parity)
      | None, _ | _, None -> None)
  | Var _ | Indexed _ | TupleProjection _ | FunApp _ | TernaryIf _ | Lit _
   |EAnd _ | EOr _ | Promotion _ ->
      None

let may_equal_int first_expr second_expr =
  if Expr.Helpers.compare_ignore_meta first_expr second_expr = 0 then true
  else
    match
      ( Expr.Helpers.int_literal_value first_expr
      , Expr.Helpers.int_literal_value second_expr )
    with
    | Some first_value, Some second_value -> first_value = second_value
    | Some _, None | None, Some _ | None, None -> (
        match (integer_parity first_expr, integer_parity second_expr) with
        | Some first_parity, Some second_parity -> first_parity = second_parity
        | Some _, None | None, Some _ | None, None -> true)

let may_be_less_or_equal first_expr second_expr =
  match
    ( Expr.Helpers.int_literal_value first_expr
    , Expr.Helpers.int_literal_value second_expr )
  with
  | Some first_value, Some second_value -> first_value <= second_value
  | Some _, None | None, Some _ | None, None -> true

let is_definitely_empty_index = function
  | Index.Between (lower, upper) -> (
      match
        ( Expr.Helpers.int_literal_value lower
        , Expr.Helpers.int_literal_value upper )
      with
      | Some lower_value, Some upper_value -> lower_value > upper_value
      | _ -> false)
  | MultiIndex array_expr -> (
      match Expr.Helpers.array_literal_elements array_expr with
      | Some [] -> true
      | _ -> false)
  | All | Single _ | Upfrom _ -> false

let rec may_indices_overlap (first_index : Expr.Typed.t Index.t)
    (second_index : Expr.Typed.t Index.t) : bool =
  if
    is_definitely_empty_index first_index
    || is_definitely_empty_index second_index
  then false
  else
    match first_index with
    | All -> true
    | Single first_value -> (
        match second_index with
        | All -> true
        | Single second_value -> may_equal_int first_value second_value
        | Upfrom lower -> may_be_less_or_equal lower first_value
        | Between (lower, upper) ->
            may_be_less_or_equal lower first_value
            && may_be_less_or_equal first_value upper
        | MultiIndex second_array ->
            array_may_overlap_index second_array first_index)
    | Upfrom first_lower -> (
        match second_index with
        | All -> true
        | Single second_value -> may_be_less_or_equal first_lower second_value
        | Upfrom _ ->
            (* If v[i:] and v[j:] are both nonempty, both include the last
               element of v. *)
            true
        | Between (_, second_upper) ->
            may_be_less_or_equal first_lower second_upper
        | MultiIndex second_array ->
            array_may_overlap_index second_array first_index)
    | Between (first_lower, first_upper) -> (
        match second_index with
        | All -> true
        | Single second_value ->
            may_be_less_or_equal first_lower second_value
            && may_be_less_or_equal second_value first_upper
        | Upfrom second_lower -> may_be_less_or_equal second_lower first_upper
        | Between (second_lower, second_upper) ->
            may_be_less_or_equal second_lower first_upper
            && may_be_less_or_equal first_lower second_upper
        | MultiIndex second_array ->
            array_may_overlap_index second_array first_index)
    | MultiIndex first_array -> (
        match second_index with
        | All -> true
        | Single _ -> array_may_overlap_index first_array second_index
        | Upfrom _ -> array_may_overlap_index first_array second_index
        | Between (_, _) -> array_may_overlap_index first_array second_index
        | MultiIndex _ -> array_may_overlap_index first_array second_index)

and array_may_overlap_index (array_expr : Expr.Typed.t)
    (other_index : Expr.Typed.t Index.t) =
  match Expr.Helpers.array_literal_elements array_expr with
  | None ->
      (* For v[idx] we don't know which elements idx selects until runtime. *)
      true
  | Some elements ->
      List.exists elements ~f:(fun element ->
          may_indices_overlap (Index.Single element) other_index)

let rec may_overlap (first_path : access_path) (second_path : access_path) :
    bool =
  match (first_path, second_path) with
  | Subscript first_index :: first_tail, Subscript second_index :: second_tail
    ->
      may_indices_overlap first_index second_index
      && may_overlap first_tail second_tail
  | TupleField first :: first_tail, TupleField second :: second_tail ->
      first = second && may_overlap first_tail second_tail
  | _ -> true

let rec is_path_overwritten (written_vars : string Set.Poly.t)
    (previous_path : access_path) (write_path : access_path) : bool =
  match (previous_path, write_path) with
  | _, [] -> true
  | TupleField previous :: previous_tail, TupleField write :: write_tail ->
      previous = write
      && is_path_overwritten written_vars previous_tail write_tail
  | ( Subscript (Index.Single previous_index) :: previous_tail
    , Subscript (Index.Single write_index) :: write_tail ) ->
      Expr.Helpers.compare_ignore_meta write_index previous_index = 0
      && (not (Mir_utils.cannot_duplicate_expr write_index))
      && (not (Mir_utils.expr_reads_any_of written_vars write_index))
      && is_path_overwritten written_vars previous_tail write_tail
  | _ -> false

let is_overwritten (written_vars : string Set.Poly.t) (previous : location)
    (write : location) : bool =
  Location.same_var previous write
  && is_path_overwritten written_vars previous.path write.path

let%test_module "Dependence helpers" =
  (module struct
    open Expr.Helpers

    let pp_location ppf {variable; path} =
      Fmt.pf ppf "%s%a" variable (pp_access_path Expr.Typed.pp) path

    let print_accesses {reads; writes} =
      let locations = Fmt.list ~sep:(Fmt.any ", ") pp_location in
      Fmt.pr "reads [%a], writes [%a]@." locations reads locations writes

    let single i = Subscript (Index.Single i)
    let whole variable = {variable; path= []}

    let%expect_test
        "pp_access_path groups consecutive subscripts before a tuple field" =
      let path =
        [single (int 1); single (int 2); TupleField 3; single (int 4)] in
      Fmt.pr "t%a@." (pp_access_path Expr.Typed.pp) path;
      [%expect {| t[1, 2].3[4] |}]

    let%expect_test
        "Location.same_var compares the base variable of two locations" =
      let first = {variable= "x"; path= [single (int 1)]} in
      List.iter ["x"; "y"] ~f:(fun variable ->
          let second = {variable; path= [single (int 2)]} in
          Fmt.pr "%a and %a: %b@." pp_location first pp_location second
            (Location.same_var first second));
      [%expect {|
        x[1] and x[2]: true
        x[1] and y[2]: false |}]

    let%expect_test
        "add_read records an indexed read alongside an existing write" =
      let accesses = {reads= []; writes= [whole "y"]} in
      print_accesses (add_read {variable= "x"; path= [single (int 2)]} accesses);
      [%expect {| reads [x[2]], writes [y] |}]

    let%expect_test
        "add_write records an indexed write alongside an existing read" =
      let accesses = {reads= [whole "y"]; writes= []} in
      print_accesses
        (add_write {variable= "x"; path= [single (int 2)]} accesses);
      [%expect {| reads [y], writes [x[2]] |}]

    let%expect_test "add_read_write records target on both sides of an update" =
      print_accesses (add_read_write target empty);
      [%expect {| reads [target], writes [target] |}]

    let%expect_test
        "prepend_subscripts places both indices before a tuple field" =
      let path =
        prepend_subscripts [Index.Single (int 2); Single (int 3)] [TupleField 1]
      in
      Fmt.pr "t%a@." (pp_access_path Expr.Typed.pp) path;
      [%expect {| t[2, 3].1 |}]

    let iter_model_statements source ~f =
      let open Frontend in
      let ast = Parse.parse_program (`Code source) |> fst |> Result.get_ok in
      let typed = Typechecker.check_program ast |> Result.get_ok |> fst in
      let mir = Ast_to_Mir.trans_prog "" typed in
      let lines = String.split_lines source |> Array.of_list in
      Mir_utils.fold_stmts mir.log_prob ~init:() ~take_expr:Fun.const
        ~take_stmt:(fun () stmt ->
          let line = stmt.Stmt.meta.begin_loc.line_num in
          if line > 0 then
            f stmt.pattern
              ~source_line:
                (Fmt.str "line %d: %s" line (String.trim lines.(line - 1))))

    let print_for_each_argument source ~f =
      iter_model_statements source ~f:(fun stmt ~source_line ->
          match stmt with
          | Stmt.Pattern.NRFunApp (CompilerInternal FnPrint, [arg]) ->
              Fmt.pr "%s -> " source_line;
              f arg
          | _ -> ())

    let%expect_test
        "location_of_lvalue keeps selections on both sides of a tuple field" =
      iter_model_statements
        {|model {
            array[3] tuple(vector[4], real) t;
            t[2].1[3] = 1;
          }|}
        ~f:(fun stmt ~source_line ->
          match stmt with
          | Stmt.Pattern.Assignment (lhs, _, _) ->
              Fmt.pr "%s -> " source_line;
              Fmt.pr "write to %a@." pp_location (location_of_lvalue lhs [])
          | _ -> ());
      [%expect {| line 3: t[2].1[3] = 1; -> write to t[2].1[3] |}]

    let%expect_test
        "collect_expr_accesses reads an array element and its subscript" =
      print_for_each_argument
        {|data { array[3] int a; int i; }
          model {
            print(a[i]);
          }|}
        ~f:(fun expr -> print_accesses (collect_expr_accesses empty expr));
      [%expect {| line 3: print(a[i]); -> reads [i, a[i]], writes [] |}]

    let%expect_test
        "integer_parity recognizes even, odd and undetermined index expressions"
        =
      print_for_each_argument
        {|data { int k; }
          model {
            print(2 * k);
            print(2 * k + 1);
            print(k);
          }|}
        ~f:(fun expr ->
          let parity =
            match integer_parity expr with
            | Some 0 -> "even"
            | Some 1 -> "odd"
            | None -> "unknown"
            | Some n -> Fmt.failwith "Invalid parity %d" n in
          Fmt.pr "%s@." parity);
      [%expect
        {|
        line 3: print(2 * k); -> even
        line 4: print(2 * k + 1); -> odd
        line 5: print(k); -> unknown
        |}]

    let%expect_test
        "may_equal_int distinguishes different literals and opposite parity" =
      let even = binop (int 2) Times (variable "k") in
      List.iter
        [(int 4, int 4); (int 2, int 4); (even, int 3); (even, int 4)]
        ~f:(fun (a, b) ->
          Fmt.pr "%a may equal %a: %b@." Expr.Typed.pp a Expr.Typed.pp b
            (may_equal_int a b));
      [%expect
        {|
        4 may equal 4: true
        2 may equal 4: false
        (2 * k) may equal 3: false
        (2 * k) may equal 4: true |}]

    let%expect_test
        "may_be_less_or_equal handles equal, reversed and unknown bounds" =
      List.iter
        [(int 2, int 2); (int 3, int 2); (variable "i", int 2)]
        ~f:(fun (a, b) ->
          Fmt.pr "%a <= %a possible: %b@." Expr.Typed.pp a Expr.Typed.pp b
            (may_be_less_or_equal a b));
      [%expect
        {|
        2 <= 2 possible: true
        3 <= 2 possible: false
        i <= 2 possible: true |}]

    let%expect_test
        "is_definitely_empty_index recognizes empty ranges and index arrays" =
      let check index =
        Fmt.pr "%a -> %b@." (Index.pp Expr.Typed.pp) index
          (is_definitely_empty_index index) in
      List.iter
        [ Index.Between (int 3, int 2); Between (int 2, int 2)
        ; Between (variable "i", variable "j") ]
        ~f:check;
      Fmt.pr "empty index array -> %b@."
        (is_definitely_empty_index (MultiIndex (array_expr [])));
      [%expect
        {|
        3:2 -> true
        2:2 -> false
        i:j -> false
        empty index array -> true |}]

    let%expect_test "may_indices_overlap includes a shared endpoint" =
      let range = Index.Between (int 1, int 2) in
      List.iter [2; 3] ~f:(fun i ->
          Fmt.pr "1:2 overlaps %d: %b@." i
            (may_indices_overlap range (Single (int i))));
      [%expect {|
        1:2 overlaps 2: true
        1:2 overlaps 3: false |}]

    let%expect_test
        "array_may_overlap_index checks each element of an index array" =
      let indices = array_expr [int 2; int 4] in
      List.iter [3; 4] ~f:(fun i ->
          Fmt.pr "{2, 4} overlaps %d: %b@." i
            (array_may_overlap_index indices (Single (int i))));
      [%expect
        {|
        {2, 4} overlaps 3: false
        {2, 4} overlaps 4: true |}]

    let%expect_test
        "is_path_overwritten distinguishes a tuple field from one of its \
         elements" =
      let field = [TupleField 1] in
      let element = [TupleField 1; single (int 2)] in
      List.iter
        [(field, element); (element, field)]
        ~f:(fun (previous, write) ->
          let path = pp_access_path Expr.Typed.pp in
          Fmt.pr "previous t%a, write t%a: overwritten %b@." path previous path
            write
            (is_path_overwritten Set.Poly.empty previous write));
      [%expect
        {|
        previous t.1, write t.1[2]: overwritten false
        previous t.1[2], write t.1: overwritten true |}]
  end)
