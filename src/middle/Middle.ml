include Mir
include Mir_pretty_printer
include Stan_math_signatures
include Type_conversion
open Core_kernel
module Validation = Validation

(* == Pretty printers ======================================================= *)

let pp_builtin_syntax = Fmt.(string |> styled `Yellow)
let pp_keyword = Fmt.(string |> styled `Blue)
let angle_brackets pp_v ppf v = Fmt.pf ppf "@[<1><%a>@]" pp_v v

let pp_autodifftype ppf = function
  | DataOnly -> pp_keyword ppf "data "
  | AutoDiffable -> ()

let rec pp_unsizedtype ppf = function
  | UInt -> pp_keyword ppf "int"
  | UReal -> pp_keyword ppf "real"
  | UVector -> pp_keyword ppf "vector"
  | URowVector -> pp_keyword ppf "row_vector"
  | UMatrix -> pp_keyword ppf "matrix"
  | UArray ut -> (Fmt.brackets pp_unsizedtype) ppf ut
  | UFun (argtypes, rt) ->
      Fmt.pf ppf {|%a => %a|}
        Fmt.(list (pair ~sep:comma pp_autodifftype pp_unsizedtype) ~sep:comma)
        argtypes pp_returntype rt
  | UMathLibraryFunction ->
      (angle_brackets Fmt.string) ppf "Stan Math function"

and pp_returntype ppf = function
  | Void -> Fmt.string ppf "void"
  | ReturnType ut -> pp_unsizedtype ppf ut

let rec pp_sizedtype pp_e ppf st =
  match st with
  | SInt -> Fmt.string ppf "int"
  | SReal -> Fmt.string ppf "real"
  | SVector expr -> Fmt.pf ppf {|vector%a|} (Fmt.brackets pp_e) expr
  | SRowVector expr -> Fmt.pf ppf {|row_vector%a|} (Fmt.brackets pp_e) expr
  | SMatrix (d1_expr, d2_expr) ->
      Fmt.pf ppf {|matrix%a|}
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets)
        (d1_expr, d2_expr)
  | SArray (st, expr) ->
      Fmt.pf ppf {|array%a|}
        Fmt.(
          pair ~sep:comma (fun ppf st -> pp_sizedtype pp_e ppf st) pp_e
          |> brackets)
        (st, expr)

let rec pp_expr pp_e ppf = function
  | Var varname -> Fmt.string ppf varname
  | Lit (Str, str) -> Fmt.pf ppf "%S" str
  | Lit (_, str) -> Fmt.string ppf str
  | FunApp (_, name, args) ->
      Fmt.string ppf name ;
      Fmt.(list pp_e ~sep:Fmt.comma |> parens) ppf args
  | TernaryIf (pred, texpr, fexpr) ->
      Fmt.pf ppf {|@[%a@ %a@,%a@,%a@ %a@]|} pp_e pred pp_builtin_syntax "?"
        pp_e texpr pp_builtin_syntax ":" pp_e fexpr
  | Indexed (expr, indices) ->
      pp_indexed pp_e ppf (Fmt.strf "%a" pp_e expr, indices)
  | EAnd (l, r) -> Fmt.pf ppf "%a && %a" pp_e l pp_e r
  | EOr (l, r) -> Fmt.pf ppf "%a || %a" pp_e l pp_e r

and pp_indexed pp_e ppf (ident, indices) =
  Fmt.pf ppf {|@[%s%a@]|} ident
    ( if List.is_empty indices then fun _ _ -> ()
    else Fmt.(list (pp_index pp_e) ~sep:comma |> brackets) )
    indices

and pp_index pp_e ppf = function
  | All -> Fmt.char ppf ':'
  | Single index -> pp_e ppf index
  | Upfrom index -> Fmt.pf ppf {|%a:|} pp_e index
  | Downfrom index -> Fmt.pf ppf {|:%a|} pp_e index
  | Between (lower, upper) -> Fmt.pf ppf {|%a:%a|} pp_e lower pp_e upper
  | MultiIndex index -> Fmt.pf ppf {|%a|} pp_e index

let pp_fun_arg_decl ppf (autodifftype, name, unsizedtype) =
  Fmt.pf ppf "%a%a %s" pp_autodifftype autodifftype pp_unsizedtype unsizedtype
    name

let pp_fun_def pp_s ppf = function
  | {fdrt; fdname; fdargs; fdbody; _} -> (
    match fdrt with
    | Some rt ->
        Fmt.pf ppf {|@[<v2>%a %s%a {@ %a@]@ }|} pp_unsizedtype rt fdname
          Fmt.(list pp_fun_arg_decl ~sep:comma |> parens)
          fdargs pp_s fdbody
    | None ->
        Fmt.pf ppf {|@[<v2>%s %s%a {@ %a@]@ }|} "void" fdname
          Fmt.(list pp_fun_arg_decl ~sep:comma |> parens)
          fdargs pp_s fdbody )

let pp_statement pp_e pp_s ppf = function
  | Assignment ((assignee, idcs), rhs) ->
      Fmt.pf ppf {|@[<h>%a :=@ %a;@]|} (pp_indexed pp_e) (assignee, idcs) pp_e
        rhs
  | TargetPE expr ->
      Fmt.pf ppf {|@[<h>%a +=@ %a;@]|} pp_keyword "target" pp_e expr
  | NRFunApp (_, name, args) ->
      Fmt.pf ppf {|@[%s%a;@]|} name Fmt.(list pp_e ~sep:comma |> parens) args
  | Break -> pp_keyword ppf "break;"
  | Continue -> pp_keyword ppf "continue;"
  | Skip -> pp_keyword ppf "skip;"
  | Return (Some expr) -> Fmt.pf ppf {|%a %a;|} pp_keyword "return" pp_e expr
  | Return _ -> pp_keyword ppf "return;"
  | IfElse (pred, s_true, Some s_false) ->
      Fmt.pf ppf {|%a(%a) %a %a %a|} pp_builtin_syntax "if" pp_e pred pp_s
        s_true pp_builtin_syntax "else" pp_s s_false
  | IfElse (pred, s_true, _) ->
      Fmt.pf ppf {|%a(%a) %a|} pp_builtin_syntax "if" pp_e pred pp_s s_true
  | While (pred, stmt) ->
      Fmt.pf ppf {|%a(%a) %a|} pp_builtin_syntax "while" pp_e pred pp_s stmt
  | For {loopvar; lower; upper; body} ->
      Fmt.pf ppf {|%a(%s in %a:%a) %a|} pp_builtin_syntax "for" loopvar pp_e
        lower pp_e upper pp_s body
  | Block stmts ->
      Fmt.pf ppf {|{@;<1 2>@[<v>%a@]@;}|} Fmt.(list pp_s ~sep:Fmt.cut) stmts
  | SList stmts -> Fmt.(list pp_s ~sep:Fmt.cut |> vbox) ppf stmts
  | Decl {decl_adtype; decl_id; decl_type} ->
      Fmt.pf ppf {|%a%a %s;|} pp_autodifftype decl_adtype (pp_sizedtype pp_e)
        decl_type decl_id

let pp_io_block ppf = function
  | Data -> Fmt.string ppf "data"
  | Parameters -> Fmt.string ppf "parameters"
  | TransformedParameters -> Fmt.string ppf "transformed_parameters"
  | GeneratedQuantities -> Fmt.string ppf "generated_quantities"

let pp_io_var pp_e ppf (name, (sized_ty, io_block)) =
  Fmt.pf ppf "@[<h>%a %a %s;@]" pp_io_block io_block (pp_sizedtype pp_e)
    sized_ty name

let pp_block label pp_elem ppf elems =
  Fmt.pf ppf {|@[<v2>%a {@ %a@]@ }|} pp_keyword label
    Fmt.(list ~sep:cut pp_elem)
    elems ;
  Format.pp_force_newline ppf ()

let pp_io_var_block label pp_e = pp_block label (pp_io_var pp_e)

let pp_input_vars pp_e ppf {input_vars; _} =
  pp_io_var_block "input_vars" pp_e ppf input_vars

let pp_output_vars pp_e ppf {output_vars; _} =
  pp_io_var_block "output_vars" pp_e ppf output_vars

let pp_functions_block pp_s ppf {functions_block; _} =
  pp_block "functions" pp_s ppf functions_block

let pp_prepare_data pp_s ppf {prepare_data; _} =
  pp_block "prepare_data" pp_s ppf prepare_data

let pp_log_prob pp_s ppf {log_prob; _} = pp_block "log_prob" pp_s ppf log_prob

let pp_generate_quantities pp_s ppf {generate_quantities; _} =
  pp_block "generate_quantities" pp_s ppf generate_quantities

let pp_transform_inits pp_s ppf {transform_inits; _} =
  pp_block "transform_inits" pp_s ppf transform_inits

let pp_prog pp_e pp_s ppf prog =
  Format.open_vbox 0 ;
  pp_functions_block (pp_fun_def pp_s) ppf prog ;
  Fmt.cut ppf () ;
  pp_input_vars pp_e ppf prog ;
  Fmt.cut ppf () ;
  pp_prepare_data pp_s ppf prog ;
  Fmt.cut ppf () ;
  pp_log_prob pp_s ppf prog ;
  Fmt.cut ppf () ;
  pp_generate_quantities pp_s ppf prog ;
  Fmt.cut ppf () ;
  pp_transform_inits pp_s ppf prog ;
  Fmt.cut ppf () ;
  pp_output_vars pp_e ppf prog ;
  Format.close_box ()

let rec pp_expr_typed_located ppf {expr; _} =
  pp_expr pp_expr_typed_located ppf expr

let rec pp_stmt_loc ppf {stmt; _} =
  pp_statement pp_expr_typed_located pp_stmt_loc ppf stmt

let rec sexp_of_expr_typed_located {expr; _} =
  sexp_of_expr sexp_of_expr_typed_located expr

let rec sexp_of_stmt_loc {stmt; _} =
  sexp_of_statement sexp_of_expr_typed_located sexp_of_stmt_loc stmt

let pp_typed_prog ppf prog = pp_prog pp_expr_typed_located pp_stmt_loc ppf prog

(* ===================== Some helper functions and values ====================== *)
let no_loc = {filename= ""; line_num= 0; col_num= 0; included_from= None}
let no_span = {begin_loc= no_loc; end_loc= no_loc}
let mk_string_of sexp_of x = Sexp.to_string (sexp_of x) ^ "__"
let string_of_internal_fn = mk_string_of sexp_of_internal_fn

let mk_of_string of_sexp x =
  try
    String.chop_suffix_exn ~suffix:"__" x |> Sexp.of_string |> of_sexp |> Some
  with
  | Sexplib.Conv.Of_sexp_error _ -> None
  | Invalid_argument _ -> None

let internal_fn_of_string = mk_of_string internal_fn_of_sexp

let internal_funapp ifn args emeta =
  {expr= FunApp (CompilerInternal, string_of_internal_fn ifn, args); emeta}

let internal_meta = {mloc= no_span; mtype= UInt; madlevel= DataOnly}
let zero = {expr= Lit (Int, "0"); emeta= internal_meta}
let loop_bottom = {expr= Lit (Int, "1"); emeta= internal_meta}
let string_of_operator = mk_string_of sexp_of_operator
let operator_of_string = mk_of_string operator_of_sexp

let%test "bad op name" = phys_equal (operator_of_string "Pluss__") None
let%test "good op name" = operator_of_string "Plus__" = Some Plus

(** remove_size [st] discards size information from a sizedtype
    to return an unsizedtype. *)
let rec remove_size = function
  | SInt -> UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SArray (t, _) -> UArray (remove_size t)

(* -- Locations and spans --------------------------------------------------- *)

(** Render a location as a string *)
let rec string_of_location loc =
  let open Format in
  let included_from_str =
    match loc.included_from with
    | None -> ""
    | Some loc2 -> sprintf ", included from\n%s" (string_of_location loc2)
  in
  sprintf "file %s, line %d, column %d%s" loc.filename loc.line_num loc.col_num
    included_from_str

(** Render a location_span as a string *)
let string_of_location_span loc_sp =
  match loc_sp with {begin_loc; end_loc} ->
    let bf = begin_loc.filename in
    let ef = end_loc.filename in
    let bl = begin_loc.line_num in
    let el = end_loc.line_num in
    let bc = begin_loc.col_num in
    let ec = end_loc.col_num in
    let open Format in
    let file_line_col_string =
      if bf = ef then
        sprintf "file %s, %s" bf
          ( if bl = el then
            sprintf "line %d, %s" bl
              ( if bc = ec then sprintf "column %d" bc
              else sprintf "columns %d-%d" bc ec )
          else sprintf "line %d, column %d to line %d, column %d" bl bc el ec
          )
      else
        sprintf "file %s, line %d, column %d to file %s, line %d, column %d" bf
          bl bc ef el ec
    in
    let included_from_str =
      match begin_loc.included_from with
      | None -> ""
      | Some loc -> sprintf ", included from\n%s" (string_of_location loc)
    in
    sprintf "%s%s" file_line_col_string included_from_str

let merge_spans left right = {begin_loc= left.begin_loc; end_loc= right.end_loc}

(** Return two lines before and after the specified location. *)
let pp_context ppf ({filename; line_num; col_num; _} : Mir.location) =
  try
    let open In_channel in
    let input = create filename in
    for _ = 1 to line_num - 3 do
      ignore (input_line_exn input)
    done ;
    let get_line num =
      if num > 0 then
        match input_line input with
        | Some input -> Printf.sprintf "%6d:  %s\n" num input
        | _ -> ""
      else ""
    in
    let line_2_before = get_line (line_num - 2) in
    let line_before = get_line (line_num - 1) in
    let our_line = get_line line_num in
    let cursor_line = String.make (col_num + 9) ' ' ^ "^\n" in
    let line_after = get_line (line_num + 1) in
    let line_2_after = get_line (line_num + 2) in
    close input ;
    Fmt.pf ppf
      "   -------------------------------------------------\n\
       %s%s%s%s%s%s   -------------------------------------------------\n"
      line_2_before line_before our_line cursor_line line_after line_2_after
  with _ -> ()

(** Return two lines before and after the specified location
    and print a message *)
let pp_message_with_location ppf (message, loc) =
  Fmt.pf ppf "%a\n%s\n\n" pp_context loc message

(*-- mutable counter for symbol names --*)
let _counter = ref 0

let gensym () =
  _counter := !_counter + 1 ;
  sprintf "sym%d__" !_counter

let gensym_enter () =
  let old_counter = !_counter in
  (gensym (), fun () -> _counter := old_counter)

let ternary_if = "TernaryIf__"

(** A hash table to hold some name conversions between the AST nodes and the
    Stan Math name of the operator *)
let string_of_operators =
  Map.Poly.of_alist_multi
    [ (string_of_operator Plus, "add")
    ; (string_of_operator PPlus, "plus")
    ; (string_of_operator Minus, "subtract")
    ; (string_of_operator PMinus, "minus")
    ; (string_of_operator Times, "multiply")
    ; (string_of_operator Divide, "mdivide_right")
    ; (string_of_operator Divide, "divide")
    ; (string_of_operator Modulo, "modulus")
    ; (string_of_operator LDivide, "mdivide_left")
    ; (string_of_operator EltTimes, "elt_multiply")
    ; (string_of_operator EltDivide, "elt_divide")
    ; (string_of_operator Pow, "pow")
    ; (string_of_operator Or, "logical_or")
    ; (string_of_operator And, "logical_and")
    ; (string_of_operator Equals, "logical_eq")
    ; (string_of_operator NEquals, "logical_neq")
    ; (string_of_operator Less, "logical_lt")
    ; (string_of_operator Leq, "logical_lte")
    ; (string_of_operator Greater, "logical_gt")
    ; (string_of_operator Geq, "logical_gte")
    ; (string_of_operator PNot, "logical_negation")
    ; (string_of_operator Transpose, "transpose")
    ; (ternary_if, "if_else")
      (* XXX I don't think the following are able to be looked up at all as they aren't Ast.operators *)
    ; ("(OperatorAssign Plus)", "assign_add")
    ; ("(OperatorAssign Minus)", "assign_subtract")
    ; ("(OperatorAssign Times)", "assign_multiply")
    ; ("(OperatorAssign Divide)", "assign_divide")
    ; ("(OperatorAssign EltTimes)", "assign_elt_times")
    ; ("(OperatorAssign EltDivide)", "assign_elt_divide") ]

(** Querying stan_math_signatures for operator signatures by string name *)
let operator_return_type_from_string op_name argtypes =
  if op_name = "Assign" || op_name = "ArrowAssign" then
    match List.map ~f:snd argtypes with
    | [ut1; ut2] when check_of_same_type_mod_array_conv "" ut1 ut2 -> Some Void
    | _ -> None
  else
    Map.Poly.find_multi string_of_operators op_name
    |> List.find_map ~f:(fun name -> stan_math_returntype name argtypes)

let operator_return_type op =
  operator_return_type_from_string (string_of_operator op)
