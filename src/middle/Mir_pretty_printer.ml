(* == Pretty printers ======================================================= *)
open Core_kernel
open Mir

let pp_builtin_syntax = Fmt.(string |> styled `Yellow)
let pp_keyword = Fmt.(string |> styled `Blue)
let angle_brackets pp_v ppf v = Fmt.pf ppf "@[<1><%a>@]" pp_v v

let pp_operator ppf = function
  | Plus | PPlus -> Fmt.pf ppf "+"
  | Minus | PMinus -> Fmt.pf ppf "-"
  | Times -> Fmt.pf ppf "*"
  | Divide -> Fmt.pf ppf "/"
  | Modulo -> Fmt.pf ppf "%%"
  | LDivide -> Fmt.pf ppf "\\"
  | EltTimes -> Fmt.pf ppf ".*"
  | EltDivide -> Fmt.pf ppf "./"
  | Pow -> Fmt.pf ppf "^"
  | Or -> Fmt.pf ppf "||"
  | And -> Fmt.pf ppf "&&"
  | Equals -> Fmt.pf ppf "=="
  | NEquals -> Fmt.pf ppf "!="
  | Less -> Fmt.pf ppf "<"
  | Leq -> Fmt.pf ppf "<="
  | Greater -> Fmt.pf ppf ">"
  | Geq -> Fmt.pf ppf ">="
  | PNot -> Fmt.pf ppf "!"
  | Transpose -> Fmt.pf ppf "'"

let pp_autodifftype ppf = function
  | DataOnly -> pp_keyword ppf "data "
  | AutoDiffable -> ()

let pp_brackets_postfix pp_e ppf = Fmt.pf ppf {|%a[]|} pp_e

let unsized_array_depth unsized_ty =
  let rec aux depth = function
    | UArray ut -> aux (depth + 1) ut
    | ut -> (ut, depth)
  in
  aux 0 unsized_ty

let rec pp_unsizedtype ppf = function
  | UInt -> pp_keyword ppf "int"
  | UReal -> pp_keyword ppf "real"
  | UVector -> pp_keyword ppf "vector"
  | URowVector -> pp_keyword ppf "row_vector"
  | UMatrix -> pp_keyword ppf "matrix"
  | UArray ut ->
      let ty, depth = unsized_array_depth ut in
      let commas = String.make depth ',' in
      Fmt.pf ppf "%a[%s]" pp_unsizedtype ty commas
  | UFun (argtypes, rt) ->
      Fmt.pf ppf {|@[<h>(%a) => %a@]|}
        Fmt.(list pp_fun_arg ~sep:comma)
        argtypes pp_returntype rt
  | UMathLibraryFunction ->
      (angle_brackets Fmt.string) ppf "Stan Math function"

and pp_fun_arg ppf (ad_ty, unsized_ty) =
  match ad_ty with
  | DataOnly -> Fmt.pf ppf {|data %a|} pp_unsizedtype unsized_ty
  | _ -> pp_unsizedtype ppf unsized_ty

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

let pp_possiblysizedtype pp_e ppf = function
  | Sized st -> pp_sizedtype pp_e ppf st
  | Unsized ust -> pp_unsizedtype ppf ust

let rec pp_expr pp_e ppf = function
  | Var varname -> Fmt.string ppf varname
  | Lit (Str, str) -> Fmt.pf ppf "%S" str
  | Lit (_, str) -> Fmt.string ppf str
  | FunApp (StanLib, name, [lhs; rhs])
    when Option.is_some (Mir_utils.operator_of_string name) ->
      Fmt.pf ppf "(%a %a %a)" pp_e lhs pp_operator
        (Option.value_exn (Mir_utils.operator_of_string name))
        pp_e rhs
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
  | Assignment ((assignee, _, idcs), rhs) ->
      Fmt.pf ppf {|@[<h>%a =@ %a;@]|} (pp_indexed pp_e) (assignee, idcs) pp_e
        rhs
  | TargetPE expr ->
      Fmt.pf ppf {|@[<h>%a +=@ %a;@]|} pp_keyword "target" pp_e expr
  | NRFunApp (_, name, args) ->
      Fmt.pf ppf {|@[%s%a;@]|} name Fmt.(list pp_e ~sep:comma |> parens) args
  | Break -> pp_keyword ppf "break;"
  | Continue -> pp_keyword ppf "continue;"
  | Skip -> pp_keyword ppf ";"
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
      Fmt.pf ppf {|%a%a %s;|} pp_autodifftype decl_adtype
        (pp_possiblysizedtype pp_e)
        decl_type decl_id

let pp_block label pp_elem ppf elems =
  match elems with
  | [] -> ()
  | elems ->
      Fmt.pf ppf {|@[<v2>%a {@ %a@]@ }|} pp_keyword label
        Fmt.(list ~sep:cut pp_elem)
        elems ;
      Format.pp_force_newline ppf ()

let pp_functions_block pp_s ppf {functions_block; _} =
  pp_block "functions" pp_s ppf functions_block

let pp_prepare_data pp_s ppf {prepare_data; _} =
  pp_block "prepare_data" pp_s ppf prepare_data

let pp_log_prob pp_s ppf {log_prob; _} = pp_block "log_prob" pp_s ppf log_prob

let pp_generate_quantities pp_s ppf {generate_quantities; _} =
  pp_block "generate_quantities" pp_s ppf generate_quantities

let pp_transform_inits pp_s ppf {transform_inits; _} =
  pp_block "transform_inits" pp_s ppf transform_inits

let pp_io_block ppf = function
  | Parameters -> Fmt.string ppf "parameters"
  | TransformedParameters -> Fmt.string ppf "transformed_parameters"
  | GeneratedQuantities -> Fmt.string ppf "generated_quantities"

let pp_output_var pp_e ppf
    (name, {out_unconstrained_st; out_constrained_st; out_block; _}) =
  Fmt.pf ppf "@[<h>%a %a %s; //%a@]" pp_io_block out_block (pp_sizedtype pp_e)
    out_constrained_st name (pp_sizedtype pp_e) out_unconstrained_st

let pp_input_var pp_e ppf (name, sized_ty) =
  Fmt.pf ppf "@[<h>%a %s;@]" (pp_sizedtype pp_e) sized_ty name

let pp_input_vars pp_e ppf {input_vars; _} =
  pp_block "input_vars" (pp_input_var pp_e) ppf input_vars

let pp_output_vars pp_e ppf {output_vars; _} =
  pp_block "output_vars" (pp_output_var pp_e) ppf output_vars

let pp_prog pp_e pp_s ppf prog =
  Format.open_vbox 0 ;
  pp_functions_block (pp_fun_def pp_s) ppf prog ;
  pp_input_vars pp_e ppf prog ;
  pp_prepare_data pp_s ppf prog ;
  pp_log_prob pp_s ppf prog ;
  pp_generate_quantities pp_s ppf prog ;
  pp_transform_inits pp_s ppf prog ;
  pp_output_vars pp_e ppf prog ;
  Format.close_box ()

let rec pp_expr_typed_located ppf {expr; _} =
  pp_expr pp_expr_typed_located ppf expr

let rec pp_stmt_loc ppf {stmt; _} =
  pp_statement pp_expr_typed_located pp_stmt_loc ppf stmt

let pp_typed_prog ppf prog = pp_prog pp_expr_typed_located pp_stmt_loc ppf prog

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
