open Frontend
open Std
open Result.Syntax

let untyped_ast_of_string s =
  let res, warnings = Parse.parse_program (`Code s) in
  Fmt.epr "%a" (Fmt.list ~sep:Fmt.nop Warnings.pp) warnings;
  res

let error_to_string ~code =
  Fmt.str "%a" (Errors.pp ?printed_filename:None ?code:(Some code))

let typed_ast_of_string_exn code =
  let ast =
    let* ast = untyped_ast_of_string code in
    Typechecker.check_program ~allow_undefined_functions:true ast
    |> Result.map_error ~f:(fun e -> Errors.Semantic_error e) in
  ast |> Result.map_error ~f:(error_to_string ~code) |> Result.get_ok' |> fst

let mir_of_string s = typed_ast_of_string_exn s |> Ast_to_Mir.trans_prog ""

(** Adapts [pp] for compact expected output. A large formatting margin reduces
    wrapping; remaining lines are trimmed and joined with spaces. A printer
    producing [A] and [B] on separate lines therefore prints [A B]. *)
let one_line (pp : 'a Fmt.t) : 'a Fmt.t =
 fun ppf x ->
  let buffer = Buffer.create 128 in
  let line_ppf = Format.formatter_of_buffer buffer in
  Format.pp_set_margin line_ppf 1_000_000;
  Fmt.pf line_ppf "%a@?" pp x;
  Buffer.contents buffer |> String.split_lines |> List.map ~f:String.trim
  |> String.concat ~sep:" " |> Fmt.string ppf

(** Prints one statement pattern with child labels instead of child bodies. For
    a loop whose body has label [8], the output is [for(n in 1:N) 8]. Other
    output rows show the statement stored at label [8]. *)
let pp_stmt : (Middle.Expr.Typed.t, int) Middle.Stmt.Pattern.t Fmt.t =
  one_line (Middle.Stmt.Pattern.pp Middle.Expr.Typed.pp Fmt.int)

(** Prints a label set in ascending order, separated by spaces. A set containing
    [3] and [1] prints as [1 3]; an empty set prints nothing. *)
let pp_labels ppf (labels : int Set.Poly.t) =
  Fmt.(list ~sep:(any " ") int) ppf (Set.Poly.to_list labels)
