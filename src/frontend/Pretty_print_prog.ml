open Core
open Core.Poly
open Ast
open Fmt
open Pretty_printing

let pp_bare_block ppf {stmts; xloc} =
  (hbox (box pp_list_of_statements)) ppf (stmts, xloc)

let pp_block block_name ppf {stmts; xloc} =
  pf ppf "%a {@,%a@,}@,"
    (Fmt.styled (`Fg `Yellow) string)
    block_name
    (indented_box pp_list_of_statements)
    (stmts, xloc)

let rec pp_block_list ppf = function
  | (name, {stmts; xloc}) :: tl ->
      if should_skip xloc.end_loc then pp_block_list ppf tl
      else (
        pp_spacing None (Some xloc.begin_loc) ppf (get_comments xloc.begin_loc);
        pp_block name ppf {stmts; xloc};
        pp_block_list ppf tl)
  | [] -> pp_spacing None None ppf (remaining_comments ())

let pp_program ~bare_functions ~line_length ~inline_includes ~strip_comments ppf
    { functionblock= bf
    ; datablock= bd
    ; transformeddatablock= btd
    ; parametersblock= bp
    ; transformedparametersblock= btp
    ; modelblock= bm
    ; generatedquantitiesblock= bgq
    ; comments } =
  Format.pp_set_margin ppf line_length;
  set_comments ~inline_includes ~strip_comments comments;
  print_included := inline_includes;
  Format.pp_open_vbox ppf 0;
  if bare_functions then pp_bare_block ppf @@ Option.value_exn bf
  else
    let blocks =
      List.filter_map
        ~f:(fun (name, block_opt) ->
          Option.map ~f:(fun b -> (name, b)) block_opt)
        [ ("functions", bf); ("data", bd); ("transformed data", btd)
        ; ("parameters", bp); ("transformed parameters", btp); ("model", bm)
        ; ("generated quantities", bgq) ] in
    pp_block_list ppf blocks

let check_correctness ?(bare_functions = false) prog pretty =
  let pretty = Common.Formatting.strip_ansi_escapes pretty in
  let result_ast =
    let res, (_ : Warnings.t list) =
      if bare_functions then Parse.parse_stanfunctions (`Code pretty)
      else Parse.parse_program (`Code pretty) in
    match res with
    | Ok prog -> prog
    | Error e ->
        let error =
          Fmt.str "%a" (Errors.pp ?printed_filename:None ~code:pretty) e in
        Common.ICE.internal_compiler_error
          [%message
            "Pretty-printed program failed to parse" error
              (prog : Ast.untyped_program)
              pretty] in
  if compare_untyped_program prog result_ast <> 0 then
    Common.ICE.internal_compiler_error
      [%message
        "Pretty-printed program does match the original!"
          (prog : Ast.untyped_program)
          (result_ast : Ast.untyped_program)]

let pretty_print_program ?(bare_functions = false) ?(line_length = 78)
    ?(inline_includes = false) ?(strip_comments = false) p =
  let result =
    (str_like stdout) "%a"
      (pp_program ~bare_functions ~line_length ~inline_includes ~strip_comments)
      p in
  check_correctness ~bare_functions p result;
  result

let pretty_print_typed_program ?(bare_functions = false) ?(line_length = 78)
    ?(inline_includes = false) ?(strip_comments = false) p =
  pretty_print_program ~bare_functions ~line_length ~inline_includes
    ~strip_comments
    (untyped_program_of_typed_program p)
