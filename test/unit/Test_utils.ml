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

(** Compile a program to optimized MIR through the same driver path as stanc, so
    [flags] selects the optimizations exactly like the command line does. *)
let compile_mir code flags =
  (* stan2mir sets the global model name, which later mir_of_string calls
     read *)
  let model_name = !Typechecker.model_name in
  Fun.protect
    ~finally:(fun () -> Typechecker.model_name := model_name)
    (fun () ->
      match
        Driver.Entry.stan2mir "entry_test_model" (`Code code) flags (fun _ ->
            ())
      with
      | Ok mir -> mir
      | Error error -> failwith (error_to_string ~code error))

(** Prints the output of [pp] on one line. If [pp] prints [A] and [B] on two
    lines, [one_line pp] prints [A B]. *)
let one_line (pp : 'a Fmt.t) : 'a Fmt.t =
 fun ppf x ->
  let buffer = Buffer.create 128 in
  let line_ppf = Format.formatter_of_buffer buffer in
  Format.pp_set_margin line_ppf 1_000_000;
  Fmt.pf line_ppf "%a@?" pp x;
  Buffer.contents buffer |> String.split_lines |> List.map ~f:String.trim
  |> String.concat ~sep:" " |> Fmt.string ppf

(** Prints a statement pattern on one line. A label is the integer that
    identifies a statement in the statement map of
    {!Analysis_and_optimization.Dependence_analysis}. A statement pattern holds
    the label of each child statement in place of the child statement. A loop
    with body label 8 prints as [for(n in 1:N) 8]. *)
let pp_stmt : (Middle.Expr.Typed.t, int) Middle.Stmt.Pattern.t Fmt.t =
  one_line (Middle.Stmt.Pattern.pp Middle.Expr.Typed.pp Fmt.int)

(** Prints a label set in ascending order, with a space between labels. The set
    of 3 and 1 prints as [1 3]. An empty set prints nothing. *)
let pp_labels ppf (labels : int Set.Poly.t) =
  Fmt.(list ~sep:(any " ") int) ppf (Set.Poly.to_list labels)
