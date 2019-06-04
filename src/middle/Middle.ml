include Mir
include Stan_math_signatures
include Type_conversion
include Mir_utils
open Core_kernel
module Validation = Validation
module Pretty = Mir_pretty_printer
module Utils = Utils

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

(*-- mutable counter for symbol names --*)
let _counter = ref 0

let gensym () =
  _counter := !_counter + 1 ;
  sprintf "sym%d__" !_counter

let gensym_enter () =
  let old_counter = !_counter in
  (gensym (), fun () -> _counter := old_counter)

let gensym_reset_danger_use_cautiously () = _counter := 0

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

let rec sexp_of_expr_typed_located {expr; _} =
  sexp_of_expr sexp_of_expr_typed_located expr

let rec sexp_of_stmt_loc {stmt; _} =
  sexp_of_statement sexp_of_expr_typed_located sexp_of_stmt_loc stmt
