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
let rec string_of_location ?(print_file = true) ?(print_line = true) loc =
  let open Format in
  let file = if print_file then sprintf "'%s', " loc.filename else "" in
  let line = if print_line then sprintf "line %d, " loc.line_num else "" in
  let incl =
    match loc.included_from with
    | Some loc2 -> sprintf ", included from\n%s" (string_of_location loc2)
    | None -> ""
  in
  sprintf "%s%scolumn %d%s" file line loc.col_num incl

(** Render a location_span as a string *)
let string_of_location_span {begin_loc; end_loc} =
  let end_loc_str =
    match begin_loc.included_from with
    | None ->
        " to "
        ^ string_of_location
            ~print_file:(begin_loc.filename <> end_loc.filename)
            ~print_line:(begin_loc.line_num <> end_loc.line_num)
            end_loc
    | Some _ -> ""
  in
  string_of_location begin_loc ^ end_loc_str

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

let rec expr_contains_fn fname accum e =
  accum
  ||
  match e.expr with
  | FunApp (_, name, _) when name = fname -> true
  | x -> fold_expr (expr_contains_fn fname) accum x

let%test "expr contains fn" =
  internal_funapp FnReadData [] ()
  |> expr_contains_fn (string_of_internal_fn FnReadData) false

let contains_fn fname s =
  let rec contains_fn_go fname accum {stmt; _} =
    match stmt with
    | NRFunApp (_, fname', _) when fname' = fname -> true
    | _ ->
        fold_statement (expr_contains_fn fname) (contains_fn_go fname) accum
          stmt
  in
  contains_fn_go fname false s

let mock_stmt stmt = {stmt; smeta= no_span}
let mir_int i = {expr= Lit (Int, string_of_int i); emeta= internal_meta}

let mock_for i body =
  For
    { loopvar= "lv"
    ; lower= mir_int 0
    ; upper= mir_int i
    ; body= mock_stmt (Block [body]) }
  |> mock_stmt

let%test "contains fn" =
  let f =
    mock_for 8
      (mock_for 9
         (mock_stmt
            (Assignment (("v", []), internal_funapp FnReadData [] internal_meta))))
  in
  contains_fn
    (string_of_internal_fn FnReadData)
    (mock_stmt (Block [f; mock_stmt Break]))

let%test "contains nrfn" =
  let f =
    mock_for 8
      (mock_for 9
         (mock_stmt
            (NRFunApp (CompilerInternal, string_of_internal_fn FnWriteParam, []))))
  in
  contains_fn
    (string_of_internal_fn FnWriteParam)
    (mock_stmt
       (Block
          [ mock_stmt
              (NRFunApp
                 (CompilerInternal, string_of_internal_fn FnWriteParam, []))
          ; f ]))
