include Mir
include Mir_pretty_printer
include Stan_math_signatures
include Type_conversion
open Core_kernel
module Validation = Validation

(* ===================== Some helper functions and values ====================== *)

let expr_from_idx (i : expr_typed_located index) =
  match i with
  | All -> []
  | Single e | Upfrom e | Downfrom e | MultiIndex e -> [e]
  | Between (e1, e2) -> [e1; e2]

(** remove_size [st] discards size information from a sizedtype
    to return an unsizedtype. *)
let rec remove_size = function
  | SInt -> UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SArray (t, _) -> UArray (remove_size t)

let remove_possible_size = function Sized t -> remove_size t | Unsized t -> t
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

let gensym_reset_danger_use_cautiously () = _counter := 0
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
