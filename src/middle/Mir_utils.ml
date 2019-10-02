open Core_kernel
open Mir

(* ===================== Some helper functions and values ====================== *)

let expr_from_idx (i : expr_typed_located index) =
  match i with
  | All -> []
  | Single e | Upfrom e | MultiIndex e -> [e]
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

let binop e1 binop e2 =
  { expr= FunApp (StanLib, string_of_operator binop, [e1; e2])
  ; emeta= internal_meta }

(** remove_size [st] discards size information from a sizedtype
    to return an unsizedtype. *)
let rec remove_size = function
  | SInt -> UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SArray (t, _) -> UArray (remove_size t)

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

(** [iter_stmt_transform transform stmts] will run the transform function
    on the list of statements until the list of statements stops changing.

    In our use case this could be replaced with a bottom-up map.
*)
let iter_stmt_transform f stmts =
  let old_stmts = ref stmts in
  let new_stmts = ref (f stmts) in
  (* The count here just enforces a hard limit on the number of iterations.
     In principle, we should only use this function with monotonic update
     functions, but in practice we may get that wrong from time to time and
     this count keeps us from infinite loops.
  *)
  let count = ref 0 in
    while (Stdlib.(!=) !old_stmts !new_stmts) && !count < 100 do
    old_stmts := !new_stmts ;
    new_stmts := f !old_stmts ;
    count := !count + 1
  done ;
  !new_stmts

(** [cleanup_stmts statements] will do a few simple transformations like
    removing Skips, collapsing empty blocks and SLists, etc. *)
let rec cleanup_stmts_one_pass stmts =
  let rec cleanup_stmt s =
    let ellide = {s with stmt= Skip} in
    match s.stmt with
    | Block [] | SList [] -> ellide
    | For {body= {stmt= Skip; _}; _} -> ellide
    | While (_, {stmt= Skip; _}) -> ellide
    | SList ls -> {s with stmt= SList (cleanup_stmts_one_pass ls)}
    | Block ls -> {s with stmt= Block (cleanup_stmts_one_pass ls)}
    | _ -> {s with stmt= map_statement Fn.id cleanup_stmt s.stmt}
  in
  let is_decl = function {stmt= Decl _; _} -> true | _ -> false in
  let flatten_block s =
    match s.stmt with
    | SList ls | Block ls ->
        if List.for_all ~f:(Fn.non is_decl) ls then ls else [s]
    | _ -> [s]
  in
  let ellide_skip s = match s.stmt with Skip -> [] | _ -> [s] in
  List.map stmts ~f:cleanup_stmt
  |> List.concat_map ~f:flatten_block
  |> List.concat_map ~f:ellide_skip

let cleanup_empty_stmts stmts = iter_stmt_transform cleanup_stmts_one_pass stmts

let%expect_test "cleanup" =
  let swrap stmt = {stmt; smeta= no_span} in
  let body = Block [Skip |> swrap] |> swrap in
  let s = For {loopvar= "i"; lower= loop_bottom; upper= loop_bottom; body} in
  let res = [s |> swrap] |> cleanup_empty_stmts in
  [%sexp (res : stmt_loc list)] |> print_s ;
  [%expect {|
    () |}]

let map_prog_stmts f p =
  { p with
    prepare_data= f p.prepare_data
  ; log_prob= f p.log_prob
  ; generate_quantities= f p.generate_quantities
  ; transform_inits= f p.transform_inits
  }
