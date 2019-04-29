(** Some helpers to produce nice error messages and for auto-formatting Stan programs *)

(* TODO: this should probably be made tail recursive by accumulating list of strings in reverse *)
(* TODO: to preserve comments during pretty printing, we should capture them during parsing and attach them to AST nodes *)
(* TODO: we could consider cutting off lines after 80 characters *)

open Core_kernel
open Ast

let indent_num = ref 1
let begin_indent _ = indent_num := 1 + !indent_num
let exit_indent _ = indent_num := -1 + !indent_num
let tabs () = String.make (2 * !indent_num) ' '

let rec unwind_sized_array_type = function
  | Mir.SArray (st, e) -> (
    match unwind_sized_array_type st with st2, es -> (st2, e :: es) )
  | st -> (st, [])

let rec unwind_array_type = function
  | Mir.UArray ut -> (
    match unwind_array_type ut with ut2, d -> (ut2, d + 1) )
  | ut -> (ut, 0)

(** XXX this should use the MIR pretty printers after AST pretty printers
    are updated to use `Fmt`. *)
let rec pretty_print_autodifftype = function
  | Mir.DataOnly -> "data "
  | Mir.AutoDiffable -> ""

and pretty_print_unsizedtype = function
  | Mir.UInt -> "int"
  | Mir.UReal -> "real"
  | Mir.UVector -> "vector"
  | Mir.URowVector -> "row_vector"
  | Mir.UMatrix -> "matrix"
  | Mir.UArray ut ->
      let ut2, d = unwind_array_type ut in
      pretty_print_unsizedtype ut2 ^ ("[" ^ String.make d ',') ^ "]"
  | Mir.UFun (argtypes, rt) ->
      "("
      ^ String.concat ~sep:", " (List.map ~f:pretty_print_argtype argtypes)
      ^ ") => " ^ pretty_print_returntype rt
  | Mir.UMathLibraryFunction -> "Stan Math function"

and pretty_print_unsizedtypes l =
  String.concat ~sep:", " (List.map ~f:pretty_print_unsizedtype l)

and pretty_print_argtype = function
  | at, ut -> pretty_print_autodifftype at ^ pretty_print_unsizedtype ut

and pretty_print_returntype = function
  | ReturnType x -> pretty_print_unsizedtype x
  | Void -> "void"

and pretty_print_identifier id = id.name

and pretty_print_operator = function
  | Mir.Plus | PPlus -> "+"
  | Minus | PMinus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Modulo -> "%"
  | LDivide -> "\\"
  | EltTimes -> ".*"
  | EltDivide -> "./"
  | Pow -> "^"
  | Or -> "||"
  | And -> "&&"
  | Equals -> "=="
  | NEquals -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | PNot -> "!"
  | Transpose -> "'"

and pretty_print_index = function
  | All -> " : "
  | Single e -> pretty_print_expression e
  | Upfrom e -> pretty_print_expression e ^ " : "
  | Downfrom e -> " : " ^ pretty_print_expression e
  | Between (e1, e2) ->
      pretty_print_expression e1 ^ " : " ^ pretty_print_expression e2

and pretty_print_list_of_indices l =
  String.concat ~sep:", " (List.map ~f:pretty_print_index l)

and pretty_print_expression {expr= e_content; _} =
  match e_content with
  | TernaryIf (e1, e2, e3) ->
      pretty_print_expression e1 ^ " ? " ^ pretty_print_expression e2 ^ " : "
      ^ pretty_print_expression e3
  | BinOp (e1, op, e2) ->
      pretty_print_expression e1 ^ " " ^ pretty_print_operator op ^ " "
      ^ pretty_print_expression e2
  | PrefixOp (op, e) -> pretty_print_operator op ^ pretty_print_expression e
  | PostfixOp (e, op) -> pretty_print_expression e ^ pretty_print_operator op
  | Variable id -> pretty_print_identifier id
  | IntNumeral i -> i
  | RealNumeral r -> r
  | FunApp (_, id, es) ->
      pretty_print_identifier id ^ "("
      ^ pretty_print_list_of_expression es
      ^ ")"
  | CondDistApp (id, es) ->
      pretty_print_identifier id ^ "("
      ^ ( match es with
        | [] -> Errors.fatal_error ()
        | e :: es' ->
            pretty_print_expression e ^ "| "
            ^ pretty_print_list_of_expression es' )
      ^ ")"
  (* GetLP is deprecated *)
  | GetLP -> "get_lp()"
  | GetTarget -> "target()"
  | ArrayExpr es -> "{" ^ pretty_print_list_of_expression es ^ "}"
  | RowVectorExpr es -> "[" ^ pretty_print_list_of_expression es ^ "]"
  | Paren e -> "(" ^ pretty_print_expression e ^ ")"
  | Indexed (e, l) -> (
      pretty_print_expression e
      ^
      match l with [] -> "" | l -> "[" ^ pretty_print_list_of_indices l ^ "]" )

and pretty_print_list_of_expression es =
  String.concat ~sep:", " (List.map ~f:pretty_print_expression es)

and pretty_print_assignmentoperator = function
  | Assign -> "="
  (* ArrowAssign is deprecated *)
  | ArrowAssign -> "<-"
  | OperatorAssign op -> pretty_print_operator op ^ "="

and pretty_print_truncation = function
  | NoTruncate -> ""
  | TruncateUpFrom e -> " T[" ^ pretty_print_expression e ^ ", ]"
  | TruncateDownFrom e -> " T[ , " ^ pretty_print_expression e ^ "]"
  | TruncateBetween (e1, e2) ->
      " T[" ^ pretty_print_expression e1 ^ ", " ^ pretty_print_expression e2
      ^ "]"

and pretty_print_printable = function
  | PString s -> s
  | PExpr e -> pretty_print_expression e

and pretty_print_list_of_printables l =
  String.concat ~sep:", " (List.map ~f:pretty_print_printable l)

and pretty_print_sizedtype = function
  | Mir.SInt -> "int"
  | Mir.SReal -> "real"
  | Mir.SVector e -> "vector[" ^ pretty_print_expression e ^ "]"
  | Mir.SRowVector e -> "row_vector[" ^ pretty_print_expression e ^ "]"
  | Mir.SMatrix (e1, e2) ->
      "matrix[" ^ pretty_print_expression e1 ^ ", "
      ^ pretty_print_expression e2 ^ "]"
  | Mir.SArray _ -> raise (Errors.FatalError "This should never happen.")

and pretty_print_transformation = function
  | Identity -> ""
  | Lower e -> "<lower=" ^ pretty_print_expression e ^ ">"
  | Upper e -> "<upper=" ^ pretty_print_expression e ^ ">"
  | LowerUpper (e1, e2) ->
      "<lower=" ^ pretty_print_expression e1 ^ ", upper="
      ^ pretty_print_expression e2 ^ ">"
  | Offset e -> "<offset=" ^ pretty_print_expression e ^ ">"
  | Multiplier e -> "<multiplier=" ^ pretty_print_expression e ^ ">"
  | OffsetMultiplier (e1, e2) ->
      "<offset=" ^ pretty_print_expression e1 ^ ", multiplier="
      ^ pretty_print_expression e2 ^ ">"
  | Ordered -> ""
  | PositiveOrdered -> ""
  | Simplex -> ""
  | UnitVector -> ""
  | CholeskyCorr -> ""
  | CholeskyCov -> ""
  | Correlation -> ""
  | Covariance -> ""

and pretty_print_transformed_type st trans =
  let unsizedtype_string, sizes_string =
    match st with
    | Mir.SInt -> (pretty_print_unsizedtype UInt, "")
    | SReal -> (pretty_print_unsizedtype UReal, "")
    | SVector e ->
        ( pretty_print_unsizedtype UVector
        , "[" ^ pretty_print_expression e ^ "]" )
    | SRowVector e ->
        ( pretty_print_unsizedtype URowVector
        , "[" ^ pretty_print_expression e ^ "]" )
    | SMatrix (e1, e2) ->
        ( pretty_print_unsizedtype UMatrix
        , "[" ^ pretty_print_expression e1 ^ ", " ^ pretty_print_expression e2
          ^ "]" )
    | SArray _ -> (
      match unwind_sized_array_type st with
      | st, _ -> (pretty_print_sizedtype st, "") )
  in
  let cov_sizes_string =
    match st with
    | SMatrix (e1, e2) ->
        if e1 = e2 then "[" ^ pretty_print_expression e1 ^ "]"
        else
          "[" ^ pretty_print_expression e1 ^ ", " ^ pretty_print_expression e2
          ^ "]"
    | _ -> ""
  in
  match trans with
  | Identity -> pretty_print_sizedtype st
  | Lower _ | Upper _ | LowerUpper _ | Offset _ | Multiplier _
   |OffsetMultiplier _ ->
      unsizedtype_string ^ pretty_print_transformation trans ^ sizes_string
  | Ordered -> "ordered" ^ sizes_string
  | PositiveOrdered -> "positive_ordered" ^ sizes_string
  | Simplex -> "simplex" ^ sizes_string
  | UnitVector -> "unit_vector" ^ sizes_string
  | CholeskyCorr -> "cholesky_factor_corr" ^ cov_sizes_string
  | CholeskyCov -> "cholesky_factor_cov" ^ cov_sizes_string
  | Correlation -> "corr_matrix" ^ cov_sizes_string
  | Covariance -> "cov_matrix" ^ cov_sizes_string

and pretty_print_array_dims = function
  | [] -> ""
  | es -> "[" ^ pretty_print_list_of_expression (List.rev es) ^ "]"

and pretty_print_statement {stmt= s_content; _} =
  match s_content with
  | Assignment
      { assign_identifier= id
      ; assign_indices= lindex
      ; assign_op= assop
      ; assign_rhs= e } ->
      pretty_print_identifier id
      ^ ( match lindex with
        | [] -> ""
        | l -> "[" ^ pretty_print_list_of_indices l ^ "]" )
      ^ " "
      ^ pretty_print_assignmentoperator assop
      ^ " " ^ pretty_print_expression e ^ ";"
  | NRFunApp (_, id, es) ->
      pretty_print_identifier id ^ "("
      ^ pretty_print_list_of_expression es
      ^ ")" ^ ";"
  | TargetPE e -> "target += " ^ pretty_print_expression e ^ ";"
  | IncrementLogProb e ->
      "increment_log_prob(" ^ pretty_print_expression e ^ ");"
  | Tilde {arg= e; distribution= id; args= es; truncation= t} ->
      pretty_print_expression e ^ " ~ " ^ pretty_print_identifier id ^ "("
      ^ pretty_print_list_of_expression es
      ^ ")" ^ pretty_print_truncation t ^ ";"
  | Break -> "break;"
  | Continue -> "continue;"
  | Return e -> "return " ^ pretty_print_expression e ^ ";"
  | ReturnVoid -> "return;"
  | Print ps -> "print(" ^ pretty_print_list_of_printables ps ^ ");"
  | Reject ps -> "reject(" ^ pretty_print_list_of_printables ps ^ ");"
  | Skip -> ";"
  | IfThenElse (e, s, None) ->
      "if (" ^ pretty_print_expression e ^ ") " ^ pretty_print_statement s
  | IfThenElse (e, s1, Some s2) ->
      "if (" ^ pretty_print_expression e ^ ") " ^ pretty_print_statement s1
      ^ "\n" ^ tabs () ^ "else " ^ pretty_print_statement s2
  | While (e, s) ->
      "while (" ^ pretty_print_expression e ^ ") " ^ pretty_print_statement s
  | For {loop_variable= id; lower_bound= e1; upper_bound= e2; loop_body= s} ->
      "for (" ^ pretty_print_identifier id ^ " in "
      ^ pretty_print_expression e1 ^ " : " ^ pretty_print_expression e2 ^ ") "
      ^ pretty_print_statement s
  | ForEach (id, e, s) ->
      "for (" ^ pretty_print_identifier id ^ " in " ^ pretty_print_expression e
      ^ ") " ^ pretty_print_statement s
  | Block vdsl ->
      let s1 = "{\n" in
      let _ = begin_indent () in
      let s2 = pretty_print_list_of_statements vdsl in
      let _ = exit_indent () in
      let s3 = tabs () ^ "}" in
      s1 ^ s2 ^ s3
  | VarDecl
      { sizedtype= st
      ; transformation= trans
      ; identifier= id
      ; initial_value= init
      ; is_global= _ } ->
      let st2, es = unwind_sized_array_type st in
      let init_string =
        match init with
        | None -> ""
        | Some e -> " = " ^ pretty_print_expression e
      in
      pretty_print_transformed_type st2 trans
      ^ " " ^ pretty_print_identifier id ^ pretty_print_array_dims es
      ^ init_string ^ ";"
  | FunDef {returntype= rt; funname= id; arguments= args; body= b} -> (
      pretty_print_returntype rt ^ " " ^ pretty_print_identifier id ^ "("
      ^ String.concat ~sep:", "
          (List.map
             ~f:(function
               | at, ut, id ->
                   pretty_print_autodifftype at
                   ^ pretty_print_unsizedtype ut
                   ^ " " ^ pretty_print_identifier id )
             args)
      ^
      match b with
      | {stmt= Skip; _} -> ");"
      | b -> ") " ^ pretty_print_statement b )

and pretty_print_list_of_statements l =
  String.concat ~sep:"\n"
    (List.map ~f:(fun x -> tabs () ^ pretty_print_statement x) l)
  ^ "\n"

and pretty_print_program = function
  | { functionblock= bf
    ; datablock= bd
    ; transformeddatablock= btd
    ; parametersblock= bp
    ; transformedparametersblock= btp
    ; modelblock= bm
    ; generatedquantitiesblock= bgq } -> (
      ( match bf with
      | None -> ""
      | Some x -> "functions {\n" ^ pretty_print_list_of_statements x ^ "}\n"
      )
      ^ ( match bd with
        | None -> ""
        | Some x -> "data {\n" ^ pretty_print_list_of_statements x ^ "}\n" )
      ^ ( match btd with
        | None -> ""
        | Some x ->
            "transformed data {\n" ^ pretty_print_list_of_statements x ^ "}\n"
        )
      ^ ( match bp with
        | None -> ""
        | Some x ->
            "parameters {\n" ^ pretty_print_list_of_statements x ^ "}\n" )
      ^ ( match btp with
        | None -> ""
        | Some x ->
            "transformed parameters {\n"
            ^ pretty_print_list_of_statements x
            ^ "}\n" )
      ^ ( match bm with
        | None -> ""
        | Some x -> "model {\n" ^ pretty_print_list_of_statements x ^ "}\n" )
      ^
      match bgq with
      | None -> ""
      | Some x ->
          "generated quantities {\n"
          ^ pretty_print_list_of_statements x
          ^ "}\n" )
