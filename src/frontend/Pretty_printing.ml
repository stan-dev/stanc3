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

let wrap_fmt fmt x = x |> fmt Format.str_formatter |> Format.flush_str_formatter

let rec unwind_sized_array_type = function
  | Middle.SArray (st, e) -> (
    match unwind_sized_array_type st with st2, es -> (st2, es @ [e]) )
  | st -> (st, [])

let rec unwind_array_type = function
  | Middle.UArray ut -> (
    match unwind_array_type ut with ut2, d -> (ut2, d + 1) )
  | ut -> (ut, 0)

(** XXX this should use the MIR pretty printers after AST pretty printers
    are updated to use `Fmt`. *)
let rec pp_autodifftype ppf = function
  | Middle.DataOnly -> Fmt.pf ppf "data "
  | Middle.AutoDiffable -> Fmt.pf ppf ""

and pretty_print_autodifftype autodifftype = autodifftype |> pp_autodifftype Format.str_formatter |> Format.flush_str_formatter

and pp_unsizedtype ppf = function
  | Middle.UInt -> Fmt.pf ppf "int"
  | Middle.UReal -> Fmt.pf ppf "real"
  | Middle.UVector -> Fmt.pf ppf "vector"
  | Middle.URowVector -> Fmt.pf ppf "row_vector"
  | Middle.UMatrix -> Fmt.pf ppf "matrix"
  | Middle.UArray ut ->
     let ut2, d = unwind_array_type ut in
     let array_str = "[" ^ (String.make d ',') ^ "]" in
     Fmt.(suffix (const string array_str) pp_unsizedtype ppf ut2)
  | Middle.UFun (argtypes, rt) ->
     Fmt.pf ppf "{|@[<h>(%a) => %a@]|}" Fmt.(list ~sep:comma pp_argtype) argtypes pp_returntype rt
  | Middle.UMathLibraryFunction -> Fmt.pf ppf "Stan Math function"

and pretty_print_unsizedtype ut = wrap_fmt pp_unsizedtype ut

and pp_unsizedtypes ppf l =
  Fmt.(list ~sep:comma pp_unsizedtype) ppf l

and pp_argtype ppf = function
  | at, ut -> Fmt.append pp_autodifftype pp_unsizedtype ppf (at, ut)

and pp_returntype ppf = function
  | ReturnType x -> pp_unsizedtype ppf x
  | Void -> Fmt.pf ppf "void"

and pretty_print_returntype rt = wrap_fmt pp_returntype rt

and pp_identifier ppf id = Fmt.pf ppf "%s" id.name

and pretty_print_identifier i = wrap_fmt pp_identifier i

and pp_operator ppf = function
  | Middle.Plus | PPlus -> Fmt.pf ppf "+"
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

and pretty_print_operator op = wrap_fmt pp_operator op

and pp_index ppf = function
  | All -> Fmt.pf ppf " : "
  | Single e -> pp_expression ppf e
  | Upfrom e -> Fmt.pf ppf "%a : " pp_expression e
  | Downfrom e -> Fmt.pf ppf " : %a" pp_expression e
  | Between (e1, e2) -> Fmt.pf ppf "%a : %a" pp_expression e1 pp_expression e2

and pp_list_of_indices ppf l =
  Fmt.(list ~sep:comma pp_index) ppf l

and pretty_print_list_of_indices is = wrap_fmt pp_list_of_indices is

and pp_expression ppf {expr= e_content; _} =
  match e_content with
  | TernaryIf (e1, e2, e3) ->
     Fmt.pf ppf "%a ? %a : %a" pp_expression e1 pp_expression e2 pp_expression e3
  | BinOp (e1, op, e2) ->
     Fmt.pf ppf "%a %a %a" pp_expression e1 pp_operator op pp_expression e2
  | PrefixOp (op, e) -> Fmt.pf ppf "%a%a" pp_operator op pp_expression e
  | PostfixOp (e, op) -> Fmt.pf ppf "%a%a" pp_expression e pp_operator op
  | Variable id -> pp_identifier ppf id
  | IntNumeral i -> Fmt.pf ppf "%s" i
  | RealNumeral r -> Fmt.pf ppf "%s" r
  | FunApp (_, id, es) -> Fmt.pf ppf "%a(%a)" pp_identifier id pp_list_of_expression es
  | CondDistApp (id, es) -> (
    match es with
    | [] -> Errors.fatal_error ()
    | e :: es' -> Fmt.pf ppf "%a(%a| %a)"
                    pp_identifier id
                    pp_expression e
                    pp_list_of_expression es')
  (* GetLP is deprecated *)
  | GetLP -> Fmt.pf ppf "get_lp()"
  | GetTarget -> Fmt.pf ppf "target()"
  | ArrayExpr es -> Fmt.pf ppf "{%a}" pp_list_of_expression es
  | RowVectorExpr es -> Fmt.pf ppf "[%a]" pp_list_of_expression es
  | Paren e -> Fmt.pf ppf "(%a)" pp_expression e
  | Indexed (e, l) ->
    match l with
    | [] -> Fmt.pf ppf "%a" pp_expression e
    | l -> Fmt.pf ppf "%a[%a]" pp_expression e pp_list_of_indices l

and pretty_print_expression e = wrap_fmt pp_expression e

and pp_list_of_expression ppf es =
  Fmt.(list ~sep:comma pp_expression) ppf es

and pretty_print_list_of_expression es = wrap_fmt pp_list_of_expression es

and pp_assignmentoperator ppf = function
  | Assign -> Fmt.pf ppf "="
  (* ArrowAssign is deprecated *)
  | ArrowAssign -> Fmt.pf ppf "<-"
  | OperatorAssign op -> Fmt.pf ppf "%a=" pp_operator op

and pretty_print_assignmentoperator op = wrap_fmt pp_assignmentoperator op

and pp_truncation ppf = function
  | NoTruncate -> Fmt.pf ppf ""
  | TruncateUpFrom e -> Fmt.pf ppf " T[%a, ]" pp_expression e
  | TruncateDownFrom e -> Fmt.pf ppf " T[ , %a]" pp_expression e
  | TruncateBetween (e1, e2) ->
     Fmt.pf ppf " T[%a, %a]" pp_expression e1 pp_expression e2

and pretty_print_truncation t = wrap_fmt pp_truncation t

and pp_printable ppf = function
  | PString s -> Fmt.pf ppf "%s" s
  | PExpr e -> pp_expression ppf e

and pp_list_of_printables ppf l =
  Fmt.(list ~sep:comma pp_printable) ppf l

and pretty_print_list_of_printables l = wrap_fmt pp_list_of_printables l

and pp_sizedtype ppf = function
  | Middle.SInt -> Fmt.pf ppf "int"
  | Middle.SReal -> Fmt.pf ppf "real"
  | Middle.SVector e -> Fmt.pf ppf "vector[%a]" pp_expression e
  | Middle.SRowVector e -> Fmt.pf ppf "row_vector[%a]" pp_expression e
  | Middle.SMatrix (e1, e2) -> Fmt.pf ppf "matrix[%a, %a]" pp_expression e1 pp_expression e2
  | Middle.SArray _ -> raise (Errors.FatalError "This should never happen.")

and pretty_print_sizedtype st = wrap_fmt pp_sizedtype st

and pp_transformation ppf = function
  | Identity -> Fmt.pf ppf ""
  | Lower e -> Fmt.pf ppf "<lower=%a>" pp_expression e
  | Upper e -> Fmt.pf ppf "<upper=%a>" pp_expression e
  | LowerUpper (e1, e2) ->
      Fmt.pf ppf "<lower=%a, upper=%a>" pp_expression e1 pp_expression e2
  | Offset e -> Fmt.pf ppf "<offset=%a>" pp_expression e
  | Multiplier e -> Fmt.pf ppf "<multiplier=%a>" pp_expression e
  | OffsetMultiplier (e1, e2) ->
     Fmt.pf ppf "<offset=%a, multiplier=%a>" pp_expression e1 pp_expression e2
  | Ordered -> Fmt.pf ppf ""
  | PositiveOrdered -> Fmt.pf ppf ""
  | Simplex -> Fmt.pf ppf ""
  | UnitVector -> Fmt.pf ppf ""
  | CholeskyCorr -> Fmt.pf ppf ""
  | CholeskyCov -> Fmt.pf ppf ""
  | Correlation -> Fmt.pf ppf ""
  | Covariance -> Fmt.pf ppf ""

and pretty_print_transformation t = wrap_fmt pp_transformation t

and pp_transformed_type ppf (st, trans) =
  let unsizedtype_fmt, sizes_fmt =
    match st with
    | Middle.SInt -> (Fmt.const pp_unsizedtype UInt, Fmt.nop)
    | SReal -> (Fmt.const pp_unsizedtype UReal, Fmt.nop)
    | SVector e ->
        ( Fmt.const pp_unsizedtype UVector
        , Fmt.const (fun ppf -> Fmt.pf ppf "[%a]" pp_expression) e)
    | SRowVector e ->
        ( Fmt.const pp_unsizedtype URowVector
        , Fmt.const (fun ppf -> Fmt.pf ppf "[%a]" pp_expression) e)
    | SMatrix (e1, e2) ->
        ( Fmt.const pp_unsizedtype UMatrix
        , Fmt.const (fun ppf -> Fmt.pf ppf "[%a, %a]" pp_expression e1 pp_expression) e2)
    | SArray _ -> (
      match unwind_sized_array_type st with st, _ ->
        (Fmt.const pp_sizedtype st, Fmt.nop) )
  in
  let cov_sizes_fmt =
    match st with
    | SMatrix (e1, e2) ->
        if e1 = e2 then Fmt.const (fun ppf -> Fmt.pf ppf "[%a]" pp_expression) e1
        else
          Fmt.const (fun ppf -> Fmt.pf ppf "[%a, %a]" pp_expression e1 pp_expression) e2
    | _ -> Fmt.nop
  in
  match trans with
  | Identity -> pp_sizedtype ppf st
  | Lower _ | Upper _ | LowerUpper _ | Offset _ | Multiplier _
    |OffsetMultiplier _ -> Fmt.pf ppf "%a%a%a" unsizedtype_fmt () sizes_fmt () pp_transformation trans
  | Ordered -> Fmt.pf ppf "ordered%a" sizes_fmt ()
  | PositiveOrdered -> Fmt.pf ppf "positive_ordered%a" sizes_fmt ()
  | Simplex -> Fmt.pf ppf "simplex%a" sizes_fmt ()
  | UnitVector -> Fmt.pf ppf "unit_vector%a" sizes_fmt ()
  | CholeskyCorr -> Fmt.pf ppf "cholesky_factor_corr%a" cov_sizes_fmt ()
  | CholeskyCov -> Fmt.pf ppf "cholesky_factor_cov%a" cov_sizes_fmt ()
  | Correlation -> Fmt.pf ppf "corr_matrix%a" cov_sizes_fmt ()
  | Covariance -> Fmt.pf ppf "cov_matrix%a" cov_sizes_fmt ()

and pretty_print_transformed_type st trans = wrap_fmt pp_transformed_type (st, trans)

and pp_array_dims ppf = function
  | [] -> Fmt.pf ppf ""
  | es -> Fmt.pf ppf "[%a]" pp_list_of_expression (List.rev es)

and pretty_print_array_dims d = wrap_fmt pp_array_dims d

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
                   ^ " " ^ pretty_print_identifier id)
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
