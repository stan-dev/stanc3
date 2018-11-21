(** Some helpers to produce nice error messages and for auto-formatting Stan programs *)

(* TODO: this should be made tail recursive by accumulating list of strings in reverse *)

open Ast

let scope_depth = ref 1

let begin_scope _ = scope_depth := 1 + !scope_depth

let exit_scope _ = scope_depth := -1 + !scope_depth

let tabs () = String.make (2 * !scope_depth) ' '

let rec unwind_sized_array_type = function
  | SArray (st, e) -> (
    match unwind_sized_array_type st with st2, es -> (st2, e :: es) )
  | st -> (st, [])

let rec pretty_print_originblock = function Data | TData -> "data " | _ -> ""

and pretty_print_unsizedtype = function
  | Int -> "int"
  | Real -> "real"
  | Vector -> "vector"
  | RowVector -> "row_vector"
  | Matrix -> "matrix"
  | Array ut -> pretty_print_unsizedtype ut ^ "[]"
  | Fun (argtypes, rt) ->
      "("
      ^ String.concat ", " (List.map pretty_print_argtype argtypes)
      ^ ") => " ^ pretty_print_returntype rt
  | PrimitiveFunction -> "Stan Math function"

and pretty_print_argtype = function
  | ob, ut -> pretty_print_originblock ob ^ pretty_print_unsizedtype ut

and pretty_print_expressiontype = function
  | _, ut -> pretty_print_unsizedtype ut

and pretty_print_opt_expressiontype = function
  | None -> "unknown"
  | Some x -> pretty_print_expressiontype x

and pretty_print_returntype = function
  | ReturnType x -> pretty_print_unsizedtype x
  | Void -> "void"

and pretty_print_identifier id = id.name

and pretty_print_infixop = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Modulo -> "%"
  | LDivide -> "\\"
  | EltTimes -> ".*"
  | EltDivide -> "./"
  | Exp -> "^"
  | Or -> "||"
  | And -> "&&"
  | Equals -> "=="
  | NEquals -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

and pretty_print_prefixop = function
  | Not -> "!"
  | UMinus -> "-"
  | UPlus -> "+"

and pretty_print_postfixop = function Transpose -> "'"

and pretty_print_index = function
  | All -> " "
  | Single e -> pretty_print_expression e
  | Upfrom e -> pretty_print_expression e ^ " : "
  | Downfrom e -> " : " ^ pretty_print_expression e
  | Between (e1, e2) ->
      pretty_print_expression e1 ^ " : " ^ pretty_print_expression e2
  | Multiple e -> pretty_print_expression e

and pretty_print_list_of_indices l =
  String.concat ", " (List.map pretty_print_index l)

and pretty_print_expression = function
  | UntypedExpr (e_content, _) -> (
    match e_content with
    | Conditional (e1, e2, e3) ->
        pretty_print_expression e1 ^ " ? " ^ pretty_print_expression e2 ^ " : "
        ^ pretty_print_expression e3
    | InfixOp (e1, op, e2) ->
        pretty_print_expression e1 ^ " " ^ pretty_print_infixop op ^ " "
        ^ pretty_print_expression e2
    | PrefixOp (op, e) -> pretty_print_prefixop op ^ pretty_print_expression e
    | PostfixOp (e, op) ->
        pretty_print_expression e ^ pretty_print_postfixop op
    | Variable id -> pretty_print_identifier id
    | IntNumeral i -> i
    | RealNumeral r -> r
    | FunApp (id, es) ->
        pretty_print_identifier id ^ "("
        ^ pretty_print_list_of_expression es
        ^ ")"
    | CondFunApp (id, es) ->
        pretty_print_identifier id ^ "("
        ^ pretty_print_expression (List.hd es)
        ^ "| "
        ^ pretty_print_list_of_expression (List.tl es)
        ^ ")"
    | GetLP -> "get_lp()"
    (* deprecated *)
    | GetTarget -> "target()"
    | ArrayExpr es -> "{" ^ pretty_print_list_of_expression es ^ "}"
    | RowVectorExpr es -> "[" ^ pretty_print_list_of_expression es ^ "]"
    | Paren e -> "(" ^ pretty_print_expression e ^ ")"
    | Indexed (e, l) -> (
        pretty_print_expression e
        ^
        match l with
        | [] -> ""
        | l -> "[" ^ pretty_print_list_of_indices l ^ "]" ) )

and pretty_print_list_of_expression es =
  String.concat ", " (List.map pretty_print_expression es)

and pretty_print_assignmentoperator = function
  | Assign -> "="
  | PlusAssign -> "+="
  | MinusAssign -> "-="
  | TimesAssign -> "*="
  | DivideAssign -> "/="
  | EltTimesAssign -> ".*="
  | EltDivideAssign -> "./="
  | ArrowAssign -> "<-"

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
  String.concat ", " (List.map pretty_print_printable l)

and pretty_print_sizedtype = function
  | SInt -> "int"
  | SReal -> "real"
  | SVector e -> "vector[" ^ pretty_print_expression e ^ "]"
  | SRowVector e -> "row_vector[" ^ pretty_print_expression e ^ "]"
  | SMatrix (e1, e2) ->
      "matrix[" ^ pretty_print_expression e1 ^ ", "
      ^ pretty_print_expression e2 ^ "]"
  | SArray _ -> raise (Errors.FatalError "This should never happen.")

and pretty_print_transformation = function
  | Identity -> ""
  | Lower e -> "<lower=" ^ pretty_print_expression e ^ ">"
  | Upper e -> "<upper=" ^ pretty_print_expression e ^ ">"
  | LowerUpper (e1, e2) ->
      "<lower=" ^ pretty_print_expression e1 ^ ", upper="
      ^ pretty_print_expression e2 ^ ">"
  | LocationScale (e1, e2) -> (
    match (e1, e2) with
    | UntypedExpr (RealNumeral "0.", _), UntypedExpr (RealNumeral "1.", _) ->
        ""
    | UntypedExpr (RealNumeral "0.", _), _ ->
        "<scale=" ^ pretty_print_expression e2 ^ ">"
    | _, UntypedExpr (RealNumeral "1.", _) ->
        "<location=" ^ pretty_print_expression e2 ^ ">"
    | _ ->
        "<location=" ^ pretty_print_expression e1 ^ ", scale="
        ^ pretty_print_expression e2 ^ ">" )
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
    | SInt -> (pretty_print_unsizedtype Int, "")
    | SReal -> (pretty_print_unsizedtype Real, "")
    | SVector e ->
        (pretty_print_unsizedtype Vector, "[" ^ pretty_print_expression e ^ "]")
    | SRowVector e ->
        ( pretty_print_unsizedtype RowVector
        , "[" ^ pretty_print_expression e ^ "]" )
    | SMatrix (e1, e2) ->
        ( pretty_print_unsizedtype Matrix
        , "[" ^ pretty_print_expression e1 ^ ", " ^ pretty_print_expression e2
          ^ "]" )
    | SArray _ -> (
      match unwind_sized_array_type st with st, _ ->
        (pretty_print_sizedtype st, "") )
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
  | Lower _ | Upper _ | LowerUpper _ | LocationScale _ ->
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

and pretty_print_statement = function
  | UntypedStmt (s_content, _) -> (
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
    | NRFunApp (id, es) ->
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
    | Skip -> ""
    | IfThen (e, s) ->
        "if (" ^ pretty_print_expression e ^ ") " ^ pretty_print_statement s
    | IfThenElse (e, s1, s2) ->
        "if (" ^ pretty_print_expression e ^ ") " ^ pretty_print_statement s1
        ^ "\n" ^ tabs () ^ "else " ^ pretty_print_statement s2
    | While (e, s) ->
        "while (" ^ pretty_print_expression e ^ ") " ^ pretty_print_statement s
    | For {loop_variable= id; lower_bound= e1; upper_bound= e2; loop_body= s}
      ->
        "for (" ^ pretty_print_identifier id ^ " in "
        ^ pretty_print_expression e1 ^ " : " ^ pretty_print_expression e2
        ^ ") " ^ pretty_print_statement s
    | ForEach (id, e, s) ->
        "for (" ^ pretty_print_identifier id ^ " in "
        ^ pretty_print_expression e ^ ") " ^ pretty_print_statement s
    | Block vdsl ->
        let s1 = "{\n" in
        let _ = begin_scope () in
        let s2 = pretty_print_list_of_statements vdsl in
        let _ = exit_scope () in
        let s3 = tabs () ^ "}" in
        s1 ^ s2 ^ s3
    | VDecl (st, id) ->
        let st2, es = unwind_sized_array_type st in
        pretty_print_sizedtype st2 ^ " " ^ pretty_print_identifier id
        ^ pretty_print_array_dims es ^ ";"
    | VDeclAss {sizedtype= st; identifier= id; value= e} ->
        let st2, es = unwind_sized_array_type st in
        pretty_print_sizedtype st2 ^ " " ^ pretty_print_identifier id
        ^ pretty_print_array_dims es ^ " = " ^ pretty_print_expression e ^ ";"
    | TVDecl (st, trans, id) ->
        let st2, es = unwind_sized_array_type st in
        pretty_print_transformed_type st2 trans
        ^ " " ^ pretty_print_identifier id ^ pretty_print_array_dims es ^ ";"
    | TVDeclAss
        {tsizedtype= st; transformation= trans; tidentifier= id; tvalue= e} ->
        let st2, es = unwind_sized_array_type st in
        pretty_print_transformed_type st2 trans
        ^ " " ^ pretty_print_identifier id ^ pretty_print_array_dims es ^ " = "
        ^ pretty_print_expression e ^ ";"
    | FunDef {returntype= rt; funname= id; arguments= args; body= b} -> (
        pretty_print_returntype rt ^ " " ^ pretty_print_identifier id ^ "("
        ^ String.concat ", "
            (List.map
               (function
                 | ob, ut, id ->
                     pretty_print_originblock ob
                     ^ pretty_print_unsizedtype ut
                     ^ " " ^ pretty_print_identifier id)
               args)
        ^
        match b with
        | UntypedStmt (Skip, _) -> ");"
        | b -> ") " ^ pretty_print_statement b ) )

and pretty_print_list_of_statements l =
  String.concat "\n" (List.map (fun x -> tabs () ^ pretty_print_statement x) l)
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

(* TODO: implement more pretty printing functions for generating error messages;
   especially for listing function signatures *)
