(** Some helpers to produce nice error messages and for auto-formatting Stan programs *)

open Core_kernel
open Core_kernel.Poly
open Ast
open Fmt

(** To avoid cluttering the AST, comments are not associated with any particular AST node but instead come in a separate list.
   The pretty printer uses the AST nodes' location metadata to insert whitespace and comments.
   The comment list is stored in a global state that is accessed by set_comments, get_comments, and skip_comments.
 *)
let comments : comment_type list ref = ref []

let skipped = ref []

let set_comments ?(inline_includes = false) ls =
  let filtered =
    if inline_includes then
      List.filter ~f:(function Include _ -> false | _ -> true) ls
    else
      List.filter
        ~f:(fun x ->
          match x with
          | Include (_, {begin_loc= {included_from= Some _; _}; _})
           |LineComment (_, {begin_loc= {included_from= Some _; _}; _})
           |BlockComment (_, {begin_loc= {included_from= Some _; _}; _})
           |Separator {included_from= Some _; _} ->
              false
          | _ -> true )
        ls in
  comments := filtered

let get_comments end_loc =
  let rec go ls =
    match ls with
    | LineComment (s, ({Middle.Location_span.begin_loc; _} as loc)) :: tl
      when Middle.Location.compare begin_loc end_loc < 0 ->
        (`Line, [s], loc) :: go tl
    | Include (s, ({Middle.Location_span.begin_loc; _} as loc)) :: tl
      when Middle.Location.compare begin_loc end_loc < 0 ->
        (`Include, [s], loc) :: go tl
    | BlockComment (s, ({Middle.Location_span.begin_loc; _} as loc)) :: tl
      when Middle.Location.compare begin_loc end_loc < 0 ->
        (`Block, s, loc) :: go tl
    | Separator loc :: tl when Middle.Location.compare loc end_loc <= 0 -> go tl
    | _ ->
        comments := ls ;
        [] in
  go !comments

let get_comments_until_separator end_loc =
  let rec go ls =
    match ls with
    | LineComment (s, ({Middle.Location_span.begin_loc; _} as loc)) :: tl
      when Middle.Location.compare begin_loc end_loc < 0 ->
        (`Line, [s], loc) :: go tl
    | Include (s, ({Middle.Location_span.begin_loc; _} as loc)) :: tl
      when Middle.Location.compare begin_loc end_loc < 0 ->
        (`Include, [s], loc) :: go tl
    | BlockComment (s, ({Middle.Location_span.begin_loc; _} as loc)) :: tl
      when Middle.Location.compare begin_loc end_loc < 0 ->
        (`Block, s, loc) :: go tl
    | _ ->
        comments := ls ;
        [] in
  go !comments

let skip_comments loc =
  skipped :=
    !skipped
    @ List.filter_map (get_comments loc) ~f:(function
        | `Include, l, loc ->
            (* This prevents against bad behavior, but also really terrible but technically allowed things fail
               For example, an if statement where the 'else' is entirely inside the include.
               This makes the failure noisy rather than ever producing anything invalid for these. *)
            Common.FatalError.fatal_error_msg
              [%message
                "Unable to format #include in this position!"
                  (l : string list)
                  (loc : Middle.Location_span.t)]
        | x, s :: l, loc -> Some (x, (" ^^^:" ^ s) :: l, loc)
        | _, [], _ -> None )

let remaining_comments () =
  let x =
    !skipped
    @ List.filter_map !comments ~f:(function
        | LineComment (a, b) -> Some (`Line, [a], b)
        | Include (a, b) -> Some (`Include, [a], b)
        | BlockComment (a, b) -> Some (`Block, a, b)
        | Separator _ -> None ) in
  skipped := [] ;
  comments := [] ;
  x

(** If false, don't print any statements which came from another file *)
let print_included = ref false

(** Checks if something should be skipped based on the print_included setting *)
let should_skip (loc : Middle.Location.t) =
  (not !print_included) && Option.is_some loc.included_from

let pp_space newline ppf (prev_loc, begin_loc) =
  let open Middle.Location in
  if should_skip begin_loc then ()
  else if
    prev_loc.filename <> begin_loc.filename
    || prev_loc.line_num + 1 < begin_loc.line_num
  then pf ppf "@,@,"
  else if newline || prev_loc.line_num < begin_loc.line_num then pf ppf "@,"
  else pf ppf " "

let pp_comment ppf
    (style, lines, {Middle.Location_span.begin_loc= {col_num; _}; _}) =
  let trim init lines =
    let init = max init 0 in
    let padding =
      List.fold lines ~init ~f:(fun m x ->
          match String.lfindi ~f:(fun _ c -> c <> ' ') x with
          | None -> m
          | Some x -> min m x ) in
    List.map lines ~f:(fun x -> String.drop_prefix x padding) in
  let trim_tail col_num lines =
    match lines with [] -> [] | hd :: tl -> hd :: trim (col_num - 2) tl in
  match style with
  | `Block -> pf ppf "/*@[<v -2>%a@]*/" (list string) (trim_tail col_num lines)
  | `Line -> pf ppf "//%s" (List.hd_exn lines)
  | `Include -> pf ppf "@[#include %s@]" (List.hd_exn lines)

let pp_spacing ?(newline = true) prev_loc next_loc ppf ls =
  let newline =
    newline
    ||
    match List.last ls with
    | Some ((`Line | `Include), _, _) -> true
    | _ -> false in
  let rec recurse prev_loc = function
    | ((_, _, {Middle.Location_span.begin_loc; end_loc}) as hd) :: tl ->
        pp_space false ppf (prev_loc, begin_loc) ;
        pp_comment ppf hd ;
        recurse end_loc tl
    | [] -> prev_loc in
  let finish prev_loc =
    Option.iter next_loc ~f:(fun next_loc ->
        pp_space newline ppf (prev_loc, next_loc) ) in
  match ls with
  | ((_, _, {Middle.Location_span.begin_loc; end_loc}) as hd) :: tl ->
      Option.iter prev_loc ~f:(fun prev_loc ->
          pp_space false ppf (prev_loc, begin_loc) ) ;
      pp_comment ppf hd ;
      let (_ : Middle.Location.t) = recurse Middle.Location.empty !skipped in
      skipped := [] ;
      recurse end_loc tl |> finish
  | [] ->
      let (_ : Middle.Location.t) = recurse Middle.Location.empty !skipped in
      skipped := [] ;
      Option.iter prev_loc ~f:finish

let pp_comments_spacing space_before f ppf loc =
  let comments = f loc in
  if not (List.is_empty comments) then (
    if space_before then sp ppf () ;
    let rec go was_block = function
      | ((block, _, _) as comment) :: tl ->
          let is_block = match block with `Block -> true | _ -> false in
          pp_comment ppf comment ;
          if not is_block then Format.pp_force_newline ppf () ;
          go is_block tl
      | [] -> if was_block && not space_before then sp ppf () in
    go true comments )

let comma_no_break = any ", "

let indented_box ?(offset = 0) pp_v ppf v =
  pf ppf "@[<h>  %a@]" (box ~indent:offset pp_v) v

let pp_unsizedtype = Middle.UnsizedType.pp
let pp_autodifftype = Middle.UnsizedType.pp_autodifftype

let rec unwind_sized_array_type = function
  | Middle.SizedType.SArray (st, e) -> (
    match unwind_sized_array_type st with st2, es -> (st2, es @ [e]) )
  | st -> (st, [])

let pp_returntype ppf = function
  | Middle.UnsizedType.ReturnType x -> pp_unsizedtype ppf x
  | Void -> pf ppf "void"

let pp_identifier ppf id = string ppf id.name
let pp_operator = Middle.Operator.pp

let pp_list_of pp (loc_of : 'a -> Middle.Location_span.t) ppf
    (es, {Middle.Location_span.end_loc; begin_loc}) =
  let rec go expr more =
    match more with
    | next :: rest ->
        pp ppf expr ;
        let next_loc = (loc_of next).begin_loc in
        pp_comments_spacing true get_comments_until_separator ppf next_loc ;
        comma ppf () ;
        pp_comments_spacing false get_comments ppf next_loc ;
        go next rest
    | [] -> pp ppf expr in
  skip_comments begin_loc ;
  ( match es with
  | [] -> ()
  | e :: es ->
      pp_comments_spacing false get_comments ppf (loc_of e).begin_loc ;
      go e es ) ;
  pp_comments_spacing true get_comments ppf end_loc

let rec pp_index ppf = function
  | All -> pf ppf " : "
  | Single e -> pp_expression ppf e
  | Upfrom e -> pf ppf "%a : " pp_expression e
  | Downfrom e -> pf ppf " : %a" pp_expression e
  | Between (e1, e2) -> pf ppf "%a : %a" pp_expression e1 pp_expression e2

and pp_list_of_indices ppf l = (list ~sep:comma_no_break pp_index) ppf l

and pp_expression ppf ({expr= e_content; emeta= {loc; _}} : untyped_expression)
    =
  match e_content with
  | TernaryIf (e1, e2, e3) ->
      let then_loc = e2.emeta.loc.begin_loc in
      let else_loc = e3.emeta.loc.begin_loc in
      pf ppf "@[%a@ %a? %a%a@ %a: %a%a@]" pp_expression e1
        (pp_comments_spacing false get_comments_until_separator)
        then_loc
        (pp_comments_spacing false get_comments)
        then_loc pp_expression e2
        (pp_comments_spacing false get_comments_until_separator)
        else_loc
        (pp_comments_spacing false get_comments)
        else_loc pp_expression e3
  | BinOp (e1, op, e2) ->
      let next_loc = e2.emeta.loc.begin_loc in
      pf ppf "@[%a@ %a%a %a%a@]" pp_expression e1
        (pp_comments_spacing false get_comments_until_separator)
        next_loc pp_operator op
        (pp_comments_spacing false get_comments)
        next_loc pp_expression e2
  | PrefixOp (op, e) ->
      pf ppf "%a%a%a"
        (pp_comments_spacing false get_comments)
        e.emeta.loc.begin_loc pp_operator op pp_expression e
  | PostfixOp (e, op) -> pf ppf "%a%a" pp_expression e pp_operator op
  | Variable id -> pp_identifier ppf id
  | IntNumeral i -> string ppf i
  | RealNumeral r -> string ppf r
  | ImagNumeral z -> pf ppf "%si" z
  | FunApp (_, id, es) ->
      pf ppf "%a(@[%a)@]" pp_identifier id pp_list_of_expression (es, loc)
  | CondDistApp (_, id, es) -> (
    match es with
    | [] ->
        Common.FatalError.fatal_error_msg
          [%message "CondDistApp with no arguments: " id.name]
    | e :: es' ->
        let begin_loc =
          List.hd es'
          |> Option.map ~f:(fun e -> e.emeta.loc.begin_loc)
          |> Option.value ~default:loc.end_loc in
        pf ppf "@[<h>%a(%a%a | %a%a)@]" pp_identifier id pp_expression e
          (pp_comments_spacing true get_comments_until_separator)
          begin_loc
          (pp_comments_spacing false get_comments)
          begin_loc pp_list_of_expression (es', loc) )
  (* GetLP is deprecated *)
  | GetLP -> pf ppf "get_lp()"
  | GetTarget -> pf ppf "target()"
  | ArrayExpr es -> pf ppf "{@[%a}@]" pp_list_of_expression (es, loc)
  | RowVectorExpr es -> pf ppf "[@[%a]@]" pp_list_of_expression (es, loc)
  | Paren e -> pf ppf "(%a)" pp_expression e
  | Promotion (e, _, _) -> pp_expression ppf e
  | Indexed (e, l) -> (
    match l with
    | [] -> pf ppf "%a" pp_expression e
    | l -> pf ppf "%a[%a]" pp_expression e pp_list_of_indices l )

and pp_list_of_expression ppf es =
  let loc_of (x : untyped_expression) = x.emeta.loc in
  pp_list_of pp_expression loc_of ppf es

let pp_lvalue ppf lhs = pp_expression ppf (expr_of_lvalue lhs)

let pp_assignmentoperator ppf = function
  | Assign -> pf ppf "="
  (* ArrowAssign is deprecated *)
  | ArrowAssign -> pf ppf "<-"
  | OperatorAssign op -> pf ppf "%a=" pp_operator op

let pp_truncation ppf = function
  | NoTruncate -> ()
  | TruncateUpFrom e -> pf ppf " T[%a, ]" pp_expression e
  | TruncateDownFrom e -> pf ppf " T[ , %a]" pp_expression e
  | TruncateBetween (e1, e2) ->
      pf ppf " T[%a, %a]" pp_expression e1 pp_expression e2

let pp_printable ppf = function
  | PString s -> string ppf s
  | PExpr e -> pp_expression ppf e

let pp_list_of_printables ppf l = (hovbox @@ list ~sep:comma pp_printable) ppf l

let pp_sizedtype ppf = function
  | Middle.SizedType.SInt -> pf ppf "int"
  | SReal -> pf ppf "real"
  | SComplex -> pf ppf "complex"
  | SVector (_, e) -> pf ppf "vector[%a]" pp_expression e
  | SRowVector (_, e) -> pf ppf "row_vector[%a]" pp_expression e
  | SMatrix (_, e1, e2) ->
      pf ppf "matrix[%a, %a]" pp_expression e1 pp_expression e2
  | SArray _ ->
      Common.FatalError.fatal_error_msg [%message "Error printing array type"]

let pp_transformation ppf = function
  | Middle.Transformation.Lower e -> pf ppf "<@[lower=%a@]>" pp_expression e
  | Upper e -> pf ppf "<@[upper=%a@]>" pp_expression e
  | LowerUpper (e1, e2) ->
      pf ppf "<@[lower=%a,@ upper=%a@]>" pp_expression e1 pp_expression e2
  | Offset e -> pf ppf "<@[offset=%a@]>" pp_expression e
  | Multiplier e -> pf ppf "<@[multiplier=%a@]>" pp_expression e
  | OffsetMultiplier (e1, e2) ->
      pf ppf "<@[offset=%a,@ multiplier=%a@]>" pp_expression e1 pp_expression e2
  | Identity | Ordered | PositiveOrdered | Simplex | UnitVector | CholeskyCorr
   |CholeskyCov | Correlation | Covariance ->
      ()

let pp_transformed_type ppf (pst, trans) =
  let rec discard_arrays pst =
    match pst with
    | Middle.Type.Sized st ->
        Middle.Type.Sized (Fn.compose fst unwind_sized_array_type st)
    | Unsized (UArray t) -> discard_arrays (Unsized t)
    | Unsized ut -> Unsized ut in
  let pst = discard_arrays pst in
  let unsizedtype_fmt =
    match pst with
    | Middle.Type.Sized (SArray _ as st) ->
        const pp_sizedtype (Fn.compose fst unwind_sized_array_type st)
    | _ -> const pp_unsizedtype (Middle.Type.to_unsized pst) in
  let sizes_fmt =
    match pst with
    | Sized (SVector (_, e)) | Sized (SRowVector (_, e)) ->
        const (fun ppf -> pf ppf "[%a]" pp_expression) e
    | Sized (SMatrix (_, e1, e2)) ->
        const (fun ppf -> pf ppf "[%a, %a]" pp_expression e1 pp_expression) e2
    | Sized (SArray _)
     |Unsized _
     |Sized Middle.SizedType.SInt
     |Sized SReal
     |Sized SComplex ->
        nop in
  let cov_sizes_fmt =
    match pst with
    | Sized (SMatrix (_, e1, e2)) ->
        if e1 = e2 then const (fun ppf -> pf ppf "[%a]" pp_expression) e1
        else
          const (fun ppf -> pf ppf "[%a, %a]" pp_expression e1 pp_expression) e2
    | _ -> nop in
  match trans with
  | Middle.Transformation.Identity ->
      pf ppf "%a%a" unsizedtype_fmt () sizes_fmt ()
  | Lower _ | Upper _ | LowerUpper _ | Offset _ | Multiplier _
   |OffsetMultiplier _ ->
      pf ppf "%a%a%a" unsizedtype_fmt () pp_transformation trans sizes_fmt ()
  | Ordered -> pf ppf "ordered%a" sizes_fmt ()
  | PositiveOrdered -> pf ppf "positive_ordered%a" sizes_fmt ()
  | Simplex -> pf ppf "simplex%a" sizes_fmt ()
  | UnitVector -> pf ppf "unit_vector%a" sizes_fmt ()
  | CholeskyCorr -> pf ppf "cholesky_factor_corr%a" cov_sizes_fmt ()
  | CholeskyCov -> pf ppf "cholesky_factor_cov%a" cov_sizes_fmt ()
  | Correlation -> pf ppf "corr_matrix%a" cov_sizes_fmt ()
  | Covariance -> pf ppf "cov_matrix%a" cov_sizes_fmt ()

let pp_array_dims ppf = function
  | [] -> ()
  | es ->
      let ({emeta= {loc= {end_loc; _}; _}; _} : untyped_expression) =
        List.hd_exn es in
      let es = List.rev es in
      let ({emeta= {loc= {begin_loc; _}; _}; _} : untyped_expression) =
        List.hd_exn es in
      pf ppf "array[@[%a]@] " pp_list_of_expression (es, {begin_loc; end_loc})

let rec pp_indent_unless_block ppf ((s : untyped_statement), loc) =
  match s.stmt with
  | Block _ -> pp_statement ppf s
  | _ ->
      let begin_loc = s.smeta.loc.begin_loc in
      pp_spacing (Some loc) (Some begin_loc) ppf (get_comments begin_loc) ;
      (indented_box pp_statement) ppf s

(** This function helps write chained if-then-else-if-... blocks
 correctly. Without it, each IfThenElse would trigger a new
 vbox in front of the if, adding spaces for each level of IfThenElse.
 *)
and pp_recursive_ifthenelse ppf (s, loc) =
  match s.stmt with
  | IfThenElse (e, s, None) ->
      pf ppf "if (%a) %a" pp_expression e pp_indent_unless_block
        (s, e.emeta.loc.end_loc)
  | IfThenElse (e, s1, Some s2) ->
      pf ppf "if (%a) %a" pp_expression e pp_indent_unless_block
        (s1, e.emeta.loc.end_loc) ;
      let newline = match s1.stmt with Block _ -> false | _ -> true in
      let loc = s1.smeta.loc.end_loc in
      pp_spacing ~newline (Some loc) (Some loc) ppf
        (get_comments_until_separator s2.smeta.loc.begin_loc) ;
      pf ppf "else %a%a"
        (pp_comments_spacing false get_comments)
        s2.smeta.loc.begin_loc pp_recursive_ifthenelse
        (s2, {loc with line_num= loc.line_num + 1})
  | _ -> pp_indent_unless_block ppf (s, loc)

and pp_statement ppf ({stmt= s_content; smeta= {loc}} as ss : untyped_statement)
    =
  match s_content with
  | Assignment {assign_lhs= l; assign_op= assop; assign_rhs= e} ->
      pf ppf "@[<h>%a %a %a;@]" pp_lvalue l pp_assignmentoperator assop
        pp_expression e
  | NRFunApp (_, id, es) ->
      pf ppf "%a(@[%a);@]" pp_identifier id pp_list_of_expression (es, loc)
  | TargetPE e -> pf ppf "target += %a;" pp_expression e
  | IncrementLogProb e ->
      pf ppf "increment_log_prob(@[<hov>%a@]);" pp_expression e
  | Tilde {arg= e; distribution= id; args= es; truncation= t} ->
      pf ppf "%a ~ %a(@[%a)@]%a;" pp_expression e pp_identifier id
        pp_list_of_expression (es, loc) pp_truncation t
  | Break -> pf ppf "break;"
  | Continue -> pf ppf "continue;"
  | Return e -> pf ppf "return %a;" (hbox pp_expression) e
  | ReturnVoid -> pf ppf "return;"
  | Print ps -> pf ppf "print(%a);" pp_list_of_printables ps
  | Reject ps -> pf ppf "reject(%a);" pp_list_of_printables ps
  | Skip -> pf ppf ";"
  | IfThenElse (_, _, _) ->
      (vbox pp_recursive_ifthenelse) ppf (ss, ss.smeta.loc.begin_loc)
  | While (e, s) -> pf ppf "while (%a) %a" pp_expression e pp_statement s
  | For {loop_variable= id; lower_bound= e1; upper_bound= e2; loop_body= s} ->
      pf ppf "@[<v>for (%a in %a : %a) %a@]" pp_identifier id pp_expression e1
        pp_expression e2 pp_indent_unless_block (s, e2.emeta.loc.end_loc)
  | ForEach (id, e, s) ->
      pf ppf "for (%a in %a) %a" pp_identifier id pp_expression e
        pp_indent_unless_block (s, e.emeta.loc.end_loc)
  | Block vdsl ->
      pf ppf "{@,%a@,}" (indented_box pp_list_of_statements) (vdsl, loc)
  | Profile (name, vdsl) ->
      pf ppf "profile(%s) {@,%a@,}" name
        (indented_box pp_list_of_statements)
        (vdsl, loc)
  | VarDecl
      { decl_type= pst
      ; transformation= trans
      ; identifier= id
      ; initial_value= init
      ; is_global= _ } ->
      let pp_init ppf init =
        match init with None -> () | Some e -> pf ppf " = %a" pp_expression e
      in
      let es =
        match pst with
        | Sized st -> Fn.compose snd unwind_sized_array_type st
        | Unsized _ -> [] in
      pf ppf "@[<h>%a%a %a%a;@]" pp_array_dims es pp_transformed_type
        (pst, trans) pp_identifier id pp_init init
  | FunDef {returntype= rt; funname= id; arguments= args; body= b} -> (
      let loc_of (_, _, id) = id.id_loc in
      pf ppf "%a %a(%a" pp_returntype rt pp_identifier id
        (box (pp_list_of pp_args loc_of))
        (args, {loc with end_loc= b.smeta.loc.begin_loc}) ;
      match b with
      | {stmt= Skip; _} -> pf ppf ");"
      | b -> pf ppf ") %a" pp_statement b )

and pp_args ppf (at, ut, id) =
  pf ppf "%a%a %a" pp_autodifftype at pp_unsizedtype ut pp_identifier id

and pp_list_of_statements ppf (l, xloc) =
  let rec pp_head ppf ls =
    match ls with
    | ({smeta= ({loc= {end_loc; _}} : located_meta); _} as s) :: l ->
        let begin_loc = Ast.get_first_loc s in
        pp_spacing None (Some begin_loc) ppf (get_comments begin_loc) ;
        if not (should_skip begin_loc) then pp_statement ppf s ;
        pp_tail end_loc ppf l
    | [] -> pp_spacing None None ppf (get_comments xloc.end_loc)
  and pp_tail loc ppf ls =
    skip_comments loc ;
    match ls with
    | ({smeta= ({loc= {end_loc; _}} : located_meta); _} as s) :: l ->
        let begin_loc = Ast.get_first_loc s in
        pp_spacing (Some loc) (Some begin_loc) ppf (get_comments begin_loc) ;
        if not (should_skip begin_loc) then pp_statement ppf s ;
        pp_tail end_loc ppf l
    | [] -> pp_spacing (Some loc) None ppf (get_comments xloc.end_loc) in
  (vbox pp_head) ppf l

let pp_bare_block ppf {stmts; xloc} =
  (hbox (box pp_list_of_statements)) ppf (stmts, xloc)

let pp_block block_name ppf {stmts; xloc} =
  pf ppf "%s {@,%a@,}@," block_name
    (indented_box pp_list_of_statements)
    (stmts, xloc)

let rec pp_block_list ppf = function
  | (name, {stmts; xloc}) :: tl ->
      if should_skip xloc.end_loc then pp_block_list ppf tl
      else (
        pp_spacing None (Some xloc.begin_loc) ppf (get_comments xloc.begin_loc) ;
        pp_block name ppf {stmts; xloc} ;
        pp_block_list ppf tl )
  | [] -> pp_spacing None None ppf (remaining_comments ())

let pp_program ~bare_functions ~line_length ~inline_includes ppf
    { functionblock= bf
    ; datablock= bd
    ; transformeddatablock= btd
    ; parametersblock= bp
    ; transformedparametersblock= btp
    ; modelblock= bm
    ; generatedquantitiesblock= bgq
    ; comments } =
  Format.pp_set_margin ppf line_length ;
  set_comments ~inline_includes comments ;
  print_included := inline_includes ;
  Format.pp_open_vbox ppf 0 ;
  if bare_functions then pp_bare_block ppf @@ Option.value_exn bf
  else
    let blocks =
      List.filter_map
        ~f:(fun (name, block_opt) ->
          Option.map ~f:(fun b -> (name, b)) block_opt )
        [ ("functions", bf); ("data", bd); ("transformed data", btd)
        ; ("parameters", bp); ("transformed parameters", btp); ("model", bm)
        ; ("generated quantities", bgq) ] in
    pp_block_list ppf blocks

let check_correctness ?(bare_functions = false) prog pretty =
  let result_ast =
    try
      let res, (_ : Warnings.t list) =
        if bare_functions then
          Parse.parse_string Parser.Incremental.functions_only pretty
        else Parse.parse_string Parser.Incremental.program pretty in
      Option.value_exn (Result.ok res)
    with _ ->
      Common.FatalError.fatal_error_msg
        [%message
          "Pretty-printed program failed to parse"
            (prog : Ast.untyped_program)
            pretty] in
  if compare_untyped_program prog result_ast <> 0 then
    Common.FatalError.fatal_error_msg
      [%message
        "Pretty-printed program does match the original!"
          (prog : Ast.untyped_program)
          (result_ast : Ast.untyped_program)]

let pp_typed_expression ppf e =
  pp_expression ppf (untyped_expression_of_typed_expression e)

let pretty_print_program ?(bare_functions = false) ?(line_length = 78)
    ?(inline_includes = false) p =
  let result =
    str "%a" (pp_program ~bare_functions ~line_length ~inline_includes) p in
  check_correctness ~bare_functions p result ;
  result

let pretty_print_typed_program ?(bare_functions = false) ?(line_length = 78)
    ?(inline_includes = false) p =
  pretty_print_program ~bare_functions ~line_length ~inline_includes
    (untyped_program_of_typed_program p)
