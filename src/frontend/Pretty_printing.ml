(** Some helpers to produce nice error messages and for auto-formatting Stan programs *)

open Core_kernel
open Ast

(** To avoid cluttering the AST, comments are not associated with any particular AST node but instead come in a separate list.
   The pretty printer uses the AST nodes' location metadata to insert whitespace and comments.
   The comment list is stored in a global state that is accessed by set_comments, get_comments, and skip_comments.
 *)
let comments : comment_type list ref = ref []

let skipped = ref []
let set_comments ls = comments := ls

let get_comments end_loc =
  let rec go ls =
    match ls with
    | LineComment (s, ({Middle.Location_span.begin_loc; _} as loc)) :: tl
      when Middle.Location.compare begin_loc end_loc < 0 ->
        (false, [s], loc) :: go tl
    | BlockComment (s, ({Middle.Location_span.begin_loc; _} as loc)) :: tl
      when Middle.Location.compare begin_loc end_loc < 0 ->
        (true, s, loc) :: go tl
    | Comma loc :: tl when Middle.Location.compare loc end_loc < 0 -> go tl
    | _ ->
        comments := ls ;
        []
  in
  go !comments

let get_comments_until_comma end_loc =
  let rec go ls =
    match ls with
    | LineComment (s, ({Middle.Location_span.begin_loc; _} as loc)) :: tl
      when Middle.Location.compare begin_loc end_loc < 0 ->
        (false, [s], loc) :: go tl
    | BlockComment (s, ({Middle.Location_span.begin_loc; _} as loc)) :: tl
      when Middle.Location.compare begin_loc end_loc < 0 ->
        (true, s, loc) :: go tl
    | _ ->
        comments := ls ;
        []
  in
  go !comments

let skip_comments loc =
  skipped :=
    !skipped
    @ List.filter_map (get_comments loc) ~f:(function
        | x, s :: l, loc -> Some (x, (" ^^^:" ^ s) :: l, loc)
        | _, [], _ -> None )

let remaining_comments () =
  let x =
    !skipped
    @ List.filter_map !comments ~f:(function
        | LineComment (a, b) -> Some (false, [a], b)
        | BlockComment (a, b) -> Some (true, a, b)
        | Comma _ -> None )
  in
  skipped := [] ;
  comments := [] ;
  x

let pp_space newline ppf (prev_loc, begin_loc) =
  let open Middle.Location in
  if
    prev_loc.filename <> begin_loc.filename
    || prev_loc.line_num + 1 < begin_loc.line_num
  then Fmt.pf ppf "@,@,"
  else if newline || prev_loc.line_num < begin_loc.line_num then
    Fmt.pf ppf "@,"
  else Fmt.pf ppf " "

let pp_comment ppf
    (is_block, lines, {Middle.Location_span.begin_loc= {col_num; _}; _}) =
  let trim init lines =
    let padding =
      List.fold lines ~init ~f:(fun m x ->
          match String.lfindi ~f:(fun _ c -> c <> ' ') x with
          | None -> m
          | Some x -> min m x )
    in
    List.map lines ~f:(fun x -> String.drop_prefix x padding)
  in
  let trim_tail col_num lines =
    match lines with [] -> [] | hd :: tl -> hd :: trim (col_num - 2) tl
  in
  if is_block then
    Fmt.pf ppf "@[<v>/*%a*/@]" Fmt.(list string) (trim_tail col_num lines)
  else Fmt.pf ppf "//%a" Fmt.string (List.hd_exn lines)

let pp_spacing ?(newline = true) prev_loc next_loc ppf ls =
  let newline =
    newline
    || match List.last ls with Some (false, _, _) -> true | _ -> false
  in
  let rec recurse prev_loc = function
    | ((_, _, {Middle.Location_span.begin_loc; end_loc}) as hd) :: tl ->
        pp_space false ppf (prev_loc, begin_loc) ;
        pp_comment ppf hd ;
        recurse end_loc tl
    | [] -> prev_loc
  in
  let finish prev_loc =
    Option.iter next_loc ~f:(fun next_loc ->
        pp_space newline ppf (prev_loc, next_loc) )
  in
  match ls with
  | ((_, _, {Middle.Location_span.begin_loc; end_loc}) as hd) :: tl ->
      Option.iter prev_loc ~f:(fun prev_loc ->
          pp_space false ppf (prev_loc, begin_loc) ) ;
      pp_comment ppf hd ;
      let _ = recurse Middle.Location.empty !skipped in
      skipped := [] ;
      recurse end_loc tl |> finish
  | [] ->
      let _ = recurse Middle.Location.empty !skipped in
      skipped := [] ;
      Option.iter prev_loc ~f:finish

let wrap_fmt fmt x =
  (* Switched from Format.str_formatter partially because of
     https://discuss.ocaml.org/t/debugging-memory-issues/3223/8
  *)
  Fmt.strf "%a" fmt x

let with_hbox ppf f =
  Format.pp_open_hbox ppf () ; f () ; Format.pp_close_box ppf () ; ()

let with_box ppf offset f =
  Format.pp_open_box ppf offset ;
  f () ;
  Format.pp_close_box ppf () ;
  ()

let with_vbox ppf offset f =
  Format.pp_open_vbox ppf offset ;
  f () ;
  Format.pp_close_box ppf () ;
  ()

let comma_no_break = Fmt.unit ", "

let with_indented_box ppf indentation offset f =
  let rec pp_print_n_spaces ppf = function
    | 0 -> ()
    | i ->
        Format.pp_print_space ppf () ;
        pp_print_n_spaces ppf (i - 1)
  in
  with_hbox ppf (fun () ->
      pp_print_n_spaces ppf indentation ;
      with_box ppf offset f ) ;
  ()

let pp_unsizedtype = Middle.UnsizedType.pp
let pp_autodifftype = Middle.UnsizedType.pp_autodifftype

let rec unwind_sized_array_type = function
  | Middle.SizedType.SArray (st, e) -> (
    match unwind_sized_array_type st with st2, es -> (st2, es @ [e]) )
  | st -> (st, [])

let pp_unsizedtypes ppf l = Fmt.(list ~sep:comma_no_break pp_unsizedtype) ppf l

let pp_argtype ppf = function
  | at, ut -> Fmt.pair ~sep:Fmt.nop pp_autodifftype pp_unsizedtype ppf (at, ut)

let pp_returntype ppf = function
  | Middle.UnsizedType.ReturnType x -> pp_unsizedtype ppf x
  | Void -> Fmt.pf ppf "void"

let pp_identifier ppf id = Fmt.pf ppf "%s" id.name

let pp_operator ppf = function
  | Middle.Operator.Plus | PPlus -> Fmt.pf ppf "+"
  | Minus | PMinus -> Fmt.pf ppf "-"
  | Times -> Fmt.pf ppf "*"
  | Divide -> Fmt.pf ppf "/"
  | Modulo -> Fmt.pf ppf "%%"
  | IntDivide -> Fmt.pf ppf "%%/%%"
  | LDivide -> Fmt.pf ppf "\\"
  | EltTimes -> Fmt.pf ppf ".*"
  | EltDivide -> Fmt.pf ppf "./"
  | Pow -> Fmt.pf ppf "^"
  | EltPow -> Fmt.pf ppf ".^"
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

let pp_list_of pp (loc_of : 'a -> Middle.Location_span.t) ppf
    (es, {Middle.Location_span.begin_loc; end_loc}) =
  let pp_maybe_space space_before comments =
    if not (List.is_empty comments) then (
      if space_before then Fmt.sp ppf () ;
      let rec go was_block = function
        | ((is_block, _, _) as comment) :: tl ->
            pp_comment ppf comment ;
            if not is_block then Format.pp_force_newline ppf () ;
            go is_block tl
        | [] -> if was_block && not space_before then Fmt.sp ppf ()
      in
      go true comments )
  in
  let rec go expr more =
    match more with
    | next :: rest ->
        pp ppf expr ;
        let next_loc = (loc_of next).begin_loc in
        pp_maybe_space true (get_comments_until_comma next_loc) ;
        let comments = get_comments next_loc in
        Fmt.comma ppf () ;
        pp_maybe_space false comments ;
        go next rest
    | [] -> pp ppf expr
  in
  skip_comments begin_loc ;
  ( match es with
  | [] -> ()
  | e :: es ->
      pp_maybe_space false (get_comments (loc_of e).begin_loc) ;
      go e es ) ;
  pp_maybe_space true (get_comments end_loc)

let rec pp_index ppf = function
  | All -> Fmt.pf ppf " : "
  | Single e -> pp_expression ppf e
  | Upfrom e -> Fmt.pf ppf "%a : " pp_expression e
  | Downfrom e -> Fmt.pf ppf " : %a" pp_expression e
  | Between (e1, e2) -> Fmt.pf ppf "%a : %a" pp_expression e1 pp_expression e2

and pp_list_of_indices ppf l =
  Fmt.(list ~sep:comma_no_break pp_index) ppf l ;
  ()

and pp_expression ppf ({expr= e_content; emeta= {loc; _}} : untyped_expression)
    =
  match e_content with
  | TernaryIf (e1, e2, e3) ->
      with_box ppf 0 (fun () ->
          Fmt.pf ppf "%a" pp_expression e1 ;
          Format.pp_print_space ppf () ;
          Fmt.pf ppf "? %a" pp_expression e2 ;
          Format.pp_print_space ppf () ;
          Fmt.pf ppf ": %a" pp_expression e3 )
  | BinOp (e1, op, e2) ->
      with_box ppf 0 (fun () ->
          Fmt.pf ppf "%a" pp_expression e1 ;
          Format.pp_print_space ppf () ;
          Fmt.pf ppf "%a %a" pp_operator op pp_expression e2 )
  | PrefixOp (op, e) -> Fmt.pf ppf "%a%a" pp_operator op pp_expression e
  | PostfixOp (e, op) -> Fmt.pf ppf "%a%a" pp_expression e pp_operator op
  | Variable id -> pp_identifier ppf id
  | IntNumeral i -> Fmt.pf ppf "%s" i
  | RealNumeral r -> Fmt.pf ppf "%s" r
  | ImagNumeral z -> Fmt.pf ppf "%si" z
  | FunApp (_, id, es) ->
      Fmt.pf ppf "%a(" pp_identifier id ;
      with_box ppf 0 (fun () -> Fmt.pf ppf "%a)" pp_list_of_expression (es, loc)
      )
  | CondDistApp (_, id, es) -> (
    match es with
    | [] -> Errors.fatal_error ()
    | e :: es' ->
        with_hbox ppf (fun () ->
            Fmt.pf ppf "%a(%a | %a)" pp_identifier id pp_expression e
              pp_list_of_expression (es', loc) ) )
  (* GetLP is deprecated *)
  | GetLP -> Fmt.pf ppf "get_lp()"
  | GetTarget -> Fmt.pf ppf "target()"
  | ArrayExpr es ->
      Fmt.pf ppf "{" ;
      with_box ppf 0 (fun () -> Fmt.pf ppf "%a}" pp_list_of_expression (es, loc)
      )
  | RowVectorExpr es ->
      Fmt.pf ppf "[" ;
      with_box ppf 0 (fun () -> Fmt.pf ppf "%a]" pp_list_of_expression (es, loc)
      )
  | Paren e -> Fmt.pf ppf "(%a)" pp_expression e
  | Indexed (e, l) -> (
    match l with
    | [] -> Fmt.pf ppf "%a" pp_expression e
    | l -> Fmt.pf ppf "%a[%a]" pp_expression e pp_list_of_indices l )

and pp_list_of_expression ppf es =
  let loc_of (x : untyped_expression) = x.emeta.loc in
  pp_list_of pp_expression loc_of ppf es

let pp_lvalue ppf lhs = pp_expression ppf (expr_of_lvalue lhs)

let pp_assignmentoperator ppf = function
  | Assign -> Fmt.pf ppf "="
  (* ArrowAssign is deprecated *)
  | ArrowAssign -> Fmt.pf ppf "<-"
  | OperatorAssign op -> Fmt.pf ppf "%a=" pp_operator op

let pretty_print_assignmentoperator op = wrap_fmt pp_assignmentoperator op

let pp_truncation ppf = function
  | NoTruncate -> Fmt.pf ppf ""
  | TruncateUpFrom e -> Fmt.pf ppf " T[%a, ]" pp_expression e
  | TruncateDownFrom e -> Fmt.pf ppf " T[ , %a]" pp_expression e
  | TruncateBetween (e1, e2) ->
      Fmt.pf ppf " T[%a, %a]" pp_expression e1 pp_expression e2

let pp_printable ppf = function
  | PString s -> Fmt.pf ppf "%s" s
  | PExpr e -> pp_expression ppf e

let pp_list_of_printables ppf l =
  Fmt.(list ~sep:comma_no_break pp_printable) ppf l

let pp_sizedtype ppf = function
  | Middle.SizedType.SInt -> Fmt.pf ppf "int"
  | SReal -> Fmt.pf ppf "real"
  | SComplex -> Fmt.pf ppf "complex"
  | SVector (_, e) -> Fmt.pf ppf "vector[%a]" pp_expression e
  | SRowVector (_, e) -> Fmt.pf ppf "row_vector[%a]" pp_expression e
  | SMatrix (_, e1, e2) ->
      Fmt.pf ppf "matrix[%a, %a]" pp_expression e1 pp_expression e2
  | SArray _ -> raise (Errors.FatalError "This should never happen.")

let pp_transformation ppf = function
  | Middle.Transformation.Identity -> Fmt.pf ppf ""
  | Lower e -> Fmt.pf ppf "<@[lower=%a@]>" pp_expression e
  | Upper e -> Fmt.pf ppf "<@[upper=%a@]>" pp_expression e
  | LowerUpper (e1, e2) ->
      Fmt.pf ppf "<@[lower=%a,@ upper=%a@]>" pp_expression e1 pp_expression e2
  | Offset e -> Fmt.pf ppf "<@[offset=%a@]>" pp_expression e
  | Multiplier e -> Fmt.pf ppf "<@[multiplier=%a@]>" pp_expression e
  | OffsetMultiplier (e1, e2) ->
      Fmt.pf ppf "<@[offset=%a,@ multiplier=%a@]>" pp_expression e1
        pp_expression e2
  | Ordered -> Fmt.pf ppf ""
  | PositiveOrdered -> Fmt.pf ppf ""
  | Simplex -> Fmt.pf ppf ""
  | UnitVector -> Fmt.pf ppf ""
  | CholeskyCorr -> Fmt.pf ppf ""
  | CholeskyCov -> Fmt.pf ppf ""
  | Correlation -> Fmt.pf ppf ""
  | Covariance -> Fmt.pf ppf ""

let pp_transformed_type ppf (pst, trans) =
  let rec discard_arrays pst =
    match pst with
    | Middle.Type.Sized st ->
        Middle.Type.Sized (Fn.compose fst unwind_sized_array_type st)
    | Unsized (UArray t) -> discard_arrays (Unsized t)
    | Unsized ut -> Unsized ut
  in
  let pst = discard_arrays pst in
  let unsizedtype_fmt =
    match pst with
    | Middle.Type.Sized (SArray _ as st) ->
        Fmt.const pp_sizedtype (Fn.compose fst unwind_sized_array_type st)
    | _ -> Fmt.const pp_unsizedtype (Middle.Type.to_unsized pst)
  in
  let sizes_fmt =
    match pst with
    | Sized (SVector (_, e)) | Sized (SRowVector (_, e)) ->
        Fmt.const (fun ppf -> Fmt.pf ppf "[%a]" pp_expression) e
    | Sized (SMatrix (_, e1, e2)) ->
        Fmt.const
          (fun ppf -> Fmt.pf ppf "[%a, %a]" pp_expression e1 pp_expression)
          e2
    | Sized (SArray _)
     |Unsized _
     |Sized Middle.SizedType.SInt
     |Sized SReal
     |Sized SComplex ->
        Fmt.nop
  in
  let cov_sizes_fmt =
    match pst with
    | Sized (SMatrix (_, e1, e2)) ->
        if e1 = e2 then
          Fmt.const (fun ppf -> Fmt.pf ppf "[%a]" pp_expression) e1
        else
          Fmt.const
            (fun ppf -> Fmt.pf ppf "[%a, %a]" pp_expression e1 pp_expression)
            e2
    | _ -> Fmt.nop
  in
  match trans with
  | Middle.Transformation.Identity ->
      Fmt.pf ppf "%a%a" unsizedtype_fmt () sizes_fmt ()
  | Lower _ | Upper _ | LowerUpper _ | Offset _ | Multiplier _
   |OffsetMultiplier _ ->
      Fmt.pf ppf "%a%a%a" unsizedtype_fmt () pp_transformation trans sizes_fmt
        ()
  | Ordered -> Fmt.pf ppf "ordered%a" sizes_fmt ()
  | PositiveOrdered -> Fmt.pf ppf "positive_ordered%a" sizes_fmt ()
  | Simplex -> Fmt.pf ppf "simplex%a" sizes_fmt ()
  | UnitVector -> Fmt.pf ppf "unit_vector%a" sizes_fmt ()
  | CholeskyCorr -> Fmt.pf ppf "cholesky_factor_corr%a" cov_sizes_fmt ()
  | CholeskyCov -> Fmt.pf ppf "cholesky_factor_cov%a" cov_sizes_fmt ()
  | Correlation -> Fmt.pf ppf "corr_matrix%a" cov_sizes_fmt ()
  | Covariance -> Fmt.pf ppf "cov_matrix%a" cov_sizes_fmt ()

let pp_array_dims ppf = function
  | [] -> Fmt.pf ppf ""
  | es ->
      Fmt.pf ppf "array[" ;
      with_box ppf 0 (fun () ->
          let ({emeta= {loc= {end_loc; _}; _}; _} : untyped_expression) =
            List.hd_exn es
          in
          let es = List.rev es in
          let ({emeta= {loc= {begin_loc; _}; _}; _} : untyped_expression) =
            List.hd_exn es
          in
          Fmt.pf ppf "%a] " pp_list_of_expression (es, {begin_loc; end_loc}) )

let rec pp_indent_unless_block ppf ((s : untyped_statement), loc) =
  match s.stmt with
  | Block _ -> pp_statement ppf s
  | _ ->
      pp_spacing (Some loc) (Some s.smeta.loc.begin_loc) ppf
        (get_comments s.smeta.loc.begin_loc) ;
      with_indented_box ppf 2 0 (fun () -> Fmt.pf ppf "%a" pp_statement s)

(** This function helps write chained if-then-else-if-... blocks
 correctly. Without it, each IfThenElse would trigger a new
 vbox in front of the if, adding spaces for each level of IfThenElse.
 *)
and pp_recursive_ifthenelse ppf (s, loc) =
  match s.stmt with
  | IfThenElse (e, s, None) ->
      Fmt.pf ppf "if (%a) %a" pp_expression e pp_indent_unless_block
        (s, e.emeta.loc.end_loc)
  | IfThenElse (e, s1, Some s2) ->
      Fmt.pf ppf "if (%a) %a" pp_expression e pp_indent_unless_block
        (s1, e.emeta.loc.end_loc) ;
      let newline = match s1.stmt with Block _ -> false | _ -> true in
      let loc = s1.smeta.loc.end_loc in
      pp_spacing ~newline (Some loc) (Some loc) ppf
        (get_comments s2.smeta.loc.begin_loc) ;
      Fmt.pf ppf "else %a" pp_recursive_ifthenelse
        (s2, {loc with line_num= loc.line_num + 1})
  | _ -> pp_indent_unless_block ppf (s, loc)

and pp_statement ppf
    ({stmt= s_content; smeta= {loc}} as ss : untyped_statement) =
  match s_content with
  | Assignment {assign_lhs= l; assign_op= assop; assign_rhs= e} ->
      with_hbox ppf (fun () ->
          Fmt.pf ppf "%a %a %a;" pp_lvalue l pp_assignmentoperator assop
            pp_expression e )
  | NRFunApp (_, id, es) ->
      Fmt.pf ppf "%a(" pp_identifier id ;
      with_box ppf 0 (fun () ->
          Fmt.pf ppf "%a);" pp_list_of_expression (es, loc) )
  | TargetPE e -> Fmt.pf ppf "target += %a;" pp_expression e
  | IncrementLogProb e ->
      with_hbox ppf (fun () ->
          Fmt.pf ppf "increment_log_prob(%a);" pp_expression e )
  | Tilde {arg= e; distribution= id; args= es; truncation= t} ->
      Fmt.pf ppf "%a ~ %a(" pp_expression e pp_identifier id ;
      with_box ppf 0 (fun () -> Fmt.pf ppf "%a)" pp_list_of_expression (es, loc)
      ) ;
      Fmt.pf ppf "%a;" pp_truncation t
  | Break -> Fmt.pf ppf "break;"
  | Continue -> Fmt.pf ppf "continue;"
  | Return e ->
      with_hbox ppf (fun () -> Fmt.pf ppf "return %a;" pp_expression e)
  | ReturnVoid -> Fmt.pf ppf "return;"
  | Print ps -> Fmt.pf ppf "print(%a);" pp_list_of_printables ps
  | Reject ps -> Fmt.pf ppf "reject(%a);" pp_list_of_printables ps
  | Skip -> Fmt.pf ppf ";"
  | IfThenElse (_, _, _) ->
      with_vbox ppf 0 (fun () ->
          pp_recursive_ifthenelse ppf (ss, ss.smeta.loc.begin_loc) )
  | While (e, s) -> Fmt.pf ppf "while (%a) %a" pp_expression e pp_statement s
  | For {loop_variable= id; lower_bound= e1; upper_bound= e2; loop_body= s} ->
      with_vbox ppf 0 (fun () ->
          Fmt.pf ppf "for (%a in %a : %a) %a" pp_identifier id pp_expression e1
            pp_expression e2 pp_indent_unless_block (s, e2.emeta.loc.end_loc)
      )
  | ForEach (id, e, s) ->
      Fmt.pf ppf "for (%a in %a) %a" pp_identifier id pp_expression e
        pp_indent_unless_block (s, e.emeta.loc.end_loc)
  | Block vdsl ->
      Fmt.pf ppf "{" ;
      Format.pp_print_cut ppf () ;
      with_indented_box ppf 2 0 (fun () -> pp_list_of_statements ppf (vdsl, loc)
      ) ;
      Format.pp_print_cut ppf () ;
      Fmt.pf ppf "}"
  | Profile (name, vdsl) ->
      Fmt.pf ppf "profile(%s) {" name ;
      Format.pp_print_cut ppf () ;
      with_indented_box ppf 2 0 (fun () -> pp_list_of_statements ppf (vdsl, loc)
      ) ;
      Format.pp_print_cut ppf () ;
      Fmt.pf ppf "}"
  | VarDecl
      { decl_type= pst
      ; transformation= trans
      ; identifier= id
      ; initial_value= init
      ; is_global= _ } ->
      let pp_init ppf init =
        match init with
        | None -> Fmt.pf ppf ""
        | Some e -> Fmt.pf ppf " = %a" pp_expression e
      in
      let es =
        match pst with
        | Sized st -> Fn.compose snd unwind_sized_array_type st
        | Unsized _ -> []
      in
      with_hbox ppf (fun () ->
          Fmt.pf ppf "%a%a %a%a;" pp_array_dims es pp_transformed_type
            (pst, trans) pp_identifier id pp_init init )
  | FunDef {returntype= rt; funname= id; arguments= args; body= b} -> (
      Fmt.pf ppf "%a %a(" pp_returntype rt pp_identifier id ;
      let loc_of (_, _, id) = id.id_loc in
      with_box ppf 0 (fun () ->
          Fmt.pf ppf "%a"
            (pp_list_of pp_args loc_of)
            (args, {loc with end_loc= b.smeta.loc.begin_loc}) ) ;
      match b with
      | {stmt= Skip; _} -> Fmt.pf ppf ");"
      | b -> Fmt.pf ppf ") %a" pp_statement b )

and pp_args ppf (at, ut, id) =
  Fmt.pf ppf "%a%a %a" pp_autodifftype at pp_unsizedtype ut pp_identifier id

and pp_list_of_statements ppf (l, xloc) =
  let rec pp_head ppf ls =
    match ls with
    | ({smeta= ({loc= {end_loc; _}} : located_meta); _} as s) :: l ->
        let begin_loc = Ast.get_first_loc s in
        pp_spacing None (Some begin_loc) ppf (get_comments begin_loc) ;
        pp_statement ppf s ;
        pp_tail end_loc ppf l
    | [] -> pp_spacing None None ppf (get_comments xloc.end_loc)
  and pp_tail loc ppf ls =
    skip_comments loc ;
    match ls with
    | ({smeta= ({loc= {end_loc; _}} : located_meta); _} as s) :: l ->
        let begin_loc = Ast.get_first_loc s in
        pp_spacing (Some loc) (Some begin_loc) ppf (get_comments begin_loc) ;
        pp_statement ppf s ;
        pp_tail end_loc ppf l
    | [] -> pp_spacing (Some loc) None ppf (get_comments xloc.end_loc)
  in
  with_vbox ppf 0 (fun () -> pp_head ppf l)

let pp_bare_block ppf {stmts; xloc} =
  pp_spacing None (Some xloc.begin_loc) ppf (get_comments xloc.begin_loc) ;
  with_indented_box ppf 0 0 (fun () ->
      Format.pp_print_cut ppf () ;
      pp_list_of_statements ppf (stmts, xloc) ;
      Format.pp_print_cut ppf () )

let pp_block block_name ppf {stmts; xloc} =
  Fmt.pf ppf "%s {" block_name ;
  Format.pp_print_cut ppf () ;
  with_indented_box ppf 2 0 (fun () -> pp_list_of_statements ppf (stmts, xloc)) ;
  Format.pp_print_cut ppf () ;
  Fmt.pf ppf "}" ;
  Format.pp_print_cut ppf ()

let rec pp_block_list ppf = function
  | (name, {stmts; xloc}) :: tl ->
      pp_spacing None (Some xloc.begin_loc) ppf (get_comments xloc.begin_loc) ;
      pp_block name ppf {stmts; xloc} ;
      pp_block_list ppf tl
  | [] -> pp_spacing None None ppf (remaining_comments ())

let pp_program ?(bare_functions = false) ppf
    { functionblock= bf
    ; datablock= bd
    ; transformeddatablock= btd
    ; parametersblock= bp
    ; transformedparametersblock= btp
    ; modelblock= bm
    ; generatedquantitiesblock= bgq
    ; comments } =
  set_comments comments ;
  Format.pp_open_vbox ppf 0 ;
  if bare_functions then pp_bare_block ppf @@ Option.value_exn bf
  else
    let blocks =
      List.filter_map
        ~f:(fun (name, block_opt) ->
          Option.map ~f:(fun b -> (name, b)) block_opt )
        [ ("functions", bf); ("data", bd); ("transformed data", btd)
        ; ("parameters", bp)
        ; ("transformed parameters", btp)
        ; ("model", bm)
        ; ("generated quantities", bgq) ]
    in
    pp_block_list ppf blocks

let check_correctness ?(bare_functions = false) prog pretty =
  let result_ast, (_ : Warnings.t list) =
    if bare_functions then
      Parse.parse_string Parser.Incremental.functions_only pretty
    else Parse.parse_string Parser.Incremental.program pretty
  in
  if
    compare_untyped_program prog (Option.value_exn (Result.ok result_ast)) <> 0
  then failwith "Pretty printing failed. Please file a bug."

let pp_typed_expression ppf e =
  pp_expression ppf (untyped_expression_of_typed_expression e)

let pretty_print_program ?(bare_functions = false) p =
  let result = wrap_fmt (pp_program ~bare_functions) p in
  check_correctness ~bare_functions p result ;
  result

let pretty_print_typed_program ?(bare_functions = false) p =
  let p = untyped_program_of_typed_program p in
  let result = wrap_fmt (pp_program ~bare_functions) p in
  check_correctness ~bare_functions p result ;
  result
