(** Generate errors for expired deprecations

   Code in this module should generally only be hot for ONE version
   after the removal, then the errors will be moved to the
   parser/typechecker/etc as appropriate.
*)

open Core
open Core.Poly
open Middle
open Ast
open Deprecation_analysis

let rec collect_removed_expr (acc : (Location_span.t * string) list)
    ({expr; emeta} : (typed_expr_meta, fun_kind) expr_with) :
    (Location_span.t * string) list =
  match expr with
  | FunApp ((StanLib _ | UserDefined _), {name; _}, l) ->
      let w =
        match Map.find stan_lib_deprecations name with
        | Some (rename, (major, minor)) ->
            if expired (major, minor) then
              let version = string_of_int major ^ "." ^ string_of_int minor in
              [ ( emeta.loc
                , name ^ " was removed in Stan " ^ version ^ ". Use " ^ rename
                  ^ " instead. This can be automatically changed using the \
                     canonicalize flag for stanc" ) ]
            else []
        | _ -> [] in
      acc @ w @ List.concat_map l ~f:(fun e -> collect_removed_expr [] e)
  | PrefixOp (PNot, ({emeta= {type_= UReal; loc; _}; _} as e)) ->
      let acc =
        acc
        @ [ ( loc
            , "Using a real as a boolean value was disallowed in Stan 2.34. \
               Use an explicit != 0 comparison instead. This can be \
               automatically changed using the canonicalize flag for stanc" ) ]
      in
      collect_removed_expr acc e
  | BinOp (({emeta= {type_= UReal; loc; _}; _} as e1), (And | Or), e2)
   |BinOp (e1, (And | Or), ({emeta= {type_= UReal; loc; _}; _} as e2)) ->
      let acc =
        acc
        @ [ ( loc
            , "Using a real as a boolean value was disallowed in Stan 2.34. \
               Use an explicit != 0 comparison instead. This can be \
               automatically changed using the canonicalize flag for stanc" ) ]
      in
      let acc = collect_removed_expr acc e1 in
      let acc = collect_removed_expr acc e2 in
      acc
  | _ -> fold_expression collect_removed_expr (fun l _ -> l) acc expr

let collect_removed_lval acc : typed_lval -> _ = function
  | l -> fold_lval_with collect_removed_expr (fun x _ -> x) acc l

let rec collect_removed_lval_pack acc = function
  | LValue l -> collect_removed_lval acc l
  | LTuplePack {lvals; _} ->
      List.fold ~init:acc ~f:collect_removed_lval_pack lvals

let rec collect_removed_stmt (acc : (Location_span.t * string) list)
    ({stmt; _} : Ast.typed_statement) : (Location_span.t * string) list =
  match stmt with
  | IfThenElse ({emeta= {type_= UReal; loc; _}; _}, ifb, elseb) ->
      let acc =
        acc
        @ [ ( loc
            , "Condition of type real was disallowed in Stan 2.34. Use an \
               explicit != 0 comparison instead. This can be automatically \
               changed using the canonicalize flag for stanc" ) ] in
      let acc = collect_removed_stmt acc ifb in
      Option.value_map ~default:acc ~f:(collect_removed_stmt acc) elseb
  | While ({emeta= {type_= UReal; loc; _}; _}, body) ->
      let acc =
        acc
        @ [ ( loc
            , "Condition of type real was disallowed in Stan 2.34. Use an \
               explicit != 0 comparison instead. This can be automatically \
               changed using the canonicalize flag for stanc" ) ] in
      collect_removed_stmt acc body
  | _ ->
      fold_statement collect_removed_expr collect_removed_stmt
        collect_removed_lval
        (fun l _ -> l)
        acc stmt

let collect_removals (program : typed_program) =
  fold_program collect_removed_stmt [] program

let pp ?printed_filename ppf (span, message) =
  let loc_str =
    if span = Location_span.empty then ""
    else " in " ^ Location.to_string ?printed_filename span.begin_loc in
  Fmt.pf ppf "@[<hov 4>Error%s: %a@]" loc_str Fmt.text message

let pp_removals ?printed_filename ppf removals =
  Fmt.(pf ppf "@[<v>%a@]@\n" (list ~sep:cut (pp ?printed_filename)) removals)
