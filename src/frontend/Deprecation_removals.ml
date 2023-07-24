(** Generate errors for expired deprecations

   Code in this module should generally only be hot for ONE version
   after the removal, then the errors will be moved to the
   parser/typechecker/etc as appropriate.
*)

open Core_kernel
open Core_kernel.Poly
open Middle
open Ast
open Deprecation_analysis

let pound_comment_usages : Location_span.t list ref = ref []
let old_array_usages : (Location_span.t * bool) list ref = ref []

let rec collect_removed_expr (acc : (Location_span.t * string) list)
    ({expr; emeta} : (typed_expr_meta, fun_kind) expr_with) :
    (Location_span.t * string) list =
  match expr with
  | GetLP ->
      acc
      @ [ ( emeta.loc
          , "The get_lp() function was removed in Stan 2.33.0. Use target() \
             instead. This can be done automatically with the canonicalize \
             flag for stanc" ) ]
  | FunApp (StanLib FnPlain, {name= "if_else"; _}, l) ->
      acc
      @ [ ( emeta.loc
          , "The if_else() function was removed in Stan 2.33.0. Use the \
             conditional operator (x ? y : z) instead; this can be \
             automatically changed using the canonicalize flag for stanc" ) ]
      @ List.concat_map l ~f:(fun e -> collect_removed_expr [] e)
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
        | _ when String.is_suffix name ~suffix:"_cdf" ->
            [ ( emeta.loc
              , "Use of " ^ name
                ^ " without a vertical bar (|) between the first two arguments \
                   of a CDF was removed in Stan 2.33.0. This can be \
                   automatically changed using the canonicalize flag for stanc"
              ) ]
        | _ -> [] in
      acc @ w @ List.concat_map l ~f:(fun e -> collect_removed_expr [] e)
  | _ -> fold_expression collect_removed_expr (fun l _ -> l) acc expr

let collect_removed_lval acc (l : Ast.typed_lval) =
  match l.lval with
  | LIndexed (l, _) ->
      let rec flatten = function
        | {lval= LVariable _; _} | {lval= LTupleProjection _; _} -> []
        | {lval= LIndexed (l, idxs); _} ->
            let flat = flatten l in
            flat @ idxs in
      if
        not
          (List.for_all
             ~f:(function
               | Single {emeta= {type_= UnsizedType.UInt; _}; _} -> true
               | _ -> false )
             (flatten l) )
      then
        acc
        @ [ ( l.lmeta.loc
            , "Nested multi-indexing on the left hand side of assignment does \
               not behave the same as nested indexing in expressions. This is \
               considered a bug and has been disallowed in Stan 2.33.0. The \
               indexing can be automatically fixed using the canonicalize flag \
               for stanc." ) ]
      else fold_lval_with collect_removed_expr (fun x _ -> x) acc l
  | _ -> fold_lval_with collect_removed_expr (fun x _ -> x) acc l

let rec collect_removed_stmt (acc : (Location_span.t * string) list)
    ({stmt; _} : Ast.typed_statement) : (Location_span.t * string) list =
  match stmt with
  | Assignment {assign_lhs; assign_op= ArrowAssign; assign_rhs} ->
      let acc =
        acc
        @ [ ( assign_lhs.lmeta.loc
            , "The arrow-style assignment operator '<-' was removed in Stan \
               2.33, use '=' instead. This can be done automatically with the \
               canonicalize flag for stanc" ) ] in
      collect_removed_lval [] assign_lhs @ collect_removed_expr acc assign_rhs
  | IncrementLogProb e ->
      let acc =
        acc
        @ [ ( e.emeta.loc
            , "The increment_log_prob(...); function was removed in Stan \
               2.33.0. Use target += ...; instead. This can be done \
               automatically with the canonicalize flag for stanc" ) ] in
      collect_removed_expr acc e
  | _ ->
      fold_statement collect_removed_expr collect_removed_stmt
        collect_removed_lval
        (fun l _ -> l)
        acc stmt

let collect_removals (program : typed_program) =
  let pounds =
    List.map !pound_comment_usages ~f:(fun loc ->
        ( loc
        , "Comments beginning with # were removed in Stan 2.33.0. Use // to \
           begin line comments; this can be done automatically using the \
           auto-format flag to stanc" ) ) in
  let arrs =
    List.map !old_array_usages ~f:(fun (loc, unsized) ->
        let placement = if unsized then "a type" else "a variable name" in
        ( loc
        , "Declaration of arrays by placing brackets after " ^ placement
          ^ " was removed in Stan 2.33.0. Instead use the array keyword before \
             the type. This can be changed automatically using the auto-format \
             flag to stanc" ) ) in
  fold_program collect_removed_stmt (pounds @ arrs) program

let pp ?printed_filename ppf (span, message) =
  let loc_str =
    if span = Location_span.empty then ""
    else " in " ^ Location.to_string ?printed_filename span.begin_loc in
  Fmt.pf ppf "@[<hov 4>Error%s: %a@]" loc_str Fmt.text message

let pp_removals ?printed_filename ppf removals =
  Fmt.(pf ppf "@[<v>%a@]@\n" (list ~sep:cut (pp ?printed_filename)) removals)
