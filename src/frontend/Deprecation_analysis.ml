open Core_kernel
open Ast
open Middle

module type DEPRECATION_ANALYZER = sig
  val find_udf_log_suffix :
    typed_statement -> (string * Middle.UnsizedType.t) option

  val update_suffix : string -> Middle.UnsizedType.t -> string

  val collect_userdef_distributions :
    typed_program -> Middle.UnsizedType.t String.Map.t

  val without_suffix : string list -> string -> string
  val is_deprecated_distribution : string -> bool
  val rename_deprecated_distribution : string -> string
  val rename_deprecated_function : string -> string
  val userdef_distributions : untyped_statement block option -> string list
  val collect_warnings : typed_program -> Warnings.t list
end

let userdef_functions program =
  match program.functionblock with
  | None -> []
  | Some {stmts; _} ->
      List.filter_map stmts ~f:(function
        | {stmt= FunDef {body= {stmt= Skip; _}; _}; _} -> None
        | {stmt= FunDef {funname; arguments; _}; _} ->
            Some (funname.name, Ast.type_of_arguments arguments)
        | _ -> None )

let is_redundant_forwarddecl fundefs funname arguments =
  let equal (id1, a1) (id2, a2) =
    String.equal id1 id2 && UnsizedType.equal_argumentlist a1 a2 in
  List.mem ~equal fundefs (funname.name, Ast.type_of_arguments arguments)

let remove_unneeded_forward_decls program =
  let fundefs = userdef_functions program in
  let drop_forwarddecl = function
    | {stmt= FunDef {body= {stmt= Skip; _}; funname; arguments; _}; _}
      when is_redundant_forwarddecl fundefs funname arguments ->
        false
    | _ -> true in
  { program with
    functionblock=
      Option.map program.functionblock ~f:(fun x ->
          {x with stmts= List.filter ~f:drop_forwarddecl x.stmts} ) }

module Make (StdLibrary : Std_library_utils.Library) : DEPRECATION_ANALYZER =
struct
  let stan_lib_deprecations =
    Map.merge_skewed StdLibrary.deprecated_distributions
      StdLibrary.deprecated_functions ~combine:(fun ~key x y ->
        Common.FatalError.fatal_error_msg
          [%message
            "Common key in deprecation map"
              (key : string)
              (x : Std_library_utils.deprecation_info)
              (y : Std_library_utils.deprecation_info)] )

  let is_deprecated_distribution name =
    Map.mem StdLibrary.deprecated_distributions name

  let rename_deprecated map name =
    Map.find map name
    |> Option.map
         ~f:(fun Std_library_utils.{replacement; canonicalize_away; _} ->
           if canonicalize_away then replacement else name )
    |> Option.value ~default:name

  let rename_deprecated_distribution =
    rename_deprecated StdLibrary.deprecated_distributions

  let rename_deprecated_function =
    rename_deprecated StdLibrary.deprecated_functions

  let userdef_distributions stmts =
    let open String in
    List.filter_map
      ~f:(function
        | {stmt= FunDef {funname= {name; _}; _}; _} ->
            if
              is_suffix ~suffix:"_log_lpdf" name
              || is_suffix ~suffix:"_log_lpmf" name
            then Some (drop_suffix name 5)
            else if is_suffix ~suffix:"_log_log" name then
              Some (drop_suffix name 4)
            else None
        | _ -> None )
      (Ast.get_stmts stmts)

  let without_suffix user_dists name =
    let open String in
    if is_suffix ~suffix:"_lpdf" name || is_suffix ~suffix:"_lpmf" name then
      drop_suffix name 5
    else if
      is_suffix ~suffix:"_log" name
      && not
           ( is_deprecated_distribution (name ^ "_log")
           || List.exists ~f:(( = ) name) user_dists )
    then drop_suffix name 4
    else name

  let update_suffix name type_ =
    let open String in
    if is_suffix ~suffix:"_cdf_log" name then drop_suffix name 8 ^ "_lcdf"
    else if is_suffix ~suffix:"_ccdf_log" name then
      drop_suffix name 9 ^ "_lccdf"
    else if Middle.UnsizedType.is_int_type type_ then
      drop_suffix name 4 ^ "_lpmf"
    else drop_suffix name 4 ^ "_lpdf"

  let find_udf_log_suffix = function
    | { stmt=
          FunDef
            { funname= {name; _}
            ; arguments= (_, ((UReal | UInt) as type_), _) :: _
            ; _ }
      ; smeta= _ }
      when String.is_suffix ~suffix:"_log" name ->
        Some (name, type_)
    | _ -> None

  let rec collect_deprecated_expr (acc : (Location_span.t * string) list)
      ({expr; emeta} : (typed_expr_meta, fun_kind) expr_with) :
      (Location_span.t * string) list =
    match expr with
    | FunApp ((StanLib _ | UserDefined _), {name; _}, l) ->
        let w =
          match Map.find stan_lib_deprecations name with
          | Some {replacement; version; extra_message; _} ->
              [ ( emeta.loc
                , name ^ " is deprecated and will be removed in Stan " ^ version
                  ^ ". Use " ^ replacement ^ " instead. " ^ extra_message ) ]
          | _ when String.is_suffix name ~suffix:"_cdf" ->
              [ ( emeta.loc
                , "Use of " ^ name
                  ^ " without a vertical bar (|) between the first two \
                     arguments of a CDF is deprecated and will be removed in \
                     Stan 2.33.0. This can be automatically changed using the \
                     canonicalize flag for stanc" ) ]
          | _ -> [] in
        acc @ w @ List.concat_map l ~f:(fun e -> collect_deprecated_expr [] e)
    | PrefixOp (PNot, ({emeta= {type_= UReal; loc; _}; _} as e)) ->
        let acc =
          acc
          @ [ ( loc
              , "Using a real as a boolean value is deprecated and will be \
                 disallowed in Stan 2.34. Use an explicit != 0 comparison \
                 instead. This can be automatically changed using the \
                 canonicalize flag for stanc" ) ] in
        collect_deprecated_expr acc e
    | BinOp (({emeta= {type_= UReal; loc; _}; _} as e1), (And | Or), e2)
     |BinOp (e1, (And | Or), ({emeta= {type_= UReal; loc; _}; _} as e2)) ->
        let acc =
          acc
          @ [ ( loc
              , "Using a real as a boolean value is deprecated and will be \
                 disallowed in Stan 2.34. Use an explicit != 0 comparison \
                 instead. This can be automatically changed using the \
                 canonicalize flag for stanc" ) ] in
        let acc = collect_deprecated_expr acc e1 in
        let acc = collect_deprecated_expr acc e2 in
        acc
    | _ -> fold_expression collect_deprecated_expr (fun l _ -> l) acc expr

  let collect_deprecated_lval acc l =
    fold_lval_with collect_deprecated_expr (fun x _ -> x) acc l

  let rec collect_deprecated_stmt fundefs
      (acc : (Location_span.t * string) list) {stmt; _} :
      (Location_span.t * string) list =
    match stmt with
    | FunDef {body= {stmt= Skip; _}; funname; arguments; _}
      when is_redundant_forwarddecl fundefs funname arguments ->
        acc
        @ [ ( funname.id_loc
            , "Functions do not need to be declared before definition; all \
               user defined function names are always in scope regardless of \
               defintion order." ) ]
    | FunDef
        { body
        ; funname= {name; id_loc}
        ; arguments= (_, ((UReal | UInt) as type_), _) :: _
        ; _ }
      when String.is_suffix ~suffix:"_log" name ->
        let acc =
          acc
          @ [ ( id_loc
              , "Use of the _log suffix in user defined probability functions \
                 is deprecated and will be removed in Stan 2.33.0, use name '"
                ^ update_suffix name type_
                ^ "' instead if you intend on using this function in ~ \
                   statements or calling unnormalized probability functions \
                   inside of it." ) ] in
        collect_deprecated_stmt fundefs acc body
    | FunDef {body; _} -> collect_deprecated_stmt fundefs acc body
    | IfThenElse ({emeta= {type_= UReal; loc; _}; _}, ifb, elseb) ->
        let acc =
          acc
          @ [ ( loc
              , "Condition of type real is deprecated and will be disallowed \
                 in Stan 2.34. Use an explicit != 0 comparison instead. This \
                 can be automatically changed using the canonicalize flag for \
                 stanc" ) ] in
        let acc = collect_deprecated_stmt fundefs acc ifb in
        Option.value_map ~default:acc
          ~f:(collect_deprecated_stmt fundefs acc)
          elseb
    | While ({emeta= {type_= UReal; loc; _}; _}, body) ->
        let acc =
          acc
          @ [ ( loc
              , "Condition of type real is deprecated and will be disallowed \
                 in Stan 2.34. Use an explicit != 0 comparison instead. This \
                 can be automatically changed using the canonicalize flag for \
                 stanc" ) ] in
        collect_deprecated_stmt fundefs acc body
    | _ ->
        fold_statement collect_deprecated_expr
          (collect_deprecated_stmt fundefs)
          collect_deprecated_lval
          (fun l _ -> l)
          acc stmt

  let collect_userdef_distributions program =
    program.functionblock |> Ast.get_stmts
    |> List.filter_map ~f:find_udf_log_suffix
    |> List.dedup_and_sort ~compare:(fun (x, _) (y, _) -> String.compare x y)
    |> String.Map.of_alist_exn

  let collect_warnings (program : typed_program) =
    let fundefs = userdef_functions program in
    fold_program (collect_deprecated_stmt fundefs) [] program
end
