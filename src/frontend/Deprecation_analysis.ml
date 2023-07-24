open Core_kernel
open Ast
open Middle

let current_removal_version = (2, 33)

let expired (major, minor) =
  let removal_major, removal_minor = current_removal_version in
  removal_major > major || (removal_major = major && removal_minor >= minor)

let deprecated_functions =
  String.Map.of_alist_exn
    [ ("multiply_log", ("lmultiply", (2, 33)))
    ; ("binomial_coefficient_log", ("lchoose", (2, 33)))
    ; ("cov_exp_quad", ("gp_exp_quad_cov", (2, 33))); ("fabs", ("abs", (2, 33)))
    ]

(* TODO need to mark lkj_cov as deprecated *)

let deprecated_odes =
  String.Map.of_alist_exn
    [ ("integrate_ode", ("ode_rk45", (3, 0)))
    ; ("integrate_ode_rk45", ("ode_rk45", (3, 0)))
    ; ("integrate_ode_bdf", ("ode_bdf", (3, 0)))
    ; ("integrate_ode_adams", ("ode_adams", (3, 0))) ]

let deprecated_distributions =
  String.Map.of_alist_exn
    (List.map
       ~f:(fun (x, y) -> (x, (y, (2, 33))))
       (List.concat_map Middle.Stan_math_signatures.distributions
          ~f:(fun (fnkinds, name, _, _) ->
            List.filter_map fnkinds ~f:(function
              | Lpdf -> Some (name ^ "_log", name ^ "_lpdf")
              | Lpmf -> Some (name ^ "_log", name ^ "_lpmf")
              | Cdf -> Some (name ^ "_cdf_log", name ^ "_lcdf")
              | Ccdf -> Some (name ^ "_ccdf_log", name ^ "_lccdf")
              | Rng | Log | UnaryVectorized _ -> None ) ) ) )

let stan_lib_deprecations =
  Map.merge_skewed deprecated_distributions deprecated_functions
    ~combine:(fun ~key x y ->
      Common.FatalError.fatal_error_msg
        [%message
          "Common key in deprecation map"
            (key : string)
            (x : string * (int * int))
            (y : string * (int * int))] )

let is_deprecated_distribution name =
  Option.is_some (Map.find deprecated_distributions name)

let rename_deprecated map name =
  Map.find map name |> Option.map ~f:fst |> Option.value ~default:name

let userdef_functions program =
  match program.functionblock with
  | None -> Hash_set.Poly.create ()
  | Some {stmts; _} ->
      List.filter_map stmts ~f:(function
        | {stmt= FunDef {body= {stmt= Skip; _}; _}; _} -> None
        | {stmt= FunDef {funname; arguments; _}; _} ->
            Some (funname.name, Ast.type_of_arguments arguments)
        | _ -> None )
      |> Hash_set.Poly.of_list

let is_redundant_forwarddecl fundefs funname arguments =
  Hash_set.mem fundefs (funname.name, Ast.type_of_arguments arguments)

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
  else if is_suffix ~suffix:"_ccdf_log" name then drop_suffix name 9 ^ "_lccdf"
  else if Middle.UnsizedType.is_discrete_type type_ then
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
  | CondDistApp ((StanLib _ | UserDefined _), {name; _}, l)
   |FunApp ((StanLib _ | UserDefined _), {name; _}, l) ->
      let w =
        match Map.find stan_lib_deprecations name with
        | Some (rename, (major, minor)) ->
            if expired (major, minor) then []
            else
              let version = string_of_int major ^ "." ^ string_of_int minor in
              [ ( emeta.loc
                , name ^ " is deprecated and will be removed in Stan " ^ version
                  ^ ". Use " ^ rename
                  ^ " instead. This can be automatically changed using the \
                     canonicalize flag for stanc" ) ]
        | _ -> (
          match Map.find deprecated_odes name with
          | Some (rename, (major, minor)) ->
              let version = string_of_int major ^ "." ^ string_of_int minor in
              [ ( emeta.loc
                , name ^ " is deprecated and will be removed in Stan " ^ version
                  ^ ". Use " ^ rename
                  ^ " instead. \n\
                     The new interface is slightly different, see: \
                     https://mc-stan.org/users/documentation/case-studies/convert_odes.html"
                ) ]
          | _ -> [] ) in
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

let rec collect_deprecated_stmt fundefs (acc : (Location_span.t * string) list)
    {stmt; _} : (Location_span.t * string) list =
  match stmt with
  | FunDef {body= {stmt= Skip; _}; funname; arguments; _}
    when is_redundant_forwarddecl fundefs funname arguments ->
      acc
      @ [ ( funname.id_loc
          , "Functions do not need to be declared before definition; all user \
             defined function names are always in scope regardless of \
             definition order." ) ]
  | FunDef {body; _} -> collect_deprecated_stmt fundefs acc body
  | IfThenElse ({emeta= {type_= UReal; loc; _}; _}, ifb, elseb) ->
      let acc =
        acc
        @ [ ( loc
            , "Condition of type real is deprecated and will be disallowed in \
               Stan 2.34. Use an explicit != 0 comparison instead. This can be \
               automatically changed using the canonicalize flag for stanc" ) ]
      in
      let acc = collect_deprecated_stmt fundefs acc ifb in
      Option.value_map ~default:acc
        ~f:(collect_deprecated_stmt fundefs acc)
        elseb
  | While ({emeta= {type_= UReal; loc; _}; _}, body) ->
      let acc =
        acc
        @ [ ( loc
            , "Condition of type real is deprecated and will be disallowed in \
               Stan 2.34. Use an explicit != 0 comparison instead. This can be \
               automatically changed using the canonicalize flag for stanc" ) ]
      in
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
