open Std
open Ast

let current_removal_version = (2, 40)

let expired (major, minor) =
  let removal_major, removal_minor = current_removal_version in
  removal_major > major || (removal_major = major && removal_minor >= minor)

let deprecated_functions = String.Map.of_list []
let stan_lib_deprecations = deprecated_functions

(* TODO deprecate other pre-variadics like algebra_solver? *)
let deprecated_odes =
  String.Map.of_list
    [ ("integrate_ode", ("ode_rk45", (3, 0)))
    ; ("integrate_ode_rk45", ("ode_rk45", (3, 0)))
    ; ("integrate_ode_bdf", ("ode_bdf", (3, 0)))
    ; ("integrate_ode_adams", ("ode_adams", (3, 0))) ]

let rename_deprecated map name =
  String.Map.find_opt name map
  |> Option.map ~f:fst |> Option.value ~default:name

let userdef_functions program =
  match program.functionblock with
  | None -> Hashtbl.create 0
  | Some {stmts; _} ->
      List.filter_map stmts ~f:(function
        | {stmt= FunDef {body= {stmt= Skip; _}; _}; _} -> None
        | {stmt= FunDef {funname; arguments; _}; _} ->
            Some (funname.name, Ast.type_of_arguments arguments)
        | _ -> None)
      |> List.map ~f:(fun x -> (x, ()))
      |> List.to_seq |> Hashtbl.of_seq

let is_redundant_forwarddecl fundefs funname arguments =
  Hashtbl.mem fundefs (funname.name, Ast.type_of_arguments arguments)

let rec collect_deprecated_expr (acc : Warnings.t list)
    ({expr; _} : Ast.typed_expression) : Warnings.t list =
  match expr with
  | CondDistApp ((StanLib _ | UserDefined _), {name; id_loc}, l)
   |FunApp ((StanLib _ | UserDefined _), {name; id_loc}, l) ->
      let w =
        match String.Map.find_opt name stan_lib_deprecations with
        | Some (rename, (major, minor)) when not (expired (major, minor)) ->
            [Warnings.function_deprecation id_loc name (major, minor) rename]
        | _ -> (
            match String.Map.find_opt name deprecated_odes with
            | Some (rename, (major, minor)) ->
                [Warnings.ode_deprecation id_loc name (major, minor) rename]
            | _ when String.equal name "lkj_cov_lpdf" ->
                [Warnings.lkj_cov_deprecation id_loc]
            | _ -> []) in
      acc @ w @ List.concat_map l ~f:(fun e -> collect_deprecated_expr [] e)
  | _ -> fold_expression collect_deprecated_expr acc expr

let collect_deprecated_lval acc l = fold_lval_with collect_deprecated_expr acc l

let rec collect_deprecated_stmt fundefs (acc : Warnings.t list) {stmt; _} :
    Warnings.t list =
  match stmt with
  | FunDef {body= {stmt= Skip; _}; funname; arguments; _}
    when is_redundant_forwarddecl fundefs funname arguments ->
      acc @ [Warnings.forward_declaration funname.id_loc]
  | Tilde {distribution; _} when String.equal distribution.name "lkj_cov" ->
      let acc = Warnings.lkj_cov_deprecation distribution.id_loc :: acc in
      fold_statement collect_deprecated_expr
        (collect_deprecated_stmt fundefs)
        collect_deprecated_lval acc stmt
  | _ ->
      fold_statement collect_deprecated_expr
        (collect_deprecated_stmt fundefs)
        collect_deprecated_lval acc stmt

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
          {x with stmts= List.filter ~f:drop_forwarddecl x.stmts}) }
