open Core_kernel
open Ast
open Middle

(* Implementation of the [--info] option which outputs on the standard
   output a JSON object containing information about the model.

   The JSON object has a field [inputs], [parameters], [transformed
   parameters], and [generated quantities] containing a dictionary
   where each entry corresponds to a variable in respectively the
   [data], [parameters], [transformed parameters], and [generated
   quantities] blocks. To each variable is associated an object with
   two fields:
   - [type]: the base type of the variable (["int"] or ["real"]).
   - [dimensions]: the number of dimensions ([0] for a scalar, [1] for
     a vector or row vector, etc.).
   The JSON object also have the fields [stanlib_calls] and [distributions]
   containing the name of the standard library functions called and
   distributions used.
*)

module SSet = Set.Make (String)

let rec sized_basetype_dims t =
  match t with
  | SizedType.SInt -> ("int", 0)
  | SReal -> ("real", 0)
  | SVector _ | SRowVector _ -> ("real", 1)
  | SMatrix _ -> ("real", 2)
  | SArray (t, _) ->
      let bt, n = sized_basetype_dims t in
      (bt, n + 1)

let rec unsized_basetype_dims t =
  match t with
  | UnsizedType.UInt -> ("int", 0)
  | UReal -> ("real", 0)
  | UVector | URowVector -> ("real", 1)
  | UMatrix -> ("real", 2)
  | UArray t ->
      let bt, n = unsized_basetype_dims t in
      (bt, n + 1)
  | UMathLibraryFunction | UFun _ -> assert false

let basetype_dims t =
  match t with
  | Type.Sized t -> sized_basetype_dims t
  | Type.Unsized t -> unsized_basetype_dims t

let get_var_decl stmts =
  List.fold_right ~init:[]
    ~f:(fun stmt acc ->
      match stmt.Ast.stmt with
      | Ast.VarDecl decl ->
          let t, n = basetype_dims decl.decl_type in
          (decl.identifier.name, t, n) :: acc
      | _ -> acc )
    stmts

let block_info name ppf block =
  let var_info ppf (name, t, n) =
    Fmt.pf ppf "\"%s\": { \"type\": \"%s\", \"dimensions\": %d}" name t n
  in
  let vars_info = Fmt.list ~sep:Fmt.comma var_info in
  Fmt.pf ppf "\"%s\": { @[<v 0>%a @]}" name vars_info
    (Option.value_map block ~default:[] ~f:get_var_decl)

let rec get_function_calls_expr (funs, distrs) expr =
  let acc =
    match expr.expr with
    | FunApp (StanLib, f, _) -> (SSet.add funs f.name, distrs)
    | CondDistApp (StanLib, f, _) -> (funs, SSet.add distrs f.name)
    | _ -> (funs, distrs)
  in
  fold_expression get_function_calls_expr (fun acc _ -> acc) acc expr.expr

let rec get_function_calls_stmt ud_dists (funs, distrs) stmt =
  let acc =
    match stmt.stmt with
    | NRFunApp (StanLib, f, _) -> (SSet.add funs f.name, distrs)
    | Tilde {distribution; _} ->
        let possible_names =
          List.map ~f:(( ^ ) distribution.name) Utils.distribution_suffices
          |> String.Set.of_list
        in
        if List.exists ~f:(fun (n, _) -> Set.mem possible_names n) ud_dists
        then (funs, distrs)
        else
          let suffix =
            Stan_math_signatures.dist_name_suffix ud_dists distribution.name
          in
          let name = distribution.name ^ Utils.unnormalized_suffix suffix in
          (funs, SSet.add distrs name)
    | _ -> (funs, distrs)
  in
  fold_statement get_function_calls_expr
    (get_function_calls_stmt ud_dists)
    (fun acc _ -> acc)
    (fun acc _ -> acc)
    acc stmt.stmt

let function_calls ppf p =
  let map f list_op =
    Option.value_map ~default:[] ~f:(List.concat_map ~f) list_op
  in
  let grab_fundef_names_and_types = function
    | {Ast.stmt= Ast.FunDef {funname; arguments= (_, type_, _) :: _; _}; _} ->
        [(funname.name, type_)]
    | _ -> []
  in
  let ud_dists = map grab_fundef_names_and_types p.functionblock in
  let funs, distrs =
    fold_program (get_function_calls_stmt ud_dists) (SSet.empty, SSet.empty) p
  in
  Fmt.pf ppf "\"functions\": [ @[<v 0>%a @]],@,"
    (Fmt.list ~sep:Fmt.comma (fun ppf s -> Fmt.pf ppf "\"%s\"" s))
    (SSet.to_list funs) ;
  Fmt.pf ppf "\"distributions\": [ @[<v 0>%a @]]"
    (Fmt.list ~sep:Fmt.comma (fun ppf s -> Fmt.pf ppf "\"%s\"" s))
    (SSet.to_list distrs)

let info ast =
  Fmt.strf "{ @[<v 0>%a,@,%a,@,%a,@,%a,@,%a @]}@." (block_info "inputs")
    ast.datablock (block_info "parameters") ast.parametersblock
    (block_info "transformed parameters")
    ast.transformedparametersblock
    (block_info "generated quantities")
    ast.generatedquantitiesblock function_calls ast
