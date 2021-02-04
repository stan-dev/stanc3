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
*)

let with_info = ref false

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

let block_info name ff block =
  let var_info ff (name, t, n) =
    Format.fprintf ff "\"%s\": { \"type\": \"%s\", \"dimensions\": %d}" name t
      n
  in
  let vars_info ff decls =
    Format.pp_print_list
      ~pp_sep:(fun ff () -> Format.fprintf ff ",@,")
      var_info ff decls
  in
  Format.fprintf ff "\"%s\": { @[<v 0>%a @]}" name vars_info
    (Option.value_map block ~default:[] ~f:get_var_decl)

let info ast =
  let ff = Format.std_formatter in
  Format.fprintf ff "{ @[<v 0>%a,@,%a,@,%a,@,%a @]}@." (block_info "inputs")
    ast.datablock (block_info "parameters") ast.parametersblock
    (block_info "transformed parameters")
    ast.transformedparametersblock
    (block_info "generated quantities")
    ast.generatedquantitiesblock ;
  exit 0
