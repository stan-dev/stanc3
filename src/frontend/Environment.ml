open Core_kernel
open Middle

(** Origin blocks, to keep track of where variables are declared *)
type originblock =
  | MathLibrary
  | Functions
  | Data
  | TData
  | Param
  | TParam
  | Model
  | GQuant
[@@deriving sexp]

type varinfo = {origin: originblock; global: bool; readonly: bool}
[@@deriving sexp]

type info =
  { type_: UnsizedType.t
  ; kind:
      [ `Variable of varinfo
      | `UserDeclared of Location_span.t
      | `StanMath
      | `UserDefined ] }
[@@deriving sexp]

type t = info list String.Map.t

let create () =
  let functions =
    Hashtbl.to_alist Stan_math_signatures.stan_math_signatures
    |> List.map ~f:(fun (key, values) ->
           ( key
           , List.map
               ~f:(fun (rt, args, mem) ->
                 let type_ =
                   UnsizedType.UFun (args, rt, Fun_kind.FnPlain, mem)
                 in
                 {type_; kind= `StanMath} )
               values ) )
    |> String.Map.of_alist_exn
  in
  functions

let add env key type_ kind = Map.add_multi env ~key ~data:{type_; kind}

let add_all_raw env key data =
  let env = Map.remove env key in
  List.fold ~init:env ~f:(fun env data -> Map.add_multi env ~key ~data) data

let find env key = Map.find_multi env key
let mem env key = Map.mem env key
let iter env f = Map.iter env ~f

(* TODO move this to SignatureMismatch.ml when semantic_check is removed *)

let extract_function_types f =
  match f with
  | {type_= UFun (args, return, _, mem); kind= `StanMath} ->
      Some (return, args, (fun x -> Ast.StanLib x), mem)
  | {type_= UFun (args, return, _, mem); _} ->
      Some (return, args, (fun x -> UserDefined x), mem)
  | _ -> None

let rec check_same_type depth t1 t2 =
  let open SignatureMismatch in
  let wrap_func = Option.map ~f:(fun e -> TypeMismatch (t1, t2, Some e)) in
  match (t1, t2) with
  | t1, t2 when t1 = t2 -> None
  | UnsizedType.(UReal, UInt) when depth < 1 -> None
  | UFun (_, _, s1, _), UFun (_, _, s2, _)
    when Fun_kind.without_propto s1 <> Fun_kind.without_propto s2 ->
      Some
        (SuffixMismatch (Fun_kind.without_propto s1, Fun_kind.without_propto s2))
      |> wrap_func
  | UFun (_, rt1, _, _), UFun (_, rt2, _, _) when rt1 <> rt2 ->
      Some (ReturnTypeMismatch (rt1, rt2)) |> wrap_func
  | UFun (l1, _, _, _), UFun (l2, _, _, _) ->
      check_compatible_arguments (depth + 1) l2 l1
      |> Option.map ~f:(fun e -> InputMismatch e)
      |> wrap_func
  | t1, t2 -> Some (TypeMismatch (t1, t2, None))

and check_compatible_arguments depth args1 args2 =
  let open SignatureMismatch in
  match List.zip args1 args2 with
  | None -> Some (ArgNumMismatch (List.length args1, List.length args2))
  | Some l ->
      List.find_mapi l ~f:(fun i ((ad1, ut1), (ad2, ut2)) ->
          match check_same_type depth ut1 ut2 with
          | Some e -> Some (ArgError (i + 1, e))
          | None ->
              if ad1 = ad2 then None
              else if depth < 2 && UnsizedType.autodifftype_can_convert ad1 ad2
              then None
              else Some (ArgError (i + 1, DataOnlyError)) )

let returntype env name args =
  let open SignatureMismatch in
  (* NB: Variadic arguments are special-cased in Semantic_check and not handled here *)
  let name = Utils.stdlib_distribution_name name in
  find env name
  |> List.filter_map ~f:extract_function_types
  |> List.sort ~compare:(fun (x, _, _, _) (y, _, _, _) ->
         UnsizedType.compare_returntype x y )
  (* Check the least return type first in case there are multiple options (due to implicit UInt-UReal conversion), where UInt<UReal *)
  |> List.fold_until ~init:[]
       ~f:(fun errors (rt, tys, funkind_constructor, _) ->
         match check_compatible_arguments 0 tys args with
         | None -> Stop (Ok (rt, funkind_constructor))
         | Some e -> Continue (((rt, tys), e) :: errors) )
       ~finish:(fun errors ->
         let errors =
           List.sort errors ~compare:(fun (_, e1) (_, e2) ->
               compare_errors e1 e2 )
         in
         let errors, omitted = List.split_n errors 5 in
         Error (errors, not (List.is_empty omitted)) )
