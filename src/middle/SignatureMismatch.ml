open Core_kernel
module TypeMap = Core_kernel.Map.Make_using_comparator (UnsizedType)

let set ctx key data = ctx := TypeMap.set !ctx ~key ~data

let get ctx key =
  match TypeMap.find !ctx key with
  | Some s -> s
  | None ->
      let s = Fmt.strf "F%d" (1 + TypeMap.length !ctx) in
      set ctx key s ; s

(** Like UnsizedType.pp but with opaque names for function types. *)
let pp_unsized_type ctx ppf =
  let rec pp ppf ty =
    match ty with
    | UnsizedType.UInt | UReal | UVector | URowVector | UMatrix
     |UMathLibraryFunction ->
        UnsizedType.pp ppf ty
    | UArray ut ->
        let ut2, d = UnsizedType.unwind_array_type ut in
        let array_str = "[" ^ String.make d ',' ^ "]" in
        Fmt.pf ppf "array%s %a" array_str pp ut2
    | UFun _ -> Fmt.pf ppf "<%s>" (get ctx ty)
  in
  pp ppf

let pp_fundef ctx ppf =
  let pp_returntype ppf = function
    | UnsizedType.Void -> Fmt.string ppf "void"
    | ReturnType ty -> pp_unsized_type ctx ppf ty
  in
  let pp_fun_arg ppf (ad, ty) =
    match ad with
    | UnsizedType.DataOnly -> Fmt.pf ppf "data %a" (pp_unsized_type ctx) ty
    | AutoDiffable -> pp_unsized_type ctx ppf ty
  in
  function
  | UnsizedType.UFun (args, rt, _) ->
      Fmt.pf ppf "@[<hov>(@[<hov>%a@]) => %a@]"
        Fmt.(list ~sep:comma pp_fun_arg)
        args pp_returntype rt
  | ty -> pp_unsized_type ctx ppf ty

let pp_with_where ctx f ppf x =
  let get_new old =
    ( !ctx
    , Map.filter_keys !ctx ~f:(fun ty -> not (Map.mem old ty))
      |> Map.to_alist
      |> List.sort ~compare:(fun (_, id1) (_, id2) -> String.compare id1 id2)
    )
  in
  let rec pp_where ppf (old, new_) =
    let pp ppf (ty, id) =
      Fmt.pf ppf "%s = @[<hov>%a@]" id (pp_fundef ctx) ty
    in
    Fmt.(list ~sep:cut) pp ppf new_ ;
    let old, new_ = get_new old in
    if not (List.is_empty new_) then Fmt.pf ppf "@,%a" pp_where (old, new_)
  in
  let old = !ctx in
  Fmt.pf ppf "@[<v>%a" f x ;
  let old, new_ = get_new old in
  if not (List.is_empty new_) then
    Fmt.pf ppf "@,where @[<v>%a@]" pp_where (old, new_) ;
  Fmt.pf ppf "@]"

type type_mismatch =
  | DataOnlyError
  | TypesMismatch of UnsizedType.t * UnsizedType.t
  | FuncTypeMismatch of UnsizedType.t * UnsizedType.t * function_mismatch

and function_mismatch =
  | SuffixMismatch of unit Fun_kind.suffix * unit Fun_kind.suffix
  | ReturnTypeMismatch of UnsizedType.returntype * UnsizedType.returntype
  | ArgError of int * type_mismatch
  | ArgNumMismatch of int * int

let rec compare_types t1 t2 =
  match (t1, t2) with
  | UnsizedType.(UArray t1, UArray t2) -> compare_types t1 t2
  | _, UArray _ -> -1
  | UArray _, _ -> 1
  | t1, t2 -> UnsizedType.compare t1 t2

let rec compare_errors e1 e2 =
  match (e1, e2) with
  | DataOnlyError, DataOnlyError -> 0
  | TypesMismatch (t1, x1), TypesMismatch (t2, x2) ->
      let c = compare_types t1 t2 in
      if c <> 0 then c else compare_types x1 x2
  | DataOnlyError, _ | _, TypesMismatch _ -> -1
  | _, DataOnlyError | TypesMismatch _, _ -> 1
  | FuncTypeMismatch (_, _, e1), FuncTypeMismatch (_, _, e2) ->
      compare_function_errors e1 e2

and compare_function_errors e1 e2 =
  match (e1, e2) with
  | ArgNumMismatch _, ArgNumMismatch _
   |ReturnTypeMismatch _, ReturnTypeMismatch _
   |SuffixMismatch _, SuffixMismatch _ ->
      0
  | ArgError (x, _), ArgError (y, _) when x <> y -> compare y x
  | ArgError (_, e1), ArgError (_, e2) -> compare_errors e1 e2
  | ArgError _, _ | _, SuffixMismatch _ -> -1
  | _, ArgError _ | SuffixMismatch _, _ -> 1
  | ReturnTypeMismatch _, ArgNumMismatch _ -> -1
  | ArgNumMismatch _, ReturnTypeMismatch _ -> 1

let rec check_same_type depth t1 t2 =
  let wrap_func = Option.map ~f:(fun e -> FuncTypeMismatch (t1, t2, e)) in
  match (t1, t2) with
  | t1, t2 when t1 = t2 -> None
  | UnsizedType.(UReal, UInt) when depth < 1 -> None
  | UFun (_, _, s1), UFun (_, _, s2)
    when Fun_kind.without_propto s1 <> Fun_kind.without_propto s2 ->
      Some
        (SuffixMismatch (Fun_kind.without_propto s1, Fun_kind.without_propto s2))
      |> wrap_func
  | UFun (_, rt1, _), UFun (_, rt2, _) when rt1 <> rt2 ->
      Some (ReturnTypeMismatch (rt1, rt2)) |> wrap_func
  | UFun (l1, _, _), UFun (l2, _, _) ->
      check_compatible_arguments (depth + 1) l2 l1 |> wrap_func
  | t1, t2 -> Some (TypesMismatch (t1, t2))

and check_compatible_arguments depth args1 args2 =
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

let check_compatible_arguments_mod_conv = check_compatible_arguments 0

let stan_math_returntype name args =
  if Stan_math_signatures.is_reduce_sum_fn name then
    (* FIXME handle variadic *)
    Result.Ok (UnsizedType.ReturnType UReal)
  else if Stan_math_signatures.is_variadic_ode_fn name then
    (* FIXME handle variadic *)
    Ok (UnsizedType.ReturnType (UArray UVector))
  else
    let name = Utils.stdlib_distribution_name name in
    Hashtbl.find_multi Stan_math_signatures.stan_math_signatures name
    |> List.sort ~compare:(fun (x, _) (y, _) ->
           UnsizedType.compare_returntype x y )
    (* Check the least return type first in case there are multiple options (due to implicit UInt-UReal conversion), where UInt<UReal *)
    |> List.fold_until ~init:[]
         ~f:(fun errors (rt, tys) ->
           match check_compatible_arguments 0 tys args with
           | None -> Stop (Ok rt)
           | Some e -> Continue (((rt, tys), e) :: errors) )
         ~finish:(fun errors ->
           let errors =
             List.sort errors ~compare:(fun (_, e1) (_, e2) ->
                 compare_function_errors e1 e2 )
           in
           let errors, omitted = List.split_n errors 10 in
           Error (errors, not (List.is_empty omitted)) )

let pp_signature_mismatch ppf (name, arg_tys, (sigs, omitted)) =
  let open Fmt in
  let ctx = ref TypeMap.empty in
  let suffix_str = function
    | Fun_kind.FnPlain -> "a pure function"
    | FnRng -> "an rng function"
    | FnLpdf _ -> "a probability density function"
    | FnTarget -> "an _lp function"
  in
  let rec pp_explain recursive ppf = function
    | ArgError (n, DataOnlyError) when recursive ->
        pf ppf "@[<hov>Argument@ %d@ %a@]" n text
          "has an incompatible data-qualifier."
    | ArgError (n, DataOnlyError) ->
        pf ppf "@[<hov>Argument@ %d@ %a@]" n text
          "must be data-only. (Local variables are assumed to depend on \
           parameters; same goes for function inputs unless they are marked \
           with the keyword 'data'.)"
    | ArgError (n, TypesMismatch (expected, found)) ->
        pf ppf "@[<hv>Argument %d must be@, %a@ but found@, %a@]" n
          (pp_unsized_type ctx) expected (pp_unsized_type ctx) found
    | ArgError (n, FuncTypeMismatch (_, _, SuffixMismatch (expected, found)))
      ->
        pf ppf
          "@[<v>Argument %d must be %s but found %s. These function types are \
           not compatible.@]"
          n (suffix_str expected) (suffix_str found)
    | ArgError (n, FuncTypeMismatch (expected, found, err)) ->
        pf ppf
          "@[<v>Argument %d must be@, %a@ but found@, %a@ @[<v 2>These are \
           not compatible because:@ @[<hov>%a@]@]@]"
          n (pp_unsized_type ctx) expected (pp_unsized_type ctx) found
          (pp_explain true) err
    | ReturnTypeMismatch (expected, found) ->
        let _ = ppf in
        pf ppf "Return types are incompatible: expected %a but found %a"
          UnsizedType.pp_returntype expected UnsizedType.pp_returntype found
    | ArgNumMismatch (expected, found) ->
        pf ppf "Expected %d arguments but found %d arguments." expected found
    | SuffixMismatch (expected, found) ->
        pf ppf
          "Expected %s but found %s: These function types are not compatible."
          (suffix_str expected) (suffix_str found)
  in
  let pp_args =
    pp_with_where ctx (fun ppf ->
        pf ppf "(@[<hov>%a@])" (list ~sep:comma (pp_unsized_type ctx)) )
  in
  let pp_signature ppf ((rt, args), err) =
    let fun_ty = UnsizedType.UFun (args, rt, FnPlain) in
    Fmt.pf ppf "%a@ @[<hov 2>  %a@]"
      (pp_with_where ctx (pp_fundef ctx))
      fun_ty (pp_explain false) err
  in
  let pp_omitted ppf () =
    if omitted then pf ppf "@,(Additional signature omitted)"
  in
  pf ppf
    "@[<v>Ill-typed arguments supplied to function '%s':@ %a@ Available \
     signatures:@ %a%a@]"
    name pp_args arg_tys
    (list ~sep:cut pp_signature)
    sigs pp_omitted ()
