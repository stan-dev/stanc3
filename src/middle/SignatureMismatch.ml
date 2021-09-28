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
  | UnsizedType.UFun (args, rt, _, _) ->
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
  | TypeMismatch of UnsizedType.t * UnsizedType.t * details option

and details =
  | SuffixMismatch of unit Fun_kind.suffix * unit Fun_kind.suffix
  | ReturnTypeMismatch of UnsizedType.returntype * UnsizedType.returntype
  | InputMismatch of function_mismatch

and function_mismatch =
  | ArgError of int * type_mismatch
  | ArgNumMismatch of int * int
[@@deriving sexp]

type signature_error =
  (UnsizedType.returntype * (UnsizedType.autodifftype * UnsizedType.t) list)
  * function_mismatch

let rec compare_types t1 t2 =
  match (t1, t2) with
  | UnsizedType.(UArray t1, UArray t2) -> compare_types t1 t2
  | _, UArray _ -> -1
  | UArray _, _ -> 1
  | t1, t2 -> UnsizedType.compare t1 t2

let rec compare_errors e1 e2 =
  match (e1, e2) with
  | ArgNumMismatch _, ArgNumMismatch _ -> 0
  | ArgError _, ArgNumMismatch _ -> -1
  | ArgNumMismatch _, ArgError _ -> 1
  | ArgError (x, _), ArgError (y, _) when x <> y -> compare y x
  | ArgError (_, e1), ArgError (_, e2) -> (
    match (e1, e2) with
    | DataOnlyError, DataOnlyError -> 0
    | DataOnlyError, TypeMismatch _ -> -1
    | TypeMismatch _, DataOnlyError -> 1
    | TypeMismatch (t1, x1, None), TypeMismatch (t2, x2, None) ->
        let c = compare_types t1 t2 in
        if c <> 0 then c else compare_types x1 x2
    | TypeMismatch (_, _, Some _), TypeMismatch (_, _, None) -> 1
    | TypeMismatch (_, _, None), TypeMismatch (_, _, Some _) -> -1
    | TypeMismatch (_, _, Some e1), TypeMismatch (_, _, Some e2) -> (
      match (e1, e2) with
      | SuffixMismatch _, SuffixMismatch _ -> 0
      | ReturnTypeMismatch _, ReturnTypeMismatch _ -> 0
      | InputMismatch e1, InputMismatch e2 -> compare_errors e1 e2
      | SuffixMismatch _, _ | _, InputMismatch _ -> -1
      | InputMismatch _, _ | _, SuffixMismatch _ -> 1 ) )

let rec check_same_type depth t1 t2 =
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
let max_n_errors = 5

let stan_math_returntype name args =
  (* NB: Variadic arguments are special-cased in Semantic_check and not handled here *)
  let name = Utils.stdlib_distribution_name name in
  Hashtbl.find_multi Stan_math_signatures.stan_math_signatures name
  |> List.sort ~compare:(fun (x, _, _) (y, _, _) ->
         UnsizedType.compare_returntype x y )
  (* Check the least return type first in case there are multiple options (due to implicit UInt-UReal conversion), where UInt<UReal *)
  |> List.fold_until ~init:[]
       ~f:(fun errors (rt, tys, _) ->
         match check_compatible_arguments 0 tys args with
         | None -> Stop (Ok rt)
         | Some e -> Continue (((rt, tys), e) :: errors) )
       ~finish:(fun errors ->
         let errors =
           List.sort errors ~compare:(fun (_, e1) (_, e2) ->
               compare_errors e1 e2 )
         in
         let errors, omitted = List.split_n errors max_n_errors in
         Error (errors, not (List.is_empty omitted)) )

let check_variadic_args allow_lpdf mandatory_arg_tys mandatory_fun_arg_tys
    fun_return args =
  let minimal_func_type =
    UnsizedType.UFun
      (mandatory_fun_arg_tys, ReturnType fun_return, FnPlain, AoS)
  in
  let minimal_args =
    (UnsizedType.AutoDiffable, minimal_func_type) :: mandatory_arg_tys
  in
  let wrap_err x = Some (minimal_args, ArgError (1, x)) in
  match args with
  | ( _
    , ( UnsizedType.UFun (fun_args, ReturnType return_type, suffix, _) as
      func_type ) )
    :: _ ->
      let mandatory, variadic_arg_tys =
        List.split_n fun_args (List.length mandatory_fun_arg_tys)
      in
      let wrap_func_error x =
        TypeMismatch (minimal_func_type, func_type, Some x) |> wrap_err
      in
      let suffix = Fun_kind.without_propto suffix in
      if suffix = FnPlain || (allow_lpdf && suffix = FnLpdf ()) then
        match check_compatible_arguments 1 mandatory mandatory_fun_arg_tys with
        | Some x -> wrap_func_error (InputMismatch x)
        | None -> (
          match check_same_type 1 return_type fun_return with
          | Some _ ->
              wrap_func_error
                (ReturnTypeMismatch
                   (ReturnType fun_return, ReturnType return_type))
          | None ->
              let expected_args =
                ((UnsizedType.AutoDiffable, func_type) :: mandatory_arg_tys)
                @ variadic_arg_tys
              in
              check_compatible_arguments 0 expected_args args
              |> Option.map ~f:(fun x -> (expected_args, x)) )
      else wrap_func_error (SuffixMismatch (FnPlain, suffix))
  | (_, x) :: _ -> TypeMismatch (minimal_func_type, x, None) |> wrap_err
  | [] -> Some ([], ArgNumMismatch (List.length mandatory_arg_tys, 0))

let pp_signature_mismatch ppf (name, arg_tys, (sigs, omitted)) =
  let open Fmt in
  let ctx = ref TypeMap.empty in
  let suffix_str = function
    | Fun_kind.FnPlain -> "a pure function"
    | FnRng -> "an rng function"
    | FnLpdf () -> "a probability density or mass function"
    | FnTarget -> "an _lp function"
  in
  let index_str = function
    | 1 -> "first"
    | 2 -> "second"
    | 3 -> "third"
    | 4 -> "fourth"
    | n -> Fmt.strf "%dth" n
  in
  let rec pp_explain_rec ppf = function
    | ArgError (n, DataOnlyError) ->
        pf ppf "@[<hov>The@ %s@ argument%a@]" (index_str n) text
          " has an incompatible data-qualifier."
    | ArgError (n, TypeMismatch (expected, found, None)) ->
        pf ppf
          "@[<hv>The types for the %s argument are incompatible: one is@, %a@ \
           but the other is@, %a@]"
          (index_str n) (pp_unsized_type ctx) expected (pp_unsized_type ctx)
          found
    | ArgError (n, TypeMismatch (_, _, Some (SuffixMismatch (expected, found))))
      ->
        pf ppf
          "@[<v>The %s argument is %s but the other is %s. These function \
           types are not compatible.@]"
          (index_str n) (suffix_str expected) (suffix_str found)
    | ArgError (n, TypeMismatch (expected, found, Some (InputMismatch err))) ->
        pf ppf
          "@[<v>The types for the %s argument are incompatible: one is@, %a@ \
           but the other is@, %a@ @[<v>These are not compatible because:@ \
           @[<hov>%a@]@]@]"
          (index_str n) (pp_fundef ctx) expected (pp_fundef ctx) found
          pp_explain_rec err
    | ArgError (n, TypeMismatch (expected, found, Some (ReturnTypeMismatch _)))
      ->
        pf ppf
          "@[<v>The %s argument must be@, %a@ but got@, %a@ The return types \
           are different.@]"
          (index_str n) (pp_fundef ctx) expected (pp_fundef ctx) found
    | ArgNumMismatch (expected, found) ->
        pf ppf "One takes %d arguments but the other takes %d arguments."
          expected found
  in
  let pp_explain ppf = function
    | ArgError (n, DataOnlyError) ->
        pf ppf "@[<hov>The@ %s@ argument%a@]" (index_str n) text
          " must be data-only. (Local variables are assumed to depend on \
           parameters; same goes for function inputs unless they are marked \
           with the keyword 'data'.)"
    | ArgError (n, TypeMismatch (expected, found, None)) ->
        pf ppf "@[<hv>The %s argument must be@, %a@ but got@, %a@]"
          (index_str n) (pp_unsized_type ctx) expected (pp_unsized_type ctx)
          found
    | ArgError (n, TypeMismatch (_, _, Some (SuffixMismatch (expected, found))))
      ->
        pf ppf
          "@[<v>The %s argument must be %s but got %s. These function types \
           are not compatible.@]"
          (index_str n) (suffix_str expected) (suffix_str found)
    | ArgError (n, TypeMismatch (expected, found, Some (InputMismatch err))) ->
        pf ppf
          "@[<v>The %s argument must be@, %a@ but got@, %a@ @[<v 2>These are \
           not compatible because:@ @[<hov>%a@]@]@]"
          (index_str n) (pp_fundef ctx) expected (pp_fundef ctx) found
          pp_explain_rec err
    | ArgError (n, TypeMismatch (expected, found, Some (ReturnTypeMismatch _)))
      ->
        pf ppf
          "@[<v>The %s argument must be@, %a@ but got@, %a@ The return types \
           are not compatible.@]"
          (index_str n) (pp_fundef ctx) expected (pp_fundef ctx) found
    | ArgNumMismatch (expected, found) ->
        pf ppf "Expected %d arguments but found %d arguments." expected found
  in
  let pp_args =
    pp_with_where ctx (fun ppf ->
        pf ppf "(@[<hov>%a@])" (list ~sep:comma (pp_unsized_type ctx)) )
  in
  let pp_signature ppf ((rt, args), err) =
    let fun_ty = UnsizedType.UFun (args, rt, FnPlain, AoS) in
    Fmt.pf ppf "%a@ @[<hov 2>  %a@]"
      (pp_with_where ctx (pp_fundef ctx))
      fun_ty pp_explain err
  in
  let pp_omitted ppf () =
    if omitted then pf ppf "@,(Additional signatures omitted)"
  in
  pf ppf
    "@[<v>Ill-typed arguments supplied to function '%s':@ %a@ Available \
     signatures:@ %a%a@]"
    name pp_args arg_tys
    (list ~sep:cut pp_signature)
    sigs pp_omitted ()
