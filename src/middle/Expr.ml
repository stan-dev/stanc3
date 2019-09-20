open Core_kernel
open Common
open Helpers

(** Pattern and fixed-point of MIR expressions *)
module Fixed = struct
  module Pattern = struct
    type litType = Int | Real | Str [@@deriving sexp, hash, compare]

    type 'a t =
      | Var of string
      | Lit of litType * string
      | FunApp of Fun_kind.t * string * 'a list
      | TernaryIf of 'a * 'a * 'a
      | EAnd of 'a * 'a
      | EOr of 'a * 'a
      | Indexed of 'a * 'a Index.t list
    [@@deriving sexp, hash, map, compare, fold]

    let pp pp_e ppf = function
      | Var varname -> Fmt.string ppf varname
      | Lit (Str, str) -> Fmt.pf ppf "%S" str
      | Lit (_, str) -> Fmt.string ppf str
      | FunApp (StanLib, name, [lhs; rhs])
        when Option.is_some (Operator.of_string_opt name) ->
          Fmt.pf ppf "(%a %a %a)" pp_e lhs Operator.pp
            (Option.value_exn (Operator.of_string_opt name))
            pp_e rhs
      | FunApp (_, name, args) ->
          Fmt.string ppf name ;
          Fmt.(list pp_e ~sep:Fmt.comma |> parens) ppf args
      | TernaryIf (pred, texpr, fexpr) ->
          Fmt.pf ppf {|@[%a@ %a@,%a@,%a@ %a@]|} pp_e pred pp_builtin_syntax "?"
            pp_e texpr pp_builtin_syntax ":" pp_e fexpr
      | Indexed (expr, indices) ->
          Fmt.pf ppf {|@[%a%a@]|} pp_e expr
            ( if List.is_empty indices then fun _ _ -> ()
            else Fmt.(list (Index.pp pp_e) ~sep:comma |> brackets) )
            indices
      | EAnd (l, r) -> Fmt.pf ppf "%a && %a" pp_e l pp_e r
      | EOr (l, r) -> Fmt.pf ppf "%a || %a" pp_e l pp_e r

    include Foldable.Make (struct type nonrec 'a t = 'a t

                                  let fold = fold
    end)
  end

  include Fixed.Make (Pattern)
end

(** Expressions without meta data *)
module NoMeta = struct
  module Meta = struct
    type t = unit [@@deriving compare, sexp, hash]

    let empty = ()
    let pp _ _ = ()
  end

  include Specialized.Make (Fixed) (Meta)

  let remove_meta x = Fixed.map (Fn.const ()) x
end

(** Expressions with associated location and type *)
module Typed = struct
  module Meta = struct
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t sexp_opaque [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype }
    [@@deriving compare, create, sexp, hash]

    let empty =
      create ~type_:UnsizedType.UInt ~adlevel:UnsizedType.DataOnly
        ~loc:Location_span.empty ()

    let adlevel {adlevel; _} = adlevel
    let type_ {type_; _} = type_
    let loc {loc; _} = loc
    let pp _ _ = ()
    let with_type ty meta = {meta with type_= ty}
  end

  include Specialized.Make (Fixed) (Meta)

  let type_of x = Meta.type_ @@ Fixed.meta_of x
  let loc_of x = Meta.loc @@ Fixed.meta_of x
  let adlevel_of x = Meta.adlevel @@ Fixed.meta_of x
end

(** Expressions with associated location, type and label *)
module Labelled = struct
  module Meta = struct
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t sexp_opaque [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype
      ; label: Label.Int_label.t [@compare.ignore] }
    [@@deriving compare, create, sexp, hash]

    let empty =
      create ~type_:UnsizedType.UInt ~adlevel:UnsizedType.DataOnly
        ~loc:Location_span.empty
        ~label:Label.Int_label.(prev init)
        ()

    let label {label; _} = label
    let adlevel {adlevel; _} = adlevel
    let type_ {type_; _} = type_
    let loc {loc; _} = loc
    let pp _ _ = ()
  end

  include Specialized.Make (Fixed) (Meta)

  let label_of x = Meta.label @@ Fixed.meta_of x
  let type_of x = Meta.type_ @@ Fixed.meta_of x
  let loc_of x = Meta.loc @@ Fixed.meta_of x
  let adlevel_of x = Meta.adlevel @@ Fixed.meta_of x

  (** Traverse a typed expression adding unique labels using locally mutable 
      state 
  *)
  let label ?(init = Label.Int_label.init) (expr : Typed.t) : t =
    let lbl = ref init in
    Fixed.map
      (fun Typed.Meta.({adlevel; type_; loc}) ->
        let cur_lbl = !lbl in
        lbl := Label.Int_label.next cur_lbl ;
        Meta.create ~label:cur_lbl ~adlevel ~type_ ~loc () )
      expr

  (** Build a map from expression labels to expressions *)
  let rec associate ?init:(assocs = Label.Int_label.Map.empty) (expr : t) =
    let assocs_result : t Label.Int_label.Map.t Map_intf.Or_duplicate.t =
      Label.Int_label.Map.add ~key:(label_of expr) ~data:expr
        (associate_pattern assocs @@ Fixed.pattern_of expr)
    in
    match assocs_result with `Ok x -> x | _ -> assocs

  and associate_pattern assocs = function
    | Fixed.Pattern.Lit _ | Var _ -> assocs
    | FunApp (_, _, args) ->
        List.fold args ~init:assocs ~f:(fun accu x -> associate ~init:accu x)
    | EAnd (e1, e2) | EOr (e1, e2) ->
        associate ~init:(associate ~init:assocs e2) e1
    | TernaryIf (e1, e2, e3) ->
        associate ~init:(associate ~init:(associate ~init:assocs e3) e2) e1
    | Indexed (e, idxs) ->
        List.fold idxs ~init:(associate ~init:assocs e) ~f:associate_index

  and associate_index assocs = function
    | All -> assocs
    | Single e | Upfrom e | MultiIndex e -> associate ~init:assocs e
    | Between (e1, e2) -> associate ~init:(associate ~init:assocs e2) e1
end

module Helpers = struct
  let int i =
    {Fixed.meta= Typed.Meta.empty; pattern= Lit (Int, string_of_int i)}

  let zero = int 0
  let one = int 1

  let binop e1 op e2 =
    { Fixed.meta= Typed.Meta.empty
    ; pattern= FunApp (StanLib, Operator.to_string op, [e1; e2]) }

  let loop_bottom = one

  let internal_funapp fn args meta =
    { Fixed.meta
    ; pattern= FunApp (CompilerInternal, Internal_fun.to_string fn, args) }

  let contains_fn fn ?(init = false) e =
    let fstr = Internal_fun.to_string fn in
    let rec aux accu e =
      accu
      ||
      match Fixed.pattern_of e with
      | FunApp (_, name, _) when name = fstr -> true
      | x -> Fixed.Pattern.fold aux accu x
    in
    aux init e

  let%test "expr contains fn" =
    internal_funapp FnReadData [] () |> contains_fn FnReadData

  let rec infer_type_of_indexed ut indices =
    match (ut, indices) with
    | _, [] -> ut
    | _, [Index.All] | _, [Upfrom _] | _, [Between _] -> ut
    | UnsizedType.UMatrix, [All; Single _]
     |UMatrix, [Upfrom _; Single _]
     |UMatrix, [Between _; Single _]
     |UMatrix, [MultiIndex _]
     |UMatrix, [Single _] ->
        UVector
    | UArray t, Single _ :: tl -> infer_type_of_indexed t tl
    | UArray t, _ :: tl -> UArray (infer_type_of_indexed t tl)
    | UMatrix, [Single _; Single _] | UVector, [_] | URowVector, [_] -> UReal
    | _ -> raise_s [%message "Can't index" (ut : UnsizedType.t)]

  (** [add_index expression index] returns an expression that (additionally)
      indexes into the input [expression] by [index].*)
  let add_int_index e i =
    let mtype = infer_type_of_indexed Typed.(type_of e) [i] in
    let meta = Typed.Meta.{e.meta with type_= mtype}
    and pattern =
      match Fixed.pattern_of e with
      | Var _ -> Fixed.Pattern.Indexed (e, [i])
      | Indexed (e, indices) -> Indexed (e, indices @ [i])
      | _ -> raise_s [%message "These should go away with Ryan's LHS"]
    in
    Fixed.fix (meta, pattern)

  (* let%expect_test "infer type of indexed" =
  [ (UnsizedType.UArray UMatrix, [Index.Single loop_bottom; Single loop_bottom])
  ; (UArray (UArray UMatrix), [Single loop_bottom])
  ; (UArray UMatrix, [Single loop_bottom])
  ; (UArray UMatrix, [Upfrom loop_bottom; Single loop_bottom])
  ; ( UArray UMatrix
    , [Single loop_bottom; Single loop_bottom; Single loop_bottom] )
  ; ( UArray UMatrix
    , [Upfrom loop_bottom; Single loop_bottom; Single loop_bottom] ) ]
  |> List.map ~f:(fun (ut, idx) -> infer_type_of_indexed ut idx)
  |> Fmt.(strf "@[<hov>%a@]" (list ~sep:comma UnsizedType.pp ))
  |> print_endline ;
  [%expect {|
    vector, matrix[], matrix, vector[], real, real[] |}]


  



(** [for_scalar unsizedtype...] generates a For statement that loops
    over the scalars in the underlying [unsizedtype].

    We can call [bodyfn] directly on scalars, make a direct For loop
    around Eigen types, or for Arrays we call mkfor but inserting a
    recursive call into the [bodyfn] that will operate on the nested
    type. In this way we recursively create for loops that loop over
    the outermost layers first.
*)
let rec for_scalar st bodyfn var smeta =
  match st with
  | SInt | SReal -> bodyfn var
  | SVector d | SRowVector d -> mkfor d bodyfn var smeta
  | SMatrix (d1, d2) ->
      mkfor d1 (fun e -> for_scalar (SRowVector d2) bodyfn e smeta) var smeta
  | SArray (t, d) -> mkfor d (fun e -> for_scalar t bodyfn e smeta) var smeta

(* Exactly like for_scalar, but iterating through array dimensions in the inverted order.*)
let for_scalar_inv st bodyfn var smeta =
  let invert_index_order = function
    | {expr= Indexed (obj, indices); emeta} ->
        {expr= Indexed (obj, List.rev indices); emeta}
    | e -> e
  in
  let rec go st bodyfn var smeta =
    match st with
    | SArray (t, d) ->
        let bodyfn' var = mkfor d bodyfn var smeta in
        go t bodyfn' var smeta
    | SMatrix (d1, d2) ->
        let bodyfn' var = mkfor d1 bodyfn var smeta in
        go (SRowVector d2) bodyfn' var smeta
    | _ -> for_scalar st bodyfn var smeta
  in
  go st (Fn.compose bodyfn invert_index_order) var smeta

let%expect_test "inverted for" =
  let int i = {expr= Lit (Int, string_of_int i); emeta= internal_meta} in
  let bodyfn var =
    {stmt= NRFunApp (StanLib, "print", [var]); smeta= no_span}
  in
  for_scalar_inv
    (SArray (SArray (SMatrix (int 2, int 3), int 5), int 4))
    bodyfn
    {expr= Var "hi"; emeta= {internal_meta with mtype= UArray (UArray UMatrix)}}
    no_span
  |> sexp_of_stmt_loc |> Sexp.to_string_hum |> print_endline ;
  [%expect
    {|
    (For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 3))
     (body
      (Block
       ((For (loopvar sym2__) (lower (Lit Int 1)) (upper (Lit Int 2))
         (body
          (Block
           ((For (loopvar sym3__) (lower (Lit Int 1)) (upper (Lit Int 5))
             (body
              (Block
               ((For (loopvar sym4__) (lower (Lit Int 1)) (upper (Lit Int 4))
                 (body
                  (Block
                   ((NRFunApp StanLib print
                     ((Indexed (Var hi)
                       ((Single (Var sym4__)) (Single (Var sym3__))
                        (Single (Var sym2__)) (Single (Var sym1__)))))))))))))))))))))) |}]

(** [for_eigen unsizedtype...] generates a For statement that loops
    over the eigen types in the underlying [unsizedtype]; i.e. just iterating
    overarrays and running bodyfn on any eign types found within.

    We can call [bodyfn] directly on scalars and Eigen types;
    for Arrays we call mkfor but insert a
    recursive call into the [bodyfn] that will operate on the nested
    type. In this way we recursively create for loops that loop over
    the outermost layers first.
*)
let rec for_eigen st bodyfn var smeta =
  match st with
  | SInt | SReal | SVector _ | SRowVector _ | SMatrix _ -> bodyfn var
  | SArray (t, d) -> mkfor d (fun e -> for_eigen t bodyfn e smeta) var smeta

let rec pull_indices {expr; _} =
  match expr with
  | Indexed (obj, indices) -> pull_indices obj @ indices
  | _ -> []

let assign_indexed decl_type vident smeta varfn var =
  let indices = pull_indices var in
  {stmt= Assignment ((vident, decl_type, indices), varfn var); smeta}

let rec eigen_size (st : mtype_loc_ad with_expr sizedtype) =
  match st with
  | SArray (t, _) -> eigen_size t
  | SMatrix (d1, d2) -> [d1; d2]
  | SRowVector dim | SVector dim -> [dim]
  | SInt | SReal -> []
  
  
  
  *)
end
