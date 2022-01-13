open Core_kernel
open Core_kernel.Poly
open Common
open Helpers

(** Pattern and fixed-point of MIR expressions *)
module Fixed = struct
  module Pattern = struct
    type litType = Int | Real | Imaginary | Str
    [@@deriving sexp, hash, compare]

    type 'a t =
      | Var of string
      | Lit of litType * string
      | FunApp of 'a Fun_kind.t * 'a list
      | TernaryIf of 'a * 'a * 'a
      | EAnd of 'a * 'a
      | EOr of 'a * 'a
      | Indexed of 'a * 'a Index.t list
      | Promotion of 'a * UnsizedType.t * UnsizedType.autodifftype
    [@@deriving sexp, hash, map, compare, fold]

    let pp pp_e ppf = function
      | Var varname -> Fmt.string ppf varname
      | Lit (Str, str) -> Fmt.pf ppf "%S" str
      | Lit (_, str) -> Fmt.string ppf str
      | FunApp (StanLib (name, FnPlain, _), [lhs; rhs])
        when Option.is_some (Operator.of_string_opt name) ->
          Fmt.pf ppf "(%a %a %a)" pp_e lhs Operator.pp
            (Option.value_exn (Operator.of_string_opt name))
            pp_e rhs
      | FunApp (fun_kind, args) ->
          Fmt.pf ppf "%a(%a)" (Fun_kind.pp pp_e) fun_kind
            Fmt.(list pp_e ~sep:Fmt.comma)
            args
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
      | Promotion (from, ut, _) ->
          Fmt.pf ppf "%a -> %a" pp_e from UnsizedType.pp ut

    include Foldable.Make (struct
      type nonrec 'a t = 'a t

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

  let remove_meta expr = Fixed.map (Fn.const ()) expr
end

(** Expressions with associated location and type *)
module Typed = struct
  module Meta = struct
    type t =
      { type_: UnsizedType.t
      ; loc: (Location_span.t[@sexp.opaque] [@compare.ignore])
      ; adlevel: UnsizedType.autodifftype }
    [@@deriving compare, create, sexp, hash]

    let empty =
      create ~type_:UnsizedType.UInt ~adlevel:UnsizedType.DataOnly
        ~loc:Location_span.empty ()

    let pp _ _ = ()
  end

  include Specialized.Make (Fixed) (Meta)

  let type_of Fixed.{meta= Meta.{type_; _}; _} = type_
  let loc_of Fixed.{meta= Meta.{loc; _}; _} = loc
  let adlevel_of Fixed.{meta= Meta.{adlevel; _}; _} = adlevel
  let fun_arg Fixed.{meta= Meta.{type_; adlevel; _}; _} = (adlevel, type_)
end

(** Expressions with associated location, type and label *)
module Labelled = struct
  module Meta = struct
    type t =
      { type_: UnsizedType.t
      ; loc: (Location_span.t[@sexp.opaque] [@compare.ignore])
      ; adlevel: UnsizedType.autodifftype
      ; label: Label.Int_label.t [@compare.ignore] }
    [@@deriving compare, create, sexp, hash]

    let empty =
      create ~type_:UnsizedType.UInt ~adlevel:UnsizedType.DataOnly
        ~loc:Location_span.empty
        ~label:Label.Int_label.(prev init)
        ()

    let pp _ _ = ()
  end

  include Specialized.Make (Fixed) (Meta)

  let type_of Fixed.{meta= Meta.{type_; _}; _} = type_
  let label_of Fixed.{meta= Meta.{label; _}; _} = label
  let adlevel_of Fixed.{meta= Meta.{adlevel; _}; _} = adlevel
  let loc_of Fixed.{meta= Meta.{loc; _}; _} = loc

  (** Traverse a typed expression adding unique labels using locally mutable
      state
  *)
  let label ?(init = Label.Int_label.init) (expr : Typed.t) : t =
    let lbl = ref init in
    Fixed.map
      (fun Typed.Meta.{adlevel; type_; loc} ->
        let cur_lbl = !lbl in
        lbl := Label.Int_label.next cur_lbl ;
        Meta.create ~label:cur_lbl ~adlevel ~type_ ~loc () )
      expr

  (** Build a map from expression labels to expressions *)
  let rec associate ?init:(assocs = Label.Int_label.Map.empty)
      ({pattern; _} as expr : t) =
    let assocs_result : t Label.Int_label.Map.t Map_intf.Or_duplicate.t =
      Label.Int_label.Map.add ~key:(label_of expr) ~data:expr
        (associate_pattern assocs @@ pattern) in
    match assocs_result with `Ok x -> x | `Duplicate -> assocs

  and associate_pattern assocs = function
    | Fixed.Pattern.Lit _ | Var _ -> assocs
    | FunApp (_, args) ->
        List.fold args ~init:assocs ~f:(fun accu x -> associate ~init:accu x)
    | EAnd (e1, e2) | EOr (e1, e2) ->
        associate ~init:(associate ~init:assocs e2) e1
    | TernaryIf (e1, e2, e3) ->
        associate ~init:(associate ~init:(associate ~init:assocs e3) e2) e1
    | Indexed (e, idxs) ->
        List.fold idxs ~init:(associate ~init:assocs e) ~f:associate_index
    (* Not sure?*)
    | Promotion (e1, _, _) -> associate ~init:assocs e1

  and associate_index assocs = function
    | All -> assocs
    | Single e | Upfrom e | MultiIndex e -> associate ~init:assocs e
    | Between (e1, e2) -> associate ~init:(associate ~init:assocs e2) e1
end

module Helpers = struct
  let int i = {Fixed.meta= Typed.Meta.empty; pattern= Lit (Int, string_of_int i)}

  let float i =
    {Fixed.meta= Typed.Meta.empty; pattern= Lit (Real, string_of_float i)}

  let str i = {Fixed.meta= Typed.Meta.empty; pattern= Lit (Str, i)}
  let variable v = {Fixed.meta= Typed.Meta.empty; pattern= Var v}
  let zero = int 0
  let one = int 1

  let binop e1 op e2 =
    { Fixed.meta= Typed.Meta.empty
    ; pattern= FunApp (StanLib (Operator.to_string op, FnPlain, AoS), [e1; e2])
    }

  let binop_list es op ~default =
    match es with
    | [] -> default
    | head :: rest ->
        List.fold ~init:head ~f:(fun accum next -> binop accum op next) rest

  let loop_bottom = one

  let internal_funapp fn args meta =
    {Fixed.meta; pattern= FunApp (CompilerInternal fn, args)}

  let contains_fn_kind is_fn_kind ?(init = false) e =
    let rec aux accu Fixed.{pattern; _} =
      accu
      ||
      match pattern with
      | FunApp (kind, _) when is_fn_kind kind -> true
      | x -> Fixed.Pattern.fold aux accu x in
    aux init e

  let%test "expr contains fn" =
    internal_funapp FnReadData [] ()
    |> contains_fn_kind (fun kind -> kind = CompilerInternal FnReadData)

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
    | _ ->
        FatalError.fatal_error_msg [%message "Can't index" (ut : UnsizedType.t)]

  (** [add_index expression index] returns an expression that (additionally)
      indexes into the input [expression] by [index].*)
  let add_int_index e i =
    let mtype = infer_type_of_indexed Typed.(type_of e) [i] in
    let meta = Typed.Meta.{e.meta with type_= mtype}
    and pattern =
      match e.pattern with
      | Var _ -> Fixed.Pattern.Indexed (e, [i])
      | Indexed (e, indices) -> Indexed (e, indices @ [i])
      | _ ->
          (* These should go away with Ryan's LHS *)
          Common.FatalError.fatal_error_msg
            [%message
              "Expected Var or Indexed but found " (e : Typed.Meta.t Fixed.t)]
    in
    Fixed.{meta; pattern}

  (** TODO: Make me tail recursive *)
  let rec collect_indices Fixed.{pattern; _} =
    match pattern with
    | Indexed (obj, indices) -> collect_indices obj @ indices
    | _ -> []

  let%expect_test "infer type of indexed" =
    [ ( UnsizedType.UArray UMatrix
      , [Index.Single loop_bottom; Single loop_bottom] )
    ; (UArray (UArray UMatrix), [Single loop_bottom])
    ; (UArray UMatrix, [Single loop_bottom])
    ; (UArray UMatrix, [Upfrom loop_bottom; Single loop_bottom])
    ; ( UArray UMatrix
      , [Single loop_bottom; Single loop_bottom; Single loop_bottom] )
    ; ( UArray UMatrix
      , [Upfrom loop_bottom; Single loop_bottom; Single loop_bottom] ) ]
    |> List.map ~f:(fun (ut, idx) -> infer_type_of_indexed ut idx)
    |> Fmt.(str "@[<hov>%a@]" (list ~sep:comma UnsizedType.pp))
    |> print_endline ;
    [%expect
      {|
      vector, array[] matrix, matrix, array[] vector, real, array[] real |}]
end
