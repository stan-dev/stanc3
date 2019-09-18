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

  (** Fixed-point of `expr` *)
  include Fixed.Make (Pattern)
end

(** Expressions without meta data *)
module NoMeta = struct
  module Meta = struct
    type t = unit [@@deriving compare, sexp, hash]

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

  let type_of x = Meta.type_ @@ Fixed.meta x
  let loc_of x = Meta.loc @@ Fixed.meta x
  let adlevel_of x = Meta.adlevel @@ Fixed.meta x
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

    let label {label; _} = label
    let adlevel {adlevel; _} = adlevel
    let type_ {type_; _} = type_
    let loc {loc; _} = loc
    let pp _ _ = ()
  end

  include Specialized.Make (Fixed) (Meta)

  let label_of x = Meta.label @@ Fixed.meta x
  let type_of x = Meta.type_ @@ Fixed.meta x
  let loc_of x = Meta.loc @@ Fixed.meta x
  let adlevel_of x = Meta.adlevel @@ Fixed.meta x

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
        (associate_pattern assocs @@ Fixed.pattern expr)
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
