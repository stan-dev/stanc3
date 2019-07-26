open Core_kernel
open Common
open State.Cps

type litType = Mir_pattern.litType = Int | Real | Str
[@@deriving sexp, hash, compare]

type 'a index = 'a Mir_pattern.index =
  | All
  | Single of 'a
  | Upfrom of 'a
  | Between of 'a * 'a
  | MultiIndex of 'a
[@@deriving sexp, hash, map, compare, fold]

module Pattern = struct
  type 'a t = 'a Mir_pattern.expr =
    | Var of string
    | Lit of litType * string
    | FunApp of Fun_kind.t * string * 'a list
    | TernaryIf of 'a * 'a * 'a
    | EAnd of 'a * 'a
    | EOr of 'a * 'a
    | Indexed of 'a * 'a index list
  [@@deriving sexp, hash, map, compare, fold]

  let map f x = Mir_pattern.map_expr f x
  let fold f init x = Mir_pattern.fold_expr f init x
  let compare f x y = Mir_pattern.compare_expr f x y
  let hash_fold_t = Mir_pattern.hash_fold_expr
  let sexp_of_t = Mir_pattern.sexp_of_expr
  let t_of_sexp = Mir_pattern.expr_of_sexp
  let pp pp_e ppf = Mir_pretty_printer.pp_expr pp_e ppf

  include Foldable.Make (struct type nonrec 'a t = 'a t

                                let fold = fold
  end)

  module Make_traversable = Mir_pattern.Make_traversable_expr
  module Make_traversable2 = Mir_pattern.Make_traversable_expr2
end

(** Fixed-point of `expr` *)
module Fixed = Fix.Make (Pattern)

(** Fixed-point expressions specialized to have no meta data *)
module NoMeta = struct
  module Meta = struct
    type t = unit [@@deriving compare, sexp, hash]

    let pp _ _ = ()
  end

  include Specialized.Make (Fixed) (Meta)

  let remove_meta x = Fixed.map (fun _ -> ()) x
end

module Typed = struct
  module Meta = struct
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t sexp_opaque [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype }
    [@@deriving compare, create, sexp, hash]

    let adlevel {adlevel; _} = adlevel
    let type_ {type_; _} = type_
    let loc {loc; _} = loc
    let pp _ _ = ()
  end

  include Specialized.Make (Fixed) (Meta)

  let type_of x = Meta.type_ @@ Fixed.meta x
  let loc_of x = Meta.loc @@ Fixed.meta x
  let adlevel_of x = Meta.adlevel @@ Fixed.meta x
end

module Labelled = struct
  module Meta = struct
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t sexp_opaque [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype
      ; label: Label.t [@compare.ignore] }
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

  module Traversable_state = Fixed.Make_traversable2 (State)

  let label ?(init = 0) (expr : Typed.t) : t =
    let f {Typed.Meta.adlevel; type_; loc} =
      State.(
        get
        >>= fun label ->
        put (label + 1)
        >>= fun _ -> return @@ Meta.create ~label ~adlevel ~type_ ~loc ())
    in
    Traversable_state.traverse ~f expr |> State.run_state ~init |> fst

  let rec associate ?init:(assocs = Label.Map.empty) (expr : t) =
    Label.Map.add_exn
      (associate_pattern assocs @@ Fixed.pattern expr)
      ~key:(label_of expr) ~data:expr

  and associate_pattern assocs = function
    | Mir_pattern.Lit _ | Var _ -> assocs
    | FunApp (_, _, args) ->
        List.fold args ~init:assocs ~f:(fun accu x -> associate ~init:accu x)
    | EAnd (e1, e2) | EOr (e1, e2) ->
        associate ~init:(associate ~init:assocs e2) e1
    | TernaryIf (e1, e2, e3) ->
        associate ~init:(associate ~init:(associate ~init:assocs e3) e2) e1
    | Indexed (e, idxs) ->
        List.fold idxs ~init:(associate ~init:assocs e) ~f:associate_index

  and associate_index assocs = function
    | Mir_pattern.All -> assocs
    | Single e | Upfrom e | MultiIndex e -> associate ~init:assocs e
    | Between (e1, e2) -> associate ~init:(associate ~init:assocs e2) e1
end

include Fixed

(* == Helpers ============================================================= *)

let var meta name = fix meta @@ Pattern.Var name
let lit_int meta value = fix meta @@ Pattern.Lit (Int, string_of_int value)
let lit_real meta value = fix meta @@ Pattern.Lit (Real, string_of_float value)
let lit_string meta value = fix meta @@ Pattern.Lit (Str, value)

let if_ meta pred true_expr false_expr =
  fix meta @@ Pattern.TernaryIf (pred, true_expr, false_expr)

let and_ meta e1 e2 = fix meta @@ Pattern.EAnd (e1, e2)
let or_ meta e1 e2 = fix meta @@ Pattern.EOr (e1, e2)
let all = All
let single e = Single e
let multi e = MultiIndex e
let upfrom e = Upfrom e
let between e1 e2 = Between (e1, e2)
let indexed meta e idxs = fix meta @@ Pattern.Indexed (e, idxs)

let indexed_bounds = function
  | All -> []
  | Single e | MultiIndex e | Upfrom e -> [e]
  | Between (e1, e2) -> [e1; e2]

let compiler_fun meta name args =
  fix meta @@ Pattern.(FunApp (CompilerInternal, name, args))

let user_fun meta name args =
  fix meta @@ Pattern.(FunApp (UserDefined, name, args))

let stanlib_fun meta name args =
  fix meta @@ Pattern.(FunApp (StanLib, name, args))

(* TODO: use operators instead?*)
let plus meta a b = stanlib_fun meta "plus" [a; b]
let gt meta a b = stanlib_fun meta "gt" [a; b]
