type 'a t = 'a Mir_pattern.possiblysizedtype =
  | Sized of 'a SizedType.t
  | Unsized of UnsizedType.t
[@@deriving sexp, compare, map, hash, fold]

let pp pp_e ppf = function 
  | Sized st -> SizedType.pp pp_e ppf st
  | Unsized ust -> UnsizedType.pp ppf ust

module Make_traversable = Mir_pattern.Make_traversable_possiblysizedtype
module Make_traversable2 = Mir_pattern.Make_traversable_possiblysizedtype2

(** Collect any expressions within a `possiblysizedtype` *)
let collect_exprs = function Sized st -> SizedType.collect_exprs st | _ -> []

let to_unsizedtype = function
  | Sized st -> SizedType.to_unsizedtype st
  | Unsized ut -> ut

let associate ?init:(assocs = Int_label.Map.empty) = function
  | Sized st -> SizedType.associate ~init:assocs st
  | _ -> assocs
