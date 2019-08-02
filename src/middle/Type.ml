type 'a t = 'a Mir_pattern.possiblysizedtype =
  | Sized of 'a SizedType.t
  | Unsized of UnsizedType.t
[@@deriving sexp, compare, map, hash, fold]

let pp pp_e ppf x = Mir_pretty_printer.pp_possiblysizedtype pp_e ppf x 

module Make_traversable = Mir_pattern.Make_traversable_possiblysizedtype
module Make_traversable2 = Mir_pattern.Make_traversable_possiblysizedtype2

(** Collect any expressions within a `possiblysizedtype` *)
let collect_exprs = function Sized st -> SizedType.collect_exprs st | _ -> []

let to_unsizedtype = function
  | Sized st -> SizedType.to_unsizedtype st
  | Unsized ut -> ut

let associate ?init:(assocs = Label.Map.empty) = function
  | Sized st -> SizedType.associate ~init:assocs st 
  | _ -> assocs 
