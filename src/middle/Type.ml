(** A type which unifies [SizedTypes] and [UnsizedTypes] for declarations *)

open Common

type 'a t = Sized of 'a SizedType.t | Unsized of UnsizedType.t
[@@deriving sexp, compare, map, hash, fold]

let pp pp_e ppf = function
  | Sized st -> SizedType.pp pp_e ppf st
  | Unsized ust -> UnsizedType.pp ppf ust

let collect_exprs = function Sized st -> SizedType.collect_exprs st | _ -> []

let to_unsized = function
  | Sized st -> SizedType.to_unsized st
  | Unsized ut -> ut

let associate ?init:(assocs = Label.Int_label.Map.empty) = function
  | Sized st -> SizedType.associate ~init:assocs st
  | Unsized _ -> assocs
