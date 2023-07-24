(** Types for function kinds, e.g. [StanLib] or [UserDefined], and
  function suffix types, e.g. [foo_ldfp], [bar_lp]
*)

open Core_kernel

type 'propto suffix = FnPlain | FnRng | FnLpdf of 'propto | FnTarget
[@@deriving compare, hash, fold, map, sexp, equal]

let without_propto = map_suffix (function true | false -> ())

type 'e t =
  | StanLib of string * bool suffix * Mem_pattern.t
  | CompilerInternal of 'e Internal_fun.t
  | UserDefined of string * bool suffix
[@@deriving compare, sexp, hash, map, fold]

let suffix_from_name fname =
  let is_suffix suffix = Core_kernel.String.is_suffix ~suffix fname in
  if is_suffix "_rng" then FnRng
  else if is_suffix "_lp" then FnTarget
  else if is_suffix "_lupdf" || is_suffix "_lupmf" then FnLpdf true
  else if is_suffix "_lpdf" || is_suffix "_lpmf" then FnLpdf false
  else FnPlain

let with_unnormalized_suffix (name : string) =
  Option.first_some
    ( String.chop_suffix ~suffix:"_lpdf" name
    |> Option.map ~f:(fun n -> n ^ "_lupdf") )
    ( String.chop_suffix ~suffix:"_lpmf" name
    |> Option.map ~f:(fun n -> n ^ "_lupmf") )

let pp pp_expr ppf kind =
  match kind with
  | StanLib (s, FnLpdf true, _) | UserDefined (s, FnLpdf true) ->
      Fmt.string ppf (with_unnormalized_suffix s |> Option.value ~default:s)
  | StanLib (s, _, _) | UserDefined (s, _) -> Fmt.string ppf s
  | CompilerInternal internal -> Internal_fun.pp pp_expr ppf internal

let collect_exprs fn = fold (fun accum e -> e :: accum) [] fn
