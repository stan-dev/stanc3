(** Types for function kinds, e.g. [StanLib] or [UserDefined], and function
    suffix types, e.g. [foo_ldfp], [bar_lp] *)

open Std
open Std.Compare
open Std.Sexp_conv

type propto = Normalized | Unnormalized
and lpxf = Density | Mass [@@deriving compare, map, sexp_of, equal]

type 'propto suffix =
  | FnPlain
  | FnRng
  | FnLpxf of lpxf * 'propto
  | FnTarget
  | FnJacobian
[@@deriving compare, map, sexp_of, equal]

let without_propto = map_suffix (Fun.const () : propto -> unit)

type 'e t =
  | StanLib of string * propto suffix
  | Operator of Operator.t
  | CompilerInternal of 'e Internal_fun.t
  | UserDefined of string * propto suffix
[@@deriving compare, sexp_of, map, fold]

let suffix_from_name fname =
  let is_suffix suffix = String.ends_with ~suffix fname in
  if is_suffix "_rng" then FnRng
  else if is_suffix "_lp" then FnTarget
  else if is_suffix "_jacobian" then FnJacobian
  else if is_suffix "_lupdf" then FnLpxf (Density, Unnormalized)
  else if is_suffix "_lupmf" then FnLpxf (Mass, Unnormalized)
  else if is_suffix "_lpdf" then FnLpxf (Density, Normalized)
  else if is_suffix "_lpmf" then FnLpxf (Mass, Normalized)
  else FnPlain

let with_unnormalized_suffix (name : string) =
  Option.first_some
    (String.chop_suffix ~suffix:"_lpdf" name
    |> Option.map ~f:(fun n -> n ^ "_lupdf"))
    (String.chop_suffix ~suffix:"_lpmf" name
    |> Option.map ~f:(fun n -> n ^ "_lupmf"))

let pp pp_expr ppf kind =
  match kind with
  | StanLib (s, FnLpxf (_, Unnormalized))
   |UserDefined (s, FnLpxf (_, Unnormalized)) ->
      Fmt.string ppf (with_unnormalized_suffix s |> Option.value ~default:s)
  | StanLib (s, _) | UserDefined (s, _) -> Fmt.string ppf s
  | Operator op -> Operator.pp ppf op
  | CompilerInternal internal -> Internal_fun.pp pp_expr ppf internal

let collect_exprs fn = fold (fun accum e -> e :: accum) [] fn
