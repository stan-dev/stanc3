(** Utilities, primarily surrounding distribution names and suffixes *)

open Std

(** Name mangling helper functions for distributions *)
let unnormalized_suffices = ["_lupdf"; "_lupmf"]

let distribution_suffices = ["_lpmf"; "_lpdf"]

let conditioning_suffices =
  ["_lpdf"; "_lupdf"; "_lupmf"; "_lpmf"; "_cdf"; "_lcdf"; "_lccdf"]

let cumulative_distribution_suffices = ["cdf"; "lcdf"; "lccdf"]

let cumulative_distribution_suffices_w_rng =
  cumulative_distribution_suffices @ ["rng"]

let is_user_ident = Fun.negate (String.ends_with ~suffix:"__")

let unnormalized_suffix = function
  | "_lpdf" -> "_lupdf"
  | "_lpmf" -> "_lupmf"
  | x -> x

let split_distribution_suffix (name : string) : (string * string) option =
  String.split_last ~sep:"_" name

let is_distribution_name s =
  List.exists
    ~f:(fun suffix -> String.ends_with s ~suffix)
    (distribution_suffices @ unnormalized_suffices)

let is_unnormalized_distribution s =
  List.exists
    ~f:(fun suffix -> String.ends_with s ~suffix)
    unnormalized_suffices

let replace_unnormalized_suffix suffix ~name =
  name
  |> String.chop_suffix ~suffix:(unnormalized_suffix suffix)
  |> Option.map ~f:(fun x -> x ^ suffix)

let stdlib_distribution_name s =
  List.map ~f:(replace_unnormalized_suffix ~name:s) distribution_suffices
  |> List.filter_map ~f:Fun.id |> List.hd |> Option.value ~default:s

let normalized_name name =
  match name with
  | x when is_distribution_name x -> stdlib_distribution_name x
  | x -> x

let%expect_test "unnormalized name mangling" =
  stdlib_distribution_name "bernoulli_logit_lupmf" |> print_string;
  stdlib_distribution_name "normal_lupdf" |> ( ^ ) "; " |> print_string;
  stdlib_distribution_name "normal_lpdf" |> ( ^ ) "; " |> print_string;
  stdlib_distribution_name "normal" |> ( ^ ) "; " |> print_string;
  [%expect {| bernoulli_logit_lpmf; normal_lpdf; normal_lpdf; normal |}]

(* Utilities for using Tuples and Transformations together *)
let tuple_trans_exn = function
  | Transformation.TupleTransformation transforms -> transforms
  | t ->
      Common.ICE.(
        internal_errorf "Expected TupleTransformation but got %t"
          [Transformation.pp Expr.Typed.pp $ t]) [@coverage off]

let zip_subtypes_tuple_trans_exn sts trans =
  match trans with
  | Transformation.Identity ->
      List.map ~f:(fun s -> (s, Transformation.Identity)) sts
  | _ ->
      let tms = tuple_trans_exn trans in
      List.combine sts tms

let zip_stuple_trans_exn pst tms =
  let rec tuple_subtypes pst =
    match pst with
    | SizedType.STuple subtypes -> subtypes
    | SArray (st, _) -> tuple_subtypes st
    | _ ->
        Common.ICE.internal_error
          "Internal error: expected Tuple with TupleTransformation"
        [@coverage off] in
  let psts = tuple_subtypes pst in
  List.combine psts tms

let zip_utuple_trans_exn pst tms =
  let rec tuple_psts pst =
    match pst with
    | UnsizedType.UTuple uts -> uts
    | UArray ut -> tuple_psts ut
    | _ ->
        Common.ICE.internal_error
          "Internal error: expected Tuple with TupleTransformation"
        [@coverage off] in
  let psts = tuple_psts pst in
  List.combine psts tms
