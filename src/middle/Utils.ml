(** Utilities, primarily surrounding distribution names and suffixes *)

open Core

let option_or_else ~if_none x = Option.first_some x if_none

(** Name mangling helper functions for distributions *)
let unnormalized_suffices = ["_lupdf"; "_lupmf"]

let distribution_suffices = ["_lpmf"; "_lpdf"]

let conditioning_suffices =
  ["_lpdf"; "_lupdf"; "_lupmf"; "_lpmf"; "_cdf"; "_lcdf"; "_lccdf"]

let cumulative_distribution_suffices =
  ["cdf"; "lcdf"; "lccdf"; "cdf_log"; "ccdf_log"]

let cumulative_distribution_suffices_w_rng =
  cumulative_distribution_suffices @ ["rng"]

let is_user_ident = Fn.non (String.is_suffix ~suffix:"__")

let unnormalized_suffix = function
  | "_lpdf" -> "_lupdf"
  | "_lpmf" -> "_lupmf"
  | x -> x

(** A wrapper around String.rsplit2 which handles _cdf_log and _ccdf_log *)
let split_distribution_suffix (name : string) : (string * string) option =
  let open String in
  if is_suffix ~suffix:"_cdf_log" name then Some (drop_suffix name 8, "cdf_log")
  else if is_suffix ~suffix:"_ccdf_log" name then
    Some (drop_suffix name 9, "ccdf_log")
  else rsplit2 ~on:'_' name

let is_distribution_name s =
  (not
     (String.is_suffix s ~suffix:"_cdf_log"
     || String.is_suffix s ~suffix:"_ccdf_log"))
  && List.exists
       ~f:(fun suffix -> String.is_suffix s ~suffix)
       (distribution_suffices @ unnormalized_suffices)

let is_unnormalized_distribution s =
  List.exists
    ~f:(fun suffix -> String.is_suffix s ~suffix)
    unnormalized_suffices

let replace_unnormalized_suffix suffix ~name =
  name
  |> String.chop_suffix ~suffix:(unnormalized_suffix suffix)
  |> Option.map ~f:(fun x -> x ^ suffix)

let stdlib_distribution_name s =
  List.map ~f:(replace_unnormalized_suffix ~name:s) distribution_suffices
  |> List.filter_opt |> List.hd |> Option.value ~default:s

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

let all_but_last_n l n =
  List.fold_right l ~init:([], n) ~f:(fun ele (accum, n) ->
      if n = 0 then (ele :: accum, n) else (accum, n - 1))
  |> fst

let%expect_test "all but last n" =
  let l = all_but_last_n [1; 2; 3; 4] 2 in
  print_s [%sexp (l : int list)];
  [%expect {| (1 2) |}]

(* Utilities for using Tuples and Transformations together *)
let tuple_trans_exn = function
  | Transformation.TupleTransformation transforms -> transforms
  | t ->
      Common.FatalError.fatal_error_msg
        [%message
          "Expected TupleTransformation but got"
            (t : Expr.Typed.t Transformation.t)]

let zip_stuple_trans_exn pst tms =
  let rec tuple_subtypes pst =
    match pst with
    | SizedType.STuple subtypes -> subtypes
    | SArray (st, _) -> tuple_subtypes st
    | _ ->
        Common.FatalError.fatal_error_msg
          [%message "Internal error: expected Tuple with TupleTransformation"]
  in
  let psts = tuple_subtypes pst in
  List.zip_exn psts tms

let zip_utuple_trans_exn pst tms =
  let rec tuple_psts pst =
    match pst with
    | UnsizedType.UTuple uts -> uts
    | UArray ut -> tuple_psts ut
    | _ ->
        Common.FatalError.fatal_error_msg
          [%message "Internal error: expected Tuple with TupleTransformation"]
  in
  let psts = tuple_psts pst in
  List.zip_exn psts tms
