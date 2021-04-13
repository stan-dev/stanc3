open Core_kernel

let option_or_else ~if_none x = Option.first_some x if_none

(* Name mangling helper functions for distributions *)
let unnormalized_suffices = ["_lupdf"; "_lupmf"]

(* _log is listed last so that it only gets picked up if no other implementation exists *)
let distribution_suffices = ["_lpmf"; "_lpdf"; "_log"]

let conditioning_suffices =
  ["_lpdf"; "_lupdf"; "_lupmf"; "_lpmf"; "_lcdf"; "_lccdf"]

let conditioning_suffices_w_log = conditioning_suffices @ ["_log"]
let is_user_ident = Fn.non (String.is_suffix ~suffix:"__")

let unnormalized_suffix = function
  | "_lpdf" -> "_lupdf"
  | "_lpmf" -> "_lupmf"
  | x -> x

let is_distribution_name s =
  (not
     ( String.is_suffix s ~suffix:"_cdf_log"
     || String.is_suffix s ~suffix:"_ccdf_log" ))
  && List.exists
       ~f:(fun suffix -> String.is_suffix s ~suffix)
       (distribution_suffices @ unnormalized_suffices)

let is_unnormalized_distribution s =
  List.exists
    ~f:(fun suffix -> String.is_suffix s ~suffix)
    unnormalized_suffices

let with_unnormalized_suffix (name : string) =
  Option.merge
    ~f:(fun x _ -> x)
    ( String.chop_suffix ~suffix:"_lpdf" name
    |> Option.map ~f:(fun n -> n ^ "_lupdf") )
    ( String.chop_suffix ~suffix:"_lpmf" name
    |> Option.map ~f:(fun n -> n ^ "_lupmf") )

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
  stdlib_distribution_name "bernoulli_logit_lupmf" |> print_string ;
  stdlib_distribution_name "normal_lupdf" |> ( ^ ) "; " |> print_string ;
  stdlib_distribution_name "normal_lpdf" |> ( ^ ) "; " |> print_string ;
  stdlib_distribution_name "normal" |> ( ^ ) "; " |> print_string ;
  [%expect {| bernoulli_logit_lpmf; normal_lpdf; normal_lpdf; normal |}]

let all_but_last_n l n =
  List.fold_right l ~init:([], n) ~f:(fun ele (accum, n) ->
      if n = 0 then (ele :: accum, n) else (accum, n - 1) )
  |> fst

let%expect_test "all but last n" =
  let l = all_but_last_n [1; 2; 3; 4] 2 in
  print_s [%sexp (l : int list)] ;
  [%expect {| (1 2) |}]
