open Core_kernel

let option_or_else ~if_none x = Option.first_some x if_none

(* Name mangling helper functions for distributions *)
let proportional_to_distribution_infix = "_propto"
let distribution_suffices = ["_lpmf"; "_lpdf"; "_log"]

let propto_suffices =
  List.map
    ~f:(fun x -> proportional_to_distribution_infix ^ x)
    distribution_suffices

let is_user_ident = Fn.non (String.is_suffix ~suffix:"__")

let is_distribution_name s =
  (not
     ( String.is_suffix s ~suffix:"_cdf_log"
     || String.is_suffix s ~suffix:"_ccdf_log" ))
  && List.exists
       ~f:(fun suffix -> String.is_suffix s ~suffix)
       distribution_suffices

let is_propto_distribution s =
  List.exists ~f:(fun suffix -> String.is_suffix s ~suffix) propto_suffices

let remove_propto_infix suffix ~name =
  name
  |> String.chop_suffix ~suffix:(proportional_to_distribution_infix ^ suffix)
  |> Option.map ~f:(fun x -> x ^ suffix)

let stdlib_distribution_name s =
  List.map ~f:(remove_propto_infix ~name:s) distribution_suffices
  |> List.filter_opt |> List.hd |> Option.value ~default:s

let%expect_test "propto name mangling" =
  stdlib_distribution_name "bernoulli_logit_propto_lpmf" |> print_string ;
  stdlib_distribution_name "normal_propto_lpdf" |> ( ^ ) "; " |> print_string ;
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
