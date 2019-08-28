open Core_kernel
open Middle
open Fmt

let is_multi_index = function
  | MultiIndex _ -> true
  | _ -> false

let rec pp_expr ppf {expr; _} = match expr with
  | Var ident -> string ppf ident
  | Lit (Str, s) -> pf ppf "%S" s
  | Lit (_, s) -> string ppf s
  | FunApp (_, fname, args) ->
    pf ppf "%s(@[<hov>%a@])" fname (list ~sep:comma pp_expr) args
  | TernaryIf (cond, iftrue, iffalse) ->
    pf ppf "%a if %a else %a" pp_expr cond pp_expr iftrue pp_expr iffalse
  | EAnd (a, b) -> pf ppf "%a and %a" pp_expr a pp_expr b
  | EOr (a, b) -> pf ppf "%a or %a" pp_expr a pp_expr b
  | Indexed (_, indices)
    when List.exists ~f:is_multi_index indices ->
    (*
       TF indexing options:
       * tf.slice
       * tf.gather
       * tf.gather_nd
       * tf.strided_slice
*)
    raise_s [%message "Multi-indices not supported yet"]
  | Indexed (obj, indices) ->
    pf ppf "%a[%a]" pp_expr obj
      (list ~sep:comma (Middle.Pretty.pp_index pp_expr)) indices

let dist_prefix = "tfd.distributions."
let remove_stan_dist_suffix s =
  List.filter_map ("_rng" :: Utils.distribution_suffices @ [""])
    ~f:(fun suffix -> String.chop_suffix ~suffix s)
  |> List.hd_exn

let pass_through_fnames = String.Set.of_list ["normal"]

let map_functions fname args = match fname with
  | "multi_normal_cholesky" -> "MultivariateNormalTriL", args
  | _ ->
    if Set.mem pass_through_fnames fname then fname, args else
      raise_s [%message "Not sure how to handle " fname " yet!"]

let rec translate_funapps {expr; emeta} =
  let expr = match expr with
  | FunApp(StanLib, fname, args) ->
    let prefix = if Utils.is_distribution_name fname then dist_prefix else "" in
    let fname = remove_stan_dist_suffix fname in
    let fname, args = map_functions fname args in
    FunApp(StanLib, prefix ^ fname, args)
  | _ -> map_expr translate_funapps expr
  in
  {expr; emeta}

let trans_prog (p: typed_prog) = map_prog translate_funapps Fn.id p
