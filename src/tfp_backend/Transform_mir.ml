open Core_kernel
open Middle

let dist_prefix = "tfd__."

let remove_stan_dist_suffix s =
  let s = Utils.stdlib_distribution_name s in
  List.filter_map
    (("_rng" :: Utils.distribution_suffices) @ [""])
    ~f:(fun suffix -> String.chop_suffix ~suffix s)
  |> List.hd_exn

let capitalize_fnames = String.Set.of_list ["normal"; "cauchy"]

let map_functions fname args =
  match fname with
  | "multi_normal_cholesky" -> ("MultivariateNormalTriL", args)
  | f when operator_of_string f |> Option.is_some -> (fname, args)
  | _ ->
      if Set.mem capitalize_fnames fname then (String.capitalize fname, args)
      else raise_s [%message "Not sure how to handle " fname " yet!"]

let map_constraints args emeta =
  match args with
  | [var; {expr= Lit (Str, "lb"); _}; lb] ->
      let f = {expr= FunApp (StanLib, "tf.exp", [var]); emeta} in
      binop f Plus lb
  | _ ->
      raise_s
        [%message "No constraint mapping for" (args : expr_typed_located list)]

let rec translate_funapps {expr; emeta} =
  let e expr = {expr; emeta} in
  match expr with
  | FunApp (CompilerInternal, fname, args)
    when internal_fn_of_string fname = Some FnConstrain ->
      map_constraints args emeta
  | FunApp (StanLib, fname, args) ->
      let prefix =
        if Utils.is_distribution_name fname then dist_prefix else ""
      in
      let fname = remove_stan_dist_suffix fname in
      let fname, args = map_functions fname args in
      FunApp (StanLib, prefix ^ fname, args) |> e
  | _ -> map_expr translate_funapps expr |> e

let trans_prog (p : typed_prog) =
  let rec map_stmt {stmt; smeta} =
    {stmt= map_statement translate_funapps map_stmt stmt; smeta}
  in
  map_prog translate_funapps map_stmt p
