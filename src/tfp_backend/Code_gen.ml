open Core_kernel
open Middle
open Fmt

let dist_prefix = "tfd.distributions."

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
      let f = {expr= FunApp (StanLib, "exp", [var]); emeta} in
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

let is_multi_index = function MultiIndex _ -> true | _ -> false

let pp_call ppf (name, pp_arg, args) =
  pf ppf "%s(@[<hov>%a@])" name (list ~sep:comma pp_arg) args

let pp_call_str ppf (name, args) = pp_call ppf (name, string, args)

let pystring_of_operator = function
  | Plus -> "+"
  | Minus -> "-"
  | x -> string_of_operator x

let rec pp_expr ppf {expr; _} =
  match expr with
  | Var ident -> string ppf ident
  | Lit (Str, s) -> pf ppf "%S" s
  | Lit (_, s) -> string ppf s
  | FunApp (StanLib, f, obs :: dist_params)
    when String.is_prefix ~prefix:dist_prefix f ->
      pf ppf "%a.log_prob(%a)" pp_call (f, pp_expr, dist_params) pp_expr obs
  | FunApp (StanLib, f, args) when operator_of_string f |> Option.is_some -> (
    match
      (operator_of_string f |> Option.value_exn |> pystring_of_operator, args)
    with
    | op, [lhs; rhs] -> pf ppf "%a %s %a" pp_expr lhs op pp_expr rhs
    | op, [unary] -> pf ppf "(%s%a)" op pp_expr unary
    | op, args ->
        raise_s
          [%message "Need to implement" op (args : expr_typed_located list)] )
  | FunApp (_, fname, args) -> pp_call ppf (fname, pp_expr, args)
  | TernaryIf (cond, iftrue, iffalse) ->
      pf ppf "%a if %a else %a" pp_expr cond pp_expr iftrue pp_expr iffalse
  | EAnd (a, b) -> pf ppf "%a and %a" pp_expr a pp_expr b
  | EOr (a, b) -> pf ppf "%a or %a" pp_expr a pp_expr b
  | Indexed (_, indices) when List.exists ~f:is_multi_index indices ->
      (*
       TF indexing options:
       * tf.slice
       * tf.gather
       * tf.gather_nd
       * tf.strided_slice
*)
      raise_s [%message "Multi-indices not supported yet"]
  | Indexed (obj, indices) ->
      let pp_indexed ppf = function
        | [] -> ()
        | indices ->
            pf ppf "[%a]"
              (list ~sep:comma (Middle.Pretty.pp_index pp_expr))
              indices
      in
      pf ppf "%a%a" pp_expr obj pp_indexed indices

let rec pp_stmt ppf {stmt; _} =
  let fake_expr expr = {expr; emeta= internal_meta} in
  match stmt with
  | Assignment (_, {expr= FunApp (CompilerInternal, f, _); _})
    when internal_fn_of_string f = Some FnReadParam ->
      ()
  | Assignment ((lhs, _, indices), rhs) ->
      let indexed = fake_expr (Indexed (fake_expr (Var lhs), indices)) in
      pf ppf "%a = %a" pp_expr indexed pp_expr rhs
  | TargetPE rhs -> pf ppf "target += %a" pp_expr rhs
  | NRFunApp (_, _, _) -> ()
  | Break -> pf ppf "break"
  | Continue -> pf ppf "continue"
  | Return rhs ->
      pf ppf "return %a" (option ~none:(const string "None") pp_expr) rhs
  | Skip -> ()
  | IfElse (_, _, _) -> ()
  | While (_, _) -> ()
  | For _ -> ()
  | Block ls | SList ls -> (list ~sep:cut pp_stmt) ppf ls
  | Decl _ -> ()

let pp_method ppf name params intro ?(outro = []) ppbody =
  pf ppf "@[<v 2>def %a:@," pp_call_str (name, params) ;
  (list ~sep:cut string) ppf (intro @ [""]) ;
  ppbody ppf ;
  if not (List.is_empty outro) then pf ppf "@ %a" (list ~sep:cut string) outro ;
  pf ppf "@, @]"

let pp_init ppf p =
  let pp_save_data ppf (idx, name) = pf ppf "self.%s = data[%d]" name idx in
  let ppbody ppf =
    (list ~sep:cut pp_save_data)
      ppf
      (List.mapi p.input_vars ~f:(fun idx (name, _) -> (idx, name)))
  in
  pp_method ppf "__init__" ["self"; "data"] [] ppbody

let pp_log_prob ppf p =
  let pp_extract_data ppf name = pf ppf "%s = self.%s" name name in
  let pp_extract_param ppf (idx, name) = pf ppf "%s = params[%d]" name idx in
  let grab_params idx = function
    | name, {out_block= Parameters; _} -> [(idx, name)]
    | _ -> []
  in
  let ppbody ppf =
    pf ppf "%a@,%a@,%a"
      (list ~sep:cut pp_extract_data)
      (List.map p.input_vars ~f:(fun (name, _) -> name))
      (list ~sep:cut pp_extract_param)
      List.(concat (mapi p.output_vars ~f:grab_params))
      (list ~sep:cut pp_stmt) p.log_prob
  in
  let intro = ["target = tf.Variable(0, dtype=np.float64)"] in
  let outro = ["return target"] in
  pp_method ppf "log_prob" ["self"; "params"] intro ~outro ppbody

let pp_methods ppf p =
  pf ppf "@ %a" pp_init p ;
  pf ppf "@ %a" pp_log_prob p

let imports =
  {|
import numpy as np
import tensorflow as tf
import tensorflow_probability as tfp
tfd = tfp.distributions
|}

let pp_prog ppf (p : typed_prog) =
  pf ppf "%s@,@,class %s(tfd.Distribution):@,@[<v 2>%a@]" imports p.prog_name
    pp_methods p
