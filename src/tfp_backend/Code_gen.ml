open Core_kernel
open Middle
open Fmt

let dist_prefix = "tfd.distributions."

let remove_stan_dist_suffix s =
  List.filter_map
    (("_rng" :: Utils.distribution_suffices) @ [""])
    ~f:(fun suffix -> String.chop_suffix ~suffix s)
  |> List.hd_exn

let pass_through_fnames = String.Set.of_list ["normal"]

let map_functions fname args =
  match fname with
  | "multi_normal_cholesky" -> ("MultivariateNormalTriL", args)
  | _ ->
      if Set.mem pass_through_fnames fname then (fname, args)
      else raise_s [%message "Not sure how to handle " fname " yet!"]

let rec translate_funapps {expr; emeta} =
  let expr =
    match expr with
    | FunApp (StanLib, fname, args) ->
        let prefix =
          if Utils.is_distribution_name fname then dist_prefix else ""
        in
        let fname = remove_stan_dist_suffix fname in
        let fname, args = map_functions fname args in
        FunApp (StanLib, prefix ^ fname, args)
    | _ -> map_expr translate_funapps expr
  in
  {expr; emeta}

let trans_prog (p : typed_prog) = map_prog translate_funapps Fn.id p
let is_multi_index = function MultiIndex _ -> true | _ -> false

let rec pp_expr ppf {expr; _} =
  match expr with
  | Var ident -> string ppf ident
  | Lit (Str, s) -> pf ppf "%S" s
  | Lit (_, s) -> string ppf s
  | FunApp (_, fname, args) ->
      pf ppf "%s(@[<hov>%a@])" fname (list ~sep:comma pp_expr) args
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
      pf ppf "%a[%a]" pp_expr obj
        (list ~sep:comma (Middle.Pretty.pp_index pp_expr))
        indices

(* let pp_stmt {stmt; _} = match stmt with
 *   | Assignment (_, _) -> (??)
 *   | TargetPE _ -> (??)
 *   | NRFunApp (_, _, _) -> (??)
 *   | Break -> (??)
 *   | Continue -> (??)
 *   | Return _ -> (??)
 *   | Skip -> (??)
 *   | IfElse (_, _, _) -> (??)
 *   | While (_, _) -> (??)
 *   | For _ -> (??)
 *   | Block _ -> (??)
 *   | SList _ -> (??)
 *   | Decl _ -> (??) *)

let pp_call ppf (name, pp_arg, args) =
  pf ppf "%s(@[<hov>%a@])" name (list ~sep:comma pp_arg) args

let pp_call_str ppf (name, args) = pp_call ppf (name, string, args)

let pp_method ppf name params intro ?(outro = []) ppbody =
  pf ppf "@[<v 2>def %a:" pp_call_str (name, params) ;
  (list ~sep:cut string) ppf intro ;
  cut ppf () ;
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

let pp_methods ppf p = pf ppf "@ %a" pp_init p

(* pf ppf "@ %a" pp_log_prob p ; *)

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
