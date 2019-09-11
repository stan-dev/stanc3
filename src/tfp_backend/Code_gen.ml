open Core_kernel
open Middle
open Fmt

let is_multi_index = function MultiIndex _ -> true | _ -> false

let pp_call ppf (name, pp_arg, args) =
  pf ppf "%s(@[<hov>%a@])" name (list ~sep:comma pp_arg) args

let pp_call_str ppf (name, args) = pp_call ppf (name, string, args)
let pystring_of_operator = function x -> strf "%a" Pretty.pp_operator x

let rec pp_expr ppf {expr; _} =
  match expr with
  | Var ident -> string ppf ident
  | Lit (Str, s) -> pf ppf "%S" s
  | Lit (_, s) -> pf ppf "tf.cast(%s, tf.float64)" s
  | FunApp (StanLib, f, obs :: dist_params)
    when String.is_prefix ~prefix:Transform_mir.dist_prefix f ->
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

let rec pp_stmt ppf s =
  let fake_expr expr = {expr; emeta= internal_meta} in
  match s.stmt with
  | Assignment ((lhs, _, indices), rhs) ->
      let indexed = fake_expr (Indexed (fake_expr (Var lhs), indices)) in
      pf ppf "%a = %a" pp_expr indexed pp_expr rhs
  | TargetPE rhs -> pf ppf "target += %a" pp_expr rhs
  | NRFunApp (StanLib, f, args) | NRFunApp (UserDefined, f, args) ->
      pp_call ppf (f, pp_expr, args)
  | Break -> pf ppf "break"
  | Continue -> pf ppf "continue"
  | Return rhs ->
      pf ppf "return %a" (option ~none:(const string "None") pp_expr) rhs
  | Block ls | SList ls -> (list ~sep:cut pp_stmt) ppf ls
  | Skip -> ()
  (* | Decl {decl_adtype= AutoDiffable; decl_id; _} ->
   *     pf ppf "%s = tf.Variable(0, name=%S, dtype=np.float64)" decl_id decl_id *)
  | Decl _ -> ()
  (* if else, for loop, while loop all need to create functions for
     their arguments. I think these functions need to be named and
     defined inline in general because lambdas are limited.
  *)
  | IfElse (_, _, _) | While (_, _) | For _ | NRFunApp (CompilerInternal, _, _)
    ->
      raise_s [%message "Not implemented" (s : stmt_loc)]

let pp_method ppf name params intro ?(outro = []) ppbody =
  pf ppf "@[<v 2>def %a:@," pp_call_str (name, params) ;
  (list ~sep:cut string) ppf (intro @ [""]) ;
  ppbody ppf ;
  if not (List.is_empty outro) then pf ppf "@ %a" (list ~sep:cut string) outro ;
  pf ppf "@, @]"

let pp_init ppf p =
  let data_names = List.map p.input_vars ~f:(fun (name, _) -> name) in
  let pp_save_data ppf name = pf ppf "self.%s = %s" name name in
  let ppbody ppf = (list ~sep:cut pp_save_data) ppf data_names in
  pp_method ppf "__init__" ("self" :: data_names) [] ppbody

let pp_extract_data ppf p =
  let rec pp_cast ppf (name, st) =
    match st with
    | SArray (t, _) -> pp_cast ppf (name, t)
    | SInt -> pf ppf "self.%s" name
    | _ -> pf ppf "tf.cast(%a, tf.float64)" pp_cast (name, SInt)
  in
  let pp_data ppf (name, st) = pf ppf "%s = %a" name pp_cast (name, st) in
  (list ~sep:cut pp_data) ppf p.input_vars

let pp_log_prob ppf p =
  let pp_extract_param ppf (idx, name) =
    pf ppf "%s = tf.cast(params[%d], tf.float64)" name idx
  in
  let grab_params idx = function
    | name, {out_block= Parameters; _} -> [(idx, name)]
    | _ -> []
  in
  let ppbody ppf =
    pf ppf "%a@,%a@,%a" pp_extract_data p
      (list ~sep:cut pp_extract_param)
      List.(concat (mapi p.output_vars ~f:grab_params))
      (list ~sep:cut pp_stmt) p.log_prob
  in
  let intro = ["target = 0"] in
  let outro = ["return target"] in
  pp_method ppf "log_prob" ["self"; "params"] intro ~outro ppbody

let rec lvalue_of_expr {expr; emeta} =
  match expr with
  | Var s -> (s, emeta.mtype, [])
  | Indexed (l, i) ->
      let vid, _, idcs = lvalue_of_expr l in
      (vid, emeta.mtype, idcs @ i)
  | _ -> failwith "Trying to convert illegal expression to lval."

let rec contains_var_expr is_vident accum {expr; _} =
  accum
  ||
  match expr with
  | Var v when is_vident v -> true
  | _ -> fold_expr (contains_var_expr is_vident) false expr

let rec contains_var_stmt is_vident accum {stmt; _} =
  fold_statement
    (contains_var_expr is_vident)
    (contains_var_stmt is_vident)
    accum stmt

let get_param_st p var =
  let vident, _, _ = lvalue_of_expr var in
  let {out_constrained_st= st; _} =
    List.Assoc.find_exn ~equal:( = ) p.output_vars vident
  in
  st

let rec get_dims = function
  | SInt | SReal -> []
  | SVector d | SRowVector d -> [d]
  | SMatrix (dim1, dim2) -> [dim1; dim2]
  | SArray (t, dim) -> dim :: get_dims t

let pp_sample ppf p =
  let params =
    List.filter_map p.output_vars ~f:(function
      | name, {out_block= Parameters; _} -> Some name
      | _ -> None )
  in
  let is_param = Set.mem (String.Set.of_list params) in
  let pp_shape ppf var =
    let st = get_param_st p var in
    let dims = get_dims st in
    pf ppf "(%a)" (list ~sep:comma pp_expr)
      ({expr= Var "nchains__"; emeta= internal_meta} :: dims)
  in
  let rec no_param_deps ppf s =
    match s.stmt with
    | TargetPE {expr= FunApp (StanLib, f, obs :: dist_args); _}
      when String.is_prefix ~prefix:Transform_mir.dist_prefix f
           && is_param (match lvalue_of_expr obs with v, _, _ -> v) ->
        let vident, _, _ = lvalue_of_expr obs in
        pf ppf "%s = %a.sample(@[<hov 4>%a@])" vident pp_call
          (f, pp_expr, dist_args) pp_shape obs
    | Block ls | SList ls -> (list ~sep:cut no_param_deps) ppf ls
    | _ -> ()
  in
  let ppbody ppf =
    pf ppf "%a@,%a@," pp_extract_data p
      (list ~sep:cut no_param_deps)
      p.log_prob
  in
  let intro = [] in
  let outro = [strf "return [@[<hov>%a@]]" (list ~sep:comma string) params] in
  pp_method ppf "sample" ["self"; "nchains__"] intro ~outro ppbody

let pp_methods ppf p =
  pf ppf "@ %a" pp_init p ;
  pf ppf "@ %a" pp_log_prob p ;
  pf ppf "@ %a" pp_sample p

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

(* Major work to do:
1. Work awareness of distributions and bijectors into the type system
2. Have backends present an environment that the frontend and middle can use for type checking and optimization.
*)
