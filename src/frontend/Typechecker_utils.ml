open Core_kernel
open Core_kernel.Poly
open Middle
open Ast
module Env = Environment

let mk_fun_app ~is_cond_dist (kind, id, arguments) =
  if is_cond_dist then CondDistApp (kind, id, arguments)
  else FunApp (kind, id, arguments)

let calculate_autodifftype current_block origin ut =
  match origin with
  | Env.(Param | TParam | Model | Functions)
    when not (UnsizedType.is_int_type ut || current_block = Env.GQuant) ->
      UnsizedType.AutoDiffable
  | _ -> DataOnly

let make_function_variable current_block loc id = function
  | UnsizedType.UFun (args, rt, FnLpdf _, mem_pattern) ->
      let type_ =
        UnsizedType.UFun
          (args, rt, Fun_kind.suffix_from_name id.name, mem_pattern) in
      mk_typed_expression ~expr:(Variable id)
        ~ad_level:(calculate_autodifftype current_block Functions type_)
        ~type_ ~loc
  | UnsizedType.UFun _ as type_ ->
      mk_typed_expression ~expr:(Variable id)
        ~ad_level:(calculate_autodifftype current_block Functions type_)
        ~type_ ~loc
  | type_ ->
      Common.FatalError.fatal_error_msg
        [%message
          "Attempting to create function variable out of "
            (type_ : UnsizedType.t)]

let arg_type x = (x.emeta.ad_level, x.emeta.type_)
let get_arg_types = List.map ~f:arg_type
let type_of_expr_typed ue = ue.emeta.type_
let has_int_type ue = ue.emeta.type_ = UInt
let has_int_array_type ue = ue.emeta.type_ = UArray UInt

let has_int_or_real_type ue =
  match ue.emeta.type_ with UInt | UReal -> true | _ -> false
