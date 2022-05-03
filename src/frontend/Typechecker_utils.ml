open Core_kernel
open Core_kernel.Poly
open Middle
open Ast
module Env = Environment

let calculate_autodifftype current_block origin ut =
  match origin with
  | Env.(Param | TParam | Model | Functions)
    when not (UnsizedType.is_int_type ut || current_block = Env.GQuant) ->
      UnsizedType.AutoDiffable
  | _ -> DataOnly

let arg_type x = (x.emeta.ad_level, x.emeta.type_)
let get_arg_types = List.map ~f:arg_type
let type_of_expr_typed ue = ue.emeta.type_
let has_int_type ue = ue.emeta.type_ = UInt
let has_int_array_type ue = ue.emeta.type_ = UArray UInt

let has_int_or_real_type ue =
  match ue.emeta.type_ with UInt | UReal -> true | _ -> false
