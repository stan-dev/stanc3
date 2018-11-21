(** Some functions for checking whether conversions between types are allowed *)
open Ast

let check_of_same_type_mod_conv name t1 t2 =
  if Core_kernel.String.is_prefix name ~prefix:"assign_" then t1 = t2
  else t1 = t2 || (t1 = Real && t2 = Int)

let check_of_same_type_mod_array_conv name t1 t2 =
  if Core_kernel.String.is_prefix name ~prefix:"assign_" then t1 = t2
  else
    match (t1, t2) with
    | Array t1elt, Array t2elt -> check_of_same_type_mod_conv name t1elt t2elt
    | _ -> t1 = t2 || (t1 = Real && t2 = Int)

let check_compatible_arguments_mod_conv name args1 args2 =
  List.length args1 = List.length args2
  && List.for_all
       (fun y -> y = true)
       (List.map2
          (fun w1 w2 ->
            check_of_same_type_mod_conv name (snd w1) (snd w2)
            && compare_originblock (fst w1) (fst w2) > -1 )
          args1 args2)

let check_of_compatible_return_type rt1 srt2 =
  match (rt1, srt2) with
  | Void, NoReturnType
   |Void, Incomplete Void
   |Void, Complete Void
   |Void, AnyReturnType ->
      true
  | ReturnType Real, Complete (ReturnType Int) -> true
  | ReturnType rt1, Complete (ReturnType rt2) -> rt1 = rt2
  | ReturnType _, AnyReturnType -> true
  | _ -> false
