open Core_kernel
open Middle
open Middle.Program
open Middle.Expr


let list_sigma_unbounded (mir : Program.Typed.t) :
  string Set.Poly.t =
  let not_lower_zero (e : Expr.Typed.t transformation) = match e with
    | Lower {pattern= Fixed.Pattern.Lit (Int, i); _} ->
      int_of_string i <> 0
    | Lower {pattern= Fixed.Pattern.Lit (Real, i); _} ->
      float_of_string i <> 0.
    | Lower _ -> false
    | LowerUpper ({pattern= Fixed.Pattern.Lit (Int, i); _}, _) ->
      int_of_string i <> 0
    | LowerUpper ({pattern= Fixed.Pattern.Lit (Real, i); _}, _) ->
      float_of_string i <> 0.
    | LowerUpper (_, _) -> false
    | _ -> true
  in
  Set.Poly.of_list
    (List.map ~f:fst
       (List.filter
          ~f:(fun (name, {out_block; out_trans; _}) ->
              (out_block = Parameters || out_block = TransformedParameters)
              && String.is_prefix ~prefix:"sigma" name
              && not_lower_zero out_trans )
          mir.output_vars))

let print_warn_sigma_unbounded (mir : Program.Typed.t) =
  let pnames = list_sigma_unbounded mir in
  let message pname =
    "Warning: Your Stan program has an unconstrained parameter \"" ^ pname ^ "\" whose name begins with \"sigma\". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.\n"
  in
  Set.Poly.iter pnames ~f:(fun pname ->
      Out_channel.output_string stderr (message pname))

let print_warn_pedantic (mir : Program.Typed.t) =
  print_warn_sigma_unbounded mir
