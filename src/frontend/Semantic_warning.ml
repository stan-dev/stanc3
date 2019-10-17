open Middle

type t =
  | WarnAutodiffLevel of location_span * string * autodifftype * autodifftype

let pp ppf = function
  | WarnAutodiffLevel (_, argname, _, _) ->
      Fmt.pf ppf
        "Warning: Argument to '%s' has an incompatible autodiff level." argname

let location = function WarnAutodiffLevel (loc_span, _, _, _) -> loc_span

let warn_autodiff_level loc argname ad0 ad1 =
  WarnAutodiffLevel (loc, argname, ad0, ad1)
