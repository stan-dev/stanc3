open Middle

type t =
  | WarnAutodiffLevel of location_span * string * autodifftype * autodifftype

let pp ppf = function
  | WarnAutodiffLevel (_, argname, ad0, ad1) ->
      Fmt.pf ppf
        "Warning: Argument %s has autodiff level %a which cannot be converted \
         to %a."
        argname Pretty_printing.pp_autodifftype ad0
        Pretty_printing.pp_autodifftype ad1

let location = function WarnAutodiffLevel (loc_span, _, _, _) -> loc_span

let warn_autodiff_level loc argname ad0 ad1 =
  WarnAutodiffLevel (loc, argname, ad0, ad1)
