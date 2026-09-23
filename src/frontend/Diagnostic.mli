val range_of_loc_span :
     ?printed_filename:string
  -> ?code:string
  -> Middle.Location_span.t
  -> Grace.Range.t * Grace.Diagnostic.Label.t list
(** Returns the range represented by the location span and a list of secondary
    diagnostics identifying where it was included from, if applicable *)

val unstyle : Grace.Diagnostic.Label.t -> Grace.Diagnostic.Label.t
(** Resets the style before the label to allow our own coloring to work better.
*)

val pp : 'a Grace.Diagnostic.t Fmt.t
val pp_compact : 'a Grace.Diagnostic.t Fmt.t
