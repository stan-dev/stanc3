open Middle

val prepare_prog : Program.Typed.t -> Program.Labelled.t * Location_span.t Label.Map.t
val pp_globals : Format.formatter -> Location_span.t list -> unit
val pp_smeta : Format.formatter -> Label.t -> unit
val no_span_num : Label.t
