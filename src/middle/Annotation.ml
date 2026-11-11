open Std.Compare
open Std.Sexp_conv

type t =
  { name: string
  ; args: string list
  ; loc: Location_span.t [@compare.ignore] [@sexp.opaque] }
[@@deriving sexp_of, compare]

let pp ppf {name; args; loc= _} =
  match args with
  | [] -> Fmt.pf ppf "%@%s" name
  | _ -> Fmt.pf ppf "%@%s(@[%a)@]" name Fmt.(list ~sep:comma string) args
