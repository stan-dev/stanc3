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

type target =
  | Function of {type_: UnsizedType.t; has_body: bool}
  | Variable of {type_: UnsizedType.t; is_global: bool; block: Block.t}

type annotation_classifier =
  string -> string list -> target -> (unit, unit Fmt.t option) result
