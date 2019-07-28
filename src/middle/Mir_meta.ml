open Core_kernel

module NoMeta = struct
    type t = unit [@@deriving compare, sexp, hash]
    let pp _ _ = ()
end

module Typed = struct 
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t sexp_opaque [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype }
    [@@deriving compare, create, sexp, hash]

    let empty = 
      create ~type_:UnsizedType.uint ~adlevel:UnsizedType.DataOnly ~loc:Location_span.empty ()

    let adlevel {adlevel; _} = adlevel
    let type_ {type_; _} = type_
    let loc {loc; _} = loc
    let pp _ _ = ()
end     

module Labelled = struct
    type t =
      { type_: UnsizedType.t
      ; loc: Location_span.t sexp_opaque [@compare.ignore]
      ; adlevel: UnsizedType.autodifftype
      ; label: Label.t [@compare.ignore] }
    [@@deriving compare, create, sexp, hash]

    let label {label; _} = label
    let adlevel {adlevel; _} = adlevel
    let type_ {type_; _} = type_
    let loc {loc; _} = loc
    let pp _ _ = ()
end