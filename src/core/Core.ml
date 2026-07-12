(** This module is intended to be a drop-in replacement for our previous use of
    [core] from Jane Street. This lightweight module only depends on Base,
    Sexplib0, and Stdio. *)

include Base
include Stdio

(** We make a few modules look more like their Core equivalents *)

module Set = struct
  include Set

  let of_map_keys m = Set.Poly.of_list (Map.keys m)

  module Poly = struct
    include Set.Poly

    let sexp_of_t sexp_of_elt s =
      List.sexp_of_t sexp_of_elt (Base.Set.to_list s)
  end
end

module Map = struct
  include Map

  module Poly = struct
    include Map.Poly

    let sexp_of_t sexp_of_key sexp_of_data m =
      List.sexp_of_t
        (fun (k, v) -> Sexp.List [sexp_of_key k; sexp_of_data v])
        (Base.Map.to_alist m)
  end
end

module String = struct
  include Base.String

  module Table = struct
    include Base.Hashtbl.M (Base.String)

    let create () = Base.Hashtbl.create (module Base.String)
    let of_alist_exn l = Base.Hashtbl.of_alist_exn (module Base.String) l
  end

  module Hash_set = struct
    include Base.Hash_set.M (Base.String)

    let create () = Base.Hash_set.create (module Base.String)
  end

  module Set = struct
    include Base.Set.M (Base.String)

    let of_list l = Base.Set.of_list (module Base.String) l
    let empty = Base.Set.empty (module Base.String)
    let sexp_of_t t = Base.Set.sexp_of_m__t (module Base.String) t
    let union_list l = Base.Set.union_list (module Base.String) l
  end

  module Map = struct
    include Base.Map.M (Base.String)

    let empty = Base.Map.empty (module Base.String)
    let of_alist_exn l = Base.Map.of_alist_exn (module Base.String) l
    let of_alist_reduce l = Base.Map.of_alist_reduce (module Base.String) l
  end
end

module Sexp = struct
  include Base.Sexp

  let of_string = Sexplib0.Sexp_conv.sexp_of_string
end

(** And declare some free functions that Core does *)

let fst3 (v, _, _) = v
let sprintf = Printf.sprintf

(** Finally, we re-export a bunch of stuff from Stdlib that Base shadowed *)

module Format = Stdlib.Format
module Printexc = Stdlib.Printexc
module Fun = Stdlib.Fun
module Marshal = Stdlib.Marshal
module Printf = Stdlib.Printf
module Scanf = Stdlib.Scanf
module Obj = Stdlib.Obj

let ( ^^ ) = Stdlib.( ^^ )
let ( ** ) = Stdlib.( ** )
let exit = Stdlib.exit

type ('a, 'b) result = ('a, 'b) Stdlib.result
