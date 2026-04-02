open Core

type 'a t = ( :: ) of 'a * 'a list
[@@deriving compare, equal, hash, map, fold]

let to_list (hd :: tl) : _ list = hd :: tl

let of_list_exn : _ list -> _ t = function
  | [] ->
      ICE.internal_compiler_error
        [%message "Nonempty_list.of_list_exn: empty list"]
  | hd :: tl -> hd :: tl

(** [@@deriving sexp] doesn't like this type, so we do it manually *)
include
  Sexpable.Of_sexpable1
    (List)
    (struct
      type nonrec 'a t = 'a t

      let to_sexpable = to_list
      let of_sexpable = of_list_exn
    end)
