type 'a t = ( :: ) of 'a * 'a list

let to_list (hd :: tl) : _ list = hd :: tl

let of_list : _ list -> _ t option = function
  | [] -> None
  | hd :: tl -> Some (hd :: tl)

let of_list_exn : _ list -> _ t = function
  | [] ->
      ICE.internal_error "Nonempty_list.of_list_exn: empty list" [@coverage off]
  | hd :: tl -> hd :: tl

(** [@@deriving sexp] doesn't like this type, so we do it manually *)

let sexp_of_t f l = Sexplib0.Sexp_conv.sexp_of_list f (to_list l)
let t_of_sexp f s = Sexplib0.Sexp_conv.list_of_sexp f s |> of_list_exn
