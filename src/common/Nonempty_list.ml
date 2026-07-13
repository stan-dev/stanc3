type 'a t = ( :: ) of 'a * 'a list

let to_list (hd :: tl) : _ list = hd :: tl

let of_list : _ list -> _ t option = function
  | [] -> None
  | hd :: tl -> Some (hd :: tl)

let of_list_exn : _ list -> _ t = function
  | [] ->
      ICE.internal_error "Nonempty_list.of_list_exn: empty list" [@coverage off]
  | hd :: tl -> hd :: tl
