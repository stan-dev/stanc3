type 'a t = ( :: ) of 'a * 'a list

let of_list : _ list -> _ t option = function
  | [] -> None
  | hd :: tl -> Some (hd :: tl)

let of_list_exn : _ list -> _ t = function
  | [] -> raise (Invalid_argument "Nonempty_list.of_list_exn: empty list")
  | hd :: tl -> hd :: tl
