open Core_kernel

type 'a t = NonEmpty of 'a * 'a list 

let map (NonEmpty(x,xs)) ~f = NonEmpty(f x, List.map ~f xs) 
let cons x (NonEmpty(y,ys)) = NonEmpty(x,y::ys)
let append xs ys =
    match (xs, ys) with
    | NonEmpty (x, []), NonEmpty (y, []) -> NonEmpty (x, [y])
    | NonEmpty (x, xs), NonEmpty (y, ys) -> NonEmpty (x, xs @ (y :: ys))

let singleton x = NonEmpty(x,[])

let hd (NonEmpty(x,_)) = x

let tl_opt (NonEmpty(_,xs)) = 
    match xs with
    | next::rest -> Some(NonEmpty(next,rest))
    | _ -> None 

let to_list (NonEmpty(x,xs)) = x::xs

let of_list_opt = function 
    | [] -> None 
    | x::xs -> Some(NonEmpty(x,xs))
