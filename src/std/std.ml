(** Extensions to the standard library. Most files in the project should begin
    [open Std]. *)

(** New modules *)

module Return = Return
module Nonempty_list = Nonempty_list

(** OCaml Stdlib with labeled functions *)

include StdLabels
include MoreLabels

(** A few extensions to builtin modules *)

module Option = struct
  include Option

  (* annoyingly, stdlabels doesn't affect options *)
  let map ~f o = map f o
  let bind o ~f = bind o f
  let first_some a b = Option.blend Fun.const a b

  let all os =
    let rec loop acc os =
      match os with
      | [] -> Some (List.rev acc)
      | Some o :: os -> loop (o :: acc) os
      | None :: _ -> None in
    loop [] os

  let value_map ~f ~default = function Some e -> f e | None -> default
end

module List = struct
  include List

  let hd_exn = hd
  let hd = function i :: _ -> Some i | _ -> None
  let tl_exn = tl
  let tl = function _ :: l -> Some l | _ -> None
  let range start stop = List.init ~len:(stop - start) ~f:(fun i -> start + i)

  let split_n lst n =
    if n <= 0 then ([], lst)
    else
      let rec loop acc n l =
        match l with
        | [] -> (lst, [])
        | e :: rem ->
            if n = 0 then (rev acc, l) else loop (e :: acc) (n - 1) rem in
      loop [] n lst
end

module String = struct
  include String

  let chop_suffix ~suffix s =
    if ends_with ~suffix s then Some (drop_last (length suffix) s) else None

  let chop_suffix_if_exists ~suffix s =
    chop_suffix ~suffix s |> Option.value ~default:s

  let chop_suffix_exn ~suffix s = chop_suffix ~suffix s |> Option.get

  let chop_prefix ~prefix s =
    if starts_with ~prefix s then Some (drop_first (length prefix) s) else None

  let chop_prefix_if_exists ~prefix s =
    chop_prefix ~prefix s |> Option.value ~default:s

  let chop_prefix_exn ~prefix s = chop_prefix ~prefix s |> Option.get

  let concat_map ~f s =
    fold_left ~f:(fun acc c -> f c :: acc) ~init:[] s
    |> List.rev |> String.concat

  module Map = Map.Make (String)
  module Set = Set.Make (String)
end

module Int = struct
  include Int

  let of_string = int_of_string
  let of_string_opt = int_of_string_opt
end

module Hashtbl = struct
  include Hashtbl

  let add = replace

  let add_multi ~key ~data map =
    match Hashtbl.find_opt map key with
    | Some lst -> add ~key ~data:(data :: lst) map
    | None -> add ~key ~data:[data] map

  let find_multi t k = match find_opt t k with Some l -> l | None -> []
  let update t key ~f = replace ~key ~data:(f (find_opt t key)) t

  (** Annoying/hard to use APIs removed *)

  let find_all = `Removed_unsavory_hashtable_apis
  let find_and_replace = `Removed_unsavory_hashtable_apis
  let replace = `Removed_unsavory_hashtable_apis
end

module Result = struct
  include Result

  let map ~f r = map f r
  let map_error ~f r = map_error f r
  let to_either = function Ok l -> Either.Left l | Error r -> Either.Right r

  let all rs =
    let rec loop acc rs =
      match rs with
      | [] -> Ok (List.rev acc)
      | Ok r :: rs -> loop (r :: acc) rs
      | Error e :: _ -> Error e in
    loop [] rs
end
