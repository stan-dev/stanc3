(** Extensions to the standard library. Most files in the project should begin
    [open Std]. *)

(** New modules *)

module Nonempty_list = Nonempty_list

(** OCaml Stdlib with labeled functions *)

include StdLabels
include MoreLabels

(** A few extensions to builtin modules *)

module Option = struct
  include Option

  let first_some a b = Option.blend Fun.const a b
  let value_map ~f ~default = function Some e -> f e | None -> default
end

module List = struct
  include List

  let hd_exn = hd
  let hd = function i :: _ -> Some i | _ -> None
  let tl_exn = tl
  let tl = function _ :: l -> Some l | _ -> None
  let range start stop = List.init ~len:(stop - start) ~f:(fun i -> start + i)
end

module String = struct
  include String

  let chop_suffix ~suffix s =
    if ends_with ~suffix s then Some (drop_last (length suffix) s) else None

  let chop_suffix_if_exists ~suffix s =
    chop_suffix ~suffix s |> Option.value ~default:s

  let chop_prefix ~prefix s =
    if starts_with ~prefix s then Some (drop_first (length prefix) s) else None

  let chop_prefix_if_exists ~prefix s =
    chop_prefix ~prefix s |> Option.value ~default:s

  let chop_prefix_exn ~prefix s = chop_prefix ~prefix s |> Option.get

  module Map = Map.Make (String)
  module Set = Set.Make (String)
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
