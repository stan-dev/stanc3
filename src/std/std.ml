(** Extensions to the standard library. Most files in the project should begin
    [open Std]. *)

(** New modules *)

module Return = Return
module Nonempty_list = Nonempty_list
module Nothing = Nothing
module Hash_set = Hash_set

(** OCaml Stdlib with labeled functions *)

include StdLabels
include MoreLabels

(** A few extensions to builtin modules *)

module Option = struct
  include Option

  (* annoyingly, stdlabels doesn't affect everything *)
  let map ~f o = map f o
  let bind o ~f = bind o f
  let iter ~f o = iter f o
  let first_some a b = Option.blend Fun.const a b

  let all os =
    let rec loop acc os =
      match os with
      | [] -> Some (List.rev acc)
      | Some o :: os -> loop (o :: acc) os
      | None :: _ -> None in
    loop [] os

  let value_map ~f ~default = function Some e -> f e | None -> default
  let some_if b v = if b then Some v else None
end

module List = struct
  include List

  let hd_exn = hd
  let hd = function i :: _ -> Some i | _ -> None
  let tl_exn = tl
  let tl = function _ :: l -> Some l | _ -> None
  let range start stop = List.init ~len:(stop - start) ~f:(fun i -> start + i)

  let min_elt ~cmp l =
    let rec loop m = function
      | [] -> m
      | e :: l -> if cmp e m < 0 then loop e l else loop m l in
    match l with [] -> None | e :: l -> Some (loop e l)

  let split_n lst n =
    if n <= 0 then ([], lst)
    else
      let rec loop acc n l =
        match l with
        | [] -> (lst, [])
        | e :: rem ->
            if n = 0 then (rev acc, l) else loop (e :: acc) (n - 1) rem in
      loop [] n lst

  let chunks_of l ~length =
    let rec aux length acc l =
      match l with
      | [] -> rev acc
      | _ :: _ ->
          let chunk, l = split_n l length in
          aux length (chunk :: acc) l in
    aux length [] l

  let[@tail_mod_cons] rec map3 ~f l1 l2 l3 =
    match (l1, l2, l3) with
    | [], [], [] -> []
    | x1 :: l1, x2 :: l2, x3 :: l3 -> f x1 x2 x3 :: map3 l1 l2 l3 ~f
    | _, _, _ -> raise (Invalid_argument "List.map3: unequal lengths")

  let concat_mapi ~f l = mapi ~f l |> concat

  let rec split3 = function
    | [] -> ([], [], [])
    | (x, y, z) :: l ->
        let rx, ry, rz = split3 l in
        (x :: rx, y :: ry, z :: rz)

  let partition3_map ~f l =
    let rec part fst snd trd = function
      | [] -> (rev fst, rev snd, rev trd)
      | x :: l ->
          begin match f x with
          | `Fst v -> part (v :: fst) snd trd l
          | `Snd v -> part fst (v :: snd) trd l
          | `Trd v -> part fst snd (v :: trd) l
          end in
    part [] [] [] l

  let find_a_dup l ~cmp =
    let sorted = sort l ~cmp in
    let rec find_a_dup_loop l =
      match l with
      | [] | [_] -> None
      | hd1 :: (hd2 :: _ as tl) ->
          if cmp hd1 hd2 = 0 then Some hd1 else find_a_dup_loop tl in
    find_a_dup_loop sorted

  (* Taken from Jane Street's [Base] library. MIT *)
  let find_all_dups l ~cmp =
    let sorted = sort ~cmp l in
    let[@tail_mod_cons] rec find_all_dups_loop sorted prev ~already_recorded =
      match sorted with
      | [] -> []
      | hd :: tl ->
          if cmp prev hd <> 0 then
            find_all_dups_loop tl hd ~already_recorded:false
          else if already_recorded then
            find_all_dups_loop tl hd ~already_recorded:true
          else hd :: find_all_dups_loop tl hd ~already_recorded:true in
    match sorted with
    | [] -> []
    | hd :: tl -> find_all_dups_loop tl hd ~already_recorded:false
end

module Set = struct
  include Set

  module type S = sig
    include Set.S

    val union_list : t list -> t
  end

  module Make (Ord : Set.OrderedType) : S with type elt = Ord.t = struct
    include Set.Make (Ord)

    let union_list = List.fold_left ~f:union ~init:empty
  end
end

module Map = struct
  include Map

  module type S = sig
    include Map.S

    val find_multi : key -> 'a list t -> 'a list
  end

  module Make (Ord : Map.OrderedType) : S with type key = Ord.t = struct
    include Map.Make (Ord)

    let find_multi k t = match find_opt k t with Some l -> l | None -> []
  end
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

  (* Taken from Jane Street's [Base] library. MIT *)
  let split_lines =
    let back_up_pos_and_eol_at_newline ~t ~pos =
      let pos = pos - if pos > 0 && Char.equal t.[pos - 1] '\r' then 2 else 1 in
      let eol = pos + 1 in
      (pos, eol) in
    fun t ->
      let n = length t in
      if n = 0 then []
      else
        (* Invariant: [-1 <= pos < eol]. *)
        let pos = n - 1 in
        let eol = n in
        (* We treat the end of the string specially, because if the string ends
           with a newline, we don't want an extra empty string at the end of the
           output. *)
        let pos, eol =
          if Char.equal t.[pos] '\n' then back_up_pos_and_eol_at_newline ~t ~pos
          else (pos, eol) in
        let rec loop ~pos ~eol ac =
          if pos >= 0 then
            if not (Char.equal t.[pos] '\n') then loop ~pos:(pos - 1) ~eol ac
            else
              (* Because [pos < eol], we know that [start <= eol]. *)
              let start = pos + 1 in
              let ac = sub t ~pos:start ~len:(eol - start) :: ac in
              let pos, eol = back_up_pos_and_eol_at_newline ~t ~pos in
              loop ~pos ~eol ac
          else sub t ~pos:0 ~len:eol :: ac in
        loop ~pos ~eol []

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

  let bind r ~f = bind r f
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

module Lazy = struct
  include Lazy

  let map ~f l = map f l
end

(** Pervasive free functions *)

let fst3 (x, _, _) = x

(** Useful for [@@deriving] derivers *)

module Sexp_conv = struct
  open Sexplib0.Sexp_conv

  let sexp_of_list = sexp_of_list
  let sexp_of_pair = sexp_of_pair
  let sexp_of_opaque = sexp_of_opaque
  let sexp_of_option = sexp_of_option
  let sexp_of_int = sexp_of_int
  let sexp_of_string = sexp_of_string
  let sexp_of_bool = sexp_of_bool
  let sexp_of_unit = sexp_of_unit
  let print_s = Format.kasprintf print_endline "%a" Sexplib0.Sexp.pp_hum
end

module Compare = struct
  let compare_string = String.compare
  let compare_int = Int.compare
  let compare_list cmp = List.compare ~cmp
  let compare_bool = Bool.compare
  let compare_unit () () = 0
  let compare_option = Option.compare
  let equal_string = String.equal
  let equal_int = Int.equal
  let equal_list eq = List.equal ~eq
  let equal_bool = Bool.equal
  let equal_unit () () = true
  let equal_option = Option.equal
end
