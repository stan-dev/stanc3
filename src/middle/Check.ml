open Core_kernel

type ('a, 'warn, 'err) t =
  | Ok of 'a
  | Warn of 'a * 'warn
  | Error of 'err * 'warn option

let map x ~f =
  match x with
  | Ok x -> Ok (f x)
  | Warn (x, w) -> Warn (f x, w)
  | Error (e, w) -> Error (e, w)

let map_error x ~f =
  match x with
  | Ok x -> Ok x
  | Warn (x, w) -> Warn (x, w)
  | Error (e, w) -> Error (f e, w)

let map_warning x ~f =
  match x with
  | Ok x -> Ok x
  | Warn (x, w) -> Warn (x, f w)
  | Error (e, w) -> Error (e, Option.map ~f w)
