open Core

let stanfunctions_suffix = ".stanfunctions"

let remove_dotstan s =
  Option.first_some
    (String.chop_suffix ~suffix:stanfunctions_suffix s)
    (String.chop_suffix ~suffix:".stan" s)
  |> Option.value ~default:s

let is_stanfunctions = String.is_suffix ~suffix:stanfunctions_suffix
