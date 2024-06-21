(** filesystem-based way of opening new lexbufs for #include directive *)

open Core

(** list of allowed directories to look in for the path *)
let lookup_paths : string list ref = ref []

let find_include fname =
  let open Lexing in
  let rec loop paths =
    match paths with
    | [] ->
        let message =
          let pp_list ppf l =
            match l with
            | [] -> Fmt.string ppf "None"
            | _ -> Fmt.(list ~sep:comma string) ppf l in
          Fmt.str
            "Could not find include file '%s' in specified include paths.@\n\
             @[Current include paths: %a@]" fname pp_list !lookup_paths in
        raise
          (Errors.SyntaxError
             (Include (message, Preprocessor.current_location_t ())))
    | path :: rest_of_paths -> (
        try
          let full_path = path ^ "/" ^ fname in
          (In_channel.create full_path |> from_channel, full_path)
        with _ -> loop rest_of_paths) in
  loop !lookup_paths
