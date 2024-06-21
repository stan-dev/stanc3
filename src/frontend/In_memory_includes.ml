(** in-memory way of opening new lexbufs for #include directive *)

open Core

(** helper to map "./foo.stan" to "foo.stan" *)
let no_leading_dotslash = String.chop_prefix_if_exists ~prefix:"./"

(** map from filenames to Stan code *)
let map : string String.Map.t ref = ref String.Map.empty

let find_include fname =
  let fname = no_leading_dotslash fname in
  match Map.find !map fname with
  | None ->
      let message =
        let pp_list ppf l =
          let keys = Map.keys l in
          if List.is_empty keys then Fmt.string ppf "None"
          else Fmt.(list ~sep:comma string) ppf keys in
        Fmt.str
          "Could not find include file '%s'.@ stanc was given information \
           about the following files:@ %a"
          fname pp_list !map in
      raise
        (Errors.SyntaxError
           (Include (message, Preprocessor.current_location_t ())))
  | Some s -> (Lexing.from_string s, fname)
