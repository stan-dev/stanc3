open Std

let internal_error s = failwith s
let internal_errorf s a = failwith (Format.lasprintf s a)
let ( $ ) pp x ppf = pp ppf x

(** Unless specifically disabled by the user, default to using backtraces *)
let backtrace_default () =
  match Sys.getenv_opt "OCAMLRUNPARAM" with
  | None -> true
  | Some v -> not (List.mem "b=0" ~set:(String.split_on_char ~sep:',' v))

let with_exn_message ?(backtraces = backtrace_default ()) f =
  Printexc.record_backtrace backtraces;
  try Ok (f ())
  with e ->
    let bt =
      if Printexc.backtrace_status () then Printexc.get_backtrace ()
      else "Backtrace missing." in
    let msg = match e with Failure msg -> msg | _ -> Printexc.to_string e in
    Error
      (Fmt.str
         "Internal compiler error:@ @[%s@]@\n\
          %s@\n\
          @\n\
          This should never happen. Please file a bug at %%PKG_ISSUES%%@ and \
          include this message and the model that caused this issue.@\n"
         msg bt)
