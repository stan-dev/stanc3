let internal_error = failwith
let internal_errorf s a = failwith (Format.lasprintf s a)
let ( $ ) pp x ppf = pp ppf x

let with_exn_message f =
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
