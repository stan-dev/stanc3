(** See https://ocaml.org/manual/5.2/bindingops.html#ss%3Aletops-conventions
    This is an alternative to the [let%bind] and [let%map] syntax from ppx_let:
    https://blog.janestreet.com/let-syntax-and-why-you-should-use-it/ *)

module Result = struct
  let ( let* ) = Base.Result.( >>= )
  let ( let+ ) = Base.Result.( >>| )
end

module Option = struct
  let ( let* ) = Base.Option.( >>= )
  let ( let+ ) = Base.Option.( >>| )
end
