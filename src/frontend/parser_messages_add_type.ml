let () =
  let code = In_channel.input_all In_channel.stdin in
  let code =
    (* we modify the code to give it a type annotation. This
       forces the compiler to check the validitiy of our messages
       as Format strings at compile time,
       rather than at runtime using Scanf *)
    Str.replace_first (Str.regexp "let message")
      "let message: int -> (unit, Format.formatter, unit) format" code in
  print_string code
