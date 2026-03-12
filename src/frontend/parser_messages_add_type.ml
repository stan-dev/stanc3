let () =
  let code = In_channel.input_all In_channel.stdin in
  let code =
    (* we modify the code to give it a type annotation. This forces the compiler
       to check the validitiy of our messages as Format strings at compile time,
       rather than at runtime using Scanf *)
    Str.replace_first (Str.regexp "let message")
      "let message: int -> Syntax_error.styled_text" code in
  (* Strict newlines ('\n') can break things in formatting, we replace them with
     break hints *)
  let code = Str.global_replace (Str.regexp {|\\n|}) "@." code in
  print_string code
