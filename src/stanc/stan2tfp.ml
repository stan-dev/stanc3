open Core_kernel

let options = Arg.align []
let usage = "Usage: stan2tfp [option] ... <model_file.stan>"
let model_file = ref ""

let set_model_file s =
  match !model_file with
  | "" -> model_file := s
  | _ -> raise_s [%message "Can only pass in one model file."]

let main () = Arg.parse options set_model_file usage
