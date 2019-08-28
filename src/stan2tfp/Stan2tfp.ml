open Core_kernel
open Frontend
open Tfp_backend

let dump_transformed_mir = ref false

let options =
  Arg.align
    [ ( "--dump-transformed-mir"
      , Arg.Set dump_transformed_mir
      , "Dump the MIR after it's been transformed by the TFP backend." ) ]

let usage = "Usage: stan2tfp [option] ... <model_file.stan>"
let model_file = ref ""
let remove_dotstan s = String.drop_suffix s 5

let set_model_file s =
  match !model_file with
  | "" ->
      model_file := s ;
      Semantic_check.model_name :=
        remove_dotstan (Filename.basename s) ^ "_model"
  | _ -> raise_s [%message "Can only pass in one model file."]

let main () =
  Arg.parse options set_model_file usage ;
  let mir =
    !model_file |> Frontend_utils.get_ast_or_exit
    |> Frontend_utils.type_ast_or_exit
    |> Ast_to_Mir.trans_prog !Semantic_check.model_name
    |> Code_gen.trans_prog
  in
  if !dump_transformed_mir then Fmt.pr "%a" Middle.Pretty.pp_typed_prog mir ;
  Fmt.pr "%a" Code_gen.pp_prog mir

let () = main ()
