open Core_kernel
open Frontend
open Tfp_backend

let dump_mir = ref false
let dump_transformed_mir = ref false

let options =
  Arg.align
    [ ( "--dump-transformed-mir"
      , Arg.Set dump_transformed_mir
      , "Dump the MIR after it's been transformed by the TFP backend." )
    ; ( "--dump-mir"
      , Arg.Set dump_mir
      , "Dump the MIR immediately after transformation from the AST." ) ]

let usage = "Usage: stan2tfp [option] ... <model_file.stan>"
let model_file = ref ""
let remove_dotstan s = String.drop_suffix s 5

let set_model_file s =
  match !model_file with
  | "" ->
      model_file := s ;
      Typechecker.model_name := remove_dotstan (Filename.basename s) ^ "_model"
  | _ -> raise_s [%message "Can only pass in one model file."]

let main () =
  Arg.parse options set_model_file usage ;
  let mir =
    !model_file |> Frontend_utils.get_ast_or_exit
    |> Frontend_utils.type_ast_or_exit
    |> Ast_to_Mir.trans_prog !Typechecker.model_name
  in
  if !dump_mir then
    mir |> Middle.Program.Typed.sexp_of_t |> Sexp.to_string_hum
    |> print_endline ;
  let mir = Transform_mir.trans_prog mir in
  if !dump_transformed_mir then Fmt.pr "%a" Middle.Program.Typed.pp mir ;
  Fmt.pr "%a" Code_gen.pp_prog mir

let () = main ()

(* TODO: Refactor this into a shared pluggable compiler interface @enetsee
talked about.*)
