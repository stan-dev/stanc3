open Core_kernel
open Frontend

let stan_model_filename =
  let open Core_kernel.Command.Param in
  anon ("stan_model_filename" %: string)
