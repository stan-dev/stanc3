(*
open Core_kernel
open Mir

let rec log1m = function
  | FnApp("log", [FnApp("minus", [Lit(_, "1"); x])]) -> FnApp("log1m", [x])
  | FnApp(n, args) -> FnApp(n, List.map log1m args)
  | x -> x

let run_peephole_opts prog =
  prog |> log1m

*)
