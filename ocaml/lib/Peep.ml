open Mir

let log1m = function
  | FnApp("log", [FnApp("minus", [Lit(_, "1"); x])]) -> FnApp("log1m", [x])
  | x -> x

let run_peephole_opts prog =
  prog |> log1m
