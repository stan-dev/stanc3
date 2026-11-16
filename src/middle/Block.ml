(** Stan program blocks (and the Math library), used to keep track of where
    variables are declared *)

type t =
  | MathLibrary
  | Functions
  | Data
  | TData
  | Param
  | TParam
  | Model
  | GQuant

let block_name = function
  | MathLibrary -> "Stan Math Library"
  | Functions -> "functions block"
  | Data -> "data block"
  | TData -> "transformed data block"
  | Param -> "parameters block"
  | TParam -> "transformed parameters block"
  | Model -> "model block"
  | GQuant -> "generated quantities block"
