type t = EmitGeneratedQuantities | EmitTransformedParameters

let enumerate = [EmitGeneratedQuantities; EmitTransformedParameters]

let to_string = function
  | EmitGeneratedQuantities -> "emit_generated_quantities__"
  | EmitTransformedParameters -> "emit_transformed_parameters__"
