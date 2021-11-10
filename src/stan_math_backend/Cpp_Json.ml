open Core_kernel
open Middle
module Str = Re.Str

let rec sizedtype_to_json (st : Expr.Typed.t SizedType.t) : Yojson.Basic.t =
  let emit_cpp_expr e =
    Fmt.strf "+ std::to_string(%a) +" Expression_gen.pp_expr e
  in
  match st with
  | SInt -> `Assoc [("name", `String "int")]
  | SReal -> `Assoc [("name", `String "real")]
  | SComplex -> `Assoc [("name", `String "complex")]
  | SVector (_, d) | SRowVector (_, d) ->
      `Assoc [("name", `String "vector"); ("length", `String (emit_cpp_expr d))]
  | SMatrix (_, d1, d2) ->
      `Assoc
        [ ("name", `String "matrix")
        ; ("rows", `String (emit_cpp_expr d1))
        ; ("cols", `String (emit_cpp_expr d2)) ]
  | SArray (st, d) ->
      `Assoc
        [ ("name", `String "array")
        ; ("length", `String (emit_cpp_expr d))
        ; ("element_type", sizedtype_to_json st) ]

let out_var_json (name, st, block) : Yojson.Basic.t =
  `Assoc
    [ ("name", `String (Mangle.remove_prefix name))
    ; ("type", sizedtype_to_json st)
    ; ("block", `String (Fmt.strf "%a" Program.pp_io_block block)) ]

let%expect_test "outvar to json pretty" =
  let var x = {Expr.Fixed.pattern= Var x; meta= Expr.Typed.Meta.empty} in
  (* the following is equivalent to:
     parameters {
       vector[N] var_one[K];
     }
  *)
  ( "var_one"
  , SArray (SVector (Common.Helpers.AoS, var "N"), var "K")
  , Parameters )
  |> out_var_json |> Yojson.Basic.pretty_to_string |> print_endline ;
  [%expect
    {|
  {
    "name": "var_one",
    "type": {
      "name": "array",
      "length": "+ std::to_string(K) +",
      "element_type": { "name": "vector", "length": "+ std::to_string(N) +" }
    },
    "block": "parameters"
  } |}]

(*Adds a backslash to all the inner quotes and then
  unslash the ones near a plus*)
let replace_cpp_expr s =
  s
  |> Str.global_replace (Str.regexp {|"|}) {|\"|}
  |> Str.global_replace (Str.regexp {|\\"\+|}) {|" +|}
  |> Str.global_replace (Str.regexp {|\+\\"|}) {|+ "|}
  |> Str.global_replace (Str.regexp {|\\n|}) {||}

let wrap_in_quotes s = "\"" ^ s ^ "\""

let out_var_interpolated_json_str vars =
  `List (List.map ~f:out_var_json vars)
  |> Yojson.Basic.to_string |> replace_cpp_expr |> wrap_in_quotes

let%expect_test "outvar to json" =
  let var x = {Expr.Fixed.pattern= Var x; meta= Expr.Typed.Meta.empty} in
  [ ( "var_one"
    , SizedType.SArray (SVector (AoS, var "N"), var "K")
    , Program.Parameters ) ]
  |> out_var_interpolated_json_str |> print_endline ;
  [%expect
    {|
    "[{\"name\":\"var_one\",\"type\":{\"name\":\"array\",\"length\":" + std::to_string(K) + ",\"element_type\":{\"name\":\"vector\",\"length\":" + std::to_string(N) + "}},\"block\":\"parameters\"}]" |}]
