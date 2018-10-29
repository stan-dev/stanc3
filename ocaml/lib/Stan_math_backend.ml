open Mir
open Core_kernel

let stan_math_map =
  String.Map.of_alist_exn
    ["+", "add"; "-", "minus"; "/", "divide"; "*", "multiply"]

let rec translate_fn_names = function
  | FnApp(f, args) -> FnApp(
      String.Map.find stan_math_map f |> Option.value ~default:f,
      List.map ~f:translate_fn_names args)
  | x -> x

exception ShouldNeverHappen

let rec emit_cpp = function
  | FnApp(f, args) ->
    let args = List.map ~f:emit_cpp args in
    begin match f with
      | "if" | "for" | "while" ->
        begin match args with
          | fst :: snd :: thrd ->
            (sprintf "%s (%s) {\n  %s;\n}%s"
               f fst snd (List.fold ~init:""
                            ~f:(fun _ e -> sprintf " else {\n  %s;\n}\n" e)
                            thrd))
          | _ -> raise ShouldNeverHappen
        end
      | f -> String.concat [f; "("; (String.concat ~sep:", " args); ")"]
    end
  | Var(v) -> v
  | Lit(Str, s) -> String.concat ["\""; s; "\""]
  | Lit(_, v) -> v
  | ExprList(l) -> String.concat ~sep:"; " (List.map ~f:emit_cpp l)
  | AssignExpr(rhs, lhs) -> String.concat [rhs; " = "; emit_cpp lhs]

let emit e =
  e
  |> translate_fn_names
  |> emit_cpp

let%expect_test "emit" =
  let e = FnApp("+", [Lit(Int, "1"); Lit(Real, "3.2")]) in
  emit e |> print_endline;
  [%expect {| add(1, 3.2) |}]

let%expect_test "emitif" =
  let e = FnApp("if", [Lit(Int, "1"); Lit(Real, "3.2")]) in
  emit e |> print_endline;
  [%expect {|
    if (1) {
      3.2;
    } |}]

let%expect_test "emitifelse" =
  let e = FnApp("if", [Lit(Int, "1"); Lit(Real, "3.2"); Lit(Real, "44.3")]) in
  emit e |> print_endline;
  [%expect {|
    if (1) {
      3.2;
    } else {
      44.3;
    } |}]
