open Std
open Common

let%expect_test "with_exn_message" =
  ICE.with_exn_message ~backtraces:false (fun () -> ICE.internal_error "oops!")
  |> Result.get_error |> print_endline;
  [%expect
    {|
    Internal compiler error:
    oops!
    Backtrace missing.

    This should never happen. Please file a bug at %PKG_ISSUES%
    and include this message and the model that caused this issue.
    |}]

(* expect_tests warn against directly including a backtrace for fragility
   reasons *)
let%expect_test "backtrace indirect test" =
  ICE.with_exn_message (fun () -> assert false) |> Result.get_error |> fun s ->
  if String.includes ~affix:"Called from Common" s then
    print_endline "Backtrace found in message"
  else print_endline "FAILED TO FIND BACKTRACE";
  [%expect {| Backtrace found in message |}]

let%expect_test "ICE triggered" =
  ICE.with_exn_message ~backtraces:false (fun () ->
      Middle.(
        Expr.Helpers.infer_type_of_indexed UnsizedType.UReal
          [Index.Single Expr.Helpers.loop_bottom]))
  |> Result.get_error |> print_endline;
  [%expect
    {|
    Internal compiler error:
    Can't index real
    Backtrace missing.

    This should never happen. Please file a bug at %PKG_ISSUES%
    and include this message and the model that caused this issue.
    |}]
