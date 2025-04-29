open Core
open Frontend

let print_ast_or_error code =
  let () =
    match Test_utils.untyped_ast_of_string code with
    | Result.Error e -> print_endline @@ Test_utils.error_to_string ~code e
    | Result.Ok ast -> print_s [%sexp (ast : Ast.untyped_program)] in
  (* reset *) Include_files.include_provider := FileSystemPaths []

let include_model = {|
#include <foo.stan>
data {
    int a;
}
|}

(* TESTS *)
let%expect_test "no includes" =
  Include_files.include_provider := InMemory String.Map.empty;
  print_ast_or_error include_model;
  [%expect
    {|
    Syntax error in 'string', line 2, column 0, include error:
       -------------------------------------------------
         1:
         2:  #include <foo.stan>
             ^
         3:  data {
         4:      int a;
       -------------------------------------------------

    Could not find include file 'foo.stan'.
    stanc was given information about the following files:
    None |}]

let%expect_test "wrong include" =
  Include_files.include_provider :=
    InMemory (String.Map.of_alist_exn [("bar.stan", "functions { }")]);
  print_ast_or_error include_model;
  [%expect
    {|
    Syntax error in 'string', line 2, column 0, include error:
       -------------------------------------------------
         1:
         2:  #include <foo.stan>
             ^
         3:  data {
         4:      int a;
       -------------------------------------------------

    Could not find include file 'foo.stan'.
    stanc was given information about the following files:
    bar.stan |}]

let a = {|
// comment here
#include <include/b.stan>
|}

let b = {|
#include <include/a.stan>
// comment here
|}

let%expect_test "recursive include" =
  Include_files.include_provider :=
    InMemory
      (String.Map.of_alist_exn [("include/a.stan", a); ("include/b.stan", b)]);
  print_ast_or_error a;
  [%expect
    {|
    Syntax error in 'include/b.stan', line 2, column 0, included from
    'include/a.stan', line 3, column 0, included from
    'include/b.stan', line 2, column 0, included from
    'string', line 3, column 0, include error:
       -------------------------------------------------
         1:
         2:  #include <include/a.stan>
             ^
         3:  // comment here
       -------------------------------------------------

    File include/a.stan recursively included itself. |}]

let foo = {|
functions {
  int foo(real a) {
      return 1;
  }
}
|}

let%expect_test "good include" =
  Include_files.include_provider :=
    InMemory (String.Map.of_alist_exn [("foo.stan", foo)]);
  print_ast_or_error include_model;
  [%expect
    {|
    ((functionblock
      (((stmts
         (((stmt
            (FunDef (returntype (ReturnType UInt))
             (funname ((name foo) (id_loc <opaque>)))
             (arguments ((AutoDiffable UReal ((name a) (id_loc <opaque>)))))
             (annotations ())
             (body
              ((stmt
                (Block
                 (((stmt
                    (Return ((expr (IntNumeral 1)) (emeta ((loc <opaque>))))))
                   (smeta ((loc <opaque>)))))))
               (smeta ((loc <opaque>)))))))
           (smeta ((loc <opaque>))))))
        (xloc
         ((begin_loc
           ((filename foo.stan) (line_num 2) (col_num 0)
            (included_from
             (((filename string) (line_num 2) (col_num 0) (included_from ()))))))
          (end_loc
           ((filename foo.stan) (line_num 6) (col_num 1)
            (included_from
             (((filename string) (line_num 2) (col_num 0) (included_from ())))))))))))
     (datablock
      (((stmts
         (((stmt
            (VarDecl (decl_type SInt) (transformation Identity) (is_global true)
             (annotations ())
             (variables
              (((identifier ((name a) (id_loc <opaque>))) (initial_value ()))))))
           (smeta ((loc <opaque>))))))
        (xloc
         ((begin_loc
           ((filename string) (line_num 3) (col_num 19) (included_from ())))
          (end_loc
           ((filename string) (line_num 5) (col_num 1) (included_from ()))))))))
     (transformeddatablock ()) (parametersblock ())
     (transformedparametersblock ()) (modelblock ())
     (generatedquantitiesblock ()) (comments <opaque>)) |}]
