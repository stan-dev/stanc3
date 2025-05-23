(** Mangle variables which are C++ reserved words into
    valid C++ identifiers.

    This is done in Transform_Mir. When one of these
    names is emitted as a string, we use remove_prefix
    such that this mangling is opaque to the user -
    e.g., a cmdstan output file would still have a column
    called "public", even if internally we called this
    "_stan_public"

    NB: the use of a leading _ is essential, because
    the lexer won't allow this in a user-created variable.
*)

open Core

let kwrds_prefix = "_stan_"
let remove_prefix s = String.chop_prefix_if_exists ~prefix:kwrds_prefix s
let prepend_kwrd x = kwrds_prefix ^ x

let cpp_kwrds =
  (* C++ keywords that are not stan keywords *)
  String.Set.of_list
    ([ "alignas"; "alignof"; "and"; "and_eq"; "asm"; "bitand"; "bitor"; "bool"
     ; "case"; "catch"; "char"; "char16_t"; "char32_t"; "class"; "compl"
     ; "const"; "constexpr"; "const_cast"; "decltype"; "default"; "delete"; "do"
     ; "double"; "dynamic_cast"; "enum"; "explicit"; "float"; "friend"; "goto"
     ; "inline"; "long"; "mutable"; "namespace"; "new"; "noexcept"; "not"
     ; "not_eq"; "nullptr"; "operator"; "or"; "or_eq"; "private"; "protected"
     ; "public"; "register"; "reinterpret_cast"; "short"; "signed"; "sizeof"
     ; "static_assert"; "static_cast"; "switch"; "template"; "this"
     ; "thread_local"; "throw"; "try"; "typeid"; "typename"; "union"; "unsigned"
     ; "using"; "virtual"; "volatile"; "wchar_t"; "xor"; "xor_eq" ]
    (* stan implementation keywords *)
    @ [ "fvar"; "STAN_MAJOR"; "STAN_MINOR"; "STAN_PATCH"; "STAN_MATH_MAJOR"
      ; "STAN_MATH_MINOR"; "STAN_MATH_PATCH" ]
    @ (* system macros *)
    [ "BSD"; "BSD4_2"; "BSD4_3"; "BSD4_4"; "EMSCRIPTEN"; "hpux"; "sun"; "linux"
    ; "VMS"; "i386"; "mips" ])

let add_prefix_to_kwrds s = if Set.mem cpp_kwrds s then prepend_kwrd s else s
