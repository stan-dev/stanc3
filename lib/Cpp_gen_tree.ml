open Core_kernel

type statement = vardecl Base_prog_tree.statement

and cpptype =
  | SInt
  | SReal
  | SArray of cpptype
  | SVector
  | SRowVector
  | SMatrix
  | SSize_t
  | SString
  | SOther of string

and autodiff =
  | AVar
  | AData

and mutability =
  | Mutable
  | Immutable
  | ByValue

and argdecl = autodiff * mutability * cpptype * string

and vardecl = cpptype * string

and fndef = {
  returntype: (autodiff * cpptype) option;
  name: string;
  arguments: argdecl list;
  body: statement;
  templates: string list;
}

and cppclass = {classname: string; super: string;
                fields: vardecl list;
                methods: fndef list;
                ctor: (vardecl list) * statement}

and prog = {
  cppclass: cppclass;
  functions: fndef list;
  includes: string list;
  namespace: string list;
  usings: string list;
}
[@@deriving sexp, hash]
