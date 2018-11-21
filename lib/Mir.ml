open Core_kernel

type statement = vardecl Base_prog_tree.statement

and stantype =
  | SInt
  | SReal
  | SArray of stantype
  | SVector
  | SRowVector
  | SMatrix

and vardecl = string * stantype

and udf_defn = {
  returntype: stantype option;
  name: string;
  arguments: vardecl list;
  body: statement;
}

and prog = {
  functions: udf_defn list;
  params: vardecl list;
  data: vardecl list;
  model: statement;
  gq: statement;
  tdata: statement;
  tparam: statement;
  prog_name: string;
  prog_path: string;
}
[@@deriving sexp, hash]

let map_statement = Base_prog_tree.map_statement

let id x = x
let map_udf_defn sf ef udf =
  {udf with body = Base_prog_tree.map_statement id sf ef udf.body}

let map_prog sf ef p =
  {p with functions = List.map ~f:(map_udf_defn sf ef) p.functions;
          model = map_statement id sf ef p.model;
          gq = map_statement id sf ef p.gq;
          tdata = map_statement id sf ef p.tdata;
          tparam = map_statement id sf ef p.tparam}
