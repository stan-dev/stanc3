(** Defines the core of the MIR *)

open Core_kernel

type fun_arg_decl = (UnsizedType.autodifftype * string * UnsizedType.t) list
[@@deriving sexp, hash, map]

type 'a fun_def =
  { fdrt: UnsizedType.t option
  ; fdname: string
  ; fdsuffix: unit Fun_kind.suffix
  ; fdargs: (UnsizedType.autodifftype * string * UnsizedType.t) list
        (* If fdbody is None, this is a function declaration without body. *)
  ; fdbody: 'a option
  ; fdloc: (Location_span.t[@sexp.opaque] [@compare.ignore]) }
[@@deriving compare, hash, sexp, map, fold]

type io_block = Parameters | TransformedParameters | GeneratedQuantities
[@@deriving sexp, hash]

type 'e outvar =
  { out_unconstrained_st: 'e SizedType.t
  ; out_constrained_st: 'e SizedType.t
  ; out_block: io_block
  ; out_trans: 'e Transformation.t }
[@@deriving sexp, map, hash, fold]

type ('a, 'b) t =
  { functions_block: 'b fun_def list
  ; input_vars: (string * 'a SizedType.t) list
  ; prepare_data: 'b list (* data & transformed data decls and statements *)
  ; log_prob: 'b list (*assumes data & params are in scope and ready*)
  ; generate_quantities: 'b list (* assumes data & params ready & in scope*)
  ; transform_inits: 'b list
  ; output_vars: (string * 'a outvar) list
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp, map, fold]

let map_stmts f p =
  { p with
    prepare_data= f p.prepare_data
  ; log_prob= f p.log_prob
  ; generate_quantities= f p.generate_quantities
  ; transform_inits= f p.transform_inits }

(* -- Pretty printers -- *)
let pp_fun_arg_decl ppf (autodifftype, name, unsizedtype) =
  Fmt.pf ppf "%a%a %s" UnsizedType.pp_autodifftype autodifftype UnsizedType.pp
    unsizedtype name

let pp_fun_def pp_s ppf = function
  | {fdrt; fdname; fdargs; fdbody; _} -> (
      let pp_body_opt ppf = function
        | None -> Fmt.pf ppf ";"
        | Some body -> pp_s ppf body in
      match fdrt with
      | Some rt ->
          Fmt.pf ppf "@[<v2>%a %s%a {@ %a@]@ }" UnsizedType.pp rt fdname
            Fmt.(list pp_fun_arg_decl ~sep:comma |> parens)
            fdargs pp_body_opt fdbody
      | None ->
          Fmt.pf ppf "@[<v2>void %s%a {@ %a@]@ }" fdname
            Fmt.(list pp_fun_arg_decl ~sep:comma |> parens)
            fdargs pp_body_opt fdbody )

let pp_io_block ppf = function
  | Parameters -> Fmt.string ppf "parameters"
  | TransformedParameters -> Fmt.string ppf "transformed_parameters"
  | GeneratedQuantities -> Fmt.string ppf "generated_quantities"

let pp_block label pp_elem ppf = function
  | [] -> ()
  | elems ->
      Fmt.pf ppf "@[<v2>%s {@ %a@]@ }@\n" label
        Fmt.(list ~sep:cut pp_elem)
        elems

let pp_functions_block pp_s ppf functions_block =
  pp_block "functions" pp_s ppf functions_block

let pp_prepare_data pp_s ppf prepare_data =
  pp_block "prepare_data" pp_s ppf prepare_data

let pp_log_prob pp_s ppf log_prob = pp_block "log_prob" pp_s ppf log_prob

let pp_generate_quantities pp_s ppf generate_quantities =
  pp_block "generate_quantities" pp_s ppf generate_quantities

let pp_transform_inits pp_s ppf transform_inits =
  pp_block "transform_inits" pp_s ppf transform_inits

let pp_output_var pp_e ppf
    (name, {out_unconstrained_st; out_constrained_st; out_block; _}) =
  Fmt.pf ppf "@[<h>%a %a %s; //%a@]" pp_io_block out_block (SizedType.pp pp_e)
    out_constrained_st name (SizedType.pp pp_e) out_unconstrained_st

let pp_input_var pp_e ppf (name, sized_ty) =
  Fmt.pf ppf "@[<h>%a %s;@]" (SizedType.pp pp_e) sized_ty name

let pp_input_vars pp_e ppf input_vars =
  pp_block "input_vars" (pp_input_var pp_e) ppf input_vars

let pp_output_vars pp_e ppf output_vars =
  pp_block "output_vars" (pp_output_var pp_e) ppf output_vars

let pp pp_e pp_s ppf
    { functions_block
    ; input_vars
    ; prepare_data
    ; log_prob
    ; generate_quantities
    ; transform_inits
    ; output_vars
    ; _ } =
  Format.open_vbox 0 ;
  pp_functions_block (pp_fun_def pp_s) ppf functions_block ;
  Fmt.cut ppf () ;
  pp_input_vars pp_e ppf input_vars ;
  Fmt.cut ppf () ;
  pp_prepare_data pp_s ppf prepare_data ;
  Fmt.cut ppf () ;
  pp_log_prob pp_s ppf log_prob ;
  Fmt.cut ppf () ;
  pp_generate_quantities pp_s ppf generate_quantities ;
  Fmt.cut ppf () ;
  pp_transform_inits pp_s ppf transform_inits ;
  Fmt.cut ppf () ;
  pp_output_vars pp_e ppf output_vars ;
  Format.close_box ()

(** Programs with typed expressions and locations *)
module Typed = struct
  type nonrec t = (Expr.Typed.t, Stmt.Located.t) t

  let pp ppf x = pp Expr.Typed.pp Stmt.Located.pp ppf x
  let sexp_of_t = sexp_of_t Expr.Typed.sexp_of_t Stmt.Located.sexp_of_t
  let t_of_sexp = t_of_sexp Expr.Typed.t_of_sexp Stmt.Located.t_of_sexp
end

module Numbered = struct
  type nonrec t = (Expr.Typed.t, Stmt.Numbered.t) t

  let pp ppf x = pp Expr.Typed.pp Stmt.Numbered.pp ppf x
  let sexp_of_t = sexp_of_t Expr.Typed.sexp_of_t Stmt.Numbered.sexp_of_t
  let t_of_sexp = t_of_sexp Expr.Typed.t_of_sexp Stmt.Numbered.t_of_sexp
end
