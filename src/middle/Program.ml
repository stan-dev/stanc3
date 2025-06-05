(** Defines the core of the MIR *)

open Core

type fun_arg_decl = (UnsizedType.autodifftype * string * UnsizedType.t) list
[@@deriving sexp, hash, map]

type 'a fun_def =
  { fdrt: UnsizedType.returntype
  ; fdname: string
  ; fdsuffix: unit Fun_kind.suffix
  ; fdargs: (UnsizedType.autodifftype * string * UnsizedType.t) list
  ; fdbody: 'a option
        (* If fdbody is None, this is an external function declaration
           (forward decls are removed during AST lowering) *)
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

type ('a, 'b, 'm) t =
  { functions_block: 'b fun_def list
  ; input_vars: (string * 'm * 'a SizedType.t) list
  ; prepare_data: 'b list (* data & transformed data decls and statements *)
  ; log_prob: 'b list (*assumes data & params are in scope and ready*)
  ; reverse_mode_log_prob: 'b list
        (* assumes data & params ready & in scope.
           A copy of log_prob but with optimizations which are specific
           to reverse-mode autodiff. This is used in the C++ backend,
           but can be ignored if not needed. It is initialized
            to [[]] in [Ast_to_Mir], set it equal to log_prob
           before calling into the optimization suite if desired. *)
  ; generate_quantities: 'b list
        (* assumes data & params ready & in scope*)
        (* NOTE: the following two items are really backend-specific,
           and are set to [] by Ast_to_mir before being populated in
           Stan_math_backend.Transform_Mir.
           It would be nice to abstract this out somehow
        *)
  ; transform_inits: 'b list
  ; unconstrain_array: 'b list
  ; output_vars: (string * 'm * 'a outvar) list
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp, map, fold]

(* -- Pretty printers -- *)
let pp_fun_arg_decl ppf (autodifftype, name, unsizedtype) =
  Fmt.pf ppf "%a%a %s" UnsizedType.pp_autodifftype autodifftype UnsizedType.pp
    unsizedtype name

let pp_fun_def pp_s ppf {fdrt; fdname; fdargs; fdbody; _} =
  match fdbody with
  | None ->
      Fmt.pf ppf "@[<v2>extern %a %s%a;@]" UnsizedType.pp_returntype fdrt fdname
        Fmt.(list pp_fun_arg_decl ~sep:comma |> parens)
        fdargs
  | Some s ->
      Fmt.pf ppf "@[<v2>%a %s%a {@ %a@]@ }" UnsizedType.pp_returntype fdrt
        fdname
        Fmt.(list pp_fun_arg_decl ~sep:comma |> parens)
        fdargs pp_s s

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

let pp_reverse_mode_log_prob pp_s ppf log_prob =
  pp_block "rev_log_prob" pp_s ppf log_prob

let pp_generate_quantities pp_s ppf generate_quantities =
  pp_block "generate_quantities" pp_s ppf generate_quantities

let pp_transform_inits pp_s ppf transform_inits =
  pp_block "transform_inits" pp_s ppf transform_inits

let pp_output_var pp_e ppf
    (name, _, {out_unconstrained_st; out_constrained_st; out_block; _}) =
  Fmt.pf ppf "@[<hov 2>%a %a %s;@ //%a@]" pp_io_block out_block
    (SizedType.pp pp_e) out_constrained_st name (SizedType.pp pp_e)
    out_unconstrained_st

let pp_input_var pp_e ppf (name, _, sized_ty) =
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
    ; reverse_mode_log_prob
    ; generate_quantities
    ; transform_inits
    ; output_vars
    ; _ } =
  Format.open_vbox 0;
  pp_functions_block (pp_fun_def pp_s) ppf functions_block;
  Fmt.cut ppf ();
  pp_input_vars pp_e ppf input_vars;
  Fmt.cut ppf ();
  pp_prepare_data pp_s ppf prepare_data;
  Fmt.cut ppf ();
  pp_log_prob pp_s ppf log_prob;
  Fmt.cut ppf ();
  pp_reverse_mode_log_prob pp_s ppf reverse_mode_log_prob;
  Fmt.cut ppf ();
  pp_generate_quantities pp_s ppf generate_quantities;
  Fmt.cut ppf ();
  pp_transform_inits pp_s ppf transform_inits;
  Fmt.cut ppf ();
  pp_output_vars pp_e ppf output_vars;
  Format.close_box ()

(** Programs with typed expressions and locations *)
module Typed = struct
  type nonrec t = (Expr.Typed.t, Stmt.Located.t, Location_span.t) t

  let pp ppf x = pp Expr.Fixed.pp Stmt.Fixed.pp ppf x

  let sexp_of_t =
    sexp_of_t Expr.Typed.sexp_of_t Stmt.Located.sexp_of_t
      Location_span.sexp_of_t

  let t_of_sexp =
    t_of_sexp Expr.Typed.t_of_sexp Stmt.Located.t_of_sexp
      Location_span.t_of_sexp
end

module Numbered = struct
  type nonrec t = (Expr.Typed.t, Stmt.Numbered.t, int) t

  let pp ppf x = pp Expr.Fixed.pp Stmt.Fixed.pp ppf x

  let sexp_of_t =
    sexp_of_t Expr.Typed.sexp_of_t Stmt.Numbered.sexp_of_t sexp_of_int

  let t_of_sexp =
    t_of_sexp Expr.Typed.t_of_sexp Stmt.Numbered.t_of_sexp int_of_sexp
end
