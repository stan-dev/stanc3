open Std
open Std.Compare
open Std.Sexp_conv
open Middle
open Mir_utils

type step = Subscript of Index_form.index Index.t | Field of int
type kind = Read | Write | Increment [@@deriving equal]
type access = {var: string; path: step list; kind: kind}

(** Builds paths from expressions and from left-hand sides of assignments. A
    path here still holds the subscript expressions, as [`Subscript] and
    [`Field] steps, so that the reads inside the subscripts can be collected
    before {!classify_path} classifies them. *)
module Path = struct
  (** The variable and the path that [expr] selects, or [None] if [expr] is not
      a variable with subscripts and fields, or indexes after a slice. *)
  let rec of_expr (expr : Expr.Typed.t) :
      (string * [`Subscript of Expr.Typed.t Index.t | `Field of int] list)
      option =
    let extend_path base steps =
      (* After a slice or a multi-index, later subscripts index the result of
         the selection. The path of [base] and [steps] then cannot join into one
         path of the base variable. *)
      match of_expr base with
      | Some (name, prefix)
        when List.for_all prefix ~f:(function
               | `Subscript (Index.Single _) | `Field _ -> true
               | `Subscript _ -> false) ->
          Some (name, prefix @ steps)
      | Some _ | None -> None in
    match expr.pattern with
    | Var name -> Some (name, [])
    | Indexed (base, indices) ->
        extend_path base (List.map indices ~f:(fun index -> `Subscript index))
    | TupleProjection (base, field) -> extend_path base [`Field field]
    | FunApp _ | TernaryIf _ | Lit _ | EAnd _ | EOr _ | Promotion _ -> None

  (** The path of the left-hand side of an assignment. *)
  let rec of_lvalue ((lbase, indices) : Expr.Typed.t Stmt.Pattern.lvalue) :
      [`Subscript of Expr.Typed.t Index.t | `Field of int] list =
    (match lbase with
      | LVariable _ -> []
      | LTupleProjection (inner, field) -> of_lvalue inner @ [`Field field])
    @ List.map indices ~f:(fun index -> `Subscript index)

  (** The expressions inside the subscripts of [path]. *)
  let bounds path : Expr.Typed.t list =
    List.concat_map path ~f:(function
      | `Subscript index -> Index.bounds index
      | `Field _ -> [])

  (** Classifies each subscript of [path] with [classify]. *)
  let classify_path ~(classify : Expr.Typed.t -> Index_form.index) path :
      step list =
    List.map path ~f:(function
      | `Subscript index -> Subscript (Index.map classify index)
      | `Field field -> Field field)
end

(** An increment of the whole of [target]. *)
let increment_target : access = {var= "target"; path= []; kind= Increment}

(** The accesses in [expr]. A subscripted variable gives one read with the whole
    path, then the accesses in the subscripts. *)
let rec of_expr ~classify (expr : Expr.Typed.t) : access list =
  let of_children () =
    Expr.Pattern.fold
      (fun running_accesses subexpr ->
        running_accesses @ of_expr ~classify subexpr)
      [] expr.pattern in
  let read var path = [{var; path; kind= Read}] in
  match expr.pattern with
  | Var name -> read name []
  | Indexed _ | TupleProjection _ -> (
      match Path.of_expr expr with
      | Some (name, path) ->
          read name (Path.classify_path ~classify path)
          @ List.concat_map (Path.bounds path) ~f:(of_expr ~classify)
      | None -> of_children ())
  | FunApp (StanLib (_, FnTarget), []) -> read "target" []
  | FunApp (fun_kind, _) when increments_target fun_kind ->
      of_children () @ [increment_target]
  | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ ->
      of_children ()

let of_stmt ~(classify : Expr.Typed.t -> Index_form.index)
    (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) : access list =
  (* The accesses of each nested statement are collected separately. The
     statement fold thus visits only the expressions of [stmt] and skips nested
     statements. *)
  let of_children () =
    Stmt.Pattern.fold
      (fun running_accesses subexpr ->
        running_accesses @ of_expr ~classify subexpr)
      Fun.const [] stmt in
  let write var path = [{var; path; kind= Write}] in
  match stmt with
  | Assignment (lhs, _, rhs) ->
      let path = Path.of_lvalue lhs in
      List.concat_map (Path.bounds path @ [rhs]) ~f:(of_expr ~classify)
      @ write
          (Stmt.Helpers.lhs_variable lhs)
          (Path.classify_path ~classify path)
  | Decl {decl_id; _} -> of_children () @ write decl_id []
  | TargetPE _ | JacobianPE _ -> of_children () @ [increment_target]
  | NRFunApp (fun_kind, _) when increments_target fun_kind ->
      of_children () @ [increment_target]
  | NRFunApp _ | Return _ | IfElse _ | While _ | For _ | Profile _ | Block _
   |SList _ | Break | Continue | Skip ->
      of_children ()

let of_var (var : string) (accesses : access list) : access list =
  List.filter accesses ~f:(fun access -> String.equal access.var var)

let read_vars (accesses : access list) : string Set.Poly.t =
  Set.Poly.of_list
    (List.filter_map accesses ~f:(fun {var; kind; _} ->
         match kind with Read | Increment -> Some var | Write -> None))

let written_vars (accesses : access list) : string Set.Poly.t =
  Set.Poly.of_list
    (List.filter_map accesses ~f:(fun {var; kind; _} ->
         match kind with Write | Increment -> Some var | Read -> None))

(** Prints one subscript, with braces around a multi-index. *)
let pp_subscript ppf (index : Index_form.index Index.t) =
  match index with
  | MultiIndex indices -> Fmt.pf ppf "{%a}" Index_form.pp indices
  | All | Single _ | Upfrom _ | Between _ -> Index.pp Index_form.pp ppf index

(** Prints a path. [pp_subscripts_after] continues an open bracket. Consecutive
    subscripts thus share one pair of brackets. *)
let rec pp_path ppf = function
  | [] -> ()
  | Field field :: rest -> Fmt.pf ppf ".%d%a" field pp_path rest
  | Subscript index :: rest ->
      Fmt.pf ppf "[%a%a" pp_subscript index pp_subscripts_after rest

and pp_subscripts_after ppf = function
  | Subscript index :: rest ->
      Fmt.pf ppf ", %a%a" pp_subscript index pp_subscripts_after rest
  | path -> Fmt.pf ppf "]%a" pp_path path

(** Prints [R], [+=] or [W]. *)
let pp_kind ppf kind =
  Fmt.string ppf
    (match kind with Read -> "R" | Increment -> "+=" | Write -> "W")

let pp_access ppf {var; path; kind} =
  Fmt.pf ppf "%a %s%a" pp_kind kind var pp_path path

let pp ppf (accesses : access list) =
  let of_kind wanted =
    List.filter accesses ~f:(fun access -> equal_kind access.kind wanted) in
  Fmt.(list ~sep:(any ", ") pp_access)
    ppf
    (of_kind Read @ of_kind Increment @ of_kind Write)
