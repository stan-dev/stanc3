open Core_kernel
open Stmt

(** Calculate the free (non-bound) variables in a statement *)
let free_vars stmt =
  let algebra : ('a, 'b, String.Set.t) Fixed.algebra = function
    | _, Assignment ((_, idxs), x) ->
        String.Set.(
          union x @@ union_list
          @@ List.concat_map ~f:Expr_helpers.index_bounds idxs)
    | _, Return e_opt -> Option.value ~default:String.Set.empty e_opt
    | _, TargetPE e -> e
    | _, NRFunApp (_, fname, args) ->
        String.Set.(union (singleton fname) @@ union_list args)
    | _, IfElse (pred, ts, fs_opt) ->
        String.Set.(
          union pred @@ union ts @@ Option.value ~default:empty fs_opt)
    | _, While (pred, body) -> String.Set.union pred body
    | _, For {lower; upper; body; _} ->
        String.Set.(union lower @@ union upper body)
    | _, Block xs | _, SList xs -> String.Set.union_list xs
    | _, Decl _ | _, Break | _, Continue | _, Skip -> String.Set.empty
  in
  Fixed.cata Expr_helpers.free_vars_algebra algebra stmt

let break meta = Fixed.fix meta Break
let continue meta = Fixed.fix meta Continue
let skip meta = Fixed.fix meta Skip
let target_pe meta e = Fixed.fix meta @@ TargetPE e

(* == Return statements ===================================================== *)

let return_ meta e = Fixed.fix meta @@ Return (Some e)
let return_void meta = Fixed.fix meta @@ Return None

let is_return stmt =
  match Fixed.pattern stmt with Return _ -> true | _ -> false

let return_value_opt stmt =
  match Fixed.pattern stmt with Return e -> e | _ -> None

(* == Assignement =========================================================== *)
let assign meta name ?(idxs = []) expr =
  Fixed.fix meta @@ Assignment ((name, idxs), expr)

let is_assignment stmt =
  match Fixed.pattern stmt with Assignment _ -> true | _ -> false

(* == Scoped blocks ========================================================= *)
let block meta xs = Fixed.fix meta @@ Block xs

let is_block stmt =
  match Fixed.pattern stmt with Block _ -> true | _ -> false

let lift_to_block stmt =
  match Fixed.pattern stmt with
  | Block _ -> stmt
  | _ ->
      let meta = Fixed.meta stmt in
      Fixed.fix meta @@ Block [stmt]

let block_statements stmt =
  match Fixed.pattern stmt with Block xs -> xs | _ -> [stmt]

let slist meta xs = Fixed.fix meta @@ SList xs

(* == While loops =========================================================== *)

let while_ meta pred body = Fixed.fix meta @@ While (pred, body)

let is_while stmt =
  match Fixed.pattern stmt with While _ -> true | _ -> false

(* == For loops ============================================================= *)

let for_ meta loopvar lower upper body =
  Fixed.fix meta @@ For {loopvar; lower; upper; body}

let is_for stmt = match Fixed.pattern stmt with For _ -> true | _ -> false

let body_opt stmt =
  match Fixed.pattern stmt with
  | While (_, body) | For {body; _} -> Some body
  | _ -> None

(* == If/Then/Else ========================================================== *)

let if_ meta pred s_true s_false_opt =
  Fixed.fix meta @@ IfElse (pred, s_true, s_false_opt)

let is_if_else stmt =
  match Fixed.pattern stmt with IfElse _ -> true | _ -> false

(* == Declarations ========================================================== *)

let declare_sized meta adtype name ty =
  Fixed.fix meta
  @@ Decl {decl_adtype= adtype; decl_id= name; decl_type= Type.Sized ty}

let declare_unsized meta adtype name ty =
  Fixed.fix meta
  @@ Decl {decl_adtype= adtype; decl_id= name; decl_type= Type.Unsized ty}

let is_decl stmt = match Fixed.pattern stmt with Decl _ -> true | _ -> false

(* == Side-effecting functions application ================================== *)

let nrfun_app meta fun_kind name args =
  Fixed.fix meta @@ NRFunApp (fun_kind, name, args)

let stanlib_nrfun meta name args = nrfun_app meta Fun_kind.StanLib name args

let internal_nrfun meta fn args =
  nrfun_app meta CompilerInternal (Internal_fun.to_string fn) args

let user_nrfun meta name args = nrfun_app meta UserDefined name args

let is_nrfun ?kind ?name stmt =
  match Fixed.pattern stmt with
  | NRFunApp (fun_kind, fun_name, _) ->
      let same_name =
        Option.value_map ~default:true ~f:(fun name -> name = fun_name) name
      and same_kind =
        Option.value_map ~default:true ~f:(fun kind -> kind = fun_kind) kind
      in
      same_name && same_kind
  | _ -> false

let is_internal_nrfun ?fn expr =
  is_nrfun expr ~kind:CompilerInternal
    ?name:(Option.map ~f:Internal_fun.to_string fn)

let contains_fun ?kind ?name stmt =
  let algebra = function
    | _, Fixed.Pattern.Break | _, Continue | _, Skip -> false
    | _, TargetPE e -> e
    | _, Return e_opt -> Option.value ~default:false e_opt
    | _, While (e, s) -> e || s
    | _, Assignment ((_, idxs), e) ->
        e
        || List.exists idxs ~f:(fun idx ->
               List.exists ~f:Fn.id @@ Expr_helpers.index_bounds idx )
    | _, IfElse (pred, t, f_opt) ->
        pred || t || Option.value ~default:false f_opt
    | _, For {lower; upper; body; _} -> lower || upper || body
    | _, Block xs | _, SList xs -> List.exists ~f:Fn.id xs
    | _, Decl {decl_type; _} ->
        List.exists ~f:Fn.id @@ Type.collect_exprs decl_type
    | _, NRFunApp (fun_kind, fun_name, args) ->
        Option.(
          value_map ~default:true ~f:(fun name -> name = fun_name) name
          && value_map ~default:true ~f:(fun kind -> kind = fun_kind) kind)
        || List.exists ~f:Fn.id args
  in
  Fixed.cata Expr_helpers.(contains_fun_algebra ?kind ?name) algebra stmt

let contains_operator ?op stmt =
  contains_fun ~kind:StanLib ?name:(Option.map ~f:Operator.to_string op) stmt

let contains_internal_fun ?fn expr =
  contains_fun ~kind:StanLib
    ?name:(Option.map ~f:Internal_fun.to_string fn)
    expr
