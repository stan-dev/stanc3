open Core_kernel
open Common
open State

(** Fixed-point of statements *)
module Fixed = struct
  module First = Expr.Fixed

  module Pattern = struct
    type ('a, 'b) t = ('a, 'b) Mir_pattern.statement =
      | Assignment of (string * 'a Expr.index list) * 'a
      | TargetPE of 'a
      | NRFunApp of Fun_kind.t * string * 'a list
      | Break
      | Continue
      | Return of 'a option
      | Skip
      | IfElse of 'a * 'b * 'b option
      | While of 'a * 'b
      | For of {loopvar: string; lower: 'a; upper: 'a; body: 'b}
      | Block of 'b list
      | SList of 'b list
      | Decl of
          { decl_adtype: UnsizedType.autodifftype
          ; decl_id: string
          ; decl_type: 'a Type.t }
    [@@deriving sexp, hash, map, fold, compare]

    let pp pp_a pp_b ppf x = Mir_pretty_printer.pp_statement pp_a pp_b ppf x

    include Bifoldable.Make (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let fold = fold
    end)

    module Make_traversable = Mir_pattern.Make_traversable_statement
    module Make_traversable2 = Mir_pattern.Make_traversable_statement2
  end

  include Fix.Make2 (First) (Pattern)
  module Traversable_state = Make_traversable2 (Cps.State)
  module Traversable_state_r = Make_traversable2 (Right.Cps.State)

  let map_accum_left ~f ~g ~init x =
    Cps.State.(
      Traversable_state.traverse x
        ~f:(fun a -> state @@ Fn.flip f a)
        ~g:(fun a -> state @@ Fn.flip g a)
      |> run_state ~init)

  let map_accum_right ~f ~g ~init x =
    Right.Cps.State.(
      Traversable_state_r.traverse x
        ~f:(fun a -> state @@ Fn.flip f a)
        ~g:(fun a -> state @@ Fn.flip g a)
      |> run_state ~init)
end

(** Statements with no meta-data *)
module NoMeta = struct
  module Meta = struct
    type t = unit [@@deriving compare, sexp, hash]

    let pp _ _ = ()
  end

  include Specialized.Make2 (Fixed) (Expr.NoMeta) (Meta)

  let remove_meta x = Fixed.map (fun _ -> ()) (fun _ -> ()) x
end

(** Statements with location information and types for contained expressions *)
module Located = struct
  module Meta = struct
    type t = (Location_span.t sexp_opaque[@compare.ignore])
    [@@deriving compare, sexp, hash]

    let pp _ _ = ()
  end

  include Specialized.Make2 (Fixed) (Expr.Typed) (Meta)

  let loc_of x = Fixed.meta x
end

(** Statements with location information, labels and types for contained 
expressions 
*)
module Labelled = struct
  module Meta = struct
    type t =
      { loc: Location_span.t sexp_opaque [@compare.ignore]
      ; label: Int_label.t [@compare.ignore] }
    [@@deriving compare, create, sexp, hash]

    let label {label; _} = label
    let loc {loc; _} = loc
    let pp _ _ = ()
  end

  include Specialized.Make2 (Fixed) (Expr.Labelled) (Meta)

  let label_of x = Meta.label @@ Fixed.meta x
  let loc_of x = Meta.loc @@ Fixed.meta x

  module Traversable_state = Fixed.Make_traversable2 (State)

  let label ?(init = 0) (stmt : Located.t) : t =
    let incr_label =
      State.(get >>= fun label -> put (label + 1) >>= fun _ -> return label)
    in
    let f {Expr.Typed.Meta.adlevel; type_; loc} =
      incr_label
      |> State.map ~f:(fun label ->
             Expr.Labelled.Meta.create ~type_ ~loc ~adlevel ~label () )
    and g loc =
      incr_label |> State.map ~f:(fun label -> Meta.create ~loc ~label ())
    in
    Traversable_state.traverse ~f ~g stmt |> State.run_state ~init |> fst

  type associations =
    {exprs: Expr.Labelled.t Int_label.Map.t; stmts: t Int_label.Map.t}

  let empty = {exprs= Int_label.Map.empty; stmts= Int_label.Map.empty}

  let rec associate ?init:(assocs = empty) (stmt : t) =
    associate_pattern
      { assocs with
        stmts=
          Int_label.Map.add_exn assocs.stmts ~key:(label_of stmt) ~data:stmt }
      (Fixed.pattern stmt)

  and associate_pattern assocs = function
    | Fixed.Pattern.Break | Skip | Continue | Return None -> assocs
    | Return (Some e) | TargetPE e ->
        {assocs with exprs= Expr.Labelled.associate ~init:assocs.exprs e}
    | NRFunApp (_, _, args) ->
        { assocs with
          exprs=
            List.fold args ~init:assocs.exprs ~f:(fun accu x ->
                Expr.Labelled.associate ~init:accu x ) }
    | Assignment ((_, idxs), rhs) ->
        let exprs =
          Expr.Labelled.(
            associate rhs
              ~init:(List.fold ~f:associate_index ~init:assocs.exprs idxs))
        in
        {assocs with exprs}
    | IfElse (pred, body, None) | While (pred, body) ->
        let exprs = Expr.Labelled.associate ~init:assocs.exprs pred in
        associate ~init:{assocs with exprs} body
    | IfElse (pred, ts, Some fs) ->
        let exprs = Expr.Labelled.associate ~init:assocs.exprs pred in
        let assocs' = {assocs with exprs} in
        associate ~init:(associate ~init:assocs' ts) fs
    | Decl {decl_type; _} -> associate_possibly_sized_type assocs decl_type
    | For {lower; upper; body; _} ->
        let exprs =
          Expr.Labelled.(
            associate ~init:(associate ~init:assocs.exprs lower) upper)
        in
        let assocs' = {assocs with exprs} in
        associate ~init:assocs' body
    | Block xs | SList xs ->
        List.fold ~f:(fun accu x -> associate ~init:accu x) ~init:assocs xs

  and associate_possibly_sized_type assocs = function
    | Mir_pattern.Sized st ->
        {assocs with exprs= SizedType.associate ~init:assocs.exprs st}
    | Mir_pattern.Unsized _ -> assocs
end

(* == Helpers =============================================================== *)

let fix = Fixed.fix
let inj = Fixed.inj
let proj = Fixed.proj
let meta = Fixed.meta
let pattern = Fixed.pattern
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
               List.exists ~f:Fn.id @@ Expr.index_bounds idx )
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
  Fixed.cata Expr.(contains_fun_algebra ?kind ?name) algebra stmt

let contains_operator ?op stmt =
  contains_fun ~kind:StanLib ?name:(Option.map ~f:Operator.to_string op) stmt

let contains_internal_fun ?fn expr =
  contains_fun ~kind:StanLib
    ?name:(Option.map ~f:Internal_fun.to_string fn)
    expr

(* == General helpers ======================================================= *)
