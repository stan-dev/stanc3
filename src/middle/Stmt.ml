open Core_kernel
open Common 
open State.Cps



type 'a possiblysizedtype = 'a Mir_pattern.possiblysizedtype =
  | Sized of 'a SizedType.t | Unsized of UnsizedType.t
[@@deriving sexp, compare, map, hash, fold]

module Fixed = struct
  module First = Expr.Fixed
  module Pattern  = struct
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
              ; decl_type: 'a possiblysizedtype }
      [@@deriving sexp, hash, map, fold, compare]

      
      let pp pp_a pp_b ppf x = Mir_pretty_printer.pp_statement pp_a pp_b ppf x

      include Bifoldable.Make (struct
          type nonrec ('a, 'b) t = ('a, 'b) t

          let fold = fold
      end)

      module Make_traversable = Mir_pattern.Make_traversable_statement
      module Make_traversable2 = Mir_pattern.Make_traversable_statement2
  end
  include Fix.Make2 (First)(Pattern)
end

module NoMeta = struct  
  module Meta = Mir_meta.NoMeta
  include Specialized.Make2 (Fixed) (Expr.NoMeta) (Meta)
  let remove_meta x = Fixed.map (fun _ -> ()) (fun _ -> ()) x
end

module Typed = struct 
  module Meta = Mir_meta.Typed   
  include Specialized.Make2 (Fixed) (Expr.Typed) (Meta)
  let type_of x = Mir_meta.Typed.type_ @@ Fixed.meta x
  let loc_of x = Mir_meta.Typed.loc @@ Fixed.meta x
  let adlevel_of x = Mir_meta.Typed.adlevel @@ Fixed.meta x
end

module Labelled = struct
  module Meta = Mir_meta.Labelled
  include Specialized.Make2 (Fixed) (Expr.Labelled) (Meta)

  let label_of x = Mir_meta.Labelled.label @@ Fixed.meta x
  let type_of x = Mir_meta.Labelled.type_ @@ Fixed.meta x
  let loc_of x = Mir_meta.Labelled.loc @@ Fixed.meta x
  let adlevel_of x = Mir_meta.Labelled.adlevel @@ Fixed.meta x

  module Traversable_state = Fixed.Make_traversable2 (State)

  let label ?(init = 0) (stmt : Typed.t) : t =
    let incr_label =
      State.(get >>= fun label -> put (label + 1) >>= fun _ -> return label)
    in
    let f {Expr.Typed.Meta.adlevel; type_; loc} =
      incr_label
      |> State.map ~f:(fun label ->
              Expr.Labelled.Meta.create ~type_ ~loc ~adlevel ~label () )
    and g {Mir_meta.Typed.adlevel; type_; loc} =
      incr_label
      |> State.map ~f:(fun label ->
              Mir_meta.Labelled.create ~type_ ~loc ~adlevel ~label () )
    in
    Traversable_state.traverse ~f ~g stmt |> State.run_state ~init |> fst

  type associations =
    {exprs: Expr.Labelled.t Label.Map.t; stmts: t Label.Map.t}

  let empty = {exprs= Label.Map.empty; stmts= Label.Map.empty}

  let rec associate ?init:(assocs = empty) (stmt : t) =
    associate_pattern
      { assocs with
        stmts= Label.Map.add_exn assocs.stmts ~key:(label_of stmt) ~data:stmt
      }
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
    | Mir_pattern.Sized st -> associate_sized_type assocs st
    | Mir_pattern.Unsized _ -> assocs

  and associate_sized_type assocs = function
    | Mir_pattern.SInt | SReal -> assocs
    | SVector e | SRowVector e ->
        let exprs = Expr.Labelled.associate ~init:assocs.exprs e in
        {assocs with exprs}
    | SMatrix (e1, e2) ->
        let exprs =
          Expr.Labelled.(
            associate ~init:(associate ~init:assocs.exprs e1) e2)
        in
        {assocs with exprs}
    | SArray (st, e) ->
        let exprs = Expr.Labelled.associate ~init:assocs.exprs e in
        let assocs' = {assocs with exprs} in
        associate_sized_type assocs' st
end

(* == Helpers ============================================================= *)

let assign meta name ?(idxs = []) expr =
 Fixed.fix meta @@ Assignment ((name, idxs), expr)

let target_pe meta e = Fixed.fix meta @@ TargetPE e
let break meta = Fixed.fix meta Break
let continue meta = Fixed.fix meta Continue
let skip meta = Fixed.fix meta Skip
let return_ meta e = Fixed.fix meta @@ Return (Some e)
let return_void meta = Fixed.fix meta @@ Return None
let block meta xs = Fixed.fix meta @@ Block xs
let slist meta xs = Fixed.fix meta @@ SList xs
let while_ meta pred body = Fixed.fix meta @@ While (pred, body)

let if_ meta pred ?when_false ~when_true =
  Fixed.fix meta @@ IfElse (pred, when_true, when_false)

let for_ meta loopvar lower upper body =
  Fixed.fix meta @@ For {loopvar; lower; upper; body}


let sized ty = Sized ty 

let unsized ty  = Unsized ty

let declare_sized meta adtype name ty =
  Fixed.fix meta
  @@ Decl
        {decl_adtype= adtype; decl_id= name; decl_type= sized ty}

let declare_unsized meta adtype name ty =
  Fixed.fix meta
  @@ Decl
        {decl_adtype= adtype; decl_id= name; decl_type= unsized ty}

let stanlib_fun meta name args =
  Fixed.fix meta @@ NRFunApp (Fun_kind.StanLib, name, args)

let compiler_fun meta name args =
  Fixed.fix meta @@ NRFunApp (Fun_kind.CompilerInternal, name, args)

let user_fun meta name args =
  Fixed.fix meta @@ NRFunApp (Fun_kind.UserDefined, name, args)


