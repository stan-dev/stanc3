open Core_kernel
open State.Cps

(* == Types of meta data we special MIR nodes with ========================== *)
module NoMeta = struct
  type t = unit [@@deriving compare, sexp, hash]

  let pp _ _ = ()
end

module TypedLocatedMeta = struct
  type t =
    { mtype: Mir_pattern.unsizedtype
    ; mloc: Location_span.t sexp_opaque [@compare.ignore]
    ; madlevel: Mir_pattern.autodifftype }
  [@@deriving compare, create, sexp, hash]

  let adlevel {madlevel; _} = madlevel
  let type_ {mtype; _} = mtype
  let loc {mloc; _} = mloc
  let pp _ _ = ()
end

module LabelledMeta = struct
  type t =
    { mtype: Mir_pattern.unsizedtype
    ; mloc: Location_span.t sexp_opaque [@compare.ignore]
    ; madlevel: Mir_pattern.autodifftype
    ; mlabel: Label.t [@compare.ignore] }
  [@@deriving compare, create, sexp, hash]

  let label {mlabel; _} = mlabel
  let adlevel {madlevel; _} = madlevel
  let type_ {mtype; _} = mtype
  let loc {mloc; _} = mloc
  let pp _ _ = ()
end

(* == Fixed-point MIR data-types and their specializations ================== *)

module Expr = struct
  (** Fixed-point of `expr` *)
  module Fixed = Fix.Make (struct
    type 'a t = 'a Mir_pattern.expr

    let map f x = Mir_pattern.map_expr f x
    let fold f init x = Mir_pattern.fold_expr f init x
    let compare f x y = Mir_pattern.compare_expr f x y
    let hash_fold_t = Mir_pattern.hash_fold_expr
    let sexp_of_t = Mir_pattern.sexp_of_expr
    let t_of_sexp = Mir_pattern.expr_of_sexp
    let pp pp_e ppf = Mir_pretty_printer.pp_expr pp_e ppf

    include Foldable.Make (struct type nonrec 'a t = 'a t

                                  let fold = fold
    end)

    module Make_traversable = Mir_pattern.Make_traversable_expr
    module Make_traversable2 = Mir_pattern.Make_traversable_expr2
  end)

  (** Fixed-point expressions specialized to have no meta data *)
  module NoMeta = struct include Meta.Specialize (Fixed) (NoMeta)
  end

  module TypedLocated = struct
    include Meta.Specialize (Fixed) (TypedLocatedMeta)

    let type_of x = LabelledMeta.type_ @@ Fixed.meta x
    let loc_of x = LabelledMeta.loc @@ Fixed.meta x
    let adlevel_of x = LabelledMeta.label @@ Fixed.meta x
  end

  module Labelled = struct
    include Meta.Specialize (Fixed) (LabelledMeta)

    let label_of x = LabelledMeta.label @@ Fixed.meta x
    let type_of x = LabelledMeta.type_ @@ Fixed.meta x
    let loc_of x = LabelledMeta.loc @@ Fixed.meta x
    let adlevel_of x = LabelledMeta.label @@ Fixed.meta x

    module Traversable_state = Fixed.Make_traversable2 (State)

    let label ?(init = 0) (expr : TypedLocated.t) : t =
      let f {TypedLocatedMeta.madlevel; mtype; mloc} =
        State.(
          get
          >>= fun mlabel ->
          put (mlabel + 1)
          >>= fun _ ->
          return @@ LabelledMeta.create ~mlabel ~madlevel ~mtype ~mloc ())
      in
      Traversable_state.traverse ~f expr |> State.run_state ~init |> fst

    let rec associate ?init:(assocs = Label.Map.empty) (expr : t) =
      Label.Map.add_exn
        (associate_pattern assocs @@ Fixed.pattern expr)
        ~key:(label_of expr) ~data:expr

    and associate_pattern assocs = function
      | Mir_pattern.Lit _ | Var _ -> assocs
      | FunApp (_, _, args) ->
          List.fold args ~init:assocs ~f:(fun accu x -> associate ~init:accu x)
      | EAnd (e1, e2) | EOr (e1, e2) ->
          associate ~init:(associate ~init:assocs e2) e1
      | TernaryIf (e1, e2, e3) ->
          associate ~init:(associate ~init:(associate ~init:assocs e3) e2) e1
      | Indexed (e, idxs) ->
          List.fold idxs ~init:(associate ~init:assocs e) ~f:associate_index

    and associate_index assocs = function
      | Mir_pattern.All -> assocs
      | Single e | Upfrom e | MultiIndex e -> associate ~init:assocs e
      | Between (e1, e2) -> associate ~init:(associate ~init:assocs e2) e1
  end

  (* == Helpers ============================================================= *)

  let var meta name = Fixed.fix meta @@ Mir_pattern.Var name

  let lit_int meta value =
    Fixed.fix meta @@ Mir_pattern.Lit (Mir_pattern.Int, string_of_int value)

  let lit_real meta value =
    Fixed.fix meta @@ Mir_pattern.Lit (Mir_pattern.Real, string_of_float value)

  let lit_string meta value =
    Fixed.fix meta @@ Mir_pattern.Lit (Mir_pattern.Str, value)

  let if_ meta pred true_expr false_expr =
    Fixed.fix meta @@ Mir_pattern.TernaryIf (pred, true_expr, false_expr)

  let and_ meta e1 e2 = Fixed.fix meta @@ Mir_pattern.EAnd (e1, e2)
  let or_ meta e1 e2 = Fixed.fix meta @@ Mir_pattern.EOr (e1, e2)
  let all = Mir_pattern.All
  let single e = Mir_pattern.Single e
  let multi e = Mir_pattern.MultiIndex e
  let upfrom e = Mir_pattern.Upfrom e
  let between e1 e2 = Mir_pattern.Between (e1, e2)
  let indexed meta e idxs = Fixed.fix meta @@ Mir_pattern.Indexed (e, idxs)

  let compiler_fun meta name args =
    Fixed.fix meta @@ Mir_pattern.(FunApp (CompilerInternal, name, args))

  let user_fun meta name args =
    Fixed.fix meta @@ Mir_pattern.(FunApp (UserDefined, name, args))

  let stanlib_fun meta name args =
    Fixed.fix meta @@ Mir_pattern.(FunApp (StanLib, name, args))

  (* TODO: use operators instead?*)
  let plus meta a b = stanlib_fun meta "plus" [a; b]
  let gt meta a b = stanlib_fun meta "gt" [a; b]
end

module Stmt = struct
  module Fixed =
    Fix.Make2
      (Expr.Fixed)
      (struct
        type ('a, 'b) t = ('a, 'b) Mir_pattern.statement

        let map f g x = Mir_pattern.map_statement f g x
        let fold f g init x = Mir_pattern.fold_statement f g init x
        let compare f g x y = Mir_pattern.compare_statement f g x y
        let hash_fold_t = Mir_pattern.hash_fold_statement
        let sexp_of_t = Mir_pattern.sexp_of_statement
        let t_of_sexp = Mir_pattern.statement_of_sexp
        let pp pp_a pp_b ppf = Mir_pretty_printer.pp_statement pp_a pp_b ppf

        include Bifoldable.Make (struct
          type nonrec ('a, 'b) t = ('a, 'b) t

          let fold = fold
        end)

        module Make_traversable = Mir_pattern.Make_traversable_statement
        module Make_traversable2 = Mir_pattern.Make_traversable_statement2
      end)

  module NoMeta = struct
    include Meta.Specialize2 (Fixed) (Expr.NoMeta) (NoMeta)
  end

  module TypedLocated = struct
    include Meta.Specialize2 (Fixed) (Expr.TypedLocated) (TypedLocatedMeta)

    let type_of x = LabelledMeta.type_ @@ Fixed.meta x
    let loc_of x = LabelledMeta.loc @@ Fixed.meta x
    let adlevel_of x = LabelledMeta.label @@ Fixed.meta x
  end

  module Labelled = struct
    include Meta.Specialize2 (Fixed) (Expr.Labelled) (LabelledMeta)

    let label_of x = LabelledMeta.label @@ Fixed.meta x
    let type_of x = LabelledMeta.type_ @@ Fixed.meta x
    let loc_of x = LabelledMeta.loc @@ Fixed.meta x
    let adlevel_of x = LabelledMeta.label @@ Fixed.meta x

    module Traversable_state = Fixed.Make_traversable2 (State)

    let label ?(init = 0) (stmt : TypedLocated.t) : t =
      let incr_label =
        State.(get >>= fun label -> put (label + 1) >>= fun _ -> return label)
      in
      let f {TypedLocatedMeta.madlevel; mtype; mloc} =
        incr_label
        |> State.map ~f:(fun mlabel ->
               LabelledMeta.create ~mtype ~mloc ~madlevel ~mlabel () )
      and g {TypedLocatedMeta.madlevel; mtype; mloc} =
        incr_label
        |> State.map ~f:(fun mlabel ->
               LabelledMeta.create ~mtype ~mloc ~madlevel ~mlabel () )
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
      | Mir_pattern.Break | Skip | Continue | Return None -> assocs
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
end
