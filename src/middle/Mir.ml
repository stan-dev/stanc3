open Core_kernel
module Operator = Operator
module Expr = Expr
module UnsizedType = UnsizedType
module SizedType = SizedType

module IO_Block = struct
  type t = Mir_pattern.io_block

  let sexp_of_t = Mir_pattern.sexp_of_io_block
  let t_of_sexp = Mir_pattern.io_block_of_sexp
  let hash_fold_t = Mir_pattern.hash_fold_io_block
  let pp ppf x = Mir_pretty_printer.pp_io_block ppf x
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

    let remove_meta x = Fixed.map (fun _ -> ()) (fun _ -> ()) x
  end

  module Typed = struct
    include Meta.Specialize2 (Fixed) (Expr.Typed) (TypedMeta)

    let type_of x = TypedMeta.type_ @@ Fixed.meta x
    let loc_of x = TypedMeta.loc @@ Fixed.meta x
    let adlevel_of x = TypedMeta.adlevel @@ Fixed.meta x
  end

  module Labelled = struct
    include Meta.Specialize2 (Fixed) (Expr.Labelled) (LabelledMeta)

    let label_of x = LabelledMeta.label @@ Fixed.meta x
    let type_of x = LabelledMeta.type_ @@ Fixed.meta x
    let loc_of x = LabelledMeta.loc @@ Fixed.meta x
    let adlevel_of x = LabelledMeta.adlevel @@ Fixed.meta x

    module Traversable_state = Fixed.Make_traversable2 (State)

    let label ?(init = 0) (stmt : Typed.t) : t =
      let incr_label =
        State.(get >>= fun label -> put (label + 1) >>= fun _ -> return label)
      in
      let f {TypedMeta.madlevel; mtype; mloc} =
        incr_label
        |> State.map ~f:(fun mlabel ->
               LabelledMeta.create ~mtype ~mloc ~madlevel ~mlabel () )
      and g {TypedMeta.madlevel; mtype; mloc} =
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

  (* == Helpers ============================================================= *)

  let assign meta name ?(idxs = []) expr =
    Fixed.fix meta @@ Mir_pattern.Assignment ((name, idxs), expr)

  let target_pe ~meta e = Fixed.fix meta @@ Mir_pattern.TargetPE e
  let break meta = Fixed.fix meta Mir_pattern.Break
  let continue meta = Fixed.fix meta Mir_pattern.Continue
  let skip meta = Fixed.fix meta Mir_pattern.Skip
  let return_ meta e = Fixed.fix meta @@ Mir_pattern.Return (Some e)
  let return_void meta = Fixed.fix meta @@ Mir_pattern.Return None
  let block meta xs = Fixed.fix meta @@ Mir_pattern.Block xs
  let slist meta xs = Fixed.fix meta @@ Mir_pattern.SList xs
  let while_ meta pred body = Fixed.fix meta @@ Mir_pattern.While (pred, body)

  let if_ meta pred ?when_false ~when_true =
    Fixed.fix meta @@ Mir_pattern.IfElse (pred, when_true, when_false)

  let for_ meta loopvar lower upper body =
    Fixed.fix meta @@ Mir_pattern.For {loopvar; lower; upper; body}

  let declare_sized meta adtype name ty =
    Fixed.fix meta
    @@ Mir_pattern.Decl
         {decl_adtype= adtype; decl_id= name; decl_type= Mir_pattern.Sized ty}

  let declare_unsized meta adtype name ty =
    Fixed.fix meta
    @@ Mir_pattern.Decl
         {decl_adtype= adtype; decl_id= name; decl_type= Mir_pattern.Unsized ty}

  let stanlib_fun meta name args =
    Fixed.fix meta @@ Mir_pattern.(NRFunApp (StanLib, name, args))

  let compiler_fun meta name args =
    Fixed.fix meta @@ Mir_pattern.(NRFunApp (CompilerInternal, name, args))

  let user_fun meta name args =
    Fixed.fix meta @@ Mir_pattern.(NRFunApp (UserDefined, name, args))

  let data_only = Mir_pattern.DataOnly
  let auto_diff = Mir_pattern.AutoDiffable
  let uint = Mir_pattern.UInt
  let ureal = Mir_pattern.UReal
  let uvector = Mir_pattern.UVector
  let urowvector = Mir_pattern.URowVector
  let umatrix = Mir_pattern.UMatrix
  let ufun args rty = Mir_pattern.UFun (args, rty)
  let umathlibfun = Mir_pattern.UMathLibraryFunction
  let uarray uty = Mir_pattern.UArray uty
  let sint = Mir_pattern.SInt
  let sreal = Mir_pattern.SReal
  let svector e = Mir_pattern.SVector e
  let srowvector e = Mir_pattern.SRowVector e
  let smatrix erow ecol = Mir_pattern.SMatrix (erow, ecol)
  let sarray sty e = Mir_pattern.SArray (sty, e)

  let rec unsized_of_sized_type = function
    | Mir_pattern.SInt -> Mir_pattern.UInt
    | SReal -> UReal
    | SVector _ -> UVector
    | SRowVector _ -> URowVector
    | SMatrix _ -> UMatrix
    | SArray (t, _) -> UArray (unsized_of_sized_type t)

  let void = Mir_pattern.Void
  let return_ty ty = Mir_pattern.ReturnType ty
end

module Program = struct
  module NoMeta = struct
    type t = (Expr.NoMeta.t, Stmt.NoMeta.t) Mir_pattern.prog

    let sexp_of_t x =
      Mir_pattern.sexp_of_prog Expr.NoMeta.sexp_of_t Stmt.NoMeta.sexp_of_t x

    let t_of_sexp x =
      Mir_pattern.prog_of_sexp Expr.NoMeta.t_of_sexp Stmt.NoMeta.t_of_sexp x
  end

  module Typed = struct
    type t = (Expr.Typed.t, Stmt.Typed.t) Mir_pattern.prog

    let sexp_of_t x =
      Mir_pattern.sexp_of_prog Expr.Typed.sexp_of_t Stmt.Typed.sexp_of_t x

    let t_of_sexp x =
      Mir_pattern.prog_of_sexp Expr.Typed.t_of_sexp Stmt.Typed.t_of_sexp x
  end
end
