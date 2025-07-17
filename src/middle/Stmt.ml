open Core
open Common

(** Fixed-point of statements *)
module Fixed = struct
  module Pattern = struct
    type ('a, 'b) t =
      | Assignment of 'a lvalue * UnsizedType.t * 'a
      | TargetPE of 'a
      | JacobianPE of 'a
      | NRFunApp of 'a Fun_kind.t * 'a list
      | Break
      | Continue
      | Return of 'a option
      | Skip
      | IfElse of 'a * 'b * 'b option
      | While of 'a * 'b
      | For of {loopvar: string; lower: 'a; upper: 'a; body: 'b}
      | Profile of string * 'b list
      | Block of 'b list
      | SList of 'b list
      | Decl of
          { decl_adtype: UnsizedType.autodifftype
          ; decl_id: string
          ; decl_type: 'a Type.t
          ; initialize: 'a decl_init }
    [@@deriving sexp, hash, map, fold, compare]

    and 'e lvalue = 'e lbase * 'e Index.t list
    [@@deriving sexp, hash, map, compare, fold]

    and 'e lbase = LVariable of string | LTupleProjection of 'e lvalue * int
    [@@deriving sexp, hash, map, compare, fold]

    and 'a decl_init = Uninit | Default | Assign of 'a
    [@@deriving sexp, hash, map, fold, compare]

    let rec pp_lvalue pp_e ppf (lbase, idcs) =
      match lbase with
      | LVariable v -> Fmt.pf ppf "%s%a" v (Index.pp_indices pp_e) idcs
      | LTupleProjection (lv, ix) ->
          Fmt.pf ppf "%a.%d%a" (pp_lvalue pp_e) lv ix (Index.pp_indices pp_e)
            idcs

    let pp pp_e pp_s ppf = function
      | Assignment (lvalue, _, rhs) ->
          Fmt.pf ppf "@[<hov>%a =@[<h>@ %a@];@]" (pp_lvalue pp_e) lvalue pp_e
            rhs
      | TargetPE expr -> Fmt.pf ppf "@[<h>target +=@ %a;@]" pp_e expr
      | JacobianPE expr -> Fmt.pf ppf "@[<h>jacobian +=@ %a;@]" pp_e expr
      | NRFunApp (kind, args) ->
          Fmt.pf ppf "@[%a%a;@]" (Fun_kind.pp pp_e) kind
            Fmt.(list pp_e ~sep:comma |> parens)
            args
      | Break -> Fmt.string ppf "break;"
      | Continue -> Fmt.string ppf "continue;"
      | Skip -> Fmt.string ppf ";"
      | Return (Some expr) -> Fmt.pf ppf "return %a;" pp_e expr
      | Return _ -> Fmt.string ppf "return;"
      | IfElse (pred, s_true, Some s_false) ->
          Fmt.pf ppf "if(%a) %a else %a" pp_e pred pp_s s_true pp_s s_false
      | IfElse (pred, s_true, _) -> Fmt.pf ppf "if(%a) %a" pp_e pred pp_s s_true
      | While (pred, stmt) -> Fmt.pf ppf "while(%a) %a" pp_e pred pp_s stmt
      | For {loopvar; lower; upper; body} ->
          Fmt.pf ppf "for(%s in %a:%a) %a" loopvar pp_e lower pp_e upper pp_s
            body
      | Profile (name, stmts) ->
          Fmt.pf ppf "profile(%s){@;<1 2>@[<v>%a@]@;}" name
            Fmt.(list pp_s ~sep:cut)
            stmts
      | Block stmts ->
          Fmt.pf ppf "{@;<1 2>@[<v>%a@]@;}" Fmt.(list pp_s ~sep:cut) stmts
      | SList stmts -> Fmt.(list pp_s ~sep:cut |> vbox) ppf stmts
      | Decl {decl_adtype; decl_id; decl_type; initialize} -> (
          match initialize with
          | Assign e ->
              Fmt.pf ppf "@[<hov 2>%a%a@ %s = %a;@]" UnsizedType.pp_autodifftype
                decl_adtype (Type.pp pp_e) decl_type decl_id pp_e e
          | Uninit | Default ->
              Fmt.pf ppf "@[<hov 2>%a%a@ %s;@]" UnsizedType.pp_autodifftype
                decl_adtype (Type.pp pp_e) decl_type decl_id)
  end

  type ('a, 'b) t = {pattern: ('a Expr.Fixed.t, ('a, 'b) t) Pattern.t; meta: 'b}
  [@@deriving compare, hash, sexp]

  let rec pp ppf {pattern; meta= _} = Pattern.pp Expr.Fixed.pp pp ppf pattern

  let rec rewrite_bottom_up ~f ~g t =
    g
      { t with
        pattern=
          Pattern.map
            (Expr.Fixed.rewrite_bottom_up ~f)
            (rewrite_bottom_up ~f ~g) t.pattern }
end

(** Statements with location information and types for contained expressions *)
module Located = struct
  module Meta = struct
    type t = (Location_span.t[@sexp.opaque] [@compare.ignore])
    [@@deriving compare, sexp, hash]

    let empty = Location_span.empty
  end

  type t = (Expr.Typed.Meta.t, (Meta.t[@sexp.opaque] [@compare.ignore])) Fixed.t
  [@@deriving compare, sexp, hash]

  let loc_of Fixed.{meta; _} = meta
  let pp = Fixed.pp

  (** This module acts as a temporary replace for the [stmt_loc_num] type that
  is currently used within [analysis_and_optimization].

  The original intent of the type was to provide explicit sharing of subterms.
  My feeling is that ultimately we want to use the recursive type directly and rely on OCaml for sharing
 *)
  module Non_recursive = struct
    type t =
      { pattern: (Expr.Typed.t, int) Fixed.Pattern.t
      ; meta: (Meta.t[@sexp.opaque] [@compare.ignore]) }
    [@@deriving compare, sexp, hash]
  end
end

module Numbered = struct
  module Meta = struct
    type t = (int[@sexp.opaque] [@compare.ignore])
    [@@deriving compare, sexp, hash]

    let empty = 0
    let from_int (i : int) : t = i
  end

  type t = (Expr.Typed.Meta.t, (Meta.t[@sexp.opaque] [@compare.ignore])) Fixed.t
  [@@deriving compare, sexp, hash]

  let pp = Fixed.pp
end

module Helpers = struct
  let temp_vars exprs : Located.t list * Expr.Typed.t list * (unit -> unit) =
    let sym, reset = Gensym.enter () in
    let rec loop es sym inits vars =
      match es with
      | [] -> (inits, vars)
      | (Expr.{Fixed.pattern= Var _; _} as e) :: es ->
          loop es sym inits (e :: vars)
      | e :: es ->
          let decl =
            { Fixed.pattern=
                Decl
                  { decl_adtype= Expr.Typed.adlevel_of e
                  ; decl_id= sym
                  ; decl_type= Unsized (Expr.Typed.type_of e)
                  ; initialize= Default }
            ; meta= e.meta.loc } in
          let assign =
            { decl with
              Fixed.pattern=
                Assignment ((LVariable sym, []), Expr.Typed.type_of e, e) }
          in
          loop es (Gensym.generate ()) (decl :: assign :: inits)
            ({e with pattern= Var sym} :: vars) in
    let setups, exprs = loop (List.rev exprs) sym [] [] in
    (setups, exprs, reset)

  let ensure_var bodyfn (expr : Expr.Typed.t) meta =
    match expr with
    | {pattern= Var _; _} -> bodyfn expr meta
    | _ ->
        let preamble, temp, reset = temp_vars [expr] in
        let body = bodyfn (List.hd_exn temp) meta in
        reset ();
        {body with Fixed.pattern= Block (preamble @ [body])}

  let internal_nrfunapp fn args meta =
    {Fixed.pattern= NRFunApp (CompilerInternal fn, args); meta}

  (** [mk_for] returns a MIR For statement from 0 to [upper] that calls the [bodyfn] with the loop
      variable inside the loop. *)
  let mk_for upper bodyfn meta =
    let loopvar, reset = Gensym.enter () in
    let loopvar_expr =
      Expr.Fixed.
        { meta= Expr.Typed.Meta.create ~type_:UInt ~loc:meta ~adlevel:DataOnly ()
        ; pattern= Var loopvar } in
    let lower = Expr.Helpers.loop_bottom in
    let body = Fixed.{meta; pattern= Pattern.Block [bodyfn loopvar_expr]} in
    reset ();
    let pattern = Fixed.Pattern.For {loopvar; lower; upper; body} in
    Fixed.{meta; pattern}

  (** [mk_nested_for] returns nested MIR For statements with ranges from 0 to each element of
      [uppers], and calls the [bodyfn] in the innermost loop with the list of loop variables. *)
  let rec mk_nested_for uppers bodyfn meta =
    match uppers with
    | [] -> bodyfn []
    | upper :: uppers' ->
        mk_for upper
          (fun loopvar ->
            mk_nested_for uppers'
              (fun loopvars -> bodyfn (loopvar :: loopvars))
              Location_span.empty)
          meta

  (** [mk_for_iteratee] returns a MIR For statement that iterates over the given expression
    [iteratee]. *)
  let mk_for_iteratee upper iteratee_bodyfn iteratee meta =
    let bodyfn loopvar =
      iteratee_bodyfn
        (Expr.Helpers.add_int_index iteratee (Index.Single loopvar)) in
    mk_for upper bodyfn meta

  let rec for_each bodyfn iteratee smeta =
    let len (e : Expr.Typed.t) =
      let emeta = e.meta in
      let emeta' = {emeta with Expr.Typed.Meta.type_= UInt} in
      Expr.Helpers.internal_funapp FnLength [e] emeta' in
    match Expr.Typed.type_of iteratee with
    | UInt | UReal | UComplex -> bodyfn iteratee
    | UVector | URowVector | UComplexVector | UComplexRowVector ->
        mk_for_iteratee (len iteratee) bodyfn iteratee smeta
    | UMatrix | UComplexMatrix ->
        let emeta = iteratee.meta in
        let emeta' = {emeta with Expr.Typed.Meta.type_= UInt} in
        let rows =
          Expr.Fixed.
            { meta= emeta'
            ; pattern= FunApp (StanLib ("rows", FnPlain, AoS), [iteratee]) }
        in
        mk_for_iteratee rows (fun e -> for_each bodyfn e smeta) iteratee smeta
    | UArray _ -> mk_for_iteratee (len iteratee) bodyfn iteratee smeta
    | UMathLibraryFunction | UFun _ | UTuple _ ->
        ICE.internal_compiler_error
          [%message "Can't iterate over " (iteratee : Expr.Typed.t)]

  let contains_fn_kind is_fn_kind ?(init = false) stmt =
    let rec aux accu Fixed.{pattern; _} =
      match pattern with
      | NRFunApp (kind, _) when is_fn_kind kind -> true
      | stmt_pattern ->
          Fixed.Pattern.fold
            (fun accu expr ->
              Expr.Helpers.contains_fn_kind is_fn_kind ~init:accu expr)
            aux accu stmt_pattern in
    aux init stmt

  (** [for_scalar unsizedtype...] generates a For statement that loops
    over the scalars in the underlying [unsizedtype].

    We can call [bodyfn] directly on scalars, make a direct For loop
    around Eigen types, or for Arrays we call mk_for_iteratee but inserting a
    recursive call into the [bodyfn] that will operate on the nested
    type. In this way we recursively create for loops that loop over
    the outermost layers first.
*)
  let rec for_scalar st bodyfn var smeta =
    match st with
    | SizedType.SInt | SReal | SComplex -> bodyfn st var
    | SVector (_, d)
     |SRowVector (_, d)
     |SComplexVector d
     |SComplexRowVector d ->
        mk_for_iteratee d (bodyfn st) var smeta
    | SMatrix (mem_pattern, d1, d2) ->
        mk_for_iteratee d1
          (fun e -> for_scalar (SRowVector (mem_pattern, d2)) bodyfn e smeta)
          var smeta
    | SComplexMatrix (d1, d2) ->
        mk_for_iteratee d1
          (fun e -> for_scalar (SComplexRowVector d2) bodyfn e smeta)
          var smeta
    | SArray (t, d) ->
        mk_for_iteratee d (fun e -> for_scalar t bodyfn e smeta) var smeta
    | STuple _ -> bodyfn st var

  (** Exactly like for_scalar, but iterating through array dimensions in the
  inverted order.*)
  let for_scalar_inv st bodyfn (var : Expr.Typed.t) smeta =
    let var = {var with pattern= Indexed (var, [])} in
    let invert_index_order (Expr.Fixed.{pattern; _} as e) =
      match pattern with
      | Indexed (obj, []) -> obj
      | Indexed (obj, idxs) -> {e with pattern= Indexed (obj, List.rev idxs)}
      | _ -> e in
    let rec go st bodyfn var smeta =
      match st with
      | SizedType.SArray (t, d) ->
          let bodyfn' _ var = mk_for_iteratee d (bodyfn st) var smeta in
          go t bodyfn' var smeta
      | SMatrix (mem_pattern, d1, d2) ->
          let bodyfn' _ var = mk_for_iteratee d1 (bodyfn st) var smeta in
          go (SRowVector (mem_pattern, d2)) bodyfn' var smeta
      | SComplexMatrix (d1, d2) ->
          let bodyfn' _ var = mk_for_iteratee d1 (bodyfn st) var smeta in
          go (SComplexRowVector d2) bodyfn' var smeta
      | _ -> for_scalar st bodyfn var smeta in
    go st (fun st var -> bodyfn st (invert_index_order var)) var smeta

  let assign_indexed decl_type (lval, idxs) meta varfn var =
    let indices = Expr.Helpers.collect_indices var in
    Fixed.
      {meta; pattern= Assignment ((lval, idxs @ indices), decl_type, varfn var)}

  let lvariable v = (Fixed.Pattern.LVariable v, [])

  let rec get_lhs_name (lval : 'a Fixed.Pattern.lvalue) =
    match lval with
    | LVariable name, _ -> name
    | LTupleProjection (sub_lval, num), _ ->
        get_lhs_name sub_lval ^ "." ^ string_of_int num

  (* Copied from AST's version in AST.ml *)
  let rec lvalue_of_expr_opt (expr : 'e Expr.Fixed.t) :
      'e Expr.Fixed.t Fixed.Pattern.lvalue option =
    let open Common.Let_syntax.Option in
    let lbase_of_expr_opt (expr : 'e Expr.Fixed.t) =
      match expr.pattern with
      | Var s -> Some (Fixed.Pattern.LVariable s)
      | TupleProjection (l, i) ->
          let+ lv = lvalue_of_expr_opt l in
          Fixed.Pattern.LTupleProjection (lv, i)
      | _ -> None in
    match expr.pattern with
    | Var s -> Some (lvariable s)
    | Indexed (l, i) ->
        let+ lv = lbase_of_expr_opt l in
        (lv, i)
    | TupleProjection (l, i) ->
        let+ lv = lvalue_of_expr_opt l in
        (Fixed.Pattern.LTupleProjection (lv, i), [])
    | _ -> None

  let rec expr_of_lvalue (lhs : 'e Expr.Fixed.t Fixed.Pattern.lvalue)
      ~(meta : 'e) : 'e Expr.Fixed.t =
    let expr_of_lbase = function
      | Fixed.Pattern.LVariable v -> Expr.Fixed.Pattern.Var v
      | LTupleProjection (lv, ix) ->
          TupleProjection (expr_of_lvalue ~meta lv, ix) in
    let pattern =
      match lhs with
      | lv, [] -> expr_of_lbase lv
      | lv, ix ->
          Expr.Fixed.Pattern.Indexed ({pattern= expr_of_lbase lv; meta}, ix)
    in
    {pattern; meta}

  let rec map_lhs_variable ~(f : string -> string)
      (lhs : 'e Fixed.Pattern.lvalue) : 'e Fixed.Pattern.lvalue =
    match lhs with
    | LVariable v, idxs -> (LVariable (f v), idxs)
    | LTupleProjection (lv, ix), idxs ->
        (LTupleProjection (map_lhs_variable ~f lv, ix), idxs)

  let lhs_indices ((_, idxs) : 'e Fixed.Pattern.lvalue) : 'e Index.t list = idxs

  let rec lhs_variable (lhs : 'e Fixed.Pattern.lvalue) : string =
    match lhs with
    | LVariable v, _ -> v
    | LTupleProjection (lv, _), _ -> lhs_variable lv

  (* Reduce an lvalue down to its "base reference", which is a variable with maximum tuple indices after it.
     For example:
     x[1,2][3] -> x
     x.1[1,2].2[3].3 -> x.1
     x.1.2[1,2][3].3 -> x.1.2
  *)
  let lvalue_base_reference (lvalue : 'e Fixed.Pattern.lvalue) =
    let get_tuple_idxs lv =
      let rec go lv acc =
        match lv with
        | Fixed.Pattern.LVariable _, _ -> acc
        | LTupleProjection (lv, ix), _ -> go lv (ix :: acc) in
      go lv [] in
    let rec build_base_reference lv acc =
      match acc with
      | [] -> lv
      | ix :: idxs ->
          build_base_reference
            (Fixed.Pattern.LTupleProjection (lv, ix), [])
            idxs in
    let idxs = get_tuple_idxs lvalue in
    let base = lvariable (lhs_variable lvalue) in
    build_base_reference base idxs

  let%expect_test "lvalue base reference" =
    let lvals =
      [ ( Fixed.Pattern.LVariable "x"
        , [Index.Single 1; Index.Single 2; Index.Single 3] )
      ; (Fixed.Pattern.LTupleProjection (lvariable "x", 1), [])
      ; (LTupleProjection (lvariable "x", 2), [Index.Single 3])
      ; ( LTupleProjection
            ((LTupleProjection (lvariable "x", 3), [Index.Single 4]), 5)
        , [] ) ] in
    let pp = Fmt.(list ~sep:comma (Fixed.Pattern.pp_lvalue int)) in
    Fmt.(str "Before: @[<hov>%a@]" pp) lvals |> print_endline;
    List.map ~f:lvalue_base_reference lvals
    |> Fmt.(str "After: @[<hov>%a@]" pp)
    |> print_endline;
    [%expect
      {|
      Before: x[1, 2, 3], x.1, x.2[3], x.3[4].5
      After: x, x.1, x.2, x.3.5 |}]
end
