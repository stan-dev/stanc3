open Core
open Common

(** Pattern and fixed-point of MIR expressions *)
module Fixed = struct
  module Pattern = struct
    type litType = Int | Real | Imaginary | Str
    [@@deriving sexp, hash, compare]

    type 'a t =
      | Var of string
      | Lit of litType * string
      | FunApp of 'a Fun_kind.t * 'a list
      | TernaryIf of 'a * 'a * 'a
      | EAnd of 'a * 'a
      | EOr of 'a * 'a
      | Indexed of 'a * 'a Index.t list
      | Promotion of 'a * UnsizedType.t * UnsizedType.autodifftype
      | TupleProjection of 'a * int
    [@@deriving sexp, hash, map, compare, fold]

    let pp pp_e ppf = function
      | Var varname -> Fmt.string ppf varname
      | Lit (Str, str) -> Fmt.pf ppf "%S" str
      | Lit (_, str) -> Fmt.string ppf str
      | FunApp (StanLib (name, FnPlain, _), [lhs; rhs])
        when Option.is_some (Operator.of_string_opt name) ->
          Fmt.pf ppf "(%a %a %a)" pp_e lhs Operator.pp
            (Option.value_exn (Operator.of_string_opt name))
            pp_e rhs
      | FunApp (fun_kind, args) ->
          Fmt.pf ppf "%a(@[<hov>%a@])" (Fun_kind.pp pp_e) fun_kind
            Fmt.(list pp_e ~sep:Fmt.comma)
            args
      | TernaryIf (pred, texpr, fexpr) ->
          Fmt.pf ppf "(@[%a@ ?@ %a@ :@ %a@])" pp_e pred pp_e texpr pp_e fexpr
      | Indexed (expr, indices) ->
          Fmt.pf ppf "@[%a%a@]" pp_e expr (Index.pp_indices pp_e) indices
      | TupleProjection (expr, ix) -> Fmt.pf ppf "@[%a.%d@]" pp_e expr ix
      | EAnd (l, r) -> Fmt.pf ppf "%a && %a" pp_e l pp_e r
      | EOr (l, r) -> Fmt.pf ppf "%a || %a" pp_e l pp_e r
      | Promotion (from, ut, ad) ->
          Fmt.pf ppf "promote(@[<hov>%a,@ %a,@ %a@])" pp_e from UnsizedType.pp
            ut UnsizedType.pp_tuple_autodifftype ad
  end

  include Fixed.Make (Pattern)
end

(** Expressions with associated location and type *)
module Typed = struct
  module Meta = struct
    type t =
      { type_: UnsizedType.t
      ; loc: (Location_span.t[@sexp.opaque] [@compare.ignore])
      ; adlevel: UnsizedType.autodifftype }
    [@@deriving compare, create, sexp, hash]

    let empty =
      create ~type_:UnsizedType.UInt ~adlevel:UnsizedType.DataOnly
        ~loc:Location_span.empty ()
  end

  include Specialized.Make (Fixed) (Meta)

  let type_of Fixed.{meta= Meta.{type_; _}; _} = type_
  let loc_of Fixed.{meta= Meta.{loc; _}; _} = loc
  let adlevel_of Fixed.{meta= Meta.{adlevel; _}; _} = adlevel
  let fun_arg Fixed.{meta= Meta.{type_; adlevel; _}; _} = (adlevel, type_)
end

module Helpers = struct
  let int i = {Fixed.meta= Typed.Meta.empty; pattern= Lit (Int, string_of_int i)}

  let float i =
    { Fixed.meta= {Typed.Meta.empty with type_= UReal}
    ; pattern= Lit (Real, Float.to_string i) }

  let complex (r, i) =
    { Fixed.meta= {Typed.Meta.empty with type_= UComplex}
    ; pattern= FunApp (StanLib ("to_complex", FnPlain, AoS), [float r; float i])
    }

  let str i = {Fixed.meta= Typed.Meta.empty; pattern= Lit (Str, i)}
  let variable v = {Fixed.meta= Typed.Meta.empty; pattern= Var v}
  let zero = int 0
  let one = int 1

  let unary_op op e =
    { Fixed.meta= Typed.Meta.empty
    ; pattern= FunApp (StanLib (Operator.to_string op, FnPlain, AoS), [e]) }

  let binop e1 op e2 =
    { Fixed.meta= Typed.Meta.empty
    ; pattern= FunApp (StanLib (Operator.to_string op, FnPlain, AoS), [e1; e2])
    }

  let binop_list es op ~default =
    match es with
    | [] -> default
    | head :: rest ->
        List.fold ~init:head ~f:(fun accum next -> binop accum op next) rest

  let row_vector l =
    { Fixed.meta= {Typed.Meta.empty with type_= URowVector}
    ; pattern= FunApp (CompilerInternal FnMakeRowVec, List.map ~f:float l) }

  let vector l =
    let v = unary_op Transpose (row_vector l) in
    {v with meta= {Typed.Meta.empty with type_= UVector}}

  let matrix l =
    { Fixed.meta= {Typed.Meta.empty with type_= UMatrix}
    ; pattern= FunApp (CompilerInternal FnMakeRowVec, List.map ~f:row_vector l)
    }

  let complex_row_vector l =
    { Fixed.meta= {Typed.Meta.empty with type_= UComplexRowVector}
    ; pattern= FunApp (CompilerInternal FnMakeRowVec, List.map ~f:complex l) }

  let complex_vector l =
    let v = unary_op Transpose (complex_row_vector l) in
    {v with meta= {Typed.Meta.empty with type_= UComplexVector}}

  let complex_matrix_from_rows l =
    { Fixed.meta= {Typed.Meta.empty with type_= UComplexMatrix}
    ; pattern= FunApp (CompilerInternal FnMakeRowVec, l) }

  let matrix_from_rows l =
    { Fixed.meta= {Typed.Meta.empty with type_= UMatrix}
    ; pattern= FunApp (CompilerInternal FnMakeRowVec, l) }

  let array_expr l =
    let type_ =
      List.hd l |> Option.value_map ~f:Typed.type_of ~default:UnsizedType.UReal
    in
    { Fixed.meta= {Typed.Meta.empty with type_= UArray type_}
    ; pattern= FunApp (CompilerInternal FnMakeArray, l) }

  let tuple_expr l =
    let type_ = UnsizedType.UTuple (List.map ~f:Typed.type_of l) in
    { Fixed.meta=
        { Typed.Meta.empty with
          type_
        ; adlevel= TupleAD (List.map ~f:Typed.adlevel_of l) }
    ; pattern= FunApp (CompilerInternal FnMakeTuple, l) }

  let try_unpack e =
    match e.Fixed.pattern with
    | FunApp (CompilerInternal (FnMakeRowVec | FnMakeArray), l) -> Some l
    | FunApp
        ( StanLib ("Transpose__", FnPlain, _)
        , [{pattern= FunApp (CompilerInternal FnMakeRowVec, l); _}] ) ->
        Some l
    | _ -> None

  let loop_bottom = one

  let internal_funapp fn args meta =
    {Fixed.meta; pattern= FunApp (CompilerInternal fn, args)}

  let contains_fn_kind is_fn_kind ?(init = false) e =
    let rec aux accu Fixed.{pattern; _} =
      accu
      ||
      match pattern with
      | FunApp (kind, _) when is_fn_kind kind -> true
      | x -> Fixed.Pattern.fold aux accu x in
    aux init e

  let%test "expr contains fn" =
    internal_funapp FnReadData [] ()
    |> contains_fn_kind (function
         | CompilerInternal FnReadData -> true
         | _ -> false)

  let rec infer_type_of_indexed ut indices =
    match (ut, indices) with
    | _, [] -> ut
    | _, [Index.All] | _, [Upfrom _] | _, [Between _] -> ut
    | UnsizedType.UMatrix, [All; Single _]
     |UMatrix, [Upfrom _; Single _]
     |UMatrix, [Between _; Single _]
     |UMatrix, [MultiIndex _]
     |UMatrix, [Single _] ->
        UVector
    | UComplexMatrix, [All; Single _]
     |UComplexMatrix, [Upfrom _; Single _]
     |UComplexMatrix, [Between _; Single _]
     |UComplexMatrix, [MultiIndex _]
     |UComplexMatrix, [Single _] ->
        UComplexVector
    | UArray t, Single _ :: tl -> infer_type_of_indexed t tl
    | UArray t, _ :: tl -> UArray (infer_type_of_indexed t tl)
    | UMatrix, [Single _; Single _] | UVector, [_] | URowVector, [_] -> UReal
    | UComplexMatrix, [Single _; Single _]
     |UComplexVector, [_]
     |UComplexRowVector, [_] ->
        UComplex
    | _ ->
        ICE.internal_compiler_error
          [%message "Can't index" (ut : UnsizedType.t)]

  (** [add_index expression index] returns an expression that (additionally)
      indexes into the input [expression] by [index].*)
  let add_int_index e i =
    let mtype = infer_type_of_indexed Typed.(type_of e) [i] in
    let meta = Typed.Meta.{e.meta with type_= mtype}
    and pattern =
      match e.pattern with
      | Var _ | TupleProjection _ -> Fixed.Pattern.Indexed (e, [i])
      | Indexed (e, indices) -> Indexed (e, indices @ [i])
      | _ ->
          ICE.internal_compiler_error
            [%message "Expected Var or Indexed but found " (e : Typed.t)] in
    Fixed.{meta; pattern}

  (** [add_tuple_index expression index] returns an expression that (additionally)
      projects into the input [expression] by the tuple index [index].
      This will raise an error at runtime if [expression] does not have a tuple type.
    *)
  let add_tuple_index e i =
    let mtype =
      match Typed.(type_of e) with
      | UTuple ts -> List.nth_exn ts (i - 1)
      | t ->
          ICE.internal_compiler_error
            [%message
              "Internal error: Attempted to apply tuple index to a non-tuple \
               type:"
                (t : UnsizedType.t)] in
    let meta = Typed.Meta.{e.meta with type_= mtype} in
    let pattern = Fixed.Pattern.TupleProjection (e, i) in
    Fixed.{meta; pattern}

  (** TODO: Make me tail recursive *)
  let rec collect_indices Fixed.{pattern; _} =
    match pattern with
    | Indexed (obj, indices) -> collect_indices obj @ indices
    | _ -> []

  let%expect_test "infer type of indexed" =
    [ ( UnsizedType.UArray UMatrix
      , [Index.Single loop_bottom; Single loop_bottom] )
    ; (UArray (UArray UMatrix), [Single loop_bottom])
    ; (UArray UMatrix, [Single loop_bottom])
    ; (UArray UMatrix, [Upfrom loop_bottom; Single loop_bottom])
    ; ( UArray UMatrix
      , [Single loop_bottom; Single loop_bottom; Single loop_bottom] )
    ; ( UArray UMatrix
      , [Upfrom loop_bottom; Single loop_bottom; Single loop_bottom] ) ]
    |> List.map ~f:(fun (ut, idx) -> infer_type_of_indexed ut idx)
    |> Fmt.(str "@[<hov>%a@]" (list ~sep:comma UnsizedType.pp))
    |> print_endline;
    [%expect
      {|
      vector, array[] matrix, matrix, array[] vector, real, array[] real |}]
end
