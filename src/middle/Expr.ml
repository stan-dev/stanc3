open Core_kernel
open Common
open State.Cps

type litType = Mir_pattern.litType = Int | Real | Str
[@@deriving sexp, hash, compare]

type 'a index = 'a Mir_pattern.index =
  | All
  | Single of 'a
  | Upfrom of 'a
  | Between of 'a * 'a
  | MultiIndex of 'a
[@@deriving sexp, hash, map, compare, fold]

let pp_index pp_e ppf x = Mir_pretty_printer.pp_index pp_e ppf x

let pp_indexed pp_e ppf (ident, indices) =
  Fmt.pf ppf {|@[%s%a@]|} ident
    ( if List.is_empty indices then fun _ _ -> ()
    else Fmt.(list (pp_index pp_e) ~sep:comma |> brackets) )
    indices

module Fixed = struct
  module Pattern = struct
    type 'a t = 'a Mir_pattern.expr =
      | Var of string
      | Lit of litType * string
      | FunApp of Fun_kind.t * string * 'a list
      | TernaryIf of 'a * 'a * 'a
      | EAnd of 'a * 'a
      | EOr of 'a * 'a
      | Indexed of 'a * 'a index list
    [@@deriving sexp, hash, map, compare, fold]

    let pp pp_e ppf = Mir_pretty_printer.pp_expr pp_e ppf

    include Foldable.Make (struct type nonrec 'a t = 'a t

                                  let fold = fold
    end)

    module Make_traversable = Mir_pattern.Make_traversable_expr
    module Make_traversable2 = Mir_pattern.Make_traversable_expr2
  end

  (** Fixed-point of `expr` *)
  include Fix.Make (Pattern)
end 

(** Fixed-point expressions specialized to have no meta data *)
module NoMeta = struct
  module Meta = Mir_meta.NoMeta

  include Specialized.Make (Fixed) (Meta)

  let remove_meta x = Fixed.map (fun _ -> ()) x
end

module Typed = struct
  module Meta = Mir_meta.Typed

  include Specialized.Make (Fixed) (Meta)

  let type_of x = Meta.type_ @@ Fixed.meta x
  let loc_of x = Meta.loc @@ Fixed.meta x
  let adlevel_of x = Meta.adlevel @@ Fixed.meta x
end

module Labelled = struct
  module Meta = Mir_meta.Labelled

  include Specialized.Make (Fixed) (Meta)

  let label_of x = Meta.label @@ Fixed.meta x
  let type_of x = Meta.type_ @@ Fixed.meta x
  let loc_of x = Meta.loc @@ Fixed.meta x
  let adlevel_of x = Meta.adlevel @@ Fixed.meta x

  module Traversable_state = Fixed.Make_traversable2 (State)

  let label ?(init = 0) (expr : Typed.t) : t =
    let f {Typed.Meta.adlevel; type_; loc} =
      State.(
        get
        >>= fun label ->
        put (label + 1)
        >>= fun _ -> return @@ Meta.create ~label ~adlevel ~type_ ~loc ())
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

let var meta name = Fixed.fix meta @@ Var name
let lit meta lit_type str_value = Fixed.fix meta @@ Lit(lit_type,str_value)
let lit_int meta value = lit meta Int @@ string_of_int value
  
let lit_real meta value = lit meta Real @@ string_of_float value
let lit_string meta value = lit meta Str value

let if_ meta pred true_expr false_expr =
  Fixed.fix meta @@ TernaryIf (pred, true_expr, false_expr)

let and_ meta e1 e2 = Fixed.fix meta @@ EAnd (e1, e2)
let or_ meta e1 e2 = Fixed.fix meta @@ EOr (e1, e2)
let index_all = All
let index_single e = Single e
let index_multi e = MultiIndex e
let index_upfrom e = Upfrom e
let index_between e1 e2 = Between (e1, e2)
let indexed meta e idxs = Fixed.fix meta @@ Indexed (e, idxs)

let index_bounds = function
  | All -> []
  | Single e | MultiIndex e | Upfrom e -> [e]
  | Between (e1, e2) -> [e1; e2]

let fun_app meta fun_kind name args = 
  Fixed.fix meta @@ FunApp (fun_kind, name, args)

let compiler_fun meta name args =
  Fixed.fix meta @@ FunApp (CompilerInternal, name, args)

let user_fun meta name args =
  Fixed.fix meta @@ FunApp (UserDefined, name, args)

let stanlib_fun meta name args =
  Fixed.fix meta @@ FunApp (StanLib, name, args)

let binop meta  op  a b = stanlib_fun meta (Operator.to_string op) [a;b]

 