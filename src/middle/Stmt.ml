open Core_kernel
open Common
open Helpers

(** Fixed-point of statements *)
module Fixed = struct
  module First = Expr.Fixed

  module Pattern = struct
    type ('a, 'b) t =
      | Assignment of (string * 'a Index.t list) * 'a
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

    let pp pp_e pp_s ppf = function
      | Assignment ((assignee, idcs), rhs) ->
          Fmt.pf ppf {|@[<h>%a =@ %a;@]|} (Index.pp_indexed pp_e)
            (assignee, idcs) pp_e rhs
      | TargetPE expr ->
          Fmt.pf ppf {|@[<h>%a +=@ %a;@]|} pp_keyword "target" pp_e expr
      | NRFunApp (_, name, args) ->
          Fmt.pf ppf {|@[%s%a;@]|} name
            Fmt.(list pp_e ~sep:comma |> parens)
            args
      | Break -> pp_keyword ppf "break;"
      | Continue -> pp_keyword ppf "continue;"
      | Skip -> pp_keyword ppf ";"
      | Return (Some expr) ->
          Fmt.pf ppf {|%a %a;|} pp_keyword "return" pp_e expr
      | Return _ -> pp_keyword ppf "return;"
      | IfElse (pred, s_true, Some s_false) ->
          Fmt.pf ppf {|%a(%a) %a %a %a|} pp_builtin_syntax "if" pp_e pred pp_s
            s_true pp_builtin_syntax "else" pp_s s_false
      | IfElse (pred, s_true, _) ->
          Fmt.pf ppf {|%a(%a) %a|} pp_builtin_syntax "if" pp_e pred pp_s s_true
      | While (pred, stmt) ->
          Fmt.pf ppf {|%a(%a) %a|} pp_builtin_syntax "while" pp_e pred pp_s
            stmt
      | For {loopvar; lower; upper; body} ->
          Fmt.pf ppf {|%a(%s in %a:%a) %a|} pp_builtin_syntax "for" loopvar
            pp_e lower pp_e upper pp_s body
      | Block stmts ->
          Fmt.pf ppf {|{@;<1 2>@[<v>%a@]@;}|}
            Fmt.(list pp_s ~sep:Fmt.cut)
            stmts
      | SList stmts -> Fmt.(list pp_s ~sep:Fmt.cut |> vbox) ppf stmts
      | Decl {decl_adtype; decl_id; decl_type} ->
          Fmt.pf ppf {|%a%a %s;|} UnsizedType.pp_autodifftype decl_adtype
            (Type.pp pp_e) decl_type decl_id

    include Foldable.Make2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let fold = fold
    end)
  end

  include Fixed.Make2 (First) (Pattern)
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
      ; label: Label.Int_label.t [@compare.ignore] }
    [@@deriving compare, create, sexp, hash]

    let label {label; _} = label
    let loc {loc; _} = loc
    let pp _ _ = ()
  end

  include Specialized.Make2 (Fixed) (Expr.Labelled) (Meta)

  let label_of x = Meta.label @@ Fixed.meta x
  let loc_of x = Meta.loc @@ Fixed.meta x

  let label ?(init = Label.Int_label.init) (stmt : Located.t) : t =
    let lbl = ref init in
    let f Expr.Typed.Meta.({adlevel; type_; loc}) =
      let cur_lbl = !lbl in
      lbl := Label.Int_label.next cur_lbl ;
      Expr.Labelled.Meta.create ~type_ ~loc ~adlevel ~label:cur_lbl ()
    and g loc =
      let cur_lbl = !lbl in
      lbl := Label.Int_label.next cur_lbl ;
      Meta.create ~loc ~label:cur_lbl ()
    in
    Fixed.map f g stmt

  type associations =
    { exprs: Expr.Labelled.t Label.Int_label.Map.t
    ; stmts: t Label.Int_label.Map.t }

  let empty =
    {exprs= Label.Int_label.Map.empty; stmts= Label.Int_label.Map.empty}

  let rec associate ?init:(assocs = empty) (stmt : t) =
    associate_pattern
      { assocs with
        stmts=
          Label.Int_label.Map.add_exn assocs.stmts ~key:(label_of stmt)
            ~data:stmt }
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
    | Type.Sized st ->
        {assocs with exprs= SizedType.associate ~init:assocs.exprs st}
    | Unsized _ -> assocs
end


module Helpers = struct
  let contains_fn fn ?init:(init = false) stmt =
    let fstr = Internal_fun.to_string fn in 

    let rec aux accu stmt = 
      match Fixed.pattern stmt with 
      | NRFunApp(_,fname,_) when fname = fstr -> true
      | stmt_pattern -> 
        Fixed.Pattern.fold_left ~init:accu stmt_pattern
          ~f:(fun accu expr -> Expr.Helpers.contains_fn fn ~init:accu expr ) 
          ~g:aux 
          
    in 
    aux init stmt
  
(* 
  let mock_stmt stmt = {stmt; smeta= no_span}
let mir_int i = {expr= Lit (Int, string_of_int i); emeta= internal_meta}

let mock_for i body =
  For
    { loopvar= "lv"
    ; lower= mir_int 0
    ; upper= mir_int i
    ; body= mock_stmt (Block [body]) }
  |> mock_stmt

let%test "contains fn" =
  let f =
    mock_for 8
      (mock_for 9
         (mock_stmt
            (Assignment
               (("v", UInt, []), internal_funapp FnReadData [] internal_meta))))
  in
  contains_fn
    (string_of_internal_fn FnReadData)
    (mock_stmt (Block [f; mock_stmt Break]))

let%test "contains nrfn" =
  let f =
    mock_for 8
      (mock_for 9
         (mock_stmt
            (NRFunApp (CompilerInternal, string_of_internal_fn FnWriteParam, []))))
  in
  contains_fn
    (string_of_internal_fn FnWriteParam)
    (mock_stmt
       (Block
          [ mock_stmt
              (NRFunApp
                 (CompilerInternal, string_of_internal_fn FnWriteParam, []))
          ; f ])) *)
end