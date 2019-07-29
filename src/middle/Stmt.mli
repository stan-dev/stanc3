open Core_kernel
open Common

type 'a possiblysizedtype = 'a Mir_pattern.possiblysizedtype =
  | Sized of 'a SizedType.t | Unsized of UnsizedType.t
[@@deriving sexp, compare, map, hash, fold]

module Fixed : sig 
    module Pattern : sig 
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
      [@@deriving sexp, hash, compare]
      include Pattern.S2 with type ('a,'b) t := ('a,'b) t
    end 
    include Fix.S2 with module First = Expr.Fixed and module Pattern := Pattern
end 

module NoMeta : sig     
    module Meta : sig
        type t = 
        unit 
        [@@deriving compare, sexp, hash]
        include Meta.S with type t := t
    end
    include Specialized.S 
        with module Meta := Meta
        and  type t = (Expr.NoMeta.Meta.t, Meta.t) Fixed.t 
    val remove_meta : ('a,'b) Fixed.t -> t
end 

module Located : sig 
    module Meta : sig 
        type t = Location_span.t sexp_opaque [@compare.ignore] [@@deriving compare, sexp, hash]
        include Meta.S with type t := t
    end 

    include Specialized.S 
        with module Meta := Meta
        and type t = (Expr.Typed.Meta.t, Meta.t) Fixed.t 
    
    val loc_of : t -> Location_span.t    
end


module Labelled : sig 
    module Meta :sig 
        type t =
        { loc: Location_span.t sexp_opaque [@compare.ignore]      
        ; label: Label.t [@compare.ignore] }
        [@@deriving compare, create, sexp, hash]
        include Meta.S with type t := t 
        val label : t -> Label.t 
        val loc : t -> Location_span.t
    end

    include Specialized.S 
        with module Meta := Meta
        and type t = (Expr.Labelled.Meta.t, Meta.t) Fixed.t 
    
    val loc_of : t -> Location_span.t    
    val label_of : t -> Label.t
    val label : ?init:int -> Located.t -> t
    type associations = 
        { exprs : Expr.Labelled.t Label.Map.t
        ; stmts : t Label.Map.t 
        }
    val associate : ?init:associations -> t -> associations
end



val assign : 'b -> string -> ?idxs: 'a Expr.Fixed.t Expr.index list -> 'a Expr.Fixed.t -> ('a,'b) Fixed.t
val target_pe : 'b -> 'a Expr.Fixed.t -> ('a,'b) Fixed.t
val break : 'b -> ('a,'b) Fixed.t 
val continue : 'b -> ('a,'b) Fixed.t
val skip : 'b -> ('a,'b) Fixed.t
val return_ : 'b -> 'a Expr.Fixed.t -> ('a,'b) Fixed.t
val return_void : 'b -> ('a,'b) Fixed.t
val block : 'b -> ('a,'b) Fixed.t list -> ('a,'b) Fixed.t
val slist : 'b -> ('a,'b) Fixed.t list -> ('a,'b) Fixed.t
val while_ : 'b -> 'a Expr.Fixed.t -> ('a,'b) Fixed.t -> ('a,'b) Fixed.t
val if_ : 'b -> 'a Expr.Fixed.t -> ('a,'b) Fixed.t -> ('a,'b) Fixed.t option -> ('a,'b) Fixed.t
val for_ : 'b -> string -> 'a Expr.Fixed.t -> 'a Expr.Fixed.t -> ('a,'b) Fixed.t -> ('a,'b) Fixed.t
val mock_for : int -> Located.t -> Located.t
val sized : 'a SizedType.t -> 'a possiblysizedtype
val unsized : UnsizedType.t -> 'a possiblysizedtype
val declare_sized : 'b  -> UnsizedType.autodifftype -> string -> 'a Expr.Fixed.t SizedType.t -> ('a,'b) Fixed.t
val declare_unsized : 'b -> UnsizedType.autodifftype -> string  -> UnsizedType.t -> ('a,'b) Fixed.t
val fun_app : 'b -> Fun_kind.t -> string -> 'a Expr.Fixed.t list -> ('a,'b) Fixed.t
val stanlib_fun : 'b -> string -> 'a Expr.Fixed.t list -> ('a,'b) Fixed.t
val compiler_fun : 'b -> string -> 'a Expr.Fixed.t list -> ('a,'b) Fixed.t
val internal_fun : 'b -> Internal_fun.t -> 'a Expr.Fixed.t list -> ('a,'b) Fixed.t
val user_fun : 'b -> string -> 'a Expr.Fixed.t list -> ('a,'b) Fixed.t
val lift_to_block : ('a,'b) Fixed.t -> ('a,'b) Fixed.t
val block_statements : ('a,'b) Fixed.t -> ('a,'b) Fixed.t list
val is_block : ('a,'b) Fixed.t -> bool
val is_decl : ('a,'b) Fixed.t -> bool
val is_fun : ?name:string -> ('a,'b) Fixed.t -> bool
val contains_fun : ?name:string -> ('a,'b) Fixed.t -> bool  