open Common

type 'a possiblysizedtype = 'a Mir_pattern.possiblysizedtype =
  | Sized of 'a SizedType.t | Unsized of UnsizedType.t
[@@deriving sexp, compare, map, hash, fold]

module Fixed : sig 
    module Pattern : Pattern.S2 with type ('a,'b) t = ('a,'b) Mir_pattern.statement    
    include Fix.S2 with module First = Expr.Fixed and module Pattern := Pattern
end 

module NoMeta : sig 
    module Meta : Mir_meta_intf.NoMeta
    include Specialized.S 
        with module Meta := Meta
        and  type t = (Expr.NoMeta.Meta.t, Meta.t) Fixed.t 
    val remove_meta : ('a,'b) Fixed.t -> t
end 

module Typed : sig 
    module Meta : Mir_meta_intf.Typed
    include Specialized.S 
        with module Meta := Meta
        and type t = (Expr.Typed.Meta.t, Meta.t) Fixed.t 
    val type_of : t -> UnsizedType.t 
    val loc_of : t -> Location_span.t
    val adlevel_of : t -> UnsizedType.autodifftype
end


module Labelled : sig 
    module Meta : Mir_meta_intf.Labelled
    include Specialized.S 
        with module Meta := Meta
        and type t = (Expr.Labelled.Meta.t, Meta.t) Fixed.t 
    val type_of : t -> UnsizedType.t 
    val loc_of : t -> Location_span.t
    val adlevel_of : t -> UnsizedType.autodifftype
    val label_of : t -> Label.t
    val label : ?init:int -> Typed.t -> t
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
val if_ : 'b -> 'a Expr.Fixed.t -> ?when_false:('a,'b) Fixed.t -> when_true:('a,'b) Fixed.t -> ('a,'b) Fixed.t
val for_ : 'b -> string -> 'a Expr.Fixed.t -> 'a Expr.Fixed.t -> ('a,'b) Fixed.t -> ('a,'b) Fixed.t
val sized : 'a SizedType.t -> 'a possiblysizedtype
val unsized : UnsizedType.t -> 'a possiblysizedtype
val declare_sized : 'b  -> UnsizedType.autodifftype -> string -> 'a Expr.Fixed.t SizedType.t -> ('a,'b) Fixed.t
val declare_unsized : 'b -> UnsizedType.autodifftype -> string  -> UnsizedType.t -> ('a,'b) Fixed.t
val stanlib_fun : 'b -> string -> 'a Expr.Fixed.t list -> ('a,'b) Fixed.t
val compiler_fun : 'b -> string -> 'a Expr.Fixed.t list -> ('a,'b) Fixed.t
val user_fun : 'b -> string -> 'a Expr.Fixed.t list -> ('a,'b) Fixed.t