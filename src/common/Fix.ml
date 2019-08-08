open Core_kernel

module type S = sig
  module Pattern : Pattern.S

  type 'a t [@@deriving compare, map, fold, hash, sexp]

  include Foldable.S with type 'a t := 'a t
  include Pretty.S1 with type 'a t := 'a t
  include Recursive.S1 with type 'a t := 'a t and module Pattern := Pattern
  include Projectable.S1 with type 'a t := 'a t and module Pattern := Pattern
  include Corecursive.S1 with type 'a t := 'a t and module Pattern := Pattern
  include Injectable.S1 with type 'a t := 'a t and module Pattern := Pattern

  (* TODO : derive *)
  module Make_traversable (A : Applicative.S) :
    Traversable.S with module A := A and type 'a t := 'a t

  module Make_traversable2 (A : Applicative.S2) :
    Traversable.S2 with module A := A and type 'a t := 'a t
end

module type S2 = sig
  module First : S
  module Pattern : Pattern.S2

  type ('a, 'b) t [@@deriving compare, map, fold, hash, sexp]

  include Bifoldable.S with type ('a, 'b) t := ('a, 'b) t
  include Pretty.S2 with type ('a, 'b) t := ('a, 'b) t

  include
    Recursive.S2
    with type ('a, 'b) t := ('a, 'b) t
     and module First := First
     and module Pattern := Pattern

  include
    Projectable.S2
    with type ('a, 'b) t := ('a, 'b) t
     and module First := First
     and module Pattern := Pattern

  include
    Corecursive.S2
    with type ('a, 'b) t := ('a, 'b) t
     and module First := First
     and module Pattern := Pattern

  include
    Injectable.S2
    with type ('a, 'b) t := ('a, 'b) t
     and module First := First
     and module Pattern := Pattern

  (* TODO : derive *)
  module Make_traversable (A : Applicative.S) :
    Bitraversable.S with module A := A and type ('a, 'b) t := ('a, 'b) t

  module Make_traversable2 (A : Applicative.S2) :
    Bitraversable.S2 with module A := A and type ('a, 'b) t := ('a, 'b) t
end

module Make (Pattern : Pattern.S) : S with module Pattern := Pattern = struct
  type 'a t = {pattern: 'a t Pattern.t; meta: 'a}
  [@@deriving compare, hash, sexp]

  let rec pp f ppf {pattern; meta} =
    Fmt.pf ppf {|%a%a|} f meta (Pattern.pp (pp f)) pattern

  let rec map f {pattern; meta} =
    {pattern= Pattern.map (map f) pattern; meta= f meta}

  let rec fold f init {pattern; meta} =
    Pattern.fold_left ~f:(fold f) ~init:(f init meta) pattern

  let proj {meta; pattern} = (meta, pattern)
  let inj (meta, pattern) = {meta; pattern}

  include Foldable.Make (struct type nonrec 'a t = 'a t

                                let fold = fold
  end)

  module Basic = struct
    module Pattern = Pattern

    type nonrec 'a t = 'a t

    let inj = inj
    let proj = proj
  end

  include Recursive.Make1 (Basic)
  include Projectable.Make1 (Basic)
  include Corecursive.Make1 (Basic)
  include Injectable.Make1 (Basic)

  (* TODO : derive *)
  module Make_traversable (A : Applicative.S) :
    Traversable.S with module A := A and type 'a t := 'a t = struct
    module TraversablePattern = Pattern.Make_traversable (A)

    let rec traverse {pattern; meta} ~f =
      A.map2 ~f:fix (f meta)
        (TraversablePattern.traverse ~f:(traverse ~f) pattern)
  end

  module Make_traversable2 (A : Applicative.S2) :
    Traversable.S2 with module A := A and type 'a t := 'a t = struct
    module TraversablePattern = Pattern.Make_traversable2 (A)

    let rec traverse {pattern; meta} ~f =
      A.map2 ~f:fix (f meta)
        (TraversablePattern.traverse ~f:(traverse ~f) pattern)
  end
end

module Make2 (First : S) (Pattern : Pattern.S2) :
  S2 with module First := First and module Pattern := Pattern = struct
  type ('a, 'b) t = {pattern: ('a First.t, ('a, 'b) t) Pattern.t; meta: 'b}
  [@@deriving compare, hash, sexp]

  let rec map f g {pattern; meta} =
    {pattern= Pattern.map (First.map f) (map f g) pattern; meta= g meta}

  let rec fold f g init {pattern; meta} =
    Pattern.fold_left
      ~f:(fun accu x -> First.fold_left ~f ~init:accu x)
      ~g:(fold f g) ~init:(g init meta) pattern

  let rec pp f g ppf {pattern; meta} =
    Fmt.pf ppf {|%a%a|} g meta (Pattern.pp (First.pp f) (pp f g)) pattern

  let proj {meta; pattern} = (meta, pattern)
  let inj (meta, pattern) = {meta; pattern}

  module Basic = struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    module First = First
    module Pattern = Pattern

    let inj = inj
    let proj = proj
  end

  include Recursive.Make2 (Basic)
  include Projectable.Make2 (Basic)
  include Corecursive.Make2 (Basic)
  include Injectable.Make2 (Basic)

  include Bifoldable.Make (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let fold = fold
  end)

  (* TODO : derive *)
  module Make_traversable (A : Applicative.S) :
    Bitraversable.S with type ('a, 'b) t := ('a, 'b) t and module A := A =
  struct
    module Traversable_pattern = Pattern.Make_traversable (A)
    module Traversable_first = First.Make_traversable (A)

    let rec traverse {pattern; meta} ~f ~g =
      A.map2 ~f:fix (g meta)
        (Traversable_pattern.traverse
           ~f:(Traversable_first.traverse ~f)
           ~g:(traverse ~f ~g) pattern)
  end

  module Make_traversable2 (A : Applicative.S2) :
    Bitraversable.S2 with type ('a, 'b) t := ('a, 'b) t and module A := A =
  struct
    module Traversable_pattern = Pattern.Make_traversable2 (A)
    module Traversable_first = First.Make_traversable2 (A)

    let rec traverse {pattern; meta} ~f ~g =
      A.map2 ~f:fix (g meta)
        (Traversable_pattern.traverse
           ~f:(Traversable_first.traverse ~f)
           ~g:(traverse ~f ~g) pattern)
  end
end
