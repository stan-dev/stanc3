(* Monad / Applicative instances *)
module State : module type of State
module Validation : module type of Validation

(* Type-class like signatures *)
module Monoid : module type of Monoid
module Foldable : module type of Foldable
module Bifoldable : module type of Bifoldable
module Traversable : module type of Traversable
module Bitraversable : module type of Bitraversable

(* Other signatures *)
module Pretty : module type of Pretty

(* 'Two-level type' signatures and functors *)
module Pattern : module type of Pattern
module Fix : module type of Fix
module Meta : module type of Meta
module Specialized : module type of Specialized

(* General helpers *)
module Helpers : module type of Helpers
module Gensym : module type of Gensym
