(* Monad / Applicative instances *)
module State : module type of State
module Validation : module type of Validation

(* Type-class like signatures *)
module Monoid : module type of Monoid
module Functor : module type of Functor
module Bifunctor : module type of Bifunctor
module Foldable : module type of Foldable
module Bifoldable : module type of Bifoldable
module Traversable : module type of Traversable
module Bitraversable : module type of Bitraversable

(* Other signatures *)
module Pretty : module type of Pretty

(* 'Two-level type' signatures and functors *)
module Projectable : module type of Projectable
module Recursive : module type of Recursive
module Injectable : module type of Injectable
module Corecursive : module type of Corecursive
module Pattern : module type of Pattern
module Fix : module type of Fix
module Label : module type of Label
module Meta : module type of Meta
module Specialized : module type of Specialized

(* General helpers *)
module Helpers : module type of Helpers
module Gensym : module type of Gensym
