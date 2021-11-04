(** Type-class like signatures *)

module Foldable : module type of Foldable

(** Other signatures *)

module Pretty : module type of Pretty

(** 'Two-level type' signatures and functors *)

module Pattern : module type of Pattern
module Fixed : module type of Fixed
module Label : module type of Label
module Specialized : module type of Specialized

(** General helpers *)

module Gensym : module type of Gensym
module Helpers : module type of Helpers
