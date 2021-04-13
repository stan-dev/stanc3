open Core_kernel

(* XXX Core_kernel now removes Polymorphic comparisons - to upgrade, for now, I'm going to just add them back.
   We should replace these with their more specific versions at some point.
*)
let ( <> ) = Poly.( <> )
let ( < ) = Poly.( < )
let ( > ) = Poly.( > )
let ( = ) = Poly.( = )
let ( <= ) = Poly.( <= )
let ( >= ) = Poly.( >= )
let compare = Poly.compare
