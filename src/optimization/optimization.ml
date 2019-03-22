open Stanc_mir

module type SIG = sig 
    val optimize : Mir.typed_prog -> Mir.typed_prog
end 

module Compose (A : SIG) (B : SIG) : SIG = struct
    let optimize typed_prog = 
        typed_prog
        |> A.optimize 
        |> B.optimize
end 

module Identity : SIG = struct 
    let optimize typed_prog = typed_prog 
end 

