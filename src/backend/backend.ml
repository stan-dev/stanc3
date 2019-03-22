open Stanc_mir

module type SIG = sig 
    type out
    val generate : Mir.typed_prog -> out
end 