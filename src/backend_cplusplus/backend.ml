open Stanc_backend
open Stan_math_code_gen

module CPlusPlus : Backend.SIG with type out = string
    = struct
    type out = string

    let generate typed_prog = 
        Format.asprintf "%a" pp_prog typed_prog
end 