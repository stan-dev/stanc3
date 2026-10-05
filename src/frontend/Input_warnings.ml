let warnings = ref []
let init () = warnings := []
let collect () = List.rev !warnings
let empty () = warnings := Warnings.EmptyFile :: !warnings
