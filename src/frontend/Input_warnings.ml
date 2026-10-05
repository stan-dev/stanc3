let warnings = ref []
let init () = warnings := []
let collect () = List.rev !warnings

let empty () =
  let loc = Preprocessor.current_location () in
  let loc =
    { loc with
      begin_loc= {loc.begin_loc with line_num= 1; col_num= 0; byte_num= 0} }
  in
  warnings := Warnings.empty_file loc :: !warnings
