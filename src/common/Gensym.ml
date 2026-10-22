module type Gensym = sig
  val next : ?infix:string -> unit -> string
  val enter : unit -> string * (unit -> unit)
end

module type Internal = sig
  val reset : unit -> unit

  include Gensym
end

let callbacks : (string * (module Internal)) list ref = ref []

let make ~prefix =
  let gen =
    match List.assoc_opt prefix !callbacks with
    | Some gen -> gen
    | None ->
        let gen =
          (module struct
            let counter = ref 0
            let reset () = counter := 0

            let next ?(infix = "") () =
              counter := !counter + 1;
              prefix ^ infix ^ "_sym" ^ string_of_int !counter ^ "__"

            let enter () =
              let old_counter = !counter in
              (next (), fun () -> counter := old_counter)
          end : Internal) in
        callbacks := (prefix, gen) :: !callbacks;
        gen in
  (module (val gen) : Gensym)

let reset_danger_use_cautiously () =
  List.iter (fun (_, (module Gen : Internal)) -> Gen.reset ()) !callbacks
