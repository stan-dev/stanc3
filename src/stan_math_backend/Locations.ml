open Core_kernel
open Middle
open Common
open State.Cps

module Numbered = struct
  module Meta = struct
    type t = int [@@deriving sexp, hash, compare]

    let pp _ _ = ()
    let init = 0
    let next meta = meta + 1
  end

  include Specialized.Make2 (Stmt.Fixed) (Expr.Typed) (Meta)
end

type typed_prog_num = (Expr.Typed.t, Numbered.t) Program.t [@@deriving sexp]
type state_t = Location_span.t list

let no_span_num = 0

module Traversable_state = Program.Make_traversable2 (State)
module Traversable_stmt_state = Stmt.Fixed.Make_traversable2 (State)
module Traversable_expr_state = Expr.Fixed.Make_traversable2 (State)

let number ~init (prog : Program.Typed.t) =
  let g loc =
    State.(
      get
      >>= fun (label, label_to_loc, loc_to_label) ->
      match Location_span.Map.find loc_to_label loc with
      | Some lbl -> return lbl
      | _ ->
          let loc_to_label' =
            Location_span.Map.add_exn loc_to_label ~key:loc ~data:label
          and label_to_loc' = Int.Map.add_exn label_to_loc ~key:label ~data:loc
          and next_label = Numbered.Meta.next label in
          put (next_label, label_to_loc', loc_to_label')
          >>= fun _ -> return label)
  in
  Traversable_state.traverse prog
    ~f:(Traversable_expr_state.traverse ~f:State.return)
    ~g:(Traversable_stmt_state.traverse ~f:State.return ~g)
  |> State.run_state ~init

let prepare_prog (mir : Program.Typed.t) =
  let label_to_loc =
    Int.(
      Map.add_exn ~key:Numbered.Meta.init ~data:Location_span.empty Map.empty)
  and loc_to_label =
    Location_span.(Map.add_exn ~key:empty ~data:Numbered.Meta.init Map.empty)
  in
  let prog, (_, label_to_loc, _) =
    number ~init:(no_span_num + 1, label_to_loc, loc_to_label) mir
  in
  let location_list =
    Map.to_alist label_to_loc
    |> List.sort ~compare:(fun x y -> compare_int (fst x) (fst y))
    |> List.map ~f:snd
  in
  (prog, location_list)

let pp_globals ppf location_list =
  let location_list =
    " (found before start of program)"
    :: ( List.filter ~f:(fun x -> x <> Location_span.empty) location_list
       |> List.map ~f:(fun x -> " (in " ^ Location_span.to_string x ^ ")") )
  in
  Fmt.pf ppf
    "@ static int current_statement__ = 0;@ static const std::vector<string> \
     locations_array__ = {@[<hov>%a@]};@ "
    Fmt.(list ~sep:comma (fmt "%S"))
    location_list

let pp_smeta ppf location_num =
  if location_num = no_span_num then ()
  else Fmt.pf ppf "current_statement__ = %d;@;" location_num
