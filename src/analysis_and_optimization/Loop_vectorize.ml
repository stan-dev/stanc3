(** Loop vectorization by pi-block code generation (design doc
    [design-docs/active/vectorize-loop-fission.md]).

    This stage only reports. For every source [For], innermost first, the
    whole-loop checks run and then [Dependence_analysis.build_loop_graph] gives
    the dependence graph of the body, printed with the pi-blocks behind
    [--debug-loop-vectorization]. The program is returned unchanged. *)

open Std
open Middle
open Mir_utils
open Dependence_analysis

(* ---- The --debug-loop-vectorization report (design §7.13) ---- *)

(** What happened to one loop: left alone by a whole-loop check before any
    analysis, or analysed with the graph of the body. *)
type decision =
  | Left_alone of string
  | Analyzed of {map: dep_info_map; graph: loop_graph}

type loop_report = {loc: Location_span.t; header: string; decision: decision}

let loop_report_log : loop_report list ref = ref []

(** The reports of the last run, in program order. *)
let loop_reports () : loop_report list = List.rev !loop_report_log

(** One loop of the report: the header, then the reason the loop was left alone,
    or one line per leaf, the edges and the pi-blocks in emission order. *)
let pp_loop_report ppf {loc; header; decision} =
  Fmt.pf ppf "loop at %a  (%s)@."
    (Location_span.pp ?printed_filename:None)
    loc header;
  match decision with
  | Left_alone reason -> Fmt.pf ppf "  loop left alone: %s@." reason
  | Analyzed {map; graph} -> pp_graph map ppf graph

(* ---- The walk ---- *)

(** Decide one [For]. The whole-loop checks come first and leave the loop alone
    without any dependence analysis: a bound with effects would be evaluated
    again by every vector statement, a [break] or [continue] leaves the loop
    early, and a bound variable written in the body changes the range. *)
let decide (mir : Program.Typed.t) (loop : Stmt.Located.t) ~lower ~upper ~body :
    decision =
  if cannot_duplicate_expr lower || cannot_duplicate_expr upper then
    Left_alone "a loop bound has side effects or draws random numbers"
  else if contains_top_break_or_continue body then
    Left_alone "break or continue in the loop body"
  else
    (* one map per loop: the written set is then exactly the body's writes *)
    let map = build_dep_info_map mir loop in
    let bounds = read_variables_at map (Set.Poly.singleton root_label) in
    let written = Accesses.written_vars (subtree_accesses map root_label) in
    match Set.Poly.to_list (Set.Poly.inter bounds written) with
    | _ :: _ as written_bounds ->
        Left_alone
          (Fmt.str "loop bound variable %s is written in the body"
             (String.concat ~sep:", " written_bounds))
    | [] -> Analyzed {map; graph= build_loop_graph map ~loop:root_label}

(** Every [For], innermost first, returned unchanged with the decision recorded;
    compiler-generated loops (data reads, parameter unpacking) carry no source
    location and are left out. *)
let analyse_stmt (mir : Program.Typed.t) : Stmt.Located.t -> Stmt.Located.t =
  Stmt.rewrite_bottom_up ~f:Fun.id ~g:(fun stmt ->
      (match stmt.pattern with
      | For {loopvar; lower; upper; body}
        when Stdlib.compare stmt.meta Location_span.empty <> 0 ->
          loop_report_log :=
            { loc= stmt.meta
            ; header=
                Fmt.str "%s in %a:%a" loopvar Expr.Typed.pp lower Expr.Typed.pp
                  upper
            ; decision= decide mir stmt ~lower ~upper ~body }
            :: !loop_report_log
      | _ -> ());
      stmt)

(** Every loop of the program except in [reverse_mode_log_prob], a copy of
    [log_prob] that would report every model loop a second time. *)
let vectorize_loops (mir : Program.Typed.t) : Program.Typed.t =
  loop_report_log := [];
  let analysed =
    Program.map Fun.id (analyse_stmt mir) Fun.id
      {mir with reverse_mode_log_prob= []} in
  {analysed with reverse_mode_log_prob= mir.reverse_mode_log_prob}
