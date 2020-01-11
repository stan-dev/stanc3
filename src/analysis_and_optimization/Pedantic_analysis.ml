open Core_kernel
open Middle
open Middle.Program
open Middle.Expr


let list_sigma_unbounded (mir : Program.Typed.t) :
  string Set.Poly.t =
  let not_lower_zero (e : Expr.Typed.t transformation) = match e with
    | Lower {pattern= Fixed.Pattern.Lit (Int, i); _} ->
      int_of_string i <> 0
    | Lower {pattern= Fixed.Pattern.Lit (Real, i); _} ->
      float_of_string i <> 0.
    | Lower _ -> false
    | LowerUpper ({pattern= Fixed.Pattern.Lit (Int, i); _}, _) ->
      int_of_string i <> 0
    | LowerUpper ({pattern= Fixed.Pattern.Lit (Real, i); _}, _) ->
      float_of_string i <> 0.
    | LowerUpper (_, _) -> false
    | _ -> true
  in
  Set.Poly.of_list
    (List.map ~f:fst
       (List.filter
          ~f:(fun (name, {out_block; out_trans; _}) ->
              (out_block = Parameters || out_block = TransformedParameters)
              && String.is_prefix ~prefix:"sigma" name
              && not_lower_zero out_trans )
          mir.output_vars))

let list_uniform (mir : Program.Typed.t) :
  (Location_span.t * string) Set.Poly.t =
  let rec collect_uniform_stmt (stmt : Stmt.Located.t) =
    match stmt.pattern with
    | Stmt.Fixed.Pattern.TargetPE
        ({pattern=
            Expr.Fixed.Pattern.FunApp
              (StanLib, "uniform_propto_log", (({pattern= Var vname; _})::_)); _})
      -> Set.Poly.singleton (stmt.meta, vname)
    | pattern
      -> Stmt.Fixed.Pattern.fold_left
           ~g:(fun l s -> Set.Poly.union l (collect_uniform_stmt s))
           ~f:(fun l _ -> l)
           ~init:Set.Poly.empty
           pattern
  in
  List.fold
    ~f:(fun l s -> Set.Poly.union l (collect_uniform_stmt s))
    ~init:Set.Poly.empty
    mir.log_prob

let warn_set (elems : 'a Set.Poly.t) (message : 'a -> string) =
  Set.Poly.iter elems ~f:(fun elem ->
      Out_channel.output_string stderr (message elem))

let print_warn_uniform (mir : Program.Typed.t) =
  let uniforms = list_uniform mir in
  let message (loc, name) =
    "Warning: At " ^ Location_span.to_string loc ^ ", your Stan program has a uniform distribution on variable " ^ name ^ ". The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).\n"
  in warn_set uniforms message

let print_warn_sigma_unbounded (mir : Program.Typed.t) =
  let pnames = list_sigma_unbounded mir in
  let message pname =
    "Warning: Your Stan program has an unconstrained parameter \"" ^ pname ^ "\" whose name begins with \"sigma\". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.\n"
  in warn_set pnames message

let print_warn_pedantic (mir : Program.Typed.t) =
  print_warn_sigma_unbounded mir;
  print_warn_uniform mir;

(* note:expressions are Common.Foldable *)
