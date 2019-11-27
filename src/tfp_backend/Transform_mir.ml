open Core_kernel
open Middle

let dist_prefix = "tfd__."

let remove_stan_dist_suffix s =
  let s = Utils.stdlib_distribution_name s in
  List.filter_map
    (("_rng" :: Utils.distribution_suffices) @ [""])
    ~f:(fun suffix -> String.chop_suffix ~suffix s)
  |> List.hd_exn

let capitalize_fnames = String.Set.of_list ["normal"; "cauchy"]
let none = {Expr.Helpers.zero with Expr.Fixed.pattern= Var "None"}

let map_functions fname args =
  match fname with
  | "multi_normal_cholesky" -> ("MultivariateNormalTriL", args)
  | "lognormal" -> ("LogNormal", args)
  | "bernoulli_logit" -> ("Bernoulli", args)
  | f when f = Operator.to_string Less -> ("tf__.less", args)
  | f when Operator.of_string_opt f |> Option.is_some -> (fname, args)
  | _ ->
      if Set.mem capitalize_fnames fname then (String.capitalize fname, args)
      else raise_s [%message "Not sure how to handle " fname " yet!"]

let translate_funapps e =
  let open Expr.Fixed in
  let f ({pattern; _} as expr) =
    match pattern with
    | FunApp (StanLib, fname, args) ->
        let prefix =
          if Utils.is_distribution_name fname then dist_prefix else ""
        in
        let fname = remove_stan_dist_suffix fname in
        let fname, args = map_functions fname args in
        {expr with pattern= FunApp (StanLib, prefix ^ fname, args)}
    | _ -> expr
  in
  rewrite_bottom_up ~f e

let%expect_test "nested dist prefixes translated" =
  let open Expr.Fixed.Pattern in
  let e pattern = {Expr.Fixed.pattern; meta= ()} in
  let f =
    FunApp
      ( Fun_kind.StanLib
      , "normal_lpdf"
      , [FunApp (Fun_kind.StanLib, "normal_lpdf", []) |> e] )
    |> e |> translate_funapps
  in
  print_s [%sexp (f : unit Expr.Fixed.t)] ;
  [%expect
    {|
    ((pattern
      (FunApp StanLib tfd__.Normal
       (((pattern (FunApp StanLib tfd__.Normal ())) (meta ())))))
     (meta ())) |}]

let minus_one e =
  { e with
    Expr.Fixed.pattern=
      FunApp (StanLib, Operator.to_string Minus, [e; Expr.Helpers.loop_bottom])
  }

let one_to_zero_indexing e =
  let open Expr.Fixed.Pattern in
  let single_minus_one = function
    | Index.Single e -> Index.Single (minus_one e)
    | i -> i
  in
  match e.Expr.Fixed.pattern with
  | Indexed (obj, idcs) ->
      {e with pattern= Indexed (obj, List.map ~f:single_minus_one idcs)}
  | _ -> e

(* All of this int to real transformation shenanigans is necessary because
   TFP / TF throw errors if you give the wrong numeric type, but this would
   also be fixed if we did handled Stan auto-promotion of ints to reals in the
   type inference stage, see https://github.com/stan-dev/stanc3/issues/373
   Right now, we've implemented a few heuristics below.
*)
let rec int_to_real e =
  let open Expr.Fixed.Pattern in
  match e.Expr.Fixed.pattern with
  | Lit (Int, s) -> {e with pattern= Lit (Real, s)}
  | FunApp (fk, f, args) when Option.is_some (Operator.of_string_opt f) ->
      (* We don't want to convert operator arguments *)
      {e with pattern= FunApp (fk, f, List.map ~f:int_to_real args)}
  | ep -> {e with pattern= Expr.Fixed.Pattern.map int_to_real ep}

let rec int_to_real_stmt s =
  let open Stmt.Fixed.Pattern in
  match s.Stmt.Fixed.pattern with
  | For ({body; _} as f) ->
      (* We don't want to convert loop bounds*)
      {s with pattern= For {f with body= int_to_real_stmt body}}
  | sp -> {s with pattern= map int_to_real int_to_real_stmt sp}

let real_transformation_args =
  Program.map_transformation (Expr.Fixed.rewrite_top_down ~f:int_to_real)

let map_transformations f p =
  { p with
    Program.output_vars=
      List.map p.Program.output_vars ~f:(function
          | n, ({Program.out_trans; _} as ov) ->
          (n, {ov with out_trans= f out_trans}) ) }

(* temporary until we get rid of these from the MIR *)
let rec remove_unused_stmts s =
  let pattern =
    match s.Stmt.Fixed.pattern with
    | Assignment (_, {Expr.Fixed.pattern= FunApp (CompilerInternal, f, _); _})
      when Internal_fun.to_string FnConstrain = f
           || Internal_fun.to_string FnUnconstrain = f ->
        Stmt.Fixed.Pattern.Skip
    | Decl _ -> Stmt.Fixed.Pattern.Skip
    | x -> Stmt.Fixed.Pattern.map Fn.id remove_unused_stmts x
  in
  {s with pattern}

let rewrite_expressions f =
  Program.map f (Stmt.Fixed.rewrite_top_down ~f ~g:Fn.id)

let trans_prog (p : Program.Typed.t) =
  let rec map_stmt {Stmt.Fixed.pattern; meta} =
    { Stmt.Fixed.pattern=
        Stmt.Fixed.Pattern.map translate_funapps map_stmt pattern
    ; meta }
  in
  Program.map translate_funapps map_stmt p
  |> Program.map Fn.id remove_unused_stmts
  |> rewrite_expressions int_to_real
  (* |> rewrite_expressions one_to_zero_indexing *)
  |> map_transformations real_transformation_args
  |> Analysis_and_optimization.Optimize.vectorize
