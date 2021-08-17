open Core_kernel
open Middle

let dist_prefix = "tfd__."
let kwrds_suffix = "__"

let append_kwrds_suffix x =
  let x_with_suffix = x ^ kwrds_suffix in
  Fmt.epr "Identifier %s is a reserved word in python, renamed to %s@," x
    x_with_suffix ;
  x_with_suffix

let python_kwrds =
  String.Set.of_list
    [ "False"; "None"; "True"; "and"; "as"; "assert"; "break"; "class"
    ; "continue"; "def"; "del"; "elif"; "else"; "except"; "finally"; "for"
    ; "from"; "global"; "if"; "import"; "in"; "is"; "lambda"; "nonlocal"; "not"
    ; "or"; "pass"; "raise"; "return"; "try"; "while"; "with"; "yield"; "await"
    ; "async" ]

let add_suffix_to_kwrds s =
  if Set.mem python_kwrds s then append_kwrds_suffix s else s

let remove_stan_dist_suffix s =
  let s = Utils.stdlib_distribution_name s in
  List.filter_map
    (("_rng" :: Utils.distribution_suffices) @ [""])
    ~f:(fun suffix -> String.chop_suffix ~suffix s)
  |> List.hd_exn

let capitalize_fnames =
  String.Set.of_list
    ["normal"; "cauchy"; "gumbel"; "exponential"; "gamma"; "beta"; "poisson"]

let map_functions fname args =
  let open Expr in
  let none = {Fixed.pattern= Var "None"; meta= Typed.Meta.empty} in
  match (fname, args) with
  | "multi_normal_cholesky", _ -> ("MultivariateNormalTriL", args)
  | "student_t", _ -> ("StudentT", args)
  | "double_exponential", _ -> ("Laplace", args)
  | "lognormal", _ -> ("LogNormal", args)
  | "chi_square", _ -> ("Chi2", args)
  | "inv_gamma", _ -> ("InverseGamma", args)
  | "lkj_corr_cholesky", _ -> ("CholeskyLKJ", args)
  | "binomial_logit", _ -> ("Binomial", args)
  | "bernoulli_logit", _ -> ("Bernoulli", args)
  | "von_mises", _ -> ("VonMises", args)
  | "binomial", [y; n; p] -> ("Binomial", [y; n; none; p])
  | "bernoulli", [y; p] -> ("Bernoulli", [y; none; p])
  | "poisson_log", [y; log_lambda] -> ("Poisson", [y; none; log_lambda])
  | "pareto", [y; y_min; alpha] -> ("Pareto", [y; alpha; y_min])
  | "neg_binomial", [y; a; b] ->
      ( "NegativeBinomial"
      , [y; a; none; Helpers.(binop (int 1) Divide (binop (int 1) Plus b))] )
  | (("neg_binomial_2" | "neg_binomial_2_log") as l), _ ->
      raise_s
        [%message l " is not supported, consider using neg_binomial instead."]
  | f, _ when Operator.of_string_opt f |> Option.is_some -> (fname, args)
  | _ ->
      if Set.mem capitalize_fnames fname then (String.capitalize fname, args)
      else raise_s [%message "Not sure how to handle " fname " yet!"]

let translate_funapps_and_kwrds e =
  let open Expr.Fixed in
  let f ({pattern; _} as expr) =
    match pattern with
    | FunApp (StanLib (fname, suffix, mem_pattern), args) ->
        let prefix =
          if Utils.is_distribution_name fname then dist_prefix else ""
        in
        let fname = remove_stan_dist_suffix fname in
        let fname, args = map_functions fname args in
        { expr with
          pattern= FunApp (StanLib (prefix ^ fname, suffix, mem_pattern), args)
        }
    | FunApp (UserDefined (fname, suffix), args) ->
        { expr with
          pattern=
            FunApp (UserDefined (add_suffix_to_kwrds fname, suffix), args) }
    | Var s -> {expr with pattern= Var (add_suffix_to_kwrds s)}
    | _ -> expr
  in
  rewrite_bottom_up ~f e

let%expect_test "nested dist prefixes translated" =
  let open Expr.Fixed.Pattern in
  let e pattern = {Expr.Fixed.pattern; meta= Expr.Typed.Meta.empty} in
  let f =
    FunApp
      ( Fun_kind.StanLib ("normal_lpdf", FnLpdf false, AoS)
      , [FunApp (Fun_kind.StanLib ("normal_lpdf", FnLpdf false, AoS), []) |> e]
      )
    |> e |> translate_funapps_and_kwrds
  in
  print_s [%sexp (f : Expr.Typed.Meta.t Expr.Fixed.t)] ;
  [%expect
    {|
    ((pattern
      (FunApp (StanLib tfd__.Normal (FnLpdf false) AoS)
       (((pattern (FunApp (StanLib tfd__.Normal (FnLpdf false) AoS) ()))
         (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
     (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))) |}]

(* temporary until we get rid of these from the MIR *)
let rec remove_unused_stmts s =
  let pattern =
    match s.Stmt.Fixed.pattern with
    | Decl _ -> Stmt.Fixed.Pattern.Skip
    | NRFunApp
        ( CompilerInternal
            ( FnCheck _ | FnValidateSize | FnValidateSizeSimplex
            | FnValidateSizeUnitVector )
        , _ ) ->
        Stmt.Fixed.Pattern.Skip
    | x -> Stmt.Fixed.Pattern.map Fn.id remove_unused_stmts x
  in
  {s with pattern}

let rec change_kwrds_stmts s =
  let open Stmt.Fixed.Pattern in
  let pattern =
    match s.Stmt.Fixed.pattern with
    | Decl e -> Decl {e with decl_id= add_suffix_to_kwrds e.decl_id}
    | NRFunApp (UserDefined (s, sfx), e) ->
        NRFunApp (UserDefined (add_suffix_to_kwrds s, sfx), e)
    | NRFunApp (StanLib (s, sfx, _), e) ->
        NRFunApp (UserDefined (add_suffix_to_kwrds s, sfx), e)
    | Assignment ((s, t, e1), e2) ->
        Assignment ((add_suffix_to_kwrds s, t, e1), e2)
    | For e -> For {e with loopvar= add_suffix_to_kwrds e.loopvar}
    | x -> map Fn.id change_kwrds_stmts x
  in
  {s with pattern}

let trans_prog (p : Program.Typed.t) =
  let rec map_stmt {Stmt.Fixed.pattern; meta} =
    { Stmt.Fixed.pattern=
        Stmt.Fixed.Pattern.map translate_funapps_and_kwrds map_stmt pattern
    ; meta }
  in
  let rename_kwrds (s, e) = (add_suffix_to_kwrds s, e) in
  let rename_fdarg (e1, s, e2) = (e1, add_suffix_to_kwrds s, e2) in
  let rename_func (s : 'a Program.fun_def) =
    { s with
      fdname= add_suffix_to_kwrds s.fdname
    ; fdargs= List.map ~f:rename_fdarg s.fdargs }
  in
  Program.map translate_funapps_and_kwrds map_stmt
    { p with
      output_vars= List.map ~f:rename_kwrds p.output_vars
    ; input_vars= List.map ~f:rename_kwrds p.input_vars
    ; functions_block= List.map ~f:rename_func p.functions_block }
  |> Program.map Fn.id change_kwrds_stmts
  |> Program.map Fn.id remove_unused_stmts
  |> Program.map_stmts Analysis_and_optimization.Mir_utils.cleanup_empty_stmts
