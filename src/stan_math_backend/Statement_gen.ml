open Core_kernel
open Middle
open Fmt
open Expression_gen

let pp_call_str ppf (name, args) = pp_call ppf (name, string, args)
let pp_block ppf (pp_body, body) = pf ppf "{@;<1 2>@[<v>%a@]@,}" pp_body body

let pp_profile ppf (pp_body, name, body) =
  let profile =
    Fmt.strf
      "profile<local_scalar_t__> profile__(%s, \
       const_cast<profile_map&>(profiles__));"
      name
  in
  pf ppf "{@;<1 2>@[<v>%s@;@;%a@]@,}" profile pp_body body

let rec contains_eigen (ut : UnsizedType.t) : bool =
  match ut with
  | UnsizedType.UArray t -> contains_eigen t
  | UMatrix | URowVector | UVector -> true
  | UInt | UReal | UComplex | UMathLibraryFunction | UFun _ -> false

(*Fill only needs to happen for containers
  * Note: This should probably be moved into its own function as data
  * does not need to be filled as we are promised user input data has the correct
  * dimensions. Transformed data must be filled as incorrect slices could lead
  * to elements of objects in transform data not being set by the user.
  *)
let pp_filler ppf (decl_id, st, nan_type, needs_filled) =
  match (needs_filled, contains_eigen (SizedType.to_unsized st)) with
  | true, true ->
      pf ppf "@[<hov 2>stan::math::fill(%s, %s);@]@," decl_id nan_type
  | _ -> ()

(*Pretty print a sized type*)
let pp_st ppf (st, adtype) =
  pf ppf "%a" pp_unsizedtype_local (adtype, SizedType.to_unsized st)

let pp_ut ppf (ut, adtype) = pf ppf "%a" pp_unsizedtype_local (adtype, ut)

(*Get a string representing for the NaN type of the given type *)
let nan_type (st, adtype) =
  match (adtype, st) with
  | UnsizedType.AutoDiffable, _ -> "DUMMY_VAR__"
  | DataOnly, _ -> "std::numeric_limits<double>::quiet_NaN()"

(*Pretty printer for the right hand side of expressions to initialize objects.
 * For scalar types this sets the value to NaN and for containers initializes the memory.
 *)
let rec pp_initialize ppf (st, adtype) =
  let init_nan = nan_type (st, adtype) in
  match st with
  | SizedType.SInt -> pf ppf "std::numeric_limits<int>::min()"
  | SReal -> pf ppf "%s" init_nan
  | SComplex ->
      let scalar = local_scalar (SizedType.to_unsized st) adtype in
      pf ppf "std::complex<%s>(%s, %s)" scalar init_nan init_nan
  | SVector (_, d) | SRowVector (_, d) ->
      pf ppf "%a(%a)" pp_st (st, adtype) pp_expr d
  | SMatrix (_, d1, d2) ->
      pf ppf "%a(%a, %a)" pp_st (st, adtype) pp_expr d1 pp_expr d2
  | SArray (t, d) ->
      pf ppf "%a(%a, %a)" pp_st (st, adtype) pp_expr d pp_initialize (t, adtype)

(*Initialize an object of a given size.*)
let pp_assign_sized ppf (decl_id, st, adtype, initialize) =
  let init_nan = nan_type (st, adtype) in
  let pp_assign ppf (decl_id, st, adtype) =
    pf ppf "@[<hov 2>%s = %a;@]@," decl_id pp_initialize (st, adtype)
  in
  pf ppf "@[%a%a@]@," pp_assign (decl_id, st, adtype) pp_filler
    (decl_id, st, init_nan, initialize)

let%expect_test "set size mat array" =
  let int = Expr.Helpers.int in
  strf "@[<v>%a@]" pp_assign_sized
    ( "d"
    , SArray (SArray (SMatrix (AoS, int 2, int 3), int 4), int 5)
    , DataOnly
    , false )
  |> print_endline ;
  [%expect
    {| d = std::vector<std::vector<Eigen::Matrix<double, -1, -1>>>(5, std::vector<Eigen::Matrix<double, -1, -1>>(4, Eigen::Matrix<double, -1, -1>(2, 3))); |}]

let%expect_test "set size mat array" =
  let int = Expr.Helpers.int in
  strf "@[<v>%a@]" pp_assign_sized
    ( "d"
    , SArray (SArray (SMatrix (AoS, int 2, int 3), int 4), int 5)
    , DataOnly
    , true )
  |> print_endline ;
  [%expect
    {|
    d = std::vector<std::vector<Eigen::Matrix<double, -1, -1>>>(5, std::vector<Eigen::Matrix<double, -1, -1>>(4, Eigen::Matrix<double, -1, -1>(2, 3)));
    stan::math::fill(d, std::numeric_limits<double>::quiet_NaN()); |}]

(** Initialize Data and Transformed Data
  This function is used in the model's constructor to
  {ol
  {- Initialize memory for the data and transformed data}
  {- If an Eigen type, place that memory into the class's Map}
  {- Set the initial values of that data to NaN}
  }
  @param ppf A pretty printer
  @param decl_id The name of the model class member
  @param st The type of the class member
 *)
let pp_assign_data ppf
    ((decl_id, st, needs_filled) : string * Expr.Typed.t SizedType.t * bool) =
  let init_nan = nan_type (st, DataOnly) in
  let pp_assign ppf (decl_id, st) =
    match st with
    | SizedType.SVector _ | SRowVector _ | SMatrix _ ->
        pf ppf "@[<hov 2>%s__ = %a;@]@," decl_id pp_initialize (st, DataOnly)
    | SInt | SReal | SComplex | SArray _ ->
        pf ppf "@[<hov 2>%s = %a;@]@," decl_id pp_initialize (st, DataOnly)
  in
  let pp_placement_new ppf (decl_id, st) =
    match st with
    | SizedType.SVector (_, d) | SRowVector (_, d) ->
        pf ppf "@[<hov 2>new (&%s) Eigen::Map<%a>(%s__.data(), %a);@]@,"
          decl_id pp_st (st, DataOnly) decl_id pp_expr d
    | SMatrix (_, d1, d2) ->
        pf ppf "@[<hov 2>new (&%s) Eigen::Map<%a>(%s__.data(), %a, %a);@]@,"
          decl_id pp_st (st, DataOnly) decl_id pp_expr d1 pp_expr d2
    | _ -> ()
  in
  pf ppf "@[%a%a%a@]@," pp_assign (decl_id, st) pp_placement_new (decl_id, st)
    pp_filler
    (decl_id, st, init_nan, needs_filled)

let%expect_test "set size map int array no initialize" =
  let int = Expr.Helpers.int in
  strf "@[<v>%a@]" pp_assign_data
    ("darrmat", SArray (SArray (SInt, int 4), int 5), false)
  |> print_endline ;
  [%expect
    {|
  darrmat = std::vector<std::vector<int>>(5, std::vector<int>(4, std::numeric_limits<int>::min())); |}]

let%expect_test "set size map mat array" =
  let int = Expr.Helpers.int in
  strf "@[<v>%a@]" pp_assign_data
    ( "darrmat"
    , SArray (SArray (SMatrix (AoS, int 2, int 3), int 4), int 5)
    , true )
  |> print_endline ;
  [%expect
    {|
    darrmat = std::vector<std::vector<Eigen::Matrix<double, -1, -1>>>(5, std::vector<Eigen::Matrix<double, -1, -1>>(4, Eigen::Matrix<double, -1, -1>(2, 3)));
    stan::math::fill(darrmat, std::numeric_limits<double>::quiet_NaN()); |}]

let%expect_test "set size map mat" =
  let int = Expr.Helpers.int in
  strf "@[<v>%a@]" pp_assign_data ("dmat", SMatrix (AoS, int 2, int 3), false)
  |> print_endline ;
  [%expect
    {|
    dmat__ = Eigen::Matrix<double, -1, -1>(2, 3);
    new (&dmat) Eigen::Map<Eigen::Matrix<double, -1, -1>>(dmat__.data(), 2, 3); |}]

let%expect_test "set size map int" =
  strf "@[<v>%a@]" pp_assign_data ("dint", SInt, true) |> print_endline ;
  [%expect {|
  dint = std::numeric_limits<int>::min(); |}]

(** [pp_for_loop ppf (loopvar, lower, upper, pp_body, body)] tries to
    pretty print a for-loop from lower to upper given some loopvar.*)
let pp_for_loop ppf (loopvar, lower, upper, pp_body, body) =
  pf ppf "@[for (@[int %s = %a;@ %s <= %a;@ ++%s@])" loopvar pp_expr lower
    loopvar pp_expr upper loopvar ;
  pf ppf " %a@]" pp_body body

let rec integer_el_type = function
  | SizedType.SInt -> true
  | SArray (st, _) -> integer_el_type st
  | _ -> false

(** Print the private members of the model class

  Accounting for types that can be moved to OpenCL.
  @param ppf A formatter
  @param vident name of the private member.
  @param ut The unsized type to print.
 *)
let pp_data_decl ppf (vident, ut) =
  let opencl_check = (Transform_Mir.is_opencl_var vident, ut) in
  let pp_type =
    match opencl_check with
    | _, UnsizedType.(UInt | UReal) | false, _ -> pp_unsizedtype_local
    | true, UArray UInt -> fun ppf _ -> pf ppf "matrix_cl<int>"
    | true, _ -> fun ppf _ -> pf ppf "matrix_cl<double>"
  in
  match (opencl_check, ut) with
  | (false, _), ut -> (
    match ut with
    | UnsizedType.URowVector | UVector | UMatrix ->
        pf ppf "%a %s__;" pp_type (DataOnly, ut) vident
    | _ -> pf ppf "%a %s;" pp_type (DataOnly, ut) vident )
  | (true, _), _ -> pf ppf "%a %s;" pp_type (DataOnly, ut) vident

(** Create string representations for [vars__.emplace_back] *)
let pp_emplace_var ppf var =
  match Expr.Typed.type_of var with
  | UnsizedType.UComplex ->
      pf ppf "@[<hov 2>vars__.emplace_back(%a.real());@]@," pp_expr var ;
      pf ppf "@[<hov 2>vars__.emplace_back(%a.imag());@]" pp_expr var
  | _ -> pf ppf "@[<hov 2>vars__.emplace_back(@,%a);@]" pp_expr var

(** Create strings representing maps of Eigen types*)
let pp_map_decl ppf (vident, ut) =
  let scalar = local_scalar ut DataOnly in
  match ut with
  | UnsizedType.UInt | UReal -> ()
  | UMatrix ->
      pf ppf "Eigen::Map<Eigen::Matrix<%s, -1, -1>> %s{nullptr, 0, 0};" scalar
        vident
  | URowVector ->
      pf ppf "Eigen::Map<Eigen::Matrix<%s, 1, -1>> %s{nullptr, 0};" scalar
        vident
  | UVector ->
      pf ppf "Eigen::Map<Eigen::Matrix<%s, -1, 1>> %s{nullptr, 0};" scalar
        vident
  | x ->
      raise_s
        [%message
          "Error during Map data construction for " vident " of type "
            (x : UnsizedType.t)
            ". This should never happen, if you see this please file a bug \
             report."]

let pp_unsized_decl ppf (vident, ut, adtype) =
  let pp_type =
    match (Transform_Mir.is_opencl_var vident, ut) with
    | _, UnsizedType.(UInt | UReal) | false, _ -> pp_unsizedtype_local
    | true, UArray UInt -> fun ppf _ -> pf ppf "matrix_cl<int>"
    | true, _ -> fun ppf _ -> pf ppf "matrix_cl<double>"
  in
  pf ppf "%a %s;" pp_type (adtype, ut) vident

let pp_sized_decl ppf (vident, st, adtype, initialize) =
  match initialize with
  | true ->
      pf ppf "%a@,%a" pp_unsized_decl
        (vident, SizedType.to_unsized st, adtype)
        pp_assign_sized
        (vident, st, adtype, initialize)
  | false ->
      pf ppf "%a" pp_unsized_decl (vident, SizedType.to_unsized st, adtype)

let pp_decl ppf (vident, pst, adtype, initialize) =
  match pst with
  | Type.Sized st -> pp_sized_decl ppf (vident, st, adtype, initialize)
  | Unsized ut -> pp_unsized_decl ppf (vident, ut, adtype)

let math_fn_translations = function
  | Internal_fun.FnLength -> Some ("length", [])
  | FnValidateSize -> Some ("validate_non_negative_index", [])
  | FnValidateSizeSimplex -> Some ("validate_positive_index", [])
  | FnValidateSizeUnitVector -> Some ("validate_unit_vector_index", [])
  | FnReadWriteEventsOpenCL x -> Some (x ^ ".wait_for_read_write_events", [])
  | _ -> None

let trans_math_fn f =
  Option.(
    value ~default:(Internal_fun.to_string f, []) (math_fn_translations f))

let pp_bool_expr ppf expr =
  match Expr.Typed.type_of expr with
  | UReal -> pp_call ppf ("as_bool", pp_expr, [expr])
  | _ -> pp_expr ppf expr

let rec pp_statement (ppf : Format.formatter) Stmt.Fixed.({pattern; meta}) =
  (* ({stmt; smeta} : (mtype_loc_ad, 'a) stmt_with) = *)
  let pp_stmt_list = list ~sep:cut pp_statement in
  ( match pattern with
  | Block _ | SList _ | Decl _ | Skip | Break | Continue -> ()
  | _ -> Locations.pp_smeta ppf meta ) ;
  match pattern with
  | Assignment
      ( (vident, _, [])
      , ( { pattern= FunApp (CompilerInternal (FnReadData | FnReadParam _), _); _
          } as rhs ) ) ->
      pf ppf "@[<hov 4>%s = %a;@]" vident pp_expr rhs
  | Assignment
      ((vident, _, []), ({meta= Expr.Typed.Meta.({type_= UInt; _}); _} as rhs))
   |Assignment ((vident, _, []), ({meta= {type_= UReal; _}; _} as rhs)) ->
      pf ppf "@[<hov 4>%s = %a;@]" vident pp_expr rhs
  | Assignment ((assignee, UInt, idcs), rhs)
   |Assignment ((assignee, UReal, idcs), rhs)
    when List.for_all ~f:is_single_index idcs ->
      pf ppf "@[<hov 4>%a = %a;@]" pp_indexed_simple (assignee, idcs) pp_expr
        rhs
  | Assignment ((assignee, _, idcs), rhs) ->
      (* XXX I think in general we don't need to do a deepcopy if e is nested
       inside some function call - the function should get its own copy
       (in all cases???) *)
      let rec maybe_deep_copy e =
        let recurse (e : 'a Expr.Fixed.t) =
          { e with
            Expr.Fixed.pattern=
              Expr.Fixed.Pattern.map maybe_deep_copy e.pattern }
        in
        match e.pattern with
        | _ when UnsizedType.is_scalar_type (Expr.Typed.type_of e) -> e
        | FunApp (CompilerInternal _, _) -> e
        | (Indexed ({Expr.Fixed.pattern= Var v; _}, _) | Var v)
          when v = assignee ->
            { e with
              Expr.Fixed.pattern= FunApp (CompilerInternal FnDeepCopy, [e]) }
        | _ -> recurse e
      in
      let rhs = maybe_deep_copy rhs in
      pf ppf "@[<hov 2>assign(@,%s,@ %a,@ %S%s%a@]);" assignee pp_expr rhs
        (strf "assigning variable %s" assignee)
        (if List.length idcs = 0 then "" else ", ")
        pp_indexes idcs
  | TargetPE e -> pf ppf "@[<hov 2>lp_accum__.add(@,%a@]);" pp_expr e
  | NRFunApp (CompilerInternal FnPrint, args) ->
      let pp_arg ppf a = pf ppf "stan_print(pstream__, %a);" pp_expr a in
      let args = args @ [Expr.Helpers.str "\n"] in
      pf ppf "if (pstream__) %a" pp_block (list ~sep:cut pp_arg, args)
  | NRFunApp (CompilerInternal FnReject, args) ->
      let err_strm = "errmsg_stream__" in
      let add_to_string ppf e = pf ppf "%s << %a;" err_strm pp_expr e in
      pf ppf "std::stringstream %s;@," err_strm ;
      pf ppf "%a@," (list ~sep:cut add_to_string) args ;
      pf ppf "throw std::domain_error(%s.str());" err_strm
  | NRFunApp (CompilerInternal (FnCheck {trans; var_name; var}), args) ->
      Option.iter (check_to_string trans) ~f:(fun check_name ->
          let function_arg = Expr.Helpers.variable "function__" in
          pf ppf "%s(@[<hov>%a@]);" ("check_" ^ check_name)
            (list ~sep:comma pp_expr)
            (function_arg :: Expr.Helpers.str var_name :: var :: args) )
  | NRFunApp (CompilerInternal (FnWriteParam {unconstrain_opt; var}), _) -> (
    match
      (unconstrain_opt, Option.bind ~f:constraint_to_string unconstrain_opt)
    with
    (* When the current block or this transformation doesn't require unconstraining,
       use vanilla write *)
    | None, _ | _, None -> pf ppf "@[<hov 2>out__.write(@,%a);@]" pp_expr var
    (* Otherwise, use stan::io::serializer's write_free functions *)
    | Some trans, Some unconstrain_string ->
        let unconstrain_args = transform_args trans in
        pf ppf "@[<hov 2>out__.write_free_%s(@,%a);@]" unconstrain_string
          (list ~sep:comma pp_expr)
          (unconstrain_args @ [var]) )
  | NRFunApp (CompilerInternal f, args) ->
      let fname, extra_args = trans_math_fn f in
      pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr)
        (extra_args @ args)
  | NRFunApp (StanLib (fname, _, _), args) ->
      pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr) args
  | NRFunApp (UserDefined (fname, suffix), args) ->
      pf ppf "%a;" pp_user_defined_fun (fname, suffix, args)
  | Break -> string ppf "break;"
  | Continue -> string ppf "continue;"
  | Return e -> pf ppf "@[<hov 4>return %a;@]" (option pp_expr) e
  | Skip -> string ppf ";"
  | IfElse (cond, ifbranch, elsebranch) ->
      let pp_else ppf x = pf ppf "else %a" pp_statement x in
      pf ppf "if (@[<hov>%a@]) %a %a" pp_bool_expr cond pp_block_s ifbranch
        (option pp_else) elsebranch
  | While (cond, body) ->
      pf ppf "while (@[<hov>%a@]) %a" pp_bool_expr cond pp_block_s body
  | For
      { body=
          { pattern=
              Assignment
                (_, {pattern= FunApp (CompilerInternal (FnReadParam _), _); _}); _
          } as body; _ } ->
      pp_statement ppf body
      (* Skip For loop part, just emit body due to the way FnReadParam emits *)
  | For {loopvar; lower; upper; body} ->
      pp_for_loop ppf (loopvar, lower, upper, pp_statement, body)
  | Profile (name, ls) -> pp_profile ppf (pp_stmt_list, name, ls)
  | Block ls -> pp_block ppf (pp_stmt_list, ls)
  | SList ls -> pp_stmt_list ppf ls
  | Decl {decl_adtype; decl_id; decl_type; initialize; _} ->
      pp_decl ppf (decl_id, decl_type, decl_adtype, initialize)

and pp_block_s ppf body =
  match body.pattern with
  | Block ls -> pp_block ppf (list ~sep:cut pp_statement, ls)
  | _ -> pp_block ppf (pp_statement, body)
