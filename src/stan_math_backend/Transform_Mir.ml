open Core_kernel
open Middle

let opencl_triggers =
  String.Map.of_alist_exn
    [ ("normal_id_glm_lpdf",
        ([0;1],
          [ (* Array of conditions under which to move to OpenCL *)
            ([1],[]) (* Argument 1 is data *)
          ]
        )
      )
    ; ("bernoulli_logit_glm_lpmf",
        ([0;1], [([1],[])])
      )
    ; ("categorical_logit_glm_lpmf",
        ([0;1], [([-1],[(1,UMatrix)])]) (* Always use OpenCL, when argument 1 is a matrix *)
      )
    ; ("neg_binomial_2_log_glm_lpmf",
        ([0;1], [([1],[])])
      )
    ; ("ordered_logistic_glm_lpmf",
        ([0;1], [([1],[(1,UMatrix)])])
      )
    ; ("poisson_log_glm_lpmf",
        ([0;1], [([1],[])])
      ) 
    ]
let opencl_suffix = "_opencl__"
let to_matrix_cl e = {e with expr= FunApp (StanLib, "to_matrix_cl", [e])}

let rec switch_expr_to_opencl _available_cl_vars e =
  let is_avail = List.mem _available_cl_vars ~equal:( = ) in
  let to_cl e =
    match e.expr with
    | Var s when is_avail s -> {e with expr= Var (s ^ opencl_suffix)}
    | _ -> to_matrix_cl e
  in
  match e.expr with
  | FunApp (StanLib, f, args) when Map.mem opencl_triggers f ->
      let (cl_args, req_args) = Map.find_exn opencl_triggers f in 
      let check_if_type arg t = 
        arg.emeta.mtype = t
      in
      let rec nth_arg_type arg i t = 
        match arg with 
        | [] -> false
        | hd :: tl -> if i=0 then check_if_type hd t else nth_arg_type tl (i-1) t
      in
      let rec data_types_match t = 
        match t with
        | (i,t) :: tl -> 
          if nth_arg_type args i t then data_types_match tl else false
        | [] -> true (* No type requirements or end of list *)
      in
      let check_if_data arg = 
        match arg.expr with 
          | Var s when is_avail s -> true
          | _ -> false
      in
      let rec nth_arg_data arg i = 
        match arg with 
        | [] -> false
        | hd :: tl -> if i=0 then check_if_data hd else nth_arg_data tl (i-1)
      in
      let req_met (data_arg, type_arg) = 
        match data_arg with
        | [-1] -> data_types_match type_arg (*no data requirements, straight to matching data types*)
        | hd :: _tl -> 
          if nth_arg_data args hd then data_types_match type_arg else false
        | [] -> true (*if the list is empty that means that all requirements are satisfied *)
      in
      let rec triggers_match _md = 
        match _md with
        | hd :: tl -> if req_met hd then true else triggers_match tl
        | [] -> false        
      in
      let move_cl_args index arg =
        if List.mem ~equal:( = ) cl_args index then to_cl arg else arg
      in
      let mapped_args = 
        if triggers_match req_args then
          List.mapi args ~f:move_cl_args
        else
          args
      in
      {e with expr= FunApp (StanLib, f, mapped_args)}      
  | x -> {e with expr= map_expr (switch_expr_to_opencl _available_cl_vars) x}

let pos = "pos__"
let is_scalar = function SInt | SReal -> true | _ -> false

let data_read smeta (decl_id, st) =
  let decl_var =
    { expr= Var decl_id
    ; emeta= {mloc= smeta; mtype= remove_size st; madlevel= DataOnly} }
  in
  let swrap stmt = {stmt; smeta} in
  let bodyfn var =
    let pos_var = {expr= Var pos; emeta= internal_meta} in
    let readfnapp var =
      let f =
        internal_funapp FnReadData
          [{var with expr= Lit (Str, decl_id)}]
          var.emeta
      in
      {expr= Indexed (f, [Single pos_var]); emeta= {var.emeta with mtype= UInt}}
    in
    let pos_increment =
      if is_scalar st then []
      else
        [Assignment ((pos, UInt, []), binop pos_var Plus (mir_int 1)) |> swrap]
    in
    SList
      ( assign_indexed (remove_size st) decl_id smeta readfnapp var
      :: pos_increment )
    |> swrap
  in
  let pos_reset = Assignment ((pos, UInt, []), loop_bottom) |> swrap in
  [pos_reset; for_scalar_inv st bodyfn decl_var smeta]

let rec base_type = function
  | SArray (t, _) -> base_type t
  | SVector _ | SRowVector _ | SMatrix _ -> UReal
  | x -> remove_size x

let rec base_ut_to_string = function
  | UMatrix -> "matrix"
  | UVector -> "vector"
  | URowVector -> "row_vector"
  | UReal -> "scalar"
  | UInt -> "integer"
  | UArray t -> base_ut_to_string t
  | t ->
      raise_s
        [%message "Another place where it's weird to get " (t : unsizedtype)]

let param_read smeta
    ( decl_id
    , {out_constrained_st= cst; out_unconstrained_st= ucst; out_block; _} ) =
  if not (out_block = Parameters) then []
  else
    let decl_id, decl =
      match cst = ucst with
      | true -> (decl_id, [])
      | false ->
          let decl_id = decl_id ^ "_in__" in
          let d =
            Decl {decl_adtype= AutoDiffable; decl_id; decl_type= Sized ucst}
          in
          (decl_id, [{stmt= d; smeta}])
    in
    let unconstrained_decl_var =
      { expr= Var decl_id
      ; emeta= {mloc= smeta; mtype= remove_size cst; madlevel= AutoDiffable} }
    in
    let bodyfn var =
      let readfnapp var =
        internal_funapp FnReadParam
          ( { expr= Lit (Str, base_ut_to_string (remove_size ucst))
            ; emeta= internal_meta }
          :: eigen_size ucst )
          {var.emeta with mtype= base_type ucst}
      in
      assign_indexed (remove_size cst) decl_id smeta readfnapp var
    in
    decl @ [for_eigen ucst bodyfn unconstrained_decl_var smeta]

let escape_name str =
  str
  |> String.substr_replace_all ~pattern:"." ~with_:"_"
  |> String.substr_replace_all ~pattern:"-" ~with_:"_"

let rec add_jacobians {stmt; smeta} =
  match stmt with
  | Assignment (lhs, {expr= FunApp (CompilerInternal, f, args); emeta})
    when internal_fn_of_string f = Some FnConstrain ->
      let var n = {expr= Var n; emeta= internal_meta} in
      let assign rhs = {stmt= Assignment (lhs, rhs); smeta} in
      { stmt=
          IfElse
            ( var "jacobian__"
            , assign
                {expr= FunApp (CompilerInternal, f, args @ [var "lp__"]); emeta}
            , Some (assign {expr= FunApp (CompilerInternal, f, args); emeta})
            )
      ; smeta }
  | _ -> {stmt= map_statement Fn.id add_jacobians stmt; smeta}

(* Make sure that all if-while-and-for bodies are safely wrapped in a block in such a way that we can insert a location update before.
   The blocks make sure that the program with the inserted location update is still well-formed C++ though.
   *)
let rec ensure_body_in_block {stmt; smeta} =
  let in_block {stmt; smeta} =
    { stmt=
        ( match stmt with
        | Block l | SList l -> Block l
        | stmt -> Block [{stmt; smeta}] )
    ; smeta }
  in
  let ensure_body_in_block_base stmt =
    match stmt with
    | IfElse (_, _, _) | While (_, _) | For _ ->
        map_statement (fun x -> x) in_block stmt
    | _ -> stmt
  in
  { stmt=
      ensure_body_in_block_base
        (map_statement (fun x -> x) ensure_body_in_block stmt)
  ; smeta }

let flatten_slist = function {stmt= SList ls; _} -> ls | x -> [x]

let add_reads stmts vars mkread =
  let var_names = String.Map.of_alist_exn vars in
  let add_read_to_decl = function
    | {stmt= Decl {decl_id; _}; smeta} as s when Map.mem var_names decl_id ->
        s :: mkread smeta (decl_id, Map.find_exn var_names decl_id)
    | s -> [s]
  in
  List.concat_map ~f:add_read_to_decl stmts |> List.concat_map ~f:flatten_slist

let gen_write (decl_id, sizedtype) =
  let bodyfn var =
    { stmt=
        NRFunApp (CompilerInternal, string_of_internal_fn FnWriteParam, [var])
    ; smeta= no_span }
  in
  for_scalar_inv sizedtype bodyfn
    { expr= Var decl_id
    ; emeta= {internal_meta with mtype= remove_size sizedtype} }
    no_span

let rec contains_var_expr is_vident accum {expr; _} =
  accum
  ||
  match expr with
  | Var v when is_vident v -> true
  | _ -> fold_expr (contains_var_expr is_vident) false expr

(* When a parameter's unconstrained type and its constrained type are different,
   we generate a new variable "<param_name>_in__" and read into that. We now need
   to change the FnConstrain calls to constrain that variable and assign to the
   actual <param_name> var.
*)
let constrain_in_params outvars stmts =
  let is_target_var = function
    | name, {out_unconstrained_st; out_constrained_st; out_block= Parameters; _}
      when not (out_unconstrained_st = out_constrained_st) ->
        Some name
    | _ -> None
  in
  let target_vars =
    List.filter_map outvars ~f:is_target_var |> String.Set.of_list
  in
  let rec change_constrain_target s =
    match s.stmt with
    | Assignment (_, {expr= FunApp (CompilerInternal, f, args); _})
      when ( internal_fn_of_string f = Some FnConstrain
           || internal_fn_of_string f = Some FnUnconstrain )
           && List.exists args
                ~f:(contains_var_expr (Set.mem target_vars) false) ->
        let rec change_var_expr e =
          match e.expr with
          | Var vident when Set.mem target_vars vident ->
              {e with expr= Var (vident ^ "_in__")}
          | _ -> {e with expr= map_expr change_var_expr e.expr}
        in
        let rec change_var_stmt s =
          {s with stmt= map_statement change_var_expr change_var_stmt s.stmt}
        in
        change_var_stmt s
    | _ -> {s with stmt= map_statement Fn.id change_constrain_target s.stmt}
  in
  List.map ~f:change_constrain_target stmts

let fn_name_map =
  String.Map.of_alist_exn [("integrate_ode", "integrate_ode_rk45")]

let rec map_fn_names s =
  let rec map_fn_names_expr e =
    let expr =
      match e.expr with
      | FunApp (k, f, a) when Map.mem fn_name_map f ->
          FunApp (k, Map.find_exn fn_name_map f, a)
      | expr -> map_expr map_fn_names_expr expr
    in
    {e with expr}
  in
  let stmt =
    match s.stmt with
    | NRFunApp (k, f, a) when Map.mem fn_name_map f ->
        NRFunApp (k, Map.find_exn fn_name_map f, a)
    | stmt -> map_statement map_fn_names_expr map_fn_names stmt
  in
  {s with stmt}

let rec insert_before f to_insert = function
  | [] -> to_insert
  | hd :: tl ->
      if f hd then to_insert @ (hd :: tl)
      else hd :: insert_before f to_insert tl

let is_opencl_var = String.is_suffix ~suffix:opencl_suffix

let rec collect_vars_expr is_target accum e =
  Set.union accum
    ( match e.expr with
    | Var s when is_target s -> String.Set.of_list [s]
    | x -> fold_expr (collect_vars_expr is_target) String.Set.empty x )

let collect_opencl_vars s =
  let rec go accum s =
    fold_statement (collect_vars_expr is_opencl_var) go accum s.stmt
  in
  go String.Set.empty s

let%expect_test "collect vars expr" =
  let mkvar s = {expr= Var s; emeta= internal_meta} in
  let args = List.map ~f:mkvar ["y"; "x_opencl__"; "z"; "w_opencl__"] in
  let fnapp = {expr= FunApp (StanLib, "print", args); emeta= internal_meta} in
  {stmt= TargetPE fnapp; smeta= no_span}
  |> collect_opencl_vars |> String.Set.sexp_of_t |> print_s ;
  [%expect {| (w_opencl__ x_opencl__) |}]

let%expect_test "insert before" =
  let l = [1; 2; 3; 4; 5; 6] |> insert_before (( = ) 6) [999] in
  [%sexp (l : int list)] |> print_s ;
  [%expect {| (1 2 3 4 5 999 6) |}]

let trans_prog (p : typed_prog) use_opencl =
  let p = map_prog Fn.id map_fn_names p in
  let init_pos =
    [ Decl {decl_adtype= DataOnly; decl_id= pos; decl_type= Sized SInt}
    ; Assignment ((pos, UInt, []), loop_bottom) ]
    |> List.map ~f:(fun stmt -> {stmt; smeta= no_span})
  in
  let log_prob = List.map ~f:add_jacobians p.log_prob in
  let get_pname_cst = function
    | name, {out_block= Parameters; out_constrained_st; _} ->
        Some (name, out_constrained_st)
    | _ -> None
  in
  let constrained_params = List.filter_map ~f:get_pname_cst p.output_vars in
  let param_writes, tparam_writes, gq_writes =
    List.map p.output_vars
      ~f:(fun (name, {out_constrained_st= st; out_block; _}) ->
        (out_block, gen_write (name, st)) )
    |> List.partition3_map ~f:(fun (b, x) ->
           match b with
           | Parameters -> `Fst x
           | TransformedParameters -> `Snd x
           | GeneratedQuantities -> `Trd x )
  in
  let tparam_start {stmt; _} =
    match stmt with
    | IfElse (cond, _, _)
      when contains_var_expr (( = ) "emit_transformed_parameters__") false cond
      ->
        true
    | _ -> false
  in
  let gq_start {stmt; _} =
    match stmt with
    | IfElse
        ( { expr= FunApp (_, _, [{expr= Var "emit_generated_quantities__"; _}]); _
          }
        , _
        , _ ) ->
        true
    | _ -> false
  in
  let translate_to_open_cl stmts =
    if use_opencl then
      let data_var_idents = List.map ~f:fst p.input_vars in
      let rec trans_stmt_to_opencl s =
        { s with
          stmt=
            map_statement
              (switch_expr_to_opencl data_var_idents)
              trans_stmt_to_opencl s.stmt }
      in
      List.map stmts ~f:trans_stmt_to_opencl
    else stmts
  in
  let gq =
    ( add_reads p.generate_quantities p.output_vars param_read
    |> translate_to_open_cl
    |> constrain_in_params p.output_vars
    |> insert_before tparam_start param_writes
    |> insert_before gq_start tparam_writes )
    @ gq_writes
  in
  let log_prob =
    add_reads log_prob p.output_vars param_read
    |> constrain_in_params p.output_vars
    |> translate_to_open_cl
  in
  let generate_quantities = gq in
  let opencl_vars =
    String.Set.union_list
      (List.concat_map
          ~f:(List.map ~f:collect_opencl_vars)
          [log_prob; generate_quantities])
    |> String.Set.to_list
  in
  let to_matrix_cl_stmts =
    List.concat_map opencl_vars ~f:(fun vident ->
        let vident_sans_opencl =
          String.chop_suffix_exn ~suffix:opencl_suffix vident
        in
        [ { stmt=
              Decl
                { decl_adtype= DataOnly
                ; decl_id= vident
                ; decl_type= Unsized UMatrix }
          ; smeta= no_span }
        ; { stmt=
              Assignment
                ( (vident, UMatrix, [])
                , to_matrix_cl
                    {expr= Var vident_sans_opencl; emeta= internal_meta} )
          ; smeta= no_span } ] )
  in
  let p =
    { p with
      log_prob
    ; prog_name= escape_name p.prog_name
    ; prepare_data=
        init_pos
        @ add_reads p.prepare_data p.input_vars data_read
        @ to_matrix_cl_stmts
    ; transform_inits=
        init_pos
        @ add_reads p.transform_inits constrained_params data_read
        @ List.map ~f:gen_write constrained_params
    ; generate_quantities }
  in
  map_prog Fn.id ensure_body_in_block p