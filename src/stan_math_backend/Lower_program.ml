open Core
open Core.Poly
open Middle
open Lower_expr
open Lower_stmt
open Lower_functions
open Cpp

let standalone_functions = ref false
let stanc_args_to_print = ref ""

let get_unconstrained_param_st lst =
  match lst with
  | _, _, {Program.out_block= Parameters; out_unconstrained_st= st; _} ->
      Some (SizedType.io_size st)
  | _ -> None

(** Create a variable for the name of model function.
  @param prog_name Name of the Stan program.
  @param fname Name of the function.
 *)
let gen_function__ prog_name fname =
  [ VariableDefn
      (make_variable_defn ~static:true ~constexpr:true
         ~type_:(Const (Pointer (TypeLiteral "char"))) ~name:"function__"
         ~init:
           (Assignment
              (Exprs.literal_string (prog_name ^ "_namespace::" ^ fname)))
         ()) ]
  @ Stmts.unused "function__"

(** Generate the private members of the model class

  This accounts for types that can be moved to OpenCL.
  @param vident name of the private member.
  @param ut The unsized type to print.
 *)
let lower_data_decl (vident, ut) : defn =
  let data_vident =
    if UnsizedType.is_eigen_type ut && not (Transform_Mir.is_opencl_var vident)
    then vident ^ "_data__"
    else vident in
  GlobalVariableDefn
    (lower_unsized_decl data_vident ut
       (UnsizedType.fill_adtype_for_type DataOnly ut))

(** Create maps of Eigen types*)
let lower_map_decl (vident, ut) : defn =
  let eigen_map_def t ndims =
    GlobalVariableDefn
      (make_variable_defn ~type_:(Types.eigen_map t) ~name:vident
         ~init:
           (InitializerList
              (Literal "nullptr" :: List.init ndims ~f:(fun _ -> Literal "0")))
         ()) in
  let scalar = local_scalar ut DataOnly in
  let open Types in
  match ut with
  | UMatrix -> eigen_map_def (matrix scalar) 2
  | URowVector -> eigen_map_def (row_vector scalar) 1
  | UVector -> eigen_map_def (vector scalar) 1
  | UComplexMatrix -> eigen_map_def (matrix (complex scalar)) 2
  | UComplexRowVector -> eigen_map_def (row_vector (complex scalar)) 1
  | UComplexVector -> eigen_map_def (vector (complex scalar)) 1
  | x ->
      Common.ICE.internal_compiler_error
        [%message
          "Error during Map data construction for " vident " of type "
            (x : UnsizedType.t)]

let rec top_level_decls Stmt.Fixed.{pattern; _} =
  match pattern with
  | Decl d when d.decl_id <> "pos__" ->
      [(d.decl_id, Type.to_unsized d.decl_type)]
  | SList stmts -> List.concat_map ~f:top_level_decls stmts
  | _ -> []

(** Generate the private data members of the model class *)
let lower_model_private {Program.prepare_data; _} =
  let data_decls = List.concat_map ~f:top_level_decls prepare_data in
  (*Filter out Any data that is not an Eigen matrix*)
  let get_eigen_map (name, ut) =
    UnsizedType.is_eigen_type ut && not (Transform_Mir.is_opencl_var name) in
  let eigen_map_decls = (List.filter ~f:get_eigen_map) data_decls in
  List.map ~f:lower_data_decl data_decls
  @ List.map ~f:lower_map_decl eigen_map_decls

let rec validate_dims ~stage name st =
  if String.is_suffix ~suffix:"__" name then []
  else if SizedType.contains_tuple st then
    (* We know tuples are given as flattened names containing "." in var_contexts *)
    let names =
      UnsizedType.enumerate_tuple_names_io name (SizedType.to_unsized st) in
    let subtypes = SizedType.flatten_tuple_io st in
    List.map2_exn ~f:(validate_dims ~stage) names subtypes |> List.concat
  else
    let open Cpp.DSL in
    let vector args =
      let cast x = Exprs.static_cast Types.size_t (lower_expr x) in
      let vec = Types.std_vector Types.size_t in
      InitializerExpr (vec, List.map ~f:cast args) in
    let context = Var "context__" in
    let validate =
      context.@?(( "validate_dims"
                 , [ literal_string stage
                   ; literal_string (Mangle.remove_prefix name)
                   ; literal_string
                       (Fmt.to_to_string Cpp.Printing.pp_type_
                          (stantype_prim (SizedType.to_unsized st)))
                   ; vector (SizedType.get_dims_io st) ] )) in
    [Expression validate]

let gen_assign_data decl_id st =
  let lower_placement_new decl_id st =
    let open Cpp.DSL in
    match st with
    | SizedType.SVector (_, d)
     |SRowVector (_, d)
     |SComplexVector d
     |SComplexRowVector d ->
        let data = Var (decl_id ^ "_data__") in
        [ Expression
            (OperatorNew
               ( decl_id
               , Types.eigen_map (lower_st st DataOnly)
               , [data.@!("data"); lower_expr d] )) ]
    | SMatrix (_, d1, d2) | SComplexMatrix (d1, d2) ->
        let data = Var (decl_id ^ "_data__") in
        [ Expression
            (OperatorNew
               ( decl_id
               , Types.eigen_map (lower_st st DataOnly)
               , [data.@!("data"); lower_expr d1; lower_expr d2] )) ]
    | _ -> [] in
  let underlying_variable decl_id st =
    match st with
    | SizedType.SVector _ | SRowVector _ | SMatrix _ | SComplexVector _
     |SComplexRowVector _ | SComplexMatrix _ ->
        decl_id ^ "_data__"
    | SInt | SReal | SComplex | SArray _ | STuple _ -> decl_id in
  Cpp.DSL.(
    underlying_variable decl_id st
    := initialize_value st
         (UnsizedType.fill_adtype_for_type UnsizedType.DataOnly
            (SizedType.to_unsized st)))
  :: lower_placement_new decl_id st

let lower_constructor
    {Program.prog_name; input_vars; prepare_data; output_vars; _} =
  let args =
    [ (Ref (TypeLiteral "stan::io::var_context"), "context__")
    ; (TypeLiteral "unsigned int", "random_seed__ = 0")
    ; (Pointer (TypeLiteral "std::ostream"), "pstream__ = nullptr") ] in
  let preamble =
    Decls.current_statement
    @ [ Using ("local_scalar_t__", Some Double)
      ; VariableDefn
          (make_variable_defn ~type_:(TypeLiteral "auto") ~name:"base_rng__"
             ~init:
               (Assignment
                  (Exprs.fun_call "stan::services::util::create_rng"
                     [Var "random_seed__"; Literal "0"]))
             ()) ]
    @ Stmts.unused "base_rng__"
    @ gen_function__ prog_name prog_name
    @ Decls.dummy_var in
  let data_idents = List.map ~f:fst3 input_vars |> String.Set.of_list in
  let lower_data (Stmt.Fixed.{pattern; meta} as s) =
    match pattern with
    | Decl {decl_id; decl_type; _} when decl_id <> "pos__" -> (
        match decl_type with
        | Sized st -> (
            Numbering.assign_loc meta
            @
            match Set.mem data_idents decl_id with
            | true ->
                validate_dims ~stage:"data initialization" decl_id st
                @ gen_assign_data decl_id st
            | false -> gen_assign_data decl_id st)
        | Unsized _ -> [])
    | _ -> lower_statement s in
  let data =
    Stmts.rethrow_located (List.concat_map ~f:lower_data prepare_data) in
  let set_num_params =
    let open Cpp.DSL in
    let output_params =
      List.filter_map ~f:get_unconstrained_param_st output_vars in
    match lower_exprs output_params |> List.reduce ~f:( + ) with
    | None -> "num_params_r__" := Literal "0U"
    | Some pars -> "num_params_r__" := pars in
  make_constructor ~args
    ~init_list:[("model_base_crtp", [Literal "0"])]
    ~body:(preamble @ data @ [set_num_params])
    ()

let gen_log_prob Program.{prog_name; log_prob; reverse_mode_log_prob; _} =
  let args : (type_ * string) list =
    [ (Ref (TemplateType "VecR"), "params_r__")
    ; (Ref (TemplateType "VecI"), "params_i__")
    ; (Pointer (TypeLiteral "std::ostream"), "pstream__ = nullptr") ] in
  (*
     NOTE: There is a bug in clang-6.0 where removing this T__ causes the
      reverse mode autodiff path to fail with an initializer list error
      for validate_array_expr_primitives on line 930. Need to investigate
      more into why this is happening
      *)
  let intro =
    let t__ = TypeLiteral "T__" in
    [ Using
        ("T__", Some (TypeTrait ("stan::scalar_type_t", [TemplateType "VecR"])))
    ; Using ("local_scalar_t__", Some t__)
    ; VariableDefn
        (make_variable_defn ~type_:t__ ~name:"lp__"
           ~init:(Construction [Literal "0.0"]) ()); Decls.lp_accum t__
    ; Decls.serializer_in ]
    @ Decls.current_statement @ Decls.dummy_var
    @ gen_function__ prog_name "log_prob" in
  let outro =
    let open Cpp.DSL in
    let lp_accum__ = Var "lp_accum__" in
    [ Expression lp_accum__.@?("add", [Var "lp__"])
    ; Return (Some lp_accum__.@!("sum")) ] in
  let template_params =
    [ Bool "propto__"; Bool "jacobian__"; Typename "VecR"; Typename "VecI"
    ; Require ("stan::require_vector_like_t", ["VecR"])
    ; Require ("stan::require_vector_like_vt", ["std::is_integral"; "VecI"]) ]
  in
  let template_nonrev =
    template_params @ [Require ("stan::require_not_st_var", ["VecR"])] in
  let template_rev =
    template_params @ [Require ("stan::require_st_var", ["VecR"])] in
  let gen_ll template lp_lst =
    FunDef
      (make_fun_defn ~templates_init:([template], true) ~inline:true
         ~return_type:(TypeTrait ("stan::scalar_type_t", [TemplateType "VecR"]))
         ~name:"log_prob_impl" ~args
         ~body:(intro @ Stmts.rethrow_located (lower_statements lp_lst) @ outro)
         ~cv_qualifiers:[Const] ()) in
  [ !//"Base log prob"; gen_ll template_nonrev log_prob
  ; !//"Reverse mode autodiff log prob"
  ; gen_ll template_rev reverse_mode_log_prob ]

let gen_write_array {Program.prog_name; generate_quantities; _} =
  let templates =
    [ Typename "RNG"; Typename "VecR"; Typename "VecI"; Typename "VecVar"
    ; Require
        ("stan::require_vector_like_vt", ["std::is_floating_point"; "VecR"])
    ; Require ("stan::require_vector_like_vt", ["std::is_integral"; "VecI"])
    ; Require ("stan::require_vector_vt", ["std::is_floating_point"; "VecVar"])
    ] in
  let args =
    [ (Ref (TemplateType "RNG"), "base_rng__")
    ; (Ref (TemplateType "VecR"), "params_r__")
    ; (Ref (TemplateType "VecI"), "params_i__")
    ; (Ref (TemplateType "VecVar"), "vars__")
    ; (Const Types.bool, "emit_transformed_parameters__ = true")
    ; (Const Types.bool, "emit_generated_quantities__ = true")
    ; (Pointer (TypeLiteral "std::ostream"), "pstream__ = nullptr") ] in
  let intro =
    [ Using ("local_scalar_t__", Some Double); Decls.serializer_in
    ; Decls.serializer_out
    ; VariableDefn
        (make_variable_defn ~static:true ~constexpr:true ~type_:Types.bool
           ~name:"propto__" ~init:(Assignment (Literal "true")) ()) ]
    @ Stmts.unused "propto__"
    @ VariableDefn
        (make_variable_defn ~type_:Double ~name:"lp__"
           ~init:(Assignment (Literal "0.0")) ())
      :: Stmts.unused "lp__"
    @ Decls.current_statement
    @ (Decls.lp_accum Double :: Decls.dummy_var)
    @ VariableDefn
        (make_variable_defn ~constexpr:true ~type_:Types.bool ~name:"jacobian__"
           ~init:(Assignment (Literal "false")) ())
      :: Stmts.unused "jacobian__"
    @ gen_function__ prog_name "write_array" in
  FunDef
    (make_fun_defn ~templates_init:([templates], true) ~inline:true
       ~return_type:Void ~name:"write_array_impl" ~args
       ~body:
         (intro @ Stmts.rethrow_located (lower_statements generate_quantities))
       ~cv_qualifiers:[Const] ())

let gen_transform_inits_impl {Program.transform_inits; output_vars; _} =
  let templates =
    [Typename "VecVar"; Require ("stan::require_vector_t", ["VecVar"])] in
  let args =
    [ (Types.const_ref (TypeLiteral "stan::io::var_context"), "context__")
    ; (Ref (TemplateType "VecVar"), "vars__")
    ; (Pointer (TypeLiteral "std::ostream"), "pstream__ = nullptr") ] in
  let intro =
    Using ("local_scalar_t__", Some Double)
    :: Decls.serializer_out :: Decls.current_statement
    @ Decls.dummy_var in
  let validate_params
      ( (name : string)
      , (loc : int)
      , (Program.{out_block; out_constrained_st; _} : 'a Program.outvar) ) =
    match out_block with
    | Parameters ->
        Some
          (Numbering.assign_loc loc
          @ validate_dims ~stage:"parameter initialization" name
              out_constrained_st)
    | _ -> None in
  let validation =
    List.filter_map ~f:validate_params output_vars |> List.concat in
  let read_inits = validation @ lower_statements transform_inits in
  FunDef
    (make_fun_defn ~templates_init:([templates], true) ~inline:true
       ~return_type:Void ~name:"transform_inits_impl" ~args
       ~body:(intro @ Stmts.rethrow_located read_inits)
       ~cv_qualifiers:[Const] ())

let gen_unconstrain_array_impl {Program.unconstrain_array; _} =
  let templates =
    [ Typename "VecVar"; Typename "VecI"
    ; Require ("stan::require_vector_t", ["VecVar"])
    ; Require ("stan::require_vector_like_vt", ["std::is_integral"; "VecI"]) ]
  in
  let args =
    [ (Types.const_ref (TemplateType "VecVar"), "params_r__")
    ; (Types.const_ref (TemplateType "VecI"), "params_i__")
    ; (Ref (TemplateType "VecVar"), "vars__")
    ; (Pointer (TypeLiteral "std::ostream"), "pstream__ = nullptr") ] in
  let intro =
    [ Using ("local_scalar_t__", Some Double); Decls.serializer_in
    ; Decls.serializer_out ]
    @ Decls.current_statement @ Decls.dummy_var in
  FunDef
    (make_fun_defn ~templates_init:([templates], true) ~inline:true
       ~return_type:Void ~name:"unconstrain_array_impl" ~args
       ~body:(intro @ Stmts.rethrow_located (lower_statements unconstrain_array))
       ~cv_qualifiers:[Const] ())

let gen_extend_vector name type_ elts =
  let open Cpp.DSL in
  let var = Var name in
  if List.is_empty elts then []
  else
    let temp = Var "temp" in
    [ VariableDefn
        (make_variable_defn ~type_ ~name:"temp" ~init:(InitializerList elts) ())
    ; Expression var.@?("reserve", [var.@!("size") + temp.@!("size")])
    ; Expression
        var.@?("insert", [var.@!("end"); temp.@!("begin"); temp.@!("end")]) ]

let gen_get_param_names {Program.output_vars; _} =
  let param_to_names name st =
    List.map
      ~f:(fun id -> Exprs.literal_string (Mangle.remove_prefix id))
      (UnsizedType.enumerate_tuple_names_io name (SizedType.to_unsized st))
  in
  let params, tparams, gqs =
    List.partition3_map output_vars ~f:(function
      | id, _, {Program.out_block= Parameters; out_constrained_st= st; _} ->
          `Fst (param_to_names id st)
      | id, _, {out_block= TransformedParameters; out_constrained_st= st; _} ->
          `Snd (param_to_names id st)
      | id, _, {out_block= GeneratedQuantities; out_constrained_st= st; _} ->
          `Trd (param_to_names id st)) in
  let names = "names__" in
  let args =
    [ (Ref (Types.std_vector Types.string), names)
    ; (Const Types.bool, "emit_transformed_parameters__ = true")
    ; (Const Types.bool, "emit_generated_quantities__ = true") ] in
  let open Cpp.DSL in
  let body =
    [names := Exprs.std_vector_init_expr Types.string (List.concat params)]
    @ [ IfElse
          ( Var "emit_transformed_parameters__"
          , Stmts.block
              (gen_extend_vector names
                 (Types.std_vector Types.string)
                 (List.concat tparams))
          , None )
      ; IfElse
          ( Var "emit_generated_quantities__"
          , Stmts.block
              (gen_extend_vector names
                 (Types.std_vector Types.string)
                 (List.concat gqs))
          , None ) ] in
  FunDef
    (make_fun_defn ~inline:true ~return_type:Void ~name:"get_param_names" ~args
       ~body ~cv_qualifiers:[Const] ())

let gen_get_dims {Program.output_vars; _} =
  (* NOTE: for tuples this is a mirror of how we give dims in var context.
      This won't generalize to ragged arrays, I don't think.

      We should probably deprecate get_dims and replace it with a
      new function later on which returns a more structured type
  *)
  let cast x = Exprs.static_cast Types.size_t (lower_expr x) in
  let pack inner_dims =
    List.map
      ~f:(fun x -> Exprs.std_vector_init_expr Types.size_t x)
      (List.map
         ~f:(fun dim -> SizedType.get_dims_io dim |> List.map ~f:cast)
         (SizedType.flatten_tuple_io inner_dims)) in
  let params, tparams, gqs =
    List.partition3_map output_vars ~f:(function
      | _, _, {Program.out_block= Parameters; Program.out_constrained_st= st; _}
        ->
          `Fst (pack st)
      | ( _
        , _
        , {out_block= TransformedParameters; Program.out_constrained_st= st; _}
        ) ->
          `Snd (pack st)
      | _, _, {out_block= GeneratedQuantities; Program.out_constrained_st= st; _}
        ->
          `Trd (pack st)) in
  let open Cpp.DSL in
  let dimss = "dimss__" in
  let args =
    [ (Ref (Types.std_vector (Types.std_vector Types.size_t)), dimss)
    ; (Const Types.bool, "emit_transformed_parameters__ = true")
    ; (Const Types.bool, "emit_generated_quantities__ = true") ] in
  let body =
    [ dimss :=
        Exprs.std_vector_init_expr
          (Types.std_vector Types.size_t)
          (List.concat params)
    ; IfElse
        ( Var "emit_transformed_parameters__"
        , Stmts.block
            (gen_extend_vector dimss
               (Types.std_vector ~dims:2 Types.size_t)
               (List.concat tparams))
        , None )
    ; IfElse
        ( Var "emit_generated_quantities__"
        , Stmts.block
            (gen_extend_vector dimss
               (Types.std_vector ~dims:2 Types.size_t)
               (List.concat gqs))
        , None ) ] in
  FunDef
    (make_fun_defn ~inline:true ~return_type:Void ~name:"get_dims" ~args ~body
       ~cv_qualifiers:[Const] ())

let rec gen_indexing_loop ?(index_ids = []) iteratee dims gen_body =
  let iter d gen_body =
    let loopvar, gensym_exit = Common.Gensym.enter () in
    let forloop =
      Stmts.fori loopvar
        (lower_expr Expr.Helpers.loop_bottom)
        d
        (Stmts.block @@ gen_body iteratee ((`Array, loopvar) :: index_ids))
    in
    gensym_exit ();
    forloop in
  match dims with
  | [] -> gen_body iteratee index_ids
  | dim :: dims ->
      [ iter dim (fun i idcs ->
            gen_indexing_loop ~index_ids:idcs i dims gen_body) ]

let emplace_name_stmt name idcs =
  let null_string = Constructor (Types.string, []) in
  let sep = function `Array -> Literal "'.'" | `Tuple -> Literal "':'" in
  let to_string e =
    match e with Literal _ -> e | _ -> Exprs.fun_call "std::to_string" [e] in
  let open Cpp.DSL in
  let param_names__ = Var "param_names__" in
  Expression
    param_names__.@?(( "emplace_back"
                     , [ null_string
                         + List.fold ~init:(literal_string name)
                             ~f:(fun acc (typ, idx) ->
                               acc + sep typ + to_string idx)
                             idcs ] ))

let rec gen_param_names ?(outer_idcs = []) (decl_id, st) =
  let gen_name name idcs =
    let idcs = outer_idcs @ idcs in
    let idcs_vars = List.map ~f:(fun (t, i) -> (t, Exprs.to_var i)) idcs in
    match st with
    | SizedType.STuple subtypes ->
        let idxes_subtypes =
          List.mapi
            ~f:(fun i typ -> ((`Tuple, string_of_int (i + 1)), (name, typ)))
            subtypes in
        List.concat_map
          ~f:(fun (idx, sub) -> gen_param_names ~outer_idcs:(idcs @ [idx]) sub)
          idxes_subtypes
    (* same name, different idcs going forward. would also cover the above case but generate worse code? *)
    | SizedType.SArray _ when SizedType.contains_tuple st ->
        gen_param_names ~outer_idcs:idcs
          (name, fst (SizedType.get_array_dims st))
    | _ when SizedType.is_complex_type st ->
        [ emplace_name_stmt name
            (idcs_vars @ [(`Array, Exprs.literal_string "real")])
        ; emplace_name_stmt name
            (idcs_vars @ [(`Array, Exprs.literal_string "imag")]) ]
    | _ ->
        let name = Mangle.remove_prefix name in
        [emplace_name_stmt name idcs_vars] in
  let dims = lower_exprs (List.rev (SizedType.get_dims st)) in
  gen_indexing_loop decl_id dims gen_name

let gen_param_names_fn name (paramvars, tparamvars, gqvars) =
  let args =
    [ (Ref (Types.std_vector Types.string), "param_names__")
    ; (Types.bool, "emit_transformed_parameters__ = true")
    ; (Types.bool, "emit_generated_quantities__ = true") ] in
  let body =
    List.concat_map ~f:gen_param_names paramvars
    @ [ IfElse
          ( Var "emit_transformed_parameters__"
          , Stmts.block (List.concat_map ~f:gen_param_names tparamvars)
          , None )
      ; IfElse
          ( Var "emit_generated_quantities__"
          , Stmts.block (List.concat_map ~f:gen_param_names gqvars)
          , None ) ] in
  FunDef
    (make_fun_defn ~inline:true ~return_type:Void ~name ~args ~body
       ~cv_qualifiers:[Const; Final] ())

let gen_constrained_param_names {Program.output_vars; _} =
  gen_param_names_fn "constrained_param_names"
    (List.partition3_map
       ~f:(function
         | id, _, {Program.out_block= Parameters; out_constrained_st= st; _} ->
             `Fst (id, st)
         | id, _, {out_block= TransformedParameters; out_constrained_st= st; _}
           ->
             `Snd (id, st)
         | id, _, {out_block= GeneratedQuantities; out_constrained_st= st; _} ->
             `Trd (id, st))
       output_vars)

let gen_unconstrained_param_names {Program.output_vars; _} =
  gen_param_names_fn "unconstrained_param_names"
    (List.partition3_map
       ~f:(function
         | id, _, {Program.out_block= Parameters; out_unconstrained_st= st; _}
           ->
             `Fst (id, st)
         | id, _, {out_block= TransformedParameters; out_unconstrained_st= st; _}
           ->
             `Snd (id, st)
         | id, _, {out_block= GeneratedQuantities; out_unconstrained_st= st; _}
           ->
             `Trd (id, st))
       output_vars)

(** Create constrained and unconstrained sizedtype methods
 in the model class
 @param name The name of the method to wrap the body in.
 @param outvars The parameters to gather the sizes for.
 *)
let gen_outvar_metadata name outvars =
  let open Cpp.DSL in
  let json_str = Cpp_Json.out_var_interpolated_json_str outvars in
  FunDef
    (make_fun_defn ~inline:true ~return_type:Types.string ~name
       ~body:[Return (Some Types.string.:{Literal json_str})]
       ~cv_qualifiers:[Const] ())

(** Print the [get_unconstrained_sizedtypes] method of the model class *)
let gen_unconstrained_types {Program.output_vars; _} =
  let grab_unconstrained (name, _, {Program.out_unconstrained_st; out_block; _})
      =
    (name, out_unconstrained_st, out_block) in
  let outvars = List.map ~f:grab_unconstrained output_vars in
  gen_outvar_metadata "get_unconstrained_sizedtypes" outvars

(** Print the [get_constrained_sizedtypes] method of the model class *)
let gen_constrained_types {Program.output_vars; _} =
  let grab_constrained (name, _, {Program.out_constrained_st; out_block; _}) =
    (name, out_constrained_st, out_block) in
  let outvars = List.map ~f:grab_constrained output_vars in
  gen_outvar_metadata "get_constrained_sizedtypes" outvars

(** The generic method overloads needed in the model class. *)
let gen_overloads {Program.output_vars; _} =
  let pstream = (Pointer (TypeLiteral "std::ostream"), "pstream = nullptr") in
  let open Cpp.DSL in
  let write_arrays =
    let templates_init = ([[Typename "RNG"]], false) in
    let emit_flags const =
      let t : type_ = if const then Const Types.bool else Types.bool in
      [ (t, "emit_transformed_parameters = true")
      ; (t, "emit_generated_quantities = true") ] in
    let sizes =
      (* An expression for the number of individual parameters in a list of output variables *)
      let num_outvars (outvars : Expr.Typed.t Program.outvar list) =
        Expr.Helpers.binop_list
          (List.map
             ~f:(fun outvar ->
               SizedType.io_size outvar.Program.out_constrained_st)
             outvars)
          Operator.Plus ~default:Expr.Helpers.zero
        |> lower_expr in
      (* The list of output variables that came from a particular block *)
      let block_outvars (block : Program.io_block) =
        List.filter_map output_vars
          ~f:(fun ((_ : string), _, (outvar : Expr.Typed.t Program.outvar)) ->
            if outvar.out_block = block then Some outvar else None) in
      let num_params = num_outvars (block_outvars Parameters) in
      let num_transformed = num_outvars (block_outvars TransformedParameters) in
      let num_gen_quantities = num_outvars (block_outvars GeneratedQuantities) in
      [ VariableDefn
          (make_variable_defn ~type_:(Const Types.size_t) ~name:"num_params__"
             ~init:(Assignment num_params) ())
      ; VariableDefn
          (make_variable_defn ~type_:(Const Types.size_t)
             ~name:"num_transformed"
             ~init:
               (Assignment
                  (Var "emit_transformed_parameters" * Parens num_transformed))
             ())
      ; VariableDefn
          (make_variable_defn ~type_:(Const Types.size_t)
             ~name:"num_gen_quantities"
             ~init:
               (Assignment
                  (Var "emit_generated_quantities" * Parens num_gen_quantities))
             ())
      ; VariableDefn
          (make_variable_defn ~type_:(Const Types.size_t) ~name:"num_to_write"
             ~init:
               (Assignment
                  (Var "num_params__" + Var "num_transformed"
                 + Var "num_gen_quantities"))
             ()) ] in
    let call_impl =
      Expression
        (Exprs.fun_call "write_array_impl"
           [ Var "base_rng"; Var "params_r"; Var "params_i"; Var "vars"
           ; Var "emit_transformed_parameters"; Var "emit_generated_quantities"
           ; Var "pstream" ]) in
    [ FunDef
        (make_fun_defn ~templates_init ~inline:true ~return_type:Void
           ~name:"write_array"
           ~args:
             ([ (Ref (TemplateType "RNG"), "base_rng")
              ; (Ref (Types.vector Double), "params_r")
              ; (Ref (Types.vector Double), "vars") ]
             @ emit_flags true @ [pstream])
           ~body:
             (sizes
             @ [ VariableDefn
                   (make_variable_defn ~type_:(Types.std_vector Int)
                      ~name:"params_i" ())
               ; "vars" :=
                   Types.vector Double
                   |::? ("Constant", [Var "num_to_write"; Exprs.quiet_NaN])
               ; call_impl ])
           ~cv_qualifiers:[Const] ())
    ; FunDef
        (make_fun_defn ~templates_init ~inline:true ~return_type:Void
           ~name:"write_array"
           ~args:
             ([ (Ref (TemplateType "RNG"), "base_rng")
              ; (Ref (Types.std_vector Double), "params_r")
              ; (Ref (Types.std_vector Int), "params_i")
              ; (Ref (Types.std_vector Double), "vars") ]
             @ emit_flags false @ [pstream])
           ~body:
             (sizes
             @ [ "vars" :=
                   (Types.std_vector Double).:{Var "num_to_write";
                   Exprs.quiet_NaN}; call_impl ])
           ~cv_qualifiers:[Const] ()) ] in
  let log_probs =
    let templates_init =
      ([[Bool "propto__"; Bool "jacobian__"; Typename "T_"]], false) in
    let call_impl =
      Return
        (Some
           (Exprs.templated_fun_call "log_prob_impl"
              [TemplateType "propto__"; TemplateType "jacobian__"]
              [Var "params_r"; Var "params_i"; Var "pstream"])) in
    [ FunDef
        (make_fun_defn ~templates_init ~inline:true
           ~return_type:(TemplateType "T_") ~name:"log_prob"
           ~args:[(Ref (Types.vector (TemplateType "T_")), "params_r"); pstream]
           ~body:
             [ VariableDefn
                 (make_variable_defn ~type_:(Types.vector Int) ~name:"params_i"
                    ()); call_impl ]
           ~cv_qualifiers:[Const] ())
    ; FunDef
        (make_fun_defn ~templates_init ~inline:true
           ~return_type:(TemplateType "T_") ~name:"log_prob"
           ~args:
             [ (Ref (Types.std_vector (TemplateType "T_")), "params_r")
             ; (Ref (Types.std_vector Int), "params_i"); pstream ]
           ~body:[call_impl] ~cv_qualifiers:[Const] ()) ] in
  let transform_inits =
    let open Cpp.DSL in
    let params_r_vec = Var "params_r_vec" in
    let eigen_map_double = Types.eigen_map (Types.vector Double) in
    [ FunDef
        (make_fun_defn ~inline:true ~return_type:Void ~name:"transform_inits"
           ~args:
             [ (Types.const_ref (TypeLiteral "stan::io::var_context"), "context")
             ; (Ref (Types.vector Double), "params_r"); pstream ]
           ~body:
             [ VariableDefn
                 (make_variable_defn ~type_:(Types.std_vector Double)
                    ~name:"params_r_vec"
                    ~init:
                      (Construction
                         [Exprs.method_call (Var "params_r") "size" [] []])
                    ())
             ; VariableDefn
                 (make_variable_defn ~type_:(Types.std_vector Int)
                    ~name:"params_i" ())
             ; Expression
                 (Exprs.fun_call "transform_inits"
                    [Var "context"; Var "params_i"; params_r_vec; Var "pstream"])
             ; "params_r" :=
                 eigen_map_double.:{params_r_vec.@!("data");
                 params_r_vec.@!("size")} ]
           ~cv_qualifiers:[Const; Final] ())
    ; (let args =
         [ (Types.const_ref (TypeLiteral "stan::io::var_context"), "context")
         ; (Ref (Types.std_vector Int), "params_i")
         ; (Ref (Types.std_vector Double), "vars")
         ; (Pointer (TypeLiteral "std::ostream"), "pstream__ = nullptr") ] in
       let body =
         let open Cpp.DSL in
         [ Expression (Var "vars").@?("resize", [Var "num_params_r__"])
         ; Expression
             (Exprs.fun_call "transform_inits_impl"
                [Var "context"; Var "vars"; Var "pstream__"]) ] in
       FunDef
         (make_fun_defn ~inline:true ~return_type:Void ~name:"transform_inits"
            ~args ~body ~cv_qualifiers:[Const] ())) ] in
  let unconstrain_array =
    let open Cpp.DSL in
    let call_impl =
      Expression
        (Exprs.fun_call "unconstrain_array_impl"
           [ Var "params_constrained"; Var "params_i"; Var "params_unconstrained"
           ; Var "pstream" ]) in
    [ FunDef
        (make_fun_defn ~inline:true ~return_type:Void ~name:"unconstrain_array"
           ~args:
             [ (Types.const_ref (Types.std_vector Double), "params_constrained")
             ; (Ref (Types.std_vector Double), "params_unconstrained"); pstream
             ]
           ~body:
             [ VariableDefn
                 (make_variable_defn
                    ~type_:Types.(Const (std_vector Int))
                    ~name:"params_i" ())
             ; "params_unconstrained" :=
                 (Types.std_vector Double).:{Var "num_params_r__"; quiet_NaN}
             ; call_impl ]
           ~cv_qualifiers:[Const (*; Final*)] ())
    ; FunDef
        (make_fun_defn ~inline:true ~return_type:Void ~name:"unconstrain_array"
           ~args:
             [ (Types.const_ref (Types.vector Double), "params_constrained")
             ; (Ref (Types.vector Double), "params_unconstrained"); pstream ]
           ~body:
             [ VariableDefn
                 (make_variable_defn
                    ~type_:Types.(Const (std_vector Int))
                    ~name:"params_i" ())
             ; "params_unconstrained" :=
                 Types.vector Double
                 |::? ("Constant", [Var "num_params_r__"; Exprs.quiet_NaN])
             ; call_impl ]
           ~cv_qualifiers:[Const (*; Final*)] ()) ] in
  (!//"Begin method overload boilerplate" :: write_arrays)
  @ log_probs @ transform_inits @ unconstrain_array

let lower_model_public p =
  gen_log_prob p
  @ [ gen_write_array p; gen_unconstrain_array_impl p; gen_transform_inits_impl p
    ; (* Begin metadata methods *) gen_get_param_names p
    ; (* Post-data metadata methods *) gen_get_dims p
    ; gen_constrained_param_names p; gen_unconstrained_param_names p
    ; gen_constrained_types p; gen_unconstrained_types p ]
  (* Boilerplate *)
  @ gen_overloads p

let model_public_basics name =
  let version_string = "stanc_version = %%NAME%%3 %%VERSION%%" in
  [ FunDef
      (make_fun_defn ~inline:true ~return_type:Types.string ~name:"model_name"
         ~cv_qualifiers:[Const; Final]
         ~body:[Return (Some (Exprs.literal_string name))]
         ())
  ; FunDef
      (make_fun_defn ~inline:true
         ~return_type:(Types.std_vector Types.string)
         ~name:"model_compile_info"
         ~body:
           [ Return
               (Some
                  (Exprs.std_vector_init_expr Types.string
                     [ Exprs.literal_string version_string
                     ; Exprs.literal_string
                         ("stancflags = " ^ !stanc_args_to_print) ])) ]
         ~cv_qualifiers:[Const; NoExcept] ()) ]

let lower_model ({Program.prog_name; _} as p) =
  let private_members = lower_model_private p in
  let public_members = model_public_basics prog_name @ lower_model_public p in
  let constructor = lower_constructor p in
  Class
    (make_class_defn ~name:prog_name ~final:true
       ~public_base:(TypeTrait ("model_base_crtp", [TypeLiteral prog_name]))
       ~private_members ~public_members ~constructor ())

(** Create the model's namespace. *)
let namespace Program.{prog_name; _} = prog_name ^ "_namespace"

let usings =
  [ GlobalUsing ("stan::model::model_base_crtp", None)
  ; GlobalUsing ("namespace stan::math", None) ]

(** Model boilerplate. *)
let new_model_boilerplate prog_name =
  let new_model =
    let args =
      [ (Ref (TypeLiteral "stan::io::var_context"), "data_context")
      ; (TypeLiteral "unsigned int", "seed")
      ; (Pointer (TypeLiteral "std::ostream"), "msg_stream") ] in
    let body =
      [ VariableDefn
          (make_variable_defn ~type_:(Pointer (TypeLiteral "stan_model"))
             ~name:"m"
             ~init:
               (Assignment
                  (AllocNew
                     ( TypeLiteral "stan_model"
                     , [Var "data_context"; Var "seed"; Var "msg_stream"] )))
             ()); Return (Some (Literal "*m")) ] in
    FunDef
      (make_fun_defn ~name:"new_model"
         ~return_type:(Ref (TypeLiteral "stan::model::model_base")) ~args ~body
         ()) in
  let profile_data =
    FunDef
      (make_fun_defn ~name:"get_stan_profile_data"
         ~return_type:(Ref (TypeLiteral "stan::math::profile_map"))
         ~body:[Return (Some (Literal (prog_name ^ "_namespace::profiles__")))]
         ()) in
  [ GlobalUsing
      ("stan_model", Some (TypeLiteral (prog_name ^ "_namespace::" ^ prog_name)))
  ; Preprocessor
      (IfNDef ("USING_R", [!//"Boilerplate"; new_model; profile_data])) ]

(* Top-level directives. The fwd and mix headers are only included if they are necessary,
   which at the moment is only if the embedded laplace functions are used. *)
let version = !//"Code generated by %%NAME%% %%VERSION%%"
let model_header_include = Preprocessor (Include "stan/model/model_header.hpp")
let math_mix_include = Preprocessor (Include "stan/math/mix.hpp")

let lower_program ?printed_filename (p : Program.Typed.t) : Cpp.program =
  (* First, do some transformations on the MIR itself before we begin printing it.*)
  let p, s, map_rect_calls, needs_mix_header = Numbering.prepare_prog p in
  let model_namespace_str = namespace p in
  let model_contents =
    usings
    @ Numbering.gen_globals ?printed_filename s
    @ collect_functors_functions p
    @ if !standalone_functions then [] else [lower_model p] in
  let model_namespace = Namespace (model_namespace_str, model_contents) in
  let includes =
    if needs_mix_header then
      [ !//"Including the mix header for embedded laplace usage"
        (* TODO should this be a #define and subsumed into the model header? A second model header? *)
      ; math_mix_include; model_header_include ]
    else [model_header_include] in
  let global_fns =
    if !standalone_functions then
      List.concat_map
        ~f:(lower_standalone_fun_def model_namespace_str)
        p.functions_block
    else
      new_model_boilerplate p.prog_name
      @ Numbering.register_map_rect_functors model_namespace_str map_rect_calls
  in
  (version :: includes) @ (model_namespace :: global_fns)

module Testing = struct
  open Fmt

  let%expect_test "model public basics" =
    model_public_basics "foobar"
    |> str "%a" Cpp.Printing.pp_program
    |> print_endline;
    [%expect
      {|
      inline std::string model_name() const final {
        return "foobar";
      }
      inline std::vector<std::string> model_compile_info() const noexcept {
        return std::vector<std::string>{"stanc_version = %%NAME%%3 %%VERSION%%",
                 "stancflags = "};
      } |}]

  let%expect_test "boilerplate" =
    new_model_boilerplate "foobar"
    |> str "%a" Cpp.Printing.pp_program
    |> print_endline;
    [%expect
      {|
        using stan_model = foobar_namespace::foobar;
        #ifndef USING_R
        // Boilerplate
        stan::model::model_base&
        new_model(stan::io::var_context& data_context, unsigned int seed,
                  std::ostream* msg_stream) {
          stan_model* m = new stan_model(data_context, seed, msg_stream);
          return *m;
        }
        stan::math::profile_map& get_stan_profile_data() {
          return foobar_namespace::profiles__;
        }
        #endif |}]

  let%expect_test "complex names" =
    gen_param_names
      ("foo", SizedType.SArray (SComplex, Middle.Expr.Helpers.variable "N"))
    |> str "@[<v>%a" (list ~sep:cut Cpp.Printing.pp_stmt)
    |> print_endline;
    [%expect
      {|
          for (int sym1__ = 1; sym1__ <= N; ++sym1__) {
            param_names__.emplace_back(std::string() + "foo" + '.' +
              std::to_string(sym1__) + '.' + "real");
            param_names__.emplace_back(std::string() + "foo" + '.' +
              std::to_string(sym1__) + '.' + "imag");
          } |}]

  let%expect_test "tuple names" =
    gen_param_names
      ( "tuple"
      , SizedType.(
          STuple [SInt; SArray (SReal, Middle.Expr.Helpers.variable "nested")])
      )
    |> str "@[<v>%a" (list ~sep:cut Cpp.Printing.pp_stmt)
    |> print_endline;
    [%expect
      {|
      param_names__.emplace_back(std::string() + "tuple" + ':' + std::to_string(1));
      for (int sym1__ = 1; sym1__ <= nested; ++sym1__) {
        param_names__.emplace_back(std::string() + "tuple" + ':' +
          std::to_string(2) + '.' + std::to_string(sym1__));
      } |}]

  let%expect_test "array of tuple names" =
    gen_param_names
      ( "arr_tuple"
      , SizedType.(
          SArray
            ( STuple
                [SInt; SArray (SReal, Middle.Expr.Helpers.variable "nested")]
            , Middle.Expr.Helpers.variable "N" )) )
    |> str "@[<v>%a" (list ~sep:cut Cpp.Printing.pp_stmt)
    |> print_endline;
    [%expect
      {|
        for (int sym1__ = 1; sym1__ <= N; ++sym1__) {
          param_names__.emplace_back(std::string() + "arr_tuple" + '.' +
            std::to_string(sym1__) + ':' + std::to_string(1));
          for (int sym2__ = 1; sym2__ <= nested; ++sym2__) {
            param_names__.emplace_back(std::string() + "arr_tuple" + '.' +
              std::to_string(sym1__) + ':' + std::to_string(2) + '.' +
              std::to_string(sym2__));
          }
        } |}]
end
