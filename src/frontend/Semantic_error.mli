open Middle

type t

val pp : Format.formatter -> t -> unit
val location : t -> location_span

val mismatched_argument_types :
  location_span -> unsizedtype -> unsizedtype -> t

val mismatched_return_types : location_span -> returntype -> returntype -> t
val mismatched_array_types : location_span -> t
val invalid_row_vector_types : location_span -> t
val int_expected : location_span -> string -> unsizedtype -> t
val int_or_real_expected : location_span -> string -> unsizedtype -> t
val int_intarray_or_range_expected : location_span -> unsizedtype -> t
val int_or_real_container_expected : location_span -> unsizedtype -> t
val array_vector_rowvector_matrix_expected : location_span -> unsizedtype -> t

val illtyped_assignment :
  location_span -> Ast.assignmentoperator -> unsizedtype -> unsizedtype -> t

val illtyped_ternary_if :
  location_span -> unsizedtype -> unsizedtype -> unsizedtype -> t

val returning_fn_expected_nonreturning_found : location_span -> string -> t
val returning_fn_expected_nonfn_found : location_span -> string -> t
val returning_fn_expected_undeclaredident_found : location_span -> string -> t
val nonreturning_fn_expected_returning_found : location_span -> string -> t
val nonreturning_fn_expected_nonfn_found : location_span -> string -> t

val nonreturning_fn_expected_undeclaredident_found :
  location_span -> string -> t

val illtyped_stanlib_fn_app : location_span -> string -> unsizedtype list -> t

val illtyped_userdefined_fn_app :
     location_span
  -> string
  -> (autodifftype * unsizedtype) list
  -> returntype
  -> unsizedtype list
  -> t

val illtyped_binary_op :
  location_span -> operator -> unsizedtype -> unsizedtype -> t

val illtyped_prefix_op : location_span -> operator -> unsizedtype -> t
val illtyped_postfix_op : location_span -> operator -> unsizedtype -> t
val not_indexable : location_span -> unsizedtype -> t
val ident_is_keyword : location_span -> string -> t
val ident_is_model_name : location_span -> string -> t
val ident_is_stanmath_name : location_span -> string -> t
val ident_in_use : location_span -> string -> t
val ident_not_in_scope : location_span -> string -> t
val invalid_map_rect_fn : location_span -> string -> t
val invalid_rng_fn : location_span -> t
val conditional_notation_not_allowed : location_span -> t
val conditioning_required : location_span -> t
val not_printable : location_span -> t
val empty_array : location_span -> t
val cannot_assign_to_read_only : location_span -> string -> t
val cannot_assign_to_global : location_span -> string -> t
val invalid_sampling_pdf_or_pmf : location_span -> t
val invalid_sampling_cdf_or_ccdf : location_span -> string -> t
val invalid_sampling_no_such_dist : location_span -> string -> t
val target_plusequals_outisde_model_or_logprob : location_span -> t
val invalid_truncation_cdf_or_ccdf : location_span -> t
val break_outside_loop : location_span -> t
val continue_outside_loop : location_span -> t
val expression_return_outside_returning_fn : location_span -> t
val void_ouside_nonreturning_fn : location_span -> t
val non_data_variable_size_decl : location_span -> t
val non_int_bounds : location_span -> t
val transformed_params_int : location_span -> t
val mismatched_fn_def_decl : location_span -> string -> unsizedtype option -> t
val fn_decl_exists : location_span -> string -> t
val fn_decl_without_def : location_span -> t
val non_real_prob_fn_def : location_span -> t
val prob_density_non_real_variate : location_span -> unsizedtype option -> t
val prob_mass_non_int_variate : location_span -> unsizedtype option -> t
val duplicate_arg_names : location_span -> t
val incompatible_return_types : location_span -> t
