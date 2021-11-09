open Middle

type t

val pp : Format.formatter -> t -> unit
val location : t -> Location_span.t

val mismatched_return_types :
  Location_span.t -> UnsizedType.returntype -> UnsizedType.returntype -> t

val mismatched_array_types :
  Location_span.t -> UnsizedType.t -> UnsizedType.t -> t

val invalid_row_vector_types : Location_span.t -> UnsizedType.t -> t
val invalid_matrix_types : Location_span.t -> UnsizedType.t -> t
val int_expected : Location_span.t -> string -> UnsizedType.t -> t
val int_or_real_expected : Location_span.t -> string -> UnsizedType.t -> t
val int_intarray_or_range_expected : Location_span.t -> UnsizedType.t -> t
val int_or_real_container_expected : Location_span.t -> UnsizedType.t -> t

val scalar_or_type_expected :
  Location_span.t -> string -> UnsizedType.t -> UnsizedType.t -> t

val array_vector_rowvector_matrix_expected :
  Location_span.t -> UnsizedType.t -> t

val illtyped_assignment :
  Location_span.t -> Operator.t -> UnsizedType.t -> UnsizedType.t -> t

val illtyped_ternary_if :
  Location_span.t -> UnsizedType.t -> UnsizedType.t -> UnsizedType.t -> t

val returning_fn_expected_nonreturning_found : Location_span.t -> string -> t
val returning_fn_expected_nonfn_found : Location_span.t -> string -> t

val returning_fn_expected_undeclaredident_found :
  Location_span.t -> string -> string option -> t

val returning_fn_expected_undeclared_dist_suffix_found :
  Location_span.t -> string * string -> t

val returning_fn_expected_wrong_dist_suffix_found :
  Location_span.t -> string * string -> t

val illtyped_reduce_sum :
     Location_span.t
  -> string
  -> UnsizedType.t list
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> SignatureMismatch.function_mismatch
  -> t

val illtyped_reduce_sum_generic :
     Location_span.t
  -> string
  -> UnsizedType.t list
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> SignatureMismatch.function_mismatch
  -> t

val illtyped_variadic_ode :
     Location_span.t
  -> string
  -> UnsizedType.t list
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> SignatureMismatch.function_mismatch
  -> t

val nonreturning_fn_expected_returning_found : Location_span.t -> string -> t
val nonreturning_fn_expected_nonfn_found : Location_span.t -> string -> t

val nonreturning_fn_expected_undeclaredident_found :
  Location_span.t -> string -> string option -> t

val illtyped_fn_app :
     Location_span.t
  -> string
  -> SignatureMismatch.signature_error list * bool
  -> UnsizedType.t list
  -> t

val illtyped_binary_op :
  Location_span.t -> Operator.t -> UnsizedType.t -> UnsizedType.t -> t

val illtyped_prefix_op : Location_span.t -> Operator.t -> UnsizedType.t -> t
val illtyped_postfix_op : Location_span.t -> Operator.t -> UnsizedType.t -> t
val not_indexable : Location_span.t -> UnsizedType.t -> int -> t
val ident_is_keyword : Location_span.t -> string -> t
val ident_is_model_name : Location_span.t -> string -> t
val ident_is_stanmath_name : Location_span.t -> string -> t
val ident_in_use : Location_span.t -> string -> t
val ident_not_in_scope : Location_span.t -> string -> string option -> t
val invalid_map_rect_fn : Location_span.t -> string -> t
val invalid_decl_rng_fn : Location_span.t -> t
val invalid_rng_fn : Location_span.t -> t
val invalid_unnormalized_fn : Location_span.t -> t
val udf_is_unnormalized_fn : Location_span.t -> string -> t
val ident_has_unnormalized_suffix : Location_span.t -> string -> t
val conditional_notation_not_allowed : Location_span.t -> t
val conditioning_required : Location_span.t -> t
val not_printable : Location_span.t -> t
val empty_array : Location_span.t -> t
val bad_int_literal : Location_span.t -> t
val cannot_assign_to_read_only : Location_span.t -> string -> t
val cannot_assign_to_global : Location_span.t -> string -> t
val invalid_sampling_pdf_or_pmf : Location_span.t -> t
val invalid_sampling_cdf_or_ccdf : Location_span.t -> string -> t
val invalid_sampling_no_such_dist : Location_span.t -> string -> t
val target_plusequals_outisde_model_or_logprob : Location_span.t -> t
val invalid_truncation_cdf_or_ccdf : Location_span.t -> t
val multivariate_truncation : Location_span.t -> t
val break_outside_loop : Location_span.t -> t
val continue_outside_loop : Location_span.t -> t
val expression_return_outside_returning_fn : Location_span.t -> t
val void_ouside_nonreturning_fn : Location_span.t -> t
val non_data_variable_size_decl : Location_span.t -> t
val non_int_bounds : Location_span.t -> t
val complex_transform : Location_span.t -> t
val transformed_params_int : Location_span.t -> t

val mismatched_fn_def_decl :
  Location_span.t -> string -> UnsizedType.t option -> t

val fn_decl_exists : Location_span.t -> string -> t
val fn_decl_without_def : Location_span.t -> t
val fn_decl_needs_block : Location_span.t -> t
val non_real_prob_fn_def : Location_span.t -> t

val prob_density_non_real_variate :
  Location_span.t -> UnsizedType.t option -> t

val prob_mass_non_int_variate : Location_span.t -> UnsizedType.t option -> t
val duplicate_arg_names : Location_span.t -> t
val incompatible_return_types : Location_span.t -> t
