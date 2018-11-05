 ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
    for_all_vector_types  ( fun v -> 
         add_plain ("student_t_rng",  (rng_return_type) (t, u, v), t, u, v) 
    ) 
  ) 
 ) 
  ; add_plain ("sub_col",  ((ReturnType Vector)),  ((ReturnType Matrix)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("sub_row",  ((ReturnType RowVector)),  ((ReturnType Matrix)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("subtract",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("subtract",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("subtract",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("subtract",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Real))) 
  ; add_plain ("subtract",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType Real))) 
  ; add_plain ("subtract",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Real))) 
  ; add_plain ("subtract",  ((ReturnType Vector)),  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("subtract",  ((ReturnType RowVector)),  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("subtract",  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("sum",  (ReturnType Int),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("sum",  ((ReturnType Real)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("sum",  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("sum",  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("sum",  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("tail",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  (ReturnType Int)) 
  ; add_plain ("tail",  ((ReturnType Vector)),  ((ReturnType Vector)),  (ReturnType Int)) 
; for i = 0 to bare_types_size-1 do    (
     add_plain ("tail",  (bare_array_type (bare_types i , 1)),
       (bare_array_type (bare_types i , 1)),  (ReturnType Int)) 
    ; add_plain ("tail",  (bare_array_type (bare_types i , 2)),
       (bare_array_type (bare_types i , 2)),  (ReturnType Int)) 
    ; add_plain ("tail",  (bare_array_type (bare_types i , 3)),
       (bare_array_type (bare_types i , 3)),  (ReturnType Int)) 
 )done
  ; add_unary_vectorized ("tan") 
  ; add_unary_vectorized ("tanh") 
  (* ; add_nullary ("target") *)
  ; add_plain ("tcrossprod",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_unary_vectorized ("tgamma") 
  ; add_plain ("to_array_1d",  (bare_array_type ((ReturnType Real), 1)),  ((ReturnType Matrix))) 
  ; add_plain ("to_array_1d",  (bare_array_type ((ReturnType Real), 1)),  ((ReturnType Vector))) 
  ; add_plain ("to_array_1d",  (bare_array_type ((ReturnType Real), 1)),  ((ReturnType RowVector))) 
; for   i=1 to 10-1 do  (
     add_plain ("to_array_1d",  (bare_array_type ((ReturnType Real), 1)),
       (bare_array_type (bare_array_type ((ReturnType Real), i)))) 
    ; add_plain ("to_array_1d",  (bare_array_type (ReturnType Int, 1)),  (bare_array_type (bare_array_type (ReturnType Int, i)))) 
 )done
  ; add_plain ("to_array_2d",  (bare_array_type ((ReturnType Real), 2)),  ((ReturnType Matrix))) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType Vector)),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType Vector)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType RowVector))) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType RowVector)),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType RowVector)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  (bare_array_type ((ReturnType Real), 1)),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  (bare_array_type ((ReturnType Real), 1)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  (bare_array_type (ReturnType Int, 1)),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  (bare_array_type (ReturnType Int, 1)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  (bare_array_type ((ReturnType Real), 2))) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  (bare_array_type (ReturnType Int, 2))) 
  ; add_plain ("to_row_vector",  ((ReturnType RowVector)),  ((ReturnType Matrix))) 
  ; add_plain ("to_row_vector",  ((ReturnType RowVector)),  ((ReturnType Vector))) 
  ; add_plain ("to_row_vector",  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("to_row_vector",  ((ReturnType RowVector)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("to_row_vector",  ((ReturnType RowVector)),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("to_vector",  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_plain ("to_vector",  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("to_vector",  ((ReturnType Vector)),  ((ReturnType RowVector))) 
  ; add_plain ("to_vector",  ((ReturnType Vector)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("to_vector",  ((ReturnType Vector)),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("trace",  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("trace_gen_quad_; form",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("trace_quad_; form",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("trace_quad_; form",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("transpose",  ((ReturnType RowVector)),  ((ReturnType Vector))) 
  ; add_plain ("transpose",  ((ReturnType Vector)),  ((ReturnType RowVector))) 
  ; add_plain ("transpose",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_unary_vectorized ("trunc") 
  ; add_unary_vectorized ("trigamma") 
; for i = 0 to vector_types_size-1 do    (
   for j = 0 to vector_types_size-1 do    (
     for k = 0 to vector_types_size-1 do    (
         add_plain ("uni; form_ccdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("uni; form_cdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("uni; form_cdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("uni; form_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("uni; form_lccdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("uni; form_lcdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("uni; form_lpdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
    )done
  )done
 )done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("uni; form_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
  ; add_plain ("variance",  ((ReturnType Real)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("variance",  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("variance",  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("variance",  ((ReturnType Real)),  ((ReturnType Matrix))) 
; for i = 0 to vector_types_size-1 do    (
   for j = 0 to vector_types_size-1 do    (
     for k = 0 to vector_types_size-1 do    (
         add_plain ("von_mises_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("von_mises_lpdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
    )done
  )done
)done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("von_mises_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
; for i = 0 to vector_types_size-1 do    (
   for j = 0 to vector_types_size-1 do    (
     for k = 0 to vector_types_size-1 do    (
         add_plain ("weibull_ccdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("weibull_cdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("weibull_cdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("weibull_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("weibull_lccdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("weibull_lcdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("weibull_lpdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
    )done
  )done
 )done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
      add_plain ("weibull_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
; for i = 0 to vector_types_size-1 do    (
   for j = 0 to vector_types_size-1 do    (
     for k = 0 to vector_types_size-1 do    (
       for l = 0 to vector_types_size-1 do    (
         for m = 0 to vector_types_size-1 do    (
             add_plain ("wiener_log",  ((ReturnType Real)), vector_types i ,
              vector_types j ,vector_types k , vector_types l ,
              vector_types m ) 
            ; add_plain ("wiener_lpdf",  ((ReturnType Real)), vector_types i ,
              vector_types j ,vector_types k , vector_types l ,
              vector_types m ) 
        )done
      )done
    )done
  )done
 )done
  ; add_plain ("wishart_log",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("wishart_lpdf",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("wishart_rng",  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix))) 

