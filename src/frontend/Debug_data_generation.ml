open Core_kernel
open Middle
open Ast

let unwrap_num_exn m e =
  let e = Ast_to_Mir.trans_expr e in
  let m = Map.Poly.map m ~f:Ast_to_Mir.trans_expr in
  let e = Analysis_and_optimization.Mir_utils.subst_expr m e in
  let e = Analysis_and_optimization.Partial_evaluator.eval_expr e in
  match e.expr with
  | Lit (_, s) -> Float.of_string s
  | _ -> raise_s [%sexp ("Cannot convert size to number." : string)]

let unwrap_int_exn m e = Int.of_float (unwrap_num_exn m e)

let gen_num_int m t =
  let def_low, diff = (2, 4) in
  let low, up =
    match t with
    | Lower e -> (unwrap_int_exn m e, unwrap_int_exn m e + diff)
    | Upper e -> (unwrap_int_exn m e - diff, unwrap_int_exn m e)
    | LowerUpper (e1, e2) -> (unwrap_int_exn m e1, unwrap_int_exn m e2)
    | _ -> (def_low, def_low + diff)
  in
  let low = if low = 0 && up <> 1 then low + 1 else low in
  Random.int (up - low + 1) + low

let gen_num_real m t =
  let def_low, diff = (2., 5.) in
  let low, up =
    match t with
    | Lower e -> (unwrap_num_exn m e, unwrap_num_exn m e +. diff)
    | Upper e -> (unwrap_num_exn m e -. diff, unwrap_num_exn m e)
    | LowerUpper (e1, e2) -> (unwrap_num_exn m e1, unwrap_num_exn m e2)
    | _ -> (def_low, def_low +. diff)
  in
  Random.float_range low up

let rec repeat n e =
  match n with n when n <= 0 -> [] | m -> e :: repeat (m - 1) e

let rec repeat_th n f =
  match n with n when n <= 0 -> [] | m -> f () :: repeat_th (m - 1) f

let wrap_int n =
  { expr= IntNumeral (Int.to_string n)
  ; emeta= {loc= no_span; ad_level= DataOnly; type_= UInt} }

let int_two = wrap_int 2

let wrap_real r =
  { expr= RealNumeral (Float.to_string r)
  ; emeta= {loc= no_span; ad_level= DataOnly; type_= UReal} }

let wrap_row_vector l =
  { expr= RowVectorExpr l
  ; emeta= {loc= no_span; ad_level= DataOnly; type_= URowVector} }

let wrap_vector l =
  { expr= PostfixOp (wrap_row_vector l, Transpose)
  ; emeta= {loc= no_span; ad_level= DataOnly; type_= UVector} }

let gen_int m t = wrap_int (gen_num_int m t)
let gen_real m t = wrap_real (gen_num_real m t)

let gen_row_vector m n t =
  { expr= RowVectorExpr (repeat_th n (fun _ -> gen_real m t))
  ; emeta= {loc= no_span; ad_level= DataOnly; type_= UMatrix} }

let gen_vector m n t =
  let gen_ordered n =
    let l = repeat_th n (fun _ -> Random.float 1.) in
    let l =
      List.fold (List.tl_exn l) ~init:[List.hd_exn l] ~f:(fun accum elt ->
          (Float.exp elt +. List.hd_exn accum) :: accum )
    in
    l
  in
  match t with
  | Simplex ->
      let l = repeat_th n (fun _ -> Random.float 1.) in
      let sum = List.fold l ~init:0. ~f:(fun accum elt -> accum +. elt) in
      let l = List.map l ~f:(fun x -> x /. sum) in
      wrap_vector (List.map ~f:wrap_real l)
  | Ordered ->
      let l = gen_ordered n in
      let halfmax =
        Option.value_exn (List.max_elt l ~compare:compare_float) /. 2.
      in
      let l = List.map l ~f:(fun x -> (x -. halfmax) /. halfmax) in
      wrap_vector (List.map ~f:wrap_real l)
  | PositiveOrdered ->
      let l = gen_ordered n in
      let max = Option.value_exn (List.max_elt l ~compare:compare_float) in
      let l = List.map l ~f:(fun x -> x /. max) in
      wrap_vector (List.map ~f:wrap_real l)
  | UnitVector ->
      let l = repeat_th n (fun _ -> Random.float 1.) in
      let sum =
        Float.sqrt
          (List.fold l ~init:0. ~f:(fun accum elt -> accum +. (elt ** 2.)))
      in
      let l = List.map l ~f:(fun x -> x /. sum) in
      wrap_vector (List.map ~f:wrap_real l)
  | _ -> {int_two with expr= PostfixOp (gen_row_vector m n t, Transpose)}

let gen_identity_matrix n m =
  { int_two with
    expr=
      RowVectorExpr
        (List.map
           (List.range 1 (n + 1))
           ~f:(fun k ->
             wrap_row_vector
               (List.map ~f:wrap_real
                  ( repeat (min (k - 1) m) 0.
                  @ (if k <= m then [1.0] else [])
                  @ repeat (m - k) 0. )) )) }

let gen_matrix mm n m t =
  match t with
  | CholeskyCorr | CholeskyCov | Correlation | Covariance ->
      gen_identity_matrix n m
  | _ ->
      { int_two with
        expr= RowVectorExpr (repeat_th n (fun () -> gen_row_vector mm m t)) }

(* TODO: do some proper random generation of these special matrices *)

let gen_array elt n _ = {int_two with expr= ArrayExpr (repeat_th n elt)}

let rec generate_value m st t =
  match st with
  | SInt -> gen_int m t
  | SReal -> gen_real m t
  | SVector e -> gen_vector m (unwrap_int_exn m e) t
  | SRowVector e -> gen_row_vector m (unwrap_int_exn m e) t
  | SMatrix (e1, e2) ->
      gen_matrix m (unwrap_int_exn m e1) (unwrap_int_exn m e2) t
  | SSparseMatrix (_, _, e1, e2) ->
      gen_matrix m (unwrap_int_exn m e1) (unwrap_int_exn m e2) t
  | SArray (st, e) ->
      let element () = generate_value m st t in
      gen_array element (unwrap_int_exn m e) t

let rec flatten e =
  let flatten_expr_list l =
    List.fold (List.map ~f:flatten l) ~init:[] ~f:(fun vals new_vals ->
        new_vals @ vals )
  in
  match e.expr with
  | PostfixOp (e, Transpose) -> flatten e
  | IntNumeral s -> [s]
  | RealNumeral s -> [s]
  | ArrayExpr l -> flatten_expr_list l
  | RowVectorExpr l -> flatten_expr_list l
  | _ -> failwith "This should never happen."

let rec dims e =
  let list_dims l =
    if List.length l = 0 then []
    else Int.to_string (List.length l) :: dims (List.hd_exn l)
  in
  match e.expr with
  | PostfixOp (e, Transpose) -> dims e
  | IntNumeral _ -> []
  | RealNumeral _ -> []
  | ArrayExpr l -> list_dims l
  | RowVectorExpr l -> list_dims l
  | _ -> failwith "This should never happen."

let rec print_value_r e =
  let expr = e.expr in
  let print_container e =
    let vals, dims = (flatten e, dims e) in
    let flattened_str = "c(" ^ String.concat ~sep:", " vals ^ ")" in
    if List.length dims <= 1 then flattened_str
    else
      "structure(" ^ flattened_str ^ ", .Dim=" ^ "c("
      ^ String.concat ~sep:", " dims
      ^ ")" ^ ")"
  in
  match expr with
  | PostfixOp (e, Transpose) -> print_value_r e
  | IntNumeral s -> s
  | RealNumeral s -> s
  | ArrayExpr _ -> print_container e
  | RowVectorExpr _ -> print_container e
  | _ -> failwith "This should never happen."

let var_decl_id d =
  match d.stmt with
  | VarDecl {identifier; _} -> identifier.name
  | _ -> failwith "This should never happen."

let var_decl_gen_val m d =
  match d.stmt with
  | VarDecl {decl_type= Sized sizedtype; transformation; _} ->
      generate_value m sizedtype transformation
  | _ -> failwith "This should never happen."

let print_data_prog s =
  let data = Option.value ~default:[] s.datablock in
  let l, _ =
    List.fold data ~init:([], Map.Poly.empty) ~f:(fun (l, m) decl ->
        let value = var_decl_gen_val m decl in
        ( l @ [var_decl_id decl ^ " <- " ^ print_value_r value]
        , Map.set m ~key:(var_decl_id decl) ~data:value ) )
  in
  String.concat ~sep:"\n" l
