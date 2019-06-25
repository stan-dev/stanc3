open Core_kernel
open Middle
open Ast
open Fmt

let rec unwrap_num_exn m e =
  match e.expr with
  | IntNumeral s -> Float.of_string s
  | RealNumeral s -> Float.of_string s
  | Variable s when Map.mem m s.name ->
      unwrap_num_exn m (Map.find_exn m s.name)
  (* TODO: insert partial evaluation here *)
  | _ -> raise_s [%sexp ("Cannot convert size to number." : string)]

let unwrap_int_exn m e = Int.of_float (unwrap_num_exn m e)

let gen_num_int m t =
  let def_low, diff = (2, 20) in
  let low, up =
    match t with
    | Lower e -> (unwrap_int_exn m e, unwrap_int_exn m e + diff)
    | Upper e -> (unwrap_int_exn m e - diff, unwrap_int_exn m e)
    | LowerUpper (e1, e2) -> (unwrap_int_exn m e1, unwrap_int_exn m e2)
    | _ -> (def_low, def_low + diff)
  in
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

let rec repeat n e = match n with 0 -> [] | m -> e :: repeat (m - 1) e

let rec repeat_th n f =
  match n with 0 -> [] | m -> f () :: repeat_th (m - 1) f

let wrap_int n = {expr= IntNumeral (Int.to_string n); emeta= {loc= no_span}}
let int_two = wrap_int 2
let wrap_real r = {expr= RealNumeral (Float.to_string r); emeta= {loc= no_span}}
let wrap_row_vector l = {expr= RowVectorExpr l; emeta= {loc= no_span}}

let wrap_vector l =
  {expr= PostfixOp (wrap_row_vector l, Transpose); emeta= {loc= no_span}}

let gen_int m t = wrap_int (gen_num_int m t)
let gen_real m t = wrap_real (gen_num_real m t)

let gen_row_vector m n t =
  {int_two with expr= RowVectorExpr (repeat_th n (fun _ -> gen_real m t))}

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
                  (repeat (k - 1) 0. @ [1.0] @ repeat (m - k) 0.)) )) }

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
  let list_dims l = Int.to_string (List.length l) :: dims (List.hd_exn l) in
  match e.expr with
  | PostfixOp (e, Transpose) -> dims e
  | IntNumeral _ -> []
  | RealNumeral _ -> []
  | ArrayExpr l -> list_dims l
  | RowVectorExpr l -> list_dims l
  | _ -> failwith "This should never happen."

(* TODO: deal with bounds *)

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
  | VarDecl {sizedtype; transformation; _} ->
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

(* ---- TESTS ---- *)

let%expect_test "whole program data generation check" =
  let open Parse in
  let ast =
    parse_string Parser.Incremental.program
      "        data {\n\
      \                  int<lower=7> K;\n\
      \                  int<lower=1> D;\n\
      \                  int<lower=0> N;\n\
      \                  int<lower=0,upper=1> y[N,D];\n\
      \                  vector[K] x[N];\n\
      \              }\n\
      \              parameters {\n\
      \                  matrix[D,K] beta;\n\
      \                  cholesky_factor_corr[D] L_Omega;\n\
      \                  real<lower=0,upper=1> u[N,D];\n\
      \              }\n\
      \            "
    |> Result.map_error ~f:render_syntax_error
    |> Result.ok_or_failwith
  in
  let str = print_data_prog ast in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
       "K <- 19\
      \nD <- 4\
      \nN <- 13\
      \ny <- structure(c(0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0), .Dim=c(13, 4))\
      \nx <- structure(c(6.32804481242085, 5.68173766561979, 2.3652769548085839, 4.5357289910613918, 3.5131929799258153, 4.4586847720929885, 6.8370129036435987, 4.3460425592300558, 4.236079038093524, 5.57044330532775, 4.40297013632479, 5.2035177758092388, 3.1623346367349097, 3.4500969860630439, 5.1662487893296616, 3.5251037024209229, 6.8837471630942284, 2.4696248704656862, 2.5406894285649768, 4.1075890325414175, 3.9882348168417239, 6.8358515530953241, 2.1034306247417267, 3.6305348739015484, 3.1670252023532717, 6.7650348033745171, 2.458985190453927, 6.1886202711890146, 6.3643316537983523, 6.0139570600649668, 5.2585335695478372, 4.1851623468667576, 3.8377391029120798, 3.1146898548382422, 5.7727253701330188, 5.74664469571191, 6.3650140932056249, 6.8625502860232208, 6.412493326241524, 4.3748899342809366, 5.9910709600237748, 2.8796096744156738, 3.2564617894824908, 4.346001743072387, 2.1414241774455447, 3.7764016096327286, 3.1042497854672586, 2.1728787126883411, 2.1421162288877871, 5.0453437529043681, 6.052290133085247, 5.6530843520324483, 6.605130516488142, 5.0933880150575055, 6.8639193501989162, 4.8197393487799083, 3.3469665202828147, 4.1567597052953822, 6.77455682541709, 2.6450189878523016, 4.4118916569083586, 6.17692542811026, 5.7463829385833591, 5.0502877793456644, 6.3568364526211569, 3.3825568676999715, 2.2112320377163437, 4.8613991672433681, 2.842716967310944, 5.4967123428108149, 4.2772341138873067, 4.603481309642774, 6.3771486836377447, 5.3142181755352409, 5.8239156043890929, 2.8051369719373973, 5.8416328677479248, 5.75313114581841, 6.2967028569417751, 2.0391695990258567, 4.5354792261141972, 5.3362618735767384, 4.5914844685002709, 3.9516037643711441, 5.8665642656289823, 2.6353763729831874, 6.6969052330141805, 5.4675539734293714, 3.8242527933689212, 2.7557818037375283, 6.7374209785557344, 4.0611382125461706, 2.1301631873459268, 3.2634565961879569, 4.7313465806729482, 4.5581681755376691, 5.4489426141401722, 3.195532597445859, 2.8468796823813753, 5.6780740968464736, 6.5866332699020509, 4.9305975781284221, 6.0663979887224926, 6.3430480959570881, 6.7134127613770644, 2.3689715820447663, 2.5587968686913358, 4.1604361823762837, 6.6116329474986, 6.5830863663924095, 5.5817072590094341, 2.5678796297961584, 4.48644631105988, 3.920509409933266, 4.8035855158639933, 2.032753266529765, 3.44073823598296, 3.0911196597724961, 5.1281157664990227, 5.8643549395764119, 6.6446008839119006, 5.5420584973249847, 2.0008765918110902, 3.0438387931497806, 3.4326928072241838, 2.1915493707904767, 6.0648569720132155, 2.5592196753676548, 2.7805033559660237, 6.06460593440673, 3.4183189411368069, 4.0767716033158834, 5.1952921358441477, 4.8379429424741227, 5.50449411076206, 4.5092183575265024, 3.2321467550995142, 5.6129743262428278, 6.2283764421777645, 2.3760824507211504, 4.6095278364784447, 6.9723718754128168, 3.3915817456957349, 5.4839654812514533, 5.5374865422223145, 2.6738023694192297, 2.779987624423983, 2.9977267737883393, 5.0925194107367595, 3.9770560845546439, 2.7433955916253576, 4.3168094489286251, 5.9037636506043745, 5.0111708946568179, 2.8576722240943773, 2.9922185548909956, 3.9631658060383339, 5.7384258294822441, 6.9677744394260461, 5.3753926219052985, 3.6158167801171341, 5.2333472124410285, 3.9659177393507976, 4.0961267254379994, 6.5051463128297273, 4.0879544136952894, 2.1647756248449643, 3.5839787411437296, 2.9165824358475314, 2.1616758648152858, 3.5272750679862259, 2.6112728309595807, 6.1927033714024837, 4.3563353153059516, 3.5018637345617281, 6.572671427738964, 6.8917467516924589, 4.7898523120306464, 4.0600024635990275, 3.98950567693723, 6.3945542521857748, 5.265005396771361, 5.6281940257385816, 6.9033227121235781, 4.0205204960701506, 2.0591547264718466, 2.0369210229209154, 4.2334309983874352, 4.9239735061382133, 2.0322436872773642, 4.88006454563493, 6.0330024544790373, 2.8004338055820144, 4.6810852530205684, 4.1058280414201116, 6.9222014365470823, 5.0826678223080339, 5.2717019072050988, 4.0523813833999158, 5.3804006786221592, 5.3123300934333582, 4.2428114734734317, 5.3597848997812569, 2.5168562771809309, 2.800085736267008, 3.1770949492436795, 3.081734276320848, 4.8058030838356434, 6.9769319083494956, 5.2472259234134953, 3.1496816302966533, 5.1908850416778813, 4.3267831514619184, 3.6505184614386934, 6.5012610937369075, 4.02174411212451, 5.0051725462417149, 2.0065661568893631, 5.4749005932732011, 4.2126133176962464, 4.6999026086186237, 4.1648450790514984, 6.595992521557549, 2.50612480168045, 6.3727719067219635, 2.8894910165285017, 2.3401196405310771, 3.3164123628021085, 2.7463467732436149, 5.6280870799844429, 4.4199340938227678, 6.052023950534501, 2.0971511554021931, 3.179916305638534, 2.5723669841481698, 3.7955076601527784, 6.8042708287731131, 5.92158428245417, 4.5535284082830643, 5.1627880257526506, 6.8497844596389257, 2.2913654572703033, 4.1851173088339912, 3.8754168444668471, 3.168385165512114, 6.100895266639677, 4.0884592234108919), .Dim=c(13, 19))" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty
      (SArray (SArray (SInt, wrap_int 3), wrap_int 4))
      Identity
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(14, 5, 15, 8, 14, 17, 2, 22, 3, 2, 18, 3), .Dim=c(4, 3))" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty
      (SArray (SArray (SArray (SInt, wrap_int 5), wrap_int 2), wrap_int 4))
      Identity
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(14, 5, 15, 8, 14, 17, 2, 22, 3, 2, 18, 3, 17, 4, 11, 19, 8, 17, 18, 20, 18, 17, 8, 2, 22, 2, 2, 12, 8, 21, 13, 18, 10, 21, 15, 12, 22, 21, 11, 17), .Dim=c(4, 2, 5))" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty (SMatrix (wrap_int 3, wrap_int 4)) Identity
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(4.1815278199399577, 6.8017664359959342, 4.8441784126802627, 4.25312636944623, 5.2015419032442969, 2.7103944900448411, 3.3282621325833865, 2.56799363086151, 4.0759938356540726, 3.604405750889411, 6.0288479433993629, 3.543689144366625), .Dim=c(3, 4))" |}]

let%expect_test "data generation check" =
  let expr = generate_value Map.Poly.empty (SVector (wrap_int 3)) Identity in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "c(4.1815278199399577, 6.8017664359959342, 4.8441784126802627)" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty
      (SArray (SVector (wrap_int 3), wrap_int 4))
      Identity
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(4.1815278199399577, 6.8017664359959342, 4.8441784126802627, 4.25312636944623, 5.2015419032442969, 2.7103944900448411, 3.3282621325833865, 2.56799363086151, 4.0759938356540726, 3.604405750889411, 6.0288479433993629, 3.543689144366625), .Dim=c(4, 3))" |}]

let%expect_test "data generation check" =
  let expr = generate_value Map.Poly.empty (SVector (wrap_int 3)) Simplex in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "c(0.22198258835220422, 0.48860644012069177, 0.289410971527104)" |}]

let%expect_test "data generation check" =
  let expr = generate_value Map.Poly.empty (SVector (wrap_int 3)) UnitVector in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "c(0.36406675257322474, 0.80134825556167411, 0.47465395076734407)" |}]

let%expect_test "data generation check" =
  let expr = generate_value Map.Poly.empty (SVector (wrap_int 30)) Ordered in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "c(-0.96832729385188365, -0.91004897647537208, -0.855488426712053, -0.8070719147445744, -0.73591866262419769, -0.64090940155884812, -0.58159948031027586, -0.498033598356894, -0.43497790423044719, -0.39103777937151057, -0.33030882131474781, -0.24446230343719536, -0.18409371422459761, -0.10156543104346247, -0.05653532094436739, 0.013923649350849741, 0.10638632763557807, 0.17175965077143784, 0.22970780349821893, 0.32496498924437173, 0.38362111488017359, 0.44807884297139466, 0.49575395920660592, 0.55125841432324352, 0.60031084605608087, 0.68104157187272929, 0.74782374244229277, 0.82298579970312025, 0.93416731632648975, 1.)" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty (SVector (wrap_int 30)) PositiveOrdered
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "c(0.0158363530740582, 0.044975511762313986, 0.072255786643973488, 0.096464042627712757, 0.13204066868790115, 0.17954529922057594, 0.20920025984486204, 0.250983200821553, 0.28251104788477643, 0.30448111031424469, 0.33484558934262609, 0.37776884828140234, 0.40795314288770118, 0.44921728447826875, 0.47173233952781629, 0.50696182467542483, 0.553193163817789, 0.58587982538571892, 0.61485390174910948, 0.66248249462218589, 0.69181055744008679, 0.72403942148569733, 0.7478769796033029, 0.77562920716162176, 0.80015542302804044, 0.84052078593636459, 0.87391187122114644, 0.91149289985156012, 0.96708365816324482, 1.)" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty
      (SMatrix (wrap_int 2, wrap_int 3))
      Correlation
  in
  print_s [%sexp (expr : untyped_expression)] ;
  [%expect
    {|
      ((expr
        (RowVectorExpr
         (((expr
            (RowVectorExpr
             (((expr (RealNumeral 1.)) (emeta ((loc <opaque>))))
              ((expr (RealNumeral 0.)) (emeta ((loc <opaque>))))
              ((expr (RealNumeral 0.)) (emeta ((loc <opaque>)))))))
           (emeta ((loc <opaque>))))
          ((expr
            (RowVectorExpr
             (((expr (RealNumeral 0.)) (emeta ((loc <opaque>))))
              ((expr (RealNumeral 1.)) (emeta ((loc <opaque>))))
              ((expr (RealNumeral 0.)) (emeta ((loc <opaque>)))))))
           (emeta ((loc <opaque>)))))))
       (emeta ((loc <opaque>)))) |}]
