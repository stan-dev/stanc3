open Core_kernel

type identifier = string [@@deriving sexp]

type type_ =
  | Auto
  | Void
  | Int
  | Double
  | Complex of type_
  | TemplateType of identifier
  | Vector of type_
  | Array of type_ * int
  | Type_literal of identifier  (** Used for things like Eigen::Index *)
  | Matrix of type_ * int * int
  | Ref of type_
  | Const of type_
  | Pointer of type_
  | TypeTrait of string * type_ list
      (** e.g. stan::promote_scalar, stan:base_type *)
[@@deriving sexp]

module Types = struct
  (** Helpers for constructing types *)

  let local_scalar = Type_literal "local_scalar_t__"
  let std_vector t = Vector t
  let bool = Type_literal "bool"
  let complex s = Complex s
  let vector s = Matrix (s, -1, 1)
  let row_vector s = Matrix (s, 1, -1)
  let matrix s = Matrix (s, -1, -1)
  let string = Type_literal "std::string"
  let size_t = Type_literal "size_t"
  let const_ref t = Const (Ref t)
  let str_array i = Array (Const (Pointer (Type_literal "char")), i)
end

type operator =
  | Multiply
  | Divide
  | Add
  | Subtract
  | Eq
  | LEq
  | GEq
  | Lthn
  | Gthn
  | And
  | Or
[@@deriving sexp]

type unary_op = PMinus | Incr [@@deriving sexp]

type expr =
  | Literal of string
  | Var of identifier
  | Parens of expr
  | FunCall of identifier * type_ list * expr list
  | MethodCall of expr * identifier * type_ list * expr list
  | StaticMethodCall of type_ * identifier * type_ list * expr list
  | Constructor of type_ * expr list
  | InitializerExpr of type_ * expr list
  | ArrayLiteral of expr list
  | TernaryIf of expr * expr * expr
  | Cast of type_ * expr
  | Index of expr * expr
  | New of string option * type_ * expr list
  | Assign of expr * expr (* NB: Not all exprs are valid lvalues! *)
  | Unary of unary_op * expr
  | StreamInsertion of expr * expr list
  | BinOp of expr * operator * expr
[@@deriving sexp]

module Exprs = struct
  (** Some helper values and functions *)

  let method_call obj name templates args =
    match obj with
    | Var _ -> MethodCall (obj, name, templates, args)
    | _ -> MethodCall (Parens obj, name, templates, args)

  let to_var s = Var s
  let literal_string s = Literal ("\"" ^ s ^ "\"")
  let std_vector_expr t es = InitializerExpr (Vector t, es)
  let fun_call s es = FunCall (s, [], es)
  let templated_fun_call s ts es = FunCall (s, ts, es)
  let quiet_NaN = fun_call "std::numeric_limits<double>::quiet_NaN" []
  let int_min = fun_call "std::numeric_limits<int>::min" []

  let binop_list es ~f ~default : expr =
    match es with
    | [] -> default
    | head :: rest -> List.fold ~init:head ~f:(fun a b -> Parens (f a b)) rest
end

module Expression_syntax = struct
  (** Some operators to make streams and method calls look more
      like the resultant C++ *)

  include Exprs

  let ( << ) a b = StreamInsertion (a, b)
  let ( .@!() ) obj name = method_call obj name [] []
  let ( .@?() ) obj (name, args) = method_call obj name [] args

  let ( .@<>() ) obj (name, templates, args) =
    method_call obj name templates args

  let ( |::! ) ty name = StaticMethodCall (ty, name, [], [])
  let ( |::? ) ty (name, args) = StaticMethodCall (ty, name, [], args)

  let ( |::<> ) ty (name, templates, args) =
    StaticMethodCall (ty, name, templates, args)

  let ( + ) a b = BinOp (a, Add, b)
  let ( - ) a b = BinOp (a, Subtract, b)
  let ( * ) a b = BinOp (a, Multiply, b)
end

type init =
  | Assignment of expr
  | Construction of expr list
  | InitalizerList of expr list
  | Uninitialized
[@@deriving sexp]

type var_defn =
  { static: bool [@default false]
  ; constexpr: bool [@default false]
  ; type_: type_
  ; name: identifier
  ; init: init [@default Uninitialized] }
[@@deriving make, sexp]

type stmt =
  | Expression of expr
  | VarDef of var_defn
  | For of var_defn * expr * expr * stmt
  | ForEach of (type_ * identifier) * expr * stmt
  | While of expr * stmt
  | IfElse of expr * stmt * stmt option
  | TryCatch of stmt * (type_ * identifier) * stmt
  | Block of stmt list
  | Return of expr option
  | Throw of expr
  | Break
  | Continue
  | Semicolon
  | Using of string * type_ option
  | Comment of string
[@@deriving sexp]

module Stmts = struct
  let block stmts = match stmts with [(Block _ as b)] -> b | _ -> Block stmts

  let rethrow_located stmts =
    TryCatch
      ( block stmts
      , (Types.const_ref (Type_literal "std::exception"), "e")
      , Block
          [ Expression
              (FunCall
                 ( "stan::lang::rethrow_located"
                 , []
                 , [ Var "e"
                   ; Index (Var "locations_array__", Var "current_statement__")
                   ] ) ) ] )

  let fori loopvar lower upper body =
    let init =
      make_var_defn ~type_:Int ~name:loopvar ~init:(Assignment lower) () in
    let stop = BinOp (Var loopvar, LEq, upper) in
    let incr = Unary (Incr, Var loopvar) in
    For (init, stop, incr, body)

  let if_block cond stmts = IfElse (cond, block stmts, None)

  let unused s =
    [Comment "supress unused var warning"; Expression (Cast (Void, Var s))]
end

module Decls = struct
  let current_statement =
    VarDef
      (make_var_defn ~type_:Int ~name:"current_statement__"
         ~init:(Assignment (Literal "0")) () )

  let dummy_var =
    VarDef
      (make_var_defn ~type_:Types.local_scalar ~name:"DUMMY_VAR__"
         ~init:(Construction [Exprs.quiet_NaN])
         () )
    :: Stmts.unused "DUMMY_VAR__"

  let serializer_in =
    VarDef
      (make_var_defn
         ~type_:(TypeTrait ("stan::io::deserializer", [Types.local_scalar]))
         ~name:"in__"
         ~init:(Construction [Var "params_r__"; Var "params_i__"])
         () )

  let serializer_out =
    VarDef
      (make_var_defn
         ~type_:(TypeTrait ("stan::io::serializer", [Types.local_scalar]))
         ~name:"out__"
         ~init:(Construction [Var "vars__"])
         () )

  let lp_accum t =
    VarDef
      (make_var_defn
         ~type_:(TypeTrait ("stan::math::accumulator", [t]))
         ~name:"lp_accum__" () )
end

type template_parameter =
  | Typename of string  (** The name of a template typename *)
  | RequireIs of string * string
      (** A C++ type trait (e.g. is_arithmetic) and the template
          name which needs to satisfy that.
          These are collated into one require_all_t<> *)
  | Require of string * string list
  | Bool of string  (** A named boolean template type *)
[@@deriving sexp]

type cv_qualifiers = Const | Final | NoExcept [@@deriving sexp]

type fun_defn =
  { templates_init: template_parameter list list * bool [@default [[]], false]
        (** Double list since some functions (mainly reduce_sum functors) need two sets of templates *)
  ; name: identifier
  ; inline: bool [@default false]
  ; return_type: type_
  ; args: (type_ * string) list
  ; cv_qualifiers: cv_qualifiers list [@default []]
  ; body: stmt list option }
[@@deriving make, sexp]

type constructor =
  { args: (type_ * string) list
  ; init_list: (identifier * expr list) list
  ; body: stmt list }
[@@deriving make, sexp]

(** Incomplete list of C++ preprocessor directives *)
type directive =
  | Include of string
  | IfNDef of string * defn list
  | MacroApply of string * string list

and class_defn =
  { class_name: identifier
  ; final: bool
  ; base: type_
  ; private_members: defn list
  ; public_members: defn list
  ; constructor: constructor
  ; destructor_body: stmt list }

and struct_defn =
  {param: template_parameter option; struct_name: identifier; body: defn list}

and defn =
  | FunDef of fun_defn
  | Class of class_defn
  | Struct of struct_defn
  | TopVarDef of var_defn
  | TopComment of string
  | TopUsing of string * type_ option
  | Namespace of identifier * defn list
  | Preprocessor of directive
[@@deriving sexp]

(* can't be derivided since it is simultaneously declared with non-records *)
let make_class_defn ~name ~base ?(final = true) ~private_members ~public_members
    ~constructor ?(destructor_body = []) () =
  { class_name= name
  ; base
  ; final
  ; private_members
  ; public_members
  ; constructor
  ; destructor_body }

let make_struct_defn ~param ~name ~body () = {param; struct_name= name; body}

type program = defn list [@@deriving sexp]

module Printing = struct
  open Fmt

  let trailing_space (t : 'a Fmt.t) : 'a Fmt.t = fun ppf -> pf ppf "%a@ " t
  let pp_identifier ppf = string ppf

  let rec pp_type_ ppf t =
    match t with
    | Auto -> string ppf "auto"
    | Void -> string ppf "void"
    | Int -> string ppf "int"
    | Double -> string ppf "double"
    | Complex t -> pf ppf "std::complex<%a>" pp_type_ t
    | TemplateType id -> pp_identifier ppf id
    | Vector t -> pf ppf "@[<2>std::vector<@,%a>@]" pp_type_ t
    | Array (t, i) -> pf ppf "@[<2>std::array<@,%a,@ %i>@]" pp_type_ t i
    | Type_literal id -> pp_identifier ppf id
    | Matrix (t, i, j) -> pf ppf "Eigen::Matrix<%a,%i,%i>" pp_type_ t i j
    | Const t -> pf ppf "const %a" pp_type_ t
    | Ref t -> pf ppf "%a&" pp_type_ t
    | Pointer t -> pf ppf "%a*" pp_type_ t
    | TypeTrait (s, ts) ->
        pf ppf "@[<2>%s<%a>@]" s (list ~sep:comma pp_type_) ts

  let pp_requires ~default ppf requires =
    match requires with
    | [] -> ()
    | _ ->
        let pp_require ppf (trait, name) = pf ppf "%s<%s>" trait name in
        pf ppf ",@ stan::require_all_t<@[%a@]>*%s"
          (list ~sep:comma pp_require)
          requires
          (if default then " = nullptr" else "")

  (**
   Pretty print a full C++ `template <parameter-list>`
  *)
  let pp_template ~default ppf template_parameters =
    let pp_basic_template ppf = function
      | `Typename name -> pf ppf "typename %s" name
      | `Bool name -> pf ppf "bool %s" name
      | `Require (requirement, args) ->
          pf ppf "%s<%a>*%s" requirement (list ~sep:comma string) args
            (if default then " = nullptr" else "") in
    match template_parameters with
    | [] -> ()
    | _ ->
        let templates, requires =
          List.partition_map template_parameters ~f:(function
            | RequireIs (trait, name) -> Second (trait, name)
            | Typename name -> First (`Typename name)
            | Bool name -> First (`Bool name)
            | Require (requirement, args) -> First (`Require (requirement, args)) )
        in
        pf ppf "template <@[%a%a@]>@ "
          (list ~sep:comma pp_basic_template)
          templates (pp_requires ~default) requires

  let pp_operator ppf = function
    | Multiply -> string ppf "*"
    | Divide -> string ppf "/"
    | Add -> string ppf "+"
    | Subtract -> string ppf "-"
    | Eq -> string ppf "=="
    | LEq -> string ppf "<="
    | GEq -> string ppf ">="
    | Lthn -> string ppf "<"
    | Gthn -> string ppf ">"
    | And -> string ppf "&&"
    | Or -> string ppf "||"

  let pp_unary_op ppf = function
    | PMinus -> string ppf "-"
    | Incr -> string ppf "++"

  let rec pp_expr ppf e =
    let maybe_templates ppf ts =
      if List.length ts = 0 then nop ppf ()
      else pf ppf "<@,%a>" (list ~sep:comma pp_type_) ts in
    match e with
    | Literal s -> pf ppf "%s" s
    | Var id -> string ppf id
    | Parens e -> pf ppf "(%a)" pp_expr e
    | Cast (t, e) -> pf ppf "@[(%a)@ %a@]" pp_type_ t pp_expr e
    | Constructor (t, es) ->
        pf ppf "@[<hov 2>%a(%a)@]" pp_type_ t (list ~sep:comma pp_expr) es
    | New (ptr, t, es) ->
        pf ppf "@[<hov 2>new %a%a(%a)@]"
          (option (trailing_space (parens string)))
          ptr pp_type_ t (list ~sep:comma pp_expr) es
    | ArrayLiteral es -> pf ppf "{%a}" (list ~sep:comma pp_expr) es
    | InitializerExpr (t, es) ->
        pf ppf "@[<2>%a{%a}@]" pp_type_ t (list ~sep:comma pp_expr) es
    | StreamInsertion (e, es) ->
        pf ppf "%a <<@[@ %a@]" pp_expr e (list ~sep:comma pp_expr) es
    | FunCall (fn, tys, es) ->
        pf ppf "@[<hov 2>%s%a(@,%a@])" fn maybe_templates tys
          (list ~sep:comma pp_expr) es
    | MethodCall (e, fn, tys, es) ->
        pf ppf "@[<hov 2>%a.%s%a(%a)@]" pp_expr e fn maybe_templates tys
          (list ~sep:comma pp_expr) es
    | StaticMethodCall (ty, fn, tys, es) ->
        pf ppf "@[<hov 2>%a::%s%a(%a)@]" pp_type_ ty fn maybe_templates tys
          (list ~sep:comma pp_expr) es
    | TernaryIf (e1, e2, e3) ->
        pf ppf "%a ? %a : %a" pp_expr e1 pp_expr e2 pp_expr e3
    | Index (e1, e2) -> pf ppf "%a[%a]" pp_expr e1 pp_expr e2
    | Assign (e1, e2) -> pf ppf "%a = %a" pp_expr e1 pp_expr e2
    | Unary (op, e) -> pf ppf "%a%a" pp_unary_op op pp_expr e
    | BinOp (e1, op, e2) ->
        pf ppf "%a@ %a@ %a" pp_expr e1 pp_operator op pp_expr e2

  let pp_var_defn ppf {static; constexpr; type_; name; init} =
    let pp_init ppf init =
      match init with
      | Uninitialized -> ()
      | Assignment e -> pf ppf "@ =@ %a" pp_expr e
      | Construction es -> pf ppf "(@[<hov 1>%a@])" (list ~sep:comma pp_expr) es
      | InitalizerList es ->
          pf ppf "{@[<hov 1>%a@]}" (list ~sep:comma pp_expr) es in
    let static = if static then "static " else "" in
    let constexpr = if constexpr then "constexpr " else "" in
    pf ppf "@[<2>%s%s%a %s%a@]" static constexpr pp_type_ type_ name pp_init
      init

  let rec pp_stmt ppf s =
    match s with
    | Expression e -> pf ppf "@[<2>%a;@]" pp_expr e
    | Return e -> pf ppf "return %a;" (option (box pp_expr)) e
    | Throw e -> pf ppf "throw %a;" pp_expr e
    | Break -> string ppf "break;"
    | Continue -> string ppf "continue;"
    | Semicolon -> string ppf ";"
    | VarDef vd -> pf ppf "%a;" pp_var_defn vd
    | For (init, cond, incr, Block stmts) ->
        (* When we know a block is here, we can do better pretty-printing*)
        pf ppf "@[<v 2>for(@[<hov>%a; %a; %a@]) {@ @[<v>%a@]@]@,}" pp_var_defn
          init pp_expr cond pp_expr incr (list ~sep:cut pp_stmt) stmts
    | For (init, cond, incr, s) ->
        pf ppf "@[<v 2>for(@[<hov>%a; %a; %a@])@ @[%a@]@]" pp_var_defn init
          pp_expr cond pp_expr incr pp_stmt s
    | ForEach ((ty, name), set, Block stmts) ->
        (* When we know a block is here, we can do better pretty-printing*)
        pf ppf "@[<v 2>for(@[<hov>%a %a: %a@]) {@ @[<v>%a@]@]@,}" pp_type_ ty
          pp_identifier name pp_expr set (list ~sep:cut pp_stmt) stmts
    | ForEach ((ty, name), set, s) ->
        pf ppf "@[<v 2>for(@[<hov>%a %a: %a@])@ @[%a@]@]" pp_type_ ty
          pp_identifier name pp_expr set pp_stmt s
    | While (e, s) ->
        pf ppf "@[<v 2>while(@[%a@])@ @[%a@]@]" pp_expr e pp_stmt s
    | IfElse (cond, thn, els) ->
        pf ppf "@[<v 2>if(@[%a@])@ @[%a@]@]@,@[<v 2>%a@]" pp_expr cond pp_stmt
          thn
          (option (fun ppf els -> pf ppf "else @[%a@]" pp_stmt els))
          els
    | Block stmts ->
        pf ppf "@[<v>@[<v 2>{@,%a@]@,}@]" (list ~sep:cut pp_stmt) stmts
    | Using (s, init) ->
        pf ppf "using %s%a;" s
          (option (fun ppf defn -> pf ppf " = %a" pp_type_ defn))
          init
    | Comment s ->
        if String.contains s '\n' then pf ppf "/@[<v>*@[@ %a@]@,@]*/" text s
        else pf ppf "//@[<h> %s@]" s
    | TryCatch (Block trys, (exn_ty, exn_name), Block thn) ->
        (* When we know this contains blocks, we can do better pretty-printing*)
        pf ppf "@[<v 2>try {@ %a@]@,@[<v 2>} catch(%a %a) {@ %a@]@,}"
          (list ~sep:cut pp_stmt) trys pp_type_ exn_ty pp_identifier exn_name
          (list ~sep:cut pp_stmt) thn
    | TryCatch (trys, (exn_ty, exn_name), thn) ->
        pf ppf "@[<v>@[<hov>try@ %a@]@,@[<hov>catch(%a %a)@ %a@]@]" pp_stmt trys
          pp_type_ exn_ty pp_identifier exn_name pp_stmt thn

  let pp_cv ppf q =
    match q with
    | Const -> string ppf " const"
    | Final -> string ppf " final"
    | NoExcept -> string ppf " noexcept"

  let pp_fun_defn ppf
      { templates_init= t, init
      ; name
      ; inline
      ; return_type
      ; args
      ; cv_qualifiers
      ; body } =
    pf ppf "@[%a%s%a@ %a(@[<hov>%a@])%a@]%a"
      (list (pp_template ~default:init))
      t
      (if inline then "inline " else "")
      pp_type_ return_type pp_identifier name
      (list ~sep:comma (pair ~sep:Fmt.sp pp_type_ pp_identifier))
      args (list ~sep:nop pp_cv) cv_qualifiers
      (option ~none:Fmt.semi (fun ppf stmts ->
           pf ppf "@,@[<v 2>{@,%a@]@,}" (list ~sep:cut pp_stmt) stmts ) )
      body

  let pp_destructor ppf (name, body) =
    pf ppf "@[~%s()@ {%a}@]" name (list ~sep:cut pp_stmt) body

  let pp_constructor ppf (name, {args; init_list; body}) =
    let pp_init ppf (id, es) = pf ppf "%s(%a)" id (list ~sep:comma pp_expr) es in
    let pp_inits =
      if List.length init_list = 0 then Fmt.nop
      else fun ppf inits -> pf ppf ": %a" (list ~sep:comma pp_init) inits in
    pf ppf "@[%s(@[%a@])%a@ @[<v 2>{@,%a}@]@]" name
      (list ~sep:comma (pair ~sep:sp pp_type_ pp_identifier))
      args pp_inits init_list (list ~sep:cut pp_stmt) body

  let rec pp_directive ppf direct =
    match direct with
    | Include file -> pf ppf "#include <%s>" file
    | IfNDef (name, defns) ->
        pf ppf "@[<v>#ifndef %s@,%a@,#endif" name (list ~sep:cut pp_defn) defns
    | MacroApply (name, args) ->
        pf ppf "@[<h>%s(%a)@]" name (list ~sep:(any ", ") string) args

  and pp_class_defn ppf
      { class_name
      ; final
      ; base
      ; private_members
      ; constructor
      ; destructor_body
      ; public_members } =
    pf ppf
      "@[<v 2>class %s%s : %a{@,\
       @[<v 2>private:@,\
       %a@]@,\
       @[<v 2>public:@,\
       %a@,\
       %a@,\
       %a@]@]@,\
       };"
      class_name
      (if final then " final" else "")
      pp_type_ base (list ~sep:cut pp_defn) private_members pp_destructor
      (class_name, destructor_body)
      pp_constructor (class_name, constructor) (list ~sep:cut pp_defn)
      public_members

  and pp_struct_defn ppf {param; struct_name; body} =
    pf ppf "%a@[<v 2>struct %s {@,%a@]@,};"
      (option (fun ppf p -> (pp_template ~default:false) ppf [p]))
      param struct_name (list ~sep:cut pp_defn) body

  and pp_defn ppf d =
    match d with
    | FunDef fd -> pp_fun_defn ppf fd
    | Class cd -> pp_class_defn ppf cd
    | Struct sd -> pp_struct_defn ppf sd
    | TopVarDef vd -> pf ppf "%a;" pp_var_defn vd
    | TopComment s ->
        if String.contains s '\n' then pf ppf "/@[<v>*@[@ %a@]@,@]*/" text s
        else pf ppf "//@[<h> %s@]" s
    | TopUsing (s, init) ->
        pf ppf "using %s%a;" s
          (option (fun ppf defn -> pf ppf " = %a" pp_type_ defn))
          init
    | Namespace (id, defns) ->
        pf ppf "@[<v 2>namespace %s {@,%a@]@,}" id (list ~sep:cut pp_defn) defns
    | Preprocessor d -> pp_directive ppf d
end

module Tests = struct
  let%expect_test "rethrow_located" =
    let s =
      [ Comment
          "A potentially very very very very very long comment which will be \
           on one line"; Comment "A potentially \n multiline comment"
      ; Expression (Assign (Var "foo", Literal "3")) ] in
    let rethrow = Stmts.rethrow_located s in
    Printing.pp_stmt Fmt.stdout rethrow ;
    [%expect
      {|
      try {
        // A potentially very very very very very long comment which will be on one line
        /* A potentially
           multiline comment
         */
        foo = 3;
      } catch(const std::exception& e) {
        stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      } |}]

  let%expect_test "types" =
    let ts =
      let open Types in
      [ matrix (complex local_scalar); str_array 43
      ; std_vector (std_vector Double); const_ref (TemplateType "T0__") ] in
    let open Fmt in
    pf stdout "@[<v>%a@]" (list ~sep:comma Printing.pp_type_) ts ;
    [%expect
      {|
        Eigen::Matrix<std::complex<local_scalar_t__>,-1,-1>,
        std::array<const char*, 43>,
        std::vector<std::vector<double>>,
        const T0__& |}]

  (* This shows off some of the fancy syntax OCaml lets us use,
      like [<<] or [.@()]*)
  let%expect_test "eigen init" =
    let open Expression_syntax in
    let open Types in
    let vector = Constructor (row_vector Double, [Literal "3"]) in
    let values = [Literal "1"; Var "a"; Literal "3"] in
    let e = (vector << values).@!("finished") in
    print_s [%sexp (e : expr)] ;
    print_endline "" ;
    Printing.pp_expr Fmt.stdout e ;
    [%expect
      {|
          (MethodCall
           (Parens
            (StreamInsertion (Constructor (Matrix Double 1 -1) ((Literal 3)))
             ((Literal 1) (Var a) (Literal 3))))
           finished () ())

          (Eigen::Matrix<double,1,-1>(3) << 1, a, 3).finished() |}]

  let%expect_test "function defn" =
    let funs =
      [ make_fun_defn
          ~templates_init:
            ([[Typename "T0__"; RequireIs ("stan::is_foobar", "T0__")]], true)
          ~name:"foobar" ~return_type:Void ~inline:true ()
      ; (let s =
           [ Comment "A potentially \n long comment"
           ; Expression (Assign (Var "foo", Literal "3")) ] in
         let rethrow = Stmts.rethrow_located s in
         make_fun_defn
           ~templates_init:
             ([[Typename "T0__"; RequireIs ("stan::is_foobar", "T0__")]], false)
           ~name:"foobar" ~return_type:Void ~inline:true ~body:[rethrow] () ) ]
    in
    let open Fmt in
    pf stdout "@[<v>%a@]" (list ~sep:cut Printing.pp_fun_defn) funs ;
    [%expect
      {|
              template <typename T0__,
                        stan::require_all_t<stan::is_foobar<T0__>>* = nullptr>
              inline void foobar();

              template <typename T0__, stan::require_all_t<stan::is_foobar<T0__>>*>
              inline void foobar()
              {
                try {
                  /* A potentially
                     long comment
                   */
                  foo = 3;
                } catch(const std::exception& e) {
                  stan::lang::rethrow_located(e, locations_array__[current_statement__]);
                }
              } |}]
end
