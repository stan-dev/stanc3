(** A set of data types representing the C++ we generate *)

open Core_kernel

type identifier = string [@@deriving sexp]

(** C++ types *)
type type_ =
  | Auto
  | Void
  | Int
  | Double
  | Complex of type_
  | TemplateType of identifier
  | StdVector of type_
      (** A std::vector. For Eigen Vectors, use [Matrix] with a row or column size of 1 *)
  | Array of type_ * int
  | Tuple of type_ list
  | TypeLiteral of identifier  (** Used for things like Eigen::Index *)
  | Matrix of type_ * int * int * Middle.Mem_pattern.t
  | Ref of type_
  | Const of type_
  | Pointer of type_
  | TypeTrait of identifier * type_ list
      (** e.g. stan::promote_scalar, stan:base_type *)
[@@deriving sexp]

module Types = struct
  (** Helpers for constructing types *)

  let local_scalar = TypeLiteral "local_scalar_t__"

  (** A [std::vector<t>] *)
  let std_vector t = StdVector t

  let bool = TypeLiteral "bool"
  let complex s = Complex s

  (** An [Eigen::Matrix<s, -1, 1>]*)
  let vector ?(mem_pattern = Middle.Mem_pattern.AoS) s =
    Matrix (s, -1, 1, mem_pattern)

  (** An [Eigen::Matrix<s, 1, -1>]*)
  let row_vector ?(mem_pattern = Middle.Mem_pattern.AoS) s =
    Matrix (s, 1, -1, mem_pattern)

  (** An [Eigen::Matrix<s, -1, -1>]*)
  let matrix ?(mem_pattern = Middle.Mem_pattern.AoS) s =
    Matrix (s, -1, -1, mem_pattern)

  (** A [std::string]*)
  let string = TypeLiteral "std::string"

  let size_t = TypeLiteral "size_t"
  let const_ref t = Const (Ref t)
  let const_char_array i = Array (Const (Pointer (TypeLiteral "char")), i)
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

type expr =
  | Literal of string  (** printed as-is *)
  | Var of identifier
  | VarRef of identifier
  | Parens of expr
  | FunCall of identifier * type_ list * expr list
  | MethodCall of expr * identifier * type_ list * expr list
  | StaticMethodCall of type_ * identifier * type_ list * expr list
  | Constructor of type_ * expr list  (** printed as [type(expr1, expr2, ...)]*)
  | InitializerExpr of type_ * expr list
      (** printed as [type{expr1, expr2, ...}]*)
  | ArrayLiteral of expr list
  | TernaryIf of expr * expr * expr
  | Cast of type_ * expr
  | Index of expr * expr
  | Deref of expr
  | AllocNew of type_ * expr list
  | OperatorNew of identifier * type_ * expr list
      (** See {{:https://en.cppreference.com/w/cpp/memory/new/operator_new}operator new} for distinctions between
          allocating and placing [new]s*)
  | Assign of expr * expr  (** NB: Not all exprs are valid lvalues! *)
  | StreamInsertion of expr * expr list  (** Corresponds to [operator<<] *)
  | BinOp of expr * operator * expr
  | PMinus of expr
  | Increment of expr
[@@deriving sexp]

module Exprs = struct
  (** Some helper values and functions *)

  (** Call a method on object, wrapping it in parentheses if
      it is not a variable
  *)
  let method_call obj name templates args =
    match obj with
    | Var _ -> MethodCall (obj, name, templates, args)
    | _ -> MethodCall (Parens obj, name, templates, args)

  let to_var s = Var s

  (** Turn an OCaml string into a quoted and escaped C++ string*)
  let literal_string s = Literal ("\"" ^ Cpp_str.escaped s ^ "\"")

  (** Equivalent to [std::vector<t>{e1,...,en}] *)
  let std_vector_init_expr t elements = InitializerExpr (StdVector t, elements)

  let fun_call name args = FunCall (name, [], args)
  let templated_fun_call name templates args = FunCall (name, templates, args)

  (** Helper for [std::numeric_limits<double>::quiet_NaN()] *)
  let quiet_NaN = fun_call "std::numeric_limits<double>::quiet_NaN" []

  (** Helper for [std::numeric_limits<int>::min()] *)
  let int_min = fun_call "std::numeric_limits<int>::min" []

  let static_cast type_ expr = FunCall ("static_cast", [type_], [expr])
end

module Expression_syntax = struct
  (** Some operators to make streams and method calls look more
      like the resultant C++ *)

  include Exprs

  (** A pun for the C++ [operator<<] *)
  let ( << ) a b = StreamInsertion (a, b)

  (** Method call: Call a no-argument method

      E.g. [foo.bar()]
  *)
  let ( .@!() ) obj name = method_call obj name [] []

  (** Method call: Call the named method with args

      E.g. [foo.bar(A1,...An)]
  *)
  let ( .@?() ) obj (name, args) = method_call obj name [] args

  (** Method call: Call the named method with template types and args

      E.g. [foo.bar<T1,...,Tn>(A1,...An)]
  *)
  let ( .@<>() ) obj (name, templates, args) =
    method_call obj name templates args

  (** Static method call: Call the named method with no arguments.

    E.g. [Foo::bar()]
  *)
  let ( |::! ) ty name = StaticMethodCall (ty, name, [], [])

  (** Static method call: Call the named method with args

    E.g. [Foo::bar(A1,...An)]
  *)
  let ( |::? ) ty (name, args) = StaticMethodCall (ty, name, [], args)

  (** Static method call: Call the named method with template types and args

    E.g. [Foo::bar<T1,...,Tn>(A1,...An)]
  *)
  let ( |::<> ) ty (name, templates, args) =
    StaticMethodCall (ty, name, templates, args)

  (** Pun for C++ [operator+(a,b)] *)
  let ( + ) a b = BinOp (a, Add, b)

  (** Pun for C++ [operator-(a,b)] *)
  let ( - ) a b = BinOp (a, Subtract, b)

  (** Pun for C++ [operator*(a,b)] *)
  let ( * ) a b = BinOp (a, Multiply, b)
end

type init =
  | Assignment of expr
  | Construction of expr list
  | InitializerList of expr list
  | Uninitialized
[@@deriving sexp]

type variable_defn =
  { static: bool [@default false]
  ; constexpr: bool [@default false]
  ; type_: type_
  ; name: identifier
  ; init: init [@default Uninitialized] }
[@@deriving make, sexp]

type stmt =
  | Expression of expr
  | VariableDefn of variable_defn
  | For of variable_defn * expr * expr * stmt
  | ForEach of (type_ * identifier) * expr * stmt
  | While of expr * stmt
  | IfElse of expr * stmt * stmt option
  | TryCatch of stmt list * (type_ * identifier) * stmt list
  | Block of stmt list
  | Return of expr option
  | Throw of expr
  | Break
  | Continue
  | Using of string * type_ option
  | Comment of string
[@@deriving sexp]

module Stmts = struct
  (** Helpers for common statement constructs *)

  (** Wrap the list of statements in a block if it isn't a singleton block already *)
  let block stmts = match stmts with [(Block _ as b)] -> b | _ -> Block stmts

  let unblock stmts = match stmts with [Block stmts] -> stmts | _ -> stmts

  (** Set up the try/catch logic for throwing an exception with
      its location set to the Stan program location. *)
  let rethrow_located stmts =
    let stmts = unblock stmts in
    match stmts with
    | [] -> []
    | _ ->
        [ TryCatch
            ( stmts
            , (Types.const_ref (TypeLiteral "std::exception"), "e")
            , [ Expression
                  (FunCall
                     ( "stan::lang::rethrow_located"
                     , []
                     , [ Var "e"
                       ; Index
                           (Var "locations_array__", Var "current_statement__")
                       ] ) ) ] ) ]

  let fori loopvar lower upper body =
    let init =
      make_variable_defn ~type_:Int ~name:loopvar ~init:(Assignment lower) ()
    in
    let stop = BinOp (Var loopvar, LEq, upper) in
    let incr = Increment (Var loopvar) in
    For (init, stop, incr, body)

  let if_block cond stmts = IfElse (cond, block stmts, None)

  (** Suppress warnings for a variable which may not be used. *)
  let unused s =
    [Comment "suppress unused var warning"; Expression (Cast (Void, Var s))]
end

module Decls = struct
  (** Declarations which get re-used often in the generated model *)

  let current_statement =
    VariableDefn
      (make_variable_defn ~type_:Int ~name:"current_statement__"
         ~init:(Assignment (Literal "0")) () )

  let dummy_var =
    VariableDefn
      (make_variable_defn ~type_:Types.local_scalar ~name:"DUMMY_VAR__"
         ~init:(Construction [Exprs.quiet_NaN])
         () )
    :: Stmts.unused "DUMMY_VAR__"

  let serializer_in =
    VariableDefn
      (make_variable_defn
         ~type_:(TypeTrait ("stan::io::deserializer", [Types.local_scalar]))
         ~name:"in__"
         ~init:(Construction [Var "params_r__"; Var "params_i__"])
         () )

  let serializer_out =
    VariableDefn
      (make_variable_defn
         ~type_:(TypeTrait ("stan::io::serializer", [Types.local_scalar]))
         ~name:"out__"
         ~init:(Construction [Var "vars__"])
         () )

  let lp_accum t =
    VariableDefn
      (make_variable_defn
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
  ; inline: bool [@default false]
  ; return_type: type_
  ; name: identifier
  ; args: (type_ * string) list
  ; cv_qualifiers: cv_qualifiers list [@default []]
  ; body: stmt list option }
[@@deriving make, sexp]

let split_fun_decl_defn (fn : fun_defn) =
  ( {fn with body= None}
  , {fn with templates_init= (fst fn.templates_init, false)} )

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

(** The Stan model class always has a non-default constructor and
      destructor *)
and class_defn =
  { class_name: identifier
  ; final: bool
  ; public_base: type_
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
  | GlobalVariableDefn of variable_defn
  | GlobalComment of string
  | GlobalUsing of string * type_ option
  | Namespace of identifier * defn list
  | Preprocessor of directive
[@@deriving sexp]

(* can't be derivided since it is simultaneously declared with non-records *)
let make_class_defn ~name ~public_base ?(final = true) ~private_members
    ~public_members ~constructor ?(destructor_body = []) () =
  { class_name= name
  ; public_base
  ; final
  ; private_members
  ; public_members
  ; constructor
  ; destructor_body }

let make_struct_defn ~param ~name ~body () = {param; struct_name= name; body}

(** Much like in C++, we define a translation unit as a list of definitions *)
type program = defn list [@@deriving sexp]

module Printing = struct
  (** Pretty-printing of the C++ type *)

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
    | StdVector t -> pf ppf "@[<2>std::vector<@,%a>@]" pp_type_ t
    | Array (t, i) -> pf ppf "@[<2>std::array<@,%a,@ %i>@]" pp_type_ t i
    | Tuple subtypes ->
        pf ppf "@[<2>std::tuple<@,%a>@]" (list ~sep:comma pp_type_) subtypes
    | TypeLiteral id -> pp_identifier ppf id
    | Matrix (t, i, j, mem_pattern) -> (
      match mem_pattern with
      | Middle.Mem_pattern.AoS ->
          pf ppf "Eigen::Matrix<%a,%i,%i>" pp_type_ t i j
      | Middle.Mem_pattern.SoA ->
          pf ppf "stan::math::var_value<Eigen::Matrix<double,%i,%i>>" i j )
    | Const t -> pf ppf "const %a" pp_type_ t
    | Ref t -> pf ppf "%a&" pp_type_ t
    | Pointer t -> pf ppf "%a*" pp_type_ t
    | TypeTrait (s, types) ->
        pf ppf "@[<2>%s<%a>@]" s (list ~sep:comma pp_type_) types

  let pp_requires ~default ppf requires =
    if not (List.is_empty requires) then
      let pp_require ppf (trait, name) = pf ppf "%s<%s>" trait name in
      pf ppf ",@ stan::require_all_t<@[%a@]>*%s"
        (list ~sep:comma pp_require)
        requires
        (if default then " = nullptr" else "")

  (**
   Pretty print a list of templates as [template <parameter-list>].name
   This function pools together [RequireIs] nodes into a [require_all_t]
  *)
  let pp_template ~default ppf template_parameters =
    let pp_basic_template ppf = function
      | `Typename name -> pf ppf "typename %s" name
      | `Bool name -> pf ppf "bool %s" name
      | `Require (requirement, args) ->
          pf ppf "%s<%a>*%s" requirement (list ~sep:comma string) args
            (if default then " = nullptr" else "") in
    if not (List.is_empty template_parameters) then
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

  let rec pp_expr ppf e =
    let maybe_templates ppf types =
      if not (List.is_empty types) then
        pf ppf "<@,%a>" (list ~sep:comma pp_type_) types in
    match e with
    | Literal s -> pf ppf "%s" s
    | Var id -> string ppf id
    | VarRef id -> pf ppf "&%s" id
    | Parens e -> pf ppf "(%a)" pp_expr e
    | Cast (t, e) -> pf ppf "@[(%a)@ %a@]" pp_type_ t pp_expr e
    | Constructor (t, es) ->
        pf ppf "@[<hov 2>%a(%a)@]" pp_type_ t (list ~sep:comma pp_expr) es
    | AllocNew (t, es) ->
        pf ppf "@[<hov 2>new %a(%a)@]" pp_type_ t (list ~sep:comma pp_expr) es
    | OperatorNew (ptr, t, es) ->
        pf ppf "@[<hov 2>new %a%a(%a)@]"
          (trailing_space (parens string))
          ("&" ^ ptr) pp_type_ t (list ~sep:comma pp_expr) es
    | ArrayLiteral es -> pf ppf "{%a}" (list ~sep:comma pp_expr) es
    | InitializerExpr (t, es) ->
        pf ppf "@[<hov 2>%a{%a}@]" pp_type_ t (list ~sep:comma pp_expr) es
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
    | Deref e -> pf ppf "*(%a)" pp_expr e
    | Assign (e1, e2) -> pf ppf "%a = %a" pp_expr e1 pp_expr e2
    | PMinus e -> pf ppf "-(%a)" pp_expr e
    | Increment e -> pf ppf "++%a" pp_expr e
    | BinOp (e1, op, e2) ->
        pf ppf "%a@ %a@ %a" pp_expr e1 pp_operator op pp_expr e2

  let pp_variable_defn ppf {static; constexpr; type_; name; init} =
    let pp_init ppf init =
      match init with
      | Uninitialized -> ()
      | Assignment e -> pf ppf " =@ %a" pp_expr e
      | Construction es -> pf ppf "(%a)" (list ~sep:comma pp_expr) es
      | InitializerList es ->
          pf ppf "{@[<hov>%a@]}" (list ~sep:comma pp_expr) es in
    let static = if static then "static " else "" in
    let constexpr = if constexpr then "constexpr " else "" in
    pf ppf "@[<hov 2>%s%s%a@ %s%a@]" static constexpr pp_type_ type_ name
      pp_init init

  let rec pp_stmt ppf s =
    match s with
    | Expression e -> pf ppf "@[<2>%a;@]" pp_expr e
    | Return e -> pf ppf "return %a;" (option (box pp_expr)) e
    | Throw e -> pf ppf "throw %a;" pp_expr e
    | Break -> string ppf "break;"
    | Continue -> string ppf "continue;"
    | VariableDefn vd -> pf ppf "%a;" pp_variable_defn vd
    | For (init, cond, incr, s) ->
        let pp ppf () =
          pf ppf "for (@[<hov>%a; %a; %a@])" pp_variable_defn init pp_expr cond
            pp_expr incr in
        pp_with_block pp ppf s
    | ForEach ((ty, name), set, s) ->
        let pp ppf () =
          pf ppf "for (@[<hov>%a %a: %a@])" pp_type_ ty pp_identifier name
            pp_expr set in
        pp_with_block pp ppf s
    | While (e, s) ->
        let pp ppf () = pf ppf "while (@[%a@])" pp_expr e in
        pp_with_block pp ppf s
    | IfElse (cond, thn, None) | IfElse (cond, thn, Some (Block [])) ->
        let pp_if ppf () = pf ppf "if (@[%a@])" pp_expr cond in
        pp_with_block pp_if ppf thn
    | IfElse (cond, thn, Some els) ->
        let pp_if ppf () = pf ppf "if (@[%a@])" pp_expr cond in
        pf ppf "%a %a" (pp_with_block pp_if) thn
          (pp_with_block ~indent:0 (any "else"))
          els
    | Block [] -> ()
    | Block stmts ->
        pf ppf "@[<v>@[<v 2>{@,%a@]@,}@]" (list ~sep:cut pp_stmt) stmts
    | Using (s, init) ->
        pf ppf "using %s%a;" s
          (option (fun ppf defn -> pf ppf " = %a" pp_type_ defn))
          init
    | Comment s ->
        if String.contains s '\n' then pf ppf "/@[<v>*@[@ %a@]@,@]*/" text s
        else pf ppf "//@[<h> %s@]" s
    | TryCatch (trys, (exn_ty, exn_name), thn) ->
        pf ppf "@[<v 2>try {@ %a@]@,@[<v 2>} catch (%a %a) {@ %a@]@,}"
          (list ~sep:cut pp_stmt) trys pp_type_ exn_ty pp_identifier exn_name
          (list ~sep:cut pp_stmt) thn

  (** When we know a block is here, we can do better pretty-printing

    @param indent: How much to indent the enclosing vbox. Usually 2, but
      set to zero for the [else] branch of an if-else to prevent
      over-indenting
  *)
  and pp_with_block ?(indent = 2) pp_wrapper ppf = function
    | Block [] ->
        let pp ppf () = pf ppf "%a {}" pp_wrapper () in
        (vbox ~indent pp) ppf ()
    | Block stmts ->
        let pp ppf () =
          pf ppf "%a {@ @[<v>%a@]" pp_wrapper () (list pp_stmt) stmts in
        pf ppf "%a@,}" (vbox ~indent pp) ()
    | stmt ->
        let pp ppf () = pf ppf "%a@ @[<v>%a@]" pp_wrapper () pp_stmt stmt in
        (vbox ~indent pp) ppf ()

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
    let pp_sig ppf () =
      pf ppf "@[%a%s%a@ %a(@[<hov>%a@])%a@]"
        (list (pp_template ~default:init))
        t
        (if inline then "inline " else "")
        pp_type_ return_type pp_identifier name
        (list ~sep:comma (pair ~sep:Fmt.sp pp_type_ pp_identifier))
        args (list ~sep:nop pp_cv) cv_qualifiers in
    match body with
    | Some stmts ->
        pf ppf "@[<v 2>%a {@,%a@]@,}" pp_sig () (list ~sep:cut pp_stmt) stmts
    | None -> pf ppf "%a;" pp_sig ()

  let pp_destructor ppf (name, body) =
    pf ppf "@[~%s()@ {%a}@]" name (list ~sep:cut pp_stmt) body

  let pp_constructor ppf (name, {args; init_list; body}) =
    let pp_init ppf (id, es) = pf ppf "%s(%a)" id (list ~sep:comma pp_expr) es in
    let pp_inits =
      if List.length init_list = 0 then Fmt.nop
      else fun ppf inits -> pf ppf ": @[%a@] " (list ~sep:comma pp_init) inits
    in
    pf ppf "@[<v 2>@[<hov 4>%s(@[%a@])@ %a@]{@,%a@]@,}" name
      (list ~sep:comma (pair ~sep:sp pp_type_ pp_identifier))
      args pp_inits init_list (list ~sep:cut pp_stmt) body

  let rec pp_directive ppf direct =
    match direct with
    | Include file -> pf ppf "#include <%s>" file
    | IfNDef (name, defns) ->
        pf ppf "@[<v>#ifndef %s@,%a@,#endif" name (list ~sep:cut pp_defn) defns
    | MacroApply (name, args) ->
        pf ppf "@[<h>%s(%a)@]" name (list ~sep:comma string) args

  and pp_class_defn ppf
      { class_name
      ; final
      ; public_base
      ; private_members
      ; constructor
      ; destructor_body
      ; public_members } =
    pf ppf
      "@[<v 1>class %s%s : public %a {@,\
       @[<v 1>private:@,\
       %a@]@,\
       @[<v 1>public:@,\
       %a@,\
       %a@,\
       %a@]@]@,\
       };"
      class_name
      (if final then " final" else "")
      pp_type_ public_base (list ~sep:cut pp_defn) private_members pp_destructor
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
    | GlobalVariableDefn vd -> pf ppf "%a;" pp_variable_defn vd
    | GlobalComment s ->
        if String.contains s '\n' then pf ppf "/@[<v>*@[@ %a@]@,@]*/" text s
        else pf ppf "//@[<h> %s@]" s
    | GlobalUsing (s, init) ->
        pf ppf "using %s%a;" s
          (option (fun ppf defn -> pf ppf " = %a" pp_type_ defn))
          init
    | Namespace (id, defns) ->
        pf ppf "@[<v>namespace %s {@,%a@]@,}" id (list ~sep:cut pp_defn) defns
    | Preprocessor d -> pp_directive ppf d

  let pp_program = vbox (list ~sep:cut pp_defn)
end

module Tests = struct
  let%expect_test "rethrow_located" =
    let s =
      [ Comment
          "A potentially very very very very very long comment which will be \
           on one line"; Comment "A potentially \n multiline comment"
      ; Expression (Assign (Var "foo", Literal "3")) ] in
    let rethrow = Stmts.rethrow_located s in
    Fmt.list Printing.pp_stmt Fmt.stdout rethrow ;
    [%expect
      {|
      try {
        // A potentially very very very very very long comment which will be on one line
        /* A potentially
           multiline comment
         */
        foo = 3;
      } catch (const std::exception& e) {
        stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      } |}]

  let%expect_test "if_else" =
    let s =
      [ Comment
          "A potentially very very very very very long comment which will be \
           on one line"; Comment "A potentially \n multiline comment"
      ; Expression (Assign (Var "foo", Literal "3")) ] in
    let ifelse = IfElse (Literal "1", Block s, Some (Block s)) in
    let if_empty = IfElse (Literal "1", Block [], None) in
    let if_noelse = IfElse (Literal "1", Block s, None) in
    Fmt.(vbox @@ list Printing.pp_stmt) Fmt.stdout [ifelse; if_empty; if_noelse] ;
    [%expect
      {|
        if (1) {
          // A potentially very very very very very long comment which will be on one line
          /* A potentially
             multiline comment
           */
          foo = 3;
        } else {
          // A potentially very very very very very long comment which will be on one line
          /* A potentially
             multiline comment
           */
          foo = 3;
        }
        if (1) {}
        if (1) {
          // A potentially very very very very very long comment which will be on one line
          /* A potentially
             multiline comment
           */
          foo = 3;
        } |}]

  let%expect_test "types" =
    let ts =
      let open Types in
      [ matrix (complex local_scalar); const_char_array 43
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
            (StreamInsertion (Constructor (Matrix Double 1 -1 AoS) ((Literal 3)))
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
           ~name:"foobar" ~return_type:Void ~inline:true ~body:rethrow () ) ]
    in
    let open Fmt in
    pf stdout "@[<v>%a@]" (list ~sep:cut Printing.pp_fun_defn) funs ;
    [%expect
      {|
              template <typename T0__,
                        stan::require_all_t<stan::is_foobar<T0__>>* = nullptr>
              inline void foobar();
              template <typename T0__, stan::require_all_t<stan::is_foobar<T0__>>*>
              inline void foobar() {
                try {
                  /* A potentially
                     long comment
                   */
                  foo = 3;
                } catch (const std::exception& e) {
                  stan::lang::rethrow_located(e, locations_array__[current_statement__]);
                }
              } |}]
end
