module Location_span = Middle.Location_span
module Location = Middle.Location

type t =
  | StancJsInclude of string
  | JacobianFunCallDataOnly of Location_span.t * string option
  | LpInTransformedParam of
      Location_span.t (* https://github.com/stan-dev/stanc3/issues/1482 *)
  | IntDivide of Location_span.t * (Format.formatter -> unit)
  | MatrixPower of Location_span.t * (Format.formatter -> unit)
  | ChainedCompare of
      Location_span.t * (Format.formatter -> unit) * (Format.formatter -> unit)
  | AssignToSelf of Location_span.t * Location_span.t
  | InitializeWithSelf of Location_span.t * Location_span.t
  | Unreachable of Location_span.t * Ast.complete
  | EmptyFile of Location_span.t
  | ForwardDecl of Location_span.t
  | LkjCov of Location_span.t
  | FunctionDeprecation of Location_span.t * string * (int * int) * string
  | OdeDeprecation of Location_span.t * string * (int * int) * string
  | Pedantic of Location_span.t * string

let stancjs_bad_include msg = StancJsInclude msg
let jacobian_dataonly loc alt = JacobianFunCallDataOnly (loc, alt)
let lp_in_transparam loc = LpInTransformedParam loc
let int_divide loc hint = IntDivide (loc, hint)
let matrix_power loc hint = MatrixPower (loc, hint)
let compare_chain loc hint1 hint2 = ChainedCompare (loc, hint1, hint2)
let assign_to_self lhs rhs = AssignToSelf (lhs, rhs)
let initialize_with_self lhs rhs = InitializeWithSelf (lhs, rhs)
let unreachable_statement loc c = Unreachable (loc, c)
let empty_file loc = EmptyFile loc
let forward_declaration loc = ForwardDecl loc
let lkj_cov_deprecation loc = LkjCov loc

let function_deprecation loc name version rename =
  FunctionDeprecation (loc, name, version, rename)

let ode_deprecation loc name version rename =
  OdeDeprecation (loc, name, version, rename)

let pedantic_warning (span, msg) = Pedantic (span, msg)

let canonicalize =
  Grace.Diagnostic.Message.createf "%a" Fmt.text
    "This can be automatically changed using the canonicalize flag for stanc."

let to_grace ?printed_filename ?code warn =
  let make, dedup = Diagnostic.(make, dedup_labels) in
  let open Grace.Diagnostic in
  let make_warning loc ?labels ?notes ?(summary : Message.t option) primary =
    make ?printed_filename ?code ?labels ?notes ?summary loc Severity.Warning
      primary in
  match warn with
  | StancJsInclude msg ->
      create Warning
        (Message.createf
           "@[<v>stanc.js failed to parse included file mapping:@ %s@]" msg)
  | Pedantic (loc, msg) ->
      make_warning loc "%a" Fmt.lines (Fmt.str "@[%a@]" Fmt.text msg)
  | EmptyFile loc ->
      let summary =
        Message.create
          "Empty model detected; this is a valid Stan model but likely \
           unintended!" in
      make_warning loc ~summary "Empty model."
  | IntDivide (loc, hint) ->
      let notes =
        [ Message.createf "%a" Fmt.text
            "If rounding is intended please use the integer division operator \
             %%/%%." ] in
      let summary =
        Message.createf
          "@[<v>Found integer division. The value will be rounded towards \
           zero.@]" in
      make_warning ~notes ~summary loc
        "If rounding is not desired you can write the division as@ @[%t@]" hint
  | MatrixPower (loc, hint) ->
      let summary =
        Message.createf
          "@[<v>Found matrix^scalar. matrix ^ number is interpreted as \
           element-wise exponentiation. If this is intended, you can silence \
           this warning by using elementwise operator .^@]" in
      make_warning ~summary loc
        "If you intended matrix exponentiation, use @[%t@] instead." hint
  | ChainedCompare (loc, this, alt) ->
      let notes =
        [ Message.createf "%a" Fmt.text
            "You can silence this warning by adding explicit parentheses."
        ; canonicalize ] in
      let summary = Message.createf "@[<v>Found chained comparison.@]" in
      make_warning ~notes ~summary loc
        "This is interpreted as @[%t@].@ Consider if the intended meaning was \
         @[%t@] instead."
        this alt
  | LpInTransformedParam loc ->
      let notes =
        [ Message.createf "%a" Fmt.text
            "Use an _jacobian function instead, as this allows change of \
             variable adjustments which are conditionally enabled by the \
             algorithms." ] in
      let summary =
        Message.create
          "Using _lp functions in transformed parameters is deprecated and \
           will be disallowed in the future." in
      make_warning ~notes ~summary loc "@[%a@]" Fmt.text
        "_lp function call in transformed parameters."
  | JacobianFunCallDataOnly (loc, alt) ->
      let notes =
        match alt with
        | None -> []
        | Some alt -> [Message.createf "Consider using %s instead" alt] in
      let summary =
        Message.createf "%a" Fmt.text
          "Calling a _jacobian function without any parameter arguments still \
           applies the Jacobian adjustments, ensure this is intentional?" in
      make_warning ~notes ~summary loc "_jacobian with no parameters"
  | Unreachable (span, cont) ->
      let range, included =
        Diagnostic.range_of_loc_span ?printed_filename ?code
          {span with end_loc= span.begin_loc} in
      let message = "Unreachable statement found, is this intended?" in
      let message ppf = Fmt.lines ppf message in
      let label ppf = Fmt.lines ppf "Never reached" in
      let labels = Label.primary ~range label :: included in
      let sec span msg =
        let range, included =
          Diagnostic.range_of_loc_span ?printed_filename ?code span in
        Label.secondary ~range (Grace.Diagnostic.Message.create msg) :: included
      in
      let rec get = function
        | Ast.CIfElse (i, e) -> get i @ get e
        | CBreak span -> sec span "exits the loop"
        | CContinue span -> sec span "returns to the beginning of the loop"
        | CReturn span -> sec span "exits the function"
        | CReject span -> sec span "stops this model evaluation"
        | CFatalError span -> sec span "halts the algorithm"
        | CWhile span -> sec span "endless loop" in
      let labels = dedup (labels @ get cont) in
      create Warning ~labels message
  | AssignToSelf (lhs, rhs) ->
      let labels =
        Diagnostic.context ?printed_filename ?code lhs "Assigned to itself."
      in
      let summary = Message.create "Assignment of variable to itself." in
      make_warning ~labels ~summary rhs "Value here."
  | InitializeWithSelf (lhs, rhs) ->
      let labels =
        Diagnostic.context ?printed_filename ?code lhs "Assigned to itself."
      in
      let summary =
        Message.create
          "Assignment of variable to itself during declaration. This is almost \
           certainly a bug." in
      make_warning ~labels ~summary rhs "Value here."
  | ForwardDecl loc ->
      let summary =
        Message.createf "%a" Fmt.lines
          (Fmt.str "@[%a@]" Fmt.text
             "Functions do not need to be declared before definition; all user \
              defined function names are always in scope regardless of \
              definition order.") in
      make_warning ~summary loc "Forward declaration."
  | LkjCov loc ->
      let notes =
        [ Message.createf
            "Use lkj_corr with an independent lognormal distribution on the \
             scales, see:@ \
             https://mc-stan.org/docs/reference-manual/deprecations.html#lkj_cov-distribution"
        ] in
      make_warning ~notes loc
        "lkj_cov is deprecated and will be removed in Stan 3.0."
  | FunctionDeprecation (loc, name, (major, minor), rename) ->
      let notes = [canonicalize] in
      let summary =
        Message.createf
          "%s is deprecated and will be removed in Stan %d.%d. Use %s instead."
          name major minor rename in
      make_warning ~notes ~summary loc "Use %s instead." rename
  | OdeDeprecation (loc, name, (major, minor), rename) ->
      let notes =
        [ Message.createf
            "The new interface is slightly different, see:@ \
             https://mc-stan.org/users/documentation/case-studies/convert_odes.html"
        ] in
      let summary =
        Message.createf
          "%s is deprecated and will be removed in Stan %d.%d. Use %s instead."
          name major minor rename in
      make_warning ~notes ~summary loc "Use %s instead." rename

let pp ?printed_filename ?code ppf warn =
  let diagnostic = to_grace ?printed_filename ?code warn in
  match warn with
  | EmptyFile _ | Pedantic _ ->
      Fmt.pf ppf "%a@." Diagnostic.pp_compact diagnostic
  | _ -> Fmt.pf ppf "%a@." Diagnostic.pp diagnostic

let pp_warnings ?printed_filename ?code ppf warnings =
  if not (List.is_empty warnings) then
    Fmt.(
      pf ppf "@[<v>%a@]" (list ~sep:cut (pp ?printed_filename ?code)) warnings)
