module Location_span = Middle.Location_span
module Location = Middle.Location

type t =
  | JacobianFunCallDataOnly of Location_span.t * string option
  | LpInTransformedParam of
      Location_span.t (* https://github.com/stan-dev/stanc3/issues/1482 *)
  | IntDivide of Location_span.t * string
  | MatrixPower of Location_span.t * string
  | ChainedCompare of Location_span.t * string * string
  | AssignToSelf of Location_span.t * Location_span.t
  | InitializeWithSelf of Location_span.t * Location_span.t
  | Unreachable of Location_span.t * Ast.complete
  | EmptyFile
  | Deprecation of Location_span.t * string
  | Pedantic of Location_span.t * string

let canonicalize () =
  Grace.Diagnostic.Message.createf "%a" Fmt.text
    "This can be automatically changed using the canonicalize flag for stanc."

let to_grace ?printed_filename ?code warn =
  let make, dedup = Diagnostic.(make, dedup_labels) in
  let open Grace.Diagnostic in
  let make_warning loc ?labels ?notes ?(summary : Message.t option) primary =
    make ?printed_filename ?code ?labels ?notes ?summary loc Severity.Warning
      primary in
  match warn with
  | Deprecation (x, y) | Pedantic (x, y) ->
      make_warning x "%a" Fmt.lines (Fmt.str "@[%a@]" Fmt.text y)
  | EmptyFile ->
      createf Warning "%a" Fmt.text
        "Empty model detected; this is a valid Stan model but likely \
         unintended!"
  | IntDivide (x, y) ->
      let notes =
        [ Message.createf "%a" Fmt.text
            "If rounding is intended please use the integer division operator \
             %%/%%." ] in
      let summary =
        Message.createf
          "@[<v>Found integer division. The value will be rounded towards \
           zero.@]" in
      make_warning ~notes ~summary x
        "If rounding is not desired you can write the division as@ @[%s@]" y
  | MatrixPower (x, y) ->
      let summary =
        Message.createf
          "@[<v>Found matrix^scalar. matrix ^ number is interpreted as \
           element-wise exponentiation. If this is intended, you can silence \
           this warning by using elementwise operator .^@]" in
      make_warning ~summary x
        "If you intended matrix exponentiation, use %s instead." y
  | ChainedCompare (x, y, z) ->
      let notes =
        [ Message.createf "%a" Fmt.text
            "You can silence this warning by adding explicit parentheses."
        ; canonicalize () ] in
      let summary = Message.createf "@[<v>Found chained comparison.@]" in
      make_warning ~notes ~summary x
        "This is interpreted as %s Consider if the intended meaning was %s \
         instead."
        y z
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
        | CReject span -> sec span "stops this sample evaluation"
        | CFatalError span -> sec span "stops sampling"
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

let pp ?printed_filename ?code ppf warn =
  let diagnostic = to_grace ?printed_filename ?code warn in
  match warn with
  | Pedantic _ | Deprecation _ ->
      Fmt.pf ppf "%a@." Diagnostic.pp_compact diagnostic
  | _ -> Fmt.pf ppf "%a@." Diagnostic.pp diagnostic

let pp_warnings ?printed_filename ?code ppf warnings =
  if not (List.is_empty warnings) then
    Fmt.(
      pf ppf "@[<v>%a@]" (list ~sep:cut (pp ?printed_filename ?code)) warnings)
