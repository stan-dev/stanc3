type t =
  | IdentifierIsKeyword of Mir.location_span * string
  | IdentifierIsModelName of Mir.location_span * string
  | IdentifierIsStanMathName of Mir.location_span * string
  | IdentifierInUse of Mir.location_span * string
  | IdentifierNotInScope of Mir.location_span * string
  | InvalidIndex of Mir.location_span * Mir.unsizedtype
  | IllTypedIfReturnTypes of
      Mir.location_span * Mir.returntype * Mir.returntype
  | IllTypedTernaryIf of
      Mir.location_span * Mir.unsizedtype * Mir.unsizedtype * Mir.unsizedtype
  | IllTypedNRFunction of Mir.location_span * string
  | IllTypedFunctionApp of Mir.location_span * string * Mir.unsizedtype list
  | IllTypedNotAFunction of Mir.location_span * string
  | IllTypedNoSuchFunction of Mir.location_span * string
  | IllTypedBinOp of
      Mir.location_span * Ast.operator * Mir.unsizedtype * Mir.unsizedtype
  | IllTypedPrefixOp of Mir.location_span * Ast.operator * Mir.unsizedtype
  | IllTypedPostfixOp of Mir.location_span * Ast.operator * Mir.unsizedtype
  | FnMapRect of Mir.location_span * string
  | FnConditioning of Mir.location_span
  | FnTargetPlusEquals of Mir.location_span
  | FnRng of Mir.location_span

val to_exception : t -> 'a
