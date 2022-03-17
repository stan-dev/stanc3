open Fmt
open Statement_gen
open Middle

let pp_unused = fmt "(void) %s;  // suppress unused var warning"

(** Print the body of exception handling for functions *)
let pp_located ppf _ =
  pf ppf
    {|stan::lang::rethrow_located(e, locations_array__[current_statement__]);|}

(** [pp_located_error ppf (pp_body_block, body_block, err_msg)] surrounds [body_block]
    with a C++ try-catch that will rethrow the error with the proper source location
    from the [body_block] (required to be a [stmt_loc Block] variant).
  @param ppf A pretty printer.
  @param pp_body_block A pretty printer for the body block
  @param body A C++ scoped body block surrounded by squiggly braces.
  *)
let pp_located_error ppf (pp_body_block, body) =
  pf ppf "@ try %a" pp_body_block body ;
  string ppf " catch (const std::exception& e) " ;
  pp_block ppf (pp_located, ())

(** [pp_located_error_b] automatically adds a Block wrapper *)
let pp_located_error_b ppf body_stmts =
  pp_located_error ppf
    ( pp_statement
    , Stmt.Fixed.{pattern= Block body_stmts; meta= Locations.no_span_num} )
