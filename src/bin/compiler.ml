open Core_kernel
open Stanc_mir
open Stanc_backend
open Stanc_frontend
open Stanc_optimization

type 'out verbose = 
    { ast : Ast.untyped_program option
    ; typed_ast : Ast.typed_program option 
    ; mir : Mir.typed_prog option
    ; optimized_mir : Mir.typed_prog option 
    ; out : 'out option
    }

type error = 
    | Lexing of string * Ast.location
    | Include of string * Ast.location
    | Parsing of string * Ast.location_span
    | SemanticError of string * Ast.location_span

let error_of_parse_error = function 
    | Errors.Lexing(err,loc) -> Lexing(err,loc)
    | Include(err,loc) -> Include(err,loc)
    | Parsing(err,locspan) -> Parsing(err,locspan)

let error_of_semantic_error (err,locspan) = 
    SemanticError(err,locspan)


module type SIG = sig
    type out

    val ast : string -> (Ast.untyped_program,error) Result.t

    val typed_ast : Ast.untyped_program -> (Ast.typed_program,error) Result.t

    val mir : string -> Ast.typed_program -> Mir.typed_prog

    val compile: string -> (out,error) Result.t

    val compile_verbose: string -> (out verbose,out verbose * error) Result.t

end 

module Make
    (Backend: Backend.SIG) 
    (Optimize: Optimization.SIG): SIG 
    with type out := Backend.out = 
struct
    let ast filename =
        try 
            Parse.parse_file Parser.Incremental.program filename
            |> Ok
        with Errors.SyntaxError err ->
            error_of_parse_error err
            |> Error

    let typed_ast ast =
        try 
            Semantic_check.semantic_check_program ast
            |> Ok
        with Errors.SemanticError err ->
            error_of_semantic_error err
            |> Error

    let mir filename typed_ast = 
        Ast_to_Mir.trans_prog filename typed_ast

    let compile filename = Result.(
        ast filename
        >>= (fun ast -> 
                typed_ast ast
                >>=
                    (fun typed_ast -> 
                        mir filename typed_ast
                        |> Optimize.optimize
                        |> Backend.generate
                        |> Ok
                    )
            )
    )



    let empty : Backend.out verbose = 
        { ast = None 
        ; typed_ast = None 
        ; mir = None 
        ; optimized_mir = None
        ; out = None 
        }

    (* Handrolled StateT for verbose output *)
    let compile_verbose filename =
        let st = empty in 
        match ast filename with 
        | Error err -> Error (st,err)
        | Ok ast -> 
            let st = {empty with ast = Some ast } in 
            match typed_ast ast with 
            | Error err -> Error (st,err)
            | Ok typed_ast -> 
                let mir =  mir filename typed_ast in
                let opt_mir = Optimize.optimize mir in 
                let out = Backend.generate opt_mir in 
                let st = 
                    { st with 
                        typed_ast = Some typed_ast 
                        ; mir = Some mir
                        ; optimized_mir = Some opt_mir
                        ; out = Some out
                    }
                in
                Ok st

end