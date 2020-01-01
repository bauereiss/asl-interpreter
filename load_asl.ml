(****************************************************************
 * Functions for processing ASL files
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

open Asl_ast

module Lexer  = Lexer
module Parser = Asl_parser
module TC     = Tcheck
module PP     = Asl_parser_pp
module AST    = Asl_ast

open Lexersupport
open Lexing

let opt_verbose = ref false

let read_file (filename : string) (isPrelude: bool): AST.declaration list =
    if !opt_verbose then Printf.printf "Processing %s\n" filename;
    let inchan = open_in filename in
    let lexbuf = Lexing.from_channel inchan in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let t =
        (try
            (* Apply offside rule to raw token stream *)
            let lexer = offside_token Lexer.token in

            (* Run the parser on this line of input. *)
            if !opt_verbose then Printf.printf "- Parsing %s\n" filename;
            Parser.declarations_start lexer lexbuf
        with
        | Parse_error_locn(l, s) -> begin
            Printf.printf "  Syntax error %s at %s\n" s (pp_loc l);
            exit 1
        end
        | PrecedenceError(loc, op1, op2) -> begin
            Printf.printf "  Syntax error: operators %s and %s require parentheses to disambiguate expression at location %s\n"
                (Utils.to_string (Asl_parser_pp.pp_binop op1))
                (Utils.to_string (Asl_parser_pp.pp_binop op2))
                (pp_loc loc);
            exit 1
        end
        | Parser.Error -> begin
            let curr = lexbuf.Lexing.lex_curr_p in
            let tok = Lexing.lexeme lexbuf in
            Printf.printf "  Parser error at %s '%s'\n" (AST.pp_lexing_position curr) tok;
            exit 1
        end
        )
    in
    close_in inchan;

    if false then PPrint.ToChannel.pretty 1.0 60 stdout (PP.pp_declarations t);
    if !opt_verbose then Printf.printf "  - Got %d declarations from %s\n" (List.length t) filename;

    let t' =
        try
            if !opt_verbose then Printf.printf "- Typechecking %s\n" filename;
            let t' = TC.tc_declarations isPrelude t in
            t'
        with
        | TC.UnknownObject (loc, what, x) ->
            Printf.printf "  %s: Type error: Unknown %s %s\n" (pp_loc loc) what x;
            exit 1
        | TC.DoesNotMatch (loc, what, x, y) ->
            Printf.printf "  %s: Type error: %s %s does not match %s\n" (pp_loc loc) what x y;
            exit 1
        | TC.IsNotA (loc, what, x) ->
            Printf.printf "  %s: Type error: %s is not a %s\n" (pp_loc loc) x what;
            exit 1
        | TC.Ambiguous (loc, what, x) ->
            Printf.printf "  %s: Type error: %s %s is ambiguous\n" (pp_loc loc) what x;
            exit 1
        | TC.TypeError (loc, what) ->
            Printf.printf "  %s: Type error: %s\n" (pp_loc loc) what;
            exit 1
    in

    if false then PPrint.ToChannel.pretty 1.0 60 stdout (PP.pp_declarations t');
    if !opt_verbose then Printf.printf "  - Got %d typechecked declarations from %s\n" (List.length t') filename;

    if !opt_verbose then Printf.printf "Finished %s\n" filename;
    flush stdout;
    t'

let read_spec (filename : string): AST.declaration list =
    let r: AST.declaration list list ref = ref [] in
    let inchan = open_in filename in
    (try
        while true do
            let t = read_file (input_line inchan) false in
            r := t :: !r
        done
    with
    | End_of_file ->
        close_in inchan
    );
    List.concat (List.rev !r)
