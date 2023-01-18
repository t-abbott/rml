(*
  [Api] sucks as a module name but dune doesn't handle nested libraries,
  [Parsing] already exists in the stdlib, and there are only so many things
  you can call variants of [Parse]/[Parser] until you need to start renaming
  stuff. TODO rename this if I can figure out how to make dune work or come
  up with something better.  
*)

open Stdlib
open Utils

let lex_and_parse lexbuf =
  try Parse.file Lex.token lexbuf
  with Parse.Error ->
    let start = lexbuf.lex_start_p in
    let curr = lexbuf.lex_curr_p in
    raise (Errors.ParseError ("unrecognised error", Location.from start curr))

let parse_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  lex_and_parse lexbuf

let parse_string s =
  let lexbuf = Lexing.from_string s in
  lex_and_parse lexbuf
