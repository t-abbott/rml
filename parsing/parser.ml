open Stdlib

let parse_string s = 
  let lexbuf = Lexing.from_string s 
  in Parse.file Lex.token lexbuf

let parse_file filename = 
  let chan = open_in filename in 
  let lexbuf = Lexing.from_channel chan in 
  Parse.file Lex.token lexbuf
