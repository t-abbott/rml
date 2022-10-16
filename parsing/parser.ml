open Stdlib

let lex_and_parse lexbuf = 
  try
    Parse.file Lex.token lexbuf
  with
  | _ ->
    failwith "got an exception while parsing"

let parse_file filename = 
  let chan = open_in filename in 
  let lexbuf = Lexing.from_channel chan in 
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename};
  lex_and_parse lexbuf

let parse_string s = 
  let lexbuf = Lexing.from_string s in
  lex_and_parse lexbuf
