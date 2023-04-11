{
  open Parse
  open Lexing

  (* taken from 
    https://github.com/mukul-rathi/bolt/blob/a81627f95af6577cd465df09290a51c9d469c667/src/frontend/parsing/lexer.mll#L15
  *)
  let next_line lexbuf = 
    let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
            }
}

let whitespace = [' ' '\t' '\r']
let newline = "\n"

let letter = ['a'-'z' 'A'-'Z']
let symbol = ['+' '-' '*' '/' '<' '>' '=' '!' '%']
let digit = ['0'-'9']

let decimal = '-'? digit+ ('.' digit+)?

let var = (letter | '_') (letter | digit | symbol | '_')*


rule token = parse
    whitespace      { token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | decimal         { NUM (Float.of_string(Lexing.lexeme lexbuf)) }
  | "//"            { read_comment lexbuf }
  | "num"           { TNUM }
  | "bool"          { TBOOL }
  | ':'             { COLON }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "fun"           { FUN }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "let"           { LET }
  | "val"           { VAL }
  | "in"            { IN } 
  | ";;"            { SEMISEMI }
  | '='             { EQUAL }
  | '<'             { LESS }
  | '>'             { GREATER }
  | "&&"            { AND }
  | "||"            { OR }
  | '|'             { LINE }
  | "->"            { ARROW }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '['             { LBRACKET }
  | ']'             { RBRACKET }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '/'             { DIV }
  | '*'             { TIMES }
  | '%'             { MOD }
  | var             { VAR (Lexing.lexeme lexbuf) }
  | eof             { EOF }

and read_comment = parse
  | newline { next_line lexbuf; token lexbuf } 
  | eof { EOF }
  | _ { read_comment lexbuf }
 
{
}