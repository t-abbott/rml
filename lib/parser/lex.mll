{
  open Base
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

    let error lexbuf = 
        let loc = Utils.Location.from lexbuf.lex_start_p lexbuf.lex_curr_p in 
        let token = Lexing.lexeme lexbuf in 
        let msg = Printf.sprintf "unrecognised token \"%s\"" token in
        raise (Errors.LexError (msg, loc)) 
}

let whitespace = [' ' '\t' '\r']
let newline = "\n"

let letter = ['a'-'z' 'A'-'Z']
let symbol = ['+' '-' '*' '/' '<' '>' '=' '!' '%']
let digit = ['0'-'9']

let number = '-'? digit+

let var = (letter | '_') (letter | digit | symbol | '_')*


rule token = parse
    whitespace      { token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | number          { INT (Int.of_string (Lexing.lexeme lexbuf)) }
  | "//"            { read_comment lexbuf }
  | "int"           { TINT }
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
  | _               { error lexbuf }

and read_comment = parse
  | newline { next_line lexbuf; token lexbuf } 
  | eof { EOF }
  | _ { read_comment lexbuf }
 
{
}