{
  open Parse
}

let whitespace = [' ' '\t' '\r']

let letter = ['a'-'z' 'A'-'Z']
let symbol = ['+' '-' '*' '/' '<' '>' '=' '!' '%']
let digit = ['0'-'9']

let decimal = '-'? digit+ ('.' digit+)?

let var = (letter | '_') (letter | digit | symbol | '_')*

rule token = parse
    whitespace      { token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | decimal         { NUM (Float.of_string(Lexing.lexeme lexbuf)) }
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

{
}