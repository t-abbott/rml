{
  open Parse
}

let whitespace = [' ' '\t' '\r']

let letter = ['a'-'z' 'A'-'Z']
let symbol = ['+' '-' '*' '/' '<' '>' '=' '!']
let digit = ['0'-'9']

let int = '-'? digit+
let decimal = '-'? digit+ '.' (digit+)?

let var = (letter | '_') (letter | digit | symbol | '_')*

rule token = parse
    whitespace      { token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | int             { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "fun"           { FUN }
  | "is"            { IS }
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
  | "->"            { ARROW }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | var             { VAR (Lexing.lexeme lexbuf) }
  | eof             { EOF }

{
}