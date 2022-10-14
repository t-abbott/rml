{
    open Base
    open Parse

    exception LexError of string 
}

let whitespace = [' ' '\n' '\r']

let letter = ['a'-'z' 'A'-'Z']
let symbol = ['+' '-' '*' '/' '<' '>' '=' '!']
let digit = ['0'-'9']

let int = '-'? digit+
let decimal = '-'? digit+ '.' (digit+)?

let ident = (letter | '_') (letter | digit | symbol | '_')*

rule token = parse
| whitespace
    { token lexbuf }
| eof 
    { EOF } 

(* Code structure *)
| ";;"
    { SEMISEMI }
| "true"
    { TRUE }
| "false"
    { FALSE }
| '('
    { LPAREN }
| ')'
    { RPAREN }

(* Operators *)
| "!="
    { NEQUALS }
| '='
    { EQUALS }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { MULT }
| '/'
    { DIV }
| "&&"
    { AND }
| "||"
    { OR } 

(* Types *)
| "->"
    { ARROW }

(* Terms *)
| "fun"
    { FUN }
| "let"
    { LET }
| "in"
    { IN }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }

(* Literals *)
| "true"
    { TRUE }
| "false"
    { FALSE }
| int as i
    { INT (Int.of_string i) }
| ident as i 
    { IDENT i }
