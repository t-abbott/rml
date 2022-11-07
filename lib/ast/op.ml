open Typing.Ty

let bi = builtin
let int_int_int = bi (TArrow ([ bi TInt; bi TInt ], bi TInt))
let int_int_bool = bi (TArrow ([ bi TInt; bi TInt ], bi TBool))
let bool_bool_bool = bi (TArrow ([ bi TBool; bi TBool ], bi TBool))

module Binop = struct
  type t = Equal | Less | Greater | Plus | Minus | Times | And | Or

  let to_string = function
    | Equal -> "="
    | Less -> "<"
    | Greater -> ">"
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | And -> "&&"
    | Or -> "||"

  let signature = function
    | Equal -> int_int_bool
    | Less -> int_int_bool
    | Greater -> int_int_bool
    | Plus -> int_int_int
    | Minus -> int_int_int
    | Times -> int_int_int
    | And -> bool_bool_bool
    | Or -> bool_bool_bool
end
