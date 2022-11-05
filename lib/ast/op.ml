open Types.Ty

(*
  Operator types are curried until I add multi-argument functions  
*)

let bool_bool = builtin (TArrow (builtin TBool, builtin TBool))
let int_int = builtin (TArrow (builtin TInt, builtin TInt))
let int_bool = builtin (TArrow (builtin TInt, builtin TBool))
let int_int_int = builtin (TArrow (builtin TInt, int_int))
let int_int_bool = builtin (TArrow (builtin TInt, int_bool))
let bool_bool_bool = builtin (TArrow (builtin TBool, bool_bool))

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
