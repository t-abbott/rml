open Typing
open Typing.Ty

let t_int = unrefined Base_ty.TInt ~source:Builtin
let t_bool = unrefined Base_ty.TBool ~source:Builtin
let int_int_int = builtin (RArrow ([ t_int; t_int ], t_int))
let int_int_bool = builtin (RArrow ([ t_int; t_int ], t_bool))
let bool_bool_bool = builtin (RArrow ([ t_bool; t_bool ], t_bool))

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
