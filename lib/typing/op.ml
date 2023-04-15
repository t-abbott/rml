open Utils

module Binop = struct
  type t =
    | Equal
    | Less
    | Greater
    | Plus
    | Minus
    | Times
    | Div
    | Mod
    | And
    | Or

  let to_string = function
    | Equal -> "="
    | Less -> "<"
    | Greater -> ">"
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"
    | Mod -> "%"
    | And -> "&&"
    | Or -> "||"

  let of_string = function
    | "=" -> Equal
    | "<" -> Less
    | ">" -> Greater
    | "+" -> Plus
    | "-" -> Minus
    | "*" -> Times
    | "/" -> Div
    | "%" -> Mod
    | "&&" -> And
    | "||" -> Or
    | _ -> failwith "invalid operator"

  let to_ident_core op = Ident_core.BuiltinSym (to_string op)
end
