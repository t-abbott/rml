module Binop = struct
  type t = Less | Greater | Equal | And | Or | Add | Sub | Mod

  let to_string = function
    | Less -> "<"
    | Greater -> ">"
    | Equal -> "="
    | And -> "&&"
    | Or -> "||"
    | Add -> "+"
    | Sub -> "-"
    | Mod -> "%"
end
