module Binop = struct
  type t = Less | Greater | Equal | And | Or | Add

  let to_string = function
    | Less -> "<"
    | Greater -> ">"
    | Equal -> "="
    | And -> "&&"
    | Or -> "||"
    | Add -> "+"
end