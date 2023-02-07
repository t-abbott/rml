module Binop = struct
  type t = Less | Greater | Equal | And | Or | Add
  type dan = int list

  let to_string = function
    | Less -> "<"
    | Greater -> ">"
    | Equal -> "="
    | And -> "&&"
    | Or -> "||"
    | Add -> "+"
end

let x : Binop.t = Greater
let y : Binop.dan = []
