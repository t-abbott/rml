(**
  Represents a constant value used in both terms and types.
*)
type t = Boolean of bool | Integer of int

let to_string = function 
  | Boolean b -> Bool.to_string b
  | Integer i -> Int.to_string i