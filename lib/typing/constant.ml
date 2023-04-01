(**
  Represents a constant value used in both terms and types.
*)
type t = Boolean of bool | Number of float

let to_string = function
  | Boolean b -> Bool.to_string b
  | Number n -> Float.to_string n
