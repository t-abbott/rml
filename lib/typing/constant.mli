(**
  Represents a constant value used in both terms and types.
*)
type t = Boolean of bool | Integer of int

val to_string : t -> string
