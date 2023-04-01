(**
  Represents a constant value used in both terms and types.
*)
type t = Boolean of bool | Number of float

val to_string : t -> string
