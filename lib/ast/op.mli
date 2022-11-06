open Types

module Binop : sig
  type t = Equal | Less | Greater | Plus | Minus | Times | And | Or

  val to_string : t -> string
  val signature : t -> Ty.t
end
