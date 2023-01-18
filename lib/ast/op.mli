open Typing

module Binop : sig
  type t = Equal | Less | Greater | Plus | Minus | Times | And | Or

  val to_string : t -> string
  val signature : t -> Ty_template.t
end
