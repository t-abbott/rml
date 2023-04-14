open Typing

module Binop : sig
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

  val to_string : t -> string
  val to_ident_core : t -> Utils.Ident_core.t
  val signature : t -> Ty_template.t
end
