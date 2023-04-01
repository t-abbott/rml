module Binop : sig
  (** Represents a binary operator over refinements *)
  type t = Less | Greater | Equal | And | Or | Add | Sub | Mod

  val to_string : t -> string
  (** Converts a [Refop.t] to a string *)
end
