open Utils

module Binop : sig
  type t = Less | Greater | Equal | And | Or | Add

  val to_string : t -> string
end

type t = t_body Location.located

and t_body =
  | Var of Ident.t
  | Const of Constant.t
  | Binop of Binop.t * t * t
  | IfThen of t * t * t

val equal : t -> t -> bool
val to_string : t -> string

val boolean : bool -> t_body
(** [boolean b] is an alias for [Const (Constant.Boolean b)] *)

val number : int -> t_body
(** [number n] is an alias for [Const (Constant.Integer n)]*)

val var : string -> t_body
(** [var v] is an alias for [Var v]*)
