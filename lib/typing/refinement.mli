type t = Var of string | Const of Constant.t

val equal : t -> t -> bool
val to_string : t -> string

val boolean : bool -> t
(** Alias for [Const (Constant.Boolean ...)] *)
