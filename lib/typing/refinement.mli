open Utils

type t = Var of Ident.t | Const of Constant.t

val equal : t -> t -> bool
val to_string : t -> string

val boolean : bool -> t
(** Alias for [Const (Constant.Boolean ...)] *)

val var : string -> t
(** [var v] is an alias for Var v *)
