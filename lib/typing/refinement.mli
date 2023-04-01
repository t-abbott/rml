open Refop
open Utils

type t_expr = t_expr_body Location.located

and t_expr_body =
  | Var of Ident.t
  | Const of Constant.t
  | Binop of Binop.t * t_expr * t_expr
  | IfThen of t_expr * t_expr * t_expr

type t = { bound_var : Ident.t; expr : t_expr }

val equal : t -> t -> bool
val to_string : t -> string

val boolean : bool -> t_expr_body
(** [boolean b] is an alias for [Const (Constant.Boolean b)] *)

val number : float -> t_expr_body
(** [number n] is an alias for [Const (Constant.Number n)]*)

val var : string -> t_expr_body
(** [var v] is an alias for [Var v]*)
