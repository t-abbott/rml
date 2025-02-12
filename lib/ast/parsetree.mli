open Typing
open Utils

type t = t_body Location.located

and t_body =
  | Annotated of t * Ty_template.t
  | Var of Ident.t
  | Integer of int
  | Boolean of bool
  | Binop of Op.Binop.t * t * t
  | If of t * t * t
  | LetIn of Ident.t * t * t
  | ValIn of Ident.t * Ty_template.t * t
  | Fun of Ident.t list * t
  | Apply of t * t list

val to_string : t -> string

type command = command_body Location.located
(** 
  A top-level command in the program 
*)

and command_body =
  | Expr of t
  | LetDef of Ident.t * t
  | ValDef of Ident.t * Ty_template.t

val command_to_string : command -> string

type program = command list

val program_to_string : program -> string
