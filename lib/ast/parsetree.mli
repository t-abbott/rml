open Types
open Utils

type ident = string

type t = t_body Location.located

and t_body =
  | Annotated of t * Ty.t
  | Var of ident
  | Integer of int
  | Boolean of bool
  | Binop of Op.Binop.t * t * t
  | If of t * t * t
  | LetIn of ident * t * t
  | Fun of t * t
  | Apply of t * t

val to_string : t -> string

type command = command_body Location.located
(** 
  A top-level command in the program 
*)

and command_body = Expr of t | LetDef of ident * t

val command_to_string : command -> string

type program = command list

val program_to_string : program -> string
