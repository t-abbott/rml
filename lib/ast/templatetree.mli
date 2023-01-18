open Typing
open Op
open Utils
module PTree = Parsetree

type t = { body : t_body; ty : Ty_template.t; loc : Location.t }

and t_body =
  | Var of Ident.t
  | Integer of int
  | Boolean of bool
  | Binop of Binop.t * t * t
  | If of t * t * t
  | LetIn of Ident.t * t * t
  | Fun of Ident.t * t
  | Apply of t * t

val to_string : t -> string

type command = Expr of t | LetDef of Ident.t * t

val command_to_string : command -> string

type program = command list

val program_to_string : program -> string
