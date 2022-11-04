open Types
open Op
open Utils
module PTree = Parsetree

type ident = string

type t = { body : t_body; ty : Ty.t; loc : Location.t }

and t_body =
  | Var of ident
  | Integer of int
  | Boolean of bool
  | Binop of Binop.t * t * t
  | If of t * t * t
  | LetIn of ident * t * t
  | Fun of ident * t
  | Apply of t * t

val to_string : t -> string

type command = Expr of t | LetDef of Ident.t * t

val command_to_string : command -> string

type program = command list

val program_to_string : program -> string
