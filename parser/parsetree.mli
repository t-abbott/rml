open Rml
open Rml.Types
open Rml.Location

type ident = string

module Op : sig
  type t =
    | Equal 
    | Less
    | Greater
    | Plus 
    | Minus
    | Times 
    | And
    | Or

    val to_string : t -> string
end

type t = t_body located
and t_body =
  | Annotated of t * Ty.t
  | Var of ident
  | Integer of int
  | Boolean of bool
  | Binop of Op.t * t * t
  | If of t * t * t
  | LetIn of ident * t * t
  | Fun of t * t
  | Apply of t * t

val to_string : t -> string
  
(** 
  A top-level command in the program 
*)
type command = command_body Location.located
and command_body =
  | Expr of t
  | LetDef of ident * t

val command_to_string : command -> string
type program = command list

val program_to_string : program -> string
