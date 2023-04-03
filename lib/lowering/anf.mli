open Ast

val anf : Templatetree.t -> Lineartree.t
(** [anf e] lowers an expression into A-normal form *)

val anf_command : Templatetree.command -> Lineartree.command
(** [anf_command c] lowers a command [c] into A-normal form *)

val anf_program : Templatetree.program -> Lineartree.program
(** [anf_program p] lowers a program [p] into A-normal form *)
