open Ast

val anf : Templatetree.t -> Lineartree.t
val anf_command : Templatetree.command -> Lineartree.command
val anf_program : Templatetree.program -> Lineartree.program
