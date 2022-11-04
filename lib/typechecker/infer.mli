open Types
open Utils
module PTree = Ast.Parsetree
module TTree = Ast.Typedtree

exception TypeError of string * Location.t
exception NameError of string * Location.t

type context = Ty.t Context.t

val type_parsetree : PTree.t -> context -> TTree.t
(**
  Build an explicity typed [Typedtree.t] from a partially 
  typed [Parsetree.t].

  Performs type checking and inference.

  {1 FIXME}
  - perform inference on untyped arguments

  {1 Notes}
  Should type inference and type checking happen in different passes? Motivating
  examples are in the branches for [PTree.Fun] and [PTree.Apply], which would be
  much cleaner if they only constructed the [TypedTree.t] and passed it on to an
  inference/semantic analysis pass for further inspection. The tricky part is 
*)

val type_command : PTree.command -> context -> TTree.command * context
val type_program : PTree.program -> context -> TTree.program
