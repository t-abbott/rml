open Typing
open Utils
module PTree = Ast.Parsetree
module TTree = Ast.Typedtree

type context = Ty.t Context.t

val type_parsetree : PTree.t -> context -> TTree.t
(**
  [type_parsetree pt ctx] builds an explicity typed [Typedtree.t] from a partially 
  typed [Parsetree.t] [pt] with respect to a typing context [ctx]

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
