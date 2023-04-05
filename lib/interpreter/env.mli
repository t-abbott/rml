open Ast.Syntaxtree
open Utils

val empty : 'a Context.t

module type ENV = functor (Tree : SYNTAXTREE) -> sig
  type t = envval Context.t
  and envval = Closure of Tree.t * t | Value of Tree.t

  val to_string : envval -> string
  val empty : t
  val find : Ident.t -> t -> envval option
  val extend : Ident.t -> envval -> t -> t
end

module Make : ENV
