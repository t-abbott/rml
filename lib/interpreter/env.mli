open Ast.Syntaxtree
open Utils
open Utils.Ident_sig

val empty : 'a Context.t

module type ENV = functor (Id : IDENT) (Tree : SYNTAXTREE) -> sig
  module Ctx : module type of Context.Make (Id)

  type t = envval Ctx.t
  and envval = Closure of Tree.t * t | Value of Tree.t

  val to_string : envval -> string
  val empty : t
  val find : Id.t -> t -> envval option
  val extend : Id.t -> envval -> t -> t
end

module Make : ENV
