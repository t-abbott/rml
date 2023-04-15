open Ast.Syntaxtree
open Utils
open Utils.Ident_sig

let empty = []

(**
  Represents an environment mapping name to values from some
  tree
*)
module type ENV = functor (Id : IDENT) (Tree : SYNTAXTREE) -> sig
  module Ctx : module type of Context.Make (Id)

  type t = envval Ctx.t

  and envval =
    | Closure of Tree.t * t
    | Value of Tree.t
    | Internal of (Tree.t -> envval) * t

  val to_string : envval -> string
  val empty : t
  val find : Id.t -> t -> envval option
  val extend : Id.t -> envval -> t -> t
end

module Make : ENV =
functor
  (Id : IDENT)
  (Tree : SYNTAXTREE)
  ->
  struct
    module Ctx = Context.Make (Id)

    type t = envval Ctx.t

    and envval =
      | Closure of Tree.t * t
      | Value of Tree.t
      | Internal of (Tree.t -> envval) * t

    let to_string = function
      | Value v -> Tree.to_string v
      | Closure (_, _) -> "[closure]"
      | Internal (_, _) -> "[builtin]"

    let empty = empty
    let find name env = Ctx.find name env
    let extend = Ctx.extend
  end
