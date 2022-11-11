open Ast.Syntaxtree
open Utils

let empty = []

(**
  Represents an environment mapping name to values from some
  tree
*)
module type ENV = functor (Tree : SYNTAXTREE) -> sig
  type t = envval Context.t
  and envval = Closure of Tree.t * t | Value of Tree.t

  val to_string : envval -> string
  val empty : t
  val find : Ident.t -> t -> envval option
  val extend : Ident.t -> envval -> t -> t
end

module MakeEnv : ENV =
functor
  (Tree : SYNTAXTREE)
  ->
  struct
    type t = envval Context.t
    and envval = Closure of Tree.t * t | Value of Tree.t

    let to_string = function
      | Value v -> Tree.to_string v
      | Closure (_, _) -> "[closure]"

    let empty = empty
    let find name (env : t) = Context.find name env
    let extend = Context.extend
  end
