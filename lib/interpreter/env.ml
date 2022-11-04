open Ast
open Utils

type t = envval Context.t
and envval = Closure of Parsetree.t * t | Value of Parsetree.t

let to_string = function
  | Value v -> Parsetree.to_string v
  | Closure (_, _) -> "[closure]"

let empty = []
let find name (env : t) = Context.find name env
let extend = Context.extend
