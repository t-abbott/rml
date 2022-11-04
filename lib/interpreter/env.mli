open Ast
open Utils

type t = envval Context.t
and envval = Closure of Parsetree.t * t | Value of Parsetree.t

val to_string : envval -> string
val empty : t
val find : Ident.t -> t -> envval option
val extend : Ident.t -> envval -> t -> t
