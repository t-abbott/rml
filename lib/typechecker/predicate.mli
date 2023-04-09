open Typing
open Utils

module InterpOp : sig
  type t = Add | Sub | Mult | Div | Mod | Equal | Less | Greater

  val type_of : t -> Ty_template.t
  val to_string : t -> string
end

type t =
  | Var of Ident_core.t
  | Bool of bool
  | Int of int
  | IntOp of InterpOp.t * t * t
  | Conj of t * t
  | Disj of t * t
  | IfThen of t * t * t

val to_string : t -> string

val of_ref : Refinement_core.t -> t
(**
    [of_ref r] creates a predicate corresponding
    to the refinement [r]
*)

val sub : Ident_core.t -> Ident_core.t -> t -> t
(**
    [sub v x p] substitutes [v] for [x] in [p]    
*)
