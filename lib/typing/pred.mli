open Utils
open Utils.Ident_sig

module Make : functor (Id : IDENT) -> sig
  module InterpOp : sig
    type t = Add | Sub | Mult | Div | Mod | Equal | Less | Greater

    val to_string : t -> string
  end

  type t = t_body Location.located

  and t_body =
    | Var of Id.t
    | Bool of bool
    | Int of int
    | IntOp of InterpOp.t * t * t
    | Conj of t * t
    | Disj of t * t
    | IfThen of t * t * t

  val to_string : t -> string

  val sub : Id.t -> Id.t -> t -> t
  (**
        [sub v x p] substitutes [v] for [x] in [p]    
    *)
end
