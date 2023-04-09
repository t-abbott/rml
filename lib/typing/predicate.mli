open Utils.Ident_sig

module InterpOp : sig
  type t = Add | Sub | Mult | Div

  val type_of : t -> Ty_template.t
  val to_string : t -> string
end

module Make : functor (Id : IDENT) -> sig
  type t =
    | Var of Id.t
    | Const of Constant.t
    | IntOp of InterpOp.t * t * t
    | Conj of t * t
    | Disj of t * t
    | IfThen of t * t * t

  val to_string : t -> string
end
