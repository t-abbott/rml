open Utils.Ident_sig

module type REFINEMENT = sig
  type t

  val to_string : t -> string
  val equal : t -> t -> bool
end

module Make : functor (Id : IDENT) -> sig
  module P : module type of Pred.Make (Id)

  type t = { vname : Id.t; base : Base_ty.t; pred : P.t }

  val to_string : t -> string
end
