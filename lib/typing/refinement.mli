open Utils.Ident_sig

module Make : functor (Id : IDENT) -> sig
  module P : module type of Predicate.Make (Id)

  type t = { vname : Id.t; base : Base_ty.t; pred : P.t option }

  val equal : t -> t -> bool
  val to_string : t -> string
end
