open Printf
open Utils.Ident_sig

module type REFINEMENT = sig
  type t

  val to_string : t -> string
  val equal : t -> t -> bool
end

module Make =
functor
  (Id : IDENT)
  ->
  struct
    module P = Pred.Make (Id)

    type t = { vname : Id.t; base : Base_ty.t; pred : P.t }

    let to_string { vname; base; pred } =
      let v = Id.to_string vname in
      let b = Base_ty.to_string base in
      let p = P.to_string pred in
      sprintf "%s[%s | %s]" v b p
  end
