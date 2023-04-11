open Printf
open Utils.Ident_sig

module Make =
functor
  (Id : IDENT)
  ->
  struct
    module P = Predicate.Make (Id)

    type t = { vname : Id.t; base : Base_ty.t; pred : P.t option }

    let to_string { vname; base; pred } =
      let v = Id.to_string vname in
      let b = Base_ty.to_string base in
      let p = match pred with Some p -> P.to_string p | None -> "<blank>" in
      sprintf "%s[%s | %s]" b v p

    let from vname base pred = { vname; base; pred }
    let equal = ( = )
  end
