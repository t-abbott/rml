open Base
open Ident_sig

module Make =
functor
  (Id : IDENT)
  ->
  struct
    type 'a t = (Id.t * 'a) list

    let empty = []

    let find (name : Id.t) (ctx : 'a t) =
      List.Assoc.find ctx name ~equal:Id.equal

    let extend (name : Id.t) value env = (name, value) :: env
  end

(* default context type
   this is nasty but I'm keeping it for compat
*)
include Make (Ident)
