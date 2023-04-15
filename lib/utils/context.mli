open Base
open Ident_sig

module Make : functor (Id : IDENT) -> sig
  type 'a t = (Id.t * 'a) list

  val empty : 'a t
  val find : Id.t -> 'a t -> 'a option
  val extend : Id.t -> 'a -> 'a t -> 'a t
end

include module type of Make (Ident)
