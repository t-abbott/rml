open Typing
open Utils
module P : module type of Predicate.Make (Ident_core)

type t =
  | Pred of P.t
  | Conj of t * t
  | Impl of { x : Ident_core.t; base : Base_ty.t; p : P.t; c : t }

val to_string : t -> string
val impl : Ident_core.t -> Base_ty.t -> P.t -> t -> t
