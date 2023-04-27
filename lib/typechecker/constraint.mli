open Typing
open Utils
module P = Ty_template.R.P

type t =
  | Pred of P.t
  | Conj of t * t
  | Impl of { x : Ident.t; base : Base_ty.t; p : P.t; c : t }

val to_string : t -> string
val to_latex : t -> string
val pred : P.t -> t
val conj : t -> t -> t
val impl : Ident.t -> Base_ty.t -> P.t -> t -> t
val c_true : t
val c_false : t
