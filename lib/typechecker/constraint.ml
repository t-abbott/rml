open Typing
open Utils

type t =
  | Pred of Predicate.t
  | Conj of t * t
  | Impl of { x : Ident_core.t; base : Base_ty.t; p : Predicate.t; c : t }

let impl x base p c = Impl { x; base; p; c }
