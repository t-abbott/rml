open Typing
open Utils
module P = Ty_template.R.P

type t =
  | Pred of P.t
  | Conj of t * t
  | Impl of { x : Ident.t; base : Base_ty.t; p : P.t; c : t }

let rec to_string = function
  | Pred p -> P.to_string p
  | Conj (l, r) ->
      let l_str, r_str = Misc.proj2 to_string l r in
      Printf.sprintf "(%s ∧ %s)" l_str r_str
  | Impl { x; base; p; c } ->
      let x_str = Ident.to_string x in
      let b_str = Base_ty.to_string base in
      let p_str = P.to_string p in
      let c_str = to_string c in
      Printf.sprintf "∀%s: %s.%s ⇒ %s" x_str b_str p_str c_str

let pred p = Pred p
let conj p q = Conj (p, q)
let impl x base p c = Impl { x; base; p; c }

(* useful aliases *)
let c_true = pred P.p_true
let c_false = pred P.p_false
