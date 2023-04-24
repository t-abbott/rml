open Utils
module L = Location

(* ad-hoc maybe refined type *)
include Liquid_ty.Make (Ident)
module P = R.P

let unrefined_body vname base = RBase { vname; base; pred = None }

let unrefined ?(source = Source.Inferred) vname base_ty =
  { body = unrefined_body vname base_ty; source }

let rec is_unrefined ty =
  match ty.body with
  | RBase { pred = None; _ } -> true
  | RArrow (_, s, t) -> is_unrefined s || is_unrefined t
  | _ -> false

let builtin body = { body; source = Source.Builtin }
let inferred body = { body; source = Source.Inferred }

(* helpers for constructing primitive types *)
let make_prim vname base pred source =
  let ref = R.from vname base pred in
  { body = RBase ref; source }

let prim_int ?(source = Source.Inferred) i =
  let open P in
  let vname = "_prim_" ^ Int.to_string i in
  let pred = Some (make_equal (mk_var vname) (mk_int i)) in
  make_prim vname Base_ty.TInt pred source

let prim_bool ?(source = Source.Inferred) b =
  let open P in
  let vname = "_prim_" ^ Bool.to_string b in
  let pred = Some (make_equal (mk_var vname) (mk_bool b)) in
  make_prim vname Base_ty.TBool pred source
