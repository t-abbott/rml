open Utils
module L = Location

(* ad-hoc maybe refined type *)
include Liquid_ty.Make (Ident)
module P = R.P

let unrefined_body vname base = RBase { vname; base; pred = None }

let unrefined ?(source = Source.Inferred) vname base_ty =
  { body = unrefined_body vname base_ty; source }

let builtin body = { body; source = Source.Builtin }
let inferred body = { body; source = Source.Inferred }

(* helpers for constructing base types *)
let t_bool ?(pred = None) vname =
  builtin (RBase { vname; base = Base_ty.TBool; pred })

let t_num ?(pred = None) vname =
  builtin (RBase { vname; base = Base_ty.TInt; pred })

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

let rec apply_types f_ty arg_tys =
  match (f_ty.body, arg_tys) with
  | ty, [] -> Some (inferred ty)
  | RArrow (_, s, t), x :: xs ->
      if equal_base s x then apply_types t xs else None
  | _ -> None
