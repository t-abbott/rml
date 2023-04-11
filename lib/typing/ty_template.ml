open Utils
module L = Location

(* ad-hoc maybe refined type *)
include Liquid_ty.Make (Ident)

let unrefined_body vname base = RBase { vname; base; pred = None }

let unrefined ?(source = Source.Inferred) vname base_ty =
  { body = unrefined_body vname base_ty; source }

let builtin body = { body; source = Source.Builtin }
let inferred body = { body; source = Source.Inferred }

let t_bool ?(pred = None) vname =
  builtin (RBase { vname; base = Base_ty.TBool; pred })

let t_num ?(pred = None) vname =
  builtin (RBase { vname; base = Base_ty.TInt; pred })

let rec apply_types f_ty arg_tys =
  match (f_ty.body, arg_tys) with
  | ty, [] -> Some (inferred ty)
  | RArrow (_, s, t), x :: xs ->
      if equal_base s x then apply_types t xs else None
  | _ -> failwith "no"
