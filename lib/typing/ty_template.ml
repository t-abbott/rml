open Utils
module Loc = Location
module Ref = Refinement_core

(* ad-hoc maybe refined type *)
include Liquid_ty.Make (struct
  type t = Refinement_core.t option

  let to_string = function
    | Some ref -> "[" ^ Refinement_core.to_string ref ^ "]"
    | None -> ""

  let equal a b =
    match (a, b) with
    | Some r1, Some r2 -> Refinement_core.equal r1 r2
    | None, None -> true
    | _ -> false
end)

let unrefined_body ty = RBase (ty, None)

let unrefined ?(source = Source.Inferred) ty =
  { body = unrefined_body ty; source }

let tbool = builtin (RBase (Base_ty.TBool, None))
let tnum = builtin (RBase (Base_ty.TInt, None))

let rec apply_types f types =
  match (f.body, types) with
  | ty, [] -> Some (inferred ty)
  | RArrow ([ ty_param ], g), ty_arg :: args_rest ->
      if equal_base ty_param ty_arg then apply_types g args_rest else None
  | RArrow (ty_param :: params_rest, g), ty_arg :: args_rest ->
      if equal_base ty_param ty_arg then
        let new_f = { f with body = RArrow (params_rest, g) } in
        apply_types new_f args_rest
      else None
  | _ -> None

let ty_of_refop = function
  | Refop.Binop.Less -> (tnum, tnum, tbool)
  | Refop.Binop.Greater -> (tnum, tnum, tbool)
  | Refop.Binop.Equal -> (tnum, tnum, tbool)
  | Refop.Binop.And -> (tbool, tbool, tbool)
  | Refop.Binop.Or -> (tbool, tbool, tbool)
  | Refop.Binop.Add -> (tnum, tnum, tnum)
  | Refop.Binop.Sub -> (tnum, tnum, tnum)
  | Refop.Binop.Mod -> (tnum, tnum, tbool)

(* TODO: refactor *)
let rec lower_refinement_expr (r_surface : Refinement_surface.t_expr)
    (ctx : t Context.t) (ty_expected : t) : Ref.t_expr =
  let lower r = lower_refinement_expr r ctx ty_expected in
  let loc = r_surface.loc in
  let module R_surf = Refinement_surface in
  let expr =
    match r_surface.body with
    | R_surf.Var v -> Ref.Var v
    | R_surf.Const c -> Ref.Const c
    | R_surf.Binop (op, l, r) -> Ref.Binop (op, lower l, lower r)
    | R_surf.IfThen (cond, if_t, if_f) ->
        Ref.IfThen (lower cond, lower if_t, lower if_f)
  in
  Location.locate loc expr

let lower_refinement (r_surface : Refinement_surface.t) (ctx : t Context.t)
    (bound_base_ty : Base_ty.t) : Ref.t =
  let bound_ty_t = unrefined bound_base_ty in
  let ctx' = Context.extend r_surface.bound_var bound_ty_t ctx in
  let expr = lower_refinement_expr r_surface.expr ctx' tbool in
  { bound_var = r_surface.bound_var; expr }

let rec of_surface (t_surface : Ty_surface.t) (ctx : t Context.t) : t =
  let source = t_surface.source in
  let t_template =
    match t_surface.body with
    | Ty_surface.SBase (ty_base, Some r_surface) ->
        RBase (ty_base, Some (lower_refinement r_surface ctx ty_base))
    | Ty_surface.SBase (ty_base, None) -> RBase (ty_base, None)
    | Ty_surface.SArrow (tys_from, ty_to) ->
        let f ty = of_surface ty ctx in
        RArrow (List.map f tys_from, of_surface ty_to ctx)
  in
  { body = t_template; source }
