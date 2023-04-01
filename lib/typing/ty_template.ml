(**
  {1 TODO}
  - implement [flatten] and [normalise] functions
*)

open Printf
open Utils
open Refinement_errors
module Loc = Location

type t = { body : t_body; source : Source.t }
and t_body = RBase of Base_ty.t * Refinement.t option | RArrow of t list * t

let rec to_string ty =
  match ty.body with
  | RBase (t, Some r) ->
      sprintf "%s[%s]" (Base_ty.to_string t) (Refinement.to_string r)
  | RBase (t, None) -> Base_ty.to_string t
  | RArrow (tys_from, ty_to) ->
      let args = List.map to_string tys_from |> String.concat " -> " in
      args ^ " -> " ^ to_string ty_to

(*
    FIXME: with_refinements not working? 
*)
let rec is_equal t1 t2 ~with_refinements =
  match (t1.body, t2.body) with
  | RArrow (t1s_from, t1_to), RArrow (t2s_from, t2_to) -> (
      match (t1s_from, t2s_from) with
      | x :: t1_rest, y :: t2_rest ->
          if is_equal x y ~with_refinements then
            let new_t1 = { t1 with body = RArrow (t1_rest, t1_to) } in
            let new_t2 = { t2 with body = RArrow (t2_rest, t2_to) } in
            is_equal new_t1 new_t2 ~with_refinements
          else false
      | [], [] -> is_equal t1_to t2_to ~with_refinements
      | [], _ -> is_equal t1_to t2 ~with_refinements
      | _, [] -> is_equal t1 t2_to ~with_refinements)
  | RBase (b1, _), RBase (b2, _) when not with_refinements ->
      Base_ty.equal b1 b2
  | RBase (b1, Some r1), RBase (b2, Some r2) ->
      Base_ty.equal b1 b2 && Refinement.equal r1 r2
  | RBase (b1, None), RBase (b2, None) -> Base_ty.equal b1 b2
  | _ -> false

let equal t1 t2 = is_equal t1 t2 ~with_refinements:true
let equal_base t1 t2 = is_equal t1 t2 ~with_refinements:false
let is_function ty = match ty.body with RArrow _ -> true | _ -> false
let builtin ty = { body = ty; source = Builtin }
let inferred ty = { body = ty; source = Inferred }
let annotated ty loc = { body = ty; source = Annotation loc }
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

let rec lower_refinement_expr (r_surface : Refinement_surface.t_expr)
    (ctx : t Context.t) (ty_expected : t) : Refinement.t_expr =
  let loc = r_surface.loc in
  let module R_surf = Refinement_surface in
  let expr, ty_actual =
    match r_surface.body with
    | R_surf.Var v -> (
        match Context.find v ctx with
        | Some ty ->
            if ty = ty_expected then (Refinement.Var v, ty)
            else
              let ty_str = to_string ty in
              let ty_expected_str = to_string ty_expected in
              let msg =
                sprintf
                  "type of variable '%s: %s' doesn't match expected type %s" v
                  ty_str ty_expected_str
              in
              raise (RefinementError (msg, loc))
        | None ->
            let msg = sprintf "reference to unknown variable %s" v in
            raise (RefinementError (msg, loc)))
    | R_surf.Const (Boolean b) -> (Refinement.boolean b, tbool)
    | R_surf.Const (Number n) -> (Refinement.number n, tnum)
    | R_surf.Binop (op, l_surf, r_surf) ->
        let ty_l, ty_r, ty_to = ty_of_refop op in
        let l = lower_refinement_expr l_surf ctx ty_l in
        let r = lower_refinement_expr r_surf ctx ty_r in
        (Refinement.Binop (op, l, r), ty_to)
    | R_surf.IfThen (cond_surf, if_t_surf, if_f_surf) ->
        let cond = lower_refinement_expr cond_surf ctx tbool in
        let if_t = lower_refinement_expr if_t_surf ctx ty_expected in
        let if_f = lower_refinement_expr if_f_surf ctx ty_expected in
        (Refinement.IfThen (cond, if_t, if_f), ty_expected)
  in
  if equal_base ty_actual ty_expected then Location.locate loc expr
  else
    let ty_a_str, ty_e_str = Misc.proj2 to_string ty_actual ty_expected in
    let msg =
      sprintf "type '%s' doesn't match expected type '%s'" ty_a_str ty_e_str
    in
    raise (RefinementError (msg, loc))

let lower_refinement (r_surface : Refinement_surface.t) (ctx : t Context.t)
    (bound_base_ty : Base_ty.t) : Refinement.t =
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

let rec flatten (ty : t) =
  match ty.body with RBase _ -> [ ty ] | RArrow (t1, t2) -> t1 @ flatten t2

(**
  [uncurry ty] produces an uncurried version of [ty] (i.e. as flattened as possible)
*)
let rec uncurry (ty : t) =
  let tys = ty |> flatten |> List.rev in
  if List.length tys = 1 then ty
  else
    let ty_to = List.hd tys in
    let tys_from = tys |> List.tl |> List.rev |> List.map uncurry in
    { ty with body = RArrow (tys_from, ty_to) }

let arity ty =
  match ty.body with RArrow (tys_from, _) -> List.length tys_from | _ -> 0
