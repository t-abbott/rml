open Printf
open Refinement
open Utils
module Loc = Location

module Make =
functor
  (Ref : REFINEMENT)
  ->
  struct
    type t = { body : t_body; source : Source.t }
    and t_body = RBase of Base_ty.t * Ref.t | RArrow of t list * t

    let rec to_string ty =
      match ty.body with
      | RBase (t, r) -> sprintf "%s%s" (Base_ty.to_string t) (Ref.to_string r)
      | RArrow (tys_from, ty_to) ->
          let args = List.map to_string tys_from |> String.concat " -> " in
          args ^ " -> " ^ to_string ty_to

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
      | RBase (b1, r1), RBase (b2, r2) ->
          Base_ty.equal b1 b2
          && if with_refinements then Ref.equal r1 r2 else true
      | _ -> false

    let equal t1 t2 = is_equal t1 t2 ~with_refinements:true
    let equal_base t1 t2 = is_equal t1 t2 ~with_refinements:false
    let is_function ty = match ty.body with RArrow _ -> true | _ -> false
    let builtin ty = { body = ty; source = Builtin }
    let inferred ty = { body = ty; source = Inferred }
    let annotated ty loc = { body = ty; source = Annotation loc }

    let rec flatten (ty : t) =
      match ty.body with
      | RBase _ -> [ ty ]
      | RArrow (t1, t2) -> t1 @ flatten t2

    let rec uncurry (ty : t) =
      let tys = ty |> flatten |> List.rev in
      if List.length tys = 1 then ty
      else
        let ty_to = List.hd tys in
        let tys_from = tys |> List.tl |> List.rev |> List.map uncurry in
        { ty with body = RArrow (tys_from, ty_to) }

    let arity ty =
      match ty.body with RArrow (tys_from, _) -> List.length tys_from | _ -> 0
  end
