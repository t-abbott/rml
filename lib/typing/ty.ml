(**
  {1 TODO}
  - implement [flatten] and [normalise] functions
*)

open Printf
open Utils

type source = Builtin | Inferred | Annotation of Location.t

type t = { body : t_body; source : source }
and t_body = RBase of Ty_basic.t * Refinement.t | RArrow of t list * t

let rec to_string ty =
  match ty.body with
  | RBase (t, r) ->
      sprintf "%s[%s]" (Ty_basic.to_string t) (Refinement.to_string r)
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
      Ty_basic.equal b1 b2
      && if with_refinements then Refinement.equal r1 r2 else true
  | _ -> false

let equal t1 t2 = is_equal t1 t2 ~with_refinements:true
let equal_base t1 t2 = is_equal t1 t2 ~with_refinements:false
let is_base ty = match ty.body with RBase _ -> true | _ -> false
let is_function ty = match ty.body with RArrow _ -> true | _ -> false
let builtin ty = { body = ty; source = Builtin }
let inferred ty = { body = ty; source = Inferred }
let annotated ty loc = { body = ty; source = Annotation loc }

let basic ?(source = Inferred) ty_b =
  { body = RBase (ty_b, Refinement.boolean true); source }

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
