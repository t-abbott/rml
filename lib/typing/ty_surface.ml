open Base
open Utils
module Loc = Location
module RSurface = Refinement_surface

type t = t_body Location.located
and t_body = SBase of Base_ty.t * RSurface.t option | SArrow of t list * t

let rec to_string (ty : t) =
  match ty.body with
  | SBase (t, r) ->
      let r_str =
        match r with
        | Some refinement -> ": " ^ RSurface.to_string refinement
        | _ -> ""
      in
      Base_ty.to_string t ^ r_str
  | SArrow (tys_from, ty_to) ->
      List.map ~f:to_string (tys_from @ [ ty_to ]) |> String.concat ~sep:" -> "

let unrefined_base ty = SBase (ty, Some (Loc.unlocated (RSurface.boolean true)))
