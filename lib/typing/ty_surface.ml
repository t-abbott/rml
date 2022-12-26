open Base
open Utils
module Loc = Location
module RSurface = Refinement_surface

type t = { body : t_body; source : Source.t }

and t_body =
  | SBase of Base_ty.t * RSurface.t option (* basic type *)
  | SArrow of t list * t (* function type *)

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
let annotated ty loc = { body = ty; source = Annotation loc }
