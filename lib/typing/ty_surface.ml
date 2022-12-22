open Utils
module RSurface = Refinement_surface

type t = t_body Location.located
and t_body = SBase of Base_ty.t * RSurface.t option | SArrow of t * t

let rec to_string (ty : t) =
  match ty.body with
  | SBase (t, r) ->
      let r_str =
        match r with
        | Some refinement -> ": " ^ RSurface.to_string refinement
        | _ -> ""
      in
      Base_ty.to_string t ^ r_str
  | SArrow (t1, t2) -> to_string t1 ^ " -> " ^ to_string t2
