open Utils
module RSurface = Refinement_surface

type t = t_body Location.located
and t_body = SBase of Ty_basic.t * RSurface.t | SArrow of t * t

let rec to_string (ty : t) =
  match ty.body with
  | SBase (t, r) -> Ty_basic.to_string t ^ ": " ^ RSurface.to_string r
  | SArrow (t1, t2) -> to_string t1 ^ " -> " ^ to_string t2
