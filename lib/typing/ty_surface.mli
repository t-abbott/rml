(**
    [Ty_surface] contains the initial representation of
    types as outputted by the parser. Through type inference
    they are normalised and converted into a more useful
    form found in the module [Ty].
*)

open Utils

type t = t_body Location.located
and t_body = SBase of Base_ty.t * Refinement_surface.t | SArrow of t * t

val to_string : t -> string
