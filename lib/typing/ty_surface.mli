(**
    [Ty_surface] contains the initial representation of
    types as found on the raw parse tree. Not every term
    on the parsetree is typed, but those that are will 
    always have a base type and may also be annotated with a 
    refinement.
*)

open Utils

type t = t_body Location.located
(**
    Represents a type in the partially-typed [Parsetree.t]
*)

and t_body =
  | SBase of Base_ty.t * Refinement_surface.t option (* sdjf *)
  | SArrow of t list * t

val to_string : t -> string

val unrefined_base : Base_ty.t -> t_body
(**
    [unrefined_base ty] creates a [Ty_surface.t_body] over the base type [ty] with the
    trivial refinement [ty[v | true]] 
*)
