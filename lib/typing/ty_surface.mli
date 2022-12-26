(**
    [Ty_surface] contains the initial representation of
    types as found on the raw parse tree. Not every term
    on the parsetree is typed, but those that are will 
    always have a base type and may also be annotated with a 
    refinement.
*)

open Utils

type t = { body : t_body; source : Source.t }
(**
    Represents a type in the partially-typed [Parsetree.t]
*)

and t_body =
  | SBase of Base_ty.t * Refinement_surface.t option
      (** Represents a basic, non-function type *)
  | SArrow of t list * t
      (** Represents a function between two [Ty_surface.t]s *)

val to_string : t -> string

val unrefined_base : Base_ty.t -> t_body
(**
    [unrefined_base ty] creates a [Ty_surface.t_body] over the base type [ty] with the
    trivial refinement [ty[v | true]] 
*)

val annotated : t_body -> Location.t -> t
(**
    [annotated ty loc] creates a [Ty_surface.t] annotated with the location [loc]
    from the raw type [ty].
*)
