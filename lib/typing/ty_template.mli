open Utils
include module type of Liquid_ty.Make (Ident)

val unrefined_body : Ident.t -> Base_ty.t -> t_body
(**
    [unrefined_body ty] creates a [Ty.t_body] over [ty] with the trivial
    refinement [ty[v | true]] 
*)

val unrefined : ?source:Source.t -> Ident.t -> Base_ty.t -> t
(** 
  [unrefined ty] creates a location-tagged liquid [Ty.t] over [ty] with the 
  trivial refinement [ty[v | true]]    
*)

val builtin : t_body -> t
val inferred : t_body -> t

val apply_types : t -> t list -> t option
(**
  [apply_types ty_f ty_xs] returns the resulting type after applying the types [ty_xs] to
  the type [ty_f].

  For example, [apply_types (RArrow ([TInt], TInt)) TInt] corresponds to applying a value
  of type [num] to a function of type [num -> num], and therefore evaluates to [TInt].

  Returns [None] in the event of a type mismatch (e.g. applying a type to a base type)
*)

val t_bool : ?pred:R.P.t option -> Ident.t -> t
(**
    Useful alias for [builtin (RBase (Base_ty.TBool, None))]
*)

val t_num : ?pred:R.P.t option -> Ident.t -> t
(**
    builtin (RBase (Base_ty.TInt, None))    
*)

(* val lower_refinement :
   Refinement_surface.t -> t Context.t -> Base_ty.t -> Refinement_core.t *)
(**
    [lower_refinement ref ctx] lowers [ref] to a concerete refinement [Refinement.t]
    while performing type checking on it's definition and making sure
    it is a predicate (i.e. it returns a bool).
*)

(* val of_surface : Ty_surface.t -> t Context.t -> t *)
(**
    [of_surface ty] creates a [Ty_template.t] corresponding to [ty]
*)
