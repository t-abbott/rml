open Utils

type t = { body : t_body; source : Source.t }
(**
  Represents the type of a term
*)

and t_body = RBase of Base_ty.t * Refinement.t option | RArrow of t list * t

val unrefined_body : Base_ty.t -> t_body
(**
    [unrefined_body ty] creates a [Ty.t_body] over [ty] with the trivial
    refinement [ty[v | true]] 
*)

val unrefined : ?source:Source.t -> Base_ty.t -> t
(** 
  [unrefined ty] creates a location-tagged liquid [Ty.t] over [ty] with the 
  trivial refinement [ty[v | true]]    
*)

val equal : t -> t -> bool
(**
  Tests if the base types of two liquid types are equal.
  
  Refinements are ignored, so comparing the terms [num[v | v > 0]], [num[v | v < 0]]
  returns [true]. 
*)

val equal_base : t -> t -> bool
(**
  Tests if the base types of two liquid types are equal.
  
  Refinements are ignored, so comparing the terms [num[v | v > 0]], [num[v | v < 0]]
  returns [true]. 
*)

val apply_types : t -> t list -> t option
(**
  [apply_types ty_f ty_xs] returns the resulting type after applying the types [ty_xs] to
  the type [ty_f].

  For example, [apply_types (RArrow ([TInt], TInt)) TInt] corresponds to applying a value
  of type [num] to a function of type [num -> num], and therefore evaluates to [TInt].

  Returns [None] in the event of a type mismatch (e.g. applying a type to a base type)
*)

val to_string : t -> string

(* val is_base : t -> bool
   (**
       [is_base ty] returns true if [ty] is a base (non-function) type
   *) *)

val is_function : t -> bool
(**
    [is_function ty] returns true if [ty] is a function type    
*)

val builtin : t_body -> t
(**
    [builtin ty] creates a builtin type from the raw type [ty]
*)

val inferred : t_body -> t
(**
    [inferred ty] creates an inferred type from the raw type [ty]
*)

val annotated : t_body -> Location.t -> t
(**
    [annotated ty] creates an annotated type from the raw type [ty]
*)

val tbool : t
(**
    Useful alias for [builtin (RBase (Base_ty.TBool, None))]
*)

val tnum : t
(**
    builtin (RBase (Base_ty.TInt, None))    
*)

val ty_of_refop : Refop.Binop.t -> t * t * t
(**
    [ty_of_refop t] returns the types of [op]s first 
    argument, second argument, and return value respectively.

*)

val lower_refinement :
  Refinement_surface.t -> t Context.t -> Base_ty.t -> Refinement.t
(**
    [lower_refinement ref ctx] lowers [ref] to a concerete refinement [Refinement.t]
    while performing type checking on it's definition and making sure
    it is a predicate (i.e. it returns a bool).
*)

val of_surface : Ty_surface.t -> t Context.t -> t
(**
    [of_surface ty] creates a [Ty_template.t] corresponding to [ty]
*)

val flatten : t -> t list
(**
  Flattens all the types of a function into a single list - e.g. 
    [num -> bool -> num] -> [[num; bool; num]]
*)

val uncurry : t -> t
(**
    Uncurries a type.    
*)

val arity : t -> int
(**
    [arity ty] returns the number of arguments taken by the
    type [ty], and [0] in the case that [ty] is not a function
    type.
*)
