open Utils

(**
  Represents the source of a type.         
*)
type source =
  | Builtin  (** the type belongs to a builtin operator/function *)
  | Inferred  (** the type was inferred during type inference *)
  | Annotation of Location.t
      (** the type was annotated by the programmer in-line *)
  | ValStmt of Location.t
      (** the type was annotated by the programmer in a [val] statement  *)

type t = { body : t_body; source : source }
(**
  Represents the type of a term
*)

and t_body = RBase of Ty_basic.t * Refinement.t | RArrow of t list * t

val unrefined_body : Ty_basic.t -> t_body
(**
    [unrefined_body ty] creates a [Ty.t_body] over [ty] with the trivial
    refinement [ty[v | true]] 
*)

val unrefined : ?source:source -> Ty_basic.t -> t
(** 
  [unrefined ty] creates a location-tagged liquid [Ty.t] over [ty] with the 
  trivial refinement [ty[v | true]]    
*)

val equal : t -> t -> bool
(**
  Tests if the base types of two liquid types are equal.
  
  Refinements are ignored, so comparing the terms [int[v | v > 0]], [int[v | v < 0]]
  returns [true]. 
*)

val equal_base : t -> t -> bool
(**
  Tests if the base types of two liquid types are equal.
  
  Refinements are ignored, so comparing the terms [int[v | v > 0]], [int[v | v < 0]]
  returns [true]. 
*)

val apply_types : t -> t list -> t option
(**
  [apply_types ty_f ty_xs] returns the resulting type after applying the types [ty_xs] to
  the type [ty_f].

  For example, [apply_types (RArrow ([TInt], TInt)) TInt] corresponds to applying a value
  of type [int] to a function of type [int -> int], and therefore evaluates to [TInt].

  Returns [None] in the event of a type mismatch (e.g. applying a type to a base type)
*)

val to_string : t -> string

val is_base : t -> bool
(**
    [is_base ty] returns true if [ty] is a base (non-function) type    
*)

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
