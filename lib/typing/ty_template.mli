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

val prim_int : ?source:Source.t -> int -> t
(**
  [prim_int i] constructs a primitive [RBase] corresponding
  to the type [int[v| v = i]]  
  *)

val prim_bool : ?source:Source.t -> bool -> t
(**
  [prim_bool b] constructs a primitive [RBase] corresponding
  to the type [bool[v | v = b]]  
 *)
