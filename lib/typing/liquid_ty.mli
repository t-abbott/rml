open Utils
open Utils.Ident_sig
module Loc = Location

(**
  Constructs a liquid type    
*)
module Make : functor (Id : IDENT) -> sig
  module R : module type of Refinement.Make (Id)

  type t = { body : t_body; source : Source.t }
  (** Represents the type of a term *)

  and t_body = RBase of R.t | RArrow of Id.t * t * t

  type context = t Context.t

  val to_string : t -> string
  val is_equal : t -> t -> with_refinements:bool -> bool

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

  val valstmt : t_body -> Location.t -> t
  (**
    [valstmt ty loc] creates a val-defined [Ty_surface.t] from
    [ty] at source location [loc].
   *)

  val flatten : t -> t list
  (**
    Flattens all the types of a function into a single list - e.g. 
    [num -> bool -> num] -> [[num; bool; num]]
   *)

  val apply_types : t -> t list -> t option
  (**
     [apply_types ty_f ty_xs] returns the resulting type after applying the types [ty_xs] to
     the type [ty_f].
   
     For example, [apply_types (RArrow ([TInt], TInt)) TInt] corresponds to applying a value
     of type [num] to a function of type [num -> num], and therefore evaluates to [TInt].
   
     Returns [None] in the event of a type mismatch (e.g. applying a type to a base type)
   *)

  val arity : t -> int
  (**
    [arity ty] returns the number of arguments taken by the
    type [ty], and [0] in the case that [ty] is not a function
    type.
    *)

  val sub : Id.t -> Id.t -> t -> t
  (**
    [sub v x ty] performs capture-avoiding substitution of [v] for [x] in the type [ty]     
    *)

  val sub_term : Id.t -> R.P.t_body -> t -> t
  (**
    [sub_term v p ty] performs capture-avoiding substitution of [v] for the term [p] in
    predicates bound under the type [ty]
    *)

  val t_bool : ?pred:R.P.t option -> Id.t -> t
  val t_num : ?pred:R.P.t option -> Id.t -> t
  val ty_of_op : Op.Binop.t -> t
end
