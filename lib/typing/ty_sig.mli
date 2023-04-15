(**
  Signature of a module representing a type.    
*)
module type TYPE = sig
  type t
  (**
      Top-level type type (expression with metadata).
    *)

  type t_body
  (**
      The actual type itself. 
    *)

  val equal : t -> t -> bool
  (**
      [equal t1 t2] is [true] if [t1] and [t2] represent the
      same type. Refinements are tested on syntactic equality
      and the name of the variable being bound is ignored.
    *)

  val equal_base : t -> t -> bool
  (**
      [equal_base t1 t2] is [true] if [t1] and [t2] represent
      same type (ignoring refinements).
    *)

  val to_string : t -> string
  (**
      [to_string ty] pretty-prints [ty].    
    *)

  val apply_types : t -> t list -> t option
  val ty_of_op : Op.Binop.t -> t
end
