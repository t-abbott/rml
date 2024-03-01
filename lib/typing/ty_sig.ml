module type TYPE = sig
  type t
  type t_body

  val hd : t -> t option
  val tl : t -> t option
  val equal : t -> t -> bool
  val equal_base : t -> t -> bool
  val to_string : t -> string
  val apply_types : ?keep_refinements:bool -> t -> t list -> t option
  val ty_of_op : Op.Binop.t -> t
end
