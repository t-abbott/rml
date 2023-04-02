module type Type = sig
  type t
  type t_body

  val equal : t -> t -> bool
  val equal_base : t -> t -> bool
  val to_string : t -> string
end
