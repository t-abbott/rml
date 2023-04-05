module type REFINEMENT = sig
  type t

  val to_string : t -> string
  val equal : t -> t -> bool
end
