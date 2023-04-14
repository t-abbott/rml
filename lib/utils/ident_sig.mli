module type IDENT = sig
  type t

  val to_string : t -> string
  val equal : t -> t -> bool
end
