module type IDENT = sig
  type t

  val to_string : t -> string
  val of_string : string -> t
  val equal : t -> t -> bool
end
