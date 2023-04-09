module type IDENT = sig
  type t

  val to_string : t -> string
end
