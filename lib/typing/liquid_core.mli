open Utils
module Ref : module type of Ref_new.Make (Ident_core)

type t = RBase of Ref.t | RArrow of Ident_core.t * t * t

val to_string : t -> string
