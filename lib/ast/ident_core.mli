type t =
  | Var of string
  | Sym of string option * int
      (**
  Internal representation of identifiers for once we start rewriting
  terms.
*)

val to_string : t -> string

val fresh : ?prefix:string -> unit -> t
(**
  Generates a fresh symbol.
*)

val var : string -> t
(**
  Generates an [Ident.Var] from a variable name.
*)
