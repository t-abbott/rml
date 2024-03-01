type t =
  | Var of string
  | GenSym of string option * int
  | BuiltinSym of string
      (**
  Internal representation of identifiers for once we start rewriting
  terms.
*)

val equal : t -> t -> bool
val to_string : t -> string

val fresh : ?prefix:string -> unit -> t
(**
  Generates a fresh symbol.
*)

val of_string : string -> t
(**
  Generates a symbol from a string.    
*)

val of_other : t -> t
(**
  Generates a fresh identifier from another. 
*)

val var : string -> t
(**
  Generates an [Ident_core.Var] from a variable name.
*)

val orig : t -> string
(**
  Recovers the original string used to generate an ident_core
*)
