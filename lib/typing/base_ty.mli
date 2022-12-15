(**
    Represents a base type like [int], [bool], or [string].    
*)
type t = TInt | TBool

val equal : t -> t -> bool
val to_string : t -> string
