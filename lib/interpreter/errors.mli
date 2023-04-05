open Utils

exception InterpError of string * Location.t

val unreachable : reason:string -> loc:Location.t -> 'a
