open Base

type 'a t = (string, 'a) List.Assoc.t

val empty : 'a t
val find : string -> 'a t -> 'a option
val extend : string -> 'a -> 'a t -> 'a t
