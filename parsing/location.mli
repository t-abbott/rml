(** Represents a location in some source file *)
type t = {
  line_start: int;
  line_end: int;
  char_start: int;
  char_end: int;
  filename: string option
}

(** Represents value [body] of type ['a] tagged with a source
    location [loc]    
*)
type 'a located = {
  loc: t;
  body: 'a 
}

(** Builds an ['a located] from a location and a value
    of type ['a]    
*)
val locate : t -> 'a -> 'a located 

(** Creates a location spanning two [Lexing.position]s *)
val from : Lexing.position -> Lexing.position -> t

(** Pretty prints a [Location.t] value *)
val to_string : t -> string