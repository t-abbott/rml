(** Represents a location in some source file *)
type t =
  | Loc of {
      line_start : int;
      line_end : int;
      char_start : int;
      char_end : int;
      filename : string option;
    }
  | Nowhere

type 'a located = { loc : t; body : 'a }
(** Represents value [body] of type ['a] tagged with a source
    location [loc]    
*)

val locate : t -> 'a -> 'a located
(** Builds an ['a located] from a location and a value
    of type ['a]    
*)

val unlocated : 'a -> 'a located
(**  *)

val from : Lexing.position -> Lexing.position -> t
(** Creates a location spanning two [Lexing.position]s *)

val to_string : t -> string
(** Pretty prints a [Location.t] value *)
