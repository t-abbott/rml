open Utils

type source = Builtin | Inferred | Annotation of Location.t

type t = { body : t_body; source : source }
and t_body = TInt | TBool | TArrow of t * t

val equal : t -> t -> bool
val to_string : t -> string
val is_base : t -> bool
val is_function : t -> bool
val builtin : t_body -> t
val inferred : t_body -> t
val annotated : t_body -> Location.t -> t
val apply_args : t -> t list -> t option
